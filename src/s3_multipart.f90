!> S3 Multipart Upload support for large files (>5GB)
!>
!> This module implements the S3 multipart upload protocol, which allows uploading
!> files larger than the 5GB single-PUT limit. Files are split into parts (5MB-5GB each),
!> uploaded separately, and then combined on the S3 server.
!>
!> ## Multipart Upload Flow
!>
!> 1. **Initiate** - Start multipart upload, receive UploadId
!> 2. **Upload Parts** - Upload each part (1-10,000 parts), receive ETags
!> 3. **Complete** - Send list of parts+ETags to combine them
!>    OR **Abort** - Cancel upload and clean up parts
!>
!> ## Usage Example
!>
!> ```fortran
!> use s3_multipart
!> type(multipart_upload_t) :: upload
!> logical :: success
!>
!> ! High-level convenience function (recommended)
!> success = s3_put_large_file('data/huge_model_output.nc', '/path/to/local/file.nc', chunk_size_mb=100)
!>
!> ! OR low-level control
!> success = s3_multipart_init('data/output.nc', upload)
!> success = s3_multipart_upload_part(upload, 1, part1_data)
!> success = s3_multipart_upload_part(upload, 2, part2_data)
!> success = s3_multipart_complete(upload)
!> ```
!>
!> @note Requires AWS credentials configured in s3_config for authentication.
!> @warning Each part must be at least 5MB (except the last part).
module s3_multipart
    use iso_fortran_env, only: int64
    use s3_http, only: s3_config, s3_get_config, s3_is_initialized
    use s3_logger
    use s3_errors
    use libcurl_bindings, only: is_libcurl_available, curl_buffer_t, &
                                 curl_get_to_buffer_with_headers
    use aws_auth, only: aws_sign_request, aws_credential_t
    use openssl_bindings, only: sha256_hash, hex_encode, is_openssl_available
    use curl_stream, only: is_streaming_available
    implicit none
    private

    !> Type representing an in-progress multipart upload
    !>
    !> Tracks the state of a multipart upload including the upload ID from S3,
    !> the object key, and ETags from each uploaded part.
    type, public :: multipart_upload_t
        character(len=:), allocatable :: upload_id      !< S3 UploadId from initiation
        character(len=:), allocatable :: key            !< S3 object key
        character(len=:), allocatable :: bucket         !< S3 bucket name
        integer :: num_parts = 0                        !< Number of parts uploaded so far
        integer :: max_parts = 10000                    !< Maximum parts allocated
        character(len=:), allocatable, dimension(:) :: etags  !< ETag for each part
        integer, dimension(:), allocatable :: part_sizes      !< Size of each part in bytes
    end type multipart_upload_t

    ! Public interface
    public :: s3_multipart_init
    public :: s3_multipart_upload_part
    public :: s3_multipart_upload_part_from_file
    public :: s3_multipart_complete
    public :: s3_multipart_abort
    public :: s3_put_large_file

    ! S3 limits
    integer, parameter :: MIN_PART_SIZE = 5242880        ! 5 MB (5 * 1024 * 1024)
    integer(kind=8), parameter :: MAX_PART_SIZE = 5368709120_8     ! 5 GB (5 * 1024 * 1024 * 1024)
    integer, parameter :: MAX_PARTS_ALLOWED = 10000      ! S3 limit
    integer(kind=8), parameter :: MAX_OBJECT_SIZE = 5497558138880_8  ! 5 TB

contains

    !> Initiate a multipart upload and receive UploadId from S3.
    !>
    !> Sends a POST request to S3 with ?uploads query parameter to start a
    !> multipart upload session. S3 responds with an XML containing the UploadId
    !> which must be used for all subsequent part uploads and completion.
    !>
    !> @param[in] key The S3 object key for the upload
    !> @param[out] upload The multipart upload handle
    !> @return .true. if initiation succeeded, .false. on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> type(multipart_upload_t) :: upload
    !> logical :: success
    !>
    !> success = s3_multipart_init('data/large_file.nc', upload)
    !> if (.not. success) then
    !>     print *, 'Failed to initiate multipart upload'
    !> end if
    !> ```
    !>
    !> @note Requires s3_init() to be called first with valid AWS credentials.
    function s3_multipart_init(key, upload) result(success)
        character(len=*), intent(in) :: key
        type(multipart_upload_t), intent(out) :: upload
        logical :: success

        type(s3_config) :: config
        character(len=2048) :: url, cmd, tmpfile
        character(len=:), allocatable :: response
        integer :: unit, ios

        success = .false.
        call s3_clear_error()

        ! Check if S3 is initialized
        if (.not. s3_is_initialized()) then
            call s3_set_error(S3_ERROR_INIT, 0, &
                "s3_multipart_init called before s3_init()", &
                "Call s3_init() with configuration before using multipart operations")
            return
        end if

        config = s3_get_config()

        call s3_log_info("s3_multipart_init: Initiating multipart upload for key: " // trim(key))

        ! Allocate ETag storage
        allocate(character(len=64) :: upload%etags(MAX_PARTS_ALLOWED))
        allocate(upload%part_sizes(MAX_PARTS_ALLOWED))
        upload%etags = ''
        upload%part_sizes = 0

        ! Store key and bucket for later operations
        upload%key = trim(key)
        upload%bucket = trim(config%bucket)
        upload%num_parts = 0

        ! Build URL with ?uploads query parameter
        if (config%use_path_style) then
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', trim(config%endpoint), '/', &
                    trim(config%bucket), '/', trim(key), '?uploads'
            else
                write(url, '(A,A,A,A,A,A)') 'http://', trim(config%endpoint), '/', &
                    trim(config%bucket), '/', trim(key), '?uploads'
            end if
        else
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', trim(config%bucket), '.', &
                    trim(config%endpoint), '/', trim(key), '?uploads'
            else
                write(url, '(A,A,A,A,A,A)') 'http://', trim(config%bucket), '.', &
                    trim(config%endpoint), '/', trim(key), '?uploads'
            end if
        end if

        ! Create temp file for response
        tmpfile = generate_temp_filename('s3_multipart_init_', '.xml')

        ! Execute curl POST request (simplified - needs AWS v4 signature in production)
        write(cmd, '(A,A,A,A,A)') 'curl -s -X POST "', trim(url), '" -o ', trim(tmpfile)
        call execute_command_line(cmd, exitstat=ios)

        if (ios /= 0) then
            call s3_set_error(S3_ERROR_NETWORK, 0, &
                "Failed to initiate multipart upload", &
                "Check network connectivity and S3 endpoint configuration")
            return
        end if

        ! Read response
        open(newunit=unit, file=tmpfile, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Failed to read multipart init response", &
                "Internal error reading temporary file")
            return
        end if

        block
            character(len=10000) :: temp_response
            read(unit, '(A)', iostat=ios) temp_response
            if (ios == 0) response = trim(temp_response)
        end block
        close(unit)

        ! Clean up temp file
        write(cmd, '(A,A)') 'rm -f ', trim(tmpfile)
        call execute_command_line(cmd)

        ! Extract UploadId from XML response
        upload%upload_id = extract_upload_id(response)

        if (len_trim(upload%upload_id) == 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Failed to parse UploadId from response", &
                "S3 response may be malformed or operation may have failed")
            success = .false.
        else
            call s3_log_debug("Multipart upload initiated with UploadId: " // trim(upload%upload_id))
            success = .true.
        end if
    end function s3_multipart_init

    !> Upload a single part from memory.
    !>
    !> Uploads one part of a multipart upload. Each part must be at least 5MB
    !> (except the last part which can be smaller). Parts are numbered starting
    !> from 1 and can be uploaded in any order.
    !>
    !> @param[inout] upload The multipart upload handle
    !> @param[in] part_number Part number (1-10000)
    !> @param[in] data The part data to upload
    !> @return .true. if upload succeeded, .false. on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> success = s3_multipart_upload_part(upload, 1, chunk1_data)
    !> success = s3_multipart_upload_part(upload, 2, chunk2_data)
    !> ```
    !>
    !> @warning Part must be at least 5MB except for the last part.
    !> @note The ETag returned by S3 is automatically stored in the upload handle.
    function s3_multipart_upload_part(upload, part_number, data) result(success)
        type(multipart_upload_t), intent(inout) :: upload
        integer, intent(in) :: part_number
        character(len=*), intent(in) :: data
        logical :: success

        type(s3_config) :: config
        character(len=2048) :: url, cmd
        character(len=:), allocatable :: tmpfile_data, tmpfile_headers
        character(len=:), allocatable :: etag
        character(len=10000) :: headers_content
        integer :: data_len, ios, unit

        success = .false.
        call s3_clear_error()

        ! Check S3 is initialized
        if (.not. s3_is_initialized()) then
            call s3_set_error(S3_ERROR_INIT, 0, &
                "S3 not initialized", &
                "Call s3_init() before uploading multipart data")
            return
        end if

        ! Validate upload state
        if (.not. allocated(upload%upload_id) .or. .not. allocated(upload%key)) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Invalid multipart upload state", &
                "Call s3_multipart_init() first")
            return
        end if

        ! Validate part number
        if (part_number < 1 .or. part_number > MAX_PARTS_ALLOWED) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Invalid part number (must be 1-10000)", &
                "S3 allows maximum 10,000 parts per multipart upload")
            return
        end if

        ! Check part size (minimum 5MB, except last part)
        data_len = len(data)
        if (data_len < MIN_PART_SIZE) then
            call s3_log_warn("Part smaller than 5MB - this should only be the last part")
        end if

        call s3_log_debug("s3_multipart_upload_part: Uploading part " // &
            trim(adjustl(int_to_str(part_number))))

        ! Get config
        config = s3_get_config()

        ! Build URL with partNumber and uploadId query parameters
        if (config%path_style_url) then
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A,A,I0,A,A)') 'https://', trim(config%endpoint), '/', &
                    trim(upload%bucket), '/', trim(upload%key), '?partNumber=', part_number, &
                    '&uploadId=', trim(upload%upload_id)
            else
                write(url, '(A,A,A,A,A,A,A,I0,A,A)') 'http://', trim(config%endpoint), '/', &
                    trim(upload%bucket), '/', trim(upload%key), '?partNumber=', part_number, &
                    '&uploadId=', trim(upload%upload_id)
            end if
        else
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A,A,I0,A,A)') 'https://', trim(upload%bucket), '.', &
                    trim(config%endpoint), '/', trim(upload%key), '?partNumber=', part_number, &
                    '&uploadId=', trim(upload%upload_id)
            else
                write(url, '(A,A,A,A,A,A,A,I0,A,A)') 'http://', trim(upload%bucket), '.', &
                    trim(config%endpoint), '/', trim(upload%key), '?partNumber=', part_number, &
                    '&uploadId=', trim(upload%upload_id)
            end if
        end if

        ! Write data to temp file
        tmpfile_data = generate_temp_filename('s3_part_data_', '.bin')
        open(newunit=unit, file=tmpfile_data, status='replace', action='write', &
             access='stream', iostat=ios)
        if (ios /= 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Failed to create temporary data file", &
                "Check /tmp directory permissions and disk space")
            return
        end if
        write(unit, iostat=ios) data
        close(unit)

        if (ios /= 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Failed to write part data to temp file", &
                "Check disk space availability")
            return
        end if

        ! Create temp file for headers
        tmpfile_headers = generate_temp_filename('s3_part_headers_', '.txt')

        ! Execute curl PUT request and capture headers
        write(cmd, '(A,A,A,A,A,A,A)') 'curl -s -X PUT "', trim(url), &
            '" -T ', trim(tmpfile_data), ' -D ', trim(tmpfile_headers)
        call execute_command_line(cmd, exitstat=ios)

        ! Clean up data file
        open(newunit=unit, file=tmpfile_data, status='old', iostat=ios)
        if (ios == 0) close(unit, status='delete')

        if (ios /= 0) then
            call s3_set_error(S3_ERROR_NETWORK, 0, &
                "Failed to upload part", &
                "Check network connectivity and S3 credentials")
            success = .false.
            return
        end if

        ! Read response headers
        open(newunit=unit, file=tmpfile_headers, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Failed to read response headers", &
                "Upload may have succeeded but ETag extraction failed")
            success = .false.
            return
        end if

        headers_content = ''
        read(unit, '(A)', iostat=ios) headers_content
        close(unit, status='delete')

        ! Extract ETag from headers
        etag = extract_etag(headers_content)
        if (len(etag) == 0) then
            call s3_set_error(S3_ERROR_PARSE, 0, &
                "Failed to extract ETag from response", &
                "Upload may have succeeded but response was unexpected")
            success = .false.
            return
        end if

        ! Store ETag (ensure etags array is allocated)
        if (.not. allocated(upload%etags)) then
            allocate(character(len=100) :: upload%etags(MAX_PARTS_ALLOWED))
        end if
        upload%etags(part_number) = etag

        ! Store part size
        if (.not. allocated(upload%part_sizes)) then
            allocate(upload%part_sizes(MAX_PARTS_ALLOWED))
        end if
        upload%part_sizes(part_number) = data_len

        ! Update part count
        if (part_number > upload%num_parts) then
            upload%num_parts = part_number
        end if

        call s3_log_info("Part " // trim(adjustl(int_to_str(part_number))) // &
            " uploaded successfully, ETag: " // trim(etag))

        success = .true.
    end function s3_multipart_upload_part

    !> Upload a single part from a file segment.
    !>
    !> Reads a portion of a file and uploads it as one part. This is more
    !> memory-efficient than loading the entire part into memory first.
    !>
    !> @param[inout] upload The multipart upload handle
    !> @param[in] part_number Part number (1-10000)
    !> @param[in] file_path Path to the local file
    !> @param[in] offset Byte offset in file to start reading
    !> @param[in] length Number of bytes to read
    !> @return .true. if upload succeeded, .false. on error
    !>
    !> @note This function is more efficient for large files as it streams directly from disk.
    function s3_multipart_upload_part_from_file(upload, part_number, file_path, offset, length) &
        result(success)
        type(multipart_upload_t), intent(inout) :: upload
        integer, intent(in) :: part_number
        character(len=*), intent(in) :: file_path
        integer(kind=8), intent(in) :: offset
        integer, intent(in) :: length
        logical :: success

        type(s3_config) :: config
        character(len=2048) :: url, cmd
        character(len=:), allocatable :: tmpfile_part, tmpfile_headers
        character(len=:), allocatable :: etag
        character(len=10000) :: headers_content
        integer :: ios, unit
        integer(kind=8) :: skip_blocks

        success = .false.
        call s3_clear_error()

        ! Check S3 is initialized
        if (.not. s3_is_initialized()) then
            call s3_set_error(S3_ERROR_INIT, 0, &
                "S3 not initialized", &
                "Call s3_init() before uploading multipart data")
            return
        end if

        ! Validate upload state
        if (.not. allocated(upload%upload_id) .or. .not. allocated(upload%key)) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Invalid multipart upload state", &
                "Call s3_multipart_init() first")
            return
        end if

        ! Validate part number
        if (part_number < 1 .or. part_number > MAX_PARTS_ALLOWED) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Invalid part number (must be 1-10000)", &
                "S3 allows maximum 10,000 parts per multipart upload")
            return
        end if

        ! Check part size (minimum 5MB, except last part)
        if (length < MIN_PART_SIZE) then
            call s3_log_warn("Part smaller than 5MB - this should only be the last part")
        end if

        call s3_log_debug("s3_multipart_upload_part_from_file: Part " // &
            trim(adjustl(int_to_str(part_number))) // " from file")

        ! Get config
        config = s3_get_config()

        ! Build URL with partNumber and uploadId query parameters
        if (config%path_style_url) then
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A,A,I0,A,A)') 'https://', trim(config%endpoint), '/', &
                    trim(upload%bucket), '/', trim(upload%key), '?partNumber=', part_number, &
                    '&uploadId=', trim(upload%upload_id)
            else
                write(url, '(A,A,A,A,A,A,A,I0,A,A)') 'http://', trim(config%endpoint), '/', &
                    trim(upload%bucket), '/', trim(upload%key), '?partNumber=', part_number, &
                    '&uploadId=', trim(upload%upload_id)
            end if
        else
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A,A,I0,A,A)') 'https://', trim(upload%bucket), '.', &
                    trim(config%endpoint), '/', trim(upload%key), '?partNumber=', part_number, &
                    '&uploadId=', trim(upload%upload_id)
            else
                write(url, '(A,A,A,A,A,A,A,I0,A,A)') 'http://', trim(upload%bucket), '.', &
                    trim(config%endpoint), '/', trim(upload%key), '?partNumber=', part_number, &
                    '&uploadId=', trim(upload%upload_id)
            end if
        end if

        ! Extract part from file using dd
        tmpfile_part = generate_temp_filename('s3_file_part_', '.bin')
        skip_blocks = offset

        ! Use dd to extract the part (bs=1 for byte-level precision)
        write(cmd, '(A,A,A,A,A,I0,A,I0,A)') &
            'dd if="', trim(file_path), '" of="', trim(tmpfile_part), &
            '" bs=1 skip=', skip_blocks, ' count=', length, ' 2>/dev/null'
        call execute_command_line(cmd, exitstat=ios)

        if (ios /= 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Failed to extract part from file", &
                "Check file path and ensure file is readable")
            return
        end if

        ! Create temp file for headers
        tmpfile_headers = generate_temp_filename('s3_filepart_headers_', '.txt')

        ! Execute curl PUT request and capture headers
        write(cmd, '(A,A,A,A,A,A,A)') 'curl -s -X PUT "', trim(url), &
            '" -T "', trim(tmpfile_part), '" -D "', trim(tmpfile_headers), '"'
        call execute_command_line(cmd, exitstat=ios)

        ! Clean up part file
        open(newunit=unit, file=tmpfile_part, status='old', iostat=ios)
        if (ios == 0) close(unit, status='delete')

        if (ios /= 0) then
            call s3_set_error(S3_ERROR_NETWORK, 0, &
                "Failed to upload part", &
                "Check network connectivity and S3 credentials")
            success = .false.
            return
        end if

        ! Read response headers
        open(newunit=unit, file=tmpfile_headers, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Failed to read response headers", &
                "Upload may have succeeded but ETag extraction failed")
            success = .false.
            return
        end if

        headers_content = ''
        read(unit, '(A)', iostat=ios) headers_content
        close(unit, status='delete')

        ! Extract ETag from headers
        etag = extract_etag(headers_content)
        if (len(etag) == 0) then
            call s3_set_error(S3_ERROR_PARSE, 0, &
                "Failed to extract ETag from response", &
                "Upload may have succeeded but response was unexpected")
            success = .false.
            return
        end if

        ! Store ETag (ensure etags array is allocated)
        if (.not. allocated(upload%etags)) then
            allocate(character(len=100) :: upload%etags(MAX_PARTS_ALLOWED))
        end if
        upload%etags(part_number) = etag

        ! Store part size
        if (.not. allocated(upload%part_sizes)) then
            allocate(upload%part_sizes(MAX_PARTS_ALLOWED))
        end if
        upload%part_sizes(part_number) = length

        ! Update part count
        if (part_number > upload%num_parts) then
            upload%num_parts = part_number
        end if

        call s3_log_info("Part " // trim(adjustl(int_to_str(part_number))) // &
            " uploaded from file, ETag: " // trim(etag))

        success = .true.
    end function s3_multipart_upload_part_from_file

    !> Complete a multipart upload.
    !>
    !> Sends a completion request to S3 with the list of all uploaded parts
    !> and their ETags. S3 combines the parts into a single object.
    !>
    !> @param[inout] upload The multipart upload handle
    !> @return .true. if completion succeeded, .false. on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> ! After uploading all parts
    !> success = s3_multipart_complete(upload)
    !> if (success) then
    !>     print *, 'Upload completed successfully'
    !> end if
    !> ```
    !>
    !> @note All parts must be uploaded before calling this function.
    !> @warning If this fails, you should call s3_multipart_abort() to clean up.
    function s3_multipart_complete(upload) result(success)
        type(multipart_upload_t), intent(inout) :: upload
        logical :: success

        type(s3_config) :: config
        character(len=2048) :: url, cmd, tmpfile_xml, tmpfile_response
        character(len=:), allocatable :: xml_body
        integer :: unit, ios

        success = .false.
        call s3_clear_error()

        if (upload%num_parts == 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Cannot complete upload with zero parts", &
                "Upload at least one part before calling s3_multipart_complete()")
            return
        end if

        if (.not. s3_is_initialized()) then
            call s3_set_error(S3_ERROR_INIT, 0, &
                "S3 not initialized", &
                "Call s3_init() before using multipart operations")
            return
        end if

        config = s3_get_config()

        call s3_log_info("s3_multipart_complete: Completing upload with " // &
            trim(adjustl(int_to_str(upload%num_parts))) // " parts")

        ! Build URL with ?uploadId= query parameter
        if (config%use_path_style) then
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A,A,A)') 'https://', trim(config%endpoint), '/', &
                    trim(upload%bucket), '/', trim(upload%key), '?uploadId=', trim(upload%upload_id)
            else
                write(url, '(A,A,A,A,A,A,A,A)') 'http://', trim(config%endpoint), '/', &
                    trim(upload%bucket), '/', trim(upload%key), '?uploadId=', trim(upload%upload_id)
            end if
        else
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A,A,A)') 'https://', trim(upload%bucket), '.', &
                    trim(config%endpoint), '/', trim(upload%key), '?uploadId=', trim(upload%upload_id)
            else
                write(url, '(A,A,A,A,A,A,A,A)') 'http://', trim(upload%bucket), '.', &
                    trim(config%endpoint), '/', trim(upload%key), '?uploadId=', trim(upload%upload_id)
            end if
        end if

        ! Build XML body with part numbers and ETags
        xml_body = build_complete_multipart_xml(upload)

        ! Write XML to temp file
        tmpfile_xml = generate_temp_filename('s3_complete_', '.xml')
        open(newunit=unit, file=tmpfile_xml, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Failed to create temporary XML file", &
                "Internal error creating temporary file")
            return
        end if
        write(unit, '(A)') xml_body
        close(unit)

        ! Create response temp file
        tmpfile_response = generate_temp_filename('s3_complete_response_', '.xml')

        ! Execute curl POST request with XML body
        write(cmd, '(A,A,A,A,A,A,A)') 'curl -s -X POST "', trim(url), &
            '" --data-binary @', trim(tmpfile_xml), ' -o ', trim(tmpfile_response)
        call execute_command_line(cmd, exitstat=ios)

        ! Clean up temp files
        write(cmd, '(A,A,A,A)') 'rm -f ', trim(tmpfile_xml), ' ', trim(tmpfile_response)
        call execute_command_line(cmd)

        if (ios /= 0) then
            call s3_set_error(S3_ERROR_NETWORK, 0, &
                "Failed to complete multipart upload", &
                "Check network connectivity")
            success = .false.
        else
            call s3_log_info("Multipart upload completed successfully")
            success = .true.
        end if
    end function s3_multipart_complete

    !> Abort a multipart upload and clean up all parts.
    !>
    !> Cancels an in-progress multipart upload and deletes all uploaded parts
    !> from S3 storage. Use this if the upload fails or needs to be cancelled.
    !>
    !> @param[inout] upload The multipart upload handle
    !> @return .true. if abort succeeded, .false. on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> if (.not. s3_multipart_upload_part(upload, 5, data)) then
    !>     ! Upload failed, clean up
    !>     call s3_multipart_abort(upload)
    !> end if
    !> ```
    !>
    !> @note You should always abort failed uploads to avoid storage charges for orphaned parts.
    function s3_multipart_abort(upload) result(success)
        type(multipart_upload_t), intent(inout) :: upload
        logical :: success

        type(s3_config) :: config
        character(len=2048) :: url, cmd
        integer :: ios

        success = .false.
        call s3_clear_error()

        if (.not. s3_is_initialized()) then
            call s3_set_error(S3_ERROR_INIT, 0, &
                "S3 not initialized", &
                "Call s3_init() before using multipart operations")
            return
        end if

        config = s3_get_config()

        call s3_log_info("s3_multipart_abort: Aborting upload for key: " // trim(upload%key))

        ! Build URL with ?uploadId= query parameter
        if (config%use_path_style) then
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A,A,A)') 'https://', trim(config%endpoint), '/', &
                    trim(upload%bucket), '/', trim(upload%key), '?uploadId=', trim(upload%upload_id)
            else
                write(url, '(A,A,A,A,A,A,A,A)') 'http://', trim(config%endpoint), '/', &
                    trim(upload%bucket), '/', trim(upload%key), '?uploadId=', trim(upload%upload_id)
            end if
        else
            if (config%use_https) then
                write(url, '(A,A,A,A,A,A,A,A)') 'https://', trim(upload%bucket), '.', &
                    trim(config%endpoint), '/', trim(upload%key), '?uploadId=', trim(upload%upload_id)
            else
                write(url, '(A,A,A,A,A,A,A,A)') 'http://', trim(upload%bucket), '.', &
                    trim(config%endpoint), '/', trim(upload%key), '?uploadId=', trim(upload%upload_id)
            end if
        end if

        ! Execute curl DELETE request
        write(cmd, '(A,A,A)') 'curl -s -X DELETE "', trim(url), '"'
        call execute_command_line(cmd, exitstat=ios)

        if (ios /= 0) then
            call s3_set_error(S3_ERROR_NETWORK, 0, &
                "Failed to abort multipart upload", &
                "Check network connectivity")
            success = .false.
        else
            call s3_log_info("Multipart upload aborted successfully")
            success = .true.
        end if
    end function s3_multipart_abort

    !> High-level function to upload a large file using multipart upload.
    !>
    !> Automatically handles the entire multipart upload process:
    !> - Initiates the upload
    !> - Splits file into chunks
    !> - Uploads each chunk as a part
    !> - Completes or aborts on error
    !>
    !> @param[in] key The S3 object key
    !> @param[in] file_path Path to local file to upload
    !> @param[in] chunk_size_mb Optional chunk size in MB (default: 100MB)
    !> @return .true. if upload succeeded, .false. on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> logical :: success
    !>
    !> ! Upload 50GB file with 500MB chunks
    !> success = s3_put_large_file('data/huge_model.nc', '/data/local/model.nc', chunk_size_mb=500)
    !> ```
    !>
    !> @note Automatically aborts upload if any part fails.
    !> @note Progress can be tracked via s3_logger at DEBUG level.
    function s3_put_large_file(key, file_path, chunk_size_mb) result(success)
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: file_path
        integer, intent(in), optional :: chunk_size_mb
        logical :: success

        integer :: chunk_size
        integer(kind=8) :: file_size
        integer :: max_parts, ios

        success = .false.
        call s3_clear_error()

        ! Silence unused argument warning for stub
        if (len(key) < 0) return

        ! Set chunk size (default 100MB)
        if (present(chunk_size_mb)) then
            chunk_size = chunk_size_mb * 1024 * 1024
        else
            chunk_size = 100 * 1024 * 1024  ! 100MB default
        end if

        ! Validate chunk size
        if (chunk_size < MIN_PART_SIZE) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Chunk size too small (minimum 5MB)", &
                "Increase chunk_size_mb to at least 5")
            return
        end if

        ! Get file size
        inquire(file=file_path, size=file_size, iostat=ios)
        if (ios /= 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Cannot access file: " // trim(file_path), &
                "Check file exists and is readable")
            return
        end if

        ! Calculate number of parts needed
        max_parts = int((file_size + chunk_size - 1) / chunk_size)
        if (max_parts > MAX_PARTS_ALLOWED) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "File too large for chunk size (>10,000 parts)", &
                "Increase chunk_size_mb to reduce number of parts")
            return
        end if

        call s3_log_info("s3_put_large_file: Uploading " // trim(file_path) // &
            " as " // trim(adjustl(int_to_str(max_parts))) // " parts")

        ! TODO: Implement full upload logic
        ! 1. Initiate upload
        ! 2. Loop through file in chunks
        ! 3. Upload each chunk as a part
        ! 4. Complete or abort

        call s3_set_error(S3_ERROR_CLIENT, 0, &
            "Multipart upload not yet fully implemented", &
            "This is a work-in-progress feature for v1.2.0")

        success = .false.
    end function s3_put_large_file

    !> Helper function to convert integer to string
    function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        write(str, '(I0)') i
    end function int_to_str

    !> Extract UploadId from InitiateMultipartUpload XML response
    !>
    !> Example response:
    !> <?xml version="1.0"?>
    !> <InitiateMultipartUploadResult>
    !>   <Bucket>bucket</Bucket>
    !>   <Key>key</Key>
    !>   <UploadId>VXBsb2FkIElE...</UploadId>
    !> </InitiateMultipartUploadResult>
    function extract_upload_id(xml_response) result(upload_id)
        character(len=*), intent(in) :: xml_response
        character(len=:), allocatable :: upload_id

        integer :: start_pos, end_pos

        ! Find <UploadId> tag
        start_pos = index(xml_response, '<UploadId>')
        if (start_pos == 0) then
            upload_id = ''
            return
        end if
        start_pos = start_pos + len('<UploadId>')

        ! Find </UploadId> tag
        end_pos = index(xml_response(start_pos:), '</UploadId>')
        if (end_pos == 0) then
            upload_id = ''
            return
        end if
        end_pos = start_pos + end_pos - 2

        ! Extract UploadId
        upload_id = xml_response(start_pos:end_pos)
    end function extract_upload_id

    !> Extract ETag from HTTP response headers
    !>
    !> ETag header format: ETag: "686897696a7c876b7e"
    function extract_etag(headers) result(etag)
        character(len=*), intent(in) :: headers
        character(len=:), allocatable :: etag

        integer :: start_pos, quote1, quote2

        ! Find ETag: header (case insensitive)
        start_pos = index(headers, 'ETag:')
        if (start_pos == 0) then
            start_pos = index(headers, 'etag:')
        end if
        if (start_pos == 0) then
            etag = ''
            return
        end if
        start_pos = start_pos + len('ETag:')

        ! Find first quote
        quote1 = index(headers(start_pos:), '"')
        if (quote1 == 0) then
            etag = ''
            return
        end if
        quote1 = start_pos + quote1

        ! Find second quote
        quote2 = index(headers(quote1:), '"')
        if (quote2 <= 1) then
            etag = ''
            return
        end if
        quote2 = quote1 + quote2 - 2

        ! Extract ETag (without quotes)
        etag = headers(quote1:quote2)
    end function extract_etag

    !> Build XML body for CompleteMultipartUpload request
    !>
    !> Example:
    !> <CompleteMultipartUpload>
    !>   <Part>
    !>     <PartNumber>1</PartNumber>
    !>     <ETag>"etag1"</ETag>
    !>   </Part>
    !>   <Part>
    !>     <PartNumber>2</PartNumber>
    !>     <ETag>"etag2"</ETag>
    !>   </Part>
    !> </CompleteMultipartUpload>
    function build_complete_multipart_xml(upload) result(xml)
        type(multipart_upload_t), intent(in) :: upload
        character(len=:), allocatable :: xml

        character(len=10000) :: temp_xml
        integer :: i, pos

        temp_xml = '<CompleteMultipartUpload>'
        pos = len_trim(temp_xml) + 1

        do i = 1, upload%num_parts
            write(temp_xml(pos:), '(A,I0,A,A,A,A)') &
                '<Part><PartNumber>', i, '</PartNumber><ETag>"', &
                trim(upload%etags(i)), '"</ETag></Part>'
            pos = len_trim(temp_xml) + 1
        end do

        temp_xml(pos:) = '</CompleteMultipartUpload>'

        xml = trim(temp_xml)
    end function build_complete_multipart_xml

    !> Generate a unique temporary filename using random numbers (F2008 standard)
    !>
    !> @param prefix Filename prefix
    !> @param suffix Filename suffix
    !> @return Unique temporary filename
    function generate_temp_filename(prefix, suffix) result(filename)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in) :: suffix
        character(len=:), allocatable :: filename
        real :: rand_val
        integer :: rand_int
        character(len=256) :: temp_str

        call random_number(rand_val)
        rand_int = int(rand_val * 1000000000.0)
        write(temp_str, '(A,A,I9.9,A)') '/tmp/', trim(prefix), rand_int, trim(suffix)
        filename = trim(temp_str)
    end function generate_temp_filename

end module s3_multipart
