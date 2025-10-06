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
    use s3_http, only: s3_config
    use s3_logger
    use s3_errors
    use libcurl_bindings, only: is_libcurl_available
    use aws_auth, only: aws_sign_request, aws_credential_t
    use openssl_bindings, only: sha256_hash, hex_encode, is_openssl_available
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
    integer, parameter :: MAX_PART_SIZE = 5368709120     ! 5 GB (5 * 1024 * 1024 * 1024)
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

        character(len=:), allocatable :: response

        success = .false.
        call s3_clear_error()

        call s3_log_info("s3_multipart_init: Initiating multipart upload for key: " // trim(key))

        ! Allocate ETag storage
        allocate(character(len=64) :: upload%etags(MAX_PARTS_ALLOWED))
        allocate(upload%part_sizes(MAX_PARTS_ALLOWED))
        upload%etags = ''
        upload%part_sizes = 0

        ! Store key for later operations
        upload%key = trim(key)
        upload%num_parts = 0

        ! TODO: Send POST request to S3 with ?uploads
        ! TODO: Parse XML response to extract UploadId
        ! For now, return false as not implemented
        call s3_set_error(S3_ERROR_CLIENT, 0, &
            "Multipart upload not yet fully implemented", &
            "This is a work-in-progress feature for v1.2.0")

        success = .false.
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

        integer :: data_len

        success = .false.
        call s3_clear_error()

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

        ! TODO: Send PUT request with ?partNumber=X&uploadId=Y
        ! TODO: Extract ETag from response headers
        ! TODO: Store ETag in upload%etags(part_number)

        call s3_set_error(S3_ERROR_CLIENT, 0, &
            "Multipart upload not yet fully implemented", &
            "This is a work-in-progress feature for v1.2.0")

        success = .false.
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

        success = .false.
        call s3_clear_error()

        call s3_log_debug("s3_multipart_upload_part_from_file: Part " // &
            trim(adjustl(int_to_str(part_number))) // " from file")

        ! TODO: Open file, seek to offset, read length bytes, upload
        call s3_set_error(S3_ERROR_CLIENT, 0, &
            "Multipart upload not yet fully implemented", &
            "This is a work-in-progress feature for v1.2.0")

        success = .false.
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

        character(len=:), allocatable :: xml_body

        success = .false.
        call s3_clear_error()

        if (upload%num_parts == 0) then
            call s3_set_error(S3_ERROR_CLIENT, 0, &
                "Cannot complete upload with zero parts", &
                "Upload at least one part before calling s3_multipart_complete()")
            return
        end if

        call s3_log_info("s3_multipart_complete: Completing upload with " // &
            trim(adjustl(int_to_str(upload%num_parts))) // " parts")

        ! TODO: Build XML body with part numbers and ETags
        ! TODO: Send POST request with ?uploadId=X

        call s3_set_error(S3_ERROR_CLIENT, 0, &
            "Multipart upload not yet fully implemented", &
            "This is a work-in-progress feature for v1.2.0")

        success = .false.
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

        success = .false.
        call s3_clear_error()

        call s3_log_info("s3_multipart_abort: Aborting upload for key: " // trim(upload%key))

        ! TODO: Send DELETE request with ?uploadId=X

        call s3_set_error(S3_ERROR_CLIENT, 0, &
            "Multipart upload not yet fully implemented", &
            "This is a work-in-progress feature for v1.2.0")

        success = .false.
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

        integer :: chunk_size, actual_chunk_size
        integer(kind=8) :: file_size, offset, bytes_remaining
        integer :: part_number, max_parts
        type(multipart_upload_t) :: upload
        integer :: unit, ios

        success = .false.
        call s3_clear_error()

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

end module s3_multipart
