!> Core S3 HTTP operations module providing direct access to S3-compatible object storage.
!>
!> This module provides low-level HTTP-based operations for interacting with S3-compatible
!> object storage services. It uses system curl commands to perform GET, PUT, DELETE, and
!> HEAD HTTP operations.
!>
!> ## Features
!>
!> - Direct S3 operations via curl HTTP requests
!> - Support for both authenticated and public bucket access
!> - URI-based operations with s3:// protocol support
!> - HTTP and HTTPS protocol support
!> - Configurable endpoints for S3-compatible services
!>
!> ## Usage
!>
!> ```fortran
!> use s3_http
!> type(s3_config) :: config
!> character(len=:), allocatable :: content
!> logical :: success
!>
!> ! Configure and initialize
!> config%bucket = 'my-bucket'
!> config%region = 'us-east-1'
!> config%use_https = .true.
!> call s3_init(config)
!>
!> ! Download object
!> success = s3_get_object('data/file.txt', content)
!> ```
!>
!> @note This module requires the `curl` command to be available in the system PATH.
!> @warning URL encoding of special characters in S3 keys is not currently supported.
module s3_http
    use curl_stream, only: stream_command_output, is_streaming_available
    use s3_logger
    use s3_errors
    use libcurl_bindings, only: is_libcurl_available, curl_get_to_buffer, &
                                curl_get_to_buffer_with_progress, curl_get_to_buffer_with_headers, &
                                curl_get_to_buffer_with_range, &
                                curl_buffer_t, curl_progress_callback, curl_get_http_status
    use aws_auth, only: aws_sign_request, aws_credential_t
    use openssl_bindings, only: sha256_hash, hex_encode, is_openssl_available
    implicit none
    private

    !> S3 configuration type containing connection parameters and credentials.
    !>
    !> This type holds all configuration needed to connect to an S3-compatible
    !> object storage service. For public buckets, credentials can be left empty.
    !>
    !> ## Example
    !>
    !> ```fortran
    !> type(s3_config) :: config
    !> config%bucket = 'noaa-gfs-bdp-pds'
    !> config%region = 'us-east-1'
    !> config%endpoint = 's3.amazonaws.com'
    !> config%use_https = .true.
    !> config%access_key = ''  ! Empty for public bucket
    !> config%secret_key = ''
    !> ```
    type, public :: s3_config
        character(len=256) :: bucket = ''        !< S3 bucket name
        character(len=256) :: region = 'us-east-1'  !< AWS region (default: us-east-1)
        character(len=256) :: endpoint = 's3.amazonaws.com'  !< S3 endpoint hostname
        character(len=256) :: access_key = ''    !< AWS access key ID (optional for public buckets)
        character(len=256) :: secret_key = ''    !< AWS secret access key (optional for public buckets)
        logical :: use_https = .true.            !< Use HTTPS protocol (recommended)
        logical :: use_path_style = .false.      !< Use path-style URLs (required for MinIO/localhost)
    end type s3_config

    ! Module variables
    type(s3_config), save :: current_config
    logical, save :: initialized = .false.

    ! Public procedures
    public :: s3_init
    public :: s3_get_object
    public :: s3_get_object_range
    public :: s3_put_object
    public :: s3_object_exists
    public :: s3_delete_object
    ! s3:// URI functions
    public :: s3_get_uri
    public :: s3_put_uri
    public :: s3_exists_uri
    public :: s3_delete_uri
    ! Configuration access
    public :: s3_get_config
    public :: s3_is_initialized
    ! Progress callback interface (re-exported from libcurl_bindings)
    public :: curl_progress_callback
    ! Error handling (re-exported from s3_errors)
    public :: s3_error_t
    public :: s3_get_last_error
    public :: s3_clear_error
    public :: s3_has_error

contains

    !> Parse an s3:// URI into bucket name and object key components.
    !>
    !> Parses URIs of the format `s3://bucket-name/path/to/object` into
    !> separate bucket and key strings for use with S3 operations.
    !>
    !> @param[in] uri The s3:// URI to parse
    !> @param[out] bucket The extracted bucket name (allocatable)
    !> @param[out] key The extracted object key/path (allocatable)
    !> @param[out] success .true. if URI was successfully parsed, .false. otherwise
    !>
    !> ## Examples
    !>
    !> ```fortran
    !> ! Parse URI with bucket and key
    !> call parse_s3_uri('s3://my-bucket/data/file.txt', bucket, key, success)
    !> ! Result: bucket='my-bucket', key='data/file.txt'
    !>
    !> ! Parse URI with only bucket
    !> call parse_s3_uri('s3://my-bucket', bucket, key, success)
    !> ! Result: bucket='my-bucket', key=''
    !> ```
    subroutine parse_s3_uri(uri, bucket, key, success)
        character(len=*), intent(in) :: uri
        character(len=:), allocatable, intent(out) :: bucket
        character(len=:), allocatable, intent(out) :: key
        logical, intent(out) :: success
        integer :: bucket_start, bucket_end, key_start

        success = .false.

        ! Check for s3:// prefix
        if (len(uri) < 6) return
        if (uri(1:5) /= 's3://') return

        ! Find bucket name (between s3:// and next /)
        bucket_start = 6
        bucket_end = index(uri(bucket_start:), '/') + bucket_start - 2

        if (bucket_end < bucket_start) then
            ! No key, just bucket
            bucket = uri(bucket_start:)
            key = ''
            success = .true.
            return
        end if

        ! Extract bucket and key
        bucket = uri(bucket_start:bucket_end)
        key_start = bucket_end + 2

        if (key_start <= len(uri)) then
            key = uri(key_start:)
        else
            key = ''
        end if

        success = .true.
    end subroutine parse_s3_uri

    !> Initialize the S3 HTTP module with configuration.
    !>
    !> This subroutine must be called before any S3 operations can be performed.
    !> It stores the provided configuration for use by all subsequent operations.
    !>
    !> @param[in] config S3 configuration containing bucket, endpoint, and credentials
    !>
    !> ## Example
    !>
    !> ```fortran
    !> type(s3_config) :: config
    !> config%bucket = 'my-bucket'
    !> config%region = 'us-east-1'
    !> config%use_https = .true.
    !> call s3_init(config)
    !> ```
    subroutine s3_init(config)
        type(s3_config), intent(in) :: config
        character(len=256) :: msg
        logical :: has_access_key, has_secret_key

        ! Initialize logger from environment
        call s3_init_logger()

        ! Clear any previous errors
        call s3_clear_error()

        current_config = config
        initialized = .true.

        call s3_log_info('S3 library initialized')
        write(msg, '(A,A)') 'Bucket: ', trim(config%bucket)
        call s3_log_debug(trim(msg))
        write(msg, '(A,A)') 'Endpoint: ', trim(config%endpoint)
        call s3_log_debug(trim(msg))
        write(msg, '(A,A)') 'Region: ', trim(config%region)
        call s3_log_debug(trim(msg))
        write(msg, '(A,L1)') 'HTTPS: ', config%use_https
        call s3_log_debug(trim(msg))
        write(msg, '(A,L1)') 'Streaming available: ', is_streaming_available()
        call s3_log_info(trim(msg))

        ! Validate authentication configuration
        has_access_key = len_trim(config%access_key) > 0
        has_secret_key = len_trim(config%secret_key) > 0

        if (has_access_key .and. .not. has_secret_key) then
            call s3_log_warn("Access key provided but secret key is missing - authentication disabled")
        else if (has_secret_key .and. .not. has_access_key) then
            call s3_log_warn("Secret key provided but access key is missing - authentication disabled")
        else if (has_access_key .and. has_secret_key) then
            if (is_openssl_available()) then
                call s3_log_info("AWS Signature v4 authentication enabled")
                if (len_trim(config%access_key) < 16) then
                    call s3_log_warn("Access key seems too short - AWS keys are typically 20 characters")
                end if
            else
                call s3_log_warn("Credentials provided but OpenSSL not available - authentication disabled")
            end if
        else
            call s3_log_debug("No credentials provided - using unauthenticated requests")
        end if
    end subroutine s3_init

    !> Download an object from S3 and return its content.
    !>
    !> Downloads the specified object from S3 using an HTTP GET request via curl.
    !> The content is returned as an allocatable string. Works with both public
    !> and authenticated buckets.
    !>
    !> @param[in] key The S3 object key (path within the bucket)
    !> @param[out] content The downloaded content as an allocatable string
    !> @return .true. if download succeeded, .false. on error
    !>
    !> @note The module must be initialized with s3_init() before calling this function.
    !> @warning Returns .false. if the module is not initialized or if the download fails.
    !>
    !> ## Example
    !>
    !> ```fortran
    !> character(len=:), allocatable :: content
    !> logical :: success
    !>
    !> success = s3_get_object('data/input.txt', content)
    !> if (success) then
    !>     print *, 'Downloaded: ', len(content), ' bytes'
    !>     print *, content
    !> else
    !>     print *, 'Download failed'
    !> end if
    !> ```
    function s3_get_object(key, content, progress_callback, byte_start, byte_end) result(success)
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: content
        procedure(curl_progress_callback), optional :: progress_callback
        integer(kind=8), intent(in), optional :: byte_start, byte_end
        logical :: success
        character(len=2048) :: url
        character(len=4096) :: cmd
        integer :: exit_status
        character(len=2048) :: msg

        success = .false.

        ! Clear previous error
        call s3_clear_error()

        if (.not. initialized) then
            call s3_set_error(S3_ERROR_INIT, 0, "s3_get_object called before s3_init()", &
                             "Call s3_init() with configuration before using S3 operations")
            return
        end if

        ! Log key (truncated if too long for buffer)
        if (len_trim(key) <= 2030) then  ! "Getting object: " is 17 chars
            write(msg, '(A,A)') 'Getting object: ', trim(key)
        else
            write(msg, '(A,A,A)') 'Getting object: ', key(1:2020), '...'
        end if
        call s3_log_info(trim(msg))

        ! Log byte range if specified
        if (present(byte_start) .and. present(byte_end)) then
            write(msg, '(A,I0,A,I0)') 'Requesting bytes ', byte_start, '-', byte_end
            call s3_log_info(trim(msg))
        end if

        ! Build URL - support both virtual-host and path-style
        if (current_config%use_path_style) then
            ! Path-style: http://endpoint/bucket/key
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            end if
        else
            ! Virtual-host style: http://bucket.endpoint/key (original behavior)
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            end if
        end if

        write(msg, '(A,A)') 'URL: ', trim(url)
        call s3_log_debug(trim(msg))

        ! Try libcurl first (fastest, cross-platform)
        if (is_libcurl_available()) then
            call s3_log_debug('Attempting native libcurl')
            success = s3_get_object_libcurl(trim(url), content, progress_callback, byte_start, byte_end)
            if (success) then
                write(msg, '(A,I0,A)') 'libcurl successful, received ', len(content), ' bytes'
                call s3_log_debug(trim(msg))
                ! Check for S3 error in response
                if (index(content, '<Error>') > 0) then
                    call s3_log_error('S3 error detected in response')
                    if (len(content) < 500) then
                        call s3_log_debug('Response: ' // content)
                    else
                        call s3_log_debug('Response (first 500 chars): ' // content(1:500))
                    end if
                    success = .false.
                end if
                return
            else
                call s3_log_warn('libcurl failed, trying fallback methods')
            end if
        end if

        ! Build curl command for subprocess methods
        if (present(byte_start) .and. present(byte_end)) then
            ! Include byte range in curl command
            write(cmd, '(A,I0,A,I0,A,A,A)') 'curl -s -r ', byte_start, '-', byte_end, ' "', trim(url), '"'
        else
            ! No byte range
            write(cmd, '(A,A,A)') 'curl -s "', trim(url), '"'
        end if
        write(msg, '(A,A)') 'Command: ', trim(cmd)
        call s3_log_trace(trim(msg))

        ! Try popen streaming (if available)
        if (is_streaming_available()) then
            call s3_log_debug('Attempting popen() streaming')
            ! Use direct streaming (no disk I/O)
            success = stream_command_output(trim(cmd), content, exit_status)
            if (success .and. exit_status == 0) then
                write(msg, '(A,I0,A)') 'Streaming successful, received ', len(content), ' bytes'
                call s3_log_debug(trim(msg))
                ! Check for S3 error in response
                if (index(content, '<Error>') > 0) then
                    call s3_log_error('S3 error detected in response')
                    if (len(content) < 500) then
                        call s3_log_debug('Response: ' // content)
                    else
                        call s3_log_debug('Response (first 500 chars): ' // content(1:500))
                    end if
                    success = .false.
                end if
                return
            else
                write(msg, '(A,I0)') 'popen() failed with exit status: ', exit_status
                call s3_log_warn(trim(msg))
            end if
        else
            call s3_log_info('popen() not available')
        end if

        ! Fallback to temp file method (last resort)
        call s3_log_debug('Falling back to temp file method')
        success = s3_get_object_fallback(key, content, byte_start, byte_end)

    end function s3_get_object

    !> libcurl implementation for GET requests.
    !>
    !> Uses native libcurl bindings for maximum performance.
    !>
    !> @param[in] url The complete URL to fetch
    !> @param[out] content The downloaded content
    !> @param[in] progress_callback Optional progress callback function
    !> @param[in] byte_start Optional starting byte for range request
    !> @param[in] byte_end Optional ending byte for range request
    !> @return .true. if successful, .false. otherwise
    function s3_get_object_libcurl(url, content, progress_callback, byte_start, byte_end) result(success)
        character(len=*), intent(in) :: url
        character(len=:), allocatable, intent(out) :: content
        procedure(curl_progress_callback), optional :: progress_callback
        integer(kind=8), intent(in), optional :: byte_start, byte_end
        logical :: success
        type(curl_buffer_t) :: buffer
        logical :: use_auth
        character(len=:), allocatable :: auth_header, timestamp, host, uri
        character(len=:), allocatable :: range_value  ! "bytes=start-end"
        character(len=64) :: range_str  ! Buffer for building range string
        character(len=512), dimension(:), allocatable :: headers_array  ! Dynamic headers
        integer :: num_headers  ! Counter for header array
        character(len=1024) :: msg  ! Buffer for log messages
        type(aws_credential_t) :: creds
        character(len=64) :: payload_hash  ! Empty SHA256

        ! Allocate headers array (max 4 headers: Host, X-Amz-Date, Range, Authorization)
        allocate(character(len=512) :: headers_array(4))

        ! Check if authentication is required and available
        use_auth = len_trim(current_config%access_key) > 0 .and. &
                   len_trim(current_config%secret_key) > 0 .and. &
                   is_openssl_available()

        ! Warn if credentials provided but OpenSSL unavailable
        if (len_trim(current_config%access_key) > 0 .and. &
            len_trim(current_config%secret_key) > 0 .and. &
            .not. is_openssl_available()) then
            call s3_log_warn("AWS credentials provided but OpenSSL not available - " // &
                            "continuing with unauthenticated request")
        end if

        ! Build range string if byte range is requested
        if (present(byte_start) .and. present(byte_end)) then
            write(range_str, '(A,I0,A,I0)') 'bytes=', byte_start, '-', byte_end
            allocate(character(len=len_trim(range_str)) :: range_value)
            range_value = trim(range_str)
            call s3_log_debug("Using byte-range request: " // trim(range_value))
        end if

        if (use_auth) then
            call s3_log_debug("AWS Signature v4 authentication enabled")
            ! Generate timestamp (ISO8601 format: YYYYMMDDTHHMMSSZ)
            call get_iso8601_timestamp(timestamp)

            ! Extract host and URI from URL
            call parse_url(url, host, uri)

            ! Hash of empty payload for GET request
            payload_hash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

            ! Setup credentials
            allocate(character(len=len_trim(current_config%access_key)) :: creds%access_key)
            allocate(character(len=len_trim(current_config%secret_key)) :: creds%secret_key)
            allocate(character(len=len_trim(current_config%region)) :: creds%region)
            allocate(character(len=2) :: creds%service)
            creds%access_key = trim(current_config%access_key)
            creds%secret_key = trim(current_config%secret_key)
            creds%region = trim(current_config%region)
            creds%service = "s3"

            ! Generate authorization header (with optional range header for signature)
            if (allocated(range_value)) then
                if (.not. aws_sign_request(creds, "GET", host, uri, "", payload_hash, &
                                          timestamp, auth_header, range_value)) then
                    call s3_log_error("Failed to generate AWS signature - check credentials and region")
                    success = .false.
                    return
                end if
            else
                if (.not. aws_sign_request(creds, "GET", host, uri, "", payload_hash, &
                                          timestamp, auth_header)) then
                    call s3_log_error("Failed to generate AWS signature - check credentials and region")
                    success = .false.
                    return
                end if
            end if

            ! Build HTTP headers
            num_headers = 0

            num_headers = num_headers + 1
            headers_array(num_headers) = "Host: " // trim(host)

            num_headers = num_headers + 1
            headers_array(num_headers) = "X-Amz-Date: " // trim(timestamp)

            ! Include Range header if present
            if (allocated(range_value)) then
                num_headers = num_headers + 1
                headers_array(num_headers) = "Range: " // trim(range_value)
                write(msg, '(A,A)') 'Adding Range header to request: ', trim(range_value)
                call s3_log_debug(trim(msg))
            end if

            num_headers = num_headers + 1
            headers_array(num_headers) = "Authorization: " // trim(auth_header)

            call s3_log_debug("Signing request with access key: " // creds%access_key(1:min(12, len(creds%access_key))) // "...")
            call s3_log_debug("Region: " // trim(creds%region))
            success = curl_get_to_buffer_with_headers(url, buffer, headers_array(1:num_headers))
        else
            ! No authentication - use dedicated range or regular request
            if (allocated(range_value)) then
                ! Use libcurl's built-in range support for unauthenticated requests
                success = curl_get_to_buffer_with_range(url, buffer, byte_start, byte_end)
            else if (present(progress_callback)) then
                success = curl_get_to_buffer_with_progress(url, buffer, progress_callback)
            else
                success = curl_get_to_buffer(url, buffer)
            end if
        end if

        if (success .and. allocated(buffer%data)) then
            content = buffer%data

            ! Check for S3 errors even on successful HTTP status
            ! For range requests, HTTP 206 is expected
            if (allocated(range_value)) then
                if (buffer%http_status == 206) then
                    write(msg, '(A,I0,A)') 'Range request successful (HTTP 206), received ', &
                                          len(content), ' bytes'
                    call s3_log_debug(trim(msg))
                else if (buffer%http_status == 200) then
                    write(msg, '(A,I0,A)') 'Range request returned HTTP 200 (server ignored range), received ', &
                                          len(content), ' bytes'
                    call s3_log_warn(trim(msg))
                else if (buffer%http_status >= 400 .or. index(content, '<Error>') > 0) then
                    call s3_set_error_from_response(buffer%http_status, content, "GET", "s3://" // &
                                                    trim(current_config%bucket) // "/" // trim(uri))
                    success = .false.
                end if
            else
                if (buffer%http_status >= 400 .or. index(content, '<Error>') > 0) then
                    ! Create detailed error from response
                    call s3_set_error_from_response(buffer%http_status, content, "GET", "s3://" // &
                                                    trim(current_config%bucket) // "/" // trim(uri))
                    success = .false.
                end if
            end if
        else
            ! Network or curl error
            if (buffer%http_status > 0) then
                ! HTTP error with response body
                if (allocated(buffer%data)) then
                    call s3_set_error_from_response(buffer%http_status, buffer%data, "GET", &
                                                    "s3://" // trim(current_config%bucket))
                else
                    call s3_set_error_from_response(buffer%http_status, "", "GET", &
                                                    "s3://" // trim(current_config%bucket))
                end if
            else
                ! Network error (no HTTP response)
                call s3_set_error(S3_ERROR_NETWORK, 0, "Failed to connect to S3", &
                                 "Check network connectivity and endpoint URL")
            end if
            success = .false.
        end if

    end function s3_get_object_libcurl

    !> Download a specific byte range from an S3 object.
    !>
    !> This is a convenience wrapper around s3_get_object() specifically for byte-range requests.
    !> Downloads only the specified byte range from an S3 object, which is useful for reading
    !> specific portions of large files without downloading the entire file.
    !>
    !> @param[in] key The S3 object key (path within the bucket)
    !> @param[out] content The downloaded content (only the requested byte range)
    !> @param[in] byte_start Starting byte position (0-indexed, inclusive)
    !> @param[in] byte_end Ending byte position (0-indexed, inclusive)
    !> @return .true. if download succeeded, .false. on error
    !>
    !> @note Requires server support for HTTP Range requests (most S3 services support this)
    !> @note Byte positions are 0-indexed (first byte is 0, not 1)
    !> @note Both start and end positions are inclusive
    !> @note Server may respond with HTTP 206 (Partial Content) for successful range requests
    !> @note Server may respond with HTTP 200 (OK) and send full content if range not supported
    !>
    !> ## Examples
    !>
    !> ```fortran
    !> character(len=:), allocatable :: content
    !> logical :: success
    !>
    !> ! Download first 1KB of a file
    !> success = s3_get_object_range('data/large_file.bin', content, 0_8, 1023_8)
    !> if (success) then
    !>     print *, 'Downloaded first 1KB: ', len(content), ' bytes'
    !> end if
    !>
    !> ! Download bytes 1000-2000 from the middle of a file
    !> success = s3_get_object_range('data/large_file.bin', content, 1000_8, 2000_8)
    !> if (success) then
    !>     print *, 'Downloaded 1001 bytes from middle of file'
    !> end if
    !>
    !> ! Download last 500 bytes (if you know file size is 10000)
    !> success = s3_get_object_range('data/file.dat', content, 9500_8, 9999_8)
    !> ```
    function s3_get_object_range(key, content, byte_start, byte_end) result(success)
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: content
        integer(kind=8), intent(in) :: byte_start, byte_end
        logical :: success

        ! Simply call s3_get_object with byte range parameters
        success = s3_get_object(key, content, byte_start=byte_start, byte_end=byte_end)

    end function s3_get_object_range

    !> Set error from HTTP response (helper function).
    !>
    !> Parses HTTP response and creates detailed error object.
    !>
    !> @param[in] http_status HTTP status code
    !> @param[in] response_body Response body (may contain XML error)
    !> @param[in] operation Operation name (GET, PUT, etc.)
    !> @param[in] resource Resource path
    subroutine s3_set_error_from_response(http_status, response_body, operation, resource)
        integer, intent(in) :: http_status
        character(len=*), intent(in) :: response_body
        character(len=*), intent(in) :: operation
        character(len=*), intent(in) :: resource

        type(s3_error_t) :: error

        ! Create comprehensive error
        error = s3_create_error_from_response(http_status, response_body, operation, resource)

        ! Set as last error
        call s3_set_error(error%code, error%http_status, error%message, &
                         error%suggestion, error%aws_error_code, error%operation, error%resource)
    end subroutine s3_set_error_from_response

    !> Fallback implementation using temporary files.
    !>
    !> Used on platforms without popen support (Windows) or if streaming fails.
    !>
    !> @param[in] key The S3 object key
    !> @param[out] content The downloaded content
    !> @param[in] byte_start Optional starting byte for range request
    !> @param[in] byte_end Optional ending byte for range request
    !> @return .true. if successful, .false. otherwise
    function s3_get_object_fallback(key, content, byte_start, byte_end) result(success)
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: content
        integer(kind=8), intent(in), optional :: byte_start, byte_end
        logical :: success
        character(len=2048) :: url
        character(len=4096) :: cmd
        character(len=256) :: tmpfile
        integer :: unit, ios, filesize
        character(len=1) :: byte
        integer :: i

        success = .false.

        ! Build URL - support both virtual-host and path-style
        if (current_config%use_path_style) then
            ! Path-style: http://endpoint/bucket/key
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            end if
        else
            ! Virtual-host style: http://bucket.endpoint/key (original behavior)
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            end if
        end if

        ! Create temp file name
        write(tmpfile, '(A,I0,A)') '/tmp/s3_get_', getpid(), '.tmp'

        ! Build curl command with optional byte range
        if (present(byte_start) .and. present(byte_end)) then
            ! Include byte range in curl command
            write(cmd, '(A,I0,A,I0,A,A,A,A,A)') 'curl -s -r ', byte_start, '-', byte_end, &
                ' -o ', trim(tmpfile), ' "', trim(url), '"'
        else
            ! No byte range
            write(cmd, '(A,A,A,A,A)') 'curl -s -o ', trim(tmpfile), ' "', trim(url), '"'
        end if

        ! Execute curl
        call execute_command_line(cmd, exitstat=ios)
        if (ios /= 0) return

        ! Read the downloaded file
        inquire(file=tmpfile, size=filesize, iostat=ios)
        if (ios /= 0) return

        allocate(character(len=filesize) :: content)

        open(newunit=unit, file=tmpfile, access='stream', &
             form='unformatted', status='old', iostat=ios)
        if (ios /= 0) then
            deallocate(content)
            return
        end if

        do i = 1, filesize
            read(unit, iostat=ios) byte
            if (ios /= 0) exit
            content(i:i) = byte
        end do

        close(unit)

        ! Clean up temp file
        write(cmd, '(A,A)') 'rm -f ', trim(tmpfile)
        call execute_command_line(cmd)

        success = (ios == 0 .and. index(content, '<Error>') == 0)
    end function s3_get_object_fallback

    !> Upload an object to S3.
    !>
    !> Uploads the provided content to S3 at the specified key using an HTTP PUT request.
    !> This operation requires AWS credentials to be configured in the s3_config.
    !>
    !> @param[in] key The S3 object key (path within the bucket) where content will be stored
    !> @param[in] content The content to upload as a string
    !> @return .true. if upload succeeded, .false. on error
    !>
    !> @note Requires AWS credentials (access_key and secret_key) to be set in configuration.
    !> @warning Returns .false. if credentials are missing or upload fails.
    !> @warning Current implementation uses simplified authentication; production use requires AWS Signature v4.
    !>
    !> ## Example
    !>
    !> ```fortran
    !> type(s3_config) :: config
    !> logical :: success
    !>
    !> config%bucket = 'my-bucket'
    !> config%access_key = 'AKIAIOSFODNN7EXAMPLE'
    !> config%secret_key = 'wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY'
    !> call s3_init(config)
    !>
    !> success = s3_put_object('results/output.txt', 'Hello S3!')
    !> ```
    function s3_put_object(key, content) result(success)
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: content
        logical :: success
        character(len=2048) :: url
        character(len=4096) :: cmd
        character(len=256) :: tmpfile
        integer :: unit, ios

        success = .false.

        ! Clear previous error
        call s3_clear_error()

        if (.not. initialized) then
            call s3_set_error(S3_ERROR_INIT, 0, "s3_put_object called before s3_init()", &
                             "Call s3_init() with configuration before using S3 operations")
            return
        end if

        ! For public buckets without auth, PUT won't work
        ! This is a simplified version - real implementation needs AWS signature
        if (len_trim(current_config%access_key) == 0) then
            call s3_set_error(S3_ERROR_AUTH, 0, "PUT requires AWS credentials", &
                             "Configure access_key and secret_key in s3_config for write operations")
            return
        end if

        ! Build URL - support both virtual-host and path-style
        if (current_config%use_path_style) then
            ! Path-style: http://endpoint/bucket/key
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            end if
        else
            ! Virtual-host style: http://bucket.endpoint/key (original behavior)
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            end if
        end if

        ! Create temp file with content
        write(tmpfile, '(A,I0,A)') '/tmp/s3_put_', getpid(), '.tmp'

        open(newunit=unit, file=tmpfile, status='replace', iostat=ios)
        if (ios /= 0) return

        write(unit, '(A)', iostat=ios) trim(content)
        close(unit)

        ! Build curl command for PUT (simplified - needs AWS v4 signature in reality)
        write(cmd, '(A,A,A,A,A)') 'curl -s -X PUT --data-binary @', &
            trim(tmpfile), ' "', trim(url), '"'

        ! Execute curl
        call execute_command_line(cmd, exitstat=ios)

        ! Clean up temp file
        write(cmd, '(A,A)') 'rm -f ', trim(tmpfile)
        call execute_command_line(cmd)

        success = (ios == 0)
    end function s3_put_object

    !> Check if an object exists in S3.
    !>
    !> Performs an HTTP HEAD request to check if an object exists without downloading it.
    !> This is more efficient than attempting a GET request when you only need to verify existence.
    !>
    !> @param[in] key The S3 object key to check
    !> @return .true. if object exists, .false. if not found or on error
    !>
    !> @note The module must be initialized with s3_init() before calling this function.
    !>
    !> ## Example
    !>
    !> ```fortran
    !> logical :: exists
    !>
    !> exists = s3_object_exists('data/input.nc')
    !> if (exists) then
    !>     print *, 'File found, proceeding with download'
    !> else
    !>     print *, 'File not found, using defaults'
    !> end if
    !> ```
    function s3_object_exists(key) result(exists)
        character(len=*), intent(in) :: key
        logical :: exists
        character(len=2048) :: url
        character(len=4096) :: cmd
        integer :: ios

        exists = .false.

        ! Clear previous error
        call s3_clear_error()

        if (.not. initialized) then
            call s3_set_error(S3_ERROR_INIT, 0, "s3_object_exists called before s3_init()", &
                             "Call s3_init() with configuration before using S3 operations")
            return
        end if

        ! Build URL - support both virtual-host and path-style
        if (current_config%use_path_style) then
            ! Path-style: http://endpoint/bucket/key
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            end if
        else
            ! Virtual-host style: http://bucket.endpoint/key (original behavior)
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            end if
        end if

        ! Use curl HEAD request to check existence
        write(cmd, '(A,A,A)') 'curl -s -I "', trim(url), '" | grep "HTTP" | grep -q "200 OK"'

        call execute_command_line(cmd, exitstat=ios)
        exists = (ios == 0)
    end function s3_object_exists

    !> Delete an object from S3.
    !>
    !> Deletes the specified object from S3 using an HTTP DELETE request.
    !> This operation requires AWS credentials to be configured.
    !>
    !> @param[in] key The S3 object key to delete
    !> @return .true. if deletion succeeded, .false. on error
    !>
    !> @note Requires AWS credentials (access_key and secret_key) to be set in configuration.
    !> @warning This operation is irreversible. Deleted objects cannot be recovered.
    !> @warning Returns .false. if credentials are missing or deletion fails.
    !>
    !> ## Example
    !>
    !> ```fortran
    !> logical :: success
    !>
    !> success = s3_delete_object('temp/scratch_data.txt')
    !> if (success) then
    !>     print *, 'Object deleted successfully'
    !> else
    !>     print *, 'Deletion failed'
    !> end if
    !> ```
    function s3_delete_object(key) result(success)
        character(len=*), intent(in) :: key
        logical :: success
        character(len=2048) :: url
        character(len=4096) :: cmd
        integer :: ios

        success = .false.

        ! Clear previous error
        call s3_clear_error()

        if (.not. initialized) then
            call s3_set_error(S3_ERROR_INIT, 0, "s3_delete_object called before s3_init()", &
                             "Call s3_init() with configuration before using S3 operations")
            return
        end if

        ! For public buckets without auth, DELETE won't work
        if (len_trim(current_config%access_key) == 0) then
            call s3_set_error(S3_ERROR_AUTH, 0, "DELETE requires AWS credentials", &
                             "Configure access_key and secret_key in s3_config for delete operations")
            return
        end if

        ! Build URL - support both virtual-host and path-style
        if (current_config%use_path_style) then
            ! Path-style: http://endpoint/bucket/key
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%endpoint), '/', &
                    trim(current_config%bucket), '/', &
                    trim(key)
            end if
        else
            ! Virtual-host style: http://bucket.endpoint/key (original behavior)
            if (current_config%use_https) then
                write(url, '(A,A,A,A,A,A)') 'https://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            else
                write(url, '(A,A,A,A,A,A)') 'http://', &
                    trim(current_config%bucket), '.', &
                    trim(current_config%endpoint), '/', &
                    trim(key)
            end if
        end if

        ! Build curl command for DELETE
        write(cmd, '(A,A,A)') 'curl -s -X DELETE "', trim(url), '"'

        call execute_command_line(cmd, exitstat=ios)
        success = (ios == 0)
    end function s3_delete_object

    ! Helper to get process ID
    function getpid() result(pid)
        integer :: pid
        real :: rand_val
        pid = 1  ! Simplified - would use actual getpid() C function
        call random_number(rand_val)
        pid = abs(int(rand_val * 100000))
    end function getpid

    !> Download an object using an s3:// URI.
    !>
    !> Convenience function that accepts s3:// URIs and automatically extracts the bucket
    !> name and object key. If the bucket differs from the current configuration, it
    !> temporarily switches to that bucket for the operation.
    !>
    !> @param[in] uri The s3:// URI (e.g., 's3://bucket-name/path/to/object')
    !> @param[out] content The downloaded content as an allocatable string
    !> @return .true. if download succeeded, .false. on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> character(len=:), allocatable :: content
    !> logical :: success
    !>
    !> ! Download from different bucket using URI
    !> success = s3_get_uri('s3://other-bucket/data/file.txt', content)
    !> ```
    function s3_get_uri(uri, content, progress_callback) result(success)
        character(len=*), intent(in) :: uri
        character(len=:), allocatable, intent(out) :: content
        procedure(curl_progress_callback), optional :: progress_callback
        logical :: success
        character(len=:), allocatable :: bucket, key
        type(s3_config) :: temp_config
        logical :: uri_parsed

        success = .false.

        ! Try to parse as s3:// URI
        call parse_s3_uri(uri, bucket, key, uri_parsed)
        if (.not. uri_parsed) then
            ! Not a s3:// URI, treat as regular key with current config
            success = s3_get_object(uri, content, progress_callback)
            return
        end if

        ! Use parsed bucket if different from current config
        if (allocated(bucket) .and. len_trim(bucket) > 0) then
            temp_config = current_config
            temp_config%bucket = bucket
            call s3_init(temp_config)
            success = s3_get_object(key, content, progress_callback)
            ! Restore original config
            call s3_init(current_config)
        else
            success = s3_get_object(key, content, progress_callback)
        end if
    end function s3_get_uri

    ! Check if object exists using s3:// URI
    function s3_exists_uri(uri) result(exists)
        character(len=*), intent(in) :: uri
        logical :: exists
        character(len=:), allocatable :: bucket, key
        type(s3_config) :: temp_config
        logical :: uri_parsed

        exists = .false.

        ! Try to parse as s3:// URI
        call parse_s3_uri(uri, bucket, key, uri_parsed)
        if (.not. uri_parsed) then
            ! Not a s3:// URI, treat as regular key
            exists = s3_object_exists(uri)
            return
        end if

        ! Use parsed bucket if different from current config
        if (allocated(bucket) .and. len_trim(bucket) > 0) then
            temp_config = current_config
            temp_config%bucket = bucket
            call s3_init(temp_config)
            exists = s3_object_exists(key)
            ! Restore original config
            call s3_init(current_config)
        else
            exists = s3_object_exists(key)
        end if
    end function s3_exists_uri

    ! Put an object using s3:// URI
    function s3_put_uri(uri, content) result(success)
        character(len=*), intent(in) :: uri
        character(len=*), intent(in) :: content
        logical :: success
        character(len=:), allocatable :: bucket, key
        type(s3_config) :: temp_config
        logical :: uri_parsed

        success = .false.

        ! Try to parse as s3:// URI
        call parse_s3_uri(uri, bucket, key, uri_parsed)
        if (.not. uri_parsed) then
            ! Not a s3:// URI, treat as regular key
            success = s3_put_object(uri, content)
            return
        end if

        ! Use parsed bucket if different from current config
        if (allocated(bucket) .and. len_trim(bucket) > 0) then
            temp_config = current_config
            temp_config%bucket = bucket
            call s3_init(temp_config)
            success = s3_put_object(key, content)
            ! Restore original config
            call s3_init(current_config)
        else
            success = s3_put_object(key, content)
        end if
    end function s3_put_uri

    ! Delete an object using s3:// URI
    function s3_delete_uri(uri) result(success)
        character(len=*), intent(in) :: uri
        logical :: success
        character(len=:), allocatable :: bucket, key
        type(s3_config) :: temp_config
        logical :: uri_parsed

        success = .false.

        ! Try to parse as s3:// URI
        call parse_s3_uri(uri, bucket, key, uri_parsed)
        if (.not. uri_parsed) then
            ! Not a s3:// URI, treat as regular key
            success = s3_delete_object(uri)
            return
        end if

        ! Use parsed bucket if different from current config
        if (allocated(bucket) .and. len_trim(bucket) > 0) then
            temp_config = current_config
            temp_config%bucket = bucket
            call s3_init(temp_config)
            success = s3_delete_object(key)
            ! Restore original config
            call s3_init(current_config)
        else
            success = s3_delete_object(key)
        end if
    end function s3_delete_uri

    !> Generate ISO8601 timestamp for AWS signature.
    !>
    !> Generates current UTC time in format: YYYYMMDDTHHMMSSZ
    !>
    !> @param timestamp [out] ISO8601 timestamp string
    subroutine get_iso8601_timestamp(timestamp)
        character(len=:), allocatable, intent(out) :: timestamp
        integer :: values(8)
        character(len=16) :: ts

        ! Get current date and time
        call date_and_time(values=values)

        ! Format as ISO8601: YYYYMMDDTHHMMSSZ
        write(ts, '(I4.4,I2.2,I2.2,A,I2.2,I2.2,I2.2,A)') &
            values(1), values(2), values(3), 'T', values(5), values(6), values(7), 'Z'

        timestamp = trim(ts)
    end subroutine get_iso8601_timestamp

    !> Parse URL to extract host and URI path.
    !>
    !> Extracts hostname and URI path from a complete URL.
    !> Example: "https://bucket.s3.amazonaws.com/path/to/object" ->
    !>          host="bucket.s3.amazonaws.com", uri="/path/to/object"
    !>
    !> @param url [in] Complete URL
    !> @param host [out] Hostname
    !> @param uri [out] URI path (starts with /)
    subroutine parse_url(url, host, uri)
        character(len=*), intent(in) :: url
        character(len=:), allocatable, intent(out) :: host, uri
        integer :: proto_end, host_start, path_start

        ! Find end of protocol (http:// or https://)
        proto_end = index(url, '://')
        if (proto_end == 0) then
            host = ""
            uri = "/"
            return
        end if

        host_start = proto_end + 3

        ! Find start of path (first / after protocol)
        path_start = index(url(host_start:), '/')
        if (path_start == 0) then
            ! No path, just host
            host = url(host_start:)
            uri = "/"
        else
            host = url(host_start:host_start + path_start - 2)
            uri = url(host_start + path_start - 1:)
        end if
    end subroutine parse_url

    !> Get the current S3 configuration.
    !>
    !> Returns the configuration set by s3_init(). This allows other modules
    !> (like s3_multipart) to access the S3 connection parameters.
    !>
    !> @return The current s3_config
    !>
    !> @note Returns an uninitialized config if s3_init() hasn't been called yet.
    function s3_get_config() result(config)
        type(s3_config) :: config
        config = current_config
    end function s3_get_config

    !> Check if S3 has been initialized.
    !>
    !> @return .true. if s3_init() has been called, .false. otherwise
    function s3_is_initialized() result(is_init)
        logical :: is_init
        is_init = initialized
    end function s3_is_initialized

end module s3_http