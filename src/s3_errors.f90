!> Enhanced error handling module for S3 operations.
!>
!> Provides detailed error categorization, AWS error code parsing,
!> and actionable recovery suggestions for S3 operations.
!>
!> ## Features
!>
!> - **Error categories**: Network, authentication, S3, client, server errors
!> - **HTTP status codes**: Proper interpretation of all S3 status codes
!> - **AWS error codes**: Parse and interpret XML error responses
!> - **Recovery suggestions**: Actionable hints for fixing common errors
!> - **Backward compatible**: Optional detailed errors, won't break existing code
!>
!> ## Usage
!>
!> ```fortran
!> use s3_errors
!> type(s3_error_t) :: error
!> logical :: success
!>
!> success = s3_get_object('key', content)
!> if (.not. success) then
!>     error = s3_get_last_error()
!>     print *, 'Error:', trim(error%message)
!>     if (allocated(error%suggestion)) then
!>         print *, 'Suggestion:', trim(error%suggestion)
!>     end if
!> end if
!> ```
module s3_errors
    use s3_logger
    implicit none
    private

    ! Public types and procedures
    public :: s3_error_t
    public :: s3_set_error
    public :: s3_get_last_error
    public :: s3_clear_error
    public :: s3_has_error
    public :: s3_parse_xml_error
    public :: s3_format_error_message
    public :: s3_error_from_http_status
    public :: s3_error_from_aws_code
    public :: s3_create_error_from_response

    ! Error category constants (public)
    integer, parameter, public :: S3_ERROR_NONE = 0
    integer, parameter, public :: S3_ERROR_NETWORK = 1
    integer, parameter, public :: S3_ERROR_AUTH = 2
    integer, parameter, public :: S3_ERROR_NOT_FOUND = 3
    integer, parameter, public :: S3_ERROR_PERMISSION = 4
    integer, parameter, public :: S3_ERROR_SERVER = 5
    integer, parameter, public :: S3_ERROR_CLIENT = 6
    integer, parameter, public :: S3_ERROR_TIMEOUT = 7
    integer, parameter, public :: S3_ERROR_INIT = 8

    !> Detailed error information type.
    !>
    !> Contains comprehensive error details including category,
    !> HTTP status, error message, recovery suggestions, and AWS-specific codes.
    type, public :: s3_error_t
        integer :: code = S3_ERROR_NONE           !< Error category code
        integer :: http_status = 0                !< HTTP status code (0 if N/A)
        character(len=:), allocatable :: message  !< Human-readable error message
        character(len=:), allocatable :: suggestion !< Recovery suggestion
        character(len=:), allocatable :: aws_error_code !< AWS error code (e.g., 'NoSuchBucket')
        character(len=:), allocatable :: operation !< Operation that failed (GET, PUT, etc.)
        character(len=:), allocatable :: resource  !< Resource involved (bucket/key)
    end type s3_error_t

    ! Module-level last error storage
    type(s3_error_t), save :: last_error

contains

    !> Set the last error with full details.
    !>
    !> @param[in] code Error category code
    !> @param[in] http_status HTTP status code (0 if not applicable)
    !> @param[in] message Error message
    !> @param[in] suggestion Recovery suggestion (optional)
    !> @param[in] aws_error_code AWS error code (optional)
    !> @param[in] operation Operation that failed (optional)
    !> @param[in] resource Resource involved (optional)
    subroutine s3_set_error(code, http_status, message, suggestion, &
                           aws_error_code, operation, resource)
        integer, intent(in) :: code
        integer, intent(in) :: http_status
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: suggestion
        character(len=*), intent(in), optional :: aws_error_code
        character(len=*), intent(in), optional :: operation
        character(len=*), intent(in), optional :: resource

        ! Clear previous error
        call s3_clear_error()

        ! Set new error
        last_error%code = code
        last_error%http_status = http_status
        last_error%message = trim(message)

        if (present(suggestion)) then
            last_error%suggestion = trim(suggestion)
        end if

        if (present(aws_error_code)) then
            last_error%aws_error_code = trim(aws_error_code)
        end if

        if (present(operation)) then
            last_error%operation = trim(operation)
        end if

        if (present(resource)) then
            last_error%resource = trim(resource)
        end if

        ! Log the error
        call s3_log_error(s3_format_error_message(last_error))
    end subroutine s3_set_error

    !> Get the last error that occurred.
    !>
    !> @return Copy of the last error
    function s3_get_last_error() result(error)
        type(s3_error_t) :: error

        error%code = last_error%code
        error%http_status = last_error%http_status

        if (allocated(last_error%message)) then
            error%message = last_error%message
        end if

        if (allocated(last_error%suggestion)) then
            error%suggestion = last_error%suggestion
        end if

        if (allocated(last_error%aws_error_code)) then
            error%aws_error_code = last_error%aws_error_code
        end if

        if (allocated(last_error%operation)) then
            error%operation = last_error%operation
        end if

        if (allocated(last_error%resource)) then
            error%resource = last_error%resource
        end if
    end function s3_get_last_error

    !> Clear the last error.
    subroutine s3_clear_error()
        last_error%code = S3_ERROR_NONE
        last_error%http_status = 0

        if (allocated(last_error%message)) deallocate(last_error%message)
        if (allocated(last_error%suggestion)) deallocate(last_error%suggestion)
        if (allocated(last_error%aws_error_code)) deallocate(last_error%aws_error_code)
        if (allocated(last_error%operation)) deallocate(last_error%operation)
        if (allocated(last_error%resource)) deallocate(last_error%resource)
    end subroutine s3_clear_error

    !> Check if there is a current error.
    !>
    !> @return .true. if an error is set, .false. otherwise
    function s3_has_error() result(has_err)
        logical :: has_err
        has_err = (last_error%code /= S3_ERROR_NONE)
    end function s3_has_error

    !> Parse S3 XML error response.
    !>
    !> Extracts AWS error code and message from XML response like:
    !> ```xml
    !> <?xml version="1.0" encoding="UTF-8"?>
    !> <Error>
    !>   <Code>NoSuchBucket</Code>
    !>   <Message>The specified bucket does not exist</Message>
    !>   <BucketName>my-bucket</BucketName>
    !> </Error>
    !> ```
    !>
    !> @param[in] xml_response The XML error response from S3
    !> @param[out] error_code AWS error code (e.g., 'NoSuchBucket')
    !> @param[out] error_message Error message from S3
    !> @return .true. if parsing succeeded, .false. otherwise
    function s3_parse_xml_error(xml_response, error_code, error_message) result(success)
        character(len=*), intent(in) :: xml_response
        character(len=:), allocatable, intent(out) :: error_code
        character(len=:), allocatable, intent(out) :: error_message
        logical :: success

        integer :: code_start, code_end, msg_start, msg_end

        success = .false.

        ! Look for <Code>...</Code>
        code_start = index(xml_response, '<Code>')
        if (code_start > 0) then
            code_start = code_start + 6  ! Length of '<Code>'
            code_end = index(xml_response(code_start:), '</Code>')
            if (code_end > 0) then
                code_end = code_start + code_end - 2
                error_code = trim(xml_response(code_start:code_end))
                success = .true.
            end if
        end if

        ! Look for <Message>...</Message>
        msg_start = index(xml_response, '<Message>')
        if (msg_start > 0) then
            msg_start = msg_start + 9  ! Length of '<Message>'
            msg_end = index(xml_response(msg_start:), '</Message>')
            if (msg_end > 0) then
                msg_end = msg_start + msg_end - 2
                error_message = trim(xml_response(msg_start:msg_end))
            end if
        end if

        ! If we got a code, consider it successful
        if (.not. allocated(error_code)) then
            success = .false.
        end if
    end function s3_parse_xml_error

    !> Format error as a human-readable message.
    !>
    !> @param[in] error Error to format
    !> @return Formatted error message string
    function s3_format_error_message(error) result(msg)
        type(s3_error_t), intent(in) :: error
        character(len=:), allocatable :: msg
        character(len=1024) :: buffer

        if (error%code == S3_ERROR_NONE) then
            msg = 'No error'
            return
        end if

        ! Start with category and HTTP status
        if (error%http_status > 0) then
            write(buffer, '(A,I0,A)') 'S3 Error [HTTP ', error%http_status, ']: '
        else
            buffer = 'S3 Error: '
        end if

        ! Add main message
        if (allocated(error%message)) then
            buffer = trim(buffer) // trim(error%message)
        else
            buffer = trim(buffer) // 'Unknown error'
        end if

        ! Add AWS error code if present
        if (allocated(error%aws_error_code)) then
            buffer = trim(buffer) // ' (AWS: ' // trim(error%aws_error_code) // ')'
        end if

        ! Add operation and resource context
        if (allocated(error%operation) .and. allocated(error%resource)) then
            buffer = trim(buffer) // ' during ' // trim(error%operation) // ' of ' // trim(error%resource)
        else if (allocated(error%operation)) then
            buffer = trim(buffer) // ' during ' // trim(error%operation)
        else if (allocated(error%resource)) then
            buffer = trim(buffer) // ' for ' // trim(error%resource)
        end if

        msg = trim(buffer)
    end function s3_format_error_message

    !> Map HTTP status code to error category and message.
    !>
    !> @param[in] http_status HTTP status code
    !> @param[out] error_code Error category constant
    !> @param[out] message Human-readable error message
    !> @param[out] suggestion Recovery suggestion
    subroutine s3_error_from_http_status(http_status, error_code, message, suggestion)
        integer, intent(in) :: http_status
        integer, intent(out) :: error_code
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable, intent(out) :: suggestion

        select case (http_status)
        ! 2xx Success
        case (200, 201, 202, 204)
            error_code = S3_ERROR_NONE
            message = 'Success'
            suggestion = ''

        ! 400 Bad Request
        case (400)
            error_code = S3_ERROR_CLIENT
            message = 'Bad Request - Invalid request format'
            suggestion = 'Check S3 key format, ensure no invalid characters. ' // &
                        'URL encoding of special characters may be needed.'

        ! 401 Unauthorized
        case (401)
            error_code = S3_ERROR_AUTH
            message = 'Unauthorized - Missing or invalid credentials'
            suggestion = 'Set AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY environment variables, ' // &
                        'or configure credentials in s3_config.'

        ! 403 Forbidden
        case (403)
            error_code = S3_ERROR_PERMISSION
            message = 'Forbidden - Access Denied'
            suggestion = 'Verify AWS credentials are correct. ' // &
                        'Check IAM policy allows s3:GetObject/PutObject on this resource. ' // &
                        'Ensure system clock is synchronized (AWS requires <15 min skew). ' // &
                        'Check for trailing whitespace in secret key.'

        ! 404 Not Found
        case (404)
            error_code = S3_ERROR_NOT_FOUND
            message = 'Not Found - Bucket or object does not exist'
            suggestion = 'Verify bucket and object key names are correct. ' // &
                        'Check bucket exists and is in the specified region.'

        ! 408 Request Timeout
        case (408)
            error_code = S3_ERROR_TIMEOUT
            message = 'Request Timeout - Request took too long'
            suggestion = 'Increase timeout value. Check network connectivity. ' // &
                        'For large files, consider using multipart upload.'

        ! 409 Conflict
        case (409)
            error_code = S3_ERROR_CLIENT
            message = 'Conflict - Resource conflict'
            suggestion = 'Bucket may already exist or deletion may be in progress. ' // &
                        'Wait and retry.'

        ! 500 Internal Server Error
        case (500)
            error_code = S3_ERROR_SERVER
            message = 'Internal Server Error - S3 service issue'
            suggestion = 'This is an S3 service error. Retry with exponential backoff. ' // &
                        'If persists, check AWS service health dashboard.'

        ! 503 Service Unavailable
        case (503)
            error_code = S3_ERROR_SERVER
            message = 'Service Unavailable - S3 temporarily unavailable'
            suggestion = 'S3 is temporarily unavailable. Wait and retry with exponential backoff. ' // &
                        'Reduce request rate if making many requests.'

        ! Unknown status
        case default
            if (http_status >= 500) then
                error_code = S3_ERROR_SERVER
                message = 'Server Error'
                suggestion = 'S3 server error. Retry with exponential backoff.'
            else if (http_status >= 400) then
                error_code = S3_ERROR_CLIENT
                message = 'Client Error'
                suggestion = 'Check request parameters and try again.'
            else if (http_status == 0) then
                error_code = S3_ERROR_NETWORK
                message = 'Network Error - Could not connect to S3'
                suggestion = 'Check network connectivity. Verify endpoint URL is correct. ' // &
                            'Check firewall settings allow HTTPS connections.'
            else
                error_code = S3_ERROR_CLIENT
                message = 'Unknown Error'
                suggestion = 'Consult AWS S3 documentation for HTTP status code details.'
            end if
        end select
    end subroutine s3_error_from_http_status

    !> Get suggestion for AWS error code.
    !>
    !> @param[in] aws_code AWS error code (e.g., 'NoSuchBucket')
    !> @param[out] suggestion Recovery suggestion
    subroutine s3_error_from_aws_code(aws_code, suggestion)
        character(len=*), intent(in) :: aws_code
        character(len=:), allocatable, intent(out) :: suggestion

        select case (trim(aws_code))
        case ('NoSuchBucket')
            suggestion = 'The specified bucket does not exist. Verify bucket name is correct and bucket exists.'

        case ('NoSuchKey')
            suggestion = 'The specified object key does not exist. Verify object key is correct.'

        case ('SignatureDoesNotMatch')
            suggestion = 'Request signature mismatch. Verify AWS secret key is correct and has no extra whitespace. ' // &
                        'Check system clock is synchronized (within 15 minutes of AWS time).'

        case ('InvalidAccessKeyId')
            suggestion = 'AWS access key ID is invalid. Verify AWS_ACCESS_KEY_ID is correct.'

        case ('ExpiredToken')
            suggestion = 'Security token has expired. Refresh your temporary credentials.'

        case ('AccessDenied')
            suggestion = 'Access denied. Check IAM policy permissions for this operation and resource. ' // &
                        'Verify bucket policy allows access from your account.'

        case ('BucketAlreadyExists', 'BucketAlreadyOwnedByYou')
            suggestion = 'Bucket name already exists. S3 bucket names must be globally unique. Choose a different name.'

        case ('InvalidBucketName')
            suggestion = 'Bucket name is invalid. Bucket names must be 3-63 characters, lowercase, ' // &
                        'and contain only letters, numbers, hyphens.'

        case ('RequestTimeout')
            suggestion = 'Request timed out. Retry the operation. For large files, consider multipart upload.'

        case ('SlowDown')
            suggestion = 'Request rate too high. Reduce request rate and implement exponential backoff.'

        case ('ServiceUnavailable', 'InternalError')
            suggestion = 'S3 service temporarily unavailable. Retry with exponential backoff.'

        case ('InvalidArgument')
            suggestion = 'Invalid request argument. Check all request parameters are valid.'

        case ('MethodNotAllowed')
            suggestion = 'HTTP method not allowed for this resource. Verify operation is supported.'

        case ('MissingSecurityHeader')
            suggestion = 'Required security header missing. Ensure AWS Signature v4 authentication headers are present.'

        case default
            suggestion = 'Consult AWS S3 API documentation for error code: ' // trim(aws_code)
        end select
    end subroutine s3_error_from_aws_code

    !> Create complete error from HTTP response.
    !>
    !> Combines HTTP status code interpretation with XML error parsing
    !> to create a comprehensive error object.
    !>
    !> @param[in] http_status HTTP status code from response
    !> @param[in] response_body Response body (may contain XML error)
    !> @param[in] operation Operation that failed (GET, PUT, DELETE, etc.)
    !> @param[in] resource Resource involved (bucket/key)
    !> @return Complete error object
    function s3_create_error_from_response(http_status, response_body, operation, resource) result(error)
        integer, intent(in) :: http_status
        character(len=*), intent(in) :: response_body
        character(len=*), intent(in), optional :: operation
        character(len=*), intent(in), optional :: resource
        type(s3_error_t) :: error

        character(len=:), allocatable :: aws_code, aws_msg, base_suggestion, aws_suggestion
        logical :: has_xml_error

        ! Initialize error
        error%code = S3_ERROR_NONE
        error%http_status = http_status

        ! Get base error info from HTTP status
        call s3_error_from_http_status(http_status, error%code, error%message, base_suggestion)

        ! Try to parse XML error response
        has_xml_error = s3_parse_xml_error(response_body, aws_code, aws_msg)

        if (has_xml_error) then
            ! Use AWS error code and message if available
            error%aws_error_code = aws_code

            ! Prefer AWS message over generic HTTP message
            if (allocated(aws_msg) .and. len_trim(aws_msg) > 0) then
                error%message = aws_msg
            end if

            ! Get AWS-specific suggestion
            call s3_error_from_aws_code(aws_code, aws_suggestion)
            error%suggestion = aws_suggestion
        else
            ! Use HTTP status suggestion
            if (allocated(base_suggestion)) then
                error%suggestion = base_suggestion
            end if
        end if

        ! Add operation and resource context
        if (present(operation)) then
            error%operation = trim(operation)
        end if

        if (present(resource)) then
            error%resource = trim(resource)
        end if
    end function s3_create_error_from_response

end module s3_errors
