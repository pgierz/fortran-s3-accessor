!> Test suite for S3 error handling and diagnostics.
!>
!> Tests error parsing, HTTP status interpretation, and AWS error code suggestions.
program test_s3_errors
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use s3_errors
    implicit none

    integer :: stat
    type(unittest_type), allocatable :: testsuite(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuite = [ &
        new_unittest("xml_error_parsing", test_xml_error_parsing), &
        new_unittest("http_status_403", test_http_status_403), &
        new_unittest("http_status_404", test_http_status_404), &
        new_unittest("aws_code_nosuchbucket", test_aws_code_nosuchbucket), &
        new_unittest("aws_code_signature_mismatch", test_aws_code_signature_mismatch), &
        new_unittest("complete_error_from_response", test_complete_error_from_response), &
        new_unittest("error_formatting", test_error_formatting) &
    ]

    call testdrive_run(testsuite, stat)

    if (stat > 0) then
        write(*, *) 'Some tests failed!'
        stop 1
    end if

contains

    !> Test XML error response parsing
    subroutine test_xml_error_parsing(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: error_code, error_message
        logical :: success
        character(len=*), parameter :: xml_response = &
            '<?xml version="1.0" encoding="UTF-8"?>' // &
            '<Error>' // &
            '<Code>NoSuchBucket</Code>' // &
            '<Message>The specified bucket does not exist</Message>' // &
            '<BucketName>my-bucket</BucketName>' // &
            '</Error>'

        success = s3_parse_xml_error(xml_response, error_code, error_message)

        call check(error, success, "XML parsing should succeed")
        if (allocated(error)) return

        call check(error, allocated(error_code), "Error code should be allocated")
        if (allocated(error)) return

        call check(error, trim(error_code) == "NoSuchBucket", "Error code should be NoSuchBucket")
        if (allocated(error)) return

        call check(error, allocated(error_message), "Error message should be allocated")
        if (allocated(error)) return

        call check(error, index(error_message, "does not exist") > 0, "Message should contain error text")
    end subroutine test_xml_error_parsing

    !> Test HTTP 403 Forbidden status interpretation
    subroutine test_http_status_403(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: error_code
        character(len=:), allocatable :: message, suggestion

        call s3_error_from_http_status(403, error_code, message, suggestion)

        call check(error, error_code == S3_ERROR_PERMISSION, "403 should map to PERMISSION error")
        if (allocated(error)) return

        call check(error, index(message, "Forbidden") > 0, "Message should mention Forbidden")
        if (allocated(error)) return

        call check(error, index(suggestion, "credentials") > 0, "Suggestion should mention credentials")
        if (allocated(error)) return

        call check(error, index(suggestion, "clock") > 0, "Suggestion should mention clock sync")
    end subroutine test_http_status_403

    !> Test HTTP 404 Not Found status interpretation
    subroutine test_http_status_404(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: error_code
        character(len=:), allocatable :: message, suggestion

        call s3_error_from_http_status(404, error_code, message, suggestion)

        call check(error, error_code == S3_ERROR_NOT_FOUND, "404 should map to NOT_FOUND error")
        if (allocated(error)) return

        call check(error, index(message, "Not Found") > 0, "Message should mention Not Found")
        if (allocated(error)) return

        call check(error, index(suggestion, "bucket") > 0, "Suggestion should mention bucket")
    end subroutine test_http_status_404

    !> Test AWS NoSuchBucket error code suggestion
    subroutine test_aws_code_nosuchbucket(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: suggestion

        call s3_error_from_aws_code("NoSuchBucket", suggestion)

        call check(error, allocated(suggestion), "Suggestion should be allocated")
        if (allocated(error)) return

        call check(error, index(suggestion, "does not exist") > 0, "Suggestion should mention bucket doesn't exist")
    end subroutine test_aws_code_nosuchbucket

    !> Test AWS SignatureDoesNotMatch error code suggestion
    subroutine test_aws_code_signature_mismatch(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: suggestion

        call s3_error_from_aws_code("SignatureDoesNotMatch", suggestion)

        call check(error, allocated(suggestion), "Suggestion should be allocated")
        if (allocated(error)) return

        call check(error, index(suggestion, "secret key") > 0, "Suggestion should mention secret key")
        if (allocated(error)) return

        call check(error, index(suggestion, "clock") > 0, "Suggestion should mention clock")
    end subroutine test_aws_code_signature_mismatch

    !> Test complete error creation from HTTP response
    subroutine test_complete_error_from_response(error)
        type(error_type), allocatable, intent(out) :: error
        type(s3_error_t) :: s3_error
        character(len=*), parameter :: xml_response = &
            '<?xml version="1.0" encoding="UTF-8"?>' // &
            '<Error>' // &
            '<Code>AccessDenied</Code>' // &
            '<Message>Access Denied</Message>' // &
            '</Error>'

        s3_error = s3_create_error_from_response(403, xml_response, "GET", "s3://my-bucket/key.txt")

        call check(error, s3_error%code == S3_ERROR_PERMISSION, "Should be PERMISSION error")
        if (allocated(error)) return

        call check(error, s3_error%http_status == 403, "HTTP status should be 403")
        if (allocated(error)) return

        call check(error, allocated(s3_error%aws_error_code), "AWS error code should be set")
        if (allocated(error)) return

        call check(error, trim(s3_error%aws_error_code) == "AccessDenied", "AWS code should be AccessDenied")
        if (allocated(error)) return

        call check(error, allocated(s3_error%operation), "Operation should be set")
        if (allocated(error)) return

        call check(error, trim(s3_error%operation) == "GET", "Operation should be GET")
    end subroutine test_complete_error_from_response

    !> Test error message formatting
    subroutine test_error_formatting(error)
        type(error_type), allocatable, intent(out) :: error
        type(s3_error_t) :: s3_error
        character(len=:), allocatable :: formatted

        s3_error%code = S3_ERROR_NOT_FOUND
        s3_error%http_status = 404
        s3_error%message = "Object not found"
        s3_error%aws_error_code = "NoSuchKey"
        s3_error%operation = "GET"
        s3_error%resource = "s3://bucket/key.txt"

        formatted = s3_format_error_message(s3_error)

        call check(error, allocated(formatted), "Formatted message should be allocated")
        if (allocated(error)) return

        call check(error, index(formatted, "404") > 0, "Should include HTTP status")
        if (allocated(error)) return

        call check(error, index(formatted, "NoSuchKey") > 0, "Should include AWS error code")
        if (allocated(error)) return

        call check(error, index(formatted, "GET") > 0, "Should include operation")
    end subroutine test_error_formatting

    !> Simple test driver (substitute for testdrive_run if not available)
    subroutine testdrive_run(tests, stat)
        type(unittest_type), intent(in) :: tests(:)
        integer, intent(out) :: stat
        type(error_type), allocatable :: error
        integer :: i

        stat = 0

        do i = 1, size(tests)
            write(*, fmt) "Running test:", tests(i)%name
            call tests(i)%test(error)
            if (allocated(error)) then
                write(*, fmt) "  FAILED:", error%message
                stat = stat + 1
                deallocate(error)
            else
                write(*, fmt) "  PASSED"
            end if
        end do

        if (stat == 0) then
            write(*, fmt) "All tests passed!"
        else
            write(*, fmt) "Failed tests:", stat
        end if
    end subroutine testdrive_run

end program test_s3_errors
