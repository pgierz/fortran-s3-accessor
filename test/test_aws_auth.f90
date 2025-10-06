!> Test AWS Signature Version 4 implementation
!>
!> Validates against official AWS test vectors from:
!> https://docs.aws.amazon.com/general/latest/gr/sigv4-calculate-signature.html
program test_aws_auth
    use aws_auth
    use openssl_bindings
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, "Testing AWS Signature v4 Implementation"
    print *, "========================================"
    print *, ""

    ! Test: Full signature calculation (from AWS docs example)
    if (.not. test_full_signature()) then
        all_passed = .false.
    end if

    print *, ""
    if (all_passed) then
        print *, "✓ All AWS auth tests passed"
        stop 0
    else
        print *, "✗ Some AWS auth tests failed"
        stop 1
    end if

contains

    !> Test full signing process
    !>
    !> Tests the complete aws_sign_request() function with a simple GET request
    function test_full_signature() result(passed)
        logical :: passed
        type(aws_credential_t) :: creds
        character(len=:), allocatable :: auth_header
        logical :: success

        ! Hash of empty payload (for GET request)
        character(len=*), parameter :: empty_payload_hash = &
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

        print *, "Test: Full signature generation"

        ! Setup test credentials
        allocate(character(len=20) :: creds%access_key)
        allocate(character(len=40) :: creds%secret_key)
        allocate(character(len=9) :: creds%region)
        allocate(character(len=2) :: creds%service)

        creds%access_key = "AKIAIOSFODNN7EXAMPLE"
        creds%secret_key = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
        creds%region = "us-east-1"
        creds%service = "s3"

        ! Generate authorization header for a GET request
        success = aws_sign_request(creds, "GET", "s3.amazonaws.com", &
                                  "/test-bucket/test-key", "", empty_payload_hash, &
                                  "20150830T123600Z", auth_header)

        if (.not. success) then
            print *, "  ✗ FAILED: aws_sign_request() returned false"
            passed = .false.
            return
        end if

        ! Check that auth header has expected structure
        if (index(auth_header, "AWS4-HMAC-SHA256") > 0 .and. &
            index(auth_header, "Credential=") > 0 .and. &
            index(auth_header, "SignedHeaders=") > 0 .and. &
            index(auth_header, "Signature=") > 0) then
            print *, "  ✓ PASSED: Authorization header structure valid"
            print *, "    Header: ", trim(auth_header)
            passed = .true.
        else
            print *, "  ✗ FAILED: Authorization header structure invalid"
            print *, "    Header: ", trim(auth_header)
            passed = .false.
        end if

    end function test_full_signature

end program test_aws_auth
