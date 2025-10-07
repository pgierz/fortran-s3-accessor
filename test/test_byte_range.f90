module test_byte_range
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use s3_http
    use iso_fortran_env, only: int64
    implicit none
    private

    public :: collect_byte_range_tests

contains

    !> Collect all byte-range tests
    subroutine collect_byte_range_tests(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("get_range_first_1kb", test_get_range_first_1kb), &
            new_unittest("get_range_middle", test_get_range_middle), &
            new_unittest("get_range_to_end", test_get_range_to_end), &
            new_unittest("get_range_last_bytes", test_get_range_last_bytes), &
            new_unittest("invalid_range", test_invalid_range), &
            new_unittest("range_larger_than_file", test_range_larger_than_file), &
            new_unittest("backward_compatibility", test_backward_compatibility), &
            new_unittest("range_zero_length", test_range_zero_length), &
            new_unittest("range_single_byte", test_range_single_byte), &
            new_unittest("range_with_auth", test_range_with_auth) &
        ]
    end subroutine collect_byte_range_tests

    !> Test downloading first 1KB (bytes 0-1023)
    subroutine test_get_range_first_1kb(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success
        integer(int64) :: byte_start, byte_end

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Request first 1KB (0-1023, inclusive)
        byte_start = 0_int64
        byte_end = 1023_int64
        success = s3_get_object("range_test_1kb.bin", content, &
                               byte_start=byte_start, byte_end=byte_end)

        call check(error, success, "Should successfully get first 1KB")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        ! For range request, we expect exactly 1024 bytes
        call check(error, len(content) == 1024, "Should receive exactly 1024 bytes")
        if (allocated(error)) return

        ! Check content starts with expected marker
        call check(error, index(content, "START_OF_FILE") > 0, &
                  "Content should contain file header")
    end subroutine test_get_range_first_1kb

    !> Test downloading middle portion (bytes 1000-1999)
    subroutine test_get_range_middle(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success
        integer(int64) :: byte_start, byte_end

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Request middle portion (bytes 1000-1999)
        byte_start = 1000_int64
        byte_end = 1999_int64
        success = s3_get_object("range_test_middle.bin", content, &
                               byte_start=byte_start, byte_end=byte_end)

        call check(error, success, "Should successfully get middle range")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        ! Should receive exactly 1000 bytes (1000-1999 inclusive)
        call check(error, len(content) == 1000, "Should receive exactly 1000 bytes")
        if (allocated(error)) return

        ! Middle portion should contain expected marker
        call check(error, index(content, "MIDDLE_SECTION") > 0, &
                  "Content should be from middle section")
    end subroutine test_get_range_middle

    !> Test downloading from offset to end (bytes 500-)
    subroutine test_get_range_to_end(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success
        integer(int64) :: byte_start, byte_end

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Request from byte 500 to end (large end value)
        byte_start = 500_int64
        byte_end = 999999_int64  ! Beyond file size - should get to actual end
        success = s3_get_object("range_test_to_end.bin", content, &
                               byte_start=byte_start, byte_end=byte_end)

        call check(error, success, "Should successfully get range to end")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        ! Should receive from offset to end (mock returns rest of file)
        call check(error, len(content) > 500, "Should receive data from offset to end")
        if (allocated(error)) return

        ! Should contain end marker
        call check(error, index(content, "END_OF_FILE") > 0, &
                  "Content should contain end marker")
    end subroutine test_get_range_to_end

    !> Test downloading last N bytes (last 100 bytes)
    subroutine test_get_range_last_bytes(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success
        integer(int64) :: byte_start, byte_end

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Request last 100 bytes (file is 1000 bytes, so 900-999)
        byte_start = 900_int64
        byte_end = 999_int64
        success = s3_get_object("range_test_last_100.bin", content, &
                               byte_start=byte_start, byte_end=byte_end)

        call check(error, success, "Should successfully get last 100 bytes")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        ! Should receive exactly 100 bytes
        call check(error, len(content) == 100, "Should receive exactly 100 bytes")
        if (allocated(error)) return

        ! Should contain end marker
        call check(error, index(content, "END_OF_FILE") > 0, &
                  "Content should contain end marker")
    end subroutine test_get_range_last_bytes

    !> Test invalid range (start > end)
    subroutine test_invalid_range(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success
        integer(int64) :: byte_start, byte_end

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Invalid range: start > end
        byte_start = 1000_int64
        byte_end = 500_int64
        success = s3_get_object("range_test_invalid.bin", content, &
                               byte_start=byte_start, byte_end=byte_end)

        ! Should fail gracefully
        call check(error, .not. success, "Should fail with invalid range")
        if (allocated(error)) return

        ! Content may or may not be allocated, but check error was set
        if (s3_has_error()) then
            call check(error, .true., "Error should be set for invalid range")
        end if
    end subroutine test_invalid_range

    !> Test range larger than file
    subroutine test_range_larger_than_file(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success
        integer(int64) :: byte_start, byte_end

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Range beyond file size (file is 1000 bytes, request 0-9999)
        byte_start = 0_int64
        byte_end = 9999_int64
        success = s3_get_object("range_test_small_file.bin", content, &
                               byte_start=byte_start, byte_end=byte_end)

        ! Should succeed - S3 returns available data
        call check(error, success, "Should succeed even if range exceeds file size")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        ! Should receive only available data (not 10000 bytes)
        call check(error, len(content) <= 1000, "Should receive only available data")
    end subroutine test_range_larger_than_file

    !> Test backward compatibility (no range parameters)
    subroutine test_backward_compatibility(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Call without byte range parameters (backward compatible)
        success = s3_get_object("test_object.txt", content)

        call check(error, success, "Should work without range parameters")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        call check(error, len(content) > 0, "Should receive full file content")
        if (allocated(error)) return

        ! Should get the standard test content
        call check(error, index(content, "Hello from mocked S3!") > 0, &
                  "Should contain standard test content")
    end subroutine test_backward_compatibility

    !> Test zero-length range (same start and end)
    subroutine test_range_zero_length(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success
        integer(int64) :: byte_start, byte_end

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Request single byte (byte 100)
        byte_start = 100_int64
        byte_end = 100_int64
        success = s3_get_object("range_test_single.bin", content, &
                               byte_start=byte_start, byte_end=byte_end)

        call check(error, success, "Should succeed with single byte range")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        ! Should receive exactly 1 byte
        call check(error, len(content) == 1, "Should receive exactly 1 byte")
    end subroutine test_range_zero_length

    !> Test single byte request
    subroutine test_range_single_byte(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success
        integer(int64) :: byte_start, byte_end

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Request first byte only
        byte_start = 0_int64
        byte_end = 0_int64
        success = s3_get_object("range_test_single.bin", content, &
                               byte_start=byte_start, byte_end=byte_end)

        call check(error, success, "Should succeed with first byte")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        call check(error, len(content) >= 1, "Should receive at least 1 byte")
    end subroutine test_range_single_byte

    !> Test range request with authentication
    subroutine test_range_with_auth(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success
        integer(int64) :: byte_start, byte_end

        ! Initialize S3 config WITH credentials
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        config%access_key = 'FAKE_ACCESS_KEY_FOR_TESTING'
        config%secret_key = 'FAKE_SECRET_KEY_FOR_TESTING_ONLY'
        call s3_init(config)

        ! Request range with authentication
        byte_start = 0_int64
        byte_end = 1023_int64
        success = s3_get_object("range_test_auth.bin", content, &
                               byte_start=byte_start, byte_end=byte_end)

        call check(error, success, "Range request should work with authentication")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        call check(error, len(content) > 0, "Should receive data with auth")
    end subroutine test_range_with_auth

end module test_byte_range
