module test_s3_http
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use s3_http
    implicit none
    private

    public :: collect_s3_http

contains

    !> Collect all S3 HTTP tests
    subroutine collect_s3_http(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("config_init", test_config_init), &
            new_unittest("get_object", test_get_object) &
        ]
    end subroutine collect_s3_http

    !> Test S3 config initialization
    subroutine test_config_init(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config

        ! Test default values
        config%bucket = 'test-bucket'
        config%region = 'us-west-2'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.

        call s3_init(config)

        ! Basic validation - just check it doesn't crash
        call check(error, config%bucket == 'test-bucket', "Config bucket should be set")
        if (allocated(error)) return

        call check(error, config%use_https .eqv. .true., "Config should use HTTPS")
    end subroutine test_config_init

    !> Test S3 get_object function
    subroutine test_get_object(error)
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

        ! Test getting an object that should exist (in mock)
        success = s3_get_object("test_object.txt", content)

        call check(error, success, "Should successfully get test_object.txt")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated")
        if (allocated(error)) return

        call check(error, len(content) > 0, "Content should not be empty")
    end subroutine test_get_object

end module test_s3_http