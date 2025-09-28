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
            new_unittest("get_object", test_get_object), &
            new_unittest("get_object_not_found", test_get_object_not_found), &
            new_unittest("object_exists_true", test_object_exists_true), &
            new_unittest("object_exists_false", test_object_exists_false), &
            new_unittest("get_object_content_validation", test_get_object_content_validation) &
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

    !> Test S3 get_object function with nonexistent file
    subroutine test_get_object_not_found(error)
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

        ! Test getting an object that doesn't exist
        success = s3_get_object("missing_file.txt", content)

        call check(error, .not. success, "Should fail for missing file")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should still be allocated")
        if (allocated(error)) return

        ! Should contain XML error response
        call check(error, index(content, "NoSuchKey") > 0, "Should contain NoSuchKey error")
    end subroutine test_get_object_not_found

    !> Test S3 object_exists function for existing file
    subroutine test_object_exists_true(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        logical :: exists

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Test object that should exist (in mock)
        exists = s3_object_exists("test_object.txt")

        call check(error, exists, "test_object.txt should exist")
    end subroutine test_object_exists_true

    !> Test S3 object_exists function for missing file
    subroutine test_object_exists_false(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        logical :: exists

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Test object that should NOT exist (in mock)
        exists = s3_object_exists("missing_file.txt")

        call check(error, .not. exists, "missing_file.txt should not exist")
    end subroutine test_object_exists_false

    !> Test S3 get_object content validation
    subroutine test_get_object_content_validation(error)
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

        ! Test getting an object and validate exact content
        success = s3_get_object("test_object.txt", content)

        call check(error, success, "Should successfully get test_object.txt")
        if (allocated(error)) return

        ! Validate exact content matches our mock response
        call check(error, index(content, "Hello from mocked S3!") > 0, "Should contain first line")
        if (allocated(error)) return

        call check(error, index(content, "This is test data.") > 0, "Should contain second line")
        if (allocated(error)) return

        call check(error, index(content, "Line 3 of test file.") > 0, "Should contain third line")
    end subroutine test_get_object_content_validation

end module test_s3_http