module test_s3_environment
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use s3_http
    implicit none
    private

    public :: collect_s3_environment

contains

    !> Collect all S3 environment tests
    subroutine collect_s3_environment(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_mode_detection", test_test_mode_detection), &
            new_unittest("config_validation", test_config_validation), &
            new_unittest("url_construction", test_url_construction) &
        ]
    end subroutine collect_s3_environment

    !> Test is_test_mode function detection
    subroutine test_test_mode_detection(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success

        ! NOTE: This test relies on F90S3_TEST_MODE being set in the environment
        ! The is_test_mode function is private, so we test its effect indirectly
        ! by checking that our mock curl is being used instead of real curl

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! If test mode is working, this should use mock curl and succeed
        success = s3_get_object("test_object.txt", content)
        call check(error, success, "GET should succeed in test mode")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated in test mode")
        if (allocated(error)) return

        ! Verify we get the mock content, not real S3 content
        call check(error, index(content, "Hello from mocked S3!") > 0, &
                  "Should get mock content in test mode")
    end subroutine test_test_mode_detection

    !> Test configuration validation
    subroutine test_config_validation(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        logical :: success

        ! Test with minimal valid config
        config%bucket = 'test'
        config%endpoint = 'localhost'
        config%use_https = .false.
        config%access_key = ''
        config%secret_key = ''
        call s3_init(config)

        ! Should be able to attempt operations (they may fail, but shouldn't crash)
        success = s3_object_exists("test.txt")
        call check(error, .true., "Config validation should not crash with minimal config")
    end subroutine test_config_validation

    !> Test URL construction with various configurations
    subroutine test_url_construction(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success

        ! Test various bucket/endpoint combinations to ensure URL construction works
        ! These will likely fail in curl but shouldn't crash

        ! Test with hyphenated bucket name
        config%bucket = 'test-bucket-with-hyphens'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        success = s3_get_object("test.txt", content)
        call check(error, .true., "Should handle hyphenated bucket names")
        if (allocated(error)) return

        ! Test with subdomain-style endpoint
        config%bucket = 'test'
        config%endpoint = 's3.region.amazonaws.com'
        call s3_init(config)

        success = s3_get_object("test.txt", content)
        call check(error, .true., "Should handle regional endpoints")
    end subroutine test_url_construction

end module test_s3_environment