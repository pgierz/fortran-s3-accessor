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
            new_unittest("get_object_content_validation", test_get_object_content_validation), &
            new_unittest("put_object_without_auth", test_put_object_without_auth), &
            new_unittest("put_object_success", test_put_object_success), &
            new_unittest("get_object_malformed_response", test_get_object_malformed_response), &
            new_unittest("delete_object_without_auth", test_delete_object_without_auth), &
            new_unittest("delete_object_success", test_delete_object_success), &
            new_unittest("http_vs_https_protocols", test_http_vs_https_protocols), &
            new_unittest("different_endpoints", test_different_endpoints), &
            new_unittest("empty_key_edge_case", test_empty_key_edge_case), &
            new_unittest("very_long_key_edge_case", test_very_long_key_edge_case), &
            new_unittest("network_failure_scenarios", test_network_failure_scenarios), &
            new_unittest("boundary_content_sizes", test_boundary_content_sizes), &
            new_unittest("auth_edge_cases", test_auth_edge_cases) &
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

    !> Test S3 put_object function without authentication
    subroutine test_put_object_without_auth(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        logical :: success

        ! Initialize S3 config WITHOUT access_key (empty)
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        config%access_key = ''  ! Empty - no authentication
        config%secret_key = ''
        call s3_init(config)

        ! Test PUT operation should fail due to missing credentials
        success = s3_put_object("test_upload.txt", "Test content for upload")

        call check(error, .not. success, "PUT should fail without authentication")
    end subroutine test_put_object_without_auth

    !> Test S3 put_object function with authentication (mocked success)
    subroutine test_put_object_success(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        logical :: success

        ! Initialize S3 config WITH fake test credentials
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        config%access_key = 'FAKE_ACCESS_KEY_FOR_TESTING'
        config%secret_key = 'FAKE_SECRET_KEY_FOR_TESTING_ONLY'
        call s3_init(config)

        ! Test PUT operation should succeed with mock
        success = s3_put_object("test_upload.txt", "Test content for upload")

        call check(error, success, "PUT should succeed with authentication")
    end subroutine test_put_object_success

    !> Test S3 get_object function with malformed response
    subroutine test_get_object_malformed_response(error)
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

        ! Test getting an object that returns malformed XML response
        success = s3_get_object("malformed_response.txt", content)

        call check(error, .not. success, "Should fail with malformed response")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should still be allocated")
        if (allocated(error)) return

        ! Should contain some XML but not NoSuchKey
        call check(error, index(content, "SomeUnknownError") > 0, "Should contain unknown error")
    end subroutine test_get_object_malformed_response

    !> Test S3 delete_object function without authentication
    subroutine test_delete_object_without_auth(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        logical :: success

        ! Initialize S3 config WITHOUT access_key (empty)
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        config%access_key = ''  ! Empty - no authentication
        config%secret_key = ''
        call s3_init(config)

        ! Test DELETE operation should fail due to missing credentials
        success = s3_delete_object("test_delete.txt")

        call check(error, .not. success, "DELETE should fail without authentication")
    end subroutine test_delete_object_without_auth

    !> Test S3 delete_object function with authentication (mocked success)
    subroutine test_delete_object_success(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        logical :: success

        ! Initialize S3 config WITH fake test credentials
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        config%access_key = 'FAKE_ACCESS_KEY_FOR_TESTING'
        config%secret_key = 'FAKE_SECRET_KEY_FOR_TESTING_ONLY'
        call s3_init(config)

        ! Test DELETE operation should succeed with mock
        success = s3_delete_object("test_delete.txt")

        call check(error, success, "DELETE should succeed with authentication")
    end subroutine test_delete_object_success

    !> Test HTTP vs HTTPS protocols for all operations
    subroutine test_http_vs_https_protocols(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success, exists

        ! Test HTTP protocol (use_https = false)
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .false.  ! HTTP instead of HTTPS
        config%access_key = 'FAKE_ACCESS_KEY_FOR_TESTING'
        config%secret_key = 'FAKE_SECRET_KEY_FOR_TESTING_ONLY'
        call s3_init(config)

        ! Test GET with HTTP
        success = s3_get_object("test_object.txt", content)
        call check(error, success, "GET should work with HTTP protocol")
        if (allocated(error)) return

        ! Test object_exists with HTTP
        exists = s3_object_exists("test_object.txt")
        call check(error, exists, "object_exists should work with HTTP protocol")
        if (allocated(error)) return

        ! Test PUT with HTTP
        success = s3_put_object("test_upload.txt", "Test content for HTTP")
        call check(error, success, "PUT should work with HTTP protocol")
        if (allocated(error)) return

        ! Test DELETE with HTTP
        success = s3_delete_object("test_delete.txt")
        call check(error, success, "DELETE should work with HTTP protocol")
    end subroutine test_http_vs_https_protocols

    !> Test different endpoints and regions
    subroutine test_different_endpoints(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success

        ! Test with different endpoint (eu-west-1)
        config%bucket = 'test-bucket'
        config%region = 'eu-west-1'
        config%endpoint = 's3.eu-west-1.amazonaws.com'
        config%use_https = .true.
        config%access_key = 'FAKE_ACCESS_KEY_FOR_TESTING'
        config%secret_key = 'FAKE_SECRET_KEY_FOR_TESTING_ONLY'
        call s3_init(config)

        ! Test GET with different endpoint
        success = s3_get_object("test_object.txt", content)
        call check(error, success, "GET should work with different endpoint")
        if (allocated(error)) return

        ! Test with MinIO-compatible endpoint
        config%endpoint = 'minio.example.com'
        call s3_init(config)

        success = s3_get_object("test_object.txt", content)
        call check(error, success, "GET should work with MinIO-compatible endpoint")
    end subroutine test_different_endpoints

    !> Test edge cases with empty and very long keys
    subroutine test_empty_key_edge_case(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success, exists

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        config%access_key = 'FAKE_ACCESS_KEY_FOR_TESTING'
        config%secret_key = 'FAKE_SECRET_KEY_FOR_TESTING_ONLY'
        call s3_init(config)

        ! Test with empty key - should handle gracefully
        success = s3_get_object("", content)
        call check(error, .not. success, "GET should fail with empty key")
        if (allocated(error)) return

        exists = s3_object_exists("")
        call check(error, .not. exists, "object_exists should fail with empty key")
    end subroutine test_empty_key_edge_case

    !> Test very long key edge case
    subroutine test_very_long_key_edge_case(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        character(len=1024) :: very_long_key
        logical :: success

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        call s3_init(config)

        ! Create a very long key (S3 key limit is 1024 UTF-8 characters)
        very_long_key = repeat('a', 1024)

        ! Test with very long key - should handle gracefully
        success = s3_get_object(very_long_key, content)
        call check(error, .not. success, "GET should fail with very long key")
    end subroutine test_very_long_key_edge_case


    !> Test network failure scenarios
    subroutine test_network_failure_scenarios(error)
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

        ! Test network failure simulation
        success = s3_get_object("network_failure_test.txt", content)
        call check(error, .not. success, "GET should fail with network failure")
        if (allocated(error)) return

        ! Test timeout simulation
        success = s3_get_object("timeout_test.txt", content)
        call check(error, .not. success, "GET should fail with timeout")
    end subroutine test_network_failure_scenarios

    !> Test boundary conditions for content sizes
    subroutine test_boundary_content_sizes(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        character(len=:), allocatable :: content
        logical :: success

        ! Initialize S3 config
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        config%access_key = 'FAKE_ACCESS_KEY_FOR_TESTING'
        config%secret_key = 'FAKE_SECRET_KEY_FOR_TESTING_ONLY'
        call s3_init(config)

        ! Test empty content file
        success = s3_get_object("empty_content.txt", content)
        call check(error, success, "GET should succeed for empty content file")
        if (allocated(error)) return

        call check(error, allocated(content), "Content should be allocated even for empty file")
        if (allocated(error)) return

        call check(error, len(content) == 0, "Content length should be 0 for empty file")
        if (allocated(error)) return

        ! Test large content file
        success = s3_get_object("large_content.txt", content)
        call check(error, success, "GET should succeed for large content file")
        if (allocated(error)) return

        call check(error, len(content) > 100, "Large content should have significant length")
        if (allocated(error)) return

        ! Test PUT with empty content
        success = s3_put_object("test_empty_upload.txt", "")
        call check(error, success, "PUT should succeed with empty content")
    end subroutine test_boundary_content_sizes

    !> Test authentication edge cases
    subroutine test_auth_edge_cases(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(s3_config) :: config
        logical :: success

        ! Test with empty access_key (the actual check in the code)
        config%bucket = 'test-bucket'
        config%endpoint = 's3.amazonaws.com'
        config%use_https = .true.
        config%access_key = ''  ! Empty access key - this is what gets checked
        config%secret_key = 'FAKE_SECRET_KEY_FOR_TESTING_ONLY'
        call s3_init(config)

        ! PUT should fail because access_key is empty (actual len_trim check)
        success = s3_put_object("test_upload.txt", "Test content")
        call check(error, .not. success, "PUT should fail with missing access key")
        if (allocated(error)) return

        ! DELETE should also fail with missing access key
        success = s3_delete_object("test_delete.txt")
        call check(error, .not. success, "DELETE should fail with missing access key")
        if (allocated(error)) return

        ! Test with whitespace-only credentials
        config%access_key = '   '
        config%secret_key = '   '
        call s3_init(config)

        success = s3_put_object("test_upload.txt", "Test content")
        call check(error, .not. success, "PUT should fail with whitespace-only credentials")
    end subroutine test_auth_edge_cases

end module test_s3_http