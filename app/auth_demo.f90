!> AWS Authentication Demo
!>
!> Demonstrates how to use AWS Signature v4 authentication with the S3 accessor.
!> This example shows both authenticated and unauthenticated access patterns.
program auth_demo
    use s3_http
    use s3_io
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success

    print *, '================================================'
    print *, 'AWS Signature v4 Authentication Demo'
    print *, '================================================'
    print *

    ! ========================================================================
    ! Example 1: Unauthenticated access to public bucket
    ! ========================================================================
    print *, 'Example 1: Public bucket access (no authentication)'
    print *, '---------------------------------------------------'

    ! Configure for public ESGF bucket
    config%bucket = 'esgf-world'
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    config%access_key = ''  ! Empty = unauthenticated
    config%secret_key = ''

    call s3_init(config)
    print *, '  Bucket:   ', trim(config%bucket)
    print *, '  Endpoint: ', trim(config%endpoint)
    print *, '  Region:   ', trim(config%region)
    print *, '  Auth:      No (public bucket)'
    print *

    print *, '  Attempting to get test file...'
    success = s3_get_object('test.txt', content)
    if (success) then
        print *, '  SUCCESS: Got', len(content), 'bytes'
    else
        print *, '  FAILED: Could not retrieve file'
    end if
    print *

    ! ========================================================================
    ! Example 2: Authenticated access to private bucket
    ! ========================================================================
    print *, 'Example 2: Private bucket access (with authentication)'
    print *, '-------------------------------------------------------'

    ! Configure for private bucket with credentials
    ! NOTE: In production, credentials should come from environment variables
    ! or a secure credential store, NOT hardcoded!
    config%bucket = 'my-private-bucket'
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-west-2'
    config%use_https = .true.

    ! WARNING: These are example credentials only!
    ! Real credentials should be loaded from:
    !   - Environment variables (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY)
    !   - AWS credentials file (~/.aws/credentials)
    !   - EC2 instance metadata service
    !   - Other secure credential providers
    config%access_key = 'AKIAIOSFODNN7EXAMPLE'
    config%secret_key = 'wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY'

    call s3_init(config)
    print *, '  Bucket:   ', trim(config%bucket)
    print *, '  Endpoint: ', trim(config%endpoint)
    print *, '  Region:   ', trim(config%region)
    print *, '  Auth:      Yes (AWS Signature v4)'
    print *, '  Key ID:   ', config%access_key(1:20), '...'
    print *

    print *, '  Attempting authenticated request...'
    print *, '  (This will generate AWS4-HMAC-SHA256 signature)'
    success = s3_get_object('private-data.txt', content)
    if (success) then
        print *, '  SUCCESS: Got', len(content), 'bytes'
    else
        print *, '  FAILED: Authentication or file not found'
        print *, '  (This is expected with example credentials)'
    end if
    print *

    ! ========================================================================
    ! Example 3: MinIO/LocalStack (localhost) with authentication
    ! ========================================================================
    print *, 'Example 3: MinIO/LocalStack (localhost:9000)'
    print *, '---------------------------------------------'

    ! Configure for local S3-compatible server
    config%bucket = 'test-bucket'
    config%endpoint = 'localhost:9000'
    config%region = 'us-east-1'
    config%use_https = .false.  ! MinIO default is HTTP
    config%use_path_style = .true.  ! Required for MinIO/localhost

    ! Default MinIO credentials
    config%access_key = 'minioadmin'
    config%secret_key = 'minioadmin'

    call s3_init(config)
    print *, '  Bucket:   ', trim(config%bucket)
    print *, '  Endpoint: ', trim(config%endpoint)
    print *, '  Region:   ', trim(config%region)
    print *, '  Auth:      Yes (AWS Signature v4)'
    print *, '  Style:     Path-style URLs (required for localhost)'
    print *

    print *, '  Attempting MinIO request...'
    success = s3_get_object('test.txt', content)
    if (success) then
        print *, '  SUCCESS: Got', len(content), 'bytes'
    else
        print *, '  FAILED: MinIO not running or file not found'
        print *, '  (This is expected if MinIO is not running locally)'
    end if
    print *

    ! ========================================================================
    ! Summary
    ! ========================================================================
    print *, '================================================'
    print *, 'Summary'
    print *, '================================================'
    print *
    print *, 'Authentication is automatically enabled when both'
    print *, 'access_key and secret_key are provided in config.'
    print *
    print *, 'The library automatically generates AWS Signature v4'
    print *, 'headers (Authorization, X-Amz-Date, Host) for all'
    print *, 'authenticated requests.'
    print *
    print *, 'Supported S3-compatible services:'
    print *, '  - AWS S3 (all regions)'
    print *, '  - MinIO (localhost or remote)'
    print *, '  - LocalStack'
    print *, '  - Other S3-compatible storage systems'
    print *
    print *, 'For production use:'
    print *, '  1. Load credentials from secure sources'
    print *, '  2. Use HTTPS for real AWS S3'
    print *, '  3. Set correct region for your bucket'
    print *, '  4. Use use_path_style=.true. for MinIO/localhost'
    print *

end program auth_demo
