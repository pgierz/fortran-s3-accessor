!> Demo program showing error message formatting
program error_demo
    use s3_errors
    implicit none

    type(s3_error_t) :: error
    character(len=:), allocatable :: formatted

    print *, '========================================='
    print *, 'S3 Error Message Examples'
    print *, '========================================='
    print *, ''

    ! Example 1: 404 Not Found
    print *, '1. Object Not Found (404):'
    print *, '-----------------------------------------'
    error = s3_create_error_from_response(404, &
        '<?xml version="1.0"?><Error><Code>NoSuchKey</Code>' // &
        '<Message>The specified key does not exist.</Message></Error>', &
        'GET', 's3://my-climate-data/CMIP6/model_output.nc')

    formatted = s3_format_error_message(error)
    print *, trim(formatted)
    print *, ''
    if (allocated(error%suggestion)) then
        print *, 'Suggestion:', trim(error%suggestion)
    end if
    print *, ''

    ! Example 2: 403 Forbidden (invalid credentials)
    print *, '2. Access Denied - Invalid Credentials (403):'
    print *, '-----------------------------------------'
    error = s3_create_error_from_response(403, &
        '<?xml version="1.0"?><Error><Code>SignatureDoesNotMatch</Code>' // &
        '<Message>The request signature we calculated does not match ' // &
        'the signature you provided. Check your key and signing method.</Message></Error>', &
        'PUT', 's3://my-bucket/data/results.nc')

    formatted = s3_format_error_message(error)
    print *, trim(formatted)
    print *, ''
    if (allocated(error%suggestion)) then
        print *, 'Suggestion:', trim(error%suggestion)
    end if
    print *, ''

    ! Example 3: 403 Permission denied
    print *, '3. Access Denied - Insufficient Permissions (403):'
    print *, '-----------------------------------------'
    error = s3_create_error_from_response(403, &
        '<?xml version="1.0"?><Error><Code>AccessDenied</Code>' // &
        '<Message>Access Denied</Message></Error>', &
        'DELETE', 's3://production-data/important-file.txt')

    formatted = s3_format_error_message(error)
    print *, trim(formatted)
    print *, ''
    if (allocated(error%suggestion)) then
        print *, 'Suggestion:', trim(error%suggestion)
    end if
    print *, ''

    ! Example 4: Network error (no HTTP response)
    print *, '4. Network Connection Failure:'
    print *, '-----------------------------------------'
    call s3_set_error(S3_ERROR_NETWORK, 0, &
        'Failed to connect to S3 endpoint', &
        'Check network connectivity and endpoint URL. ' // &
        'Verify DNS resolution and firewall settings.', &
        resource='s3://my-bucket/file.nc')

    error = s3_get_last_error()
    formatted = s3_format_error_message(error)
    print *, trim(formatted)
    print *, ''
    if (allocated(error%suggestion)) then
        print *, 'Suggestion:', trim(error%suggestion)
    end if
    print *, ''

    ! Example 5: 401 Unauthorized
    print *, '5. Missing Credentials (401):'
    print *, '-----------------------------------------'
    error = s3_create_error_from_response(401, &
        '<?xml version="1.0"?><Error><Code>InvalidAccessKeyId</Code>' // &
        '<Message>The AWS Access Key Id you provided does not exist in our records.</Message></Error>', &
        'GET', 's3://private-bucket/data.nc')

    formatted = s3_format_error_message(error)
    print *, trim(formatted)
    print *, ''
    if (allocated(error%suggestion)) then
        print *, 'Suggestion:', trim(error%suggestion)
    end if
    print *, ''

    ! Example 6: Uninitialized library
    print *, '6. Library Not Initialized:'
    print *, '-----------------------------------------'
    call s3_set_error(S3_ERROR_INIT, 0, &
        's3_get_object called before s3_init()', &
        'Call s3_init() with configuration before using S3 operations', &
        operation='GET', &
        resource='s3://bucket/file.nc')

    error = s3_get_last_error()
    formatted = s3_format_error_message(error)
    print *, trim(formatted)
    print *, ''
    if (allocated(error%suggestion)) then
        print *, 'Suggestion:', trim(error%suggestion)
    end if
    print *, ''

    print *, '========================================='

end program error_demo
