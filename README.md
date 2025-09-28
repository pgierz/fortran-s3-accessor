# fortran-s3-accessor

A simple Fortran library for S3-compatible object storage access using direct HTTP calls.

## Features

- **Simple HTTP-based S3 operations**: GET, PUT, DELETE, and HEAD requests
- **No external dependencies**: Uses only standard Fortran and system curl
- **Comprehensive testing**: 22+ test cases with mock-based testing framework
- **Production ready**: Designed for scientific computing workflows like FESOM

## Quick Start

### Basic Usage with s3:// URIs
```fortran
program s3_example
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success

    ! Configure default S3 settings (for auth and region)
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    config%access_key = 'YOUR_ACCESS_KEY'
    config%secret_key = 'YOUR_SECRET_KEY'
    call s3_init(config)

    ! Download using s3:// URI (bucket specified in URI)
    success = s3_get_uri('s3://my-bucket/data/input.txt', content)
    if (success) then
        print *, 'Downloaded:', len(content), 'bytes'
    end if

    ! Upload content
    success = s3_put_uri('s3://my-bucket/data/output.txt', 'Hello S3!')
    if (success) then
        print *, 'Upload successful'
    end if

    ! Check if object exists
    if (s3_exists_uri('s3://my-bucket/data/input.txt')) then
        print *, 'File exists'
    end if
end program
```

### Traditional Key-Based Usage
```fortran
program s3_traditional
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success

    ! Configure S3 access (bucket specified in config)
    config%bucket = 'my-bucket'
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    call s3_init(config)

    ! Download using object key only
    success = s3_get_object('data/input.txt', content)
    if (success) then
        print *, 'Downloaded:', len(content), 'bytes'
    end if
end program
```

## Building

### Using FPM (recommended)
```bash
fpm build
fpm test
```

### Using CMake
```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j
```

## Testing

All tests use mock curl for reliable testing:

```bash
# Run tests with mock curl
PATH="test/scripts:$PATH" fpm test
```

## Documentation

- [Development Guide](CLAUDE.md) - Setup and development instructions
- [API Documentation](https://pgierz.github.io/fortran-s3-accessor/) - Auto-generated docs

## Limitations

- **URL encoding**: Special characters in S3 keys are untested - use alphanumeric keys and underscores only
- **Authentication**: Simplified credential checking (production AWS Signature v4 required)

## License

See LICENSE file for details.
