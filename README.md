# fortran-s3-accessor

A simple Fortran library for S3-compatible object storage access using direct HTTP calls.

## Features

- **Simple HTTP-based S3 operations**: GET, PUT, DELETE, and HEAD requests
- **No external dependencies**: Uses only standard Fortran and system curl
- **Comprehensive testing**: 22+ test cases with mock-based testing framework
- **Production ready**: Designed for scientific computing workflows like FESOM

## Quick Start

```fortran
program s3_example
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success

    ! Configure S3 access
    config%bucket = 'my-bucket'
    config%region = 'us-east-1'
    config%endpoint = 's3.amazonaws.com'
    config%use_https = .true.
    call s3_init(config)

    ! Download an object
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
