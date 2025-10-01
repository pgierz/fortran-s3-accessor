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

## Performance Notice

### Current Implementation (v1.1.0)

The library uses direct memory streaming via POSIX `popen()` on Linux/macOS/Unix systems:

```
Network → Memory (POSIX systems)
Network → /tmp → Memory (Windows fallback)
```

**Performance Characteristics:**

| Platform | Method | Overhead | Notes |
|----------|--------|----------|-------|
| Linux/macOS/Unix | Direct streaming | Minimal (~10ms) | Uses popen() C binding |
| Windows | Temp file fallback | ~10-30% | Falls back to v1.0.0 method |

### Platform Support

**Full Performance (Direct Streaming):**
- Linux (all HPC clusters)
- macOS
- Unix-like systems with POSIX compliance

**Fallback Mode (Temporary Files):**
- Windows systems
- Non-POSIX platforms

The library automatically detects platform capabilities and uses the fastest method available. No configuration required.

### Roadmap: v1.2.0 Future Enhancements

**Planned for v1.2.0+:**

1. **libcurl integration** - For maximum performance and features:
   - Native libcurl via `iso_c_binding`
   - Proper AWS Signature v4 authentication
   - Progress callbacks and streaming APIs
   - Multipart upload support
   - Windows native streaming support

**Contributing:** See [issues tagged `performance`](https://github.com/pgierz/fortran-s3-accessor/labels/performance) to contribute to these improvements.

## Current Limitations

- **URL encoding**: Special characters in S3 keys are untested - use alphanumeric keys and underscores only
- **Authentication**: Simplified credential checking (production AWS Signature v4 implementation planned for v1.2.0)
- **Windows streaming**: Temporary file fallback on Windows (native streaming planned for v1.2.0)
- **Error messages**: Limited error diagnostics from curl (improved error handling planned)

## Use Cases

### Primary Use Case: FESOM Climate Model

This library is designed to enable cloud-friendly ocean modeling workflows:

- Download forcing data from S3 buckets
- Read NetCDF initialization files from object storage
- Upload simulation outputs to cloud storage
- Access CMIP6 and ESGF datasets directly

### Proven Applications

- **Public bucket access** - NOAA GFS, ESGF climate data, NASA Earth observations
- **Configuration files** - Model parameters, namelist files, metadata
- **Small to medium datasets** - Regional model outputs, analysis results
- **Prototype workflows** - Testing cloud integration before production deployment

## NetCDF Integration

The library integrates seamlessly with NetCDF-Fortran for reading climate data directly from S3:

```fortran
program netcdf_s3_example
    use s3_http
    use netcdf
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    integer :: ncid, status

    ! Download NetCDF file from S3 to memory
    call s3_init(config)
    call s3_get_object('path/to/data.nc', content)

    ! Write to temp file (prefer /dev/shm RAM disk on Linux)
    open(unit=10, file='/dev/shm/temp.nc', form='unformatted', access='stream')
    write(10) content
    close(10)

    ! Open with NetCDF library
    status = nf90_open('/dev/shm/temp.nc', NF90_NOWRITE, ncid)
    ! ... use NetCDF normally ...
    status = nf90_close(ncid)
end program
```

**See:** `examples/netcdf_minimal.f90` for a complete working example

**Requirements:** NetCDF-Fortran must be installed separately
- Ubuntu/Debian: `sudo apt-get install libnetcdf-dev libnetcdff-dev`
- Compile with: `gfortran ... $(nf-config --fflags) $(nf-config --flibs)`

**For full-featured integration** with transparent S3 URIs and automatic cleanup, see the separate [`fortran-s3-netcdf`](https://github.com/pgierz/fortran-s3-netcdf) package (coming soon).

## Installation

### From FPM Registry

```bash
# Add to your fpm.toml:
[dependencies]
fortran-s3-accessor = "1.0.0"
```

### From Source

```bash
git clone https://github.com/pgierz/fortran-s3-accessor.git
cd fortran-s3-accessor
fpm build
```

## Contributing

Contributions welcome! Priority areas for v1.1.0:

1. **Performance** - Stream curl output directly
2. **Error handling** - Better diagnostics and error messages
3. **Testing** - Real S3 integration tests (beyond mocks)
4. **Documentation** - More examples for scientific workflows

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

See LICENSE file for details.
