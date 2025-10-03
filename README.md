# fortran-s3-accessor

A simple Fortran library for S3-compatible object storage access using direct HTTP calls.

**Platform Support**: Linux only. This library uses libcurl through Fortran's C interoperability, which works reliably on Linux but has ABI incompatibility issues on macOS.

## Features

- **Simple HTTP-based S3 operations**: GET, PUT, DELETE, and HEAD requests
- **No external dependencies**: Uses only standard Fortran and system curl (Linux)
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

### Roadmap: v1.2.0 - Christmas Release 🎄

**Target: December 25, 2025** | [Milestone](https://github.com/pgierz/fortran-s3-accessor/milestone/1) | [All Issues](https://github.com/pgierz/fortran-s3-accessor/issues?q=is%3Aissue+milestone%3A%22v1.2.0+-+Christmas+Release+%F0%9F%8E%84%22)

Major enhancements planned:

1. **[libcurl integration](https://github.com/pgierz/fortran-s3-accessor/issues/9)** (#9) - Native performance and Windows support
   - Direct C API via `iso_c_binding`
   - Cross-platform streaming (including Windows!)
   - Better error diagnostics
   - ~50% performance improvement over v1.1.0

2. **[AWS Signature v4 authentication](https://github.com/pgierz/fortran-s3-accessor/issues/10)** (#10) - Production-grade security
   - Full AWS authentication protocol
   - Private bucket access
   - Temporary credentials (STS) support
   - All AWS regions

3. **[Progress callbacks](https://github.com/pgierz/fortran-s3-accessor/issues/11)** (#11) - Real-time transfer monitoring
   - Download/upload progress reporting
   - Speed metrics and ETA
   - Cancellation support
   - Easy callback interface

4. **[Multipart upload](https://github.com/pgierz/fortran-s3-accessor/issues/12)** (#12) - Large file support
   - Upload files >5GB (up to 5TB!)
   - Chunked uploads with resume capability
   - Optional parallel chunk uploads
   - Per-chunk progress tracking

5. **[Enhanced error diagnostics](https://github.com/pgierz/fortran-s3-accessor/issues/13)** (#13) - Better troubleshooting
   - Clear, actionable error messages
   - Categorized errors (network, auth, S3)
   - AWS error code interpretation
   - Recovery suggestions

**Contributing:** See [v1.2.0 milestone](https://github.com/pgierz/fortran-s3-accessor/milestone/1) to contribute to these improvements!

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

> **Note:** FPM registry integration is coming soon! See [#14](https://github.com/pgierz/fortran-s3-accessor/issues/14) for progress.
> For now, use git-based dependencies (see below).

```toml
# Future: When published to FPM registry
[dependencies]
fortran-s3-accessor = "1.1.0"
```

### From Git (Recommended)

```toml
# Add to your fpm.toml:
[dependencies]
fortran-s3-accessor = { git = "https://github.com/pgierz/fortran-s3-accessor.git", tag = "v1.1.0" }
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
