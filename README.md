# fortran-s3-accessor

A simple Fortran library for S3-compatible object storage access using direct HTTP calls.

## Platform Support

| Platform | S3 Operations | Progress Callbacks | Performance | Status |
|----------|---------------|-------------------|-------------|---------|
| **Linux** | ✅ Full support | ✅ Yes | Excellent (libcurl direct) | Production ready |
| **macOS** | ✅ Core operations | ❌ No | Good (subprocess fallback) | Development only |
| **Windows** | ❌ Not supported | ❌ No | N/A | Not tested |

**Production deployments**: Linux only (ubuntu-latest or HPC clusters)

**Development on macOS**: Works with automatic fallback to popen() subprocess streaming (no progress callbacks)

**Windows**: Not supported or tested. CI only runs on Linux.

## Features

- **AWS Signature v4 Authentication**: Full production-grade authentication for private buckets
- **Simple HTTP-based S3 operations**: GET, PUT, DELETE, and HEAD requests
- **Progress callbacks**: Real-time download monitoring for large files (Linux only)
- **libcurl direct integration**: Zero-copy streaming with excellent performance (Linux)
- **macOS fallback**: popen() subprocess streaming for development (macOS only)
- **Comprehensive testing**: 27 unit tests + 8 integration tests with MinIO in CI
- **S3-compatible services**: Works with AWS S3, MinIO, LocalStack, and other S3-compatible storage
- **Production ready**: Designed for Linux HPC environments and scientific computing workflows

## Authentication

The library supports both **public** (unauthenticated) and **private** (authenticated) S3 buckets.

### Public Buckets (No Authentication)

For public buckets, simply leave `access_key` and `secret_key` empty:

```fortran
config%bucket = 'esgf-world'
config%endpoint = 's3.amazonaws.com'
config%region = 'us-east-1'
config%access_key = ''  ! Empty = unauthenticated
config%secret_key = ''
call s3_init(config)
```

### Private Buckets (AWS Signature v4)

For private buckets, provide your AWS credentials. The library automatically uses **AWS Signature v4** authentication:

```fortran
config%bucket = 'my-private-bucket'
config%endpoint = 's3.amazonaws.com'
config%region = 'us-west-2'
config%access_key = 'AKIAIOSFODNN7EXAMPLE'
config%secret_key = 'wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY'
call s3_init(config)
! All requests now automatically signed with AWS Signature v4
```

**Requirements:** Authentication requires OpenSSL to be available at runtime (libssl, libcrypto).

**Security Note:** In production, load credentials from:
- Environment variables (`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`)
- AWS credentials file (`~/.aws/credentials`)
- EC2 instance metadata service
- Never hardcode credentials in source code!

### S3-Compatible Services (MinIO, LocalStack)

The library works with any S3-compatible service:

```fortran
config%bucket = 'test-bucket'
config%endpoint = 'localhost:9000'
config%region = 'us-east-1'
config%use_https = .false.
config%use_path_style = .true.  ! Required for localhost
config%access_key = 'minioadmin'
config%secret_key = 'minioadmin'
call s3_init(config)
```

**See:** `app/auth_demo.f90` for complete authentication examples.

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

The library includes comprehensive testing at multiple levels:

### Unit Tests (27 tests)

**Run with:** `PATH="test/scripts:$PATH" fpm test`

All unit tests use **mock curl responses** for fast, reliable testing without network dependencies.

#### S3 HTTP Operations (`test/test_s3_http.f90`) - 21 tests

**Core Operations:**
- `config_init` - Validates S3 configuration initialization and default values
- `get_object` - Tests successful object download from S3
- `get_object_not_found` - Verifies proper handling of 404 Not Found errors
- `object_exists_true` - Tests HEAD request for existing objects
- `object_exists_false` - Tests HEAD request for non-existent objects
- `get_object_content_validation` - Validates downloaded content integrity
- `put_object_without_auth` - Tests upload to public buckets without authentication
- `put_object_success` - Tests successful authenticated object upload
- `delete_object_without_auth` - Tests deletion in public buckets
- `delete_object_success` - Tests authenticated object deletion

**Protocol & Configuration:**
- `http_vs_https_protocols` - Verifies both HTTP and HTTPS protocol support
- `different_endpoints` - Tests various S3 endpoints (AWS, MinIO, custom)
- `path_style_config` - Validates path-style URL configuration
- `path_style_url_get` - Tests GET operations with path-style URLs

**Edge Cases & Error Handling:**
- `empty_key_edge_case` - Tests behavior with empty object keys
- `very_long_key_edge_case` - Tests handling of very long object keys
- `network_failure_scenarios` - Simulates and validates network failure handling
- `boundary_content_sizes` - Tests empty, small, and large content sizes
- `get_object_malformed_response` - Tests resilience to malformed S3 responses
- `auth_edge_cases` - Tests partial credentials and authentication edge cases

**S3 URI Support:**
- `s3_uri_parsing` - Validates parsing of `s3://bucket/key` URIs
- `s3_uri_operations` - Tests GET, PUT, DELETE operations with s3:// URIs

#### Uninitialized State Tests (`test/test_uninitialized_cases.f90`) - 4 tests

**Safety & Error Handling:**
- `get_object_uninitialized` - Ensures GET fails gracefully when library not initialized
- `object_exists_uninitialized` - Ensures HEAD fails gracefully when library not initialized
- `put_object_uninitialized` - Ensures PUT fails gracefully when library not initialized
- `delete_object_uninitialized` - Ensures DELETE fails gracefully when library not initialized

#### AWS Authentication Tests (`test/test_aws_auth.f90`) - 1 test

**Authentication:**
- `test_full_signature` - Validates complete AWS Signature v4 signing process with test vectors

#### Framework Validation (`test/test_math.f90`) - 1 test

- `basic_addition` - Sanity check to validate test-drive framework is working

### Integration Tests (8 tests)

Integration tests run against **real S3 services** (not mocks) to validate end-to-end functionality. **All integration tests now run automatically in CI** with every commit!

#### ✅ Runs in CI (Every Build)

**1. NetCDF Integration (`examples/netcdf_minimal.f90`)**
   - Downloads real CMIP6 climate data (~8MB NetCDF) from ESGF public S3 bucket
   - Writes to temp file (preferring `/dev/shm` RAM disk on Linux)
   - Opens and validates with NetCDF-Fortran library
   - Tests dimensions, variables, and attributes
   - **CI Job:** `NetCDF Integration Example (optional)`

**2. Simple S3 Operations (`app/test_simple.f90`)**
   - `s3_object_exists()` - Check if AWI climate NetCDF file exists on ESGF
   - `s3_get_object()` - Download small test file from ESGF
   - `s3_open()/s3_read_line()/s3_close()` - Fortran I/O interface
   - **CI Job:** `Integration Tests (Real S3 + MinIO)`

**3. Streaming Performance (`app/test_streaming.f90`)**
   - Detects platform capabilities (`is_streaming_available()`)
   - Downloads real S3 data with debug logging from ESGF
   - Counts temp files to verify zero-copy streaming
   - Compares streaming vs temp file fallback performance
   - **CI Job:** `Integration Tests (Real S3 + MinIO)`

**4. Direct File Download (`app/file_download_demo.f90`)** - Linux only
   - Tests libcurl direct-to-file streaming
   - Downloads from `httpbin.org` test endpoint
   - Verifies zero memory buffering (streams to disk)
   - **CI Job:** `Integration Tests (Real S3 + MinIO)`

**5. Progress Callbacks (`app/progress_demo.f90`)** - Linux only
   - Real-time download progress tracking
   - Progress bar, percentage, speed, ETA display
   - Validates callback cancellation support
   - **CI Job:** `Integration Tests (Real S3 + MinIO)`

**6. Authentication Demo (`app/auth_demo.f90`)**
   - Public ESGF bucket access (unauthenticated)
   - AWS Signature v4 header generation (example credentials)
   - MinIO/LocalStack configuration examples
   - **CI Job:** `Integration Tests (Real S3 + MinIO)` + `fpm run --verbose`

**7. MinIO Dedicated Test (`app/test_minio.f90`)**
   - Tests authenticated access to MinIO (localhost:9000)
   - Validates AWS Signature v4 with MinIO
   - Downloads small text files, nested paths, and ~1MB binary files
   - Tests `s3_object_exists()`, `s3_get_object()`, and `s3_open()/read/close`
   - **CI Job:** `Integration Tests (Real S3 + MinIO)`

**8. MinIO Service (automated in CI)**
   - MinIO service container running in CI
   - Bucket creation and test data upload via MinIO client (`mc`)
   - Test data: small/medium files, multiline text, nested paths
   - **CI Job:** `Integration Tests (Real S3 + MinIO)`

### Test Coverage Summary

**Unit Tests (27):** ✅ Core S3 operations, authentication, protocols, edge cases, error handling
**Integration Tests (8):** ✅ Real ESGF S3 data, NetCDF workflows, MinIO authenticated access, streaming, progress tracking
**CI Coverage:** ✅ Runs on every commit with gcc 11, 12, 13 on Linux
**Test Services:** ESGF CMIP6 climate data, httpbin.org endpoints, MinIO container (localhost)

**Total Coverage:**
- S3 operations (GET, PUT, DELETE, HEAD)
- AWS Signature v4 authentication (AWS S3 + MinIO)
- HTTP/HTTPS protocols
- Virtual-hosted and path-style URLs
- S3 URI parsing (`s3://bucket/key`)
- Network failures and malformed responses
- Uninitialized state protection
- NetCDF integration workflows
- Zero-copy streaming validation
- Progress callback functionality
- S3-compatible services (MinIO/LocalStack)

## Documentation

- [Development Guide](CLAUDE.md) - Setup and development instructions
- [API Documentation](https://pgierz.github.io/fortran-s3-accessor/) - Auto-generated docs

## Performance Notice

### Current Implementation (v1.1.0+)

**Linux (Production):**
- Direct libcurl integration via C bindings
- Zero-copy streaming to memory
- Full feature support including progress callbacks
- Excellent performance

**macOS (Development):**
- Automatic fallback to popen() subprocess streaming
- Good performance for development and testing
- No progress callbacks (libcurl ABI incompatibility)

The library automatically detects platform capabilities. No configuration required.

### Roadmap: v1.2.0 - Christmas Release 🎄

**Target: December 25, 2025** | [Milestone](https://github.com/pgierz/fortran-s3-accessor/milestone/1) | [All Issues](https://github.com/pgierz/fortran-s3-accessor/issues?q=is%3Aissue+milestone%3A%22v1.2.0+-+Christmas+Release+%F0%9F%8E%84%22)

**Recently Completed:**

1. ✅ **[libcurl integration](https://github.com/pgierz/fortran-s3-accessor/issues/9)** (#9) - Native performance for Linux
   - Direct C API via `iso_c_binding`
   - Zero-copy streaming on Linux
   - Better error diagnostics

2. ✅ **[AWS Signature v4 authentication](https://github.com/pgierz/fortran-s3-accessor/issues/10)** (#10) - Production-grade security
   - Full AWS authentication protocol
   - Private bucket access
   - All AWS regions
   - MinIO/LocalStack support

3. ✅ **[Progress callbacks](https://github.com/pgierz/fortran-s3-accessor/issues/11)** (#11) - Real-time transfer monitoring
   - Download/upload progress reporting
   - Easy callback interface (Linux only)

**Planned Enhancements:**

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

- **Platform support**: Linux only for production. macOS works for development (no progress callbacks)
- **URL encoding**: Special characters in S3 keys are untested - use alphanumeric keys and underscores only
- **Progress callbacks**: Only available on Linux (requires direct libcurl binding)

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
