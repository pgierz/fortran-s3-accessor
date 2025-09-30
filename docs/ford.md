---
project: fortran-s3-accessor
summary: Fortran library providing S3-compatible object storage access with familiar I/O interfaces
author: Paul Gierz
github: https://github.com/pgierz/fortran-s3-accessor
email: pgierz@awi.de
src_dir: ../src
output_dir: ./generated
page_dir: ./pages
media_dir: ./media
docmark: !
predocmark: >
display: public
         protected
         private
source: true
graph: true
preprocess: false
macro: TEST
       FORD
license: by-nc
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html

Brief description
=================

`fortran-s3-accessor` provides a simple, direct interface for accessing S3-compatible object storage from Fortran programs. The library is designed for scientific computing workflows that need to read and write data to cloud object storage with minimal friction.

## Key Features

- **Direct HTTP-based S3 operations**: GET, PUT, DELETE, and HEAD requests via curl
- **Dual interface design**:
  - Low-level `s3_http` module for direct S3 operations
  - High-level `s3_io` module providing familiar Fortran I/O patterns (open/read/write/close)
- **URI support**: Work with `s3://bucket/key` URIs for seamless cross-bucket operations
- **Public bucket access**: Read from public S3 buckets without authentication
- **Comprehensive testing**: Mock-based testing framework with 22+ test cases
- **Zero dependencies**: Uses only standard Fortran 2008 and system curl
- **Production ready**: Designed for integration with scientific computing workflows (FESOM, climate models, etc.)

Architecture
============

The library follows a layered architecture with two main modules:

## Core Module: `s3_http`

Provides direct curl-based HTTP access to S3 operations:

- **`s3_config`**: Configuration type containing bucket name, region, endpoint, credentials, and protocol settings
- **`s3_init(config)`**: Initialize the library with an S3 configuration
- **`s3_get_object(key, content)`**: Download object content from S3
- **`s3_put_object(key, content)`**: Upload object content to S3 (requires authentication)
- **`s3_object_exists(key)`**: Check if an object exists using HTTP HEAD requests
- **`s3_delete_object(key)`**: Delete an object from S3 (requires authentication)

### URI-based Operations

For convenience, the library provides URI-aware versions of all operations:

- **`s3_get_uri(uri, content)`**: Get object using `s3://bucket/key` format
- **`s3_put_uri(uri, content)`**: Put object using URI
- **`s3_exists_uri(uri)`**: Check existence using URI
- **`s3_delete_uri(uri)`**: Delete object using URI

These functions automatically parse the bucket name from the URI and temporarily switch contexts when accessing different buckets.

## High-level Module: `s3_io`

Provides Fortran-like I/O interface built on top of `s3_http`:

- **`s3_open(unit, key, mode, iostat)`**: Open an S3 object for reading or writing
- **`s3_close(unit, iostat)`**: Close the S3 object (uploads on write mode)
- **`s3_read_line(unit, line, iostat)`**: Read a line from the object
- **`s3_write_line(unit, line, iostat)`**: Write a line to the object buffer
- **`s3_rewind(unit, iostat)`**: Rewind read position to beginning

This module manages up to 100 concurrent file handles with internal buffering.

Testing
=======

The library includes comprehensive testing infrastructure using the [test-drive](https://github.com/fortran-lang/test-drive) framework:

## Test Coverage

- **Mock-based testing**: PATH manipulation replaces curl with controllable mock script
- **Error condition testing**: Network failures, authentication errors, HTTP error codes, malformed responses
- **Edge case testing**: Empty keys, very long keys, special characters, boundary conditions
- **Uninitialized state testing**: Separate executable ensures proper isolation
- **Protocol testing**: Both HTTP and HTTPS, various S3-compatible endpoints
- **URI parsing tests**: Complex URIs, edge cases in bucket/key extraction

## Running Tests

```bash
# Run all tests with mock curl
PATH="test/scripts:$PATH" fpm test

# Tests are defined in test/test_s3_http.f90
# Mock responses stored in test/data/responses/
```

Use Cases
=========

## Scientific Data Access

Access climate model output, observational data, and analysis results stored in S3:

- Read NetCDF files from ESGF data nodes
- Process CMIP6 climate model output
- Access NOAA, NASA, and other public scientific datasets
- Integrate with existing Fortran scientific workflows

## Public Bucket Integration

The library excels at reading from public S3 buckets commonly used in scientific computing:

- NOAA Global Forecast System (GFS) data
- NASA Earth observation data
- CMIP6 climate model archives
- Genomics datasets (1000 Genomes, NIH)

Limitations
===========

- **URL encoding**: Special characters in S3 keys are currently untested; use alphanumeric keys with underscores for safety
- **Authentication**: Uses simplified credential checking; production write operations require AWS Signature v4 (planned)
- **Binary data**: Current implementation optimized for text; binary support exists but is less tested
- **Dependencies**: Requires system `curl` command to be available in PATH
- **Parallel I/O**: No built-in support for concurrent multi-threaded access

Examples
========

## Example 1: Basic Object Download

Download a text file from a public S3 bucket:

```fortran
program basic_download
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success

    ! Configure for public NOAA bucket
    config%bucket = 'noaa-gfs-bdp-pds'
    config%region = 'us-east-1'
    config%endpoint = 's3.amazonaws.com'
    config%use_https = .true.
    call s3_init(config)

    ! Download README
    success = s3_get_object('README.md', content)
    if (success) then
        print *, 'Downloaded ', len(content), ' bytes'
        print *, content
    else
        print *, 'Download failed'
    end if
end program basic_download
```

## Example 2: Check Object Existence

Verify an object exists before attempting to download:

```fortran
program check_exists
    use s3_http
    implicit none

    type(s3_config) :: config
    logical :: exists

    config%bucket = 'my-bucket'
    config%use_https = .true.
    call s3_init(config)

    exists = s3_object_exists('data/input.nc')
    if (exists) then
        print *, 'File exists, proceeding with download...'
    else
        print *, 'File not found, using default data'
    end if
end program check_exists
```

## Example 3: Using Fortran I/O Interface

Read an S3 object line by line like a regular file:

```fortran
program read_lines
    use s3_http
    use s3_io
    implicit none

    type(s3_config) :: config
    integer :: unit, iostat, line_count
    character(len=1024) :: line

    ! Initialize
    config%bucket = 'my-data-bucket'
    call s3_init(config)

    ! Open S3 object
    call s3_open(unit, 'data/measurements.csv', 'read', iostat)
    if (iostat /= 0) stop 'Failed to open file'

    ! Read all lines
    line_count = 0
    do
        call s3_read_line(unit, line, iostat)
        if (iostat /= 0) exit
        line_count = line_count + 1
        print *, 'Line ', line_count, ': ', trim(line)
    end do

    call s3_close(unit, iostat)
    print *, 'Read ', line_count, ' lines'
end program read_lines
```

## Example 4: URI-based Access Across Buckets

Use `s3://` URIs to seamlessly access multiple buckets:

```fortran
program uri_access
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success

    ! Initialize with default bucket
    config%bucket = 'default-bucket'
    config%use_https = .true.
    call s3_init(config)

    ! Access different bucket via URI
    success = s3_get_uri('s3://other-bucket/path/to/data.txt', content)
    if (success) then
        print *, 'Content from other-bucket: ', content
    end if

    ! Original bucket still accessible
    success = s3_get_object('local-data.txt', content)
end program uri_access
```

## Example 5: NetCDF Climate Data

Download and process NetCDF files from ESGF climate data archives:

```fortran
program climate_data
    use s3_http
    use netcdf
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: nc_data
    character(len=*), parameter :: climate_uri = &
        's3://esgf-world/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/fx/areacella/gn/v20200212/' // &
        'areacella_fx_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn.nc'
    logical :: success

    ! Configure for public ESGF bucket
    config%use_https = .true.
    call s3_init(config)

    ! Download NetCDF file
    success = s3_get_uri(climate_uri, nc_data)
    if (success) then
        ! Write to temp file and process with NetCDF library
        open(10, file='/tmp/climate.nc', form='unformatted', access='stream')
        write(10) nc_data
        close(10)

        ! Now use NetCDF library to read the file
        print *, 'Climate data downloaded, ready for processing'
    end if
end program climate_data
```

## Example 6: Writing Data (with Authentication)

Upload data to S3 (requires AWS credentials):

```fortran
program write_data
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: data
    logical :: success

    ! Configure with credentials
    config%bucket = 'my-output-bucket'
    config%access_key = 'AKIAIOSFODNN7EXAMPLE'
    config%secret_key = 'wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY'
    config%use_https = .true.
    call s3_init(config)

    ! Create data
    data = 'simulation results: temp=25.3, pressure=1013.2'

    ! Upload to S3
    success = s3_put_object('results/output.txt', data)
    if (success) then
        print *, 'Data uploaded successfully'
    else
        print *, 'Upload failed'
    end if
end program write_data
```

Getting Started
===============

1. **Clone the repository**:
   ```bash
   git clone https://github.com/pgierz/fortran-s3-accessor.git
   ```

2. **Build with FPM**:
   ```bash
   fpm build
   fpm test  # Run tests
   ```

3. **Or build with CMake**:
   ```bash
   mkdir build && cd build
   cmake .. -DCMAKE_BUILD_TYPE=Release
   make -j
   ```

4. **Run examples**:
   ```bash
   fpm run test_simple              # Basic operations
   fpm run --example s3_netcdf_example  # NetCDF example
   ```

Integration
===========

To use in your project with FPM, add to `fpm.toml`:

```toml
[dependencies]
fortran-s3-accessor = { git = "https://github.com/pgierz/fortran-s3-accessor.git" }
```

Then in your code:

```fortran
use s3_http        ! For direct S3 operations
use s3_io          ! For Fortran I/O interface
```