---
project: fortran-s3-accessor
summary: Simple Fortran library for S3-compatible object storage access
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
macro: TEST
       FORD
license: by-nc
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html

Brief description
=================

`fortran-s3-accessor` provides a simple, direct interface for accessing S3-compatible object storage from Fortran programs. The library offers:

- **Simple HTTP-based S3 operations**: GET, PUT, DELETE, and HEAD requests
- **Familiar Fortran interface**: Configuration through derived types
- **Comprehensive testing**: Mock-based testing framework with 22+ test cases
- **No external dependencies**: Uses only standard Fortran and system curl
- **Production ready**: Designed for integration with scientific computing workflows like FESOM

Architecture
============

The library consists of a single main module `s3_http` that provides direct curl-based access to S3 operations:

- `s3_config`: Configuration type for bucket, region, endpoint, and credentials
- `s3_init()`: Initialize the library with configuration
- `s3_get_object()`: Download objects from S3
- `s3_put_object()`: Upload objects to S3 (requires authentication)
- `s3_object_exists()`: Check if objects exist using HEAD requests
- `s3_delete_object()`: Delete objects from S3 (requires authentication)

Testing
=======

The library includes comprehensive testing using the test-drive framework:

- **Mock testing**: Uses PATH manipulation to replace curl with mock script
- **Error conditions**: Tests network failures, authentication errors, malformed responses
- **Edge cases**: Empty keys, very long keys, uninitialized state
- **Protocol support**: HTTP and HTTPS, different S3 endpoints

Limitations
===========

- **URL encoding**: Special characters in S3 keys are untested and unsupported
- **Authentication**: Uses simplified credential checking (production requires AWS Signature v4)
- **Dependencies**: Requires system curl command to be available

Usage Example
=============

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