title: Examples Catalog
author: Paul Gierz

# Examples Catalog

This page provides a comprehensive catalog of examples demonstrating how to use the fortran-s3-accessor library for various tasks.

## Table of Contents

- [Basic Operations](#basic-operations)
- [Advanced Usage](#advanced-usage)
- [Scientific Computing Examples](#scientific-computing-examples)
- [Error Handling](#error-handling)
- [Performance Tips](#performance-tips)

---

## Basic Operations

### Example 1: Simple File Download

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

**Use Case**: Accessing documentation or metadata files from public S3 buckets.

---

### Example 2: Check Object Existence

Verify an object exists before attempting to download:

```fortran
program check_exists
    use s3_http
    implicit none

    type(s3_config) :: config
    logical :: exists
    character(len=:), allocatable :: content

    config%bucket = 'my-data-bucket'
    config%use_https = .true.
    call s3_init(config)

    ! Check if file exists first
    exists = s3_object_exists('data/input.nc')

    if (exists) then
        print *, 'File exists, downloading...'
        if (s3_get_object('data/input.nc', content)) then
            print *, 'Successfully downloaded data'
        end if
    else
        print *, 'File not found, using default data'
    end if
end program check_exists
```

**Use Case**: Conditional processing based on file availability, fallback to defaults.

---

### Example 3: Line-by-Line File Reading

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
    config%use_https = .true.
    call s3_init(config)

    ! Open S3 object for reading
    call s3_open(unit, 'data/measurements.csv', 'read', iostat)
    if (iostat /= 0) then
        print *, 'Error: Failed to open file'
        stop 1
    end if

    ! Read all lines
    line_count = 0
    do
        call s3_read_line(unit, line, iostat)
        if (iostat /= 0) exit
        line_count = line_count + 1
        print *, 'Line ', line_count, ': ', trim(line)
    end do

    call s3_close(unit, iostat)
    print *, 'Total lines read: ', line_count
end program read_lines
```

**Use Case**: Processing CSV files, configuration files, or log files stored in S3.

---

### Example 4: Writing Data to S3

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

    ! Create simulation results
    data = 'simulation_id: 12345' // new_line('') // &
           'temperature: 25.3' // new_line('') // &
           'pressure: 1013.2' // new_line('') // &
           'timestamp: 2025-09-30T10:00:00Z'

    ! Upload to S3
    success = s3_put_object('results/sim_12345.txt', data)
    if (success) then
        print *, 'Results uploaded successfully'
    else
        print *, 'Upload failed'
    end if
end program write_data
```

**Use Case**: Saving simulation results, logging outputs, archiving processed data.

---

### Example 5: Line-by-Line Writing

Write data line by line using the I/O interface:

```fortran
program write_csv
    use s3_http
    use s3_io
    implicit none

    type(s3_config) :: config
    integer :: unit, iostat, i
    real :: temperature, pressure

    ! Initialize
    config%bucket = 'my-output-bucket'
    config%access_key = 'YOUR_ACCESS_KEY'
    config%secret_key = 'YOUR_SECRET_KEY'
    call s3_init(config)

    ! Open for writing
    call s3_open(unit, 'results/measurements.csv', 'write', iostat)
    if (iostat /= 0) stop 'Failed to open file for writing'

    ! Write CSV header
    call s3_write_line(unit, 'time,temperature,pressure', iostat)

    ! Write measurement data
    do i = 1, 10
        temperature = 20.0 + real(i) * 0.5
        pressure = 1013.0 + real(i) * 0.1
        write(line, '(I0,",",F5.1,",",F6.1)') i, temperature, pressure
        call s3_write_line(unit, trim(line), iostat)
    end do

    ! Close and upload
    call s3_close(unit, iostat)
    if (iostat == 0) then
        print *, 'CSV file uploaded successfully'
    end if
end program write_csv
```

**Use Case**: Creating CSV outputs, generating reports, logging time-series data.

---

## Advanced Usage

### Example 6: URI-based Multi-Bucket Access

Use `s3://` URIs to seamlessly access multiple buckets:

```fortran
program multi_bucket
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: config_content, data_content
    logical :: success

    ! Initialize with default bucket
    config%bucket = 'default-bucket'
    config%use_https = .true.
    call s3_init(config)

    ! Read configuration from one bucket
    success = s3_get_uri('s3://config-bucket/app/config.txt', config_content)
    if (success) then
        print *, 'Configuration loaded from config-bucket'
    end if

    ! Read data from another bucket
    success = s3_get_uri('s3://data-bucket/input/dataset.txt', data_content)
    if (success) then
        print *, 'Dataset loaded from data-bucket'
    end if

    ! Still can access default bucket
    success = s3_get_object('local-file.txt', data_content)
end program multi_bucket
```

**Use Case**: Applications that need to access resources across multiple S3 buckets.

---

### Example 7: Rewind and Re-read

Rewind a file and read it multiple times:

```fortran
program rewind_example
    use s3_http
    use s3_io
    implicit none

    type(s3_config) :: config
    integer :: unit, iostat, pass
    character(len=1024) :: line

    config%bucket = 'my-bucket'
    call s3_init(config)

    call s3_open(unit, 'data/config.txt', 'read', iostat)

    ! First pass: count lines
    do pass = 1, 2
        if (pass == 2) then
            print *, 'Rewinding for second pass...'
            call s3_rewind(unit, iostat)
        end if

        print *, 'Pass ', pass
        do
            call s3_read_line(unit, line, iostat)
            if (iostat /= 0) exit
            print *, '  ', trim(line)
        end do
    end do

    call s3_close(unit, iostat)
end program rewind_example
```

**Use Case**: Two-pass file processing, counting then processing, or multiple analyses.

---

### Example 8: HTTP vs HTTPS

Configure for different protocols:

```fortran
program protocol_example
    use s3_http
    implicit none

    type(s3_config) :: config
    logical :: success
    character(len=:), allocatable :: content

    ! HTTPS (recommended for production)
    config%bucket = 'secure-bucket'
    config%use_https = .true.
    config%endpoint = 's3.amazonaws.com'
    call s3_init(config)
    success = s3_get_object('secure-data.txt', content)

    ! HTTP (for local testing or MinIO)
    config%bucket = 'local-bucket'
    config%use_https = .false.
    config%endpoint = 'localhost:9000'
    call s3_init(config)
    success = s3_get_object('test-data.txt', content)
end program protocol_example
```

**Use Case**: Local development with MinIO, testing, or specialized endpoints.

---

## Scientific Computing Examples

### Example 9: NetCDF Climate Data

Download and process NetCDF files from ESGF climate data archives:

```fortran
program climate_data
    use s3_http
    use netcdf
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: nc_data
    character(len=*), parameter :: climate_uri = &
        's3://esgf-world/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/' // &
        'r1i1p1f1/fx/areacella/gn/v20200212/' // &
        'areacella_fx_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn.nc'
    logical :: success
    integer :: ncid, status

    ! Configure for public ESGF bucket
    config%endpoint = 's3.amazonaws.com'
    config%use_https = .true.
    call s3_init(config)

    print *, 'Downloading NetCDF from ESGF...'
    success = s3_get_uri(climate_uri, nc_data)

    if (.not. success) then
        print *, 'Error: Failed to download climate data'
        stop 1
    end if

    ! Write to temp file for NetCDF processing
    open(10, file='/tmp/climate.nc', form='unformatted', &
         access='stream', status='replace')
    write(10) nc_data
    close(10)

    ! Process with NetCDF library
    status = nf90_open('/tmp/climate.nc', NF90_NOWRITE, ncid)
    if (status == NF90_NOERR) then
        print *, 'NetCDF file opened successfully'
        ! ... process NetCDF data ...
        status = nf90_close(ncid)
    end if

    ! Clean up
    open(10, file='/tmp/climate.nc', status='old')
    close(10, status='delete')
end program climate_data
```

**Use Case**: Accessing CMIP6 climate model output, ESGF datasets, remote NetCDF files.

---

### Example 10: Processing NOAA GFS Data

Access real-time weather forecast data from NOAA:

```fortran
program gfs_example
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: readme_content
    logical :: file_exists, success

    ! Configure for NOAA GFS bucket
    config%bucket = 'noaa-gfs-bdp-pds'
    config%region = 'us-east-1'
    config%endpoint = 's3.amazonaws.com'
    config%use_https = .true.
    call s3_init(config)

    print *, 'Accessing NOAA GFS bucket...'

    ! Check if README exists
    file_exists = s3_object_exists('README.md')
    if (file_exists) then
        print *, 'Bucket is accessible'

        ! Download README
        success = s3_get_object('README.md', readme_content)
        if (success) then
            print *, 'Downloaded README:'
            print *, readme_content
        end if
    else
        print *, 'Cannot access NOAA GFS bucket'
    end if
end program gfs_example
```

**Use Case**: Weather forecasting applications, meteorological research, atmospheric modeling.

---

### Example 11: Batch Processing Multiple Files

Download and process multiple files in a loop:

```fortran
program batch_processing
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    character(len=256) :: key
    integer :: i, file_count, success_count
    logical :: success

    config%bucket = 'simulation-results'
    call s3_init(config)

    file_count = 10
    success_count = 0

    print *, 'Processing ', file_count, ' files...'

    do i = 1, file_count
        ! Construct key name
        write(key, '(A,I0.4,A)') 'results/output_', i, '.txt'

        ! Download and process
        success = s3_get_object(trim(key), content)
        if (success) then
            success_count = success_count + 1
            print *, 'Processed: ', trim(key)
            ! ... process content ...
        else
            print *, 'Failed: ', trim(key)
        end if
    end do

    print *, 'Successfully processed ', success_count, ' of ', file_count, ' files'
end program batch_processing
```

**Use Case**: Post-processing simulation outputs, batch data analysis, aggregation tasks.

---

## Error Handling

### Example 12: Comprehensive Error Handling

Robust error handling for production code:

```fortran
program error_handling
    use s3_http
    use s3_io
    implicit none

    type(s3_config) :: config
    integer :: unit, iostat
    character(len=1024) :: line
    logical :: success, exists

    ! Initialize configuration
    config%bucket = 'my-bucket'
    config%region = 'us-east-1'
    config%use_https = .true.
    call s3_init(config)

    ! Check existence before downloading
    exists = s3_object_exists('data/input.txt')
    if (.not. exists) then
        print *, 'ERROR: Input file does not exist in S3'
        print *, 'Please check bucket name and object key'
        stop 1
    end if

    ! Try to open file
    call s3_open(unit, 'data/input.txt', 'read', iostat)
    if (iostat /= 0) then
        print *, 'ERROR: Failed to open S3 object'
        print *, 'Check network connectivity and credentials'
        stop 1
    end if

    print *, 'File opened successfully, unit: ', unit

    ! Read with error handling
    do
        call s3_read_line(unit, line, iostat)
        if (iostat /= 0) then
            ! Could be EOF or error
            exit
        end if

        ! Process line
        print *, trim(line)
    end do

    ! Close with error checking
    call s3_close(unit, iostat)
    if (iostat /= 0) then
        print *, 'WARNING: Error closing file'
    else
        print *, 'File closed successfully'
    end if

    print *, 'Processing complete'
end program error_handling
```

**Use Case**: Production code requiring robust error handling and clear diagnostics.

---

### Example 13: Upload with Verification

Upload data and verify it was written correctly:

```fortran
program verified_upload
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: original_data, downloaded_data
    logical :: upload_success, exists
    character(len=*), parameter :: key = 'results/verified_output.txt'

    ! Configure with credentials
    config%bucket = 'my-bucket'
    config%access_key = 'YOUR_ACCESS_KEY'
    config%secret_key = 'YOUR_SECRET_KEY'
    call s3_init(config)

    ! Prepare data
    original_data = 'Important results: value=42'

    ! Upload
    print *, 'Uploading data...'
    upload_success = s3_put_object(key, original_data)

    if (.not. upload_success) then
        print *, 'ERROR: Upload failed'
        stop 1
    end if

    print *, 'Upload reported success, verifying...'

    ! Verify existence
    exists = s3_object_exists(key)
    if (.not. exists) then
        print *, 'ERROR: File does not exist after upload'
        stop 1
    end if

    ! Download and compare
    upload_success = s3_get_object(key, downloaded_data)
    if (.not. upload_success) then
        print *, 'ERROR: Could not download for verification'
        stop 1
    end if

    if (downloaded_data == original_data) then
        print *, 'SUCCESS: Upload verified correctly'
    else
        print *, 'ERROR: Downloaded data does not match'
        print *, 'Original: ', original_data
        print *, 'Downloaded: ', downloaded_data
        stop 1
    end if
end program verified_upload
```

**Use Case**: Critical data uploads requiring verification, audit trails, data integrity checks.

---

## Performance Tips

### Example 14: Efficient Large File Handling

Tips for handling large files:

```fortran
program large_file_example
    use s3_http
    use s3_io
    implicit none

    type(s3_config) :: config
    integer :: unit, iostat, line_count, chunk_size
    character(len=10000) :: line  ! Large buffer for long lines

    config%bucket = 'big-data-bucket'
    call s3_init(config)

    ! Open large file
    call s3_open(unit, 'data/large_dataset.csv', 'read', iostat)
    if (iostat /= 0) stop 'Failed to open file'

    ! Process in chunks
    line_count = 0
    chunk_size = 1000

    do
        call s3_read_line(unit, line, iostat)
        if (iostat /= 0) exit

        line_count = line_count + 1

        ! Process line
        ! ... your processing code ...

        ! Status update every chunk
        if (mod(line_count, chunk_size) == 0) then
            print *, 'Processed ', line_count, ' lines...'
        end if
    end do

    call s3_close(unit, iostat)
    print *, 'Total lines processed: ', line_count
end program large_file_example
```

**Performance Tips**:
- Use appropriate buffer sizes for your data
- Process data in chunks for large files
- Consider memory constraints when allocating content buffers
- The entire file is downloaded at open time, so opening is the slowest operation

---

### Example 15: Caching Configuration

Reuse configuration for multiple operations:

```fortran
program config_reuse
    use s3_http
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    integer :: i
    character(len=256) :: key

    ! Configure once
    config%bucket = 'my-bucket'
    config%region = 'us-east-1'
    config%use_https = .true.
    call s3_init(config)

    ! Reuse configuration for multiple operations
    do i = 1, 100
        write(key, '(A,I0,A)') 'data/file_', i, '.txt'

        if (s3_object_exists(trim(key))) then
            if (s3_get_object(trim(key), content)) then
                ! Process content
            end if
        end if
    end do
end program config_reuse
```

**Performance Tips**:
- Initialize configuration once and reuse
- Batch operations when possible
- Check existence before downloading if appropriate
- Consider parallel processing for independent operations

---

## Summary

This examples catalog covers:

1. **Basic Operations**: Download, upload, existence checks
2. **I/O Interface**: Line-by-line reading and writing
3. **Advanced Features**: URI support, multi-bucket access, rewind
4. **Scientific Computing**: NetCDF files, NOAA data, climate datasets
5. **Error Handling**: Robust error checking and verification
6. **Performance**: Efficient large file handling and configuration reuse

For more information, see:
- [API Documentation](../lists/procedures.html)
- [Module s3_http](../module/s3_http.html)
- [Module s3_io](../module/s3_io.html)