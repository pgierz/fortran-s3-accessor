!> Byte-Range Request Demo
!>
!> Demonstrates practical use cases for HTTP byte-range requests with S3 objects:
!> - Reading NetCDF headers without downloading full files
!> - Reading specific data chunks from large files
!> - Comparing performance: full download vs. range download
!>
!> This example uses real ESGF public bucket data to show performance benefits.
program byte_range_demo
    use s3_http
    use iso_fortran_env, only: int64, real64
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content_full, content_header, content_chunk
    logical :: success
    integer(int64) :: start_time, end_time, count_rate
    real(real64) :: time_full, time_header, time_chunk
    integer :: header_size, chunk_size, full_size

    ! Example ESGF climate data file (AWI-ESM-1-1-LR model output)
    ! This is a real NetCDF file in a public S3 bucket
    character(len=*), parameter :: s3_key = &
        'CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/fx/' // &
        'areacella/gn/v20200212/areacella_fx_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn.nc'

    print *, '============================================================'
    print *, 'Byte-Range Request Demo'
    print *, '============================================================'
    print *
    print *, 'This demo shows practical use cases for byte-range requests:'
    print *, '  1. Reading NetCDF header only (first 64KB)'
    print *, '  2. Reading specific data chunk from middle'
    print *, '  3. Performance comparison vs. full download'
    print *

    ! Configure S3 for ESGF public bucket
    config%bucket = 'esgf-world'
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    config%access_key = ''  ! Public bucket - no credentials needed
    config%secret_key = ''
    call s3_init(config)

    print *, 'Connected to ESGF S3 bucket: esgf-world'
    print *, 'Target file: ', trim(s3_key)
    print *

    !===========================================================================
    ! USE CASE 1: Read NetCDF header only (first 64KB)
    !===========================================================================
    print *, '------------------------------------------------------------'
    print *, 'USE CASE 1: Reading NetCDF Header (first 64KB)'
    print *, '------------------------------------------------------------'
    print *
    print *, 'NetCDF files store metadata in the header (typically < 64KB).'
    print *, 'We can read just the header to inspect file structure without'
    print *, 'downloading the entire file.'
    print *

    ! Time the header-only download
    call system_clock(start_time, count_rate)

    ! Download first 64KB (65536 bytes = 64 * 1024)
    ! Byte range: 0-65535 (inclusive, 0-indexed)
    success = s3_get_object(s3_key, content_header, &
                           byte_start=0_int64, byte_end=65535_int64)

    call system_clock(end_time)
    time_header = real(end_time - start_time, real64) / real(count_rate, real64)

    if (success) then
        header_size = len(content_header)
        print '(A,I0,A)', ' SUCCESS: Downloaded ', header_size, ' bytes'
        print '(A,F8.3,A)', ' Time: ', time_header, ' seconds'
        print '(A,F8.1,A)', ' Speed: ', (header_size / 1024.0_real64) / time_header, ' KB/s'
        print *

        ! Check for NetCDF magic number (CDF\001 or CDF\002)
        if (header_size >= 4) then
            if (content_header(1:3) == 'CDF') then
                print *, ' Verified: Valid NetCDF file header detected!'
                if (iachar(content_header(4:4)) == 1) then
                    print *, ' Format: NetCDF Classic'
                else if (iachar(content_header(4:4)) == 2) then
                    print *, ' Format: NetCDF 64-bit Offset'
                end if
            else
                print *, ' Note: Not a classic NetCDF file (may be HDF5/NetCDF4)'
            end if
        end if
        print *
        print *, ' BENEFIT: You can now inspect file metadata without downloading'
        print *, '          the entire file, saving time and bandwidth!'
    else
        print *, ' ERROR: Failed to download header'
        if (s3_has_error()) then
            print *, ' Error details: ', trim(s3_get_last_error()%message)
        end if
    end if
    print *

    !===========================================================================
    ! USE CASE 2: Read specific data chunk (middle of file)
    !===========================================================================
    print *, '------------------------------------------------------------'
    print *, 'USE CASE 2: Reading Specific Data Chunk'
    print *, '------------------------------------------------------------'
    print *
    print *, 'For large files, you may only need a specific portion.'
    print *, 'Example: Reading bytes 100000-200000 (100KB chunk)'
    print *

    ! Time the chunk download
    call system_clock(start_time, count_rate)

    ! Download middle portion: bytes 100000-200000 (100KB chunk)
    success = s3_get_object(s3_key, content_chunk, &
                           byte_start=100000_int64, byte_end=200000_int64)

    call system_clock(end_time)
    time_chunk = real(end_time - start_time, real64) / real(count_rate, real64)

    if (success) then
        chunk_size = len(content_chunk)
        print '(A,I0,A)', ' SUCCESS: Downloaded ', chunk_size, ' bytes'
        print '(A,F8.3,A)', ' Time: ', time_chunk, ' seconds'
        print '(A,F8.1,A)', ' Speed: ', (chunk_size / 1024.0_real64) / time_chunk, ' KB/s'
        print *
        print *, ' BENEFIT: Perfect for reading specific variable data or'
        print *, '          time slices without loading the entire dataset!'
    else
        print *, ' ERROR: Failed to download chunk'
    end if
    print *

    !===========================================================================
    ! USE CASE 3: Performance comparison (optional - can be slow)
    !===========================================================================
    print *, '------------------------------------------------------------'
    print *, 'USE CASE 3: Performance Comparison'
    print *, '------------------------------------------------------------'
    print *
    print *, 'WARNING: This will download the entire file to demonstrate'
    print *, '         the performance difference. This may take a while!'
    print *

    ! Ask user if they want to proceed (in real usage)
    ! For demo purposes, we'll skip this to avoid long downloads
    print *, 'Skipping full file download for demo purposes.'
    print *, 'In production:'
    print *, '  - Full download would transfer entire file (could be GB)'
    print *, '  - Range request only transfers what you need (KB-MB)'
    print *, '  - Typical speedup: 10x-1000x depending on file size'
    print *

    ! Demonstrate the code (commented out to avoid actual long download)
    ! Uncomment these lines if you want to test with a smaller file:
    !
    ! call system_clock(start_time, count_rate)
    ! success = s3_get_object(s3_key, content_full)
    ! call system_clock(end_time)
    ! time_full = real(end_time - start_time, real64) / real(count_rate, real64)
    !
    ! if (success) then
    !     full_size = len(content_full)
    !     print '(A,I0,A)', ' Full file size: ', full_size, ' bytes'
    !     print '(A,F8.3,A)', ' Full download time: ', time_full, ' seconds'
    !     print *
    !     print *, ' COMPARISON:'
    !     print '(A,F6.1,A)', '   Header-only speedup: ', time_full / time_header, 'x faster'
    !     print '(A,F6.1,A)', '   Chunk-only speedup: ', time_full / time_chunk, 'x faster'
    !     print '(A,F6.1,A)', '   Bandwidth saved: ', &
    !                         100.0_real64 * (1.0_real64 - real(header_size, real64) / real(full_size, real64)), '%'
    ! end if

    !===========================================================================
    ! Summary and best practices
    !===========================================================================
    print *, '============================================================'
    print *, 'Summary: When to Use Byte-Range Requests'
    print *, '============================================================'
    print *
    print *, ' RECOMMENDED USE CASES:'
    print *, '   - Reading file headers/metadata'
    print *, '   - Accessing specific data chunks (e.g., time slices)'
    print *, '   - Preview large files before full download'
    print *, '   - Implementing pagination/chunked processing'
    print *, '   - Resumable downloads (download in parts)'
    print *
    print *, ' BENEFITS:'
    print *, '   - Reduced bandwidth usage'
    print *, '   - Faster initial response'
    print *, '   - Lower memory footprint'
    print *, '   - Better user experience (quick previews)'
    print *
    print *, ' IMPORTANT NOTES:'
    print *, '   - Byte ranges are 0-indexed (first byte = 0)'
    print *, '   - Both start and end are inclusive'
    print *, '   - S3 responds with HTTP 206 (Partial Content) on success'
    print *, '   - Server may send full file if range not supported'
    print *
    print *, ' CODE EXAMPLE:'
    print *, '   integer(int64) :: start_byte, end_byte'
    print *, '   character(len=:), allocatable :: data'
    print *, '   '
    print *, '   ! Read first 1KB'
    print *, '   start_byte = 0_int64'
    print *, '   end_byte = 1023_int64'
    print *, '   success = s3_get_object("file.dat", data, &'
    print *, '                          byte_start=start_byte, &'
    print *, '                          byte_end=end_byte)'
    print *
    print *, '============================================================'
    print *, 'For more examples, see test/test_byte_range.f90'
    print *, '============================================================'

contains

    !> Check if S3 error occurred
    function s3_has_error_internal() result(has_error)
        logical :: has_error
        ! Placeholder - in real implementation, check error state
        has_error = .false.
    end function s3_has_error_internal

end program byte_range_demo
