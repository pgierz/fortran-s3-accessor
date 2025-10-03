!> Direct file download demonstration
!>
!> Downloads data using libcurl and streams directly to file without
!> memory buffering. Ideal for large files (e.g., NetCDF) that need to
!> be on disk anyway.
!>
!> @note Linux only - requires libcurl direct binding
program file_download_demo
    use libcurl_bindings
    use s3_logger
    implicit none

    logical :: success
    character(len=512) :: test_url
    character(len=256) :: output_file
    integer :: file_size

    ! Simple test URL
    test_url = 'http://httpbin.org/bytes/1024'
    output_file = '/tmp/test_download.bin'

    print *, '=========================================='
    print *, 'Direct File Download Demonstration'
    print *, '=========================================='
    print *, ''
    print *, 'This demonstrates streaming directly to file'
    print *, 'without subprocess overhead or memory buffering.'
    print *, ''

    ! Check libcurl availability
    if (.not. is_libcurl_available()) then
        print *, 'ERROR: libcurl not available - direct file streaming requires libcurl'
        print *, 'This feature is Linux-only (libcurl/Fortran interop issues on macOS)'
        stop 1
    end if

    print *, 'URL:         ', trim(test_url)
    print *, 'Output file: ', trim(output_file)
    print *, ''

    ! Download directly to file
    print *, 'Downloading...'
    success = curl_get_to_file(trim(test_url), trim(output_file))

    print *, ''
    if (success) then
        ! Check file size
        inquire(file=output_file, size=file_size)
        print '(A)', 'Download successful!'
        print '(A,I0,A)', 'File size: ', file_size, ' bytes'
        print '(A,A)', 'Location: ', trim(output_file)
        print *, ''
        print *, 'Benefits:'
        print *, '  - No subprocess overhead (direct libcurl)'
        print *, '  - No memory buffering (streams to disk)'
        print *, '  - Perfect for NetCDF files that need to be on disk'
    else
        print *, 'Download failed!'
        stop 1
    end if

end program file_download_demo
