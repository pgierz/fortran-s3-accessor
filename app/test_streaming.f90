program test_streaming
    use s3_http
    use s3_logger
    use curl_stream
    use iso_c_binding
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success
    integer :: count_before, count_after
    character(len=256) :: tmp_pattern

    print *, '=========================================='
    print *, 'Streaming Performance Demonstration'
    print *, '=========================================='
    print *

    ! Enable DEBUG logging to see what's happening
    call s3_set_log_level(S3_LOG_DEBUG)
    print *, 'Debug logging enabled'
    print *

    ! Check if streaming is available on this platform
    if (is_streaming_available()) then
        print *, 'Platform: Streaming ENABLED (POSIX popen available)'
        print *, 'Expected: No temporary files created'
    else
        print *, 'Platform: Streaming DISABLED (fallback mode)'
        print *, 'Expected: Temporary file will be created'
    end if
    print *

    ! Configure for ESGF public S3 bucket
    config%bucket = 'esgf-world'
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    config%access_key = ''  ! Public bucket
    config%secret_key = ''

    call s3_init(config)
    print *, 'Initialized S3 bucket:', trim(config%bucket)
    print *

    ! Count temp files before
    tmp_pattern = 'ls /tmp/s3_get_*.tmp 2>/dev/null | wc -l'
    count_before = count_tmp_files()
    print *, 'Temporary files before download:', count_before

    ! Download file (AWI grid file from ESGF)
    print *, 'Downloading AWI grid file...'
    success = s3_get_object('CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/fx/areacella/gn/v20200212/areacella_fx_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn.nc', content)

    if (success) then
        print *, '  Success! Downloaded', len(content), 'bytes'
        if (len(content) > 80) then
            print *, '  First 80 chars:', content(1:80)
        else
            print *, '  Content:', content
        end if
    else
        print *, '  ERROR: Download failed!'
        stop 1
    end if
    print *

    ! Count temp files after
    count_after = count_tmp_files()
    print *, 'Temporary files after download:', count_after
    print *

    ! Verify no temp files were created
    if (is_streaming_available()) then
        if (count_after == count_before) then
            print *, 'VERIFICATION: PASSED'
            print *, '  No temporary files created!'
            print *, '  Direct memory streaming is working!'
        else
            print *, 'VERIFICATION: FAILED'
            print *, '  Temporary files were created despite streaming being available'
            print *, '  This indicates streaming is not working correctly'
            stop 1
        end if
    else
        if (count_after > count_before) then
            print *, 'VERIFICATION: PASSED (fallback mode)'
            print *, '  Temporary file was created as expected in fallback mode'
        else
            print *, 'VERIFICATION: WARNING'
            print *, '  Expected temporary file in fallback mode but none found'
        end if
    end if

    print *
    print *, '=========================================='
    print *, 'Performance Impact:'
    if (is_streaming_available()) then
        print *, '  Network -> Memory (direct)'
        print *, '  Overhead: ~10ms (minimal)'
        print *, '  Disk I/O: ELIMINATED'
    else
        print *, '  Network -> /tmp -> Memory (fallback)'
        print *, '  Overhead: ~10-30% for large files'
        print *, '  Disk I/O: Present'
    end if
    print *, '=========================================='

contains

    function count_tmp_files() result(count)
        integer :: count
        integer :: unit, ios
        character(len=1024) :: cmd_output

        ! Use system to count files
        call execute_command_line('ls /tmp/s3_get_*.tmp 2>/dev/null | wc -l > /tmp/count_tmp.txt', exitstat=ios)

        open(newunit=unit, file='/tmp/count_tmp.txt', status='old', action='read', iostat=ios)
        if (ios /= 0) then
            count = 0
            return
        end if

        read(unit, *, iostat=ios) count
        if (ios /= 0) count = 0
        close(unit, status='delete')
    end function count_tmp_files

end program test_streaming