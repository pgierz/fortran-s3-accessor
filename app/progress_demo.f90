!> Progress callback demonstration
!>
!> Shows real-time download progress with a simple progress bar
program progress_demo
    use iso_c_binding
    use s3_logger
    use libcurl_bindings
    implicit none

    type(curl_buffer_t) :: buffer
    logical :: success
    character(len=512) :: test_url

    ! Use a simple test URL first
    test_url = 'http://httpbin.org/bytes/1024'

    print *, '=========================================='
    print *, 'Download Progress Demonstration'
    print *, '=========================================='
    print *, ''
    print *, 'Downloading ~8MB test file from AWS Open Data...'
    print *, 'Source: 1000 Genomes Project (public dataset)'
    print *, ''

    ! Check libcurl availability
    if (.not. is_libcurl_available()) then
        print *, 'ERROR: libcurl not available - progress callbacks require libcurl'
        stop 1
    end if

    print *, 'URL: ', trim(test_url)
    print *, ''

    ! Download with progress callback
    print *, 'Starting download with progress callback...'
    print *, ''
    success = curl_get_to_buffer_with_progress(trim(test_url), buffer, my_progress_callback)

    print *, ''
    if (success) then
        print '(A,I0,A)', 'Download complete! Received ', buffer%size, ' bytes'
    else
        print *, 'Download failed!'
        stop 1
    end if

contains

    !> Progress callback function
    !>
    !> Called by libcurl during download to report progress
    !> Displays a simple text progress bar
    function my_progress_callback(dltotal, dlnow, ultotal, ulnow) result(abort) bind(C)
        integer(c_int64_t), value :: dltotal, dlnow, ultotal, ulnow
        integer(c_int64_t) :: abort

        real :: percent
        integer :: bar_width, filled_width, i
        character(len=50) :: bar

        abort = 0  ! Continue transfer

        ! Skip if total size unknown
        if (dltotal == 0) return

        ! Calculate percentage
        percent = 100.0 * real(dlnow) / real(dltotal)

        ! Create progress bar
        bar_width = 40
        filled_width = int(bar_width * percent / 100.0)

        bar = '['
        do i = 1, bar_width
            if (i <= filled_width) then
                bar = trim(bar) // '='
            else
                bar = trim(bar) // ' '
            end if
        end do
        bar = trim(bar) // ']'

        ! Print progress (using carriage return to overwrite line)
        write(*, '(A,A,F6.2,A,I0,A,I0,A)', advance='no') achar(13), trim(bar), &
            percent, '% (', dlnow, '/', dltotal, ' bytes)'

        ! Flush output
        flush(6)

    end function my_progress_callback

end program progress_demo
