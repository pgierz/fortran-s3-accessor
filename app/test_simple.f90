program test_simple
    use s3_http
    use s3_io
    implicit none

    type(s3_config) :: config
    integer :: unit, iostat, i
    character(len=1024) :: line
    logical :: exists
    character(len=:), allocatable :: content

    print *, 'Simple S3 Accessor Test'
    print *, '======================='

    ! Configure for NOAA public S3 bucket
    config%bucket = 'noaa-gfs-bdp-pds'
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    config%access_key = ''  ! Public bucket
    config%secret_key = ''

    ! Initialize
    call s3_init(config)
    print *, 'Initialized S3 with bucket:', trim(config%bucket)

    ! Test 1: Check if object exists
    print *
    print *, 'Test 1: Checking if README.md exists...'
    exists = s3_object_exists('README.md')
    if (exists) then
        print *, '  README.md found!'
    else
        print *, '  README.md not found'
    end if

    ! Test 2: Direct get object
    print *
    print *, 'Test 2: Getting README.md directly...'
    if (s3_get_object('README.md', content)) then
        print *, '  Got content, length:', len(content)
        if (len(content) > 100) then
            print *, '  First 100 chars:', content(1:100)
        else
            print *, '  Content:', content
        end if
    else
        print *, '  Failed to get README.md'
    end if

    ! Test 3: Using I/O interface
    print *
    print *, 'Test 3: Using I/O interface to read README.md...'
    call s3_open(unit, 'README.md', 'read', iostat)
    if (iostat == 0) then
        print *, '  Opened file, unit:', unit

        ! Read first 3 lines
        do i = 1, 3
            call s3_read_line(unit, line, iostat)
            if (iostat /= 0) exit
            print *, '  Line', i, ':', trim(line)
        end do

        call s3_close(unit, iostat)
        print *, '  File closed'
    else
        print *, '  Failed to open README.md'
    end if

    print *
    print *, 'Tests completed!'

end program test_simple