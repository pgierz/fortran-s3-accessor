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

    ! Configure for ESGF public S3 bucket
    config%bucket = 'esgf-world'
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    config%access_key = ''  ! Public bucket
    config%secret_key = ''

    ! Initialize
    call s3_init(config)
    print *, 'Initialized S3 with bucket:', trim(config%bucket)

    ! Test 1: Check if NetCDF file exists
    print *
    print *, 'Test 1: Checking if AWI grid file exists...'
    exists = s3_object_exists('CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/fx/' // &
        'areacella/gn/v20200212/areacella_fx_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn.nc')
    if (exists) then
        print *, '  Grid file found!'
    else
        print *, '  Grid file not found'
    end if

    ! Test 2: Direct get object (small metadata file)
    print *
    print *, 'Test 2: Getting a small test file...'
    if (s3_get_object('test.txt', content)) then
        print *, '  Got content, length:', len(content)
        if (len(content) > 100) then
            print *, '  First 100 chars:', content(1:100)
        else
            print *, '  Content:', content
        end if
    else
        print *, '  Failed to get test file'
    end if

    ! Test 3: Using I/O interface
    print *
    print *, 'Test 3: Using I/O interface...'
    call s3_open(unit, 'test.txt', 'read', iostat)
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
        print *, '  Failed to open test file'
    end if

    print *
    print *, 'Tests completed!'

end program test_simple