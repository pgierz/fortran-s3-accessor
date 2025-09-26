!> Example program demonstrating S3 accessor functionality
program s3_example
    use fortran_s3_accessor
    implicit none

    type(s3_config_t) :: config
    integer :: unit, ios, i
    character(len=1024) :: line
    character(len=:), allocatable :: version
    logical :: exists

    ! Print version info
    call print_version_info()
    print *

    ! Show available backends
    print *, 'Available backends:'
    call list_available_backends()
    print *

    ! Test 1: Local filesystem mode (mock backend)
    print *, '========================================='
    print *, 'Test 1: Local filesystem mode'
    print *, '========================================='

    config%use_s3 = .false.
    config%backend_type = 'mock'

    call s3_initialize(config, ios)
    if (ios /= 0) then
        print *, 'ERROR: Failed to initialize with mock backend'
        stop 1
    end if

    ! Write a test file
    unit = s3_get_unit()
    call s3_open(unit, 'test_local.txt', mode='write', iostat=ios)
    if (ios == 0) then
        call s3_write(unit, 'Line 1: This is a test file', iostat=ios)
        call s3_write(unit, 'Line 2: Written using mock backend', iostat=ios)
        call s3_write(unit, 'Line 3: Stored locally', iostat=ios)
        call s3_close(unit, iostat=ios)
        print *, 'Created test_local.txt'
    else
        print *, 'ERROR: Could not create test file'
    end if

    ! Read it back
    call s3_open(unit, 'test_local.txt', mode='read', iostat=ios)
    if (ios == 0) then
        print *, 'Reading test_local.txt:'
        do i = 1, 3
            call s3_read(unit, line, iostat=ios)
            if (ios /= 0) exit
            print *, '  ', trim(line)
        end do
        call s3_close(unit)
    end if

    call s3_finalize()
    print *

    ! Test 2: HTTP backend with NOAA public S3 data
    print *, '========================================='
    print *, 'Test 2: HTTP backend with NOAA S3 data'
    print *, '========================================='

    config%use_s3 = .true.
    config%backend_type = 'http'
    config%bucket = 'noaa-ghcn-pcd'  ! NOAA Global Historical Climatology Network
    config%region = 'us-east-1'
    config%endpoint = ''  ! Use default AWS endpoint
    config%use_ssl = .true.
    config%access_key = ''  ! Public bucket, no auth needed
    config%secret_key = ''

    print *, 'Initializing HTTP backend for NOAA S3 bucket...'
    call s3_initialize(config, ios)
    if (ios /= 0) then
        print *, 'ERROR: Failed to initialize HTTP backend'
        stop 1
    end if

    ! Check if a known file exists
    print *, 'Checking for README.md in NOAA bucket...'
    exists = s3_file_exists('README.md')
    if (exists) then
        print *, 'README.md found in bucket'

        ! Try to read it
        unit = s3_get_unit()
        call s3_open(unit, 'README.md', mode='read', iostat=ios)
        if (ios == 0) then
            print *, 'First few lines of README.md:'
            do i = 1, 5
                call s3_read(unit, line, iostat=ios)
                if (ios /= 0) exit
                print *, '  ', trim(line)
            end do
            call s3_close(unit)
        else
            print *, 'Could not read README.md, status:', ios
        end if
    else
        print *, 'README.md not found (may need different path or bucket)'
    end if

    call s3_finalize()
    print *

    ! Test 3: File operations wrapper
    print *, '========================================='
    print *, 'Test 3: File operations wrapper'
    print *, '========================================='

    ! Initialize for local mode
    config%use_s3 = .false.
    config%backend_type = 'mock'
    call s3_accessor_init(config, ios)

    ! Use file operations
    print *, 'Using file_* operations...'
    call file_open('test_ops.txt', 'write', unit, ios)
    if (ios == 0) then
        call file_write(unit, 'Testing file operations', ios)
        call file_write(unit, 'This uses the wrapper functions', ios)
        call file_close(unit, ios)
        print *, 'Created test_ops.txt'
    end if

    ! Check if file exists
    if (file_exists('test_ops.txt')) then
        print *, 'test_ops.txt exists'

        ! Copy it
        call file_copy('test_ops.txt', 'test_ops_copy.txt', ios)
        if (ios == 0) then
            print *, 'Copied to test_ops_copy.txt'
        end if

        ! Read the copy
        call file_open('test_ops_copy.txt', 'read', unit, ios)
        if (ios == 0) then
            print *, 'Contents of copy:'
            do
                call file_read(unit, line, ios)
                if (ios /= 0) exit
                print *, '  ', trim(line)
            end do
            call file_close(unit)
        end if
    end if

    call s3_finalize()

    print *
    print *, '========================================='
    print *, 'All tests completed'
    print *, '========================================='

end program s3_example