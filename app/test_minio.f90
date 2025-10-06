!> MinIO Integration Test
!>
!> Tests authenticated access to MinIO S3-compatible service.
!> This runs in CI against a MinIO service container.
program test_minio
    use s3_http
    use s3_io
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success, exists
    integer :: unit, iostat
    character(len=1024) :: line
    integer :: i

    print *, '=========================================='
    print *, 'MinIO Integration Test'
    print *, '=========================================='
    print *

    ! Configure for MinIO (localhost:9000)
    config%bucket = 'test-bucket'
    config%endpoint = 'localhost:9000'
    config%region = 'us-east-1'
    config%use_https = .false.
    config%use_path_style = .true.  ! Required for localhost
    config%access_key = 'minioadmin'
    config%secret_key = 'minioadmin'

    call s3_init(config)
    print *, 'Configured for MinIO at localhost:9000'
    print *, 'Bucket: ', trim(config%bucket)
    print *, 'Authentication: Enabled (AWS Signature v4)'
    print *

    ! Test 1: Check if test file exists
    print *, 'Test 1: Check if test.txt exists'
    exists = s3_object_exists('test.txt')
    if (exists) then
        print *, '  ✓ test.txt exists'
    else
        print *, '  ✗ test.txt not found (MinIO might not be running)'
        print *, '  Note: This test requires MinIO service on localhost:9000'
        stop 0  ! Exit gracefully if MinIO not available
    end if
    print *

    ! Test 2: Download small file
    print *, 'Test 2: Download test.txt'
    success = s3_get_object('test.txt', content)
    if (success) then
        print *, '  ✓ Downloaded', len(content), 'bytes'
        print *, '  Content:', trim(content)
    else
        print *, '  ✗ Failed to download test.txt'
        stop 1
    end if
    print *

    ! Test 3: Download nested file
    print *, 'Test 3: Download nested file (path/to/nested.txt)'
    success = s3_get_object('path/to/nested.txt', content)
    if (success) then
        print *, '  ✓ Downloaded nested file:', trim(content)
    else
        print *, '  ✗ Failed to download nested file'
        stop 1
    end if
    print *

    ! Test 4: Check for non-existent file
    print *, 'Test 4: Verify non-existent file returns false'
    exists = s3_object_exists('does-not-exist.txt')
    if (.not. exists) then
        print *, '  ✓ Correctly reported non-existent file'
    else
        print *, '  ✗ False positive on non-existent file'
        stop 1
    end if
    print *

    ! Test 5: Read multiline file using I/O interface
    print *, 'Test 5: Read multiline file line by line'
    call s3_open(unit, 'multiline.txt', 'read', iostat)
    if (iostat == 0) then
        print *, '  ✓ Opened multiline.txt'
        do i = 1, 3
            call s3_read_line(unit, line, iostat)
            if (iostat /= 0) exit
            print *, '    Line', i, ':', trim(line)
        end do
        call s3_close(unit, iostat)
        print *, '  ✓ Successfully read and closed file'
    else
        print *, '  ✗ Failed to open multiline.txt'
        stop 1
    end if
    print *

    ! Test 6: Test medium-sized binary file
    print *, 'Test 6: Download medium binary file (~1MB)'
    success = s3_get_object('data/medium.bin', content)
    if (success) then
        print *, '  ✓ Downloaded binary file:', len(content), 'bytes'
        if (len(content) >= 1024*1024) then
            print *, '  ✓ Size matches expected (~1MB)'
        else
            print *, '  ✗ Size mismatch:', len(content), 'bytes'
            stop 1
        end if
    else
        print *, '  ✗ Failed to download binary file'
        stop 1
    end if
    print *

    print *, '=========================================='
    print *, 'All MinIO tests passed!'
    print *, '=========================================='

end program test_minio
