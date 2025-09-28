module s3_http
    implicit none
    private

    ! Public configuration type
    type, public :: s3_config
        character(len=256) :: bucket = ''
        character(len=256) :: region = 'us-east-1'
        character(len=256) :: endpoint = 's3.amazonaws.com'
        character(len=256) :: access_key = ''
        character(len=256) :: secret_key = ''
        logical :: use_https = .true.
    end type s3_config

    ! Module variables
    type(s3_config), save :: current_config
    logical, save :: initialized = .false.

    ! Public procedures
    public :: s3_init
    public :: s3_get_object
    public :: s3_put_object
    public :: s3_object_exists
    public :: s3_delete_object

contains

    ! Check if we're in test mode
    function is_test_mode() result(test_mode)
        logical :: test_mode
        character(len=32) :: test_value
        integer :: status

        test_mode = .false.
        call get_environment_variable("F90S3_TEST_MODE", test_value, status=status)
        if (status == 0) then
            test_mode = (trim(test_value) == "true" .or. trim(test_value) == "1")
        end if
    end function is_test_mode

    subroutine s3_init(config)
        type(s3_config), intent(in) :: config
        current_config = config
        initialized = .true.
    end subroutine s3_init

    ! Get an object from S3
    function s3_get_object(key, content) result(success)
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: content
        logical :: success
        character(len=2048) :: url
        character(len=4096) :: cmd
        character(len=256) :: tmpfile
        integer :: unit, ios, filesize
        character(len=1) :: byte
        integer :: i

        success = .false.
        if (.not. initialized) return

        ! Build URL
        if (current_config%use_https) then
            write(url, '(A,A,A,A,A,A)') 'https://', &
                trim(current_config%bucket), '.', &
                trim(current_config%endpoint), '/', &
                trim(key)
        else
            write(url, '(A,A,A,A,A,A)') 'http://', &
                trim(current_config%bucket), '.', &
                trim(current_config%endpoint), '/', &
                trim(key)
        end if

        ! Create temp file name
        write(tmpfile, '(A,I0,A)') '/tmp/s3_get_', getpid(), '.tmp'

        ! Build curl command
        write(cmd, '(A,A,A,A,A)') 'curl -s -o ', trim(tmpfile), ' "', trim(url), '"'

        ! Execute curl
        call execute_command_line(cmd, exitstat=ios)
        if (ios /= 0) return

        ! Read the downloaded file
        inquire(file=tmpfile, size=filesize, iostat=ios)
        if (ios /= 0) return

        allocate(character(len=filesize) :: content)

        open(newunit=unit, file=tmpfile, access='stream', &
             form='unformatted', status='old', iostat=ios)
        if (ios /= 0) then
            deallocate(content)
            return
        end if

        do i = 1, filesize
            read(unit, iostat=ios) byte
            if (ios /= 0) exit
            content(i:i) = byte
        end do

        close(unit)

        ! Clean up temp file
        write(cmd, '(A,A)') 'rm -f ', trim(tmpfile)
        call execute_command_line(cmd)

        success = (ios == 0 .and. index(content, '<Error>') == 0)
    end function s3_get_object

    ! Put an object to S3
    function s3_put_object(key, content) result(success)
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: content
        logical :: success
        character(len=2048) :: url
        character(len=4096) :: cmd
        character(len=256) :: tmpfile
        integer :: unit, ios

        success = .false.
        if (.not. initialized) return

        ! For public buckets without auth, PUT won't work
        ! This is a simplified version - real implementation needs AWS signature
        if (len_trim(current_config%access_key) == 0) then
            print *, 'Warning: PUT requires AWS credentials'
            return
        end if

        ! Build URL
        if (current_config%use_https) then
            write(url, '(A,A,A,A,A,A)') 'https://', &
                trim(current_config%bucket), '.', &
                trim(current_config%endpoint), '/', &
                trim(key)
        else
            write(url, '(A,A,A,A,A,A)') 'http://', &
                trim(current_config%bucket), '.', &
                trim(current_config%endpoint), '/', &
                trim(key)
        end if

        ! Create temp file with content
        write(tmpfile, '(A,I0,A)') '/tmp/s3_put_', getpid(), '.tmp'

        open(newunit=unit, file=tmpfile, status='replace', iostat=ios)
        if (ios /= 0) return

        write(unit, '(A)', iostat=ios) trim(content)
        close(unit)

        ! Build curl command for PUT (simplified - needs AWS v4 signature in reality)
        write(cmd, '(A,A,A,A,A)') 'curl -s -X PUT --data-binary @', &
            trim(tmpfile), ' "', trim(url), '"'

        ! Execute curl
        call execute_command_line(cmd, exitstat=ios)

        ! Clean up temp file
        write(cmd, '(A,A)') 'rm -f ', trim(tmpfile)
        call execute_command_line(cmd)

        success = (ios == 0)
    end function s3_put_object

    ! Check if object exists
    function s3_object_exists(key) result(exists)
        character(len=*), intent(in) :: key
        logical :: exists
        character(len=2048) :: url
        character(len=4096) :: cmd
        integer :: ios

        exists = .false.
        if (.not. initialized) return

        ! Build URL
        if (current_config%use_https) then
            write(url, '(A,A,A,A,A,A)') 'https://', &
                trim(current_config%bucket), '.', &
                trim(current_config%endpoint), '/', &
                trim(key)
        else
            write(url, '(A,A,A,A,A,A)') 'http://', &
                trim(current_config%bucket), '.', &
                trim(current_config%endpoint), '/', &
                trim(key)
        end if

        ! Use curl HEAD request to check existence
        write(cmd, '(A,A,A)') 'curl -s -I "', trim(url), '" | grep "HTTP" | grep -q "200 OK"'

        call execute_command_line(cmd, exitstat=ios)
        exists = (ios == 0)
    end function s3_object_exists

    ! Delete an object
    function s3_delete_object(key) result(success)
        character(len=*), intent(in) :: key
        logical :: success
        character(len=2048) :: url
        character(len=4096) :: cmd
        integer :: ios

        success = .false.
        if (.not. initialized) return

        ! For public buckets without auth, DELETE won't work
        if (len_trim(current_config%access_key) == 0) then
            print *, 'Warning: DELETE requires AWS credentials'
            return
        end if

        ! Build URL
        if (current_config%use_https) then
            write(url, '(A,A,A,A,A,A)') 'https://', &
                trim(current_config%bucket), '.', &
                trim(current_config%endpoint), '/', &
                trim(key)
        else
            write(url, '(A,A,A,A,A,A)') 'http://', &
                trim(current_config%bucket), '.', &
                trim(current_config%endpoint), '/', &
                trim(key)
        end if

        ! Build curl command for DELETE
        write(cmd, '(A,A,A)') 'curl -s -X DELETE "', trim(url), '"'

        call execute_command_line(cmd, exitstat=ios)
        success = (ios == 0)
    end function s3_delete_object

    ! Helper to get process ID
    function getpid() result(pid)
        integer :: pid
        real :: rand_val
        pid = 1  ! Simplified - would use actual getpid() C function
        call random_number(rand_val)
        pid = abs(int(rand_val * 100000))
    end function getpid

end module s3_http