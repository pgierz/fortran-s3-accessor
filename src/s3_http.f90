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
    ! s3:// URI functions
    public :: s3_get_uri
    public :: s3_put_uri
    public :: s3_exists_uri
    public :: s3_delete_uri

contains

    ! Parse s3:// URI into bucket and key
    ! Format: s3://bucket-name/path/to/object
    subroutine parse_s3_uri(uri, bucket, key, success)
        character(len=*), intent(in) :: uri
        character(len=:), allocatable, intent(out) :: bucket
        character(len=:), allocatable, intent(out) :: key
        logical, intent(out) :: success
        integer :: bucket_start, bucket_end, key_start

        success = .false.

        ! Check for s3:// prefix
        if (len(uri) < 6) return
        if (uri(1:5) /= 's3://') return

        ! Find bucket name (between s3:// and next /)
        bucket_start = 6
        bucket_end = index(uri(bucket_start:), '/') + bucket_start - 2

        if (bucket_end < bucket_start) then
            ! No key, just bucket
            bucket = uri(bucket_start:)
            key = ''
            success = .true.
            return
        end if

        ! Extract bucket and key
        bucket = uri(bucket_start:bucket_end)
        key_start = bucket_end + 2

        if (key_start <= len(uri)) then
            key = uri(key_start:)
        else
            key = ''
        end if

        success = .true.
    end subroutine parse_s3_uri

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

    ! Get an object using s3:// URI
    function s3_get_uri(uri, content) result(success)
        character(len=*), intent(in) :: uri
        character(len=:), allocatable, intent(out) :: content
        logical :: success
        character(len=:), allocatable :: bucket, key
        type(s3_config) :: temp_config
        logical :: uri_parsed

        success = .false.

        ! Try to parse as s3:// URI
        call parse_s3_uri(uri, bucket, key, uri_parsed)
        if (.not. uri_parsed) then
            ! Not a s3:// URI, treat as regular key with current config
            success = s3_get_object(uri, content)
            return
        end if

        ! Use parsed bucket if different from current config
        if (allocated(bucket) .and. len_trim(bucket) > 0) then
            temp_config = current_config
            temp_config%bucket = bucket
            call s3_init(temp_config)
            success = s3_get_object(key, content)
            ! Restore original config
            call s3_init(current_config)
        else
            success = s3_get_object(key, content)
        end if
    end function s3_get_uri

    ! Check if object exists using s3:// URI
    function s3_exists_uri(uri) result(exists)
        character(len=*), intent(in) :: uri
        logical :: exists
        character(len=:), allocatable :: bucket, key
        type(s3_config) :: temp_config
        logical :: uri_parsed

        exists = .false.

        ! Try to parse as s3:// URI
        call parse_s3_uri(uri, bucket, key, uri_parsed)
        if (.not. uri_parsed) then
            ! Not a s3:// URI, treat as regular key
            exists = s3_object_exists(uri)
            return
        end if

        ! Use parsed bucket if different from current config
        if (allocated(bucket) .and. len_trim(bucket) > 0) then
            temp_config = current_config
            temp_config%bucket = bucket
            call s3_init(temp_config)
            exists = s3_object_exists(key)
            ! Restore original config
            call s3_init(current_config)
        else
            exists = s3_object_exists(key)
        end if
    end function s3_exists_uri

    ! Put an object using s3:// URI
    function s3_put_uri(uri, content) result(success)
        character(len=*), intent(in) :: uri
        character(len=*), intent(in) :: content
        logical :: success
        character(len=:), allocatable :: bucket, key
        type(s3_config) :: temp_config
        logical :: uri_parsed

        success = .false.

        ! Try to parse as s3:// URI
        call parse_s3_uri(uri, bucket, key, uri_parsed)
        if (.not. uri_parsed) then
            ! Not a s3:// URI, treat as regular key
            success = s3_put_object(uri, content)
            return
        end if

        ! Use parsed bucket if different from current config
        if (allocated(bucket) .and. len_trim(bucket) > 0) then
            temp_config = current_config
            temp_config%bucket = bucket
            call s3_init(temp_config)
            success = s3_put_object(key, content)
            ! Restore original config
            call s3_init(current_config)
        else
            success = s3_put_object(key, content)
        end if
    end function s3_put_uri

    ! Delete an object using s3:// URI
    function s3_delete_uri(uri) result(success)
        character(len=*), intent(in) :: uri
        logical :: success
        character(len=:), allocatable :: bucket, key
        type(s3_config) :: temp_config
        logical :: uri_parsed

        success = .false.

        ! Try to parse as s3:// URI
        call parse_s3_uri(uri, bucket, key, uri_parsed)
        if (.not. uri_parsed) then
            ! Not a s3:// URI, treat as regular key
            success = s3_delete_object(uri)
            return
        end if

        ! Use parsed bucket if different from current config
        if (allocated(bucket) .and. len_trim(bucket) > 0) then
            temp_config = current_config
            temp_config%bucket = bucket
            call s3_init(temp_config)
            success = s3_delete_object(key)
            ! Restore original config
            call s3_init(current_config)
        else
            success = s3_delete_object(key)
        end if
    end function s3_delete_uri

end module s3_http