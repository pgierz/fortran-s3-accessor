!> HTTP Backend Module - S3 access via HTTP REST API using curl
!> @author Paul Gierz
!>
!> This module provides an HTTP backend that uses curl commands
!> to access S3.
module s3_http_backend_mod
    use s3_backend_mod, only: s3_backend_t, s3_object_metadata_t
    use s3_config_mod, only: s3_config_t
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    private

    !> HTTP backend implementation
    type, extends(s3_backend_t), public :: s3_http_backend_t
        character(len=256) :: endpoint = ''
        character(len=256) :: bucket = ''
        character(len=256) :: access_key = ''
        character(len=256) :: secret_key = ''
        character(len=64) :: region = ''
        logical :: use_ssl = .true.
    contains
        procedure, public :: init => http_init
        procedure, public :: cleanup => http_cleanup
        procedure, public :: get_object => http_get_object
        procedure, public :: put_object => http_put_object
        procedure, public :: delete_object => http_delete_object
        procedure, public :: exists => http_exists
        procedure, public :: list_objects => http_list_objects
        procedure, public :: get_metadata => http_get_metadata
    end type s3_http_backend_t

    ! Interface to C getpid function
    interface
        function c_getpid() bind(c, name='getpid')
            import :: c_int
            integer(c_int) :: c_getpid
        end function c_getpid
    end interface

contains

    !> Initialize HTTP backend
    subroutine http_init(this, config, status)
        class(s3_http_backend_t), intent(inout) :: this
        type(s3_config_t), intent(in) :: config
        integer, intent(out) :: status

        ! Store configuration
        this%endpoint = config%endpoint
        this%bucket = config%bucket
        this%access_key = config%access_key
        this%secret_key = config%secret_key
        this%region = config%region
        this%use_ssl = config%use_ssl

        print *, 'HTTP Backend initialized'
        print *, '  Endpoint: ', trim(this%endpoint)
        print *, '  Bucket: ', trim(this%bucket)
        print *, '  Region: ', trim(this%region)
        print *, '  Using SSL: ', this%use_ssl

        ! Test if curl is available
        call execute_command_line('which curl > /dev/null 2>&1', exitstat=status)
        if (status /= 0) then
            print *, 'ERROR: curl command not found!'
            status = -1
            return
        end if

        status = 0
    end subroutine http_init

    !> Cleanup HTTP backend
    subroutine http_cleanup(this)
        class(s3_http_backend_t), intent(inout) :: this
        print *, 'HTTP Backend cleaned up'
    end subroutine http_cleanup

    !> Get object via HTTP using curl
    subroutine http_get_object(this, key, data, status)
        class(s3_http_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: data
        integer, intent(out) :: status

        character(len=1024) :: url
        character(len=2048) :: command
        character(len=256) :: tempfile
        integer :: unit, filesize, ios
        logical :: file_exists

        ! Build URL
        if (this%use_ssl) then
            url = 'https://'
        else
            url = 'http://'
        end if

        if (len_trim(this%endpoint) > 0) then
            url = trim(url) // trim(this%endpoint) // '/'
            url = trim(url) // trim(this%bucket) // '/'
        else
            url = trim(url) // trim(this%bucket) // '.s3.' // &
                  trim(this%region) // '.amazonaws.com/'
        end if
        url = trim(url) // trim(key)

        print *, 'HTTP GET: ', trim(url)

        ! Create temporary file for download
        write(tempfile, '(A,I0,A)') '/tmp/s3_http_', get_pid(), '.tmp'

        ! Build curl command
        if (len_trim(this%access_key) == 0) then
            ! Public bucket access - no authentication
            write(command, '(A,A,A,A,A)') &
                'curl -s -f -o ', trim(tempfile), ' "', trim(url), '"'
        else
            ! TODO: Implement AWS signature v4 for authenticated access
            print *, 'WARNING: Authenticated access not yet implemented'
            print *, 'For now, only public buckets are supported'
            write(command, '(A,A,A,A,A)') &
                'curl -s -f -o ', trim(tempfile), ' "', trim(url), '"'
        end if

        ! Execute curl command
        print *, 'Executing: ', trim(command)
        call execute_command_line(trim(command), exitstat=ios)
        if (ios /= 0) then
            print *, 'ERROR: curl failed with exit code ', ios
            status = -1
            return
        end if

        ! Read downloaded file
        inquire(file=tempfile, exist=file_exists, size=filesize)
        if (.not. file_exists) then
            print *, 'ERROR: Downloaded file not found'
            status = -1
            return
        end if

        allocate(character(len=filesize) :: data)
        open(newunit=unit, file=tempfile, form='unformatted', &
             access='stream', status='old', iostat=ios)
        if (ios /= 0) then
            print *, 'ERROR: Cannot open downloaded file'
            status = ios
            return
        end if
        read(unit) data
        close(unit)

        ! Clean up temp file
        call execute_command_line('rm -f ' // trim(tempfile))

        print *, 'Successfully downloaded ', filesize, ' bytes'
        status = 0
    end subroutine http_get_object

    !> Put object via HTTP using curl
    subroutine http_put_object(this, key, data, status)
        class(s3_http_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: data
        integer, intent(out) :: status

        character(len=1024) :: url
        character(len=2048) :: command
        character(len=256) :: tempfile
        integer :: unit, ios

        ! Build URL
        if (this%use_ssl) then
            url = 'https://'
        else
            url = 'http://'
        end if

        if (len_trim(this%endpoint) > 0) then
            url = trim(url) // trim(this%endpoint) // '/'
            url = trim(url) // trim(this%bucket) // '/'
        else
            url = trim(url) // trim(this%bucket) // '.s3.' // &
                  trim(this%region) // '.amazonaws.com/'
        end if
        url = trim(url) // trim(key)

        print *, 'HTTP PUT: ', trim(url)

        ! Write data to temporary file
        write(tempfile, '(A,I0,A)') '/tmp/s3_http_put_', get_pid(), '.tmp'
        open(newunit=unit, file=tempfile, form='unformatted', &
             access='stream', status='replace', iostat=ios)
        if (ios /= 0) then
            print *, 'ERROR: Cannot create temp file'
            status = ios
            return
        end if
        write(unit) data
        close(unit)

        ! Build curl command for PUT
        if (len_trim(this%access_key) == 0) then
            ! Note: Most S3 buckets don't allow anonymous PUT
            write(command, '(A,A,A,A,A)') &
                'curl -s -X PUT --data-binary @', trim(tempfile), &
                ' "', trim(url), '"'
        else
            print *, 'ERROR: Authenticated PUT not yet implemented'
            status = -1
            call execute_command_line('rm -f ' // trim(tempfile))
            return
        end if

        ! Execute curl command
        print *, 'Executing: ', trim(command)
        call execute_command_line(trim(command), exitstat=ios)

        ! Clean up temp file
        call execute_command_line('rm -f ' // trim(tempfile))

        if (ios /= 0) then
            print *, 'ERROR: curl PUT failed with exit code ', ios
        else
            print *, 'Successfully uploaded data'
        end if
        status = ios
    end subroutine http_put_object

    !> Delete object via HTTP using curl
    subroutine http_delete_object(this, key, status)
        class(s3_http_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer, intent(out) :: status

        character(len=1024) :: url
        character(len=2048) :: command
        integer :: ios

        ! Build URL
        if (this%use_ssl) then
            url = 'https://'
        else
            url = 'http://'
        end if

        if (len_trim(this%endpoint) > 0) then
            url = trim(url) // trim(this%endpoint) // '/'
            url = trim(url) // trim(this%bucket) // '/'
        else
            url = trim(url) // trim(this%bucket) // '.s3.' // &
                  trim(this%region) // '.amazonaws.com/'
        end if
        url = trim(url) // trim(key)

        print *, 'HTTP DELETE: ', trim(url)

        ! Build curl command for DELETE
        write(command, '(A,A,A)') 'curl -s -X DELETE "', trim(url), '"'

        ! Execute curl command
        print *, 'Executing: ', trim(command)
        call execute_command_line(trim(command), exitstat=ios)

        if (ios /= 0) then
            print *, 'ERROR: curl DELETE failed with exit code ', ios
        else
            print *, 'Delete request sent'
        end if
        status = ios
    end subroutine http_delete_object

    !> Check if object exists via HTTP HEAD request
    function http_exists(this, key) result(exists)
        class(s3_http_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        logical :: exists

        character(len=1024) :: url
        character(len=2048) :: command
        integer :: ios

        ! Build URL
        if (this%use_ssl) then
            url = 'https://'
        else
            url = 'http://'
        end if

        if (len_trim(this%endpoint) > 0) then
            url = trim(url) // trim(this%endpoint) // '/'
            url = trim(url) // trim(this%bucket) // '/'
        else
            url = trim(url) // trim(this%bucket) // '.s3.' // &
                  trim(this%region) // '.amazonaws.com/'
        end if
        url = trim(url) // trim(key)

        ! Use HEAD request to check existence
        write(command, '(A,A,A)') &
            'curl -s -I -f "', trim(url), '" > /dev/null 2>&1'

        ! Execute and check exit code
        call execute_command_line(trim(command), exitstat=ios)
        exists = (ios == 0)

        if (exists) then
            print *, 'Object exists: ', trim(key)
        else
            print *, 'Object not found: ', trim(key)
        end if
    end function http_exists

    !> List objects with prefix (not fully implemented)
    subroutine http_list_objects(this, prefix, keys, count, status)
        class(s3_http_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: prefix
        character(len=256), allocatable, intent(out) :: keys(:)
        integer, intent(out) :: count
        integer, intent(out) :: status

        ! This would require XML parsing of ListBucket response
        allocate(keys(100))
        count = 0
        status = 0

        print *, 'WARNING: http_list_objects not fully implemented'
        print *, 'Would need to parse XML response from S3 ListBucket API'
    end subroutine http_list_objects

    !> Get object metadata via HTTP HEAD
    subroutine http_get_metadata(this, key, metadata, status)
        class(s3_http_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(s3_object_metadata_t), intent(out) :: metadata
        integer, intent(out) :: status

        character(len=1024) :: url
        character(len=2048) :: command
        character(len=256) :: tempfile
        integer :: ios

        ! Build URL
        if (this%use_ssl) then
            url = 'https://'
        else
            url = 'http://'
        end if

        if (len_trim(this%endpoint) > 0) then
            url = trim(url) // trim(this%endpoint) // '/'
            url = trim(url) // trim(this%bucket) // '/'
        else
            url = trim(url) // trim(this%bucket) // '.s3.' // &
                  trim(this%region) // '.amazonaws.com/'
        end if
        url = trim(url) // trim(key)

        ! Get headers with curl
        write(tempfile, '(A,I0,A)') '/tmp/s3_headers_', get_pid(), '.tmp'
        write(command, '(A,A,A,A,A)') &
            'curl -s -I "', trim(url), '" > ', trim(tempfile), ' 2>&1'

        call execute_command_line(trim(command), exitstat=ios)
        if (ios == 0) then
            ! TODO: Parse headers to extract content-length, etag, etc.
            metadata%size = 0
            metadata%content_type = 'application/octet-stream'
            print *, 'Got headers for: ', trim(key)
        else
            print *, 'Failed to get metadata for: ', trim(key)
        end if

        ! Clean up
        call execute_command_line('rm -f ' // trim(tempfile))

        status = ios
    end subroutine http_get_metadata

    !> Get process ID using C library function
    function get_pid() result(pid)
        integer :: pid
        pid = int(c_getpid())
    end function get_pid

end module s3_http_backend_mod