!> Direct libcurl bindings for native HTTP operations.
!>
!> This module provides ISO_C_BINDING interfaces to libcurl for high-performance
!> HTTP operations without subprocess overhead. Automatically detects libcurl
!> availability at runtime and falls back gracefully if not present.
!>
!> @note Requires libcurl library (libcurl.so / libcurl.dylib / libcurl.dll)
!> @warning If libcurl not found, calling code must use fallback mechanism
module libcurl_bindings
    use iso_c_binding
    use s3_logger
    implicit none
    private

    ! Public API
    public :: curl_buffer_t
    public :: curl_init
    public :: curl_cleanup
    public :: curl_get_to_buffer
    public :: is_libcurl_available
    public :: get_curl_error_string

    !> Maximum size for error message buffer
    integer, parameter :: CURL_ERROR_SIZE = 256

    !> libcurl option codes (from curl.h)
    integer(c_int), parameter :: CURLOPT_URL = 10002
    integer(c_int), parameter :: CURLOPT_WRITEFUNCTION = 20011
    integer(c_int), parameter :: CURLOPT_WRITEDATA = 10001
    integer(c_int), parameter :: CURLOPT_ERRORBUFFER = 10010
    integer(c_int), parameter :: CURLOPT_FAILONERROR = 45
    integer(c_int), parameter :: CURLOPT_FOLLOWLOCATION = 52

    !> libcurl result codes
    integer(c_int), parameter :: CURLE_OK = 0

    !> Buffer type for receiving curl data
    type :: curl_buffer_t
        character(len=:), allocatable :: data
        integer :: size = 0
    end type curl_buffer_t

    !> Module state
    logical, save :: libcurl_loaded = .false.
    logical, save :: libcurl_checked = .false.

    ! C function interfaces
    interface
        !> Initialize a curl session
        function curl_easy_init() bind(C, name="curl_easy_init")
            import :: c_ptr
            type(c_ptr) :: curl_easy_init
        end function curl_easy_init

        !> Perform the curl operation
        function curl_easy_perform(handle) bind(C, name="curl_easy_perform")
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int) :: curl_easy_perform
        end function curl_easy_perform

        !> Clean up curl session
        subroutine curl_easy_cleanup(handle) bind(C, name="curl_easy_cleanup")
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine curl_easy_cleanup

        !> Get string description of error code
        function curl_easy_strerror(code) bind(C, name="curl_easy_strerror")
            import :: c_ptr, c_int
            integer(c_int), value :: code
            type(c_ptr) :: curl_easy_strerror
        end function curl_easy_strerror
    end interface

    !> Generic interface for curl_easy_setopt with different parameter types
    interface curl_easy_setopt
        module procedure curl_setopt_ptr
        module procedure curl_setopt_funptr
        module procedure curl_setopt_int
        module procedure curl_setopt_string
    end interface curl_easy_setopt

contains

    !> Set curl option with c_ptr parameter.
    function curl_setopt_ptr(handle, option, parameter) result(res)
        type(c_ptr), value :: handle
        integer(c_int), value :: option
        type(c_ptr), value :: parameter
        integer(c_int) :: res
        interface
            function curl_easy_setopt_c(handle, option, parameter) &
                bind(C, name="curl_easy_setopt")
                import :: c_ptr, c_int
                type(c_ptr), value :: handle
                integer(c_int), value :: option
                type(c_ptr), value :: parameter
                integer(c_int) :: curl_easy_setopt_c
            end function curl_easy_setopt_c
        end interface
        res = curl_easy_setopt_c(handle, option, parameter)
    end function curl_setopt_ptr

    !> Set curl option with c_funptr parameter (function pointer).
    function curl_setopt_funptr(handle, option, parameter) result(res)
        type(c_ptr), value :: handle
        integer(c_int), value :: option
        type(c_funptr), value :: parameter
        integer(c_int) :: res
        interface
            function curl_easy_setopt_c(handle, option, parameter) &
                bind(C, name="curl_easy_setopt")
                import :: c_ptr, c_int, c_funptr
                type(c_ptr), value :: handle
                integer(c_int), value :: option
                type(c_funptr), value :: parameter
                integer(c_int) :: curl_easy_setopt_c
            end function curl_easy_setopt_c
        end interface
        res = curl_easy_setopt_c(handle, option, parameter)
    end function curl_setopt_funptr

    !> Set curl option with integer parameter.
    function curl_setopt_int(handle, option, parameter) result(res)
        type(c_ptr), value :: handle
        integer(c_int), value :: option
        integer(c_int), value :: parameter
        integer(c_int) :: res
        interface
            function curl_easy_setopt_c(handle, option, parameter) &
                bind(C, name="curl_easy_setopt")
                import :: c_ptr, c_int
                type(c_ptr), value :: handle
                integer(c_int), value :: option
                integer(c_int), value :: parameter
                integer(c_int) :: curl_easy_setopt_c
            end function curl_easy_setopt_c
        end interface
        res = curl_easy_setopt_c(handle, option, parameter)
    end function curl_setopt_int

    !> Set curl option with string parameter.
    function curl_setopt_string(handle, option, parameter) result(res)
        type(c_ptr), value :: handle
        integer(c_int), value :: option
        character(len=*, kind=c_char), intent(in) :: parameter
        integer(c_int) :: res
        interface
            function curl_easy_setopt_c(handle, option, parameter) &
                bind(C, name="curl_easy_setopt")
                import :: c_ptr, c_int, c_char
                type(c_ptr), value :: handle
                integer(c_int), value :: option
                character(kind=c_char), dimension(*) :: parameter
                integer(c_int) :: curl_easy_setopt_c
            end function curl_easy_setopt_c
        end interface
        res = curl_easy_setopt_c(handle, option, parameter)
    end function curl_setopt_string

    !> Check if libcurl is available on this system.
    !>
    !> Attempts to call curl_easy_init() to verify libcurl can be loaded.
    !> Result is cached for subsequent calls.
    !>
    !> @return .true. if libcurl is available, .false. otherwise
    function is_libcurl_available() result(available)
        logical :: available
        type(c_ptr) :: test_handle

        ! Return cached result if already checked
        if (libcurl_checked) then
            available = libcurl_loaded
            return
        end if

        call s3_log_debug('Checking libcurl availability...')

        ! Try to initialize curl
        test_handle = curl_easy_init()

        if (c_associated(test_handle)) then
            call curl_easy_cleanup(test_handle)
            libcurl_loaded = .true.
            available = .true.
            call s3_log_info('libcurl is available - using native implementation')
        else
            libcurl_loaded = .false.
            available = .false.
            call s3_log_info('libcurl not available - will use fallback method')
        end if

        libcurl_checked = .true.

    end function is_libcurl_available

    !> Initialize libcurl for use.
    !>
    !> Must be called before any other libcurl operations.
    !>
    !> @param[out] handle Curl handle for subsequent operations
    !> @return .true. if initialization successful
    function curl_init(handle) result(success)
        type(c_ptr), intent(out) :: handle
        logical :: success

        success = .false.

        if (.not. is_libcurl_available()) then
            call s3_log_error('Cannot initialize curl - libcurl not available')
            handle = c_null_ptr
            return
        end if

        handle = curl_easy_init()

        if (c_associated(handle)) then
            success = .true.
            call s3_log_trace('Curl handle initialized')
        else
            call s3_log_error('curl_easy_init() failed')
        end if

    end function curl_init

    !> Clean up curl handle.
    !>
    !> @param handle Curl handle to clean up
    subroutine curl_cleanup(handle)
        type(c_ptr), intent(in) :: handle

        if (c_associated(handle)) then
            call curl_easy_cleanup(handle)
            call s3_log_trace('Curl handle cleaned up')
        end if

    end subroutine curl_cleanup

    !> Callback function for libcurl to write received data.
    !>
    !> Called by libcurl with chunks of data as they arrive.
    !> Appends data to the provided buffer.
    !>
    !> @param ptr Pointer to data chunk from libcurl
    !> @param size Size of each data element (always 1)
    !> @param nmemb Number of elements
    !> @param userdata Pointer to our curl_buffer_t
    !> @return Number of bytes handled (must equal size*nmemb for success)
    function write_callback(ptr, size, nmemb, userdata) &
        bind(C) result(bytes_written)
        type(c_ptr), value :: ptr
        integer(c_size_t), value :: size, nmemb
        type(c_ptr), value :: userdata
        integer(c_size_t) :: bytes_written

        type(curl_buffer_t), pointer :: buffer
        character(kind=c_char), pointer :: chunk(:)
        integer :: chunk_size, i
        character(len=:), allocatable :: temp_data
        character(len=256) :: msg

        bytes_written = 0

        ! Get pointer to our buffer
        call c_f_pointer(userdata, buffer)
        if (.not. associated(buffer)) then
            call s3_log_error('Write callback: buffer not associated')
            return
        end if

        ! Calculate chunk size
        chunk_size = int(size * nmemb)
        if (chunk_size <= 0) return

        write(msg, '(A,I0,A)') 'Write callback: receiving ', chunk_size, ' bytes'
        call s3_log_trace(trim(msg))

        ! Get pointer to received data
        call c_f_pointer(ptr, chunk, [chunk_size])

        ! Append to buffer
        if (allocated(buffer%data)) then
            allocate(character(len=len(buffer%data) + chunk_size) :: temp_data)
            temp_data = buffer%data
            do i = 1, chunk_size
                temp_data(len(buffer%data) + i:len(buffer%data) + i) = chunk(i)
            end do
            call move_alloc(temp_data, buffer%data)
        else
            allocate(character(len=chunk_size) :: buffer%data)
            do i = 1, chunk_size
                buffer%data(i:i) = chunk(i)
            end do
        end if

        buffer%size = buffer%size + chunk_size
        bytes_written = int(chunk_size, c_size_t)

        write(msg, '(A,I0,A)') 'Write callback: total buffer size now ', buffer%size, ' bytes'
        call s3_log_trace(trim(msg))

    end function write_callback

    !> Perform HTTP GET and store result in buffer.
    !>
    !> @param url URL to fetch
    !> @param buffer Buffer to store response
    !> @return .true. if successful, .false. on error
    function curl_get_to_buffer(url, buffer) result(success)
        character(len=*), intent(in) :: url
        type(curl_buffer_t), intent(out), target :: buffer
        logical :: success

        type(c_ptr) :: handle
        integer(c_int) :: res
        character(len=256) :: msg
        procedure(write_callback), pointer :: callback_ptr

        success = .false.

        ! Initialize buffer
        if (allocated(buffer%data)) deallocate(buffer%data)
        buffer%size = 0

        ! Initialize curl
        if (.not. curl_init(handle)) return

        ! Set URL
        res = curl_easy_setopt(handle, CURLOPT_URL, trim(url) // C_NULL_CHAR)
        if (res /= CURLE_OK) then
            call s3_log_error('Failed to set URL: ' // trim(url))
            call curl_cleanup(handle)
            return
        end if

        ! Set write callback
        callback_ptr => write_callback
        res = curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, c_funloc(callback_ptr))
        if (res /= CURLE_OK) then
            call s3_log_error('Failed to set write callback')
            call curl_cleanup(handle)
            return
        end if

        ! Set write data (our buffer)
        res = curl_easy_setopt(handle, CURLOPT_WRITEDATA, c_loc(buffer))
        if (res /= CURLE_OK) then
            call s3_log_error('Failed to set write data')
            call curl_cleanup(handle)
            return
        end if

        ! Set other options
        res = curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, 1_c_int)
        res = curl_easy_setopt(handle, CURLOPT_FAILONERROR, 1_c_int)

        ! Perform the request
        write(msg, '(A,A)') 'Performing GET request to: ', trim(url)
        call s3_log_debug(trim(msg))

        res = curl_easy_perform(handle)

        if (res == CURLE_OK) then
            success = .true.
            write(msg, '(A,I0,A)') 'GET request successful, received ', buffer%size, ' bytes'
            call s3_log_debug(trim(msg))
        else
            ! Extract error message
            write(msg, '(A,A)') 'curl error: ', trim(get_curl_error_string(res))
            call s3_log_error(trim(msg))
        end if

        ! Cleanup
        call curl_cleanup(handle)

    end function curl_get_to_buffer

    !> Get human-readable error message for curl error code.
    !>
    !> @param code Curl error code
    !> @return Error message string
    function get_curl_error_string(code) result(error_msg)
        integer(c_int), intent(in) :: code
        character(len=:), allocatable :: error_msg
        type(c_ptr) :: c_str
        character(kind=c_char), dimension(:), pointer :: c_str_array
        integer :: max_len

        c_str = curl_easy_strerror(code)
        if (c_associated(c_str)) then
            ! Convert c_ptr to character array (max 256 chars)
            max_len = 256
            call c_f_pointer(c_str, c_str_array, [max_len])
            error_msg = c_to_f_string(c_str_array)
        else
            error_msg = 'Unknown curl error'
        end if

    end function get_curl_error_string

    !> Convert C string to Fortran string.
    !>
    !> @param c_str C string (null-terminated character array)
    !> @return Fortran allocatable string
    function c_to_f_string(c_str) result(f_str)
        character(kind=c_char), dimension(:), intent(in) :: c_str
        character(len=:), allocatable :: f_str
        integer :: i, length

        ! Find null terminator
        length = 0
        do i = 1, size(c_str)
            if (c_str(i) == C_NULL_CHAR) exit
            length = i
        end do

        ! Copy to Fortran string
        allocate(character(len=length) :: f_str)
        do i = 1, length
            f_str(i:i) = c_str(i)
        end do

    end function c_to_f_string

end module libcurl_bindings
