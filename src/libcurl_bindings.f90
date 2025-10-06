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
    public :: curl_progress_callback
    public :: curl_init
    public :: curl_cleanup
    public :: curl_get_to_buffer
    public :: curl_get_to_buffer_with_progress
    public :: curl_get_to_buffer_with_headers
    public :: curl_get_to_file
    public :: curl_get_http_status
    public :: is_libcurl_available
    public :: get_curl_error_string
    ! Generic interface and specific setopt functions
    public :: curl_easy_setopt
    public :: curl_setopt_string
    public :: curl_setopt_ptr
    public :: curl_setopt_funptr
    public :: curl_setopt_int
    ! Constants
    public :: CURLOPT_URL
    public :: CURLINFO_RESPONSE_CODE
    public :: CURLE_OK

    !> Maximum size for error message buffer
    integer, parameter :: CURL_ERROR_SIZE = 256

    !> libcurl option codes (from curl.h)
    integer(c_int), parameter :: CURLOPT_URL = 10002
    integer(c_int), parameter :: CURLOPT_WRITEFUNCTION = 20011
    integer(c_int), parameter :: CURLOPT_WRITEDATA = 10001
    integer(c_int), parameter :: CURLOPT_ERRORBUFFER = 10010
    integer(c_int), parameter :: CURLOPT_FAILONERROR = 45
    integer(c_int), parameter :: CURLOPT_FOLLOWLOCATION = 52
    integer(c_int), parameter :: CURLOPT_NOPROGRESS = 43
    integer(c_int), parameter :: CURLOPT_XFERINFOFUNCTION = 20219
    integer(c_int), parameter :: CURLOPT_XFERINFODATA = 10057
    integer(c_int), parameter :: CURLOPT_HTTPHEADER = 10023

    !> libcurl info codes (for curl_easy_getinfo)
    integer(c_int), parameter :: CURLINFO_RESPONSE_CODE = 2097154

    !> libcurl result codes
    integer(c_int), parameter :: CURLE_OK = 0

    !> Buffer type for receiving curl data
    type :: curl_buffer_t
        character(len=:), allocatable :: data
        integer :: size = 0
    end type curl_buffer_t

    !> Progress callback interface
    !> @param dltotal Total bytes to download (0 if unknown)
    !> @param dlnow Bytes downloaded so far
    !> @param ultotal Total bytes to upload (0 if unknown)
    !> @param ulnow Bytes uploaded so far
    !> @return 0 to continue, non-zero to abort transfer
    abstract interface
        function curl_progress_callback(dltotal, dlnow, ultotal, ulnow) &
            result(abort) bind(C)
            import :: c_int64_t
            integer(c_int64_t), value :: dltotal, dlnow, ultotal, ulnow
            integer(c_int64_t) :: abort
        end function curl_progress_callback
    end interface

    !> Module state
    logical, save :: libcurl_loaded = .false.
    logical, save :: libcurl_checked = .false.

    !> Module-level URL buffer - CRITICAL for libcurl pointer lifetime
    !> libcurl stores pointer, not copy, so memory must persist until curl_easy_perform
    integer, parameter :: S3_URL_MAXLEN = 8192
    character(kind=c_char), target, save :: module_url_buffer(S3_URL_MAXLEN)

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

        !> Get information from curl handle (for HTTP status, etc.)
        function curl_easy_getinfo_long(handle, info, data) bind(C, name="curl_easy_getinfo")
            import :: c_ptr, c_int, c_long
            type(c_ptr), value :: handle
            integer(c_int), value :: info
            type(c_ptr), value :: data
            integer(c_int) :: curl_easy_getinfo_long
        end function curl_easy_getinfo_long

        !> Append a string to a curl slist (linked list of strings for headers)
        function curl_slist_append(list, string) bind(C, name="curl_slist_append")
            import :: c_ptr, c_char
            type(c_ptr), value :: list
            character(kind=c_char), dimension(*) :: string
            type(c_ptr) :: curl_slist_append
        end function curl_slist_append

        !> Free an entire curl slist
        subroutine curl_slist_free_all(list) bind(C, name="curl_slist_free_all")
            import :: c_ptr
            type(c_ptr), value :: list
        end subroutine curl_slist_free_all
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
    !> Converts Fortran string to C-compatible null-terminated string using transfer().
    function curl_setopt_string(handle, option, parameter) result(res)
        type(c_ptr), value :: handle
        integer(c_int), value :: option
        character(len=*), intent(in) :: parameter
        integer(c_int) :: res
        ! Fixed-size array (max URL length 4096) - SAVE to persist across calls
        character(kind=c_char), target, save :: c_str(4097)

        interface
            function curl_easy_setopt_c(handle, option, str_ptr) &
                bind(C, name="curl_easy_setopt")
                import :: c_ptr, c_int
                type(c_ptr), value :: handle
                integer(c_int), value :: option
                type(c_ptr), value :: str_ptr  ! C pointer to string
                integer(c_int) :: curl_easy_setopt_c
            end function curl_easy_setopt_c
        end interface

        ! Check length
        if (len_trim(parameter) > 4096) then
            call s3_log_error('String too long for curl_setopt_string (max 4096 chars)')
            res = -1
            return
        end if

        ! Convert using transfer intrinsic (cleaner than manual loop)
        c_str = transfer(trim(parameter) // C_NULL_CHAR, c_str)

        ! Pass pointer to C string
        res = curl_easy_setopt_c(handle, option, c_loc(c_str))

    end function curl_setopt_string

    !> Check if libcurl is available on this system.
    !>
    !> Attempts to call curl_easy_init() to verify libcurl can be loaded.
    !> Result is cached for subsequent calls.
    !>
    !> Can be disabled for testing by setting S3_DISABLE_LIBCURL=1 environment variable.
    !>
    !> @return .true. if libcurl is available, .false. otherwise
    function is_libcurl_available() result(available)
        logical :: available
        type(c_ptr) :: test_handle
        character(len=256) :: env_value
        integer :: status

        ! Return cached result if already checked
        if (libcurl_checked) then
            available = libcurl_loaded
            return
        end if

        ! Check if libcurl is disabled via environment variable (for testing)
        call get_environment_variable('S3_DISABLE_LIBCURL', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            if (env_value(1:1) == '1' .or. env_value(1:1) == 'Y' .or. env_value(1:1) == 'y') then
                libcurl_checked = .true.
                libcurl_loaded = .false.
                available = .false.
                call s3_log_info('libcurl disabled via S3_DISABLE_LIBCURL - using fallback')
                return
            end if
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
        integer(c_int8_t), pointer :: chunk_bytes(:)
        integer :: chunk_size
        character(len=:), allocatable :: temp_data, chunk_str
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

        ! Get pointer to received data as byte array
        call c_f_pointer(ptr, chunk_bytes, [chunk_size])

        ! Convert bytes to Fortran string using transfer (avoids pointer aliasing issues)
        allocate(character(len=chunk_size) :: chunk_str)
        chunk_str = transfer(chunk_bytes(1:chunk_size), chunk_str)

        ! Append to buffer
        if (allocated(buffer%data)) then
            allocate(character(len=len(buffer%data) + chunk_size) :: temp_data)
            temp_data(1:len(buffer%data)) = buffer%data
            temp_data(len(buffer%data)+1:) = chunk_str
            call move_alloc(temp_data, buffer%data)
        else
            call move_alloc(chunk_str, buffer%data)
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
        character(len=1024) :: msg
        character(len=512) :: url_truncated
        procedure(write_callback), pointer :: callback_ptr
        integer :: n

        success = .false.

        ! Initialize buffer
        if (allocated(buffer%data)) deallocate(buffer%data)
        buffer%size = 0

        ! Initialize curl
        if (.not. curl_init(handle)) return

        ! Convert URL to C string with null terminator using module-level buffer
        ! CRITICAL: Using module_url_buffer ensures memory persists for libcurl
        n = len_trim(url)
        if (n >= S3_URL_MAXLEN) then
            write(msg, '(A,I0,A)') 'URL too long: ', n, ' chars (max 8192)'
            call s3_log_error(trim(msg))
            call curl_cleanup(handle)
            return
        end if
        ! Use transfer() to convert to C string
        module_url_buffer = transfer(trim(url) // C_NULL_CHAR, module_url_buffer)

        ! Set URL using the ptr variant with c_loc() of module buffer
        res = curl_setopt_ptr(handle, CURLOPT_URL, c_loc(module_url_buffer))

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

        ! Perform the request (truncate URL for logging if too long)
        if (len_trim(url) > 500) then
            url_truncated = url(1:497) // '...'
        else
            url_truncated = trim(url)
        end if
        write(msg, '(A,A)') 'Performing GET request to: ', trim(url_truncated)
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

    !> Perform HTTP GET with progress reporting.
    !>
    !> @param url URL to fetch
    !> @param buffer Buffer to store response
    !> @param progress_cb Progress callback function
    !> @return .true. if successful, .false. on error
    function curl_get_to_buffer_with_progress(url, buffer, progress_cb) result(success)
        character(len=*), intent(in) :: url
        type(curl_buffer_t), intent(out), target :: buffer
        procedure(curl_progress_callback) :: progress_cb
        logical :: success

        type(c_ptr) :: handle
        integer(c_int) :: res
        character(len=1024) :: msg
        character(len=512) :: url_truncated
        procedure(write_callback), pointer :: write_cb_ptr
        procedure(curl_progress_callback), pointer :: progress_cb_ptr
        integer :: n

        success = .false.

        ! Initialize buffer
        if (allocated(buffer%data)) deallocate(buffer%data)
        buffer%size = 0

        ! Initialize curl
        if (.not. curl_init(handle)) return

        ! Convert URL to C string with null terminator using module-level buffer
        ! CRITICAL: Using module_url_buffer ensures memory persists for libcurl
        n = len_trim(url)
        if (n >= S3_URL_MAXLEN) then
            write(msg, '(A,I0,A)') 'URL too long: ', n, ' chars (max 8192)'
            call s3_log_error(trim(msg))
            call curl_cleanup(handle)
            return
        end if
        ! Use transfer() to convert to C string
        module_url_buffer = transfer(trim(url) // C_NULL_CHAR, module_url_buffer)

        ! Set URL using the ptr variant with c_loc() of module buffer
        res = curl_setopt_ptr(handle, CURLOPT_URL, c_loc(module_url_buffer))
        if (res /= CURLE_OK) then
            call s3_log_error('Failed to set URL: ' // trim(url))
            call curl_cleanup(handle)
            return
        end if

        ! Set write callback
        write_cb_ptr => write_callback
        res = curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, c_funloc(write_cb_ptr))
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

        ! Set progress callback
        progress_cb_ptr => progress_cb
        res = curl_easy_setopt(handle, CURLOPT_XFERINFOFUNCTION, c_funloc(progress_cb_ptr))
        if (res /= CURLE_OK) then
            call s3_log_error('Failed to set progress callback')
            call curl_cleanup(handle)
            return
        end if

        ! Enable progress meter
        res = curl_easy_setopt(handle, CURLOPT_NOPROGRESS, 0_c_int)
        if (res /= CURLE_OK) then
            call s3_log_error('Failed to enable progress')
            call curl_cleanup(handle)
            return
        end if

        ! Set other options
        res = curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, 1_c_int)
        res = curl_easy_setopt(handle, CURLOPT_FAILONERROR, 1_c_int)

        ! Perform the request (truncate URL for logging if too long)
        if (len_trim(url) > 500) then
            url_truncated = url(1:497) // '...'
        else
            url_truncated = trim(url)
        end if
        write(msg, '(A,A)') 'Performing GET request with progress to: ', trim(url_truncated)
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

    end function curl_get_to_buffer_with_progress

    !> Perform HTTP GET and stream directly to file.
    !>
    !> This function downloads data using libcurl and writes it directly to a file
    !> without buffering in memory. Ideal for large files that need to be on disk
    !> (e.g., NetCDF files). Avoids subprocess overhead while minimizing memory usage.
    !>
    !> @param url URL to fetch
    !> @param filepath Path to output file (will be created/overwritten)
    !> @return .true. if successful, .false. on error
    !>
    !> @note Linux only - requires libcurl direct binding
    !> @note File is created fresh (existing file will be overwritten)
    function curl_get_to_file(url, filepath) result(success)
        character(len=*), intent(in) :: url
        character(len=*), intent(in) :: filepath
        logical :: success

        type(c_ptr) :: handle
        integer(c_int) :: res
        character(len=1024) :: msg
        character(len=512) :: url_truncated
        integer :: file_unit, ios
        integer :: n

        success = .false.

        ! Check libcurl availability
        if (.not. is_libcurl_available()) then
            call s3_log_error('libcurl not available for direct file streaming')
            return
        end if

        ! Initialize curl
        if (.not. curl_init(handle)) return

        ! Convert URL to C string using module-level buffer
        n = len_trim(url)
        if (n >= S3_URL_MAXLEN) then
            write(msg, '(A,I0,A)') 'URL too long: ', n, ' chars (max 8192)'
            call s3_log_error(trim(msg))
            call curl_cleanup(handle)
            return
        end if
        module_url_buffer = transfer(trim(url) // C_NULL_CHAR, module_url_buffer)

        ! Set URL
        res = curl_setopt_ptr(handle, CURLOPT_URL, c_loc(module_url_buffer))
        if (res /= CURLE_OK) then
            call s3_log_error('Failed to set URL: ' // trim(url))
            call curl_cleanup(handle)
            return
        end if

        ! Open output file (stream mode for direct byte writing)
        open(newunit=file_unit, file=trim(filepath), access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(msg, '(A,A,A,I0)') 'Failed to open file: ', trim(filepath), ', iostat=', ios
            call s3_log_error(trim(msg))
            call curl_cleanup(handle)
            return
        end if

        ! Set write callback to write to file
        res = curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, c_funloc(write_to_file_callback))
        if (res /= CURLE_OK) then
            call s3_log_error('Failed to set write callback')
            close(file_unit)
            call curl_cleanup(handle)
            return
        end if

        ! Pass file unit as user data
        ! Convert file_unit (integer) to c_ptr using c_intptr_t size
        block
            integer(c_intptr_t) :: file_unit_ptr
            file_unit_ptr = int(file_unit, c_intptr_t)
            res = curl_easy_setopt(handle, CURLOPT_WRITEDATA, &
                transfer(file_unit_ptr, c_null_ptr))
        end block
        if (res /= CURLE_OK) then
            call s3_log_error('Failed to set write data')
            close(file_unit)
            call curl_cleanup(handle)
            return
        end if

        ! Set other options
        res = curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, 1_c_int)
        res = curl_easy_setopt(handle, CURLOPT_FAILONERROR, 1_c_int)

        ! Perform the request
        if (len_trim(url) > 500) then
            url_truncated = url(1:497) // '...'
        else
            url_truncated = trim(url)
        end if
        write(msg, '(A,A,A,A)') 'Streaming to file: ', trim(filepath), ' from: ', trim(url_truncated)
        call s3_log_debug(trim(msg))

        res = curl_easy_perform(handle)

        ! Close file
        close(file_unit)

        if (res == CURLE_OK) then
            success = .true.
            call s3_log_debug('File download successful')
        else
            write(msg, '(A,A)') 'curl error: ', trim(get_curl_error_string(res))
            call s3_log_error(trim(msg))
        end if

        ! Cleanup
        call curl_cleanup(handle)

    end function curl_get_to_file

    !> Perform HTTP GET request with custom headers and return response in buffer.
    !>
    !> This function allows adding custom HTTP headers (e.g., Authorization)
    !> to the request. Useful for authenticated S3 requests.
    !>
    !> @param url [in] URL to fetch
    !> @param buffer [out] Buffer to store response
    !> @param headers [in] Array of header strings (e.g., "Authorization: AWS4...")
    !> @return success [logical] True if request succeeded, false otherwise
    function curl_get_to_buffer_with_headers(url, buffer, headers) result(success)
        character(len=*), intent(in) :: url
        type(curl_buffer_t), intent(out), target :: buffer
        character(len=*), intent(in) :: headers(:)
        logical :: success

        type(c_ptr) :: handle, header_list
        integer(c_int) :: res
        procedure(write_callback), pointer :: callback_ptr
        character(len=1024) :: msg
        character(len=512) :: url_truncated
        integer :: i
        character(kind=c_char), allocatable, target :: c_header(:)

        success = .false.

        ! Initialize buffer
        buffer%size = 0
        if (allocated(buffer%data)) deallocate(buffer%data)
        allocate(character(len=0) :: buffer%data)

        ! Initialize curl
        handle = curl_easy_init()
        if (.not. c_associated(handle)) then
            call s3_log_error('curl_easy_init failed')
            return
        end if

        ! Set URL (using module buffer for lifetime)
        ! CRITICAL: Using module_url_buffer ensures memory persists for libcurl
        module_url_buffer = transfer(trim(url) // C_NULL_CHAR, module_url_buffer)
        res = curl_setopt_ptr(handle, CURLOPT_URL, c_loc(module_url_buffer))
        if (res /= CURLE_OK) then
            write(msg, '(A,A)') 'Failed to set URL: ', trim(get_curl_error_string(res))
            call s3_log_error(trim(msg))
            call curl_cleanup(handle)
            return
        end if

        ! Set write callback
        callback_ptr => write_callback
        res = curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, c_funloc(callback_ptr))
        if (res /= CURLE_OK) then
            write(msg, '(A,A)') 'Failed to set write callback: ', trim(get_curl_error_string(res))
            call s3_log_error(trim(msg))
            call curl_cleanup(handle)
            return
        end if

        ! Set buffer as userdata
        res = curl_easy_setopt(handle, CURLOPT_WRITEDATA, c_loc(buffer))
        if (res /= CURLE_OK) then
            write(msg, '(A,A)') 'Failed to set write data: ', trim(get_curl_error_string(res))
            call s3_log_error(trim(msg))
            call curl_cleanup(handle)
            return
        end if

        ! Build header list
        header_list = c_null_ptr
        do i = 1, size(headers)
            ! Convert Fortran string to C null-terminated string
            allocate(c_header(len_trim(headers(i)) + 1))
            c_header(1:len_trim(headers(i))) = transfer(trim(headers(i)), c_header(1:len_trim(headers(i))))
            c_header(len_trim(headers(i)) + 1) = c_null_char
            header_list = curl_slist_append(header_list, c_header)
            deallocate(c_header)
        end do

        ! Set HTTP headers if any were added
        if (c_associated(header_list)) then
            res = curl_easy_setopt(handle, CURLOPT_HTTPHEADER, header_list)
            if (res /= CURLE_OK) then
                write(msg, '(A,A)') 'Failed to set HTTP headers: ', trim(get_curl_error_string(res))
                call s3_log_error(trim(msg))
                call curl_slist_free_all(header_list)
                call curl_cleanup(handle)
                return
            end if
        end if

        ! Set standard curl options
        res = curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, 1_c_int)
        res = curl_easy_setopt(handle, CURLOPT_FAILONERROR, 1_c_int)

        ! Log the request
        if (len(url) > 100) then
            url_truncated = url(1:97) // '...'
        else
            url_truncated = url
        end if
        write(msg, '(A,I0,A,A)') 'GET request with ', size(headers), ' headers to: ', trim(url_truncated)
        call s3_log_debug(trim(msg))

        ! Perform request
        res = curl_easy_perform(handle)

        ! Cleanup header list
        if (c_associated(header_list)) then
            call curl_slist_free_all(header_list)
        end if

        if (res == CURLE_OK) then
            success = .true.
            write(msg, '(A,I0,A)') 'Received ', buffer%size, ' bytes'
            call s3_log_debug(trim(msg))
        else
            write(msg, '(A,A)') 'curl error: ', trim(get_curl_error_string(res))
            call s3_log_error(trim(msg))
        end if

        ! Cleanup
        call curl_cleanup(handle)

    end function curl_get_to_buffer_with_headers

    !> Callback function for writing to file.
    !>
    !> Called by libcurl to write received data chunks directly to file.
    function write_to_file_callback(ptr, size, nmemb, userdata) result(written) bind(C)
        type(c_ptr), value :: ptr
        integer(c_size_t), value :: size, nmemb
        type(c_ptr), value :: userdata
        integer(c_size_t) :: written

        integer(c_size_t) :: total_size
        character(kind=c_char), dimension(:), pointer :: data
        integer :: file_unit, ios, i

        total_size = size * nmemb
        written = 0

        if (total_size == 0) return

        ! Convert userdata back to file unit
        block
            integer(c_intptr_t) :: file_unit_ptr
            file_unit_ptr = transfer(userdata, file_unit_ptr)
            file_unit = int(file_unit_ptr, kind(file_unit))
        end block

        ! Convert C pointer to Fortran array
        call c_f_pointer(ptr, data, [total_size])

        ! Write data to file (stream mode writes byte by byte)
        do i = 1, int(total_size)
            write(file_unit, iostat=ios) data(i)
            if (ios /= 0) then
                ! Write failed
                return
            end if
        end do

        written = total_size

    end function write_to_file_callback

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

    !> Get HTTP status code from curl handle.
    !>
    !> Extracts the HTTP response code after curl_easy_perform.
    !>
    !> @param handle Curl handle (must have performed a request)
    !> @param http_status Output HTTP status code (0 if error)
    !> @return .true. if successful, .false. on error
    function curl_get_http_status(handle, http_status) result(success)
        type(c_ptr), intent(in) :: handle
        integer, intent(out) :: http_status
        logical :: success

        integer(c_long), target :: response_code
        integer(c_int) :: res

        success = .false.
        http_status = 0

        if (.not. c_associated(handle)) then
            call s3_log_error('curl_get_http_status: invalid handle')
            return
        end if

        ! Get response code
        res = curl_easy_getinfo_long(handle, CURLINFO_RESPONSE_CODE, c_loc(response_code))

        if (res == CURLE_OK) then
            http_status = int(response_code)
            success = .true.
        else
            call s3_log_error('curl_get_http_status: curl_easy_getinfo failed')
        end if

    end function curl_get_http_status

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
