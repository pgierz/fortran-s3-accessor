! Standalone minimal libcurl test - no dependencies on our bindings
! This can be compiled directly: gfortran test_curl_minimal_standalone.f90 -lcurl
! Tests if basic curl_easy_setopt works from Fortran
program test_curl_minimal_standalone
    use iso_c_binding
    implicit none

    ! Curl constants
    integer(c_int), parameter :: CURLOPT_URL = 10002

    ! Curl functions
    interface
        function curl_easy_init() bind(c, name='curl_easy_init')
            import :: c_ptr
            type(c_ptr) :: curl_easy_init
        end function curl_easy_init

        function curl_easy_setopt_ptr(handle, option, value) bind(c, name='curl_easy_setopt')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int), value :: option
            type(c_ptr), value :: value
            integer(c_int) :: curl_easy_setopt_ptr
        end function curl_easy_setopt_ptr

        function curl_easy_perform(handle) bind(c, name='curl_easy_perform')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int) :: curl_easy_perform
        end function curl_easy_perform

        subroutine curl_easy_cleanup(handle) bind(c, name='curl_easy_cleanup')
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine curl_easy_cleanup
    end interface

    ! Variables
    type(c_ptr) :: curl
    integer(c_int) :: res
    character(len=50, kind=c_char), target :: url_buffer
    type(c_ptr) :: url_ptr

    print '(A)', "=== Minimal Fortran/libcurl Test ==="
    print '(A)', "Platform: ", compiler_version()
    print '(A)', ""

    ! Initialize curl
    curl = curl_easy_init()
    if (.not. c_associated(curl)) then
        print '(A)', "FAIL: curl_easy_init() returned NULL"
        stop 1
    end if
    print '(A)', "PASS: curl_easy_init()"

    ! Prepare URL with null terminator
    url_buffer = "http://httpbin.org/get" // c_null_char
    url_ptr = c_loc(url_buffer)

    ! Set URL
    res = curl_easy_setopt_ptr(curl, CURLOPT_URL, url_ptr)
    if (res /= 0) then
        print '(A,I0)', "FAIL: curl_easy_setopt returned ", res
        call curl_easy_cleanup(curl)
        stop 1
    end if
    print '(A)', "PASS: curl_easy_setopt(CURLOPT_URL)"

    ! Perform the request (this will output HTTP response)
    print '(A)', ""
    print '(A)', "Attempting HTTP request..."
    res = curl_easy_perform(curl)
    print '(A)', ""

    if (res /= 0) then
        print '(A,I0)', "FAIL: curl_easy_perform returned ", res
        call curl_easy_cleanup(curl)
        stop 1
    end if

    print '(A)', "PASS: curl_easy_perform()"
    call curl_easy_cleanup(curl)

    print '(A)', ""
    print '(A)', "=== ALL TESTS PASSED ==="

end program test_curl_minimal_standalone
