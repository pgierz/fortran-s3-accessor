program test_basic
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite
    use test_math, only : collect_math
    use test_s3_http, only : collect_s3_http
    use test_byte_range, only : collect_byte_range_tests
    implicit none

    integer :: stat

    stat = 0

    call run_testsuite(collect_math, error_unit, stat)
    call run_testsuite(collect_s3_http, error_unit, stat)
    call run_testsuite(collect_byte_range_tests, error_unit, stat)

    if (stat > 0) then
        write(error_unit, '(i0, " test(s) failed!")') stat
        error stop 1
    end if

end program test_basic