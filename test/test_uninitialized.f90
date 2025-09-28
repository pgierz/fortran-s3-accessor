program test_uninitialized
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite
    use test_uninitialized_cases, only : collect_uninitialized_cases
    implicit none

    integer :: stat

    stat = 0

    call run_testsuite(collect_uninitialized_cases, error_unit, stat)

    if (stat > 0) then
        write(error_unit, '(i0, " test(s) failed!")') stat
        error stop 1
    end if

end program test_uninitialized