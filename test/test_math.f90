module test_math
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_math

contains

    !> Collect all exported unit tests
    subroutine collect_math(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("basic_addition", test_basic_addition) &
        ]
    end subroutine collect_math

    !> Test basic addition
    subroutine test_basic_addition(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer :: result

        result = 1 + 1
        call check(error, result == 2, "1 + 1 should equal 2")
    end subroutine test_basic_addition

end module test_math