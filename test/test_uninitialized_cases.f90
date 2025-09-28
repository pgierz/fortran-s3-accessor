module test_uninitialized_cases
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use s3_http
    implicit none
    private

    public :: collect_uninitialized_cases

contains

    !> Collect all uninitialized state tests
    subroutine collect_uninitialized_cases(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("get_object_uninitialized", test_get_object_uninitialized), &
            new_unittest("object_exists_uninitialized", test_object_exists_uninitialized), &
            new_unittest("put_object_uninitialized", test_put_object_uninitialized), &
            new_unittest("delete_object_uninitialized", test_delete_object_uninitialized) &
        ]
    end subroutine collect_uninitialized_cases

    !> Test S3 get_object function before initialization
    subroutine test_get_object_uninitialized(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: content
        logical :: success

        ! DO NOT call s3_init() - test uninitialized state

        ! Test getting an object without initialization
        success = s3_get_object("test_object.txt", content)

        call check(error, .not. success, "GET should fail when uninitialized")
    end subroutine test_get_object_uninitialized

    !> Test S3 object_exists function before initialization
    subroutine test_object_exists_uninitialized(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: exists

        ! DO NOT call s3_init() - test uninitialized state

        ! Test checking object existence without initialization
        exists = s3_object_exists("test_object.txt")

        call check(error, .not. exists, "object_exists should fail when uninitialized")
    end subroutine test_object_exists_uninitialized

    !> Test S3 put_object function before initialization
    subroutine test_put_object_uninitialized(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: success

        ! DO NOT call s3_init() - test uninitialized state

        ! Test putting an object without initialization
        success = s3_put_object("test_upload.txt", "test content")

        call check(error, .not. success, "PUT should fail when uninitialized")
    end subroutine test_put_object_uninitialized

    !> Test S3 delete_object function before initialization
    subroutine test_delete_object_uninitialized(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: success

        ! DO NOT call s3_init() - test uninitialized state

        ! Test deleting an object without initialization
        success = s3_delete_object("test_delete.txt")

        call check(error, .not. success, "DELETE should fail when uninitialized")
    end subroutine test_delete_object_uninitialized

end module test_uninitialized_cases