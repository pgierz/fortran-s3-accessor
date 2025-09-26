!> Abstract backend interface for S3 operations
!> @author Paul Gierz
!>
!> This module defines the abstract interface for S3 backends,
!> allowing different implementations (e.g., via C bindings, HTTP, etc.)
module s3_backend_mod
    use s3_config_mod, only: s3_config_t
    implicit none
    private

    !> Abstract backend interface
    type, abstract, public :: s3_backend_t
    contains
        procedure(backend_init_interface), deferred, public :: init
        procedure(backend_cleanup_interface), deferred, public :: cleanup
        procedure(backend_get_object_interface), deferred, public :: get_object
        procedure(backend_put_object_interface), deferred, public :: put_object
        procedure(backend_delete_object_interface), deferred, public :: delete_object
        procedure(backend_exists_interface), deferred, public :: exists
        procedure(backend_list_objects_interface), deferred, public :: list_objects
        procedure(backend_get_metadata_interface), deferred, public :: get_metadata
    end type s3_backend_t

    !> Mock backend for local filesystem (testing/fallback)
    type, extends(s3_backend_t), public :: s3_mock_backend_t
        character(len=512) :: base_path = '.'
    contains
        procedure, public :: init => mock_init
        procedure, public :: cleanup => mock_cleanup
        procedure, public :: get_object => mock_get_object
        procedure, public :: put_object => mock_put_object
        procedure, public :: delete_object => mock_delete_object
        procedure, public :: exists => mock_exists
        procedure, public :: list_objects => mock_list_objects
        procedure, public :: get_metadata => mock_get_metadata
    end type s3_mock_backend_t

    !> Object metadata type
    type, public :: s3_object_metadata_t
        integer(kind=8) :: size = 0          !< Object size in bytes
        character(len=64) :: etag = ''       !< ETag/checksum
        character(len=32) :: last_modified = '' !< Last modification timestamp
        character(len=128) :: content_type = 'application/octet-stream'
    end type s3_object_metadata_t

    !> Interface definitions
    abstract interface
        subroutine backend_init_interface(this, config, status)
            import :: s3_backend_t, s3_config_t
            class(s3_backend_t), intent(inout) :: this
            type(s3_config_t), intent(in) :: config
            integer, intent(out) :: status
        end subroutine backend_init_interface

        subroutine backend_cleanup_interface(this)
            import :: s3_backend_t
            class(s3_backend_t), intent(inout) :: this
        end subroutine backend_cleanup_interface

        subroutine backend_get_object_interface(this, key, data, status)
            import :: s3_backend_t
            class(s3_backend_t), intent(inout) :: this
            character(len=*), intent(in) :: key
            character(len=:), allocatable, intent(out) :: data
            integer, intent(out) :: status
        end subroutine backend_get_object_interface

        subroutine backend_put_object_interface(this, key, data, status)
            import :: s3_backend_t
            class(s3_backend_t), intent(inout) :: this
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: data
            integer, intent(out) :: status
        end subroutine backend_put_object_interface

        subroutine backend_delete_object_interface(this, key, status)
            import :: s3_backend_t
            class(s3_backend_t), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer, intent(out) :: status
        end subroutine backend_delete_object_interface

        function backend_exists_interface(this, key) result(exists)
            import :: s3_backend_t
            class(s3_backend_t), intent(inout) :: this
            character(len=*), intent(in) :: key
            logical :: exists
        end function backend_exists_interface

        subroutine backend_list_objects_interface(this, prefix, keys, count, status)
            import :: s3_backend_t
            class(s3_backend_t), intent(inout) :: this
            character(len=*), intent(in) :: prefix
            character(len=256), allocatable, intent(out) :: keys(:)
            integer, intent(out) :: count
            integer, intent(out) :: status
        end subroutine backend_list_objects_interface

        subroutine backend_get_metadata_interface(this, key, metadata, status)
            import :: s3_backend_t, s3_object_metadata_t
            class(s3_backend_t), intent(inout) :: this
            character(len=*), intent(in) :: key
            type(s3_object_metadata_t), intent(out) :: metadata
            integer, intent(out) :: status
        end subroutine backend_get_metadata_interface
    end interface

contains

    !> Initialize mock backend
    subroutine mock_init(this, config, status)
        class(s3_mock_backend_t), intent(inout) :: this
        type(s3_config_t), intent(in) :: config
        integer, intent(out) :: status

        if (config%use_s3) then
            this%base_path = trim(config%cache_dir)
        else
            this%base_path = '.'
        end if
        status = 0
    end subroutine mock_init

    !> Cleanup mock backend
    subroutine mock_cleanup(this)
        class(s3_mock_backend_t), intent(inout) :: this
        ! Nothing to clean up for mock backend
    end subroutine mock_cleanup

    !> Get object from mock storage
    subroutine mock_get_object(this, key, data, status)
        class(s3_mock_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: data
        integer, intent(out) :: status

        character(len=512) :: filepath
        integer :: unit, filesize, ios
        logical :: file_exists

        ! Build file path
        filepath = trim(this%base_path) // '/' // trim(key)

        inquire(file=filepath, exist=file_exists, size=filesize)
        if (.not. file_exists) then
            status = -1
            return
        end if

        ! Allocate and read data
        allocate(character(len=filesize) :: data)
        open(newunit=unit, file=filepath, form='unformatted', access='stream', &
             status='old', iostat=ios)
        if (ios /= 0) then
            status = ios
            return
        end if
        read(unit) data
        close(unit)
        status = 0
    end subroutine mock_get_object

    !> Put object to mock storage
    subroutine mock_put_object(this, key, data, status)
        class(s3_mock_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: data
        integer, intent(out) :: status

        character(len=512) :: filepath
        integer :: unit, ios

        ! Build file path
        filepath = trim(this%base_path) // '/' // trim(key)

        open(newunit=unit, file=filepath, form='unformatted', access='stream', &
             status='replace', iostat=ios)
        if (ios /= 0) then
            status = ios
            return
        end if
        write(unit) data
        close(unit)
        status = 0
    end subroutine mock_put_object

    !> Delete object from mock storage
    subroutine mock_delete_object(this, key, status)
        class(s3_mock_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer, intent(out) :: status

        character(len=512) :: filepath
        integer :: unit, ios

        filepath = trim(this%base_path) // '/' // trim(key)
        open(newunit=unit, file=filepath, status='old', iostat=ios)
        if (ios /= 0) then
            status = ios
            return
        end if
        close(unit, status='delete')
        status = 0
    end subroutine mock_delete_object

    !> Check if object exists
    function mock_exists(this, key) result(exists)
        class(s3_mock_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        logical :: exists

        character(len=512) :: filepath

        filepath = trim(this%base_path) // '/' // trim(key)
        inquire(file=filepath, exist=exists)
    end function mock_exists

    !> List objects with prefix
    subroutine mock_list_objects(this, prefix, keys, count, status)
        class(s3_mock_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: prefix
        character(len=256), allocatable, intent(out) :: keys(:)
        integer, intent(out) :: count
        integer, intent(out) :: status

        ! Simplified implementation - allocate small array
        allocate(keys(100))
        count = 0
        status = 0
        ! In real implementation, would scan directory
    end subroutine mock_list_objects

    !> Get object metadata
    subroutine mock_get_metadata(this, key, metadata, status)
        class(s3_mock_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(s3_object_metadata_t), intent(out) :: metadata
        integer, intent(out) :: status

        character(len=512) :: filepath
        logical :: file_exists
        integer(kind=8) :: filesize

        filepath = trim(this%base_path) // '/' // trim(key)
        inquire(file=filepath, exist=file_exists, size=filesize)

        if (.not. file_exists) then
            status = -1
            return
        end if

        metadata%size = filesize
        metadata%content_type = 'application/octet-stream'
        status = 0
    end subroutine mock_get_metadata

end module s3_backend_mod