!> Backend Selector Module - Select between compiled-in S3 backends
!> @author Paul Gierz
!>
!> This module provides a SELECT CASE based backend selection mechanism.
!> All backends must be compiled in - this is NOT a dynamic factory!
!> It's just a switch statement that picks between available backends.
module s3_backend_selector_mod
    use s3_backend_mod, only: s3_backend_t, s3_mock_backend_t
    use s3_config_mod, only: s3_config_t
    implicit none
    private

    !> Public procedures
    public :: select_backend
    public :: list_available_backends

contains

    !> Select a backend based on name using a SELECT CASE statement
    !> @param backend_name Name of backend ('mock', 'http', etc.)
    !> @return Allocated backend or null if unknown
    function select_backend(backend_name) result(backend)
        character(len=*), intent(in) :: backend_name
        class(s3_backend_t), allocatable :: backend

        ! This is just a SELECT CASE - all backends must be compiled in!
        select case(trim(backend_name))

        case('mock', 'file')
            ! Mock backend for local filesystem
            allocate(s3_mock_backend_t :: backend)
            print *, 'Selected mock/file backend (local filesystem)'

        case('http')
            ! HTTP backend using curl commands
            block
                use s3_http_backend_mod, only: s3_http_backend_t
                allocate(s3_http_backend_t :: backend)
                print *, 'Selected HTTP backend (using curl)'
            end block

        case default
            ! Unknown backend
            print *, 'ERROR: Unknown backend "', trim(backend_name), '"'
            print *, 'Available backends: mock, file, http'
            ! Return unallocated

        end select

    end function select_backend

    !> List available backends (hardcoded - these are all compiled in)
    subroutine list_available_backends()
        print *, 'Available S3 Backends (compiled-in):'
        print *, '------------------------------------'
        print *, '  mock - Local filesystem mock backend'
        print *, '  file - Local filesystem (alias for mock)'
        print *, '  http - HTTP REST API using curl commands'
        print *, ''
        print *, 'Note: All backends are compiled in. This is not a plugin system.'
    end subroutine list_available_backends

end module s3_backend_selector_mod