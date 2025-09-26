!> Main module for Fortran S3 Accessor Library
!> @author Paul Gierz
!>
!> This module provides the main interface for the Fortran S3 accessor library,
!> re-exporting the most commonly used procedures and types.
module fortran_s3_accessor
    ! Re-export configuration types and procedures
    use s3_config_mod, only: s3_config_t, s3_config

    ! Re-export backend types
    use s3_backend_mod, only: s3_backend_t, s3_mock_backend_t, s3_object_metadata_t

    ! Re-export interface procedures
    use s3_interface_mod, only: s3_initialize, s3_finalize, s3_open, s3_close, &
                               s3_read, s3_write, s3_inquire, s3_get_unit, &
                               s3_file_exists, s3_file_t

    ! Re-export file operations
    use s3_file_ops_mod, only: file_open, file_close, file_read, file_write, &
                              file_read_all, file_exists, file_copy, file_move, &
                              file_delete, unified_file_t, s3_accessor_init, &
                              s3_accessor_version

    ! Re-export backend selector
    use s3_backend_selector_mod, only: list_available_backends

    ! Re-export version info
    use version_mod, only: VERSION, GIT_HASH, BUILD_DATE, LIBRARY_NAME, &
                          print_version_info

    implicit none
    public

    ! Everything is public by default due to the re-exports above

end module fortran_s3_accessor
