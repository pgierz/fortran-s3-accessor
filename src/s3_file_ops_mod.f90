!> File operations module for S3 accessor
!> @author Paul Gierz
!>
!> This module provides additional file operations that build on
!> the core S3 interface, similar to POSIX file operations
module s3_file_ops_mod
    use s3_config_mod, only: s3_config_t, s3_config
    use s3_interface_mod, only: s3_open, s3_close, s3_read, s3_write, s3_get_unit, &
                               s3_file_exists, s3_initialize, s3_finalize, s3_file_t, s3_files
    implicit none
    private

    !> Unified file type that can work with both local and S3 files
    type, public :: unified_file_t
        logical :: is_s3 = .false.           !< True if S3 file
        logical :: is_open = .false.         !< File open status
        integer :: unit = -1                 !< Unit number
        character(len=512) :: filename = ''  !< File name/key
        character(len=16) :: mode = ''       !< Access mode
    contains
        procedure, public :: open => ufile_open
        procedure, public :: close => ufile_close
        procedure, public :: read_line => ufile_read_line
        procedure, public :: write_line => ufile_write_line
        procedure, public :: exists => ufile_exists
    end type unified_file_t

    !> Public procedures
    public :: file_open, file_close, file_read, file_write, file_exists
    public :: file_read_all
    public :: file_copy, file_move, file_delete
    public :: file_list_directory
    public :: s3_accessor_init, s3_accessor_version

contains

    !> Initialize S3 accessor library
    subroutine s3_accessor_init(config, status)
        type(s3_config_t), intent(in), optional :: config
        integer, intent(out), optional :: status

        call s3_initialize(config, status)
    end subroutine s3_accessor_init

    !> Get library version
    function s3_accessor_version() result(version)
        character(len=:), allocatable :: version
        version = '0.1.0'
    end function s3_accessor_version

    !> Open file (S3 or local based on configuration)
    subroutine file_open(filename, mode, unit, iostat)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: mode
        integer, intent(out) :: unit
        integer, intent(out), optional :: iostat

        if (s3_config%use_s3) then
            call s3_open(unit, filename, mode=mode, iostat=iostat)
        else
            open(newunit=unit, file=filename, iostat=iostat)
        end if
    end subroutine file_open

    !> Close file
    subroutine file_close(unit, iostat)
        integer, intent(in) :: unit
        integer, intent(out), optional :: iostat

        if (s3_config%use_s3) then
            call s3_close(unit, iostat=iostat)
        else
            close(unit, iostat=iostat)
        end if
    end subroutine file_close

    !> Read from file (one line/record, like standard Fortran read)
    subroutine file_read(unit, data, iostat)
        integer, intent(in) :: unit
        character(len=*), intent(out) :: data
        integer, intent(out), optional :: iostat

        if (s3_config%use_s3) then
            call s3_read(unit, data, iostat=iostat)
        else
            read(unit, '(A)', iostat=iostat) data
        end if
    end subroutine file_read

    !> Read entire file content
    subroutine file_read_all(unit, data, iostat)
        integer, intent(in) :: unit
        character(len=:), allocatable, intent(out) :: data
        integer, intent(out), optional :: iostat

        integer :: ios

        if (s3_config%use_s3) then
            ! Access the S3 file handle directly to get all content
            if (unit > 0 .and. unit <= 1000) then
                call s3_files(unit)%read_all(data, ios)
            else
                ios = -1
            end if
        else
            ! For local files, read everything using stream access
            integer :: filesize
            inquire(unit=unit, size=filesize, iostat=ios)
            if (ios == 0) then
                allocate(character(len=filesize) :: data)
                read(unit, iostat=ios) data
            end if
        end if

        if (present(iostat)) iostat = ios
    end subroutine file_read_all

    !> Write to file
    subroutine file_write(unit, data, iostat)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: data
        integer, intent(out), optional :: iostat

        if (s3_config%use_s3) then
            call s3_write(unit, data, iostat=iostat)
        else
            write(unit, '(A)', iostat=iostat) data
        end if
    end subroutine file_write

    !> Check if file exists
    function file_exists(filename) result(exists)
        character(len=*), intent(in) :: filename
        logical :: exists

        if (s3_config%use_s3) then
            exists = s3_file_exists(filename)
        else
            inquire(file=filename, exist=exists)
        end if
    end function file_exists

    !> Copy file
    subroutine file_copy(source, dest, iostat)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: dest
        integer, intent(out), optional :: iostat

        integer :: src_unit, dst_unit, ios
        character(len=1024) :: line

        ios = 0

        ! Open source file
        call file_open(source, 'read', src_unit, ios)
        if (ios /= 0) then
            if (present(iostat)) iostat = ios
            return
        end if

        ! Open destination file
        call file_open(dest, 'write', dst_unit, ios)
        if (ios /= 0) then
            call file_close(src_unit)
            if (present(iostat)) iostat = ios
            return
        end if

        ! Copy content line by line (standard Fortran pattern)
        do
            call file_read(src_unit, line, ios)
            if (ios /= 0) exit
            call file_write(dst_unit, trim(line), ios)
            if (ios /= 0) exit
        end do

        ! Close files
        call file_close(src_unit)
        call file_close(dst_unit)

        if (present(iostat)) iostat = ios
    end subroutine file_copy

    !> Move file
    subroutine file_move(source, dest, iostat)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: dest
        integer, intent(out), optional :: iostat

        integer :: ios

        ! Copy file
        call file_copy(source, dest, ios)
        if (ios /= 0) then
            if (present(iostat)) iostat = ios
            return
        end if

        ! Delete source
        call file_delete(source, ios)
        if (present(iostat)) iostat = ios
    end subroutine file_move

    !> Delete file
    subroutine file_delete(filename, iostat)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: iostat

        integer :: unit, ios

        if (s3_config%use_s3) then
            ! For S3, use a dummy unit to access the backend
            unit = s3_get_unit()
            if (unit > 0) then
                ! Open the file to get backend access, then delete via backend
                call s3_open(unit, filename, mode='read', iostat=ios)
                if (ios == 0) then
                    call s3_close(unit, iostat=ios)
                    ! Use backend directly for delete - access through global backend
                    ! This requires exposing backend or adding delete interface
                    ios = -1  ! Not fully implemented yet
                else
                    ios = -1
                end if
            else
                ios = -1
            end if
        else
            open(newunit=unit, file=filename, status='old', iostat=ios)
            if (ios == 0) then
                close(unit, status='delete', iostat=ios)
            end if
        end if

        if (present(iostat)) iostat = ios
    end subroutine file_delete

    !> List directory contents
    subroutine file_list_directory(path, files, count)
        character(len=*), intent(in) :: path
        character(len=256), allocatable, intent(out) :: files(:)
        integer, intent(out) :: count

        character(len=1024) :: command
        character(len=256) :: tempfile
        integer :: unit, ios
        character(len=256) :: line

        ! Use ls command to get directory listing
        write(tempfile, '(A,I0)') '/tmp/dirlist_', 12345
        write(command, '(A,A,A,A,A)') 'ls -1 "', trim(path), '" > ', trim(tempfile), ' 2>/dev/null'

        call execute_command_line(trim(command), exitstat=ios)

        if (ios == 0) then
            ! Read the file to count lines first
            open(newunit=unit, file=tempfile, status='old', iostat=ios)
            if (ios == 0) then
                count = 0
                do
                    read(unit, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    count = count + 1
                end do
                close(unit)

                ! Allocate array and read files
                allocate(files(count))
                open(newunit=unit, file=tempfile, status='old')
                do ios = 1, count
                    read(unit, '(A)') files(ios)
                end do
                close(unit)
            else
                allocate(files(0))
                count = 0
            end if
        else
            allocate(files(0))
            count = 0
        end if

        ! Clean up temp file
        call execute_command_line('rm -f ' // trim(tempfile))
    end subroutine file_list_directory

    ! Unified file type methods

    !> Open unified file
    subroutine ufile_open(this, filename, mode, iostat)
        class(unified_file_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: mode
        integer, intent(out), optional :: iostat

        integer :: ios

        this%filename = filename
        this%mode = mode
        this%is_s3 = s3_config%use_s3

        if (this%is_s3) then
            call s3_open(this%unit, filename, mode=mode, iostat=ios)
        else
            open(newunit=this%unit, file=filename, iostat=ios)
        end if

        if (ios == 0) then
            this%is_open = .true.
        end if

        if (present(iostat)) iostat = ios
    end subroutine ufile_open

    !> Close unified file
    subroutine ufile_close(this, iostat)
        class(unified_file_t), intent(inout) :: this
        integer, intent(out), optional :: iostat

        integer :: ios

        if (this%is_open) then
            if (this%is_s3) then
                call s3_close(this%unit, iostat=ios)
            else
                close(this%unit, iostat=ios)
            end if

            this%is_open = .false.
            this%unit = -1
        else
            ios = 0
        end if

        if (present(iostat)) iostat = ios
    end subroutine ufile_close

    !> Read line from unified file
    subroutine ufile_read_line(this, line, iostat)
        class(unified_file_t), intent(inout) :: this
        character(len=*), intent(out) :: line
        integer, intent(out), optional :: iostat

        integer :: ios

        if (.not. this%is_open) then
            ios = -1
        else if (this%is_s3) then
            call s3_read(this%unit, line, iostat=ios)
        else
            read(this%unit, '(A)', iostat=ios) line
        end if

        if (present(iostat)) iostat = ios
    end subroutine ufile_read_line

    !> Write line to unified file
    subroutine ufile_write_line(this, line, iostat)
        class(unified_file_t), intent(inout) :: this
        character(len=*), intent(in) :: line
        integer, intent(out), optional :: iostat

        integer :: ios

        if (.not. this%is_open) then
            ios = -1
        else if (this%is_s3) then
            call s3_write(this%unit, line, iostat=ios)
        else
            write(this%unit, '(A)', iostat=ios) line
        end if

        if (present(iostat)) iostat = ios
    end subroutine ufile_write_line

    !> Check if unified file exists
    function ufile_exists(this, filename) result(exists)
        class(unified_file_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        logical :: exists

        exists = file_exists(filename)
    end function ufile_exists

end module s3_file_ops_mod