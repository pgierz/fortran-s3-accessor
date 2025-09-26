!> S3 Interface Module - Provides Fortran I/O-like interface to S3
!> @author Paul Gierz
!>
!> This module provides a familiar Fortran I/O interface for S3 operations,
!> mimicking standard open/read/write/close operations
module s3_interface_mod
    use s3_config_mod, only: s3_config_t, s3_config
    use s3_backend_mod, only: s3_backend_t, s3_mock_backend_t, s3_object_metadata_t
    use s3_backend_selector_mod, only: select_backend, list_available_backends
    implicit none
    private

    !> Maximum number of simultaneous S3 file handles
    integer, parameter, public :: MAX_S3_UNITS = 1000

    !> S3 file handle type
    type, public :: s3_file_t
        logical :: is_open = .false.         !< File open status
        integer :: unit = -1                 !< Unit number
        character(len=512) :: key = ''       !< S3 object key/path
        character(len=16) :: mode = ''       !< Access mode (read/write/append)
        integer :: position = 1              !< Current position in file
        character(len=:), allocatable :: buffer !< File content buffer
        logical :: modified = .false.        !< Buffer modified flag
        type(s3_object_metadata_t) :: metadata !< Object metadata
    contains
        procedure, public :: open => s3_file_open
        procedure, public :: close => s3_file_close
        procedure, public :: read_line => s3_file_read_line
        procedure, public :: write_line => s3_file_write_line
        procedure, public :: read_all => s3_file_read_all
        procedure, public :: write_all => s3_file_write_all
        procedure, public :: rewind => s3_file_rewind
        procedure, public :: backspace => s3_file_backspace
        procedure, public :: endfile => s3_file_endfile
        procedure, public :: flush => s3_file_flush
    end type s3_file_t

    !> Global S3 file handle array
    type(s3_file_t), save :: s3_files(MAX_S3_UNITS)

    !> Backend instance
    class(s3_backend_t), allocatable, save :: backend

    !> Public procedures
    public :: s3_open, s3_close, s3_read, s3_write
    public :: s3_inquire, s3_rewind, s3_backspace, s3_endfile
    public :: s3_initialize, s3_finalize
    public :: s3_get_unit, s3_file_exists

contains

    !> Initialize S3 interface with configuration
    !> @param config Optional configuration (uses global if not provided)
    !> @param status Output status (0=success)
    subroutine s3_initialize(config, status)
        type(s3_config_t), intent(in), optional :: config
        integer, intent(out), optional :: status

        type(s3_config_t) :: cfg
        integer :: stat

        ! Use provided config or global
        if (present(config)) then
            cfg = config
        else
            cfg = s3_config
        end if

        ! Select backend using SELECT CASE based on config
        if (.not. allocated(backend)) then
            backend = select_backend(cfg%backend_type)

            ! Check if backend was selected successfully
            if (.not. allocated(backend)) then
                print *, 'ERROR: Failed to select backend "', trim(cfg%backend_type), '"'
                call list_available_backends()

                ! Fall back to mock backend
                print *, 'Falling back to mock backend'
                backend = select_backend('mock')
                if (.not. allocated(backend)) then
                    if (present(status)) status = -1
                    return
                end if
            end if
        end if

        ! Initialize backend
        call backend%init(cfg, stat)

        if (present(status)) status = stat
    end subroutine s3_initialize

    !> Finalize S3 interface
    subroutine s3_finalize()
        integer :: i

        ! Close all open files
        do i = 1, MAX_S3_UNITS
            if (s3_files(i)%is_open) then
                call s3_files(i)%close()
            end if
        end do

        ! Cleanup backend
        if (allocated(backend)) then
            call backend%cleanup()
            deallocate(backend)
        end if
    end subroutine s3_finalize

    !> Get a free unit number
    !> @return Free unit number or -1 if none available
    function s3_get_unit() result(unit)
        integer :: unit
        integer :: i

        unit = -1
        do i = 1, MAX_S3_UNITS
            if (.not. s3_files(i)%is_open) then
                unit = i
                return
            end if
        end do
    end function s3_get_unit

    !> Open S3 object for reading/writing
    subroutine s3_open(unit, file, mode, status, action, iostat)
        integer, intent(inout) :: unit
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: mode
        character(len=*), intent(in), optional :: status
        character(len=*), intent(in), optional :: action
        integer, intent(out), optional :: iostat

        integer :: ios

        ! Get unit if needed
        if (unit <= 0) then
            unit = s3_get_unit()
            if (unit < 0) then
                if (present(iostat)) iostat = -1
                return
            end if
        end if

        ! Open file
        call s3_files(unit)%open(file, mode, status, action, ios)
        if (present(iostat)) iostat = ios
    end subroutine s3_open

    !> Close S3 object
    subroutine s3_close(unit, iostat, status)
        integer, intent(in) :: unit
        integer, intent(out), optional :: iostat
        character(len=*), intent(in), optional :: status

        integer :: ios

        if (unit > 0 .and. unit <= MAX_S3_UNITS) then
            call s3_files(unit)%close(ios)
            if (present(iostat)) iostat = ios
        else
            if (present(iostat)) iostat = -1
        end if
    end subroutine s3_close

    !> Read from S3 object
    subroutine s3_read(unit, data, iostat)
        integer, intent(in) :: unit
        character(len=*), intent(out) :: data
        integer, intent(out), optional :: iostat

        integer :: ios

        if (unit > 0 .and. unit <= MAX_S3_UNITS) then
            call s3_files(unit)%read_line(data, ios)
            if (present(iostat)) iostat = ios
        else
            if (present(iostat)) iostat = -1
        end if
    end subroutine s3_read

    !> Write to S3 object
    subroutine s3_write(unit, data, iostat)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: data
        integer, intent(out), optional :: iostat

        integer :: ios

        if (unit > 0 .and. unit <= MAX_S3_UNITS) then
            call s3_files(unit)%write_line(data, ios)
            if (present(iostat)) iostat = ios
        else
            if (present(iostat)) iostat = -1
        end if
    end subroutine s3_write

    !> Inquire about S3 object
    subroutine s3_inquire(file, exist, opened, unit, iostat)
        character(len=*), intent(in), optional :: file
        logical, intent(out), optional :: exist
        logical, intent(out), optional :: opened
        integer, intent(inout), optional :: unit
        integer, intent(out), optional :: iostat

        integer :: i
        logical :: found

        if (present(file)) then
            if (present(exist)) then
                exist = s3_file_exists(file)
            end if

            if (present(opened)) then
                found = .false.
                do i = 1, MAX_S3_UNITS
                    if (s3_files(i)%is_open .and. s3_files(i)%key == file) then
                        found = .true.
                        if (present(unit)) unit = i
                        exit
                    end if
                end do
                opened = found
            end if
        end if

        if (present(iostat)) iostat = 0
    end subroutine s3_inquire

    !> Check if S3 object exists
    function s3_file_exists(file) result(exists)
        character(len=*), intent(in) :: file
        logical :: exists

        if (allocated(backend)) then
            exists = backend%exists(file)
        else
            exists = .false.
        end if
    end function s3_file_exists

    !> Rewind S3 file
    subroutine s3_rewind(unit, iostat)
        integer, intent(in) :: unit
        integer, intent(out), optional :: iostat

        if (unit > 0 .and. unit <= MAX_S3_UNITS) then
            call s3_files(unit)%rewind()
            if (present(iostat)) iostat = 0
        else
            if (present(iostat)) iostat = -1
        end if
    end subroutine s3_rewind

    !> Backspace S3 file
    subroutine s3_backspace(unit, iostat)
        integer, intent(in) :: unit
        integer, intent(out), optional :: iostat

        if (unit > 0 .and. unit <= MAX_S3_UNITS) then
            call s3_files(unit)%backspace()
            if (present(iostat)) iostat = 0
        else
            if (present(iostat)) iostat = -1
        end if
    end subroutine s3_backspace

    !> Write endfile record
    subroutine s3_endfile(unit, iostat)
        integer, intent(in) :: unit
        integer, intent(out), optional :: iostat

        if (unit > 0 .and. unit <= MAX_S3_UNITS) then
            call s3_files(unit)%endfile()
            if (present(iostat)) iostat = 0
        else
            if (present(iostat)) iostat = -1
        end if
    end subroutine s3_endfile

    ! S3 File Type Methods

    !> Open file
    subroutine s3_file_open(this, key, mode, status, action, iostat)
        class(s3_file_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(in), optional :: mode
        character(len=*), intent(in), optional :: status
        character(len=*), intent(in), optional :: action
        integer, intent(out) :: iostat

        character(len=16) :: fmode, fstatus
        logical :: exists

        ! Set defaults
        fmode = 'read'
        if (present(mode)) fmode = mode
        fstatus = 'old'
        if (present(status)) fstatus = status

        ! Check if file exists
        exists = backend%exists(key)

        ! Handle status
        if (fstatus == 'old' .and. .not. exists) then
            iostat = -1
            return
        else if (fstatus == 'new' .and. exists) then
            iostat = -1
            return
        end if

        ! Store file info
        this%key = key
        this%mode = fmode
        this%is_open = .true.
        this%position = 1
        this%modified = .false.

        ! Load content for reading/appending
        if (exists .and. (fmode == 'read' .or. fmode == 'append')) then
            call backend%get_object(key, this%buffer, iostat)
            if (fmode == 'append') then
                this%position = len(this%buffer) + 1
            end if
        else
            this%buffer = ''
            iostat = 0
        end if
    end subroutine s3_file_open

    !> Close file
    subroutine s3_file_close(this, iostat)
        class(s3_file_t), intent(inout) :: this
        integer, intent(out), optional :: iostat

        integer :: ios

        ios = 0

        ! Flush if modified
        if (this%modified) then
            call this%flush(ios)
        end if

        ! Reset state
        this%is_open = .false.
        this%key = ''
        this%mode = ''
        this%position = 1
        this%modified = .false.
        if (allocated(this%buffer)) deallocate(this%buffer)

        if (present(iostat)) iostat = ios
    end subroutine s3_file_close

    !> Read line from file
    subroutine s3_file_read_line(this, line, iostat)
        class(s3_file_t), intent(inout) :: this
        character(len=*), intent(out) :: line
        integer, intent(out) :: iostat

        integer :: i, newline_pos, buflen

        if (.not. this%is_open) then
            iostat = -1
            return
        end if

        if (.not. allocated(this%buffer)) then
            iostat = -1
            return
        end if

        buflen = len(this%buffer)
        if (this%position > buflen) then
            iostat = -1  ! EOF
            return
        end if

        ! Find next newline
        newline_pos = 0
        do i = this%position, buflen
            if (this%buffer(i:i) == new_line('a')) then
                newline_pos = i
                exit
            end if
        end do

        if (newline_pos == 0) then
            ! No newline found - read to end
            line = this%buffer(this%position:buflen)
            this%position = buflen + 1
        else
            ! Read to newline
            if (newline_pos > this%position) then
                line = this%buffer(this%position:newline_pos-1)
            else
                line = ''
            end if
            this%position = newline_pos + 1
        end if

        iostat = 0
    end subroutine s3_file_read_line

    !> Write line to file
    subroutine s3_file_write_line(this, line, iostat)
        class(s3_file_t), intent(inout) :: this
        character(len=*), intent(in) :: line
        integer, intent(out) :: iostat

        if (.not. this%is_open) then
            iostat = -1
            return
        end if

        ! Append to buffer
        if (allocated(this%buffer)) then
            this%buffer = this%buffer // line // new_line('a')
        else
            this%buffer = line // new_line('a')
        end if

        this%modified = .true.
        iostat = 0
    end subroutine s3_file_write_line

    !> Read entire file
    subroutine s3_file_read_all(this, data, iostat)
        class(s3_file_t), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: data
        integer, intent(out) :: iostat

        if (.not. this%is_open) then
            iostat = -1
            return
        end if

        if (allocated(this%buffer)) then
            data = this%buffer
            iostat = 0
        else
            iostat = -1
        end if
    end subroutine s3_file_read_all

    !> Write entire file
    subroutine s3_file_write_all(this, data, iostat)
        class(s3_file_t), intent(inout) :: this
        character(len=*), intent(in) :: data
        integer, intent(out) :: iostat

        if (.not. this%is_open) then
            iostat = -1
            return
        end if

        this%buffer = data
        this%modified = .true.
        iostat = 0
    end subroutine s3_file_write_all

    !> Rewind file
    subroutine s3_file_rewind(this)
        class(s3_file_t), intent(inout) :: this
        this%position = 1
    end subroutine s3_file_rewind

    !> Backspace one record
    subroutine s3_file_backspace(this)
        class(s3_file_t), intent(inout) :: this
        integer :: i

        if (this%position <= 1) return

        ! Find previous newline
        do i = this%position - 2, 1, -1
            if (this%buffer(i:i) == new_line('a')) then
                this%position = i + 1
                return
            end if
        end do

        this%position = 1
    end subroutine s3_file_backspace

    !> Write endfile
    subroutine s3_file_endfile(this)
        class(s3_file_t), intent(inout) :: this

        if (allocated(this%buffer) .and. this%position <= len(this%buffer)) then
            this%buffer = this%buffer(1:this%position-1)
            this%modified = .true.
        end if
    end subroutine s3_file_endfile

    !> Flush buffer to backend
    subroutine s3_file_flush(this, iostat)
        class(s3_file_t), intent(inout) :: this
        integer, intent(out), optional :: iostat

        integer :: ios

        if (this%modified .and. allocated(this%buffer)) then
            call backend%put_object(this%key, this%buffer, ios)
            if (ios == 0) this%modified = .false.
            if (present(iostat)) iostat = ios
        else
            if (present(iostat)) iostat = 0
        end if
    end subroutine s3_file_flush

end module s3_interface_mod