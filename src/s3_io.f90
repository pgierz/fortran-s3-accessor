!> High-level Fortran I/O interface for S3 operations.
!>
!> This module provides a familiar Fortran-style I/O interface for working with S3 objects.
!> It supports operations similar to standard Fortran file I/O: open, read, write, close, and rewind.
!> The module internally buffers content for efficient line-by-line operations.
!>
!> ## Features
!>
!> - Familiar Fortran I/O patterns (open/read/write/close)
!> - Line-based text file operations
!> - Internal buffering for efficient I/O
!> - Support for up to 100 concurrent file handles
!> - Automatic upload on close for write operations
!>
!> ## Usage
!>
!> ```fortran
!> use s3_http
!> use s3_io
!> type(s3_config) :: config
!> integer :: unit, iostat
!> character(len=1024) :: line
!>
!> ! Initialize S3
!> config%bucket = 'my-bucket'
!> call s3_init(config)
!>
!> ! Open and read
!> call s3_open(unit, 'data/input.txt', 'read', iostat)
!> call s3_read_line(unit, line, iostat)
!> call s3_close(unit, iostat)
!> ```
!>
!> @note This module depends on the s3_http module for underlying S3 operations.
module s3_io
    use s3_http
    implicit none
    private

    integer, parameter :: MAX_FILES = 100  !< Maximum number of concurrent open files

    !> Internal file handle type for managing S3 objects as file-like entities.
    !>
    !> This type maintains the state of an open S3 object, including its content buffer,
    !> read/write position, and mode. Used internally by the module.
    type :: s3_file
        logical :: is_open = .false.                !< Whether this file handle is in use
        character(len=256) :: key = ''              !< S3 object key
        character(len=:), allocatable :: buffer     !< Content buffer
        integer :: position = 1                     !< Current read/write position
        logical :: is_write = .false.               !< Write mode flag
    end type s3_file

    type(s3_file), save :: files(MAX_FILES)

    public :: s3_open
    public :: s3_close
    public :: s3_read_line
    public :: s3_write_line
    public :: s3_rewind

contains

    !> Open an S3 object for reading or writing.
    !>
    !> Opens an S3 object and returns a unit number for subsequent I/O operations.
    !> For read mode, the object is downloaded immediately. For write mode, content
    !> is buffered in memory until s3_close() is called.
    !>
    !> @param[out] unit The allocated unit number (set to -1 on error)
    !> @param[in] key The S3 object key to open
    !> @param[in] mode Open mode: 'read'/'r' for reading, 'write'/'w' for writing
    !> @param[out] iostat Status code: 0 on success, -1 on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> integer :: unit, iostat
    !>
    !> ! Open for reading
    !> call s3_open(unit, 'data/input.txt', 'read', iostat)
    !> if (iostat == 0) then
    !>     ! Read operations...
    !>     call s3_close(unit, iostat)
    !> end if
    !> ```
    subroutine s3_open(unit, key, mode, iostat)
        integer, intent(out) :: unit
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: mode
        integer, intent(out) :: iostat
        integer :: i
        character(len=:), allocatable :: content

        iostat = 0
        unit = -1

        ! Find available unit
        do i = 1, MAX_FILES
            if (.not. files(i)%is_open) then
                unit = i
                exit
            end if
        end do

        if (unit < 0) then
            iostat = -1
            return
        end if

        files(unit)%key = key
        files(unit)%is_open = .true.
        files(unit)%position = 1

        select case(mode)
        case('read', 'r')
            files(unit)%is_write = .false.
            ! Download the file content
            if (s3_get_object(key, content)) then
                files(unit)%buffer = content
            else
                iostat = -1
                files(unit)%is_open = .false.
            end if

        case('write', 'w')
            files(unit)%is_write = .true.
            files(unit)%buffer = ''

        case default
            iostat = -1
            files(unit)%is_open = .false.
        end select
    end subroutine s3_open

    !> Close an S3 file handle.
    !>
    !> Closes an open S3 file handle. For write mode, this uploads the buffered
    !> content to S3. The file handle is released and can be reused.
    !>
    !> @param[in] unit The unit number to close
    !> @param[out] iostat Status code: 0 on success, -1 on error
    !>
    !> @warning For write mode, upload errors will be reflected in iostat.
    !>
    !> ## Example
    !>
    !> ```fortran
    !> call s3_close(unit, iostat)
    !> if (iostat /= 0) then
    !>     print *, 'Error closing file'
    !> end if
    !> ```
    subroutine s3_close(unit, iostat)
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        logical :: success

        iostat = 0

        if (unit < 1 .or. unit > MAX_FILES) then
            iostat = -1
            return
        end if

        if (.not. files(unit)%is_open) then
            iostat = -1
            return
        end if

        ! If writing, upload the buffer
        if (files(unit)%is_write .and. allocated(files(unit)%buffer)) then
            success = s3_put_object(files(unit)%key, files(unit)%buffer)
            if (.not. success) iostat = -1
        end if

        ! Clean up
        files(unit)%is_open = .false.
        files(unit)%key = ''
        files(unit)%position = 1
        files(unit)%is_write = .false.
        if (allocated(files(unit)%buffer)) deallocate(files(unit)%buffer)
    end subroutine s3_close

    !> Read a line from an open S3 file.
    !>
    !> Reads the next line from the file buffer. Lines are delimited by newline characters.
    !> The file must be opened in read mode.
    !>
    !> @param[in] unit The unit number to read from
    !> @param[out] line The line content (truncated if longer than buffer)
    !> @param[out] iostat Status code: 0 on success, -1 on EOF or error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> character(len=1024) :: line
    !> integer :: iostat
    !>
    !> do
    !>     call s3_read_line(unit, line, iostat)
    !>     if (iostat /= 0) exit
    !>     print *, trim(line)
    !> end do
    !> ```
    subroutine s3_read_line(unit, line, iostat)
        integer, intent(in) :: unit
        character(len=*), intent(out) :: line
        integer, intent(out) :: iostat
        integer :: i, line_end, buffer_len

        iostat = 0
        line = ''

        if (unit < 1 .or. unit > MAX_FILES) then
            iostat = -1
            return
        end if

        if (.not. files(unit)%is_open .or. files(unit)%is_write) then
            iostat = -1
            return
        end if

        if (.not. allocated(files(unit)%buffer)) then
            iostat = -1
            return
        end if

        buffer_len = len(files(unit)%buffer)

        ! Check if at end of file
        if (files(unit)%position > buffer_len) then
            iostat = -1  ! EOF
            return
        end if

        ! Find next newline
        line_end = 0
        do i = files(unit)%position, buffer_len
            if (files(unit)%buffer(i:i) == new_line('')) then
                line_end = i - 1
                exit
            end if
        end do

        ! If no newline found, read to end
        if (line_end == 0) then
            line_end = buffer_len
        end if

        ! Extract line
        if (line_end >= files(unit)%position) then
            line = files(unit)%buffer(files(unit)%position:line_end)
            files(unit)%position = line_end + 2  ! Skip newline
        else
            iostat = -1
        end if
    end subroutine s3_read_line

    !> Write a line to an open S3 file.
    !>
    !> Appends a line to the file buffer. A newline character is automatically added.
    !> The file must be opened in write mode. Content is uploaded when s3_close() is called.
    !>
    !> @param[in] unit The unit number to write to
    !> @param[in] line The line content to write
    !> @param[out] iostat Status code: 0 on success, -1 on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> call s3_write_line(unit, 'temperature,pressure', iostat)
    !> call s3_write_line(unit, '25.3,1013.2', iostat)
    !> ```
    subroutine s3_write_line(unit, line, iostat)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: line
        integer, intent(out) :: iostat
        character(len=:), allocatable :: new_buffer

        iostat = 0

        if (unit < 1 .or. unit > MAX_FILES) then
            iostat = -1
            return
        end if

        if (.not. files(unit)%is_open .or. .not. files(unit)%is_write) then
            iostat = -1
            return
        end if

        ! Append line to buffer
        if (.not. allocated(files(unit)%buffer)) then
            files(unit)%buffer = trim(line) // new_line('')
        else
            new_buffer = files(unit)%buffer // trim(line) // new_line('')
            files(unit)%buffer = new_buffer
        end if
    end subroutine s3_write_line

    !> Rewind an S3 file to the beginning.
    !>
    !> Resets the read position to the start of the file buffer. Only valid for read mode.
    !>
    !> @param[in] unit The unit number to rewind
    !> @param[out] iostat Status code: 0 on success, -1 on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> ! Read file twice
    !> call s3_open(unit, 'data/file.txt', 'read', iostat)
    !> ! ... read operations ...
    !> call s3_rewind(unit, iostat)
    !> ! ... read again from start ...
    !> call s3_close(unit, iostat)
    !> ```
    subroutine s3_rewind(unit, iostat)
        integer, intent(in) :: unit
        integer, intent(out) :: iostat

        iostat = 0

        if (unit < 1 .or. unit > MAX_FILES) then
            iostat = -1
            return
        end if

        if (.not. files(unit)%is_open) then
            iostat = -1
            return
        end if

        files(unit)%position = 1
    end subroutine s3_rewind

end module s3_io