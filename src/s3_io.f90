module s3_io
    use s3_http
    implicit none
    private

    integer, parameter :: MAX_FILES = 100

    type :: s3_file
        logical :: is_open = .false.
        character(len=256) :: key = ''
        character(len=:), allocatable :: buffer
        integer :: position = 1
        logical :: is_write = .false.
    end type s3_file

    type(s3_file), save :: files(MAX_FILES)

    public :: s3_open
    public :: s3_close
    public :: s3_read_line
    public :: s3_write_line
    public :: s3_rewind

contains

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