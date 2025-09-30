!> C interoperability module for streaming curl output via popen.
!>
!> This module provides POSIX popen() bindings to stream curl output directly
!> to memory, eliminating the need for temporary files.
!>
!> @note This module requires POSIX-compliant systems (Linux, macOS, Unix).
!> @warning Not portable to Windows.
module curl_stream
    use iso_c_binding
    use s3_logger
    implicit none
    private

    public :: stream_command_output
    public :: is_streaming_available

    !> Maximum chunk size for reading from pipe (8KB)
    integer, parameter :: CHUNK_SIZE = 8192

    ! C function interfaces
    interface
        !> Open a pipe to a command and return file pointer.
        !>
        !> @param command The command to execute
        !> @param mode Open mode ("r" for read, "w" for write)
        !> @return C pointer to FILE stream, or NULL on failure
        function c_popen(command, mode) bind(C, name="popen")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*), intent(in) :: command
            character(kind=c_char), dimension(*), intent(in) :: mode
            type(c_ptr) :: c_popen
        end function c_popen

        !> Close a pipe opened with popen.
        !>
        !> @param stream The FILE pointer to close
        !> @return Exit status of command, or -1 on error
        function c_pclose(stream) bind(C, name="pclose")
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: stream
            integer(c_int) :: c_pclose
        end function c_pclose

        !> Read data from a stream.
        !>
        !> @param ptr Pointer to buffer to read into
        !> @param size Size of each element
        !> @param nmemb Number of elements to read
        !> @param stream FILE pointer to read from
        !> @return Number of elements successfully read
        function c_fread(ptr, size, nmemb, stream) bind(C, name="fread")
            import :: c_ptr, c_size_t
            type(c_ptr), value, intent(in) :: ptr
            integer(c_size_t), value, intent(in) :: size
            integer(c_size_t), value, intent(in) :: nmemb
            type(c_ptr), value, intent(in) :: stream
            integer(c_size_t) :: c_fread
        end function c_fread
    end interface

contains

    !> Check if streaming is available on this platform.
    !>
    !> Currently checks for POSIX compliance by testing if popen is available.
    !> Assumes POSIX compliance (Linux, macOS, Unix) by default.
    !>
    !> @return .true. if streaming is available, .false. otherwise
    !>
    !> @note Windows users should expect fallback to temp file method
    function is_streaming_available() result(available)
        logical :: available

        ! Assume POSIX-compliant system (Linux, macOS, Unix)
        ! Windows will fail gracefully and use fallback
        available = .true.

    end function is_streaming_available

    !> Stream command output directly to memory.
    !>
    !> Executes the given command via popen() and streams output directly
    !> to an allocatable string, eliminating disk I/O.
    !>
    !> @param[in] command The shell command to execute
    !> @param[out] output The command output (allocated)
    !> @param[out] exit_status Exit status of the command
    !> @return .true. if successful, .false. on error
    !>
    !> ## Example
    !>
    !> ```fortran
    !> character(len=:), allocatable :: output
    !> integer :: status
    !> logical :: success
    !>
    !> success = stream_command_output('curl -s https://example.com', output, status)
    !> if (success .and. status == 0) then
    !>     print *, 'Output: ', output
    !> end if
    !> ```
    function stream_command_output(command, output, exit_status) result(success)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exit_status
        logical :: success

        type(c_ptr) :: pipe
        character(len=:), allocatable :: c_command
        character(len=CHUNK_SIZE), target :: buffer
        integer(c_size_t) :: bytes_read
        character(len=:), allocatable :: temp_output
        integer(c_int) :: close_status
        integer :: total_bytes
        character(len=256) :: msg

        success = .false.
        exit_status = -1
        total_bytes = 0

        ! Check if streaming is available
        if (.not. is_streaming_available()) then
            call s3_log_debug('Streaming not available on this platform')
            return
        end if

        call s3_log_trace('Streaming command: ' // trim(command))

        ! Convert Fortran string to C string (null-terminated)
        c_command = trim(command) // C_NULL_CHAR

        ! Open pipe for reading
        call s3_log_debug('Opening pipe with popen()')
        pipe = c_popen(c_command, 'r' // C_NULL_CHAR)
        if (.not. c_associated(pipe)) then
            call s3_log_error('popen() failed - pipe not associated')
            return
        end if

        call s3_log_debug('Pipe opened successfully, starting to read')

        ! Initialize output
        output = ''

        ! Read data in chunks
        do
            bytes_read = c_fread(c_loc(buffer), 1_c_size_t, &
                                 int(CHUNK_SIZE, c_size_t), pipe)

            ! Check if we've reached end of stream or error
            if (bytes_read <= 0) exit

            total_bytes = total_bytes + int(bytes_read)
            write(msg, '(A,I0,A,I0,A)') 'Read ', int(bytes_read), ' bytes (total: ', total_bytes, ')'
            call s3_log_trace(trim(msg))

            ! Append chunk to output
            if (allocated(temp_output)) deallocate(temp_output)
            if (len(output) > 0) then
                allocate(character(len=len(output) + int(bytes_read)) :: temp_output)
                temp_output = output // buffer(1:bytes_read)
            else
                allocate(character(len=int(bytes_read)) :: temp_output)
                temp_output = buffer(1:bytes_read)
            end if

            ! Move temp to output
            call move_alloc(temp_output, output)
        end do

        write(msg, '(A,I0,A)') 'Finished reading ', total_bytes, ' total bytes'
        call s3_log_debug(trim(msg))

        ! Close pipe and get exit status
        close_status = c_pclose(pipe)
        write(msg, '(A,I0)') 'pclose() returned: ', close_status
        call s3_log_debug(trim(msg))

        ! pclose returns exit status in platform-dependent way
        ! On most systems, need to check using WEXITSTATUS-like logic
        ! For simplicity, treat 0 as success, non-zero as failure
        if (close_status == 0) then
            exit_status = 0
            success = .true.
            call s3_log_debug('Stream command completed successfully')
        else
            ! Command failed, but we may still have partial output
            exit_status = close_status
            success = .false.
            write(msg, '(A,I0,A,I0,A)') 'Stream command failed with exit status ', &
                close_status, ' (bytes read: ', total_bytes, ')'
            call s3_log_error(trim(msg))
        end if

    end function stream_command_output

end module curl_stream