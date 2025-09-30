!> Logging module for S3 accessor library.
!>
!> Provides configurable logging with multiple severity levels to help
!> diagnose issues and understand library behavior.
!>
!> ## Log Levels
!>
!> - **NONE (0)**: No logging output
!> - **ERROR (1)**: Critical errors only
!> - **WARN (2)**: Warnings and errors
!> - **INFO (3)**: General information, warnings, and errors
!> - **DEBUG (4)**: Detailed debugging information
!> - **TRACE (5)**: Very detailed tracing (includes curl commands, responses)
!>
!> ## Usage
!>
!> ```fortran
!> use s3_logger
!>
!> ! Set log level (default is ERROR)
!> call s3_set_log_level(S3_LOG_DEBUG)
!>
!> ! Log messages at different levels
!> call s3_log_error('Failed to download object')
!> call s3_log_warn('Retrying connection')
!> call s3_log_info('Initialized S3 connection')
!> call s3_log_debug('Built URL: https://...')
!> call s3_log_trace('curl command: curl -s ...')
!> ```
!>
!> ## Environment Variable
!>
!> Log level can be set via environment variable `S3_LOG_LEVEL`:
!> ```bash
!> export S3_LOG_LEVEL=DEBUG
!> ./my_program
!> ```
!>
!> Valid values: NONE, ERROR, WARN, INFO, DEBUG, TRACE (case-insensitive)
module s3_logger
    implicit none
    private

    ! Public procedures
    public :: s3_set_log_level
    public :: s3_get_log_level
    public :: s3_log_error
    public :: s3_log_warn
    public :: s3_log_info
    public :: s3_log_debug
    public :: s3_log_trace
    public :: s3_init_logger

    ! Log level constants (public)
    integer, parameter, public :: S3_LOG_NONE = 0
    integer, parameter, public :: S3_LOG_ERROR = 1
    integer, parameter, public :: S3_LOG_WARN = 2
    integer, parameter, public :: S3_LOG_INFO = 3
    integer, parameter, public :: S3_LOG_DEBUG = 4
    integer, parameter, public :: S3_LOG_TRACE = 5

    ! Current log level (default ERROR)
    integer, save :: current_log_level = S3_LOG_ERROR

contains

    !> Initialize logger from environment variable.
    !>
    !> Reads S3_LOG_LEVEL environment variable and sets log level accordingly.
    !> If not set or invalid, defaults to ERROR level.
    !>
    !> Called automatically by s3_init(), but can be called manually.
    subroutine s3_init_logger()
        character(len=256) :: env_value
        integer :: status

        call get_environment_variable('S3_LOG_LEVEL', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            call parse_log_level(trim(env_value), current_log_level)
        end if
    end subroutine s3_init_logger

    !> Set the current log level.
    !>
    !> @param[in] level Log level (S3_LOG_NONE to S3_LOG_TRACE)
    subroutine s3_set_log_level(level)
        integer, intent(in) :: level
        if (level >= S3_LOG_NONE .and. level <= S3_LOG_TRACE) then
            current_log_level = level
        end if
    end subroutine s3_set_log_level

    !> Get the current log level.
    !>
    !> @return Current log level
    function s3_get_log_level() result(level)
        integer :: level
        level = current_log_level
    end function s3_get_log_level

    !> Log an error message.
    !>
    !> @param[in] message Error message to log
    subroutine s3_log_error(message)
        character(len=*), intent(in) :: message
        if (current_log_level >= S3_LOG_ERROR) then
            write(*, '(A,A)') '[S3 ERROR] ', trim(message)
        end if
    end subroutine s3_log_error

    !> Log a warning message.
    !>
    !> @param[in] message Warning message to log
    subroutine s3_log_warn(message)
        character(len=*), intent(in) :: message
        if (current_log_level >= S3_LOG_WARN) then
            write(*, '(A,A)') '[S3 WARN]  ', trim(message)
        end if
    end subroutine s3_log_warn

    !> Log an informational message.
    !>
    !> @param[in] message Info message to log
    subroutine s3_log_info(message)
        character(len=*), intent(in) :: message
        if (current_log_level >= S3_LOG_INFO) then
            write(*, '(A,A)') '[S3 INFO]  ', trim(message)
        end if
    end subroutine s3_log_info

    !> Log a debug message.
    !>
    !> @param[in] message Debug message to log
    subroutine s3_log_debug(message)
        character(len=*), intent(in) :: message
        if (current_log_level >= S3_LOG_DEBUG) then
            write(*, '(A,A)') '[S3 DEBUG] ', trim(message)
        end if
    end subroutine s3_log_debug

    !> Log a trace message (very detailed).
    !>
    !> @param[in] message Trace message to log
    subroutine s3_log_trace(message)
        character(len=*), intent(in) :: message
        if (current_log_level >= S3_LOG_TRACE) then
            write(*, '(A,A)') '[S3 TRACE] ', trim(message)
        end if
    end subroutine s3_log_trace

    !> Parse log level from string.
    !>
    !> @param[in] level_str Log level as string (NONE, ERROR, WARN, INFO, DEBUG, TRACE)
    !> @param[out] level Parsed log level constant
    subroutine parse_log_level(level_str, level)
        character(len=*), intent(in) :: level_str
        integer, intent(out) :: level
        character(len=256) :: upper_str
        integer :: i

        ! Convert to uppercase
        upper_str = level_str
        do i = 1, len_trim(upper_str)
            if (upper_str(i:i) >= 'a' .and. upper_str(i:i) <= 'z') then
                upper_str(i:i) = char(ichar(upper_str(i:i)) - 32)
            end if
        end do

        select case (trim(upper_str))
        case ('NONE', '0')
            level = S3_LOG_NONE
        case ('ERROR', '1')
            level = S3_LOG_ERROR
        case ('WARN', 'WARNING', '2')
            level = S3_LOG_WARN
        case ('INFO', '3')
            level = S3_LOG_INFO
        case ('DEBUG', '4')
            level = S3_LOG_DEBUG
        case ('TRACE', '5')
            level = S3_LOG_TRACE
        case default
            level = S3_LOG_ERROR  ! Default to ERROR
            write(*, '(A,A,A)') '[S3 WARN] Unknown log level "', trim(level_str), &
                '", defaulting to ERROR'
        end select
    end subroutine parse_log_level

end module s3_logger