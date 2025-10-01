!> Minimal NetCDF + S3 integration example
!>
!> This demonstrates the basic pattern for using S3-downloaded data with NetCDF.
!>
!> Requirements:
!>   - NetCDF-Fortran library must be installed on your system
!>   - Compile with: -lnetcdff -lnetcdf flags
!>
!> This is a minimal example showing the integration pattern. For a full-featured
!> wrapper with automatic cleanup and transparent S3 URIs, see the separate
!> fortran-s3-netcdf package.
program netcdf_minimal_example
    use s3_http
    use netcdf
    use iso_fortran_env, only: int64
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    character(len=256) :: temp_file
    integer :: ncid, status, unit, ios
    integer :: ndims, nvars, ngatts, unlimdimid
    character(len=*), parameter :: s3_key = &
        'CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/fx/' // &
        'areacella/gn/v20200212/areacella_fx_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn.nc'
    logical :: use_shm

    print *, '==========================================='
    print *, 'Minimal S3 + NetCDF Integration Example'
    print *, '==========================================='
    print *

    ! Step 1: Configure S3 for ESGF public bucket
    config%bucket = 'esgf-world'
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    config%access_key = ''  ! Public bucket
    config%secret_key = ''
    call s3_init(config)
    print *, 'Configured S3 for bucket: esgf-world'
    print *

    ! Step 2: Download NetCDF file from S3 to memory
    print *, 'Downloading AWI climate data from S3...'
    if (.not. s3_get_object(s3_key, content)) then
        print *, 'ERROR: Failed to download file from S3'
        stop 1
    end if
    print *, 'Downloaded', len(content), 'bytes to memory'
    print *

    ! Step 3: Choose optimal temp location (prefer RAM disk)
    call get_temp_file_path(temp_file, use_shm)
    if (use_shm) then
        print *, 'Using RAM disk: /dev/shm (zero disk I/O!)'
    else
        print *, 'Using temp directory:', trim(temp_file)
    end if
    print *

    ! Step 4: Write memory buffer to temp file
    print *, 'Writing to temporary file...'
    open(newunit=unit, file=trim(temp_file), form='unformatted', &
         access='stream', status='replace', iostat=ios)
    if (ios /= 0) then
        print *, 'ERROR: Failed to create temp file'
        stop 1
    end if
    write(unit, iostat=ios) content
    close(unit)
    if (ios /= 0) then
        print *, 'ERROR: Failed to write temp file'
        stop 1
    end if

    ! Step 5: Open with NetCDF library
    print *, 'Opening with NetCDF library...'
    status = nf90_open(trim(temp_file), NF90_NOWRITE, ncid)
    if (status /= NF90_NOERR) then
        print *, 'ERROR: NetCDF open failed:', trim(nf90_strerror(status))
        stop 1
    end if
    print *, 'Successfully opened NetCDF file!'
    print *

    ! Step 6: Query basic information
    status = nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid)
    if (status == NF90_NOERR) then
        print *, 'Dataset contains:'
        print *, '  Dimensions:', ndims
        print *, '  Variables:', nvars
        print *, '  Global attributes:', ngatts
    end if
    print *

    ! Step 7: Close NetCDF file
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) then
        print *, 'WARNING: NetCDF close failed'
    end if

    ! Step 8: Clean up temp file
    open(newunit=unit, file=trim(temp_file), status='old', iostat=ios)
    if (ios == 0) then
        close(unit, status='delete')
        print *, 'Cleaned up temporary file'
    end if

    print *
    print *, '==========================================='
    print *, 'Integration Pattern Summary:'
    print *, '==========================================='
    print *, '1. Download from S3 to memory (streaming)'
    print *, '2. Write to temp file (prefer /dev/shm)'
    print *, '3. Open with NetCDF library'
    print *, '4. Use NetCDF normally'
    print *, '5. Close and cleanup temp file'
    print *
    print *, 'For automatic cleanup and transparent S3'
    print *, 'URIs, see: fortran-s3-netcdf package'
    print *, '==========================================='

contains

    !> Determine optimal temp file path (prefer /dev/shm on Linux)
    subroutine get_temp_file_path(path, using_shm)
        character(len=*), intent(out) :: path
        logical, intent(out) :: using_shm
        logical :: exists
        integer :: pid

        ! Check if /dev/shm is available (Linux RAM disk)
        inquire(file='/dev/shm/.', exist=exists)

        ! Get process ID for unique filename
        call get_pid(pid)

        if (exists) then
            write(path, '(A,I0,A)') '/dev/shm/s3_netcdf_', pid, '.nc'
            using_shm = .true.
        else
            write(path, '(A,I0,A)') '/tmp/s3_netcdf_', pid, '.nc'
            using_shm = .false.
        end if
    end subroutine get_temp_file_path

    !> Get process ID for unique temp filename
    subroutine get_pid(pid)
        integer, intent(out) :: pid
        integer :: unit, ios
        character(len=32) :: pid_str

        ! Try to read from /proc/self (Linux)
        open(newunit=unit, file='/proc/self/stat', status='old', &
             action='read', iostat=ios)
        if (ios == 0) then
            read(unit, *, iostat=ios) pid
            close(unit)
            if (ios == 0) return
        end if

        ! Fallback: use a semi-random value based on current time
        call system_clock(pid)
        pid = modulo(pid, 99999) + 10000
    end subroutine get_pid

end program netcdf_minimal_example
