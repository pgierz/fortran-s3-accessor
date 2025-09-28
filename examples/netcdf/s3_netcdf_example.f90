program s3_netcdf_example
    use s3_http
    use netcdf
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success
    integer :: ncid, varid, status
    character(len=*), parameter :: temp_file = "/tmp/goes16_data.nc"
    character(len=*), parameter :: goes16_uri = &
        "s3://noaa-goes16/ABI-L2-CMIPF/2024/001/00/OR_ABI-L2-CMIPF-M6C01_G16_s20240010001173_e20240010003546_c20240010004028.nc"

    ! File metadata variables
    character(len=100) :: title, source_name
    integer :: file_size

    print *, '================================================'
    print *, 'NOAA GOES-16 NetCDF via S3 Example'
    print *, '================================================'
    print *, ''

    ! Configure S3 access for public bucket (no authentication needed)
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    config%access_key = ''  ! Public bucket
    config%secret_key = ''
    call s3_init(config)

    print *, 'Downloading GOES-16 NetCDF file from S3...'
    print *, 'URI: ', goes16_uri
    print *, ''

    ! Download the NetCDF file from S3
    success = s3_get_uri(goes16_uri, content)
    if (.not. success) then
        print *, 'ERROR: Failed to download file from S3'
        print *, 'This might be due to:'
        print *, '  - Network connectivity issues'
        print *, '  - File not found (GOES-16 files are updated daily)'
        print *, '  - S3 access restrictions'
        stop 1
    end if

    print *, 'Successfully downloaded file, size: ', len(content), ' bytes'

    ! Write content to temporary file for NetCDF processing
    print *, 'Writing to temporary file: ', temp_file
    open(unit=10, file=temp_file, form='unformatted', access='stream', status='replace')
    write(10) content
    close(10)
    print *, 'Temporary file written successfully'
    print *, ''

    ! Open NetCDF file and read metadata
    print *, 'Opening NetCDF file...'
    status = nf90_open(temp_file, NF90_NOWRITE, ncid)
    if (status /= NF90_NOERR) then
        print *, 'ERROR: Failed to open NetCDF file: ', nf90_strerror(status)
        stop 1
    end if

    ! Read global attributes
    print *, 'Reading NetCDF metadata:'
    print *, '------------------------'

    ! Get title attribute
    status = nf90_get_att(ncid, NF90_GLOBAL, 'title', title)
    if (status == NF90_NOERR) then
        print *, 'Title: ', trim(title)
    end if

    ! Get source name
    status = nf90_get_att(ncid, NF90_GLOBAL, 'instrument_type', source_name)
    if (status == NF90_NOERR) then
        print *, 'Instrument: ', trim(source_name)
    end if

    ! List some variables
    call list_netcdf_variables(ncid)

    ! Close NetCDF file
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) then
        print *, 'Warning: Failed to close NetCDF file properly'
    end if

    ! Clean up temporary file
    open(unit=10, file=temp_file, status='old')
    close(10, status='delete')

    print *, ''
    print *, 'Example completed successfully!'
    print *, ''
    print *, 'This demonstrates:'
    print *, '  1. Downloading NetCDF files from S3 using s3:// URIs'
    print *, '  2. Processing NetCDF metadata with fortran-netcdf'
    print *, '  3. Integration with public scientific datasets'

contains

    subroutine list_netcdf_variables(ncid)
        integer, intent(in) :: ncid
        integer :: nvars, i, status, varid
        character(len=NF90_MAX_NAME) :: var_name
        integer :: var_type, ndims
        integer, dimension(NF90_MAX_VAR_DIMS) :: dimids

        ! Get number of variables
        status = nf90_inquire(ncid, nvariables=nvars)
        if (status /= NF90_NOERR) return

        print *, ''
        print *, 'Variables in this NetCDF file:'
        print *, '------------------------------'

        ! List first 5 variables to avoid too much output
        do i = 1, min(nvars, 5)
            varid = i
            status = nf90_inquire_variable(ncid, varid, var_name, var_type, ndims, dimids)
            if (status == NF90_NOERR) then
                print *, trim(var_name), ' (type: ', var_type, ', dims: ', ndims, ')'
            end if
        end do

        if (nvars > 5) then
            print *, '... and ', nvars - 5, ' more variables'
        end if
    end subroutine list_netcdf_variables

end program s3_netcdf_example