program s3_netcdf_example
    use s3_http
    use netcdf
    implicit none

    type(s3_config) :: config
    character(len=:), allocatable :: content
    logical :: success
    integer :: ncid, varid, status
    character(len=*), parameter :: temp_file = "/tmp/esgf_climate_data.nc"
    character(len=*), parameter :: climate_uri = &
        "s3://esgf-world/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/fx/areacella/gn/v20200212/areacella_fx_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn.nc"

    ! File metadata variables
    character(len=100) :: title, source_name
    integer :: file_size

    print *, 'S3 NetCDF Reading Example'
    print *, '========================================'
    print *, ''

    ! Configure S3 access for public bucket (no authentication needed)
    config%endpoint = 's3.amazonaws.com'
    config%region = 'us-east-1'
    config%use_https = .true.
    config%access_key = ''  ! Public bucket
    config%secret_key = ''
    call s3_init(config)

    print *, 'Reading NetCDF file from S3: ', climate_uri
    print *, ''

    ! Download the NetCDF file from S3
    success = s3_get_uri(climate_uri, content)
    if (.not. success) then
        print *, 'Error reading dataset: Failed to download from S3'
        stop 1
    end if

    print *, 'Dataset successfully opened!'
    print *, ''

    ! Write content to temporary file for NetCDF processing
    open(unit=10, file=temp_file, form='unformatted', access='stream', status='replace')
    write(10) content
    close(10)
    status = nf90_open(temp_file, NF90_NOWRITE, ncid)
    if (status /= NF90_NOERR) then
        print *, 'Error: Failed to open NetCDF file: ', nf90_strerror(status)
        stop 1
    end if

    print *, 'Dataset representation:'
    call display_netcdf_info(ncid)

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

    subroutine display_netcdf_info(ncid)
        integer, intent(in) :: ncid
        integer :: ndims, nvars, ngatts, unlimdimid, status
        integer :: i, varid, var_type, var_ndims
        character(len=NF90_MAX_NAME) :: var_name, dim_name
        integer, dimension(NF90_MAX_VAR_DIMS) :: dimids
        integer :: dim_len
        character(len=256) :: att_value

        ! Get file info
        status = nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid)
        if (status /= NF90_NOERR) return

        print *, 'NetCDF Dataset Information:'
        print *, '---------------------------'
        print *, 'Dimensions:'

        ! Show dimensions
        do i = 1, ndims
            status = nf90_inquire_dimension(ncid, i, dim_name, dim_len)
            if (status == NF90_NOERR) then
                print *, '  ', trim(dim_name), ': ', dim_len
            end if
        end do

        print *, 'Data variables:'

        ! Show first few variables
        do i = 1, min(nvars, 5)
            varid = i
            status = nf90_inquire_variable(ncid, varid, var_name, var_type, var_ndims, dimids)
            if (status == NF90_NOERR) then
                print *, '  ', trim(var_name), ' (dims: ', var_ndims, ')'
            end if
        end do

        if (nvars > 5) then
            print *, '  ... and ', nvars - 5, ' more variables'
        end if

        ! Show some global attributes
        print *, 'Attributes:'
        status = nf90_get_att(ncid, NF90_GLOBAL, 'title', att_value)
        if (status == NF90_NOERR) then
            print *, '  title: ', trim(att_value)
        end if

        status = nf90_get_att(ncid, NF90_GLOBAL, 'source', att_value)
        if (status == NF90_NOERR) then
            print *, '  source: ', trim(att_value)
        end if

        status = nf90_get_att(ncid, NF90_GLOBAL, 'institution', att_value)
        if (status == NF90_NOERR) then
            print *, '  institution: ', trim(att_value)
        end if

    end subroutine display_netcdf_info

end program s3_netcdf_example