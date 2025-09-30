program s3_netcdf_example
    use s3_http
    use s3_netcdf
    use netcdf
    implicit none

    type(s3_config) :: config
    integer :: ncid, status
    character(len=*), parameter :: climate_uri = &
        "s3://esgf-world/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/fx/areacella/gn/" // &
        "v20200212/areacella_fx_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn.nc"


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
    print *, 'Performance optimization:'
    print *, '  Using: ', trim(get_optimal_temp_dir())
    if (index(get_optimal_temp_dir(), '/dev/shm') > 0) then
        print *, '  Mode: RAM disk (zero disk I/O!)'
    else
        print *, '  Mode: Standard temp directory'
    end if
    print *, ''

    ! Open NetCDF file directly from S3 (transparent streaming!)
    status = s3_nf90_open(climate_uri, NF90_NOWRITE, ncid)
    if (status /= NF90_NOERR) then
        print *, 'Error: Failed to open NetCDF file: ', nf90_strerror(status)
        stop 1
    end if

    print *, 'Dataset successfully opened!'
    print *, ''

    print *, 'Dataset representation:'
    call display_netcdf_info(ncid)

    ! Close NetCDF file and auto-cleanup temp file
    status = s3_nf90_close(ncid)
    if (status /= NF90_NOERR) then
        print *, 'Warning: Failed to close NetCDF file properly'
    end if

    print *, ''
    print *, 'Example completed successfully!'
    print *, ''
    print *, 'This demonstrates:'
    print *, '  1. Direct NetCDF opening from S3 using s3_nf90_open()'
    print *, '  2. Transparent streaming with automatic temp file management'
    print *, '  3. Platform-optimized storage (/dev/shm on Linux, /tmp elsewhere)'
    print *, '  4. Zero-copy S3 downloads (Network -> Memory via streaming)'
    print *, '  5. Automatic cleanup via s3_nf90_close()'

contains

    subroutine display_netcdf_info(ncid)
        integer, intent(in) :: ncid
        integer :: ndims, nvars, ngatts, unlimdimid, status
        integer :: i, varid, var_type, var_ndims
        character(len=NF90_MAX_NAME) :: var_name, dim_name
        integer, dimension(NF90_MAX_VAR_DIMS) :: dimids
        integer :: dim_len

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
        call safe_get_attribute(ncid, 'title')
        call safe_get_attribute(ncid, 'source')
        call safe_get_attribute(ncid, 'institution')

    end subroutine display_netcdf_info

    subroutine safe_get_attribute(ncid, attr_name)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: attr_name
        integer :: status, att_len
        character(len=:), allocatable :: att_value

        ! First get the attribute length
        status = nf90_inquire_attribute(ncid, NF90_GLOBAL, attr_name, len=att_len)
        if (status /= NF90_NOERR) return

        ! Allocate string of correct length
        allocate(character(len=att_len) :: att_value)

        ! Get the attribute value
        status = nf90_get_att(ncid, NF90_GLOBAL, attr_name, att_value)
        if (status == NF90_NOERR) then
            print *, '  ', trim(attr_name), ': ', trim(att_value)
        end if

    end subroutine safe_get_attribute

end program s3_netcdf_example