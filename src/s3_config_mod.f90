!> Configuration module for S3 accessor
!> @author Paul Gierz
!>
!> This module handles runtime configuration through Fortran namelists,
!> allowing users to switch between S3 and local filesystem access
module s3_config_mod
    implicit none
    private

    !> Configuration type holding all S3 access parameters
    type, public :: s3_config_t
        logical :: use_s3 = .false.          !< Flag to enable S3 storage
        character(len=64) :: backend_type = 'mock' !< Backend implementation to use
        character(len=256) :: endpoint = ''  !< S3 endpoint URL
        character(len=256) :: bucket = ''    !< S3 bucket name
        character(len=256) :: prefix = ''    !< S3 object prefix
        character(len=64) :: region = 'us-east-1'  !< AWS region
        character(len=256) :: access_key = '' !< Access key (from env if empty)
        character(len=256) :: secret_key = '' !< Secret key (from env if empty)
        logical :: use_ssl = .true.          !< Use SSL/TLS for connections
        logical :: verify_ssl = .true.       !< Verify SSL certificates
        integer :: debug_level = 0           !< Debug verbosity (0=off)
        character(len=256) :: cache_dir = '/tmp/s3_cache' !< Local cache directory
        logical :: use_cache = .false.       !< Enable local caching
        integer :: cache_ttl = 3600          !< Cache time-to-live in seconds
    contains
        procedure, public :: read_namelist => config_read_namelist
        procedure, public :: write_namelist => config_write_namelist
        procedure, public :: print_config => config_print
        procedure, public :: validate => config_validate
    end type s3_config_t

    !> Global configuration instance
    type(s3_config_t), public, save :: s3_config

    !> Module variables for namelist
    logical :: use_s3
    character(len=64) :: backend_type
    character(len=256) :: endpoint, bucket, prefix, access_key, secret_key, cache_dir
    character(len=64) :: region
    logical :: use_ssl, verify_ssl, use_cache
    integer :: debug_level, cache_ttl

    !> Namelist for S3 configuration
    namelist /s3_config_nml/ use_s3, backend_type, endpoint, bucket, prefix, region, &
                             access_key, secret_key, use_ssl, verify_ssl, &
                             debug_level, cache_dir, use_cache, cache_ttl

contains

    !> Read configuration from namelist file
    !> @param filename Path to namelist file
    !> @param iostat Optional I/O status
    subroutine config_read_namelist(this, filename, iostat)
        class(s3_config_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: iostat

        integer :: unit, ios
        logical :: file_exists

        ! Initialize with defaults
        use_s3 = this%use_s3
        backend_type = this%backend_type
        endpoint = this%endpoint
        bucket = this%bucket
        prefix = this%prefix
        region = this%region
        access_key = this%access_key
        secret_key = this%secret_key
        use_ssl = this%use_ssl
        verify_ssl = this%verify_ssl
        debug_level = this%debug_level
        cache_dir = this%cache_dir
        use_cache = this%use_cache
        cache_ttl = this%cache_ttl

        ! Check if file exists
        inquire(file=trim(filename), exist=file_exists)
        if (.not. file_exists) then
            if (present(iostat)) iostat = -1
            return
        end if

        ! Open and read namelist
        open(newunit=unit, file=trim(filename), status='old', iostat=ios)
        if (ios /= 0) then
            if (present(iostat)) iostat = ios
            return
        end if

        read(unit, nml=s3_config_nml, iostat=ios)
        close(unit)

        if (ios /= 0) then
            if (present(iostat)) iostat = ios
            return
        end if

        ! Copy to structure
        this%use_s3 = use_s3
        this%backend_type = backend_type
        this%endpoint = endpoint
        this%bucket = bucket
        this%prefix = prefix
        this%region = region
        this%access_key = access_key
        this%secret_key = secret_key
        this%use_ssl = use_ssl
        this%verify_ssl = verify_ssl
        this%debug_level = debug_level
        this%cache_dir = cache_dir
        this%use_cache = use_cache
        this%cache_ttl = cache_ttl

        ! Get credentials from environment if not provided
        call this%load_env_credentials()

        if (present(iostat)) iostat = 0

    end subroutine config_read_namelist

    !> Load credentials from environment variables if not set
    subroutine load_env_credentials(this)
        class(s3_config_t), intent(inout) :: this
        character(len=256) :: env_value
        integer :: length, status

        ! Check AWS_ACCESS_KEY_ID
        if (len_trim(this%access_key) == 0) then
            call get_environment_variable('AWS_ACCESS_KEY_ID', env_value, length, status)
            if (status == 0) then
                this%access_key = trim(env_value)
                if (this%debug_level > 0) then
                    print *, 'Loaded AWS_ACCESS_KEY_ID from environment'
                end if
            end if
        end if

        ! Check AWS_SECRET_ACCESS_KEY
        if (len_trim(this%secret_key) == 0) then
            call get_environment_variable('AWS_SECRET_ACCESS_KEY', env_value, length, status)
            if (status == 0) then
                this%secret_key = trim(env_value)
                if (this%debug_level > 0) then
                    print *, 'Loaded AWS_SECRET_ACCESS_KEY from environment'
                end if
            end if
        end if

        ! Check AWS_DEFAULT_REGION if region not set
        if (trim(this%region) == 'us-east-1' .or. len_trim(this%region) == 0) then
            call get_environment_variable('AWS_DEFAULT_REGION', env_value, length, status)
            if (status == 0) then
                this%region = trim(env_value)
                if (this%debug_level > 0) then
                    print *, 'Loaded AWS_DEFAULT_REGION from environment: ', trim(this%region)
                end if
            end if
        end if

        ! Check S3_ENDPOINT_URL for custom endpoints (MinIO, etc)
        if (len_trim(this%endpoint) == 0) then
            call get_environment_variable('S3_ENDPOINT_URL', env_value, length, status)
            if (status == 0) then
                this%endpoint = trim(env_value)
                if (this%debug_level > 0) then
                    print *, 'Loaded S3_ENDPOINT_URL from environment: ', trim(this%endpoint)
                end if
            end if
        end if

    end subroutine load_env_credentials

    !> Write configuration to namelist file
    !> @param filename Path to output file
    !> @param iostat Optional I/O status
    subroutine config_write_namelist(this, filename, iostat)
        class(s3_config_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: iostat

        integer :: unit, ios

        ! Copy to module variables
        use_s3 = this%use_s3
        backend_type = this%backend_type
        endpoint = this%endpoint
        bucket = this%bucket
        prefix = this%prefix
        region = this%region
        access_key = this%access_key
        secret_key = this%secret_key
        use_ssl = this%use_ssl
        verify_ssl = this%verify_ssl
        debug_level = this%debug_level
        cache_dir = this%cache_dir
        use_cache = this%use_cache
        cache_ttl = this%cache_ttl

        ! Write namelist
        open(newunit=unit, file=trim(filename), status='replace', iostat=ios)
        if (ios /= 0) then
            if (present(iostat)) iostat = ios
            return
        end if

        write(unit, nml=s3_config_nml)
        close(unit)

        if (present(iostat)) iostat = 0

    end subroutine config_write_namelist

    !> Print configuration to stdout
    subroutine config_print(this)
        class(s3_config_t), intent(in) :: this

        print '(A)', '=== S3 Configuration ==='
        print '(A,L1)', 'Use S3: ', this%use_s3
        print '(A,A)', 'Backend Type: ', trim(this%backend_type)
        if (this%use_s3) then
            print '(A,A)', 'Endpoint: ', trim(this%endpoint)
            print '(A,A)', 'Bucket: ', trim(this%bucket)
            print '(A,A)', 'Prefix: ', trim(this%prefix)
            print '(A,A)', 'Region: ', trim(this%region)
            print '(A,L1)', 'Use SSL: ', this%use_ssl
            print '(A,L1)', 'Verify SSL: ', this%verify_ssl
            print '(A,L1)', 'Use Cache: ', this%use_cache
            if (this%use_cache) then
                print '(A,A)', 'Cache Directory: ', trim(this%cache_dir)
                print '(A,I0)', 'Cache TTL (seconds): ', this%cache_ttl
            end if
            print '(A,I0)', 'Debug Level: ', this%debug_level
        end if
        print '(A)', '======================='

    end subroutine config_print

    !> Validate configuration
    !> @return True if configuration is valid
    function config_validate(this) result(is_valid)
        class(s3_config_t), intent(in) :: this
        logical :: is_valid

        is_valid = .true.

        if (this%use_s3) then
            ! Check required fields for S3
            if (len_trim(this%bucket) == 0) then
                print *, 'ERROR: S3 bucket name is required when use_s3=.true.'
                is_valid = .false.
            end if

            ! Validate cache settings
            if (this%use_cache) then
                if (this%cache_ttl <= 0) then
                    print *, 'ERROR: Cache TTL must be positive'
                    is_valid = .false.
                end if
            end if
        end if

    end function config_validate

end module s3_config_mod