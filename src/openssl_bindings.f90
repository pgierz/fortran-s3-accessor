!> OpenSSL bindings for cryptographic operations.
!>
!> Provides ISO_C_BINDING interfaces to OpenSSL for SHA256 hashing and HMAC-SHA256
!> signing, required for AWS Signature Version 4 authentication.
!>
!> @note Requires OpenSSL library (libssl.so / libcrypto.so on Linux)
!> @note Linux only - runtime detection with graceful fallback
module openssl_bindings
    use iso_c_binding
    use s3_logger
    implicit none
    private

    ! Public API
    public :: sha256_hash
    public :: hmac_sha256
    public :: hex_encode
    public :: is_openssl_available

    ! Constants
    integer, parameter :: SHA256_DIGEST_LENGTH = 32  ! SHA256 produces 32 bytes

    ! Module state
    logical, save :: openssl_loaded = .false.
    logical, save :: openssl_checked = .false.

    ! C function interfaces
    interface
        !> OpenSSL EVP_Digest for SHA256
        function EVP_Digest(data, count, md, size, type, impl) &
            bind(C, name="EVP_Digest")
            import :: c_ptr, c_size_t, c_int
            type(c_ptr), value :: data
            integer(c_size_t), value :: count
            type(c_ptr), value :: md
            type(c_ptr), value :: size
            type(c_ptr), value :: type
            type(c_ptr), value :: impl
            integer(c_int) :: EVP_Digest
        end function EVP_Digest

        !> OpenSSL EVP_sha256 - returns SHA256 type
        function EVP_sha256() bind(C, name="EVP_sha256")
            import :: c_ptr
            type(c_ptr) :: EVP_sha256
        end function EVP_sha256

        !> OpenSSL HMAC for HMAC-SHA256
        function HMAC(evp_md, key, key_len, data, data_len, md, md_len) &
            bind(C, name="HMAC")
            import :: c_ptr, c_int, c_size_t
            type(c_ptr), value :: evp_md
            type(c_ptr), value :: key
            integer(c_int), value :: key_len
            type(c_ptr), value :: data
            integer(c_size_t), value :: data_len
            type(c_ptr), value :: md
            type(c_ptr), value :: md_len
            type(c_ptr) :: HMAC
        end function HMAC
    end interface

contains

    !> Check if OpenSSL is available on this system.
    !>
    !> Performs runtime detection by attempting to call EVP_sha256().
    !> Result is cached after first check.
    !>
    !> @return .true. if OpenSSL is available, .false. otherwise
    function is_openssl_available() result(available)
        logical :: available
        type(c_ptr) :: sha_ptr
        character(len=256) :: msg

        if (openssl_checked) then
            available = openssl_loaded
            return
        end if

        openssl_checked = .true.

        ! Try to get SHA256 type - if this works, OpenSSL is available
        sha_ptr = EVP_sha256()

        if (c_associated(sha_ptr)) then
            openssl_loaded = .true.
            call s3_log_info('OpenSSL is available - authentication enabled')
        else
            openssl_loaded = .false.
            call s3_log_warn('OpenSSL not available - authentication disabled')
        end if

        available = openssl_loaded

    end function is_openssl_available

    !> Compute SHA256 hash of data.
    !>
    !> @param data Input data to hash
    !> @param hash Output hash (32 bytes)
    !> @return .true. if successful, .false. on error
    function sha256_hash(data, hash) result(success)
        character(len=*), intent(in) :: data
        character(len=SHA256_DIGEST_LENGTH), intent(out) :: hash
        logical :: success

        type(c_ptr) :: sha_type
        character(kind=c_char), dimension(:), allocatable, target :: c_data
        character(kind=c_char), dimension(SHA256_DIGEST_LENGTH), target :: c_hash
        integer(c_size_t) :: data_len
        integer(c_int) :: result
        integer :: i

        success = .false.

        if (.not. is_openssl_available()) then
            call s3_log_error('OpenSSL not available for SHA256')
            return
        end if

        ! Convert Fortran string to C array
        data_len = len(data, kind=c_size_t)
        allocate(c_data(data_len))
        do i = 1, int(data_len)
            c_data(i) = data(i:i)
        end do

        ! Get SHA256 type
        sha_type = EVP_sha256()
        if (.not. c_associated(sha_type)) then
            call s3_log_error('Failed to get SHA256 type')
            deallocate(c_data)
            return
        end if

        ! Compute hash
        result = EVP_Digest(c_loc(c_data), data_len, c_loc(c_hash), &
                           c_null_ptr, sha_type, c_null_ptr)

        if (result == 1) then
            ! Copy result to output
            do i = 1, SHA256_DIGEST_LENGTH
                hash(i:i) = c_hash(i)
            end do
            success = .true.
        else
            call s3_log_error('SHA256 computation failed')
        end if

        deallocate(c_data)

    end function sha256_hash

    !> Compute HMAC-SHA256 of data with key.
    !>
    !> @param key HMAC key
    !> @param data Input data to sign
    !> @param hmac Output HMAC (32 bytes)
    !> @return .true. if successful, .false. on error
    function hmac_sha256(key, data, hmac) result(success)
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: data
        character(len=SHA256_DIGEST_LENGTH), intent(out) :: hmac
        logical :: success

        type(c_ptr) :: sha_type, result_ptr
        character(kind=c_char), dimension(:), allocatable, target :: c_key, c_data
        character(kind=c_char), dimension(SHA256_DIGEST_LENGTH), target :: c_hmac
        integer(c_int) :: key_len
        integer(c_size_t) :: data_len
        integer :: i

        success = .false.

        if (.not. is_openssl_available()) then
            call s3_log_error('OpenSSL not available for HMAC-SHA256')
            return
        end if

        ! Convert Fortran strings to C arrays
        key_len = len(key, kind=c_int)
        data_len = len(data, kind=c_size_t)

        allocate(c_key(key_len))
        allocate(c_data(data_len))

        do i = 1, key_len
            c_key(i) = key(i:i)
        end do
        do i = 1, int(data_len)
            c_data(i) = data(i:i)
        end do

        ! Get SHA256 type
        sha_type = EVP_sha256()
        if (.not. c_associated(sha_type)) then
            call s3_log_error('Failed to get SHA256 type')
            deallocate(c_key, c_data)
            return
        end if

        ! Compute HMAC
        result_ptr = HMAC(sha_type, c_loc(c_key), key_len, c_loc(c_data), &
                         data_len, c_loc(c_hmac), c_null_ptr)

        if (c_associated(result_ptr)) then
            ! Copy result to output
            do i = 1, SHA256_DIGEST_LENGTH
                hmac(i:i) = c_hmac(i)
            end do
            success = .true.
        else
            call s3_log_error('HMAC-SHA256 computation failed')
        end if

        deallocate(c_key, c_data)

    end function hmac_sha256

    !> Convert binary data to lowercase hexadecimal string.
    !>
    !> @param binary Binary data
    !> @param hex Output hex string (length must be 2*len(binary))
    subroutine hex_encode(binary, hex)
        character(len=*), intent(in) :: binary
        character(len=*), intent(out) :: hex

        integer :: i, byte_val, high, low
        character(len=16), parameter :: hex_chars = '0123456789abcdef'

        do i = 1, len(binary)
            byte_val = ichar(binary(i:i))
            high = byte_val / 16 + 1
            low = mod(byte_val, 16) + 1
            hex(2*i-1:2*i-1) = hex_chars(high:high)
            hex(2*i:2*i) = hex_chars(low:low)
        end do

    end subroutine hex_encode

end module openssl_bindings
