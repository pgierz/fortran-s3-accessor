!> AWS Signature Version 4 authentication for S3
!>
!> This module implements AWS Signature Version 4 signing process for S3 requests.
!> Reference: https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html
module aws_auth
    use iso_fortran_env, only: int64
    use openssl_bindings
    use s3_logger
    implicit none
    private

    ! Public interface
    public :: aws_sign_request
    public :: aws_credential_t

    !> AWS credentials type
    type :: aws_credential_t
        character(len=:), allocatable :: access_key      !< AWS access key ID
        character(len=:), allocatable :: secret_key      !< AWS secret access key
        character(len=:), allocatable :: region          !< AWS region (e.g., "us-east-1")
        character(len=:), allocatable :: service         !< Service name (typically "s3")
    end type aws_credential_t

contains

    !> Generate AWS Signature v4 Authorization header value
    !>
    !> @param credentials [in] AWS credentials (access key, secret key, region, service)
    !> @param http_method [in] HTTP method (GET, PUT, DELETE, HEAD, etc.)
    !> @param host [in] S3 host (e.g., "s3.amazonaws.com" or "localhost:9000")
    !> @param uri [in] Request URI/path (e.g., "/bucket/key")
    !> @param query_string [in] URL query parameters (e.g., "prefix=test&delimiter=/"), empty string if none
    !> @param payload_hash [in] SHA256 hash of request payload (hex-encoded), use empty string hash for GET/DELETE
    !> @param timestamp [in] ISO8601 timestamp (e.g., "20230615T120000Z")
    !> @param authorization_header [out] Generated Authorization header value
    !> @return success [logical] True if signing succeeded, false otherwise
    function aws_sign_request(credentials, http_method, host, uri, query_string, &
                              payload_hash, timestamp, authorization_header) result(success)
        type(aws_credential_t), intent(in) :: credentials
        character(len=*), intent(in) :: http_method
        character(len=*), intent(in) :: host
        character(len=*), intent(in) :: uri
        character(len=*), intent(in) :: query_string
        character(len=*), intent(in) :: payload_hash
        character(len=*), intent(in) :: timestamp
        character(len=:), allocatable, intent(out) :: authorization_header
        logical :: success

        character(len=:), allocatable :: canonical_headers, signed_headers
        character(len=:), allocatable :: canonical_request
        character(len=64) :: canonical_request_hash  ! SHA256 hex output is 64 chars
        character(len=:), allocatable :: date_stamp, credential_scope
        character(len=:), allocatable :: string_to_sign
        character(len=:), allocatable :: signature
        character(len=8) :: date_only

        ! Extract date from timestamp (first 8 chars: YYYYMMDD)
        date_only = timestamp(1:8)
        date_stamp = trim(date_only)

        ! Build credential scope: date/region/service/aws4_request
        credential_scope = trim(date_stamp) // "/" // trim(credentials%region) // "/" // &
                          trim(credentials%service) // "/aws4_request"

        ! Build canonical headers (must be sorted alphabetically and lowercase)
        ! For S3, we need at minimum: host and x-amz-date
        canonical_headers = "host:" // trim(host) // new_line('a') // &
                           "x-amz-date:" // trim(timestamp) // new_line('a')
        signed_headers = "host;x-amz-date"

        ! Step 1: Create canonical request
        canonical_request = build_canonical_request(http_method, uri, query_string, &
                                                    canonical_headers, signed_headers, payload_hash)

        ! Hash the canonical request
        if (.not. sha256_hash(canonical_request, canonical_request_hash)) then
            call s3_log_error("aws_sign_request: Failed to hash canonical request")
            success = .false.
            return
        end if

        ! Step 2: Create string to sign
        string_to_sign = build_string_to_sign("AWS4-HMAC-SHA256", timestamp, &
                                             credential_scope, canonical_request_hash)

        ! Step 3: Calculate signature
        if (.not. calculate_signature(credentials%secret_key, date_stamp, &
                                     credentials%region, credentials%service, &
                                     string_to_sign, signature)) then
            call s3_log_error("aws_sign_request: Failed to calculate signature")
            success = .false.
            return
        end if

        ! Build Authorization header value
        authorization_header = "AWS4-HMAC-SHA256 Credential=" // trim(credentials%access_key) // &
                              "/" // trim(credential_scope) // ", SignedHeaders=" // &
                              trim(signed_headers) // ", Signature=" // trim(signature)

        success = .true.

    end function aws_sign_request

    !> Build canonical request string
    !>
    !> @param http_method [in] HTTP method
    !> @param uri [in] Request URI
    !> @param query_string [in] Query parameters
    !> @param canonical_headers [in] Canonical headers (sorted, formatted)
    !> @param signed_headers [in] Semicolon-separated list of signed header names
    !> @param payload_hash [in] Hex-encoded SHA256 of payload
    !> @return canonical_request [allocatable string] Canonical request string
    function build_canonical_request(http_method, uri, query_string, &
                                    canonical_headers, signed_headers, payload_hash) result(canonical_request)
        character(len=*), intent(in) :: http_method
        character(len=*), intent(in) :: uri
        character(len=*), intent(in) :: query_string
        character(len=*), intent(in) :: canonical_headers
        character(len=*), intent(in) :: signed_headers
        character(len=*), intent(in) :: payload_hash
        character(len=:), allocatable :: canonical_request

        ! Canonical request format (each field separated by newline):
        ! HTTPMethod + '\n' +
        ! CanonicalURI + '\n' +
        ! CanonicalQueryString + '\n' +
        ! CanonicalHeaders + '\n' +
        ! SignedHeaders + '\n' +
        ! HashedPayload

        canonical_request = trim(http_method) // new_line('a') // &
                           trim(uri) // new_line('a') // &
                           trim(query_string) // new_line('a') // &
                           trim(canonical_headers) // new_line('a') // &
                           trim(signed_headers) // new_line('a') // &
                           trim(payload_hash)

    end function build_canonical_request

    !> Build string to sign
    !>
    !> @param algorithm [in] Signing algorithm (e.g., "AWS4-HMAC-SHA256")
    !> @param timestamp [in] ISO8601 timestamp
    !> @param credential_scope [in] Scope string (date/region/service/aws4_request)
    !> @param canonical_request_hash [in] Hex-encoded SHA256 of canonical request
    !> @return string_to_sign [allocatable string] String to sign
    function build_string_to_sign(algorithm, timestamp, credential_scope, &
                                 canonical_request_hash) result(string_to_sign)
        character(len=*), intent(in) :: algorithm
        character(len=*), intent(in) :: timestamp
        character(len=*), intent(in) :: credential_scope
        character(len=*), intent(in) :: canonical_request_hash
        character(len=:), allocatable :: string_to_sign

        ! String to sign format:
        ! Algorithm + '\n' +
        ! RequestDateTime + '\n' +
        ! CredentialScope + '\n' +
        ! HashedCanonicalRequest

        string_to_sign = trim(algorithm) // new_line('a') // &
                        trim(timestamp) // new_line('a') // &
                        trim(credential_scope) // new_line('a') // &
                        trim(canonical_request_hash)

    end function build_string_to_sign

    !> Calculate AWS Signature v4 signature using HMAC-SHA256 chain
    !>
    !> @param secret_key [in] AWS secret access key
    !> @param date_stamp [in] Date in YYYYMMDD format
    !> @param region [in] AWS region
    !> @param service [in] Service name
    !> @param string_to_sign [in] String to sign
    !> @param signature [out] Hex-encoded signature
    !> @return success [logical] True if signature calculation succeeded, false otherwise
    function calculate_signature(secret_key, date_stamp, region, service, &
                                string_to_sign, signature) result(success)
        character(len=*), intent(in) :: secret_key
        character(len=*), intent(in) :: date_stamp
        character(len=*), intent(in) :: region
        character(len=*), intent(in) :: service
        character(len=*), intent(in) :: string_to_sign
        character(len=:), allocatable, intent(out) :: signature
        logical :: success

        character(len=:), allocatable :: k_secret
        character(len=32) :: k_date, k_region, k_service, k_signing
        character(len=32) :: signature_bytes
        character(len=64) :: signature_hex

        ! AWS Signature v4 HMAC-SHA256 chain:
        ! kSecret = "AWS4" + secret_key
        ! kDate = HMAC-SHA256(kSecret, date_stamp)
        ! kRegion = HMAC-SHA256(kDate, region)
        ! kService = HMAC-SHA256(kRegion, service)
        ! kSigning = HMAC-SHA256(kService, "aws4_request")
        ! signature = hex(HMAC-SHA256(kSigning, string_to_sign))

        ! Step 1: Create kSecret
        k_secret = "AWS4" // trim(secret_key)

        ! Step 2: kDate = HMAC(kSecret, date_stamp)
        if (.not. hmac_sha256(k_secret, date_stamp, k_date)) then
            call s3_log_error("calculate_signature: Failed to compute kDate")
            success = .false.
            return
        end if

        ! Step 3: kRegion = HMAC(kDate, region)
        if (.not. hmac_sha256(k_date, region, k_region)) then
            call s3_log_error("calculate_signature: Failed to compute kRegion")
            success = .false.
            return
        end if

        ! Step 4: kService = HMAC(kRegion, service)
        if (.not. hmac_sha256(k_region, service, k_service)) then
            call s3_log_error("calculate_signature: Failed to compute kService")
            success = .false.
            return
        end if

        ! Step 5: kSigning = HMAC(kService, "aws4_request")
        if (.not. hmac_sha256(k_service, "aws4_request", k_signing)) then
            call s3_log_error("calculate_signature: Failed to compute kSigning")
            success = .false.
            return
        end if

        ! Step 6: signature = hex(HMAC(kSigning, string_to_sign))
        if (.not. hmac_sha256(k_signing, string_to_sign, signature_bytes)) then
            call s3_log_error("calculate_signature: Failed to compute signature")
            success = .false.
            return
        end if

        ! Convert to hex
        call hex_encode(signature_bytes, signature_hex)
        signature = trim(signature_hex)

        success = .true.

    end function calculate_signature

end module aws_auth
