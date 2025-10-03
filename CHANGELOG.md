# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **Progress callback support** for HTTP downloads (Linux only)
  - New `curl_get_to_buffer_with_progress()` function with real-time progress reporting
  - `curl_progress_callback` abstract interface for user-defined callbacks
  - Demo application (`app/progress_demo.f90`) showing progress bar implementation
  - Useful for monitoring large climate data downloads

### Changed

- **Simplified to Linux-focused development**
  - Removed macOS and Windows from CI workflows (Linux gcc 11, 12, 13 only)
  - Clarified platform support: Linux for production, macOS/Windows work via subprocess fallbacks
  - Core S3 operations work on all platforms via automatic fallback chain
  - Progress callbacks and optimal performance require Linux (libcurl direct binding)
  - Rationale: Production deployments are Linux-only where full feature support matters

## [1.1.0] - 2025-10-01

### Added

- **Direct memory streaming** via POSIX `popen()` eliminates disk I/O overhead on Linux/macOS/Unix systems
  - New `curl_stream` module with C interoperability for high-performance streaming
  - Automatic platform detection with fallback to temp files on Windows
  - Zero-copy streaming reduces overhead from 10-30% to ~10ms for most operations
- **Comprehensive logging system** via new `s3_logger` module
  - Configurable log levels: NONE, ERROR, WARN, INFO, DEBUG, TRACE
  - Environment variable control: `S3_LOG_LEVEL`
  - Detailed operation logging for debugging and monitoring
- **NetCDF integration example** demonstrating climate data workflows
  - Minimal standalone example (`examples/netcdf_minimal.f90`)
  - Tested with ESGF climate data (AWI-ESM-1-1-LR)
  - Optional CI job for NetCDF integration testing
  - RAM disk optimization guide (`/dev/shm` on Linux)

### Changed

- Performance: Network → Memory (direct) instead of Network → /tmp → Memory on POSIX systems
- Documentation: Updated README with v1.1.0 performance characteristics
- Documentation: FORD docs now describe streaming architecture and logging

### Fixed

- Buffer overflow in `s3_get_object` logging for very long S3 keys (increased buffer to 2048 chars)
- Logger constant name collision with subroutines (renamed to `S3_LOG_LEVEL_*`)
- Line truncation errors in test applications with long ESGF S3 paths
- Module compilation order in CMake and CI workflows

### Documentation

- Added comprehensive logging documentation to FORD
- Updated README with performance tables for v1.1.0
- Added NetCDF integration section to README
- Created examples catalog page in FORD docs

### Infrastructure

- NetCDF integration CI job (optional, non-blocking)
- Fixed Code Quality workflow module compilation order
- Added s3_logger to CMake build system

## [1.0.1] - 2025-09-28

### Fixed

- Minor bug fixes and documentation improvements
- CI workflow enhancements

## [1.0.0] - 2025-09-25

### Added

- Initial release
- Direct HTTP-based S3 operations (GET, PUT, DELETE, HEAD)
- URI support (`s3://bucket/key` format)
- Dual interface: low-level `s3_http` and high-level `s3_io`
- Comprehensive test suite with mock-based testing (22+ test cases)
- FPM and CMake build system support
- FORD documentation
- Public bucket access without authentication
- Cross-platform compatibility (Linux, macOS, Windows)

[1.1.0]: https://github.com/pgierz/fortran-s3-accessor/compare/v1.0.1...v1.1.0
[1.0.1]: https://github.com/pgierz/fortran-s3-accessor/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/pgierz/fortran-s3-accessor/releases/tag/v1.0.0
