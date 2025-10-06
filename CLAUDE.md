# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`fortran-s3-accessor` is a Fortran library that provides S3-compatible object storage access with a familiar Fortran I/O interface. It supports both local filesystem (mock) and HTTP-based S3 backends, allowing seamless switching between local and cloud storage.

## Environment Setup

### Using direnv (recommended)
```bash
# Enable direnv for automatic environment loading
direnv allow

# This loads spack and fpm automatically when entering the directory
# Configured in .envrc file with:
# - module load spack
# - spack load fpm
```

## Build Commands

### Using CMake (preferred)
```bash
# Configure and build
mkdir -p build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j

# Run tests
./test_s3_accessor  # When implemented
./test_minimal      # Minimal test
./test_file_ops     # File operations test
./s3_example        # Example program
```

### Using FPM (Fortran Package Manager)
```bash
# Build library and all executables
fpm build

# Run tests
fpm test

# Run specific executable
fpm run s3_example
```

## Architecture

The library follows a modular, layered architecture:

1. **Backend Layer** (`s3_backend_mod.f90`)
   - Abstract `s3_backend_t` interface defining S3 operations
   - `s3_mock_backend_t`: Local filesystem implementation
   - `s3_http_backend_t`: HTTP-based S3 implementation

2. **Interface Layer** (`s3_interface_mod.f90`)
   - Provides Fortran I/O-like interface (open/read/write/close)
   - Manages file handles and buffers
   - Maps standard I/O operations to backend calls

3. **File Operations Layer** (`s3_file_ops_mod.f90`)
   - Higher-level file operations (copy, move, delete)
   - Unified file type for both local and S3 files
   - Convenience wrappers around core interface

4. **Configuration** (`s3_config_mod.f90`)
   - `s3_config_t` type for backend configuration
   - Controls backend selection and S3 parameters

5. **Main Module** (`fortran-s3-accessor.f90`)
   - Re-exports all public interfaces
   - Single import point for library users

## Module Dependencies

The current simple implementation consists of:
1. `s3_http.f90` - Main S3 HTTP operations module

## Key Design Patterns

- **Direct HTTP Interface**: Simple curl-based S3 operations without abstraction layers
- **Configuration-Driven**: S3 parameters controlled via `s3_config` type
- **Stateful Design**: Initialize once with `s3_init()`, then perform operations

## Platform Support

**Primary target: Linux** (production ready with full feature support)

The library works on all platforms with automatic fallback:
- **Linux**: Uses libcurl direct binding (best performance, all features including progress callbacks)
- **macOS**: Falls back to popen() subprocess streaming (good performance, core operations work)
- **Windows**: Falls back to temp file method (acceptable performance, core operations work)

**Feature availability:**
- S3 operations (GET, PUT, DELETE, HEAD): ✅ All platforms
- Progress callbacks: ✅ Linux only (requires libcurl direct)
- Zero-copy streaming: ✅ Linux only (requires libcurl direct)

**Recommendation**: Use Linux for production deployments. macOS/Windows suitable for development and testing.

## Testing

The library includes comprehensive testing infrastructure:
- **test-drive framework**: 22+ test cases covering all operations
- **Mock testing system**: PATH-based curl mocking for reliable testing
- **Edge case coverage**: Authentication, network failures, malformed responses
- **Uninitialized state testing**: Separate test executable for proper isolation

## Common Development Tasks

### Running Tests

```bash
# Run tests with mock curl (required for S3 operation testing)
PATH="test/scripts:$PATH" fpm test
```

**CI Testing:**
The GitHub Actions CI tests on Linux (ubuntu-latest) with gcc 11, 12, 13

### Adding New Tests
1. Add test case to `test/test_s3_http.f90` using test-drive framework
2. Create any needed mock response files in `test/data/responses/`
3. Update mock curl script if new HTTP methods are needed

## Important Notes

- **Linux recommended for production** - full feature support with optimal performance
- Works on macOS/Windows with automatic subprocess fallbacks (core operations only)
- The library is designed for Fortran 2008 compatibility
- No external dependencies beyond standard Fortran and system curl
- **URL encoding in S3 keys is currently untested and unsupported** - use simple alphanumeric keys and underscores only
- **Progress callbacks require Linux** - uses libcurl direct binding which has ABI issues on macOS

## Local Testing on macOS

**IMPORTANT**: Local testing with `fpm build` or `fpm test` on macOS is problematic due to libcurl direct binding compatibility issues. The library uses C interoperability with libcurl which has ABI differences on macOS.

**Recommended workflow**:
1. **Do NOT test builds locally on macOS** - compilation may fail with linking errors or missing symbols
2. **Always rely on CI for validation** - push commits and let GitHub Actions (Linux) validate the build
3. **Focus on code correctness** - ensure syntax is correct, then validate via CI
4. **CI is the source of truth** - all tests run on Linux where libcurl bindings work correctly

**Why this happens**:
- macOS libcurl has different ABI/symbols than Linux
- Direct C bindings (`iso_c_binding`) are platform-specific
- The library is designed primarily for Linux HPC environments
- macOS local builds may show errors that don't occur in production (Linux)