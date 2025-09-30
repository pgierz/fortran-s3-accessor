# Contributing to fortran-s3-accessor

Thank you for your interest in contributing to fortran-s3-accessor! This document provides guidelines and information for contributors.

## Code of Conduct

Be respectful and constructive in all interactions. We are committed to providing a welcoming and inclusive environment for all contributors.

## Getting Started

### Prerequisites

- Fortran compiler (gfortran 11+ recommended)
- FPM (Fortran Package Manager) or CMake
- Git
- curl (system command)

### Setting Up Development Environment

```bash
# Clone the repository
git clone https://github.com/pgierz/fortran-s3-accessor.git
cd fortran-s3-accessor

# Build with FPM
fpm build

# Run tests (uses mock curl)
chmod +x test/scripts/curl
PATH="${PWD}/test/scripts:$PATH" fpm test

# Or build with CMake
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug
make
```

## How to Contribute

### Reporting Bugs

Before creating bug reports, please check existing issues. When creating a bug report, include:

- Description of the issue
- Steps to reproduce
- Expected vs actual behavior
- Fortran compiler and version
- Operating system
- Relevant code snippets or error messages

### Suggesting Features

Feature requests are welcome! Please:

- Check if the feature has already been suggested
- Clearly describe the use case
- Explain how it would benefit users
- Consider implementation complexity

### Pull Requests

1. **Fork the repository** and create your branch from `master`

2. **Make your changes**:
   - Write clean, readable Fortran code
   - Follow existing code style
   - Add tests for new functionality
   - Update documentation as needed

3. **Test thoroughly**:
   ```bash
   # Run all tests
   PATH="${PWD}/test/scripts:$PATH" fpm test

   # Test with CMake
   cd build && ctest

   # Check code quality
   gfortran -c -Wall -Wextra -Werror -std=f2008 src/*.f90
   ```

4. **Update documentation**:
   - Add FORD documentation comments for new procedures
   - Update README if needed
   - Add examples if appropriate

5. **Commit your changes**:
   ```bash
   git add .
   git commit -m "Brief description of changes"
   ```

   Use clear, descriptive commit messages. No need for GPG signing.

6. **Push to your fork** and submit a pull request

7. **Respond to review feedback**

## Development Guidelines

### Code Style

- Use Fortran 2008 standard
- Use lowercase for keywords and identifiers
- Indent with 4 spaces (no tabs)
- Keep lines under 100 characters when possible
- Use meaningful variable names
- Add comments for complex logic

Example:
```fortran
function s3_get_object(key, content) result(success)
    character(len=*), intent(in) :: key
    character(len=:), allocatable, intent(out) :: content
    logical :: success

    ! Implementation here
end function
```

### Documentation

Use FORD-style documentation comments:

```fortran
!> Brief description of the function.
!>
!> Detailed description if needed. Can span multiple lines.
!>
!> @param[in] key The S3 object key
!> @param[out] content The downloaded content
!> @return .true. if successful, .false. otherwise
!>
!> ## Example
!>
!> ```fortran
!> success = s3_get_object('data/file.txt', content)
!> ```
function s3_get_object(key, content) result(success)
```

### Testing

- Write tests for all new functionality
- Use the test-drive framework
- Add mock responses for S3 operations in `test/data/responses/`
- Test edge cases and error conditions
- Ensure tests pass before submitting PR

### Commit Messages

Write clear commit messages:

```
Brief summary (50 chars or less)

More detailed explanation if needed. Wrap at 72 characters.
Explain the problem this commit solves and why you chose this approach.

- Bullet points are okay
- Use present tense ("Add feature" not "Added feature")
```

## Priority Areas for Contributions

See the [v1.1.0 roadmap issue](https://github.com/pgierz/fortran-s3-accessor/issues/8) for priority improvements:

1. **Performance** - Direct memory streaming to eliminate disk I/O
2. **Error handling** - Better diagnostics and error messages
3. **Testing** - Real S3 integration tests
4. **Documentation** - More examples for scientific workflows
5. **Features** - AWS Signature v4 authentication, multipart uploads

## Questions?

- Open an issue for general questions
- Check existing documentation in `docs/` and README
- Review the [FORD documentation](https://pgierz.github.io/fortran-s3-accessor/)

## License

By contributing to fortran-s3-accessor, you agree that your contributions will be licensed under the MIT License.