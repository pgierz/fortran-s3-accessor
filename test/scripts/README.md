# Mock Curl Script for Testing

This directory contains a Python-based mock curl script for testing S3 operations without requiring a real S3 connection.

## Cross-Platform Support

**Scripts:**
- `curl` (Python script with shebang) - Used on Linux/macOS
- `curl.bat` (Windows batch wrapper) - Calls Python script on Windows

The mock curl is implemented in Python for maximum portability. All logic is in the Python script; the Windows `.bat` file is just a 2-line wrapper that calls it.

**Requirements:**
- Python 3.6+ (pre-installed on all GitHub Actions runners)
- Uses only standard library (pathlib, shutil, sys)
- No external dependencies

**Usage (all platforms):**
```bash
chmod +x test/scripts/curl  # Linux/macOS only
PATH="${PWD}/test/scripts:$PATH" fpm test
```

**How it works:**
- **Linux/macOS:** Shell finds `curl` (Python script), executes via shebang
- **Windows:** Shell finds `curl.bat` first (due to extension priority), which calls Python script

## How It Works

The mock curl scripts intercept curl commands and return pre-defined responses from `test/data/responses/`:

- **GET requests**: Return content from response files
- **HEAD requests**: Check if response files exist
- **PUT requests**: Mock success (no actual upload)
- **DELETE requests**: Mock success (with some failure cases for testing)

## Special Test Cases

The scripts handle special keys for testing error conditions:

| Key | Behavior | Exit Code |
|-----|----------|-----------|
| `network_failure_test.txt` | Simulate connection failure | 7 |
| `timeout_test.txt` | Simulate timeout | 28 |
| `malformed_response.txt` | Return malformed XML | 0 (but invalid data) |
| `delete_failure_test.txt` | Simulate DELETE failure | 22 |
| Any missing file | Return NoSuchKey XML error | 0 |

## Adding New Test Data

To add new mock responses:

1. Create a file in `test/data/responses/` with the object key name
2. Add the expected content
3. The mock curl will automatically serve it for GET requests

For HEAD requests, create a file named `head_<key>` with appropriate HTTP headers.

## Implementation Details

The Python script uses:
- `pathlib.Path` for cross-platform path handling
- `shutil.copy` for file operations
- `sys.exit()` for exit codes matching real curl behavior

This ensures identical behavior across all platforms without platform-specific code.

## CI Integration

The GitHub Actions CI workflow uses a single unified test command for all platforms:

```yaml
- name: Run tests with FPM
  shell: bash
  run: |
    chmod +x test/scripts/curl
    PATH="${PWD}/test/scripts:$PATH" fpm test --verbose
```

See `.github/workflows/ci.yml` for implementation details.
