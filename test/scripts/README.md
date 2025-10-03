# Mock Curl Script for Testing

This directory contains a Python-based mock curl script for testing S3 operations without requiring a real S3 connection.

**Requirements:**
- Python 3.6+ (pre-installed on GitHub Actions runners)
- Uses only standard library (pathlib, shutil, sys)
- No external dependencies

**Usage:**
```bash
chmod +x test/scripts/curl
PATH="${PWD}/test/scripts:$PATH" fpm test
```

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
- `pathlib.Path` for path handling
- `shutil.copy` for file operations
- `sys.exit()` for exit codes matching real curl behavior

## CI Integration

The GitHub Actions CI workflow (Linux only):

```yaml
- name: Run tests with FPM
  run: |
    chmod +x test/scripts/curl
    PATH="${PWD}/test/scripts:$PATH" fpm test --verbose
```

See `.github/workflows/ci.yml` for implementation details.
