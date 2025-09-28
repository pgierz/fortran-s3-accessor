# Documentation

This directory contains all documentation for the fortran-s3-accessor project.

## Files

- `CLAUDE.md` - Development guidance for Claude Code (claude.ai/code)
- `CLAUDE_CONTEXT_DISASTER.md` - Context preservation documentation
- `ford.md` - FORD documentation generator configuration
- `generated/` - Auto-generated API documentation (created by FORD)

## Building Documentation

To generate the API documentation locally:

```bash
# Install FORD
pip install ford

# Generate documentation
ford docs/ford.md

# Generated docs will be in docs/generated/
```

The documentation is automatically built and deployed to GitHub Pages via CI when changes are pushed to the main branch.