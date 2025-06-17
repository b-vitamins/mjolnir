# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Mjolnir is an Emacs minor mode for advanced window and buffer management. It allows users to rotate buffers through visible windows and summon hidden buffers without changing window focus.

## Build and Development Commands

```bash
# Compile all Emacs Lisp files
make compile

# Run the test suite
make test

# Lint the package (requires package-lint)
make lint-package

# Clean compiled files
make clean

# Run pre-commit hooks
pre-commit run --all-files

# Install development environment (run as root)
./setup-dev.sh
```

## Architecture

The codebase follows a modular architecture:

- **mjolnir.el**: Main entry point, mode definition, and keybindings
- **core/**: Core algorithms and state management
  - `mjolnir-core.el`: Buffer rotation and summoning algorithms
  - `mjolnir-state.el`: Frame-local state management with caching
  - `mjolnir-windows.el`: Window selection and manipulation
  - `mjolnir-buffers.el`: Buffer filtering and selection logic
- **ui/**: User interface components
  - `mjolnir-echo.el`: Display system (echo area and overlays)
- **features/**: Optional features
  - `mjolnir-transient.el`: Transient menu interface (requires transient.el)
- **test/**: ERT-based test suite

Key architectural decisions:
- Frame-local state with smart caching for performance
- Separation of core logic from UI concerns
- Optional dependencies (transient) loaded conditionally
- Worthiness system allows marking windows/buffers to exclude from rotation

## Development Guidelines

From AGENTS.md:
- Use Conventional Commits format: `<type>(<scope>): <description>`
- Types: `feat`, `fix`, `chore`, `docs`, `refactor`, `test`
- Maintain CHANGELOG.md with unreleased changes
- Emacs 27.1 minimum (previously was 25.1)
- Run pre-commit hooks before committing
- Keep functions small with lexical binding
- Document public APIs with docstrings

## Testing

Tests use Emacs' built-in ERT framework. The test runner is configured in the Makefile and can be run with `make test`. Tests are located in `test/mjolnir-test.el`.

## CI/CD

GitHub Actions CI tests against multiple Emacs versions:
- 27.1 (minimum supported)
- 28.2
- 29.4
- snapshot (latest development)

The CI pipeline runs compile, test, and lint-package targets.