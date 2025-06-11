# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Notes
- v0.4.0 final release pending CI stabilization and testing feedback
- Report issues with the RC at: https://github.com/b-vitamins/mjolnir/issues
### Added
- Helper macros for tests to create temporary buffers and handle frame failures
### Fixed
- Byte compilation error when loading due to early use of `mjolnir-mode-map`
  and `mjolnir--define-key`
- Test suite no longer fails when terminal frames cannot be created

## [0.4.0-rc1] - 2025-06-11
### ⚠️ Release Candidate
This is a release candidate for v0.4.0. Please test thoroughly before using in production.
The API is stable, but implementation details may change based on testing feedback.

### Changed
- **BREAKING**: Complete architectural overhaul with modular design
  - Minimum Emacs version raised to 27.1 (was 25.1)
  - Complete rewrite while maintaining API compatibility
- Modular file structure:
  - `core/` - State management, core algorithms, window/buffer operations
  - `ui/` - Display system (echo area and overlays)
  - `features/` - Optional features (transient interface)
- Frame-local state management with smart caching
- Improved window angle calculation with better edge case handling

### Added
- **Hidden Realm**: Buffer summoning (M-N/M-P)
  - Summon buffers not visible in any window
  - LRU-based buffer ordering
  - Configurable timeout for position reset
- **Enhanced worthiness system**:
  - Mode-line indicators (⚡ for worthy, ⚔ for unworthy)
  - Per-buffer worthiness control
  - Minimum window height filtering
  - Custom predicates via `mjolnir-worthy-predicates` hook
- **Project support** (Emacs 28+):
  - Limit summoning to current project buffers
  - Toggle with `mjolnir-toggle-project-limit`
- **Display options**:
  - Echo area with smart buffer name truncation
  - Window overlay numbers for preview
  - Configurable display method (echo/overlays/both)
- **Transient interface** (optional):
  - Visual menu for all commands
  - Requires transient.el
- **Comprehensive test suite**:
  - 40+ unit tests
  - Stress tests for stability
  - Performance benchmarks
  - Memory leak detection
- **CI/CD pipeline**:
  - Multi-platform testing (Linux, macOS, Windows)
  - Multiple Emacs versions (27.1 through snapshot)
  - Strict compilation and linting
  - Coverage reporting

### Fixed
- Lexical binding throughout all files
- Proper frame-local state isolation
- Better error handling for edge cases
- Window deletion during rotation
- Buffer killing during operations

### Performance
- Smart caching system for window angles
- Frame-local buffer visibility tracking
- Lazy evaluation and cache invalidation
- Reduced memory allocation through state reuse
- Sub-linear scaling with window count

## [0.3.0] - 2025-06-11
### Changed
- **BREAKING**: Renamed package from `mjolnir-mode` to `mjolnir`
  - Users must now `(require 'mjolnir)` instead of `(require 'mjolnir-mode)`
  - The mode itself remains `mjolnir-mode` (following Emacs conventions)
- Added lexical binding to main file
- Added proper package metadata (Package-Requires, etc.)
- Fixed parentheses error in `mjolnir-toggle-fixed-window`

## [0.2.0] - 2024-03-17
### Changed
- Use `set-window-dedicated-p` in `mjolnir-toggle-fixed-window` for better window management

### Added
- Improved README with clearer usage examples

## [0.1.0] - 2024-03-16
### Added
- Initial release
- Core functionality for buffer cycling through windows
- Window angle-based sorting algorithm
- Fixed/unworthy window support
- Customizable key bindings
- Cache system for performance
- Global minor mode

[Unreleased]: https://github.com/b-vitamins/mjolnir/compare/v0.4.0-rc1...HEAD
[0.4.0-rc1]: https://github.com/b-vitamins/mjolnir/compare/v0.3.0...v0.4.0-rc1
[0.3.0]: https://github.com/b-vitamins/mjolnir/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/b-vitamins/mjolnir/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/b-vitamins/mjolnir/releases/tag/v0.1.0
