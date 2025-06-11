# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/b-vitamins/mjolnir/compare/v0.3.0...HEAD
[0.3.0]: https://github.com/b-vitamins/mjolnir/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/b-vitamins/mjolnir/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/b-vitamins/mjolnir/releases/tag/v0.1.0
