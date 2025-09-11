# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0] - 2025-12-09

### Added
- POSIX bc calculator implementation based on Eval 1.13
- Command-line argument parsing with `-h`, `-v`, and `-q` options
- POSIX-compliant `scale`, `ibase`, and `obase` commands
- Support for stdin/stdout file processing
- Interactive prompt with `>>>` display
- Euler's number constant `_e`
- Comprehensive manpage and documentation
- GPL v2 license

### Changed
- Converted from Eval calculator to POSIX bc standard
- Updated version numbering to reflect bc compatibility
- Modified operator precedence to match POSIX bc standard
- Improved range validation for base and scale commands

### Fixed
- Fixed range validation bug for `scale`, `ibase`, and `obase` commands
- Fixed alphabetical ordering of constants array for proper binary search
- Fixed build errors related to missing function declarations
- Corrected power operator precedence (more accurate than macOS bc)

### Technical Details
- ANSI C compliant code
- SAS C smake build system
- Based on Eval 1.13 by Will Menninger

---

## [1.13] - 1993-04-13

### Changed
- Modified command line argument interpretation so that eval could evaluate a single expression and quit

## [1.12] - 1993-04-09

### Added
- Default write directory and read path for scripts
- Ability to specify a certain page of help

## [1.11] - 1993-02-XX (Unreleased)

### Added
- Better script support

## [1.10] - 1993-01-XX

### Added
- More flexible output with user specified precision
- Automatic pausing every 22 lines
- Ability to specify input and output in any number base
- Rewritten math library using original coding (with help from Abramowitz and Stegun)
- Public domain release

### Technical Details
- Completely original implementation to avoid copyright issues with Numerical Recipes in C

## [1.00] - 1988-09-XX

### Added
- Initial release of Eval as a flexible PC calculator
- Emphasis on Bessel functions for laboratory work
- ANSI C implementation for portability across platforms
- Support for Unix, VMS, and PC (Amiga, IBM, Mac) platforms

### Technical Details
- Originally written to be more powerful than a simple calculator mock-up
- Designed for laboratory work requiring Bessel functions
- Cross-platform compatibility through ANSI C

---

## Legend

- **Added** for new features
- **Changed** for changes in existing functionality
- **Deprecated** for soon-to-be removed features
- **Removed** for now removed features
- **Fixed** for any bug fixes
- **Security** for vulnerability fixes
