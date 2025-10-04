# Changelog

All notable changes to the f77 Fortran 77 compiler frontend will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.5] - 2025-10-04

### Added
- POSIX compliance improvements for better cross-platform compatibility
- Standard compiler options: -I (include directories), -L (library search paths), -D (preprocessor definitions), -U (undefine macros)
- Enhanced usage information with comprehensive help text
- Improved man page documentation with POSIX-standard formatting
- Support for standard preprocessor macro definitions with -D name=value syntax

### Changed
- Reorganized option documentation to list standard POSIX options first
- Enhanced error messages and help text for better user experience
- Improved option parsing to handle new POSIX-compliant options
- Updated usage function to display all available options with descriptions

### Fixed
- Improved option parsing and validation for new POSIX options
- Better error handling for invalid options
- Enhanced backward compatibility while adding new features

### Security
- Added input validation for new option parameters to prevent buffer overflows

## [1.4] - 1994-10-26

### Added
- NOOPTSIZE OPTTIME switches to sc command line when using -O optimization
- Enhanced error handling and debugging capabilities

### Changed
- Improved spawn() return code handling in f2c() function
- Removed temporary file handling from f2c() to avoid issues with #line directives
- Simplified cc() function by removing superfluous third argument

### Fixed
- Fixed minor bug in spawn() return code handling in f2c()

## [1.3] - 1994-10-21

### Added
- NOERRSRC flag to sc invocation for cleaner compilation
- -noext switch to disable Fortran 77 extensions
- Filename length validation (128 character limit) to prevent buffer overflow

### Changed
- Changed -U option to +U (old form still accepted for backward compatibility)
- Improved subprocess spawning to prevent buffer overflows

### Security
- Added protection against buffer overflow in subprocess spawning

## [1.2] - 1994-10-20

### Changed
- Moved redirection in f2c invocation to ensure DOS 1.x compatibility
- Improved cross-version compatibility

## [1.1] - 1994-10-20

### Added
- Initial release of f77 frontend for f2c
- Basic Fortran 77 compilation support
- SAS/C integration
- Standard compiler options (-c, -o, -g, -v, etc.)
- Library linking support
- Object file generation

### Features
- Support for .f, .c, .o, and .lib file types
- Integration with f2c Fortran to C translator
- SAS/C compiler and linker integration
- AmigaDOS-specific optimizations
---

## Unreleased Changes

*No unreleased changes at this time.*
