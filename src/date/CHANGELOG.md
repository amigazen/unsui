# Changelog

All notable changes to the date command will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0.0] - 2025-12-09

### Added
- Full POSIX compliance with all standard format specifiers
- Hybrid POSIX/Amiga argument parsing system
- Native Amiga clock integration using getclk() and chgclk() APIs
- Amiga FileInfoBlock integration for file timestamp access
- Enhanced file timestamp support with -r/--reference option
- Date string parsing in multiple formats with -d/--date option
- System date/time setting with -s/--set option
- UTC time support with -u/--utc option
- Interactive date/time setting with Amiga-style prompts
- Daylight Saving Time (DST) automatic adjustment
- Memory-safe resource management with proper cleanup
- Amiga-specific format extensions (%Q, %K, %x, %q)
- Color codes for enhanced display (%0-%7, %), %!, %@, %#)
- Interactive help system with template support
- Environment variable support (TIMEZONE, DEFAULT)
- Integrated datehelp functionality via --help-format option
- Integrated dst2 functionality via --dst option
- Comprehensive man page documentation

### Changed
- Complete refactoring from original udate implementation
- Updated from * escape character to % for POSIX compliance
- Enhanced error handling and validation
- Improved code structure and organization
- Updated documentation to reflect modern standards

### Fixed
- All ANSI C89 compliance issues
- Variable declaration placement in statement blocks
- File handling with proper Lock/UnLock sequences
- Memory management and resource cleanup
- Build system configuration for multiple targets

### Removed
- Legacy AmigaDOS-specific argument parsing
- Non-standard escape character usage
- Global variable dependencies
- Obsolete help system implementation
- Separate datehelp.c and dst2.c files (functionality integrated)

## [1.15c] - 1990-01-14

### Added
- Environment variable DEFAULT to change the default date string
- Command line date and time set options
- Daylight savings time functions
- Removed all global variables for pure/resident compatibility

### Changed
- Re-compiled with Lattice 5.04a
- Fixed getenv function compatibility

## [1.14] - 1989-12-30

### Added
- Daylight savings time function integrated into udate code
- Command line date and time setting via prompts or direct strings
- Pure/resident compatibility (no global variables)

### Changed
- Year display correctly until 2079
- Combined udate and dst functionality into single program

## [1.11b] - 1989-11-01

### Changed
- Re-compiled with Lattice 5.04
- Variable assignments updated

## [1.11] - 1989-10-29

### Added
- Removed requirement for strings to begin with '!'
- Made escape value default to '*' but changeable using setenv command

### Changed
- Re-compiled with updated variable assignments

## [1.10] - 1989-10-04

### Added
- Removed requirement for strings to begin with '!'
- Attempted to make udate work similar to UNIX version

### Changed
- Escape value default changed to '*' for AmigaDOS echo compatibility