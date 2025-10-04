# Changelog

All notable changes to the f2c Fortran to C translator will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.3] - 2025-10-04

### Added
- Added usage text
- BSD 2-Clause License
- Refactoring for ANSI C compliance improvements

### Fixed
- Fixed ANSI C compliance issues in sysdep.c
- Improved assignment in conditional statements

## [3.2] - 1994-06-15

### Added
- Amiga port by Torsten Poulin using SAS/C 6.51
- Complete Fortran 77 to C translation capability
- Support for complex and double complex data types
- Comprehensive runtime libraries (libF77 and libI77)
- Full Fortran I/O implementation including formatted, unformatted, list-directed, and namelist I/O
- Support for all Fortran 77 intrinsic functions
- Command-line options for various translation modes

### Changed
- Ported from original AT&T Bell Laboratories implementation
- Adapted for AmigaDOS and SAS/C compiler
- Updated build system to use SAS/C SMake format

### Fixed
- Fixed temporary file handling bug for 32-bit addressing
- Improved memory management for Amiga platform
- Enhanced error handling and diagnostics

## [1.0] - 1990-1994 (Original AT&T Release)

### Added
- Complete Fortran 77 to C translator
- Support for all Fortran 77 language features
- Comprehensive runtime library implementation
- Extensive command-line options
- Cross-platform compatibility

### Features
- Fortran 77 source parsing and translation
- C code generation with proper type mapping
- Runtime library for Fortran intrinsics and I/O
- Support for complex arithmetic
- Memory management and optimization
- Error reporting and diagnostics

---

## Development History Summary

The f2c project has an extensive development history spanning from 1989 to 1994, with over 1,900 individual changes documented in the original changes file. Key development phases include:

### 1989-1990: Initial Development
- Core Fortran 77 language support
- Basic C code generation
- Runtime library development
- Cross-platform compatibility

### 1990-1992: Feature Enhancement
- Advanced Fortran features (NAMELIST, complex arithmetic)
- Improved error handling and diagnostics
- Performance optimizations
- Extended command-line options

### 1992-1994: Maturity and Porting
- Bug fixes and stability improvements
- Amiga port development
- Final optimizations and refinements
- Documentation completion

### 2025: Modernization
- POSIX compliance improvements
- ANSI C compliance enhancements
- Documentation modernization
- License standardization

---

## Unreleased Changes

*No unreleased changes at this time.*
