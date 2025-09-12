# date Command - unsui POSIX runtime for Amiga

## Overview

The `date` command is a comprehensive date and time utility that provides full POSIX compliance while maintaining native Amiga integration. This implementation combines the best of both worlds, offering standard POSIX functionality alongside Amiga-specific enhancements.

## Features

### POSIX Compliance
- Full IEEE Std 1003.1-2001 ("POSIX.1") compliance
- All standard format specifiers (%a, %A, %b, %B, %c, %d, %e, %F, %g, %G, %h, %H, %I, %j, %k, %l, %m, %M, %n, %N, %p, %P, %r, %R, %s, %S, %t, %T, %u, %U, %V, %w, %W, %x, %X, %y, %Y, %z, %:z, %::z, %:::z, %Z)
- Standard command line options (-d, -r, -s, -u, -h, -V)
- Extended options (--help-format, --dst) integrated from separate utilities
- Date string parsing in multiple formats

### Amiga Integration
- Native Amiga clock integration using getclk() and chgclk() APIs
- Amiga FileInfoBlock integration for file timestamp access
- Hybrid argument parsing supporting both POSIX and Amiga styles
- Daylight Saving Time (DST) automatic adjustment
- Interactive date/time setting with Amiga-style prompts
- Memory-safe resource management

### Amiga Extensions
- Amiga-specific format extensions (%Q, %K, %x, %q)
- Color codes for enhanced display (%0-%7, %), %!, %@, %#)
- Interactive help system with template support
- Environment variable support (TIMEZONE, DEFAULT)

## Installation

Copy the integrated date program to the sys:c directory:

```
1> copy date c:
```

This will replace the original AmigaDOS date program with the enhanced version that includes all functionality from the separate datehelp and dst2 utilities.

## Usage

### POSIX Style
```bash
date                           # Display current date/time
date "+%Y-%m-%d %H:%M:%S"     # Custom format
date -d "2024-01-01 12:00:00" # Display specific time
date -r filename               # Show file modification time
date -s "2024-12-09 14:30:00" # Set system time
date -u                        # Use UTC time
date -h                        # Show help
date --help-format             # Show format specifier help
date --dst                     # Adjust for Daylight Saving Time
```

### Amiga Style
```bash
date ?                        # Interactive help
date FORMAT="%Y-%m-%d"        # Custom format
date SET                      # Interactive date/time setting
date UTC                      # Use UTC time
date REFERENCE=filename       # Show file modification time
date DEBUG                    # Enable DST adjustment
date POSIX -d "2024-01-01"    # Force POSIX parsing
```

## Environment Variables

- `TIMEZONE`: Set timezone for display (e.g., "EST:EDT", "MST:MDT", "PST")
- `DEFAULT`: Set default format string when no format is specified

## Daylight Saving Time

The date utility automatically adjusts for daylight saving time. Add to your startup-sequence:

```
1> date -d
```

Set your battery clock to standard time and the utility will handle DST automatically.

## History

### Version 2.0 (December 9, 2025)
- Complete POSIX compliance refactoring
- Hybrid POSIX/Amiga argument parsing
- Native Amiga clock integration
- Enhanced file timestamp support
- Interactive date/time setting
- Memory-safe resource management
- Amiga-specific format extensions and color codes

### Original Implementation
Based on the original udate implementation by George Kerber (1989-1990), this version has been completely refactored for modern POSIX compliance while retaining all Amiga-specific functionality.

## Author

Part of unsui from amigazen project.
Based on the original udate implementation by George Kerber (1989-1990).
Refactored for POSIX compliance and Amiga integration by amigazen project.