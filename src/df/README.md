# DF - Disk Free Space Utility

A POSIX compliant `df` command for Amiga systems, converted from the original InfoQ.c utility. This implementation provides both POSIX standard compatibility and Amiga native functionality.

## Features

- **POSIX Compliant** - Full IEEE Std 1003.1-2004 compliance
- **Hybrid Parsing** - Supports both POSIX `getopt` and Amiga `ReadArgs`
- **Disk Space Info** - Shows usage for all mounted filesystems
- **Standard Options** - Supports `-a`, `-i`, `-k`, `-P`, `-t` options
- **Amiga Native** - Uses AmigaDOS APIs for filesystem enumeration
- **Interactive Help** - Built-in help system with `?` command
- **Case Insensitive** - Device name matching works with any case

## Usage

### POSIX Style
```bash
df [OPTION]... [FILE]...
```

### Amiga Style
```bash
df [FS=FILESYSTEM/M] [ALL/S] [KB=KILOBYTES/S] [P=PORTABILITY/S] [INODES/S] [POSIX/K/F]
```

## Options

| Option | Long Option | Description |
|--------|-------------|-------------|
| `-a` | `--all` | Include pseudo, duplicate, inaccessible file systems |
| `-i` | `--inodes` | List inode information instead of block usage |
| `-k` | `--kilobytes` | Use 1024-byte blocks instead of 512-byte blocks |
| `-P` | `--portability` | Use the POSIX output format |
| `-t` | `--type` | Limit listing to filesystems of specified type |
| `-h` | `--help` | Display help and exit |
| `-V` | `--version` | Output version information and exit |

## Examples

```bash
# Show all mounted filesystems
df

# Show specific device
df DF0:

# Show sizes in 1K blocks
df -k

# Show all filesystems including zero-size ones
df -a

# POSIX portable format
df -P

# Filter by filesystem type
df -t DOS                  # Show only DOS filesystems
df -t VOLUME               # Show only volumes

# Interactive help
df ?
```

## Output Format

The output displays filesystem information in a table format:

```
Filesystem          1024-blocks  Used Available Capacity Mounted on
RAM:                   269586       21   269565     1% RAM Disk
DH0:                    65534    19953    45581    31% System
DF0:                     1758      837      921    48% Floppy
```

**Columns:**
- **Filesystem**: Device name (e.g., `DF0:`, `DH0:`)
- **1024-blocks/512-blocks**: Total space in blocks
- **Used**: Used space in blocks
- **Available**: Free space in blocks
- **Capacity**: Percentage used
- **Mounted on**: Mount point or volume name

## Technical Details

- **API**: Uses Amiga DOS `Info()` API for filesystem information
- **Enumeration**: `LockDosList()` and `NextDosEntry()` for device/volume discovery
- **Block Sizes**: Supports both 512-byte and 1024-byte block reporting
- **Memory**: Follows Amiga memory management conventions
- **Inodes**: Limited inode support on Amiga systems (shows as 0)

## Building

The project uses SAS/C SMake for building:

```bash
smake
```









## Credits

- **Original**: "Quarky" Dieter Temme (1992) - in the form of his command InfoQ
- **POSIX compliant adaptation**: amigazen project (2025)

