# unsui runtime

This is unsui, a complete POSIX runtime for Amiga.

## [amigazen project](http://www.amigazen.com)

*A web, suddenly*

*Forty years meditation*

*Minds awaken, free*

**amigazen project** uses modern software development tools and methods to update and rerelease classic Amiga open source software. Our releases include a new AWeb, the unsui platform, and the ToolKit project - a universal SDK for Amiga.

Key to our approach is ensuring every project can be built with the same common set of development tools and configurations, so we created the ToolKit project to provide a standard configuration for Amiga development. All *amigazen project* releases will be guaranteed to build against the ToolKit standard so that anyone can download and begin contributing straightaway without having to tailor the toolchain for their own setup.

The original authors of the POSIX commands included here are not affiliated with the amigazen project. This software is redistributed on terms described in the documentation, particularly the file LICENSE.md

Our philosophy is based on openness:

*Open* to anyone and everyone	- *Open* source and free for all	- *Open* your mind and create!

PRs for all of our projects are gratefully received at [GitHub](https://github.com/amigazen/). While our focus now is on classic 68k software, we do intend that all amigazen project releases can be ported to other Amiga-like systems including AROS and MorphOS where feasible.

## About unsui

The unsui platform represents a collection of individual POSIX command implementations, each referred to as a "koan" - a self-contained unit of wisdom and functionality. Each koan is designed to be built independently while following consistent patterns and standards.

The name "unsui" (雲水) refers to Zen monks who travel from monastery to monastery seeking enlightenment, symbolizing the journey of learning and discovery through individual command implementations. Each koan represents a step on this path.

Rather than forcing Amiga to become Unix, unsui provides a bridge that allows users to access the vast ecosystem of Unix software while preserving the Amiga experience. The focus is on creating a pragmatic, single-user development environment rather than a full multi-user server OS.

unsui follows the principle of "harmony through compatibility" - Unix standards and Amiga's unique capabilities can coexist and complement each other. Each component name carries both poetic and technical meaning, creating a layered understanding that elevates the project from being merely thematic to being aligned with both its philosophical roots and its practical function.

This project aims to provide a comprehensive set of POSIX utilities for Amiga, ensuring they can be built out of the box against the ToolKit standard by anyone with an Amiga computer.

Each koan in the unsui platform is designed to be:
- Self-contained and independently buildable
- Compliant with POSIX standards where applicable
- Optimized for Amiga platform characteristics
- Following consistent coding and build patterns

### UnixLib3 - The 'zafu' (座蒲)
**Foundation C Standard Library**: Full implementation of stdio.h, stdlib.h, string.h, time.h, math.h with all standard functions (printf, malloc, strcpy, fopen, fread, sin, cos, sqrt, pow).

**POSIX.1 Extensions**: File system APIs (opendir, readdir), process control (fork, execve, waitpid, kill), pipes and I/O (pipe, dup2), and basic signal handling (signal, raise).

In zen, the zafu (座蒲) is the meditation cushion that provides the foundation for proper posture and balance. Just as the zafu supports the practitioner's meditation, UnixLib3 serves as a stable base for all software in the runtime environment.

### Shami (沙弥) - POSIX Shell
**POSIX sh Compatibility**: Command execution, I/O redirection (>, <, >>), pipes (|), environment variables ($PATH, $HOME), and basic scripting (if, for, while, case statements).

With the unsui environment as the path and the Shami shell as the guide, users can finally stop fighting the system and achieve a state of productive harmony on the machine they love. The name "Shami" (沙弥) means "Novice Monk" - a dedicated practitioner at the beginning of their journey, representing the tool for mastery as users learn to work with Unix tools on Amiga.

### Koans (公案) - Core Utilities
**The Tools of Insight**: In Zen, a koan (公案) is a riddle used to break through conventional thinking. Mastering the cryptic but powerful Unix utilities is a similar journey. Each tool is a practical puzzle that, once solved, grants deeper understanding and control over the system.

**File Management**: ls, cd, cp, mv, rm, mkdir, ln...
**Text Processing**: cat, grep, sed, awk, head, tail...
**System & Archiving**: ps, kill, uname, tar, gzip...

### Development Tools
**Self-Hosting Environment**: C compiler (cc/gcc), linker (ld), make build system, standard headers in /usr/include. Enables development within the unsui environment itself.

### Terminal Support
**ncurses Library**: Essential for terminal control, enabling text editors (vi, emacs) and interactive command-line applications. Core to a rich command-line experience.

## Current Koans

The following POSIX commands are currently implemented as koans:

- **basename** - Extract filename from path
- **dirname** - Extract directory from path  
- **grep** - Pattern matching and searching
- **head** - Display beginning of files
- **tail** - Display end of files
- **touch** - Update file timestamps
- **wc** - Word, line, and character counting
- **cc** - C compiler wrapper

Each koan includes:
- Complete source code implementation
- Unit tests and validation
- Build configuration (SMakefile)
- Documentation and usage examples
- License information

## The Zen Philosophy Behind Our Names

For readers unfamiliar with Zen Buddhism, the naming choices may seem unusual at first. This section explains the deeper philosophy that inspired these names and why they are particularly fitting for a Unix subsystem on Amiga.

**Zen and the Art of Computing**: Zen Buddhism emphasizes direct experience, practical wisdom, and finding harmony between seemingly opposing forces. It is about achieving clarity through practice rather than through complex theories. This philosophy mirrors the Unix philosophy of simple, composable tools that work together harmoniously.

**unsui (雲水) - "Cloud, Water"**: In zen monasteries, an unsui is a wandering student who travels between different temples to learn from various masters. Like clouds that drift freely across the sky or water that flows around obstacles, the unsui represents adaptability and the ability to move between different environments without losing their essential nature. This metaphor captures the goal: allowing Amiga users to access Unix software while maintaining the unique character of their platform.

**Why This Matters**: These names are not just decorative - they reflect the principle that good software design shares the same principles as good zen practice: simplicity, directness, and harmony. By choosing names that carry this deeper meaning, the project reminds users that computing can be more than just technical - it can be an art form that brings joy and insight.

## About ToolKit

**ToolKit** exists to solve the problem that most Amiga software was written in the 1980s and 90s, by individuals working alone, each with their own preferred setup for where their dev tools are run from, where their include files, static libs and other toolchain artifacts could be found, which versions they used and which custom modifications they made. Open source collaboration did not exist as we know it in 2025. 

**ToolKit** from amigazen project is a work in progress to make a standardised installation of not just the Native Developer Kit, but the compilers, build tools and third party components needed to be able to consistently build projects in collaboration with others, without each contributor having to change build files to work with their particular toolchain configuration. 

All *amigazen project* releases will release in a ready to build configuration according to the ToolKit standard.

Each component of **ToolKit** is open source and will have it's own github repo, while ToolKit itself will eventually be released as an easy to install package containing the redistributable components, as well as scripts to easily install the parts that are not freely redistributable from archive.

## Building Individual Koans

Each koan can be built independently using the ToolKit standard. Navigate to the specific koan directory and use the provided SMakefile:

```
cd src/[koan-name]
smake
```

The build system ensures consistency across all koans while allowing independent development and testing.

## Contact 

- At GitHub https://github.com/amigazen/unsui/ 
- on the web at http://www.amigazen.com/unsui/ (Amiga browser compatible)
- or email unsui@amigazen.com

## Acknowledgements

*Amiga* is a trademark of **Amiga Inc**. 

The POSIX utilities included here are based on various open source implementations and standards. Each individual koan maintains its own licensing and attribution as documented in their respective directories.

The unsui platform concept and implementation is a product of the **amigazen project**, see LICENSE.md for details.