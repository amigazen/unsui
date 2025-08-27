# _unsui_ 

This is unsui, a POSIX environment for Amiga.

## [amigazen project](http://www.amigazen.com)

*A web, suddenly*

*Forty years meditation*

*Minds awaken, free*

**amigazen project** uses modern software development tools and methods to update and rerelease classic Amiga open source software. Our releases include a new AWeb, the _unsui_ platform, and the ToolKit project - a universal SDK for Amiga.

Key to the amigazen project approach is ensuring every project can be built with the same common set of development tools and configurations, so the ToolKit was created to provide a standard configuration for Amiga development. All *amigazen project* releases will be guaranteed to build against the ToolKit standard so that anyone can download and begin contributing straightaway without having to tailor the toolchain for their own setup.

The original authors of unix.lib or the POSIX commands included here are not affiliated with the amigazen project. This software is redistributed on terms described in the documentation, particularly the file LICENSE.md

Our philosophy is based on openness:

*Open* to anyone and everyone	- *Open* source and free for all	- *Open* your mind and create!

PRs for all amigazen projects are gratefully received at [GitHub](https://github.com/amigazen/). While the amigazen project focus now is on classic 68k software, it is intended that all amigazen project releases can be ported to other Amiga and Amiga-like systems including AROS and MorphOS where feasible.

## About _unsui_

The _unsui_ platform represents a collection of individual POSIX command implementations, each referred to as a "koan" - a self-contained unit of wisdom and functionality. Each koan is designed to be built independently while following consistent patterns and standards.

The name _unsui_ (雲水) refers to Zen monks who travel from monastery to monastery seeking enlightenment, symbolizing the journey of learning and discovery through individual command implementations. Each koan represents a step on this path.

Rather than forcing Amiga to become Unix, _unsui_ provides a bridge that allows users to access the vast ecosystem of Unix software while preserving the Amiga experience. The focus is on creating a pragmatic, single-user development environment rather than a full multi-user server OS.

unsui follows the principle of "harmony through compatibility" - Unix standards and Amiga's unique capabilities can coexist and complement each other. Each component name carries both poetic and technical meaning, creating a layered understanding that elevates the project from being merely thematic to being aligned with both its philosophical roots and its practical function.

This project aims to provide a comprehensive set of POSIX utilities for Amiga, ensuring they can be built out of the box against the ToolKit standard by anyone with an Amiga computer.

### UnixLib3 - The _zafu_ (座蒲)

**Standard C Library**: UnixLib3 is a (WIP) near complete POSIX and C99 standard C library implementation for Amiga and the SAS/C (and soon DICE) compiler. It provides many more functions and new versions of functions to extend the SAS/C standard C library, similar to VBCC's PosixLib. It does not replace the original SAS/C startup code or sc.lib except where the function prototype has changed since the days of ANSI/C89, in which case UnixLib3 provides a new modern version of the same function. Using UnixLib3 it should be possible to port more modern functions as well as taking advantage of C99 improvements such as the memory-safe string functions. UnixLib3 is a new version of original emacs-for-Amiga developer David Gay's BSD compatible unix.lib including the extensions added by Amiga xfig developer Enrico Forestieri, as well as code added in from Irmen de Jong's Amiga Python, and all-new code from amigazen project. Furthermore UnixLib3 includes some modern resource tracking and cleanup features to make Amiga software developed with it more stable, and unlike earlier versions makes full use of the latest operating system features.

In zen buddhism, the _zafu_ (座蒲) is the meditation cushion that provides the foundation for proper posture and balance. Just as the zafu supports the practitioner's meditation, UnixLib3 serves as a stable base for all software in the runtime environment.

### POSIX environment - The _shami_ (沙弥) 

**POSIX compatibility shell**: _shami_ is a Unix-style shell for running scripts and command sequences that follow POSIX conventions.

The name _shami_ (沙弥) means "Novice Monk" - a dedicated practitioner at the beginning of their journey, representing the tool for mastery as users learn to work with Unix tools on Amiga.

### Shell commands - The _koans_ (公案)

**The Tools of Insight**: A complete set of the most common commands found on Unix-like systems, that are Amiga native on the inside and POSIX compatible on the outside, all _koan_ commands simultaneously support both getops and ReadArgs style command arguments, and running from both the AmigaShell or _shami_ sh shell.

In zen buddhism, a _koan_ (公案) is a riddle used to break through conventional thinking. Mastering the cryptic but powerful POSIX commands is a similar journey. Each tool is a practical puzzle that, once solved, grants deeper understanding and control over the system.

Each _koan_ is designed to be:
- Self-contained and independently buildable
- Compliant with POSIX standards where applicable
- Optimized for Amiga platform characteristics
- Following consistent coding and build patterns

## Frequently Asked Questions

### How does amigazen project decide what software to include in the _unsui_ runtime?

The _unsui_ selects items for inclusion based on a combination what would be useful and what's already available as Open Source that can be updated and integrated.

### Is _unsui_ intended to be a certified POSIX compatible environment?

No _unsui_ is a pragmatic solution providing the most common tools found in environments that adhere to Unix standard such as POSIX, the Single Unix Specification, or what was the Linux Standard Base, while taking into account the Amiga's single user, personal computing purpose.

Like modern macOS, _unsui_ includes only things that are useful to the user, without adhering to a strict specification and without necessarily always having the best version with the very latest features. Indeed, since many Amiga ports have not been updated for a long time, you can expect that functional and stable releases have been included over the cutting edge.

That means that useful tools originating from Unix platforms that are NOT part of any standard specification will also be included.

### Is this like GeekGadgets, or Cygwin/cygnix, or even Homebrew?

Yes and no. GeekGadgets was a project of Fred Fish, Markus Wild and many others in the 1980s and 90s to bring mostly Unix, and more specifically mostly GNU, software to Amiga, porting using the ixemul.library. GeekGadgets fizzled even before the AMiga, and GNU software became increasingly complicated, introducing features like autoconf to support the Linux kernel, while retaining cross platform portability. Despite that, GeekGadgets lives on in various ways whether the ixemul subsystem of MorphOS or the GCC based SDK for OS4.

This project is maybe not as ambitious as GeekGadgets, and yet also much more modern, with a deliberate choice to NOT rely on GNU toolchains and libraries in favour of Amiga native solutions or more straightforward BSD-style solutions, much as macOS also does with FreeBSD.

Cygwin is likewise a project to port GNU software to Windows. This has in turn inspired AmiCygnix for OS4, which focusses primarily on x11 enabled software.

### What Open Source license applies to the components of _unsui_?

_unsui_ like all amigazen projects is Free and Open Source Software. In most cases this means it is licensed under a BSD 2-clause license, except where the original component is licensed on different terms such as GPL. Therefore all components of _unsui_ carry their own specific license agreement and you should check the LICENSE.md file in each component's src directory.

Components integrated from other older projects are checked for compatible licensing terms before incorporation, in some cases (such as _curses_) even tracking down the original author to seek clarification where the licensing is ambiguous, as a lot of Amiga software was written before the modern understanding of FOSS and well known licenses such as BSD, MIT and GPL.

### What does the name _unsui_ really mean? Is it meant to be UNix SUIte?

It is left to the imagination of the reader to determine if there is greater meaning in the name _unsui_. Could it mean UNified Standard Unix-specification Implementation? Or UNix SUbsystem Interface? Perhaps _unsui_ is Not a Supported Unix Instance? What do you think it means?

### Will _unsui_ be released on Aminet?

All stable, polished releases will be uploaded to Aminet yes. Work in progress can always be found in public repositories at https://github.com/amigazen/

### Who are amigazen project?

To learn more about amigazen project, see AMIGAZEN.md or the Amiga-compatible website at http://www.amigazen.com/

## Contact 

- At GitHub https://github.com/amigazen/unsui/ 
- on the web at http://www.amigazen.com/unsui/ (Amiga browser compatible)
- or email unsui@amigazen.com

## Acknowledgements

*Amiga* is a trademark of **Amiga Inc**. 

The POSIX utilities included here are based on various open source implementations and standards. Each individual koan maintains its own licensing and attribution as documented in their respective src directories.

The _unsui_ platform concept and implementation is a product of the **amigazen project**