# _unsui_ 

This is _unsui_, a POSIX environment for Amiga.

## [amigazen project](http://www.amigazen.com)

*A web, suddenly*

*Forty years meditation*

*Minds awaken, free*

**amigazen project** uses modern software development tools and methods to update and rerelease classic Amiga open source software. Releases include a new AWeb, the _unsui_ platform, and the ToolKit project - a universal SDK for Amiga.

Key to the amigazen project approach is ensuring every project can be built with the same common set of development tools and configurations, so the ToolKit was created to provide a standard configuration for Amiga development. All *amigazen project* releases will be guaranteed to build against the ToolKit standard so that anyone can download and begin contributing straightaway without having to tailor the toolchain for their own setup.

The original authors of the libraries and commands included here are not affiliated with the amigazen project. This software is redistributed on terms described in the documentation, particularly the file LICENSE.md and individual LICENSE.md files for each component.

Our philosophy is based on openness:

*Open* to anyone and everyone	- *Open* source and free for all	- *Open* your mind and create!

PRs for all amigazen projects are gratefully received at [GitHub](https://github.com/amigazen/). While the amigazen project focus now is on classic 68k software, it is intended that all amigazen project releases can be ported to other Amiga and Amiga-like systems including AROS and MorphOS where feasible.

## About _unsui_

_unsui_ is a Unix-like runtime environment for Amiga consisting of UnixLib3, a POSIX and C99 standard C library, shami, a POSIX-like shell, and the _koans_ - individual Amiga ports of standard commands found on BSD and Linux operating systems.

The name _unsui_ (雲水) refers to Zen monks who travel from monastery to monastery seeking enlightenment, symbolizing the journey of learning and discovery through individual command implementations. Each _koan_ represents a step on this path.

Rather than forcing Amiga to become Unix, _unsui_ provides a bridge that allows users to access the vast ecosystem of Unix software while preserving the Amiga experience. The focus is on creating a pragmatic, single-user development environment rather than a full multi-user server OS.

unsui follows the principle of "harmony through compatibility" - Unix standards and Amiga's unique capabilities can coexist and complement each other. Each component name carries both poetic and technical meaning, creating a layered understanding that elevates the project from being merely thematic to being aligned with both its philosophical roots and its practical function.

This project aims to provide a comprehensive set of POSIX utilities and libraries for Amiga, ensuring they can be built out of the box against the ToolKit standard by anyone with an Amiga computer.

### UnixLib3 - The _zafu_ (座蒲)

**Standard C Library**: UnixLib3 is a (WIP) near complete POSIX and C99 standard C library implementation for Amiga and the SAS/C (and soon DICE) compiler. It provides many more functions and new versions of functions to extend the SAS/C standard C library, similar to VBCC's PosixLib. It does not replace the original SAS/C startup code or sc.lib except where the function prototype has changed since the days of ANSI/C89, in which case UnixLib3 provides a new modern version of the same function. You could almost says it's an 'unClib'.

Using UnixLib3 it should be possible to port more modern functions as well as taking advantage of C99 improvements such as the memory-safe string functions. 

UnixLib3 is a new version of original emacs-for-Amiga developer David Gay's BSD compatible unix.lib including the extensions added by Amiga xfig developer Enrico Forestieri, as well as code added in from Irmen de Jong's Amiga Python, and all-new code from amigazen project. 

Furthermore UnixLib3 includes some modern resource tracking and cleanup features to make Amiga software developed with it more stable, and unlike earlier versions makes full use of the latest operating system features. Apart from unix.lib, additional popular libraries found on Unix-like systems are also planned for inclusion, collectively forming the _zafu_ for _unsui_.

In zen buddhism, the _zafu_ (座蒲) is the meditation cushion that provides the foundation for proper posture and balance. Just as the zafu supports the practitioner's meditation, UnixLib3 serves as a stable base for all software in the runtime environment.

### POSIX environment - The _shami_ (沙弥) 

**POSIX compatibility shell**: _shami_ is a Unix-style shell for running scripts and command sequences that follow POSIX conventions.

The name _shami_ (沙弥) means "Novice Monk" - a dedicated practitioner at the beginning of their journey, as users learn to work with a Unix-like shell and tools on Amiga.

### Shell commands - The _koans_ (公案)

**The Tools of Insight**: A complete set of the most common commands found on Unix-like systems, that are Amiga native on the inside and POSIX compatible on the outside, all _koan_ commands simultaneously support both getopts and ReadArgs style command arguments, and running from both the AmigaShell or _shami_ sh shell.

In zen buddhism, a _koan_ (公案) is a riddle used to break through conventional thinking. Mastering the cryptic but powerful POSIX commands is a similar journey. Each tool is a practical puzzle that, once solved, grants deeper understanding and control over the system. In many cases simply creating an Amiga port is a _koan_ in itself.

Each _koan_ is designed to be:
- Self-contained and independently buildable
- Compliant with POSIX standards where applicable
- Amiga native in its use of system APIs
- Refactored to become consistent with the other parts of _unsui_ 
- Utilize the _zafu_ for a consistent approach to supporting both Unix-like and Amiga conventions

## Frequently Asked Questions

### How does amigazen project decide what software to include in the _unsui_ runtime?

The _unsui_ selects items for inclusion based on a combination what would be useful and what's already available as Open Source that can be updated and integrated.

### Is _unsui_ intended to be a certified POSIX compatible environment?

No _unsui_ is a pragmatic solution providing the most common tools found in environments that adhere to Unix standard such as POSIX, the Single Unix Specification, or what was the Linux Standard Base, while taking into account the Amiga's single user, personal computing purpose.

Like modern macOS, _unsui_ includes only things that are useful to the user, without adhering to a strict specification and without necessarily always having the best version with the very latest features. Indeed, since many Amiga ports have not been updated for a long time, you can expect that functional and stable releases have been included over the cutting edge.

That means that useful tools originating from Unix platforms that are NOT part of any standard specification will also be included.

### Is this like GeekGadgets, or Cygwin/cygnix, or even Homebrew?

Yes and no. GeekGadgets was a project of Fred Fish, Markus Wild and many others in the 1980s and 90s to bring mostly Unix, and more specifically mostly GNU, software to Amiga, porting using the ixemul.library. GeekGadgets fizzled out even before the Amiga itself, and GNU software became increasingly complicated, introducing features like autoconf to support both the Linux kernel while retaining cross platform portability, and ironically dramatically increasing the complexity of porting GNU software. Despite that, GeekGadgets lives on in various ways whether the ixemul subsystem of MorphOS or the GCC based SDK for OS4.

This project is maybe not as ambitious as GeekGadgets, and yet also much more modern, with a deliberate choice to NOT rely on GNU toolchains and libraries in favour of Amiga native solutions or more straightforward BSD-style solutions, much as macOS also does with FreeBSD.

Cygwin is likewise a project to port GNU software to Windows. This has in turn inspired AmiCygnix for OS4, which focusses primarily on x11 enabled software.

### Isn't Unix-like software bloated and slow, the antithesis of Amiga's refined system architecture?

It's certainly the case that one of the appeals of Amiga is the focus on lean, efficient, waste-free software design. Mechanisms like libdl are insanely inefficient in their implementation, and POSIX commands seem to go out of their way to obfuscate their usage.

GeekGadgets could certainly be said to be an acquired taste, choosing to enable mostly automated porting of GNU software in the name of gaining access to the rich GNU library.

_unsui_ tries to strike a balance between Amiga _kanso_ and Unix kitchen-sink style functionality, acknowledging that there are other places to find the most efficient Amiga software, while _unsui_ makes some compromises in the name of functionality, stability and ease of porting, without going to the same extremes as GeekGadgets, yet acknowledging that the Amiga of today has substantially more resources than 40 years ago. _unsui_ is for the Amiga of 2025 not 1985.

### What Open Source license applies to the components of _unsui_?

_unsui_ like all amigazen projects is Free and Open Source Software. In most cases this means it is licensed under a BSD 2-clause license, except where the original component is licensed on different terms such as GPL. Therefore all components of _unsui_ carry their own specific license agreement and you should check the LICENSE.md file in each component's src directory.

Components integrated from other older projects are checked for compatible licensing terms before incorporation, in some cases (such as _curses_) even tracking down the original author to seek clarification where the licensing is ambiguous, as a lot of Amiga software was written before the modern understanding of FOSS and well known licenses such as BSD, MIT and GPL.

### What does the name _unsui_ really mean? Is it meant to be UNix SUIte?

It is left to the imagination of the reader to determine if there is greater meaning in the name _unsui_. Could it mean UNified Standard Unix-specification Implementation? Or UNix SUbsystem Interface? Perhaps _unsui_ is Not a Supported Unix Instance? Is it Unix-like System Userland Idiom? What do you think it means? Similarly, is _zafu_ a Zen Amiga Foundation for Unix? What else could that mean?

### Where is the UnixLib3 and the other parts of _unsui_?

You can find the work in progress new version of unix.lib on github in its own repository. _unsui_ is still work in progress with new _koans_ being added every week.

### Will _unsui_ changes be contributed back to the original project mainlines, or updated to the latest ?

Generally no, the code here should be considered forked from the original projects, since the forks often date back 10, 20 or even 30 or more years, and few if any of the upstream maintainers will accept Amiga specific patches nor would such patches easily fit into the modern cross-platform auto configuration system, even for GeekGadgets or OS4 versions. Keeping the forks forked is in the best interest of the project and allows _unsui_ to focus on optimisation for Amiga native features, and this approach is really no different from what macOS does. This is by no means a bad thing - many of the POSIX commands have barely changed in core functionality in that time, with additional code written to support more modern platforms, to support unicode, or 64-bit, or network and server features. Exactly the kind of code-bloat Amiga users eschew and which would conflict with the balance _unsui_ is designed to strike with regard to Amiga _kanso_ versus Unix complexity.

New features that offer real value to Amiga users may be cherry picked from more recent versions from time to time.

### Will _unsui_ be released on Aminet?

All stable, polished releases will be uploaded to Aminet yes. Work in progress can always be found in public repositories at https://github.com/amigazen/

### Who are amigazen project?

To learn more about amigazen project and its aims, see AMIGAZEN.md or the Amiga-compatible website at http://www.amigazen.com/

## Contact 

- At GitHub https://github.com/amigazen/unsui/ 
- on the web at http://www.amigazen.com/unsui/ (Amiga browser compatible)
- or email unsui@amigazen.com

## Acknowledgements

*Amiga* is a trademark of **Amiga Inc**. 

The POSIX commands and libraries included here are based on various open source implementations and standards. Each individual _koan_ maintains its own licensing and attribution as documented in their respective src directories.

The _unsui_ platform concept and implementation is a product of the **amigazen project**