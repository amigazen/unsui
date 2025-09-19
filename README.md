# _unsui_ 

This is _unsui_, a POSIX environment for Amiga.

## [amigazen project](http://www.amigazen.com)

*A web, suddenly*

*Forty years meditation*

*Minds awaken, free*

**amigazen project** uses modern software development tools and methods to update and rerelease classic Amiga open source software. Releases include a new AWeb, this new Amiga Python 2, and the ToolKit project - a universal SDK for Amiga.

Key to the amigazen project approach is ensuring every project can be built with the same common set of development tools and configurations, so the ToolKit project was created to provide a standard configuration for Amiga development. All *amigazen project* releases will be guaranteed to build against the ToolKit standard so that anyone can download and begin contributing straightaway without having to tailor the toolchain for their own setup.

The amigazen project philosophy is based on openness:

*Open* to anyone and everyone	- *Open* source and free for all	- *Open* your mind and create!

PRs for all of amigazen projects are gratefully received at [GitHub](https://github.com/amigazen/). While the focus now is on classic 68k software, it is intended that all amigazen project releases can be ported to other Amiga-like systems including AROS and MorphOS where feasible.

The original authors of the libraries and commands included here are not affiliated with the amigazen project. This software is redistributed on terms described in the documentation, particularly the file LICENSE.md and individual LICENSE.md files for each component. Since the software in this project is derived from many different sources, each is licensed independently, as some carry GPL licenses while others are licensed under BSD or more specific license terms.

## About _unsui_

_unsui_ is a Unix-like runtime environment for Amiga consisting of *UniLib3*, a POSIX and C99 standard C library, _shami_, a POSIX-like shell, and the _koans_ - individual Amiga ports of standard commands found on BSD and Linux operating systems.

The name _unsui_ (雲水) refers to Zen monks who travel from monastery to monastery seeking enlightenment, symbolizing the journey of learning and discovery through individual command implementations. Each _koan_ represents a step on this path.

Rather than forcing Amiga to become Unix, _unsui_ provides a bridge that allows users to access the vast ecosystem of Unix software while preserving the Amiga experience. The focus is on creating a pragmatic, single-user development environment rather than a full multi-user server OS.

unsui follows the principle of "harmony through compatibility" - Unix standards and Amiga's unique capabilities can coexist and complement each other. Each component name carries both poetic and technical meaning, creating a layered understanding that elevates the project from being merely thematic to being aligned with both its philosophical roots and its practical function.

This project aims to provide a comprehensive set of POSIX utilities and libraries for Amiga, ensuring they can be built out of the box against the ToolKit standard by anyone with an Amiga computer.

### UniLib3 - The _zafu_ (座蒲)

**Standard C Library**: UniLib3 is a (WIP) near complete POSIX and C99 standard C library implementation for Amiga and the SAS/C (and soon DICE) compiler. It provides many more functions and new versions of functions to extend the SAS/C standard C library, similar to VBCC's PosixLib. It does not replace the original SAS/C startup code or sc.lib except where the function prototype has changed since the days of ANSI/C89, in which case UniLib3 provides a new modern version of the same function.

Using UniLib3 it should be possible to port more modern Unix command line software as well as taking advantage of C99 improvements such as the memory-safe string functions. 

UniLib3 is a new version of original emacs-for-Amiga developer David Gay's BSD compatible unix.lib including the extensions added by Amiga GhostScript and xfig developer Enrico Forestieri, as well as code added in from Irmen de Jong's Amiga Python, other similar projects and finished off with all-new code from amigazen project. 

Furthermore UniLib3 includes some modern resource tracking and cleanup features to make Amiga software developed with it more stable, and unlike earlier versions of unix.lib makes full use of the latest operating system features. Apart from unix.lib, additional popular libraries found on Unix-like systems are also planned for inclusion, collectively forming the _zafu_ for _unsui_.

In zen buddhism, the _zafu_ (座蒲) is the meditation cushion that provides the foundation for proper posture and balance. Just as the zafu supports the practitioner's meditation, UniLib3 serves as a stable base for all software in the runtime environment.

### POSIX environment - The _shami_ (沙弥) 

**POSIX compatibility shell**: _shami_ is a Unix-style shell for running scripts and command sequences that follow POSIX conventions.

The name _shami_ (沙弥) means "Novice Monk" - a dedicated practitioner at the beginning of their journey, as users learn to work with a Unix-like shell and tools on Amiga.

### Shell commands - The _koans_ (公案)

**The Tools of Enlightenment**: A complete set of the most common commands found on Unix-like systems, that are Amiga native on the inside and POSIX compatible on the outside, all _koan_ commands simultaneously support both getopts and ReadArgs style command arguments, and running from both the AmigaShell or _shami_ sh shell.

In zen buddhism, a _koan_ (公案) is a riddle used to break through conventional thinking. Mastering the cryptic but powerful POSIX commands is a similar journey. Each tool is a practical puzzle that, once solved, grants deeper understanding and control over the system. In many cases simply creating an Amiga port is a _koan_ in itself.

Each _koan_ is designed to be:
- Self-contained and independently buildable
- Compliant with POSIX standards where applicable
- Amiga native in its use of system APIs
- Refactored to become consistent with the other parts of _unsui_ 
- Utilize the _zafu_ for a consistent approach to supporting both Unix-like and Amiga conventions

#### POSIX.1-2017 Command Utilities

This section lists all POSIX mandated commands, according to the following key:
- **bold** items are already included in the latest _unsui_ pre-release
- *italic* items are in development here in the _unsui_ git repository
- (builtin) items are planned as shell builtins in the _shami_ shell not standalone commands 
- ~~strikethrough~~ items are not going to be included in _unsui_ because they are irrelevant in 2025 and irrelevant to Amiga users
- no markup items are for future consideration

##### Standard-mandated Utilities

(alias)	*ar*	~~asa~~	*at*	**awk**
**basename**	batch*	**bc**	(bg)	*cal*
**cat**	*cd*	**chgrp**	**chmod**	*chown*
*cksum*	**cmp**	*comm*	(command)	**compress**
**cp**	*crontab*	*csplit*	*cut*	**date**
**dd**	**df**	*diff*	**dirname**	**du**
**echo**	**ed**	**env**	*expand*	**expr**
**false**	(fc)	(fg)	**file**	*find*
*fold*	fuser	getconf	(getopts)	**grep**
*hash*	**head**	*iconv*	*id*	*ipcrm*
*ipcs*	(jobs)	*join*	*kill*	**link**
**ln**	locale	localedef	logger	logname
*lp*	**ls**	**man**	mailx   ~~mesg~~
**mkdir**	*mkfifo*	**mv**	newgrp	**nice**
*nl*	(nohup)	**od**	*paste*	pathchk
pax	**pr**	*printf*	*ps*	**pwd**
(read)	renice	**rm**	**rmdir**	**sed**
(set)	**sh**	*sleep*	**sort**	*split*
(stty)	*tabs*	**tail**	talk	**tee**
test*	**time**	**touch**	tput	**tr**
**true**	tsort	(tty)	(type)	(ulimit)
(umask)	(unalias)	*uname*	*uncompress*	*unexpand*
**uniq**	unlink	(unset)	*uudecode*	*uuencode*
(wait)	**wc**	who	~~write~~	*xargs*

##### Optional Utilities

###### User Portability Utilities Option (UPU) 🖥️
*ex*	*more*	**vi**

###### Development Utilities Option (DEV) 🧑‍💻
~~admin~~	*c99*	*cflow*	*ctags*	*cxref*
~~delta~~	~~get~~	*lex*	**m4**	*make*
*nm*	*patch*	~~prs~~	~~rmdel~~	sact
~~sccs~~	**strings**	**strip**	~~unget~~	~~val~~
~~what~~	*yacc*

###### Network Utilities

*finger*	*ftp*	*netstat*	*rsync*	*ssh*
*svn*	*telnet*

###### Other Common Unix Utilities

*arc*	*banner*	**base64**	*bash*	**bison**
*bzip2*	*cflow*	*cforth*	*clzip*	*cpp*
**cron**	*crc32*	**d**	**dir**	*emacs*
*eval*	*f2c*	*f77*	*fcmp*	*foreach*
**fmt**	**flex**	**gawk**	*groff*	*ispell*
**less**	*lua*	*lzip*	*makemake*	**md5**
**md5sum**	*memacs*	*mime64*	*nano*	*ne*
*nuweb*	*nroff*	**pexec**	**pipe**	*pcre*
*perl3*	*perl4*	**pg**	*push*	*ranlib*
*regex*	**roff**	**shar**	*seq*	*sha1*
*smalltalk*	**stat**	*strcmp*	**strip**	*tar*
*tasks*	*tcl*	*tcsh*	*termcap*	*tex*
**top**	*troff*	**unshar**	*unzip*	**uptime**
**v**	**vdir**	*view*	**vim**	**whereis**
*width*	*xlisp*	*z*	*zip*	*zoo*
**xvi**


## Frequently Asked Questions

### Are these Amiga command line tools or POSIX?

Both! _unsui_ implements POSIX standard commands in a hybrid way - they are hybrid both in that they are Amiga native on the inside, and offer POSIX standard features and output on the outside, whilst also supporting both Unix-style 'getopts' arguments (letters preceded by a '-' and words preceded by '--') and Amiga ReadArgs parameters, automatically recognising whichever mode you use.

As a convention, all _unsui_ command binaries are named in lowercase - this gives the user an at-a-glance way of knowing if the command they are about to run is a normal Amiga native command. Especially useful for commands with same/similar names (such as Ed and ed or Eval and eval).

### How does amigazen project decide what software to include in the _unsui_ runtime?

Items for inclusion are selected based on a combination of what would be useful and what's already available as Open Source that can be updated and integrated, cross referenced against POSIX mandated commands and commands included in popular Unix-like distributions, from macOS to Ubuntu. The goal is to offer a full complement of POSIX-compliant command line tools, excepting those which make no sense in Amiga's single user context or simply cannot be made to work on Amiga architecture, as well as additional useful tools commonly found in Unix/Linux based distributions. As a rule of thumb, _unsui_ will prefer a BSD style command in preference to a GNU style command where there is a clear difference, much as macOS does.

### Is _unsui_ intended to be a certified POSIX compatible environment?

No _unsui_ is a pragmatic solution providing the most common tools found in environments that adhere to Unix standard such as POSIX, the Single Unix Specification, or what was the Linux Standard Base, while taking into account the Amiga's single user, personal computing purpose.

Like modern macOS, _unsui_ includes only things that are useful to the user, without adhering to a strict specification and without necessarily always having the best version with the very latest features. Indeed, since many Amiga ports have not been updated for a long time, you can expect that functional and stable releases have been included here rather than the latest and most feature-full releases.

### Is this like GeekGadgets, or Cygwin/cygnix, or even Homebrew?

Yes and no. GeekGadgets was a project of Fred Fish, Markus Wild and many others in the 1980s and 90s to bring mostly Unix, and more specifically mostly GNU, software to Amiga, porting using the ixemul.library. GeekGadgets fizzled out even before the Amiga itself, and GNU software became increasingly complicated, introducing features like autoconf to support both the Linux kernel while retaining cross platform portability, and ironically dramatically increasing the complexity of porting GNU software. Despite that, GeekGadgets lives on in various ways whether the ixemul subsystem of MorphOS or the GCC based SDK for OS4.

This project is maybe not as ambitious as GeekGadgets, and yet also much more modern, with a deliberate choice to NOT rely on GNU toolchains and libraries in favour of Amiga native solutions or more straightforward BSD-style solutions, much as macOS also does with FreeBSD.

Cygwin is likewise a project to port GNU software to Windows. This has in turn inspired AmiCygnix for OS4, which focusses primarily on x11 enabled software.

### Isn't Unix-like software bloated and slow, the antithesis of Amiga's refined system architecture?

It's certainly the case that one of the appeals of Amiga is the focus on lean, efficient, waste-free software design. Mechanisms like libdl are insanely inefficient in their implementation, and POSIX commands seem to go out of their way to obfuscate their usage.

GeekGadgets could certainly be said to be an acquired taste, choosing to enable mostly automated porting of GNU software in the name of gaining access to the rich GNU library.

_unsui_ tries to strike a balance between Amiga _kanso_ and Unix kitchen-sink style functionality, acknowledging that there are other places to find the most efficient Amiga software, while _unsui_ makes some compromises in the name of functionality, stability and ease of porting, without going to the same extremes as GeekGadgets, yet acknowledging that the Amiga of today has substantially more resources than 40 years ago. _unsui_ is for the Amiga of 2025 not 1985.

### What Open Source license applies to the components of _unsui_?

_unsui_ like all amigazen projects is Free and Open Source Software. In most cases this means it is licensed under a BSD 2-clause license, except where the original component is licensed on different terms such as GPL. Therefore all components of _unsui_ carry their own specific license agreement and you should check the LICENSE.md file in each component's src directory. Most components fall under a BSD license, and where more than one option was available typically BSD licensed versions have been chosen over GPL versions.

Components integrated from other older projects are checked for compatible licensing terms before incorporation, in some cases (such as _curses_) even tracking down the original author to seek clarification where the licensing is ambiguous, as a lot of Amiga software was written before the modern understanding of FOSS and well known licenses such as BSD, MIT and GPL.

### Where did all these _koan_ POSIX commands come from?

Amiga has an enormous and rich archive of free and open source projects, including Aminet, the world's largest (and best) software archive. Most of the POSIX commands included here started life in the 1980s and '90s, often as ports of common BSD utils, and later GNU utils, that 'just worked' in the ANSI C compilers of the day. 

At that time Amiga and its C libraries offered a largely BSD compatible standard C API, and developers such as Fred Fish himself (a significant GNU contributor in the early days), Matt Dillon (of CSh, fifo, DICE and later DragonflyBSD fame), David Gay (developer of emacs for Amiga and creator of unix.lib) and others, traded public domain source code for tools openly on usenet and floppy disk as the 'standard stack' for modern computing matured.

Trawling Aminet and most especially the Fish Disk collection, which contains a huge variety of hidden gems from the early years of Amiga when development was contemporaneous to the crystallisation of Unix standards, as well as more modern repositories such as Github, has turned up an enormous trove of POSIX-like software already ported to Amiga, but largely lost to time. Yet it represents almost the entire set of mandated POSIX commands, and many more besides such as _less_, _vim_ (which started life on Amiga, as did _ne_) _bzip2_ and many more that are commonly found in other distributions. _unsui_ is bringing them all together for the first time in one single, integrated distribution, and filling the remaining gaps as well.

In some cases, open source tools for Amiga have been completely rewritten and renamed, with the objective that every _unsui_ _koan_ is Amiga native on the inside but POSIX compliant on the outside. For example Eval has become a full version of 'bc', it's original inspiration.

A complete inventory of the origin of each of these tools, including the license terms, will be available in the file POSIX.md when the collection is completed.

Just a handful of the POSIX commands here are brand new original creations of amigazen project, where no previous example existed, or the example was not even written in C in the first place!

### What work has been done to update the _koan_ POSIX commands?

The typical process each command has undergone entails:

- Commit the original source code to the _unsui_ git repository so that all subsequent changes can be tracked
- Check if the source code can be built with SAS/C, or else another ToolKit-supported compiler
- Update the source code to ANSI C (many older tools might follow older coding styles such as K&R)
- Fix any other incompatibilities and build errors preventing 
- Separate out the header file content from the C file contents where necessary
- Update Amiga specific includes to the appropriate NDK version, usually <proto/....h>, and remove any hardcoded definitions from the original source to avoid clashes
- Provide a complete makefile for at least one ToolKit-supported compiler
- Add Amiga $VER: version tag and $STACK: stack cookie
- Integrate the _unsui_ hybrid getopts/ReadArgs command line technology to allow command like arguments to be provided both in POSIX and AmigaDOS styles
- Alter command line options and add new features necessary to make the command fully POSIX compliant
- Update the documentation including the open source license, man page, and text documentation

Once the first functionally complete release is ready, over time new features are added:
- To take advantage of any newer Amiga features such as memory pools, utility.library functions, locale etc.
- Replace older unsafe functions like C89 string functions with memory safe versions
- Retrofit standard debug build features
- Refine build configuration for making fully optimised release binaries

### What does the name _unsui_ really mean? Is it meant to be UNix SUIte?

It is left to the imagination of the reader to determine if there is greater meaning in the name _unsui_. Could it mean UNified Standard Unix-specification Implementation? Or UNix SUbsystem Inspiration? Perhaps _unsui_ is Not a Supported Unix Instance? Is it the Unix-like Native Shell & Userland Interface? What do you think it means? Similarly, is _zafu_ a Zen Amiga Foundation for Unix? What else could that mean?

### Where is the UniLib3 and the other parts of _unsui_?

You can find the work in progress new version of unix.lib on github in its own repository. _unsui_ is also still work in progress with new _koans_ being added every week.

### Will _unsui_ changes be contributed back to the original project mainlines?

Generally no, the code here should be considered forked from the original projects, since the forks often date back 10, 20 or even 30 or more years, and few if any of the upstream maintainers will accept Amiga specific patches nor would such patches easily fit into the modern cross-platform auto configuration system, even for GeekGadgets or OS4 versions. Keeping the forks forked is in the best interest of the project and allows _unsui_ to focus on optimisation for Amiga native features, and this approach is really no different from what macOS does. This is by no means a bad thing - many of the POSIX commands have barely changed in core functionality in that time, with additional code written to support more modern platforms, to support unicode, or 64-bit, or network and server features. Exactly the kind of code-bloat Amiga users eschew and which would conflict with the balance _unsui_ is designed to strike with regard to Amiga _kanso_ versus Unix complexity.

New features that offer real value to Amiga users may be cherry picked from more recent versions from time to time. Forking in this way allows implementations to deviate from retaining cross platform compatibility and use Amiga native mechanisms internally wherever possible, to bring back some of that Amiga _kanso_.

### Don't some standard AmigaDOS command line tools clash

Yes, commands such as alias, set and eval clash with with similar AmigaDOS command line tools found in the standard path C: directory.

For _unsui_ some of these commands have been implemented as shell builtins, with the result that the POSIX version will be used if you are inside the _shami_ (sh) shell or another POSIX style shell in the _unsui_ collection, while the Amiga Shell will continue to use whichever has priority in it's path. 

### Does _unsui_ support MultiUserFileSystem 

Some individual commands may support multiuser-aware filesystems when built against the UniLib3 unix.lib since the appropriate functions there such as chown() and chmod() and stat() are designed to use direct filesystem access packet i/o to bypass dos.library limitations, but currently no specific support for MUFS is added in general to the _koans_

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

This product includes software developed by the University of California, Berkeley and its contributors.