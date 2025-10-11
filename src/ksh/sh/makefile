# PD Bourne/Korn Shell
# $Id: Makefile,v 1.6 93/05/08 15:59:56 sjg Exp $

#### Start of system configuration section. ####

VPATH = .
srcdir = .

# Common prefix for machine-independent installed files.
prefix = /usr/local
# Common prefix for machine-dependent installed files.
exec_prefix = $(prefix)

# Directory to install executables in.
bindir = $(exec_prefix)/bin
# Directory to install libraries in.
libdir = $(exec_prefix)/lib
# Directory to install the Info files in.
infodir = $(prefix)/info
# Directory to install the man page in.
mandir = $(prefix)/man/man$(manext)
# Number to put on the man page filename.
manext = 1

# Program to install executables.
INSTALL_PROGRAM = install
# Program to install data like man pages.
INSTALL_DATA = install -m 644
# Generic install program.
INSTALL = install

# AmigaOS specific configuration
CC = m68k-amigaos-gcc
DEFS = -D_BSD -Damigados -DUSE_TRACE -DDEBUG_AFREE 
# FIXME: Configure should add -resident and -mstackextend
# to CFLAGS only when the target is AmigaOS.
CFLAGS = -O2 -resident -mstackextend

# FIXME: Configure should add -resident to LDFLAGS only
# when the target is AmigaOS.
LDFLAGS = -s -resident
LIBS = 

# AmigaOS hack.  Symbolic links are available, but the actual link
# name needs to be AmigaOS format (device:path1/path2/...).  When
# ixemul.library makes them in this format, this can be enabled.
#LN_S = @LN_S@
LN_S = cp

#### End of system configuration section. ####

# "-I." is needed to find config.h in the build directory.
.c.o:
	$(CC) -c -I. -I$(srcdir) $(DEFS) $(CFLAGS) $< $(OUTPUT_OPTION)

SHELL =	/bin/sh
MAKE  = make
CHMOD =	chmod

XOBJS=amiga.o

COMPATLIBS =
LDLIBS = $(COMPATLIBS) $(XLIBS)

MANPAGES = ksh.1

HDRS =	sh.h table.h expand.h lex.h tree.h tty.h trace.h
SRCS1 =	version.c main.c misc.c trap.c alloc.c io.c \
	syn.c lex.c edit.c emacs.c vi.c history.c tree.c 
SRCS2 =	exec.c jobs.c \
	c_sh.c c_ksh.c c_test.c getopts.c do_ulimit.c \
	var.c table.c eval.c expr.c mail.c sigact.c trace.c
SRCS =	Makefile $(HDRS) $(SRCS1) $(SRCS2) 

OBJS =	version.o main.o misc.o \
	syn.o lex.o edit.o emacs.o vi.o tree.o \
	exec.o jobs.o trap.o \
	c_sh.o c_ksh.o c_test.o \
	do_ulimit.o getopts.o expr.o history.o \
	var.o table.o alloc.o io.o eval.o mail.o sigact.o trace.o $(XOBJS)

ksh: $(OBJS) 
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)

install: ksh
	$(INSTALL_PROGRAM) -c ksh $(bindir)/ksh
	$(INSTALL_PROGRAM) -c ksh $(bindir)/sh

clean:
	rm -f *.o *.out core

clobber: clean
	rm -f ksh

link:
	($(SHELL) ../std/mklinks ../std/h stdh.h)
	

depend:
	makedepend $(CFLAGS) $(SRCS1) $(SRCS2)

.c.s:
	$(CC) $(CFLAGS) -S -o $@ $<

asms: $(OBJS:.o=.s)


# DO NOT DELETE THIS LINE -- make depend depends on it.
# If this runs make out of memory, delete /usr/include lines.
alloc.o: alloc.c
alloc.o: config.h
alloc.o: sh.h
alloc.o: stdh.h
alloc.o: trace.h
c_ksh.o: c_ksh.c
c_ksh.o: config.h
c_ksh.o: sh.h
c_ksh.o: stdh.h
c_ksh.o: table.h
c_ksh.o: trace.h
c_sh.o: c_sh.c
c_sh.o: config.h
c_sh.o: lex.h
c_sh.o: sh.h
c_sh.o: stdh.h
c_sh.o: table.h
c_sh.o: trace.h
c_sh.o: tree.h
c_test.o: c_test.c
c_test.o: config.h
c_test.o: sh.h
c_test.o: stdh.h
c_test.o: trace.h
edit.o: config.h
edit.o: edit.c
edit.o: edit.h
edit.o: lex.h
edit.o: sh.h
edit.o: stdh.h
edit.o: trace.h
edit.o: tty.h
emacs.o: config.h
emacs.o: edit.h
emacs.o: emacs.c
emacs.o: expand.h
emacs.o: lex.h
emacs.o: sh.h
emacs.o: stdh.h
emacs.o: table.h
emacs.o: trace.h
emacs.o: tree.h
eval.o: config.h
eval.o: eval.c
eval.o: expand.h
eval.o: lex.h
eval.o: sh.h
eval.o: stdh.h
eval.o: table.h
eval.o: trace.h
eval.o: tree.h
exec.o: config.h
exec.o: exec.c
exec.o: lex.h
exec.o: sh.h
exec.o: stdh.h
exec.o: table.h
exec.o: trace.h
exec.o: tree.h
expr.o: config.h
expr.o: expr.c
expr.o: sh.h
expr.o: stdh.h
expr.o: table.h
expr.o: trace.h
getopts.o: config.h
getopts.o: getopts.c
getopts.o: sh.h
getopts.o: stdh.h
getopts.o: table.h
getopts.o: trace.h
history.o: config.h
history.o: history.c
history.o: lex.h
history.o: sh.h
history.o: stdh.h
history.o: trace.h
io.o: config.h
io.o: io.c
io.o: sh.h
io.o: stdh.h
io.o: trace.h
jobs.o: config.h
jobs.o: jobs.c
jobs.o: sh.h
jobs.o: stdh.h
jobs.o: trace.h
jobs.o: tree.h
lex.o: config.h
lex.o: expand.h
lex.o: lex.c
lex.o: lex.h
lex.o: sh.h
lex.o: stdh.h
lex.o: table.h
lex.o: trace.h
lex.o: tree.h
mail.o: config.h
mail.o: mail.c
mail.o: sh.h
mail.o: stdh.h
mail.o: table.h
mail.o: trace.h
main.o: config.h
main.o: lex.h
main.o: main.c
main.o: sh.h
main.o: stdh.h
main.o: table.h
main.o: trace.h
main.o: tree.h
misc.o: config.h
misc.o: expand.h
misc.o: misc.c
misc.o: sh.h
misc.o: stdh.h
misc.o: trace.h
sigact.o: sigact.h sigact.c
table.o: config.h
table.o: sh.h
table.o: stdh.h
table.o: table.c
table.o: table.h
table.o: trace.h
trace.o: trace.c
trap.o: config.h
trap.o: sh.h
trap.o: stdh.h
trap.o: trace.h
trap.o: trap.c
tree.o: config.h
tree.o: sh.h
tree.o: stdh.h
tree.o: trace.h
tree.o: tree.c
tree.o: tree.h
do_ulimit.o: config.h
do_ulimit.o: sh.h
do_ulimit.o: stdh.h
do_ulimit.o: trace.h
do_ulimit.o: do_ulimit.c
var.o: config.h
var.o: expand.h
var.o: sh.h
var.o: stdh.h
var.o: table.h
var.o: trace.h
var.o: var.c
version.o: config.h
version.o: sh.h
version.o: stdh.h
version.o: trace.h
version.o: version.c
vi.o: config.h
vi.o: edit.h
vi.o: expand.h
vi.o: lex.h
vi.o: sh.h
vi.o: stdh.h
vi.o: table.h
vi.o: trace.h
vi.o: tree.h
vi.o: vi.c
# WARNING: Put nothing here or make depend will gobble it up!
