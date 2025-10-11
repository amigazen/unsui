# PD Bourne/Korn Shell
# $Id: Makefile,v 1.5 93/05/08 15:59:28 sjg Exp $

#### Start of system configuration section. ####

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

#### End of system configuration section. ####

SHELL =		/bin/sh
MAKE =		make

# AmigaOS specific configuration
CC = m68k-amigaos-gcc -resident -O2
CONFIG = -D_BSD -Damigados -DUSE_TRACE -DDEBUG_AFREE

STD = ../std

CCOPTS = $(DBG)
CFLAGS = $(CCOPTS) $(CONFIG)

LDOPTS = -s
LDFLAGS = $(LDOPTS) -L$(STD)

#COMPATLIBS = -lstdc # -lposix
#OTHERLIBS = -lc_s
#OTHERLIBS = -ldirent
LDLIBS = $(COMPATLIBS) $(OTHERLIBS)

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
	var.o table.o alloc.o io.o eval.o mail.o sigact.o trace.o amiga.o

all: ksh

ksh: $(OBJS) 
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)

install: ksh
	$(INSTALL_PROGRAM) -c ksh $(bindir)/ksh
	$(INSTALL_DATA) -c $(srcdir)/ksh.1 $(mandir)/ksh.1

clean:
	rm -f *.o *.out core

clobber: clean
	rm -f ksh

edit.o emacs.o:	edit.h
