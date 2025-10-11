# PD Bourne/Korn Shell
# NOTE: the are no depencies of .c file on .h files yet.

#SHELL = /bin/sh

# This is for the sun386i your mileage may vary :-)
#CC=scc -ansi -Dsun386 -Dsun -Di386
CC=scc  -resident -O2

#CONFIG= -D_SYSV
CONFIG= -D_BSD -DUSE_TRACE -DDEBUG_AFREE

STD=	../std

#CC = shcc
CCOPTS = $(DBG)
#CFLAGS = $(CCOPTS) -I$(STD)/h $(CONFIG)
CFLAGS = $(CCOPTS) $(CONFIG)

#LDOPTS = -s
LDFLAGS = $(LDOPTS) -L$(STD)

#COMPATLIBS =  -lstdc # -lposix
#OTHERLIBS = -lc_s
#OTHERLIBS = -ldirent
LDLIBS = $(COMPATLIBS) $(OTHERLIBS)

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

ksh: $(OBJS) 
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)

clean:
	rm -f *.o *.out core

clobber: clean
	rm -f ksh


edit.o emacs.o:	edit.h
