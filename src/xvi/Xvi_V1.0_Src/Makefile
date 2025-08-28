########################################################################
#                                                                      #
# Makefile for SAS "C" Version 6.50 compiler.  Dan Schmelzer 10/09/94. #
#                                                                      #
########################################################################

# DESCRIPTION OF DEFINES:
#
# "def AMIGA" is needed to compile for AMIGA required code changes.
#
# "def OPTIMIZE_AMIGA" is not required but optimizes the generic code
# for better performance in the AMIGA environment.
#
# "def DEBUG=1" thru "def DEBUG=4" is just for debugging Amiga specific
# routines with 1 being minimal debug information and 4 being lots.
#
# "def NO_OUT_BUFFERING" specifies that output buffering is turned off.
# This is useful to see all the gory details of output when debugging
# but slows down output a little bit if left in.
#
# "def OLD_AMIGA_OS" indicates building for Amiga OS version 1.3.  If
# this define is missing it is assumed we are building for OS 2.0 or
# higher.

CFLAGS= def AMIGA def OPTIMIZE_AMIGA Ignore=all optimize
#CFLAGS= def AMIGA def OPTIMIZE_AMIGA Ignore=all
#CFLAGS= def AMIGA def OPTIMIZE_AMIGA Ignore=all profile
#CFLAGS= def AMIGA Ignore=all
#CFLAGS= def AMIGA def OPTIMIZE_AMIGA def NO_OUT_BUFFERING Ignore=all
#CFLAGS= def AMIGA def NO_OUT_BUFFERING Ignore=all
#CFLAGS= def AMIGA Ignore=all debug=fullflush
#CFLAGS= def AMIGA def OPTIMIZE_AMIGA def DEBUG=1 Ignore=all debug=fullflush
#CFLAGS= def AMIGA def OPTIMIZE_AMIGA def DEBUG=2 Ignore=all debug=fullflush
#CFLAGS= def AMIGA def DEBUG=3 def NO_OUT_BUFFERING Ignore=all debug=fullflush
#CFLAGS= def AMIGA def DEBUG=4 def NO_OUT_BUFFERING Ignore=all debug=fullflush

# To build a version of Xvi to run under Amiga OS version 1.3 use:
#CFLAGS= def AMIGA def OLD_AMIGA_OS def OPTIMIZE_AMIGA Ignore=all optimize
#CFLAGS= def AMIGA def OLD_AMIGA_OS def OPTIMIZE_AMIGA Ignore=all
#CFLAGS= def AMIGA def OLD_AMIGA_OS def OPTIMIZE_AMIGA def DEBUG=1 Ignore=all debug=fullflush
#CFLAGS= def AMIGA def OLD_AMIGA_OS def OPTIMIZE_AMIGA def DEBUG=2 Ignore=all debug=fullflush


RM= delete

INCLUDE= ascii.h param.h ptrfunc.h regexp.h regmagic.h xvi.h virtscr.h

OBJ= alloc.o amiga.o ascii.o buffers.o cmdline.o cursor.o defscr.o \
     edit.o events.o ex_cmds1.o ex_cmds2.o fileio.o \
     find.o flexbuf.o map.o mark.o misccmds.o mouse.o movement.o \
     normal.o param.o pipe.o preserve.o ptrfunc.o \
     regexp.o screen.o search.o signal.o startup.o status.o \
     tags.o undo.o version.o windows.o yankput.o

xvi: $(OBJ) 
	slink <with <(xvi.lnk)
# Suitable commands to create the link file need to go here:
from SC:lib/c.o $(OBJ)
to xvi
lib SC:lib/sc.lib SC:lib/scm.lib
map xvi.map x
<

clean:
	$(RM) $(OBJ) xvi.lnk

amiga.o: amiga.c amiga.h xvi.h
	sc $(CFLAGS) $<

# End of file. #########################################################
