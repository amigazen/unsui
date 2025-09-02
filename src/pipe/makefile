#
# Makefile for pipe
#
# Copyright © 1994 Ville Saari
# All rights reserved
#
# Created: 30-Dec-94
#

CC=        sc
#DEBUG=    dbg=ff def=DEBUG
OPT=       opt opttime optinlocal optdep=5 ign=147
CFLAGS=    $(DEBUG) $(OPT) strmer code=n data=fo parm=r nostkchk \
           nostkext nolink nomodified
LFLAGS=    scode sdata nostartup link

all:       pipe

version.h: pipe.c addtask.a
           incver version.h

pipe.o:    pipe.c version.h makefile
           $(CC) $(CFLAGS) pipe.c

addtask.o: addtask.a makefile
           $(CC) $(CFLAGS) addtask.a

pipe:      pipe.o addtask.o
           $(CC) $(LFLAGS) to pipe pipe.o addtask.o
           protect pipe +p

clean:
           delete \#?.(lnk|o)
