CC = lc
CFLAGS = -O -DNONAMES -DDEFBLOCKING=20

SRCS = buffer.c create.c diffarch.c extract.c getoldopt.c getopt.c list.c \
	port.c tar.c utime.c wildmat.c amiga.c

OBJS = buffer.o create.o diffarch.o extract.o getoldopt.o getopt.o list.o \
	port.o tar.o utime.o wildmat.o amiga.o

all:	tar

tar:	$(OBJS)
	blink with tar.lnk

clean:
	rm *.o tar
