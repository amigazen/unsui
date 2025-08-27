# This file makes routines that are used everywher.
# 

SITEDIR=..
CC=gcc.020 -Wall
OPT=-O3
CFLAGS=$(OPT) -I$(SITEDIR)  -DTeX
#-fomit-frame-pointer -fcombine-regs -fstrength-reduce
LDFLAGS=$(OPT)
SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .c
.c.o:
		$(CC) $(CFLAGS) -c $*.c

default: extra.o

triptrap: all
all: main.o extra.o

main.o: main.c extra.h $(SITEDIR)/site.h

extra.o: extra.c $(SITEDIR)/site.h extra.h


# We don't make `texmf.o', since the TeX and Metafont need to use
# different names, hence they need different .o files.


clean:
	rm -f *.o

veryclean: clean
