
OBJS = scc.o sc_rexx.o output.o
LIBS = -lamiga
CFLAGS = -noixemul -O2 -fbaserel -msmall-code -fomit-frame-pointer
CC=gcc

all:	scc

scc:	$(OBJS) makefile
	$(CC) $(CFLAGS) $(OBJS) -o scc

clean:
	rm *.o scc
