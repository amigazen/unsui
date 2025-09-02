
CC		= gcc

CFLAGS		= -I. -D__progname=\"ar\" -O2 -fbaserel -msmall-code -std=c89 -pedantic

LDFLAGS		= -resident

LDLIBS		=

OBJS		= append.o ar.o archive.o contents.o delete.o extract.o \
		  misc.o move.o print.o replace.o strmode.o err.o#strerror.o

ar:		$(OBJS)
		$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
