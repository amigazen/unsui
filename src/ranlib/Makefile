
ARPATH		= ../ar

vpath %.h $(ARPATH)
vpath %.c $(ARPATH)

CC		= gcc

CFLAGS		= -I$(ARPATH) -D__progname=\"ranlib\" -O2 -fbaserel -msmall-code -std=c89 -pedantic

LDFLAGS		= -resident

LDLIBS		=

OBJS		= archive.o build.o misc.o ranlib.o touch.o err.o#strerror.o

ranlib:		$(OBJS)
		$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
