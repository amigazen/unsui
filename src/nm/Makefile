
CC		= gcc

CFLAGS		= -O2 -fbaserel -msmall-code -std=c89 -pedantic

LDFLAGS		= -resident

LDLIBS		=

OBJS		= nm.o#strerror.o

nm:		$(OBJS)
		$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
