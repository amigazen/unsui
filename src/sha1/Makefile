#
# $Id: Makefile,v 1.3 2005/09/18 21:03:17 craz1 Exp $
#

OPENSFV_VERSION = 1.0b4

CC = gcc -mcrt=newlib
CFLAGS = -Wall -O3
SRCS = functions.c crc32.c sfv.c dir.c md5.c md5file.c sha1.c sha1file.c
OBJS = $(SRCS:.c=.o)
LDFLAGS = -lgen -lunix
#STRIP = strip -s -R.note -R.comment
STRIP = true

## CPU OPTIMIZATION ##
######################
# Intel x64
# CFLAGS += -m64 -march=nocona

# AMD-64
# CFLAGS += -m64 -march=athlon64

# Intel x32
# CFLAGS += -m32 -march=pentium4

# AMD x32
# CFLAGS += -m32 -march athlon-xp

#####################

# do not change the following line
CDEFS  = -DOPENSFV_VERSION=\"$(OPENSFV_VERSION)\"

# if you are compiling this under Cygwin, uncomment the following line
#CDEFS += -DCYGWIN


all: opensfv

%.o: %.c
	$(CC) $(CDEFS) $(CFLAGS) -c $< -o $@

opensfv: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@ $(LDFLAGS)

strip:
	$(STRIP) opensfv

clean:
	rm -f *.[o] *~ opensfv
