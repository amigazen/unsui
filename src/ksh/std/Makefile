#
# Makefile for the compatibility libraries
#
# $Id: Makefile,v 1.3 93/05/05 21:17:35 sjg Exp $
#

SHELL = /bin/sh
MAKE  = make

#CONFIG = -D_SYSV
CONFIG = -D_BSD

LN = ln

libs:	h libstdc.a libposix.a

h:
	mkdir h
	( cd stdc ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' 'LN=$(LN)' clobber link )
	( cd posix  ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' 'LN=$(LN)' clobber link )
	( cd ../sh ;  $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' 'LN=$(LN)' link )

libstdc.a: FORCED
	( cd stdc ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' all )

libposix.a: FORCED
	( cd posix ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' all )

clean:
	-rm -f *.out
	( cd stdc ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' $@ )
	( cd posix  ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' $@ )

clobber:
	-rm -rf h *.a
	( cd stdc ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' $@ )
	( cd posix  ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' $@ )

FORCED:
