# This file makes TeX in its various incarnations.
#

SITEDIR=..
CC=gcc
OPT=-g
CFLAGS=$(OPT) -I$(SITEDIR)
LDFLAGS=$(OPT)

objs=tex0.o tex1.o tex2.o tex3.o tex4.o tex5.o tex6.o tex7.o tex8.o
iobjs=iextra.o initex.o $(objs)
vobjs=extra.o itex.o $(objs)

makeargs=SITEDIR="$(SITEDIR)" CC="$(CC)" OPT="$(OPT)" LDFLAGS="$(LDFLAGS)"

.SUFFIXES:
.SUFFIXES: .o .c
.c.o:
		$(CC) $(CFLAGS) -c $*.c


default:	all

all:		tex.p tex0.c initex virtex

triptrap:	triptex

initex:		$(iobjs)
		$(CC) $(LDFLAGS) -o initex $(iobjs)

virtex:		$(vobjs)
		$(CC) $(LDFLAGS) -o virtex $(vobjs)

run-triptrap:
		copy TeXtrip/trip.tex ""
		/texware/pltotf TeXtrip/trip.pl trip.tfm
		triptex < TeXtrip/trip1.in
		diff TeXtrip/tripin.log trip.log
		triptex < TeXtrip/trip2.in > trip.fot
		diff TeXtrip/trip.log trip.log
		diff TeXtrip/trip.fot trip.fot
		/texware/dvitype trip.dvi < TeXtrip/dvitype.in
		diff TeXtrip/trip.typ dvitype.out

# The `ini_to_trip' script changes texd.h, instead of the change file, so
# retangling is not necessary.  After changing constants, making a
# triptex means making an initex.
# 
triptex:	
		execute initotrip
		make $(makeargs) initex
		rename initex triptex


# tangle produces tex.p and tex.pool.
# 

# The convert script produces tex[0-8].c, itex.c, coerce.h, and texd.h.
# The (hand-coded) memory.h is included in one of tex?.c files; it isn't
# really necessary to `convert' again if it changes, only to recompile
# that file, but describing that relationship is ugly to do.
# (Furthermore, memory.h is stable.)
# 

# The (hand-coded) file extra.c and the (generated) file itex.c have
# #ifdefs on INITEX, so we have to compile them with -DINITEX for initex
# and triptex.
# 
iextra.o:	extra.c texd.h
		copy extra.c iextra.c
		$(CC) $(CFLAGS) -DINITEX -c iextra.c 

initex.o:       itex.c
		copy itex.c initex.c
		$(CC) $(CFLAGS) -DINITEX -c initex.c

tex.p:		tex.web ctex.ch
		/web/tangle tex.web ctex.ch

tex0.c:		tex.p
		convert

extra.o:	texd.h
tex0.o:		tex0.c
tex1.o:		tex1.c
tex2.o:		tex2.c
tex3.o:		tex3.c
tex4.o:		tex4.c
tex5.o:		tex5.c
tex6.o:		tex6.c
tex7.o:		tex7.c
tex8.o:		tex8.c

