# This file makes TeX in its various incarnations.
#
# The file ../common/texmf.h uses the symbol `TeX' to define certain
# things that are different for TeX and Metafont, hence the addition to
# CFLAGS.
# 

SITEDIR=..
CC=gcc.020 -Wall
OPT=-O3
NO_OPT=-I$(SITEDIR)/common -I$(SITEDIR)/gcclib -I$(SITEDIR) -DTeX
#	-fomit-frame-pointer -fcombine-regs -fstrength-reduce
CFLAGS=$(OPT) $(NO_OPT)

LDFLAGS=-u __doprnt -liio# force inclusion of fixnum printf lib only
# LDFLAGS=-lcposix
SHELL=/bin/sh


makeargs=SITEDIR="$(SITEDIR)" CC="$(CC)" OPT="$(OPT)" LDFLAGS="$(LDFLAGS)"

.SUFFIXES:
.SUFFIXES: .o .c .s
.c.o:
	$(CC) $(CFLAGS) -c $*.c

.s.o:
	$(CC) $(CFLAGS) -S $*.c


# Routines used everywhere.
# 
#commondefines=../common/common.defines ../common/texmf.defines
commonh=../common/extra.h
commono=../common/extra.o

# Routines used in both TeX and Metafont.
# 
extrac=../common/texmf.c
extrah=../common/texmf.h
extrao=texmf.o

# Routines used in TeX, Metafont, and BibTeX.
# 
fileioc=../common/fileio.c
fileioh=../common/fileio.h
fileioo=fileio.o

# We don't add `$(commono)' here, since it doesn't depend on anything in
# this directory, and we write a dependency for `$(objs)' below.
# 
objs1=stat.o unx2dos.o math.o buildbox.o gettoken.o arith.o mainctrl.o filename.o
objs2=$(fileioo) shipout.o scanning.o mltex.o help.o overflow.o getmem.o
objs3=tex0.o tex1.o tex2.o tex3.o readfont.o tex4.o tex5.o tex6.o tex7.o tex8.o
objs=$(objs1) $(objs2) $(objs3)
iobjs=initprim.o ilinebrk.o idump.o iextra.o initex.o initini.o $(objs)
vobjs=linebrk.o dump.o texextra.o itex.o init.o $(objs)

default:	all

all:		initex.ttp virtex.ttp
triptrap:	triptex

tstcnf.ttp:	tstcnf.o initini.o $(commono) $(fileioo)
		$(CC) -o tstcnf.ttp initini.o tstcnf.o $(fileioo) $(commono) $(LDFLAGS)

tstcnf.o:	tstcnf.c $(extrah) $(commonh)
		$(CC) $(CFLAGS) -I. -c tstcnf.c


initex.ttp:	$(iobjs) $(commono) banner.o
		$(CC) -o initex.ttp banner.o $(iobjs) $(commono) $(LDFLAGS)

virtex.ttp:	$(vobjs) $(commono) banner.o
		$(CC) -o virtex.ttp banner.o $(vobjs) $(commono) $(LDFLAGS)

run-trip:
		rm -f trip.tex
		-ln TeXtrip/trip.tex .
		-pltotf TeXtrip/trip.pl trip.tfm
		-$(SHELL) -c '. ./tripenv; \
                  ./triptex < TeXtrip/trip1.in > /dev/null 2>&1'
		-diff TeXtrip/tripin.log trip.log
		-$(SHELL) -c '. ./tripenv; \
                  ./triptex < TeXtrip/trip2.in > trip.fot'
		-diff TeXtrip/trip.log trip.log
		-diff TeXtrip/trip.fot trip.fot
		$(SHELL) -c '. ./tripenv; \
		dvitype trip.dvi < TeXtrip/dvitype.in > trip.typ'
		-diff TeXtrip/trip.typ trip.typ

# The (hand-coded) file $(extrac) and the (generated) file itex.c have
# #ifdefs for INITEX, so we compile them differently.
# 
iextra.o:	$(extrac) texd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -I. -DINITEX -DINI -c $(extrac) -o iextra.o
#		mv $(extrao) iextra.o

iextra.s:	$(extrac) texd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -I. -DINITEX -DINI -S $(extrac) -o iextra.s

initex.o:       itex.c texd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -DINITEX -c itex.c -o initex.o

initex.s:       itex.c texd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -DINITEX -S itex.c -o initex.s

ilinebrk.o:	linebrk.c texd.h
		$(CC) $(CFLAGS) -DINITEX -c linebrk.c -o ilinebrk.o

ilinebrk.s:	linebrk.c texd.h
		$(CC) $(CFLAGS) -DINITEX -S linebrk.c -o ilinebrk.s

initprim.o:	initprim.c texd.h
		$(CC) $(CFLAGS) -DINITEX -c initprim.c

initini.o:      init.c texd.h
		$(CC) $(CFLAGS) -DINITEX -c init.c -o initini.o

idump.o:	dump.c texd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -DINITEX -c dump.c -o idump.o

idump.s:	dump.c texd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -DINITEX -S dump.c -o idump.s

$(objs):	texd.h
texextra.o:	$(extrac) texd.h
		$(CC) $(CFLAGS) -I. -c $(extrac) -o texextra.o
#		mv $(extrao) texextra.o

$(fileioo):	$(fileioc) $(fileioh)
		$(CC) $(CFLAGS) -I. -c $(fileioc)

stat.o:		../gcclib/stat.c
		$(CC) $(CFLAGS) -I. -c $*.c

unx2dos.o:	../gcclib/unx2dos.c
		$(CC) $(CFLAGS) -I. -c $*.c

banner.o:	#touch-banner
		$(CC) $(CFLAGS) -I. -c banner.c

touch-banner:

arith.o:	arith.c
linebrk.o:	linebrk.c
shipout.o:	shipout.c
gettoken.o:	gettoken.c
scanning.o:	scanning.c
buildbox.o:	buildbox.c
math.o:		math.c
mltex.o:	mltex.c
help.o:		help.c
overflow.o:	overflow.c
getmem.o:	getmem.c


init.o:		init.c
dump.o:		dump.c
tex0.o:		tex0.c
tex0b.o:	tex0b.c
tex1.o:		tex1.c
tex2.o:		tex2.c
tex3.o:		tex3.c
filename.o:	filename.c
readfont.o:	readfont.c
#		$(CC) $(NO_OPT) -c $*.c

tex4.o:		tex4.c
tex5.o:		tex5.c
tex6.o:		tex6.c
tex7.o:		tex7.c
tex8.o:		tex8.c
mainctrl.o:	mainctrl.c

clean:
		rm -f stamp-* *.o triptex initex virtex
		rm -f texd.h coerce.h *.log *.fmt *.aux
		rm -f 8terminal.tex trip.* tripos.tex dvitype.out

veryclean:	clean
		rm -f tex?.c itex.c tex.p tex.pool texput.*
		rm -f \#*\# *~ *.bak *.ckp core
