.SUFFIXES:	.c .s

CFLAGS =	+l
LIBS =		-lc32

SRCS =	append.c catsub.c ckglob.c deflt.c del.c docmd.c doglob.c\
	doprnt.c doread.c dowrite.c ed.c egets.c find.c getfn.c getlst.c\
	getnum.c getone.c getptr.c getrhs.c gettxt.c ins.c maksub.c move.c\
	optpat.c set.c setbuf.c subst.c getpat.c matchs.c amatch.c\
	unmkpat.c omatch.c makepat.c bitmap.c dodash.c esc.c system.c\
	amiga.c

OBJS =	append.o catsub.o ckglob.o deflt.o del.o docmd.o doglob.o\
	doprnt.o doread.o dowrite.o ed.o egets.o find.o getfn.o getlst.o\
	getnum.o getone.o getptr.o getrhs.o gettxt.o ins.o maksub.o move.o\
	optpat.o set.o setbuf.o subst.o getpat.o matchs.o amatch.o\
	unmkpat.o omatch.o makepat.o bitmap.o dodash.o esc.o system.o\
	amiga.o

ed:	$(OBJS)
	ln -o ed $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS) ed

shar:
	shar Makefile ed.h tools.h $(SRCS) >ed.shar
