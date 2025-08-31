SRCS = number.c filter.c getopt.c
OBJS = ff.o number.o filter.o getopt.o
DOCS = ff.1 ff.test
LIBS =
DESTDIR = .
CFLAGS = -O
TEXT = $(HDRS) $(SRCS)

LINT   = /usr/bin/lint -hp
PR     = cpr
SPELL  = sp
SHAR   = shar -a
RCS    = ci -l
CC     = cc

ff :	$(OBJS)
	$(CC) $(CFLAGS) -o ff $(OBJS)

install : ff
	cp -i ff $(DESTDIR)/ff

print :
	@$(PR) ff.c

lint :
	$(LINT) $(TEXT) ff.c

spell :
	seec -cqe ff.c | $(SPELL)

test :
	ff.test

archive : $(DOCS) [Mm]akefile $(TEXT) ff.c
	@$(SHAR) $(DOCS) [Mm]akefile $(TEXT) > archive.1
	@$(SHAR) ff.c > archive.2

clean :
	rm -f *.o core a.out mon.out gmon.out scmon.out

gprof :
	make CFLAGS="$(CFLAGS) -pg"
scprof :
	make CFLAGS="$(CFLAGS) -p" CC=sc

xref : cscope.out
	ccall -dr > xref.r
	ccall -a > xref.a
	touch xref
cscope.out : ff.c
	cscope ff.c

style : style.out
style.out :
	cstyle ff.c > style.out

rcs : RCS
	$(RCS) $(TEXT) ff.c
RCS : $(TEXT) ff.c

ff.1 : ff.c
	@seec -t MANUAL ff.c > ff.1

.PRECIOUS : $(TEXT) $(DOCS) ff.c

ff.o :		ff.c
		$(CC) -c $(CFLAGS) ff.c

number.o :	number.c
		$(CC) -c $(CFLAGS) number.c

filter.o :	filter.c
		$(CC) -c $(CFLAGS) filter.c

getopt.o :	getopt.c
		$(CC) -c $(CFLAGS) getopt.c

