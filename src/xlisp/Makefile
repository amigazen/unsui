SRC1 =	xlobj.c xllist.c xlcont.c xlbfun.c
SRC2 =	xldmem.c xleval.c xlfio.c xlftab.c xlglob.c xlio.c xlisp.c xljump.c
SRC2a =	xlmath.c xlprin.c xlread.c xlinit.c
SRC3 =	xlstr.c xlsubr.c xlsym.c xlsys.c xldbug.c asstuff.c
SRCS =	$(SRC1) $(SRC2) $(SRC2a) $(SRC3) xlisp.h

OBJS1 =	xlbfun.o xlcont.o xldbug.o xldmem.o xleval.o xlfio.o
OBJS2 =	xlftab.o xlglob.o xlinit.o xlio.o xlisp.o xljump.o xllist.o xlmath.o
OBJS3 =	xlobj.o xlprin.o xlread.o xlstr.o xlsubr.o xlsym.o xlsys.o asstuff.o
OBJS =	$(OBJS1) $(OBJS2) $(OBJS3)

MISC1 =	Makefile fact.lsp init.lsp object.lsp prolog.lsp trace.lsp 
MISC2 =	xlstub.c.NOTUSED 
MISC  =	$(MISC1) $(MISC2)

CFLAGS =	-O
CC  =		cc
LIBS =		-lm

xlisp : $(OBJS)
	cc -o xlisp $(CFLAGS) $(OBJS) $(LIBS)

rcs : $(SRCS)
	rcs -l $?
	touch rcs

lint :
	lint -ach $(SRCS)

new : clean
	rm -f xlisp
	make xlisp

clean :
	rm -f *.o

shar : $(SRCS) $(MISC)
	shar -c -v xlisp.doc > xlisp1.shar
	shar -c -v $(SRC1) > xlisp2.shar
	shar -c -v $(SRC2) > xlisp3.shar
	shar -c -v $(SRC3) $(MISC) > xlisp4.shar


xlbfun.o :	xlbfun.c xlisp.h
		$(CC) -c $(CFLAGS) xlbfun.c

xlcont.o :	xlcont.c xlisp.h
		$(CC) -c $(CFLAGS) xlcont.c

xldbug.o :	xldbug.c xlisp.h
		$(CC) -c $(CFLAGS) xldbug.c

xldmem.o :	xldmem.c xlisp.h
		$(CC) -c $(CFLAGS) xldmem.c

xleval.o :	xleval.c xlisp.h
		$(CC) -c $(CFLAGS) xleval.c

xlfio.o :	xlfio.c xlisp.h
		$(CC) -c $(CFLAGS) xlfio.c

xlftab.o :	xlftab.c xlisp.h
		$(CC) -c $(CFLAGS) xlftab.c

xlglob.o :	xlglob.c xlisp.h
		$(CC) -c $(CFLAGS) xlglob.c

xlinit.o :	xlinit.c xlisp.h
		$(CC) -c $(CFLAGS) xlinit.c

xlio.o :	xlio.c xlisp.h
		$(CC) -c $(CFLAGS) xlio.c

xlisp.o :	xlisp.c xlisp.h
		$(CC) -c $(CFLAGS) xlisp.c

xljump.o :	xljump.c xlisp.h
		$(CC) -c $(CFLAGS) xljump.c

xllist.o :	xllist.c xlisp.h
		$(CC) -c $(CFLAGS) xllist.c

xlmath.o :	xlmath.c xlisp.h
		$(CC) -c $(CFLAGS) xlmath.c

xlobj.o :	xlobj.c xlisp.h
		$(CC) -c $(CFLAGS) xlobj.c

xlprin.o :	xlprin.c xlisp.h
		$(CC) -c $(CFLAGS) xlprin.c

xlread.o :	xlread.c xlisp.h
		$(CC) -c $(CFLAGS) xlread.c

xlstr.o :	xlstr.c xlisp.h
		$(CC) -c $(CFLAGS) xlstr.c

xlstub.o :	xlstub.c xlisp.h
		$(CC) -c $(CFLAGS) xlstub.c

xlsubr.o :	xlsubr.c xlisp.h
		$(CC) -c $(CFLAGS) xlsubr.c

xlsym.o :	xlsym.c xlisp.h
		$(CC) -c $(CFLAGS) xlsym.c

xlsys.o :	xlsys.c xlisp.h
		$(CC) -c $(CFLAGS) xlsys.c

asstuff.o :	asstuff.c
		$(CC) -c $(CFLAGS) asstuff.c
