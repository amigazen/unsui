CC = gcc
PROGS = base64
#CFLAGS = -g
CFLAGS = -O -Xlinker -s

all:	$(PROGS)

clean:
	rm -f $(PROGS) *.o *.bak *.shar core *.out

base64: base64.o
	$(CC) base64.o -o base64 $(CFLAGS)

#   Create shell archive

shar:
	shar -a README Makefile base64.1 base64.c >base64.shar

#   Check for lint

lint:
	lint base64.c

#   View manual page

manpage:
	nroff -man base64.1 | $(PAGER)

#   Print manual page

printman:
	ptroff -man base64.1
