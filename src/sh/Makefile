CC =		cc
CFLAGS =
LDFLAGS =
LIBS =

OBJECTS =	command.o execom.o globals.o main.o set.o sub.o

sh :		$(OBJECTS)
		$(CC) -o sh $(LDFLAGS) $(OBJECTS) $(LIBS)

command.o :	command.c
		$(CC) -c $(CFLAGS) command.c

execom.o :	execom.c
		$(CC) -c $(CFLAGS) execom.c

globals.o :	globals.c
		$(CC) -c $(CFLAGS) globals.c

main.o :	main.c
		$(CC) -c $(CFLAGS) main.c

set.o :		set.c
		$(CC) -c $(CFLAGS) set.c

sub.o :		sub.c
		$(CC) -c $(CFLAGS) sub.c
