CC =		cc
CFLAGS =	-O
LDFLAGS =
OBJ =		itoeng.o tpr0.o tpr1.o

tpr :		$(OBJ)
		$(CC) $(LDFLAGS) -o tpr $(OBJ)

itoeng.o :	itoeng.c tpr.h
		$(CC) -c $(CFLAGS) itoeng.c

tpr0.o :	tpr0.c tpr.h
		$(CC) -c $(CFLAGS) tpr0.c

tpr1.o :	tpr1.c tpr.h
		$(CC) -c $(CFLAGS) tpr1.c
