#
# Turbo C 2.0 Makefile for EVAL.EXE
#
# Uses small model and the emulator math switch
#
CC=tcc
CCFLAGS=-c -f
LN=tcc
LNFLAGS=-f
LIBC=
LIBM=
EXE=.exe
OBJ=.obj
HEADER=eval.h
OBJFILES=eval$(OBJ) funcs$(OBJ) parse$(OBJ) estack$(OBJ) base$(OBJ) \
         bitwise$(OBJ) etable$(OBJ) help$(OBJ) emath$(OBJ)

.c$(OBJ):
    $(CC) $(CCFLAGS) $*.c

$(OBJFILES):  $(HEADER)

eval$(EXE):  $(OBJFILES)
    $(LN) $(LNFLAGS) $(OBJFILES) $(LIBM) $(LIBC)

eval:   eval$(EXE)

all:    eval$(EXE)
