# Makefile for TeX to C conversion programs.
#

SITEDIR=..
CC=gcc
OPT=-g -DFLEX
CFLAGS=$(OPT) -I$(SITEDIR)
LDFLAGS=$(OPT)
SHELL=/bin/sh

# You can substitute `bison -y' for `yacc' if you like.
YACC=bison -y
# Note: If you're going to use flex, compile web2cl.c with "-DFLEX".
LEX=flex

all:		web2c fixwrites splitup regfix

web2c:		web2cy.o web2cl.o web2c.o
		$(CC) $(LDFLAGS) -o web2c web2c.o web2cy.o web2cl.o 

web2cl.o:	web2cl.c web2c.h web2cy.h symtab.h
		$(CC) $(CFLAGS) -c web2cl.c

web2cy.o:	web2cy.c web2c.h symtab.h 
		$(CC) $(CFLAGS) -c web2cy.c

web2c.o:	web2c.c symtab.h web2cy.h 
		$(CC) $(CFLAGS) -c web2c.c


web2cl.c:	web2c.lex
		$(LEX) web2c.lex
		rename lex.yy.c web2cl.c

web2cy.c:	web2c.yacc
		$(YACC) -d web2c.yacc
		rename y.tab.c web2cy.c
	        copy y.tab.h web2cy.h

fixwrites:	fixwrites.c
		$(CC) $(CFLAGS) -o fixwrites fixwrites.c

splitup:	splitup.c 
		$(CC) $(CFLAGS) -o splitup splitup.c

regfix:		regfix.c 
		$(CC) $(CFLAGS) -o regfix regfix.c

.c.o:
		$(CC) $(CFLAGS) -c $*.c