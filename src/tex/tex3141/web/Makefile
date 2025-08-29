# This file makes TANGLE and WEAVE from their WEB sources.
#

SITEDIR=..
COMP=gcc
OPT=-g
CFLAGS=$(OPT) -Dlink=link_array -I$(SITEDIR)
LDFLAGS=

default:	all

.SUFFIXES: .c .o .ch .p

.p.c:
		convert $*.p $*.c

.ch.p:
		tangle $*.web $*.ch

.c.o:
		$(COMP) $(CFLAGS) -c $*.c

all:		bootstrap tangle weave

# Tangle:
# 
tangle:		tangle.o tanglext.o
		$(COMP) $(LDFLAGS) -o tangle tangle.o tanglext.o

tangle.o: 	tangle.c 

tangle.c: 	tangle.p web.defines

tangle.p:	tangle.web tangle.ch
		tangle tangle.web tangle.ch		

bootstrap:	tangleboot.o tanglext.o
		$(COMP) $(LDFLAGS) -o tangle tangleboot.o tanglext.o

tanglext.o: 	tanglext.c 


# Weave:
# 
weave: 		weave.o weavext.o
		$(COMP) $(LDFLAGS) -o weave weave.o weavext.o

weave.o: 	weave.c 

weave.c:	weave.p web.defines

weave.p: 	weave.web weave.ch

weavext.o:	weavext.c 


