#
#   FILE
#	makefile for xsh
#



xsh:	xsh.o
	SLINK TO xsh FROM xsh.o

xsh.o:	xsh.c
	sc NOSTKCHK xsh.c

xsh11.lha:
	lha a xsh11.lha xsh xsh.doc xsh.c xshell.el xshell.elc Makefile

xsh100.lha:
	lha a xsh100.lha xsh xsh.doc xsh.c wshell.el wshell.elc Makefile

