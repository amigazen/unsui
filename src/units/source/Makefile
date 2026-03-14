# makefile for SAS/C 6.51 for Amiga.

CFLAGS=DEFINE "UNITSFILE=*"units.lib*""

/units:	units.c 
	sc $(CFLAGS) units.c getopt.c PNAME=/units LINK MATH=S STACKEXTEND

clean:
	delete *.o *.lnk
