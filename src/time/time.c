;/* time - compiles with SAS/C 5.10a by executing this file
lc -cfis -v -b0 -j73 -O -M time
blink time.o to time lib lib:lcnb.lib lib:amiga.lib SC ND VERBOSE
quit ;*/
/*
*	time - measure time command takes to complete.
*
*	Usage: time <command-and-its-args> 
*
*	Martin W. Scott, 4/92.
*/
#include <exec/types.h>
#include <dos/dos.h>
#include <dos/rdargs.h>

#include <proto/exec.h>		/* use PRAGMAS */
#include <proto/dos.h>

void main(void);
void PrintDateStamp(struct DateStamp *);
void SubDateStamp(struct DateStamp *, struct DateStamp *);


char version_str[] = "$VER: time v1.0";
struct DosLibrary *DOSBase;
struct RDArgs rdargs;

void main(void)		/* entry: start command and print how long it took */
{
	struct DateStamp before, after;
	struct RDArgs *readargs;
	LONG rargs[1];

	if (DOSBase = (struct DosLibrary *)OpenLibrary("dos.library", 37L))
	{
		rdargs.RDA_ExtHelp = "Usage: time <command>\n prints time command takes to execute, in form HH:MM:SS.SS\n";
		if (readargs = ReadArgs("COMMAND/A/F", rargs, &rdargs))
		{
			DateStamp(&before);
			System((char *)rargs[0], NULL);
			DateStamp(&after);
			SubDateStamp(&after,&before);
			PrintDateStamp(&after);
		}
		else PrintFault(IoErr(), "time");

		CloseLibrary(DOSBase);
	}

} /* main */

#include <stdarg.h>
LONG __stdargs DosPrintf(char *s, ...);

LONG __stdargs DosPrintf(char *s, ...)		/* stub for VPrintf */
{
	va_list ap;
	va_start(ap,s);
	return VPrintf(s, (long *)ap);
}


#define TICKS_PER_MINUTE	(TICKS_PER_SECOND*60)
#define MINUTES_PER_DAY		(60*24)


void PrintDateStamp(struct DateStamp *ds)	/* print datestamp as HH:MM:SS.SS */
{
	LONG h,m,s,hs;

	h = ds->ds_Days*24 + ds->ds_Minute / 60;		/* hours */
	m = ds->ds_Minute % 60;			/* minutes */
	s = ds->ds_Tick / TICKS_PER_SECOND;		/* seconds */
							
	hs = (100 *					/* hundreths of a second */ 
		(ds->ds_Tick % TICKS_PER_SECOND)) / TICKS_PER_SECOND;

	DosPrintf("time %ld:%02ld:%02ld.%02ld\n", h,m,s,hs);
}

void SubDateStamp(struct DateStamp *ds, struct DateStamp *amount)
{
	/* subtract amount from ds */

	if (ds->ds_Tick < amount->ds_Tick)
	{
		ds->ds_Tick += TICKS_PER_MINUTE;
		ds->ds_Minute--;
	}

	if (ds->ds_Minute < amount->ds_Minute)
	{
		ds->ds_Minute += MINUTES_PER_DAY;
		ds->ds_Days--;
	}

	ds->ds_Days -= amount->ds_Days;
	ds->ds_Minute -= amount->ds_Minute;
	ds->ds_Tick -= amount->ds_Tick;
}