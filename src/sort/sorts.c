/*
 * Sort utility subroutines.
 */

#ifdef	DOCUMENTATION

title	sorts	Sort Utility Subroutines
index		Sort Utility Subroutines

synopsis
	.nf
		sorta(record)
		char		*record;
	.s
		sorto(buffer)
		char		**buffer;
	.f

description

	Sorta() and sorto() comprise a general-purpose C-callable
	sort utility.  There are no restrictions on file or item
	size.  The routines are used as follows:
	.lm +8
	.s.i -4;sorta(record)
	.s
	Add the named record to the data to be sorted.
	After adding the last record, execute "sorta(NULL)"
	to terminate the data.
	.s.i -4;sorto(buffer)
	.s
	Sort output: move the next (sorted) record into the buffer.
	Return "buffer" or NULL when all records have been returned.
	.s.lm -8
	The routines are meant to be used in the following manner:
	.s.nf
		while (next_datum()) {
			sorta(datum);
		}
		sorta(NULL);
		while (sorto(buffer)) {
			process(buffer);
		}
	.s.f
	After sorto() returns NULL, sorta() may be called to start a new
	sort sequence.
	.s
	Data is normally sorted in ascending Ascii order, using the
	entire record as the key.  If this is not satisfactory, the
	following global symbols may be used to modify the sort:
	.lm +8
	.s.i -4;extern int sort__l;
	.s
	This value defines the maximum record length.  It may be
	changed before calling sorta() for the first
	time.
	.s.i -4;extern int (*sort__c)();
	.s
	This defines the comparison function which, by default, is strcmp().
	To call a user-provided function, the main program should contain:
	.s.nf
		extern int (*sort_c)();
		...
		main()
		{
			extern int	myfun();
	
			(*sort_c)() = _&myfun;
			...
		}
		...
		int myfun(record1, record2)
		char	*record1, *record2;
		/*
		 * Compare records, returning
		 *  -1	record1  < record2
		 *   0	record1 == record2
		 *  +1	record1  > record2
		 */
		{
			...
		}
	.f
	.s.i -4;extern int sort__r;
	.s
	This flag reverses the sense of the sort.  Thus, to sort in
	reverse alphanumeric order, the main program should contain:
	.s.nf
		extern int sort_r;
		...
		main()
		{
			...
			sort_r = 1;
		}
	.f
	.s.i -4;extern char *sort__f;
	.s
	This names the sort work file and may be changed if, for example,
	the file should be written to a private disk.  It is used
	as follows:
	.s.nf
		extern char *sort_f;
		...
		main()
		{
			...
			sort_f = "myfile.tmp";
		}
	.f
	.lm -8

diagnostics

	.lm +8
	.s.i -8;SORTS-E-cannot create temp. file "filename"
	.s
	The required file cannot be created in the current directory.
	.s.i -8;SORTS-E-out of main memory.
	.s
	The program ran out of main memory.  Sorts may be optionally
	compiled to dump internal tables (run tables) on this error.
	.s.i -8;SORTS-E-Error writing temp. file
	.s
	An occurred when writing the temp. file.  It is probably
	"out of space on the disk".
	.s.i -8;SORTS-F-Can't reopen temp. file
	.s.i -8;SORTS-F-Empty run
	.s.i -8;SORTS-F-Unexpected eof
	.s.lm -8
	All error are fatal.  "-E" errors are probably correctable by the
	user.  "-F" errors are serious problems.  If the user program
	defined its own comparison function, that should be checked
	for consistancy.

author

	David Conroy, Martin Minow
	.s
	Revised by Bob Denny and Tim Coad

bugs

#endif

#include <stdio.h>

#define	RUNSIZE		512
#define	STACKSIZE	10		/* Log2(RUNSIZE) + 1		*/
#define	TEMP		"sort.tmp"

typedef struct	run {
	struct	run *r_rp;
	long	r_seek;
	int	r_size;
} RUN;

typedef struct	heap {
	struct	run *h_rp;
	char	*h_lp;
} HEAP;

typedef struct	stack {
	int	rght;
	int	lft;
} STACK;

static	RUN		*curr_run	= NULL;
static	RUN		*first_run	= NULL;
static	RUN		*last_run	= NULL;
static	char	**line;
static	FILE	*tfp			= NULL;
static	HEAP		*heap;
static	RUN		*run_pointer;
static	HEAP		*heap_pointer;
static	STACK		stack[STACKSIZE];
static	STACK		*stackptr = &stack[STACKSIZE-1];

static	int	nline	=	0;
static	int	nruns	=	0;

/*
 * The following may be changed by the caller to modify the sort
 */

extern	int	sort_l;		/* Maximum length			*/
extern	int	(*sort_c)();	/* Sort function			*/
extern	int	sort_r;		/* Reverse order			*/
extern	char	*sort_f;	/* Sort file name			*/

extern	int	strcmp();	/* Default string compare		*/
int	sort_l	= 256;		/* Define maximum record length		*/
int	(*sort_c)() = &strcmp;	/* Define default compare routine	*/
int	sort_r	=	0;	/* Non-zero for reverse comparison	*/
char	*sort_f	=	TEMP;	/* Change for a different temp. file	*/
extern	long	ftell();	/* File position routine		*/

static	int	first	=	1;
static	int	lbuf	=	NULL;
/*
 * First values:
 *	+1		Before first call of sorta (sorto illegal)
 *	 0		During calls to sorta
 *	-1		During calls to sorto
 */









sorta(datum)
char	*datum;
/*
 * Add datum to the stuff to be sorted
 */
{
	register char	*cp;
	register int	ndatum;
	char		*malloc();
	char		*nalloc();

	if (first != 0) {
		/*
		 * First call of sorta().  Open the work file and the
		 * head of the linked list of run descriptors.
		 */
		first = 0;
		if ((tfp = fopen(sort_f, "wun")) == NULL) {
			die("E-Cannot create temp file", sort_f);
		}
		/*
		 * This is necessary to allocate the work buffer
		 * before we run out of main memory.
		 */
		putc(0, tfp);
		line = nalloc(RUNSIZE * sizeof(char *));
		curr_run  = nalloc(sizeof(RUN));
		lbuf = nalloc(sort_l + 1);
	}
	if (datum != NULL) {
		/*
		 * Add datum to the stuff to sort
		 */
		ndatum = strlen(datum) + sizeof(char);
		if (nline >= RUNSIZE || (cp = malloc(ndatum)) == NULL) {
			/*
			 * Either the run is complete or we're out
			 * of dynamic memory.  Sort this run and
			 * save it, then allocate a new current run
			 * descriptor node.
			 */
			quick(0, nline-1);
			saverun();
			curr_run = nalloc(sizeof(RUN));
			cp  = nalloc(ndatum);
		}
		/*
		 * Save the datum.
		 */
		strcpy(cp, datum);
		line[nline++] = cp;
	}
	else {
		/*
		 * sorta(NULL) called, finish off the last (partial) run
		 * and setup for sorto().  heap_pointer will be NULL
		 * if the data was so small it all fit in main memory.
		 */
		quick(0, nline-1);
		if (first_run == NULL) {
			heap_pointer = NULL;
			nruns = 0;
		}
		else {
			/*
			 * Multiple runs, save the last, free up space,
			 * and get set for sorto().
			 */
			saverun();
			free(line);
/*
 *			if (freopen(sort_f, "run", tfp) == NULL)
 */
			fclose(tfp);
			if ((tfp = fopen(sort_f, "run")) == NULL) {
				die("F-Can't reopen temp. file.", sort_f);
			}
			/*
			 * Get the dummy first byte to allocate the input
			 * buffer.
			 */
			getc(tfp);
			heap = nalloc(nruns * sizeof(HEAP));
			run_pointer = first_run;
			heap_pointer = heap;
		}
		first = -1;		/* Flag sorta(NULL) called	*/
	}
}







char *
sorto(buffer)
char	*buffer;	/* Where output goes				*/
/*
 * Write the next record to the output buffer
 */
{
	register RUN		*rp;
	register HEAP		*hp;
	register union {
		char		*cp;
		int		i;
	} r;
	char			*nalloc();
	char			*getline();

	if (first != -1)
		die("E-sorto out of sync", NULL);
	if ((hp = heap_pointer) == NULL) {
		/*
		 * Only one buffer load was given
		 */
		if (nruns >= nline) {
			goto alldone;
		}
		else {
			strcpy(buffer, line[nruns]);
			free(line[nruns]);
			nruns++;
			return(buffer);
		}
	}			
	/*
	 * Multiple runs.
	 */
	if ((rp = run_pointer) != NULL) {
		/*
		 * First call of sorto().  Build the initial heap.
		 * This is done in two steps, following R. W. Floyd's
		 * method as given in N. Wirth:
		 * "Algorithms + Data Structures = Programs"
		 */
		do {
			hp->h_rp = rp;
			if (((hp++)->h_lp = getline(rp)) == NULL) {
				die("F-Empty run.", sort_f);
			}
		} while ((rp = rp->r_rp) != NULL);
		/*
		 * Now, sift the top half of the heap.
		 */
		for (r.i = nruns/2; --r.i >= 0;)
			sift(r.i);
		run_pointer = NULL;		/* Do this once only	*/
	}
	if (nruns) {
		/*
		 * We have more data to do, get the smallest entry
		 * from the heap to the user's buffer, free the entry
		 * and refill the heap (sifting the new entry into
		 * place.
		 */
		r.cp = heap[0].h_lp;
		strcpy(buffer, r.cp);
		free(r.cp);
		if ((heap[0].h_lp = getline(heap[0].h_rp)) == NULL) {
			--nruns;
			heap[0].h_rp = heap[nruns].h_rp;
			heap[0].h_lp = heap[nruns].h_lp;
		}
		sift(0);
		return(buffer);
	}
alldone:
	first = 1;			/* All done		*/
	free(lbuf);
	if (tfp != NULL) {
		fclose(tfp);
		delete(TEMP);
	}
	return(NULL);
}














/*
 * Quicksort as described in N. Wirths's
 * "Algorithms + Data Structures = Programs"
 * A pearl of software engineering.
 *
 */

quick()
   {
   register	i, j;
   int		l, r;
   char		*t;
   char		*p;

   stackptr--;                          /* push initial partition on stack */
   stackptr->lft = 0;
   stackptr->rght = nline - 1;
   do
      {
      l = stackptr->lft;                /* pop top partition from stack */
      r = stackptr->rght;
      stackptr++;
      do
         {
         i = l;
         j = r;
         p = line[(l + r) / 2];       /* split partition */
         do
            {
            while (compare(line[i], p) < 0) i++;
            while (compare(p, line[j]) < 0) j--;
            if (i <= j)
               {
               t = line[i];           /* swap position of recs */
               line[i] = line[j];
               line[j] = t;
               i++;
               j--;
               }
            } while (i <= j);
         if (j-l < r-i)               /* continue sorting smaller section */
            {
            if (i < r)
               {                       /* stack request for */
               stackptr--;             /* sorting right partition */
               if (stackptr < stack)
                  error("Stack overflow.\n");
               stackptr->lft = i;
               stackptr->rght = r;
               }
            r = j;                     /* continue sorting left */
            }
         else
            {
            if (l < j)
               {                       /* stack request for */
               stackptr--;             /* sorting left partition */
               if (stackptr < stack)
                  error("Stack overflow.\n");
               stackptr->lft = l;
               stackptr->rght = j;
               }
            l = i;                     /* continue sorting right */
            }
         } while (l < r);
      } while (stackptr != stack + STACKSIZE);
   }





/*
 * Sift an item through the heap. Handles variable size heap,
 * sifting from node 'n' through the bottom (node 'nruns-1').
 * Algorithm due to R.W. Floyd., described in Wirth (loc. cit.)
 */
sift(n)
int n;					/* Index of current top node */
   {
   register int		i, j;
   register HEAP	*h;		/* Fast heap node pointer */
   RUN			*trp;		/* Temp run ptr. */
   char			*tlp;		/* Temp line ptr. */

   i = n;
   h = heap;
   trp = h[i].h_rp;
   tlp = h[i].h_lp;

   while((j=2*i+1) < nruns)
      {
      if (j < nruns-1 && compare(h[j+1].h_lp, h[j].h_lp) < 0)
         ++j;
      if (compare(tlp, h[j].h_lp) <= 0)
         break;
      /*
       * Sift.
       */
      h[i].h_rp = h[j].h_rp;
      h[i].h_lp = h[j].h_lp;
      i = j;
      }
   h[i].h_rp = trp;
   h[i].h_lp = tlp;
   }














/*
 * Get a line from the specified run
 * on the temp. file.
 * Pack the line into allocated storage
 * and return a pointer to it.
 * Return NULL if there are no lines left
 * in the run; real end of file is an
 * internal botch.
 */
static char *
getline(rp)
register RUN	*rp;
{
	register char	*cp;
	register int	size;
	char		*nalloc();

	if (rp->r_size == 0)
		return (NULL);
	fseek(tfp, rp->r_seek, 0);
	size = fget(lbuf, sort_l, tfp);
	if (feof(tfp)) {
		fprintf(stderr, "run size = %d, seeked to %06o, read %d bytes",
				rp->r_size, rp->r_seek, size);
		die("F-Unexpected end of file.", sort_f);
	}
	rp->r_seek = ftell(tfp);
	--rp->r_size;
	cp = nalloc(size);
	strcpy(cp, lbuf);
	return (cp);
}
















/*
 * Save a run.
 * The run block has been preallocated
 * because there may not be enough space
 * to allocate it now.
 *
 * Dump the lines in the array `line' to
 * the temp. file.
 */
static
saverun()
{
	register i;

	curr_run->r_rp = NULL;
	curr_run->r_seek = ftell(tfp);
	curr_run->r_size = nline;
	if (first_run == NULL)
		first_run = curr_run;
	else
		last_run->r_rp = curr_run;
	last_run = curr_run;
	++nruns;
	for (i=0; i<nline; ++i) {
		fput(line[i], strlen(line[i]) + 1, tfp);
		if (ferror(tfp)) {
			die("E-error writing temp. file", sort_f);
		}
		free(line[i]);
	}
	nline = 0;
}
















/*
 * Compare routine.
 */
compare(a, b)
char *a, *b;
{
	register c;

	c = (*sort_c)(a, b);
	if (sort_r)
		c = -c;
	return (c);
}

/*
 * Allocate space.
 * If no space, abort with a nasty
 * little message.
 */
static char *
nalloc(n)
{
	register char	*p;
	char		*malloc();

	if ((p = malloc(n)) == NULL) {
		die("E-out of main memory.", NULL);
	}
	return (p);
}

static
die(s, arg)
char		*s;
char		*arg;
/*
 * Abort routine
 */
{
	if (arg != NULL)
		perror(arg);
	fprintf(stderr, "?SORTS-%s\n", s);
	error("Cannot continue");
}

