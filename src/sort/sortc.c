/*
 *				s o r t c . c
 *
 * Sort utility
 */

/*)BUILD
		$(PROGRAM)	= sortc
		$(FILES)	= { sortc qksort }
		$(TKBOPTIONS) = {
			TASK 	= ...SOR
			ACTFIL	= 6
			UNITS	= 6
		}
*/

#ifdef	DOCUMENTATION

title	sort	Sort Data Files
index		Sort Data Files

synopsis

	sortc [-options] [-oOUTFILE] [file ...]

description

	Sortc sorts all of the named files together and writes the result
	to the standard output or to the file named in the -o option.
	The standard input is sorted if no file names are supplied; sort
	may thus be used as a filter.
	.s
	The output file may be the same as one of the input files.
	.s
	The default sort is ascending in ASCII collating sequence
	using the entire line.  Upper and lower case are considered
	different.  Using optional arguments, up to ten key fields
	may be specified.  Lines with equal keys are further ordered
	using the entire line as a single ASCII key.
	.s
	The following options apply to the entire sort:
	.s.lm +4
	.s.i-4;-o##The sorted output is written to the named file
	instead of to the standard output.  The file may be the
	same as one of the input files.
	.s.i-4;-u##(Unique) only the first of a set of lines
	having equal keys is output.  Only the specified keys are
	considered in the definition of 'unique.'
	.s.i-4;-v##(Verbose) Print elapsed time, etc.
	.s.lm -4
	The following options define the form of the sort.  If they
	preceed the first key field definition, they define the default
	for all keys.  If the option includes a key definition, only
	that key is affected.
	.s.lm +4
	.s.i-4;-b##(Blank) causes leading whitespace in the key
	field to be ignored.
	.s.i-4;-d##(Dictionary) sorts in dictionary order;  only
	letters, digits, and blanks are considered in the compare,
	all else is ignored.  (Note: national letters (with values greater
	than 128 decimal) are not processed correctly.)
	.s.i-4;-f##(Fold case) folds all letters to lower-case for
	comparisons.
	.s.i-4;-i##(Ignore whitespace) causes all non-printing characters
	to be ignored.
	.s.i-4;-k##(Key) selects a field to be used as a sort key.
	Up to ten key fields can be specified.  The keys are in order
	of decreasing significance.  Key fields are compared, beginning
	with the most significant one, until a not-equal key is found.
	.s
	The format of the -k option is "-kM1.N1,M2.N2"  The formatting
	flags "bdfinrt?" may be applied to a particular key by preceeding
	the 'k' appropriately as will be shown in the examples.
	.s
	The values following the "-k" define the starting and ending
	position of the key using a field/offset value:
	.lm+8
	.s.i-8;M1.N1##Start of the key position (first character that
	is to be compared).
	.s.i-8;M2.N2##End of the key position (first character after
	the key).
	.lm-8
	The 'M' values define the number of fields to skip, from the
	start of the data line.  (0 means "skip no fields").
	.s
	The 'N' values define the number of bytes to skip in the
	selected field.
	.s
	If "M1.N1" are omitted, start at the beginning of the line.
	If "M2.N2" are omitted, end at the end of the line.
	Except for "M2.N2" being omitted, omitting any of the four
	values means "use zero" for that value.
	.s
	Neither of these specifiers will go beyond the end of the line.
	A record can have a null key if the start-of-key is on or
	after the end-of-key or at or after the end-of-line.
	A null key is lower than any non-null key.
	.s
	If -b was specified, leading whitespace is skipped after
	advancing to the M1 (M2) field, but before advancing by
	N1 (N2) characters.
	.s.i-4;-n##(Numeric) changes the sort ordering to ascending
	arithmetic on a leading signed integer numeric string.
	.s.i-4;-r##(Reverse) changes the sort ordering from ascending
	to descending.
	.s.i-4;-t?#(Terminator) changes the field terminator.  The
	new definition is:
	.s.lm+4
	String of zero or more characters ending with the '?' character.
	.s.lm-4
	The terminator character may be escaped by using the backslash
	convention.  By default, whitespace (blanks or tabs) terminates
	a field.
	.lm-4

Examples of Key Field Selection

	.br;Let a record be "ABC#DEF##########GHI"
	.s.nf
	Flag		Key field
	-k1		" DEF         GHI"
	-bk1		"DEF         GHI"
	-bk1.1		"EF         GHI"
	-bk1,2		"DEF         "
	-k1,1		"" (Null field)
	-k1.0,2.1	" DEF "
	-bk1.0,2.1	"DEF         G"
	-k0.1,1.3	"BC DE"
	-k1.0,2.0	" DEF"
	-k0.5,1.0	"" (Null field)
	.s.f
	Let a record be "ABC,DEFGHIJ,KLM"
	.s.nf
	Flag		Key field
	-t,k1		"DEFGHIJ,KLM"
	-t,k1,2		"DEFGHIJ,"
	-t,k0.1,0.6	"BC,DE"
	-t,k1.0,0.10	"DEFGHI"
	.f	

Files

	sort.tmp

diagnostics

	The following messages occur on a non-severe error. SORTC
	will exit with "error" status.

	.lm +8
	.s.i -8;"?SORT-F-Cannot create temp. file"
	.br
	The required temporary file cannot be created  in  the
	current directory.
	.s.i -8;"?SORT-F-Cannot open input file."
	.br
	An input file cannot be accessed for reading.
	.s.i -8;"?SORT-F-Cannot create output file."
	.br
	An output file cannot be  created  for writing.
	.s.i -8;"?SORT-F-Out of space."
	.br
	There was insufficient memory space for the sort.
	.lm -8
	The following messages occur on a severe error. SORT will
	exit with "severe error" status. Get help.
	.s
	.nf
	"?SORT-U-Unexpected end of file"
	"?SORT-U-Empty run"
	"?SORT-U-temp. file"
	.f
author

	David Conroy
	.s
	Very slightly modified by Martin Minow
	.s.nf
	Extensively modified by
	  Ray Van Tassle
	  Motorola
	  1301 E. Algonquin Rd.
	  Room 4135
	  Shaumburg, Ill.
	  (312)-576-6017

Bugs

Internal

	See the source of sortc.c for a discussion of Workfile strategies
	and sort timings.

#endif

/*--- EDIT # 0492	27 Apr 1982   13:01:26	DB1:[21,6]SORTX.C;1077  */
/*--- PREVIOUS EDIT	27 Apr 1982   12:59:18	DB1:[21,6]SORTX.C;1076  */
/*
 * Edit history (after 0492)
 * 22-Jun-82	MM	Changed systime call to use library ftime routine.
 */

/*
 *
 *   
 * Work-file(s)
 *   This program uses a Quicksort for the distribution phase (creating
 *   runs of sorted lines) and puts each run into a work file. In essence,
 *   the merge is one n-way merge onto the output file. ("n" is the number
 *   of runs created in the 1st pass, as the file position is saved for each
 *   run).
 *   
 * During the distribution (input) phase, the sorted array (line)
 * is used as a heap while putting the run out to the work file.
 * We replace the lines with input lines, to attempt to increase the size
 * of the run. It is muchly and widely claimed that this doubles the
 * average run size.
 * My experiments bear this out. This should make the merge phase
 * go faster (less runs).
 * Oh hell. It turns out that doing so greatly increases the time
 * of the distribution phase (50% to 100%) but decreases the time of the
 * merge phase only a little bit. Perhaps it would help
 * in a very specialized environment, where you could make several
 * merge work-files, all on different devices, and have them contiguous,
 * so that you wouldn't have the overhead of arm seeks.
 * The absolute best you could ever hope to do would require 2 passes
 * over the data: one for the distribution phase, and one for
 * the merge phase. According to one of my books, about the best
 * thing to use with several work files is a "three file polyphase merge",
 * with a Fibonacci series for the number of runs on each file.
 * The quoted figures are 2.7-4.6 equivalent passes over the entire data
 * (polyphase is rough to figure exactly, because it does not pass the
 * entire file on each pass).
 * My figures on several varied files are the entire sort taking anywhere
 * from 3 to 5 times as long as just passing the file thru a pgm which
 * converts it to lower case. This seems quite good, as the time spent
 * in doing comparisons is hefty. Maybe changine the way we do the
 * merge could improve things a lot, but I don't think so. Anyway, this
 * is a pretty neat trick, cramming all the runs into one file.
 *
 *
 * I also pre-read several lines from each run whenever I do a seek
 * to that run (this is H_P_READ). This improves the time of the merge
 * phase by making it as much as ten (!!!) times faster. Evidently,
 * the seek time is the dominating factor of this phase, and
 * reading in several lines at each seek buys us a whole lot. Note that
 * each actual read operation gets 512 bytes, which is usually several
 * lines, so we have done all the work to get many lines, so
 * we might as well take advantage of that fact and save some of them.
 * It turns out that there is very little to be gained by increasing the
 * size of the pre-read from 5 to 10 lines. I suspect that most
 * of the improvement is in going from none to 1, and each incremental line
 * that is added gains less than the last.
 *
 * Doing all these things, and increasing the size of the in-core sort
 * area (MX_RLINE) will make this a pretty fast sort program.
 *
 * Some timings:
 * which	distr	merge	runs (distr,merge times in seconds 11/70)
 * 	Orig	327.2	146.1	214	Joe Sventeks DICT 45000 words
 *	This	171	177	75	same (The file is already sorted)
 *	pass	117			convert it to lower case (pass it)
 *
 *	orig	96.4	2038	161	32000 random numbers (ascii sort)
 *	this	174	271	54	same
 *	pass	111
 *
 *	orig	17.3	100.7	49	words.doc (9628 words)
 *	this	40	61	17
 *	pass	23
 *
 */
#include <stdio.h>

#ifdef unix
#include	<sys/types.h>
#include	<sys/timeb.h>
#else
#ifndef AMIGA
#include	<timeb.h>
#endif
#endif

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (0)
#endif

#ifndef EOS
#define EOS '\000'
#endif

#define MX_RLINE 600	/* number of lines in a run */
#define TEMP	"sort.tmp"
#define MAX_WK_FILES 1	/* # of different work-files to use */
#define MAX_KEYS 11	/* Max number of key fields that can be specified */
#define H_P_READ 8	/* # of lines a heapelt can hold */

typedef char BYTE;

/*********** */
/* These change the way internal routines perform */
#define TREEVER	/* verify the tree after doing the reheap */
#undef TREEVER
#define REPLPUT 	/* read replacement lines in putline */
#undef REPLPUT

struct work_file {
   char w_filename[35];
   FILE *w_fp;
};

struct	run {
   struct run *r_rp;
   long r_seek;
   int r_size;
};

struct heap_s {
   char *h_lp_a[H_P_READ];
   struct run *h_rp;
};

struct	run *crp  = NULL;	/* current run ptr */
struct	run *frp  = NULL;	/* 1st run ptr */
struct	run *lrp  = NULL;	/* last run ptr */

struct work_file wkf[MAX_WK_FILES];

char **line = NULL;
int first_out = TRUE;	/* This is the 1st output line */

FILE	*ofp = NULL;
FILE	*tfp	= NULL;
FILE	*ifp	= NULL;
char	*ofn	= NULL;
struct	heap_s *heap;
int	nline	= 0;
int	nruns	= 0;
int max_mline = 0;    /* max # of lines we can hold in memory during merge*/
int pre_lines = 0;    /* # of lines actually pre-read into memory */

long int in_records = 0;	/* # of records read from the input files */
long int in_bytes = 0;		/* # of bytes in them */

static char lbuf[256];		/* Input line buffer */

static char *documentation[] = {
	"-b  ignore leading blanks",
	"-d  dictionary order",
	"-f  fold letters to lowercase for compare",
	"-i  ignore non-printing chars",
	"-km1.n1,m2.n2 select key field (max 9 keys)",
	"-n  numeric comparison",
	"-oOUT  specify output file (otherwise it is stdout)",
	"-r  reverse order to descending",
	"-t? specify field terminator char",
	"-u  output only 1st line with equal keys",
	"-v  verbose",
	NULL
};

int nlbuf = -1;	/* length of string in lbuf (<0 means none) */
char	**llout = NULL;	/* last line put out (for -u) */

int	dflag[MAX_KEYS];	/* dictionary order */
int	iflag[MAX_KEYS];	/* ignore all whitespace */
int	fflag[MAX_KEYS];	/* fold upper-case to lower-case */
int	vflag = 0;		/* give elapsed times, etc */
int	hard_way[MAX_KEYS];	/* if any of the above are set, this
				* comparison must be done the hard way.*/
int	uflag = 0;		/* only output 1st of set of lines with
				* equal keys */
int	nflag[MAX_KEYS];	/* numeric comparison (else alphabetic) */
int	rflag[MAX_KEYS];	/* reverse sort sense
				* (descending instead of ascending) */
int	bflag[MAX_KEYS];	/* ignore leading whitespace */
char term_char[MAX_KEYS];	/* field terminator character */
int	kflag = 0;		/* number of field specifiers (0 = none) */

/***** m1/n1 specify the 1st character of the key */
int	m1[MAX_KEYS];		/* fields to skip to start-of-key */
int	n1[MAX_KEYS];		/* chars to skip from start-of-field */

/***** m2/n2 specify the 1st character after the key. This char isn't
 *	included in the key. */
int	m2[MAX_KEYS];		/* Number of fields to skip to end-of-key.
				 * -1 means 'end of line' */
int	n2[MAX_KEYS];		/* chars to skip from end-of-key field */

extern	char *getline();
extern  char *xalloc();
extern int compare();	/* compare routine */
extern qksort();	/* the sorting routine */
struct heap_s heap_tmp;	/* temp area for the reheap */

#ifdef TIMER
extern long int systime();	/* time in msec since midnight */
long int start_time;
#endif

main(argc, argv)
char *argv[];
{
   register struct run *rp;
   register char *cp;
   char *cptmp;
   char *argsave;
   struct heap_s *hp;
   int c, i, nf;

#ifdef TIMER
   start_time = systime();		/* init for elapsed time */
#endif
   for (i = 0; i < MAX_KEYS; i++) {
      m2[i] = -1;
   }
   nf = argc - 1;
   for (i=1; i<argc; ++i) {
      cp = argsave = argv[i];
      if (*cp == '-') {
	 --nf;
	 argv[i] = NULL;
	 ++cp;
	 while (c = *cp++) {
	    switch (tolower(c)) {

	    case 'v':
	       vflag++;
	       break;

	    case 'n':
	       ++nflag[kflag];
	       break;

	    case 'b':
	       ++bflag[kflag];
	       break;

	    case 'd':
	       ++dflag[kflag];
	       ++hard_way[kflag];
	       break;

	    case 'i':
	       ++iflag[kflag];
	       ++hard_way[kflag];
	       break;

	    case 'f':
	       ++fflag[kflag];
	       ++hard_way[kflag];
	       break;

	    case 'u':
	       ++uflag;
	       break;

	    case 'o':
	       if (*cp == NULL) { 	/* value is next arg */
		  if (++i >= argc)
		     usage("argument need after", argsave, c);
					/* no next arg, error! */
		  if (*(ofn = argv[i]) == '-')
		     usage("next argument may not be an option", argsave, c);
					/* next arg is itself an arg!! */
		  argv[i] = NULL;	/* eliminate the arg	*/
	       }
	       else {		/* value is in this arg */
		  ofn = cp;
	       }
	       cp = argv[i];	/* re-examine current arg (to skip it) */
	       --nf;
	       break;

	    case 'r':
	       ++rflag[kflag];
	       break;

	    case 'k':
	       defin_key(cp);
	       kflag++;
	       m1[kflag] = n1[kflag] = n2[kflag] = 0; m2[kflag] = -1;
	       bflag[kflag] = rflag[kflag] = nflag[kflag] = 0;
	       iflag[kflag] = fflag[kflag] = dflag[kflag] = 0;
	       hard_way[kflag] = term_char[kflag] = 0;
	       cp = &argv[i];
	       break;

	    case 't':
	       if ((c = *cp++) == EOS)
			usage("no field separator after 't' option",
				argsave, c);
	       if (c == '\\') {		/* escaped char */
		  if (*cp == NULL)
			usage("no field separator after 't\\' option",
				argsave, c);
		  cptmp = cp;
		  c = esc_char(&cptmp);   cp = cptmp;
	       }
	       term_char[kflag] = c;
	       break;

	    default:
	       usage("unknown option", argsave, c);
	    }
	 }
      }
   }
   if (nf == 0)
      ifp = stdin;
   if ((tfp = fopen(TEMP, "w")) == NULL) {
      fprintf(stderr, "Cannot create temp file.\n");
      exit(1);
   }
#ifdef AMIGA || unix
   strcpy (wkf[0].w_filename, TEMP);
#else
   fgetname(tfp, wkf[0].w_filename);
#endif
   line = xalloc(MX_RLINE * sizeof(char *));
   crp = xalloc(sizeof(struct run));
   crp->r_size = 0;
/* There is always an implied last key of: ascii, ascending order,
 *	using the entire line.
 */
   if (kflag == 0) {
      kflag++;
   }
   m1[kflag] = n1[kflag] = n2[kflag] = 0; m2[kflag] = -1;
   bflag[kflag] = rflag[kflag] = nflag[kflag] = 0;
   iflag[kflag] = fflag[kflag] = dflag[kflag] = 0;
   hard_way[kflag] = term_char[kflag] = 0;
   if (m1[kflag] == m1[kflag-1] && n1[kflag] == n1[kflag-1] &&
	m2[kflag] == m2[kflag-1] && n2[kflag] == n2[kflag-1])
   if (bflag[kflag] == bflag[kflag-1] && rflag[kflag] == rflag[kflag-1] &&
	nflag[kflag] == nflag[kflag-1] && iflag[kflag] == iflag[kflag-1])
   if (fflag[kflag] == fflag[kflag-1] && dflag[kflag] == dflag[kflag-1] &&
	term_char[kflag] == term_char[kflag-1])
   kflag--;
/*** Distribution phase. Create sorted runs from the input file(s)
 * to the temp file(s).
*/
   for (;;) {
      if (ifp == NULL) {
	 for (i=1; i<argc; ++i)	/* find next file-name argument */
	    if ((cp = argv[i]) != NULL)
	    break;
	 if (i >= argc)
	    break;
	 argv[i] = NULL;
	 if ((ifp = fopen(cp, "r")) == NULL) {
	    fprintf(stderr, "%s: cannot open.\n", cp);
	    quit();
	 }
      }
      get_in_line();		/* get line from input */
      if (nlbuf >= 0) {
	 if (nline >= MX_RLINE
	  || (cp = malloc(nlbuf + sizeof(char))) == NULL) {
	    do {
	       qksort(line, nline, sizeof(line[0]), &compare);
	       saverun();
	       putline(tfp);
	       crp = xalloc(sizeof(struct run));
	       crp->r_size = 0;
	    } while (nline == MX_RLINE);
	    if (nlbuf >= 0)
		cp = xalloc(nlbuf + sizeof (char));
	 }
	 if (nlbuf >= 0) {
	    strcpy(cp, lbuf);
	    nlbuf = -1;
	    line[nline++] = cp;
	 }
      }
   }
   qksort(line, nline, sizeof (line[0]), &compare);
   if (frp == NULL) { /* We have only 1 run, so put it right out. */
      openoutput();
      if (uflag)
	 llout = xalloc(sizeof(lbuf));
      putline(ofp);
      pr_eltim("Completed, all data fit in memory");
      quit();
   }

/*** Merge phase. We are all done with the input files. */
   if (nline > 0) {
      saverun();
      putline(tfp);
   }
   fclose(tfp);

   pr_eltim("Distribution phase complete");
#ifdef TIMER
   start_time = systime();
#endif
   free(line);
   if (uflag)
      llout = xalloc(sizeof(lbuf));
   openoutput();
   if ((tfp = fopen(wkf[0].w_filename, "r")) == NULL)
      panic("Cannot reopen temp file.\n");
   heap = xalloc(nruns * sizeof(struct heap_s));

/* See how many lines can be pre-read form the various runs. All this is
 * in an attempt to read several lines from a run into memory each time
 * we read, because the seek time is most of the time expense of the
 * merge phase.
 * We are limited by: 1) the size of the h_lps array in a heap element,
 * and 2) the amount of memory we have available to put these lines into.
 * To arrive at the memory we have/need, allocate as much as we need.
 * This will most likely fail (no way is a very large file going to
 * fit in memory). This failure point defines the max # of lines we will
 * be able to hold. There is a safety margin here, but bad luck could
 * cause the alloc to fail during the merge. If this happens, bump up
 * the safety margin. With any luck, using the average record size will
 * be good enough.
*/
   i = nruns * H_P_READ;
   if (i > in_records) i = in_records;
   c = in_bytes / in_records;
   if (c < (sizeof(int))) c = sizeof(int);
   lrp = xalloc(25 * c);	/* alloc a safety margin */
   lrp->r_rp = NULL;
   while (i-- && (crp = malloc(c)) != NULL) {
      crp->r_rp = lrp;
      lrp = crp;
      max_mline++;
   }
   /* now free the space all up */
   while (lrp != NULL) {
      crp = lrp;
      lrp = lrp->r_rp;
      free(crp);
   }

   /* Read the same number of lines initially for all runs. */
   i = max_mline / nruns;
   if (i <= 0) i = 1;
   if (i > H_P_READ) i = H_P_READ;
   rp = frp;
   hp = &heap[0];
   while (rp != NULL) { /* init for the merge */
      hp->h_rp = rp;
      run_read(hp, i);
      if (hp->h_lp_a[0] == NULL)
	 panic("Empty run.\n");
      rp = rp->r_rp;
      hp++;
   }
   /* Sort it, to get the heap initially set up. */
   qksort (heap, nruns, sizeof (struct heap_s), &compare);

   while (nruns) {
      cp = heap[0].h_lp_a[0];
      if (llout)
	 sp_fputs(cp, ofp);
      else {
	 fputs(cp, ofp);
	 fputs("\n", ofp);
      }
      free(cp);
      memcopy(&heap_tmp, &heap[0], sizeof (struct heap_s));
      pre_lines--;	/* shift up the other pre-read lines */
      memcopy(&heap_tmp.h_lp_a[0], &heap_tmp.h_lp_a[1],
	   sizeof(heap[0].h_lp_a[0]) * (H_P_READ-1));
      heap_tmp.h_lp_a[H_P_READ - 1] = NULL;
      if (heap_tmp.h_lp_a[0] == NULL)	/* we used them all up */
	 run_read(&heap_tmp, H_P_READ);
      if (heap_tmp.h_lp_a[0] == NULL) { /* Done with this run. */
	 pr_eltim("Run complete");
	 --nruns;
	 reheap (heap, nruns, sizeof (struct heap_s), &heap[nruns]);
      }
      else
	 reheap (heap, nruns, sizeof (struct heap_s), &heap_tmp);
   }
   if (vflag) {
#ifdef TIMER
      i = (systime() - start_time) /100;	/* get&print elapsed time*/
      fprintf (stderr,"Merge Elapsed: %d.%01d sec\n", i/10, i%10);
#endif
   }
   quit();
}


/*********************************************/
pr_eltim(why)
char	*why;
{
   int i;

   if (!vflag)
      return;
#ifdef TIMER
   i = (systime() - start_time) /100;	/* get&print elapsed time*/
#endif
   fprintf(stderr, "%s, %ld records, %ld bytes, run number %d.\n",
	why, in_records, in_bytes, nruns);
#ifdef TIMER
   fprintf (stderr,"Elapsed time: %d.%01d sec\n", i/10, i%10);
#endif
}

/*********************************************/
/* Get the next line from the input file to "lbuf".
 * ifp == NULL means input file is closed.
 * nlbuf == length of the line in lbuf (<0 means nothing in lbuf).
*/
get_in_line()
{
   if (ifp == NULL) return;
   if (fgets(lbuf, sizeof lbuf, ifp) == NULL) {
      if (ifp != stdin) fclose(ifp);
      ifp = NULL;
      nlbuf = -1;
   }
   else {
      lbuf[strlen(lbuf) - 1] = EOS;
      nlbuf = strlen(lbuf);
      in_records++; in_bytes += nlbuf;
   }
}

/*
 * Open the output file and stash its file
 * pointer in 'ofp'. If no output file is
 * given 'ofp' is a dup. of 'stdout'.
 */
openoutput()
{
   if (ofn == NULL)
      ofp = stdout;
   else if ((ofp = fopen(ofn, "w")) == NULL) {
      fprintf(stderr, "%s: cannot create.\n", ofn);
      quit();
   }
}

/******************************************/
/* Special fputs, used for '-u' flag, to put out only
 * unique lines.
 * The very first line is deemed 'unique', and is always put out.
*/
sp_fputs(buf, usr_file)
char *buf;
FILE *usr_file;
{
   if (com_par(&llout, &buf, kflag? kflag-1 : kflag) || first_out) {
      fputs(buf, usr_file); /* this line different from last */
      fputs("\n", usr_file);
      first_out = FALSE;
      strcpy (llout, buf);
   }
}

/**************************************/
/* read some lines from a run into memory */
run_read(helt, m_lines)
struct heap_s *helt;	/* the heap element for this run */
int m_lines;		/* max # of lines to read */
{
   int i;

   for (i = 0; i < m_lines && pre_lines <= max_mline; i++) {
      if ((helt->h_lp_a[i] = getline(helt->h_rp)) == NULL)
	 break;
      pre_lines++;
   }
   if (i < H_P_READ) helt->h_lp_a[i] = NULL;
}


/************************************************/
/* convert escaped char to a char value. Update cp. */
esc_char (cpp)
char **cpp;
{
   register char c;	/* the converted character */
   register char c1;
   register char *p;	/* buffer ptr */

   p = *cpp;	/* point to the string */
   c = tolower(*p++);
   if (c == 't')
      c = '\t';			/* \t is tab */
   else if (c == 's')
      c = ' ';			/* \s is space */
   else if (c >= '0' && c <= '7') { /*  \digits is the obvious */
      c -= '0';
      while ((c1 = *p++) >= '0' && c1 <= '7')
	 c = (c<<3) + (c1 - '0');
      p--;			/* back up, we went too far */
   }
   else			/* anything else is itself */
      c = *(p-1);
   *cpp = p;		/* update the buffer pointer */
   return (c);
}


















/******************** routines passed into quicksort *******/
/*
 * Compare routine.
 */
static int     compare(sa, sb)
char *sa[], *sb[];
{
   return (com_par(sa, sb, kflag));
}



static int     com_par(sa, sb, num_keys)
char *sa[], *sb[];
int num_keys;	/* max number of keys to examine */
{
   extern char *get_to_key();
   register char *a, *b;
   register c;
   char ch;
   long d, atol();
   int field_num;			/* field number loop counter */
   char *aeok_ptr, *beok_ptr;	/* for saving the char after the key */
   char aeok_char, beok_char;

   c = 0;
   for (field_num = 0; !c && (field_num <= num_keys); field_num++) {
      /* find the key-field addresses */
      aeok_ptr = beok_ptr = &ch;	/* in case EOK = EOL */
      a = get_to_key(m1[field_num], n1[field_num],
	   m2[field_num], n2[field_num],
	   bflag[field_num], *sa, &aeok_ptr, term_char[field_num]);
      b = get_to_key(m1[field_num], n1[field_num],
	   m2[field_num], n2[field_num],
	   bflag[field_num], *sb, &beok_ptr, term_char[field_num]);
      aeok_char = *aeok_ptr; *aeok_ptr = NULL;	/* terminate keys */
      beok_char = *beok_ptr; *beok_ptr = NULL;	/* terminate keys */
      /*fprintf(stderr,"a key:'%s'\n", a);*/

      if (nflag[field_num]) {
	 if ((d = atol(a)-atol(b)) < 0)
	    --c;
	 else if (d > 0)
	    ++c;
      }
      else if (hard_way[field_num])
	 c = hard_cmp(a, b, iflag[field_num], dflag[field_num],
	   fflag[field_num]);
      else
	 c = strcmp(a, b);
      /* restore the char after the keys */
      *aeok_ptr = aeok_char;
      *beok_ptr = beok_char;
      if (rflag[field_num])
	 c = -c;
   }
   return (c);
}

/***************/
/* return pointer to the next field of the string.
 * A field is defined as: optional whitespace followed by
 *	non-whitespace; the next field is the next
 *	whitespace or NULL.
 *
 * The returned result is a pointer to this 1st char of the
 * next field.
 * It will never advance past the trailing NULL.
*/
char *nxt_fld(s, tch)
register char *s;
register char tch;	/* field terminator char or NULL */
{
   register char c;

   if (tch != NULL) {
      while (*s != NULL && *s++ != tch) ;
   }
   else
      {
      while (((c=*s) != NULL) && iswhite(c)) s++;
      while (((c=*s) != NULL) && !iswhite(c)) s++;
   }
   return (s);
}

/*****************************************/
/* whitespace is anything not between space & 0177 */
iswhite(c)
register char c;
{
   /* make it an unsigned byte */
   c &= 0377;
   return ( (c <= ' ') || (0177 <= c));
}
/*****************************************/
/********* get the definition for a key field
*/
defin_key(cp)
char *cp;
{
   if (kflag >= MAX_KEYS - 1)
      panic ("Too many keys specified\n");

   n1[kflag] = n2[kflag] = 0; m2[kflag] = -1;
   m1[kflag] = atodl(&cp);
   if (*cp == '.') {
      cp++;		/* char adv from start of field */
      n1[kflag] = atodl(&cp);
   }
   if (!*cp++)	return;	/* no end-of-key */

   m2[kflag] = atodl(&cp);
   if (!*cp) return;
   if (*cp++ != '.')
      panic ("Invalid key field format\n");
   n2[kflag] = atodl(&cp);

   return;
}

/*****************/
/* unsigned ascii to decimal conversion.
 * stops on a non-digit. Update the buffer pointer to
 * point to this terminating non-digit.
*/
int atodl(cp)
char **cp;
{
   register int n;
   register char *p;
   register char ch;

   n = 0;
   p = *cp;	/* point to the string */
   while (isdigit((ch = *p++)))
      n = 10*n + (ch - '0');
   *cp = p-1;	/* update the buffer pointer */
   return (n);
}

/************************************************/
/* get pointers to the start-of-key, and end-of-key.
 * returns ptr to 1st char of the key.
 * "eok_ptr" points to next char after the key, unless eok = "end-of-line".
 * The pointers will never go past the NULL.
 * eok-ptr will never be before sok-ptr.
*/
char *get_to_key (sok_fld, sok_adv, eok_fld, eok_adv, skip_ws,
     str, eok_ptr, term_ch)
int sok_fld, sok_adv;	/* start-of-key fields & chars to skip*/
int eok_fld, eok_adv;	/* end-of-key fields & chars to skip*/
int skip_ws;		/* skip leading whitespace before advancing by chars*/
char *str;		/* the string */
char **eok_ptr;	/* set to point to the 1st char after the key,
		 * if any eok was specified. 
		 * otherwise not modified. */
char term_ch;		/* field terminator or NULL */
{
   register char *kptr;	/* start-of-key pointer */
   register char *eptr;	/* end-of-key pointer */
   register int i;
   char ch;

/* skip over fields to get to start-of-key */
   kptr = str;	/* init to start of line */
   i = sok_fld;
   while (i--) kptr = nxt_fld(kptr, term_ch);
   /* probably we can keep going forward from the SOK to EOK field */
   i = eok_fld - sok_fld;	eptr = kptr;
/* advance SOK ptr by chars (never past end) */
   if (skip_ws)
      while ((ch=*kptr) && iswhite(ch)) kptr++;
   while (sok_adv-- && *kptr++);
   if (eok_fld >= 0) { /* position to eok field*/
      if (i < 0) {
	 i = eok_fld;	/* eok search from start of line */
	 eptr = str;
      }
      while (i--) eptr = nxt_fld(eptr, term_ch);
      /* advance eok ptr by chars (never past end) */
      if (skip_ws)
	 while ((ch=*eptr) && iswhite(ch)) eptr++;
      while (eok_adv-- && *eptr++);
      *eok_ptr = eptr;	/* return ptr to char after key */
      if (eptr < kptr)
	 *eok_ptr = kptr;	/* force null key */
   }
   return (kptr);
}

/*************************************************************/
/* string comparisons the hard-way, one character at a time, 
 * because one of the special flags was set.
*/
hard_cmp(a, b, ifl, dfl, ffl)
register char *a, *b;	/* the two strings to compare */
int ifl;	/* ignore all whitespace */
int dfl;	/* dictionary order, ignore all but letters, digits,
		* and blanks
		*/
int ffl;	/* fold upper-case to lower case before compare */
{
   register char achar, bchar;
   int c;	/* result */

   c = 0;
   while (!c && (achar = *a) && (bchar = *b)) {
      if (dfl) {	/* dictionary-style */
	 while ((achar = *a) && !(isdigit(achar) ||
	      isalpha(achar) || (achar == ' '))) a++;
	 while ((bchar = *b) && !(isdigit(bchar) ||
	      isalpha(bchar) || (bchar == ' '))) b++;
      }
      if (ifl) {	/* ignore all whitespace */
	 while ((achar = *a) && iswhite(achar)) a++;
	 while ((bchar = *b) && iswhite(bchar)) b++;
      }
      if (ffl) {	/* fold to lower-case */
	 achar = tolower(achar);
	 bchar = tolower(bchar);
      }
      c = achar - bchar;
      if (!c && achar) a++;
      if (!c && bchar) b++;
   }
   if (!c) c = achar - bchar;
   return(c);
}











/*
 * Rebuild a heap.
 * This is essentially TREESORT.
 * Used to reorder the heap when a new item
 * is read into it.
 * The 'heap' is a binary tree, such that for each node 'i', the key at
 * 'i' is the lowest of it and it's two sons, (2*i + 1) and (2*i + 2).
 * The new element is to be inserted where it should go.
 * We can move things up in the heap, as array element [0] is empty.
 * At each step, h[i] is empty.
 */

reheap(h, n, elt_size, new_elt)
BYTE h[];		/* The array which is the heap */
int n;			/* number of elements in array h*/
int elt_size;		/* size of array element in bytes */
struct heap_s *new_elt;	/* new element to be added */
{
   register int i, j;
   BYTE *ip, *jp, *jp1;	/* ptrs for i, j, j+1 */

   for (i = 0, ip = h; (j = 2*i+1) < n; i = j, ip = jp) {
      jp = h+(j*elt_size);
      jp1 = jp + elt_size;
      if ((j+1 < n) && (compare(jp1, jp) < 0)) {
	 jp = jp1;
	 ++j;
      }
      /* j now points to the smaller child of i */
      if (compare(new_elt, jp) <= 0)
	 break;
      memcopy(ip, jp, elt_size);
   }
   memcopy(ip, new_elt, elt_size);
#ifdef	TREEVER	/**** verify that the tree is ok */
   for (i = 0; (j = 2*i+1) < nruns; i++) {
      if (h != heap) break;
      if ((j < nruns && compare(&heap[i].h_lp, &heap[j].h_lp) > 0) ||
	   (j+1 < nruns && compare(&heap[i].h_lp,&heap[j+1].h_lp) > 0)) {
	 fprintf (stderr, "Tree out of order. n = %d. Index = %d.\n",
	      nruns, i);
	 fprintf (stderr,"addresses: %o %o %o\n", &heap[i], &heap[j],
	      &heap[j+1]);
	 fprintf (stderr,"nruns: %d\n%d->%s%d->%s%d->%s", nruns,
	      i,heap[i].h_lp,j,heap[j].h_lp,j+1,
	      j+1<nruns ? heap[j+1].h_lp : NULL);
	 fprintf (stderr,"New->%s", new_elt->h_lp);
	 fprintf (stderr,"At %o\n", new_elt);
	 fprintf (stderr,"%d->%s", 0, heap[0].h_lp);
	 for (i = 0; i <n; i++) {
	    fprintf (stdout, "%d->%s", i, heap[i].h_lp);
	 }
	 abort();
      }
   }
   for (i = 0; (j = 2*i+1) < nline; i++) {
      if (h != line) break;
      if ((j < nline && compare(&line[i], &line[j]) > 0) ||
	   (j+1 < nline && compare(&line[i],&line[j+1]) > 0)) {
	 fprintf (stderr, "Tree out of order. n = %d. Index = %d.\n",
	      nline, i);
	 fprintf (stderr,"addresses: %o %o %o\n", &line[i], &line[j],
	      &line[j+1]);
	 fprintf (stderr,"nline: %d\n%d->%s%d->%s%d->%s", nline,
	      i,line[i],j,line[j],j+1,
	      j+1<nline ? line[j+1] : NULL);
	 for (i = 0; i <n; i++) {
	    fprintf (stdout, "%d->%s", i, line[i]);
	 }
	 abort();
      }
   }
#endif
}

/*
 * Save a run.
 * The run block has been preallocated
 * because there may not be enough space
 * to allocate it now.
 */
saverun()
{
   long ftell();

   crp->r_rp = NULL;
   crp->r_seek = ftell(tfp);
   if (frp == NULL)
      frp = crp;
   else
      lrp->r_rp = crp;
   lrp = crp;
   ++nruns;
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
char *getline(rp)
register struct run *rp;
{
   register char *cp;
   long ftell();

   if (rp->r_size == 0)
      return (NULL);
   fseek(tfp, rp->r_seek, 0);
   if (fgets(lbuf, sizeof lbuf, tfp) == NULL)
      panic("Unexpected end of file\n");
   lbuf[strlen(lbuf) - 1] = EOS;
   rp->r_seek = ftell(tfp);
   --rp->r_size;
   cp = xalloc(strlen(lbuf) + sizeof(char));
   strcpy(cp, lbuf);
   return (cp);
}

/*************************/
/* Dump the lines in the array to the file.	*/
putline(fp)
FILE *fp;	/* the output fp */
{
   register int ilater;	/* ptr for lines that can't go in this run */
   register int i;
   register char *cp;
#ifdef REPLPUT
   char *ncp;
#endif

   crp->r_size = nline;
#ifndef REPLPUT
   for (i=0; i<nline; i++) {
      cp = line[i];
      if (llout)
	 sp_fputs(cp, fp);
      else {
	 fputs(cp, fp);
	 fputs("\n", fp);
      }
      free(cp);
   }
   nline = 0;
#else

/*
 * To improve the length of the runs, replace each line as it goes out.
 * If it can, it will become part of this run, otherwise it will be
 * saved up to be part of the next run.
 *
 * In general, when this routine returns, the array will be filled with
 * lines that wouldn't go in this run. This will be the case except when
 * we hit EOF on the input.
 *
 * The next input line is already in lbuf.
*/
   ilater = MX_RLINE;
   while (nline > 0) {
      cp = line[0];
      if (llout)
	 sp_fputs(cp, fp);
      else {
	 fputs(cp, fp);
	 fputs("\n", fp);
      }
      if (nlbuf >= 0 && crp->r_size < 30000 &&
	   (ncp = malloc(nlbuf + sizeof(char))) != NULL) {
	 strcpy(ncp, lbuf);
	 get_in_line();
	 if (compare(&ncp, &cp) >= 0) { /* the line can be in this run */
	    crp->r_size++;
	    reheap(line, nline, sizeof(line[0]), &ncp);
	 }
	 else {	/* it will be part of the next run */
	    nline--;
	    reheap (line, nline, sizeof(line[0]), &line[nline]);
	    line[--ilater] = ncp;	/* tuck it away */
	 }
      }
      else {	/* nothing to replace with */
	 nline--;
	 reheap(line, nline, sizeof(line[0]), &line[nline]);
      }
      free(cp);
   }

/* The new replacement lines that couldn't become part of
 * this run are at the end of the array. Move them up and adjust the
 * index.
*/
   nline = MX_RLINE - ilater;
   if (nline != 0 && ilater != 0)
      memcopy(&line[0], &line[ilater], nline * sizeof(line[0]));
#endif
}

/*
 * Allocate space.
 * If no space, abort with a nasty
 * little message.
 */
char *xalloc(n)
{
   register char *p;

   if ((p = malloc(n)) == NULL) {
      fprintf(stderr, "Out of space.\n");
      exit(1);
   }
   return (p);
}

/*
 * Quit.
 * Get rid of the temp. file.
 * Exit.
 */
quit()
{
   if (tfp != NULL)
#if AMIGA || unix
      unlink (wkf[0].w_filename);
#else
      fmkdl(tfp);
#endif
   exit(0);
}

/*
 * Tell the user just what is expected
 * of him.
 */
usage(why, argsave, c)
char		*why;
char		*argsave;
char		c;
{
   if (c != '?') {
      fprintf(stderr, "Sort error: %s", why);
      if (c == EOS)
         fprintf(stderr, "\nat end of option string");
      else if (c < ' ')
         fprintf(stderr, " at CTRL/%c", c + '@');
      else fprintf(stderr, " at '%c'", c);
      fprintf(stderr, ", argument = \"%s\"\n", argsave);
   }
   fprintf(stderr, "Usage: sort [-bdfinrt?uv] [-[bdfinrt?]km1.n1,m2.n2]");
   fprintf (stderr, " [-k...]\n\t    [-oOUTFILE] [file ...]\n");
   if (c == '?') {
      prtdoc();
   }
   exit(1);
}
/*
 * Errors.
 * Print a message and die with "error" status on RT/RSX,
 * "error" on UNIX (I think).
 */
errxit(a)
   {
   fprintf(stderr,"?SORT-F-%r", &a);
#ifdef decus                           /* Exit status per rt/rsx/unix */
#ifdef rt11
   exit(4);
#endif
#ifdef rsx
   exit(2);
#endif
#else
   exit(0);
#endif
   }

/*
 * Fatal errors.
 * Print a message and die.
 */
panic(a)
{
   fprintf(stderr, "Panic: %r", &a);
#ifdef decus                           /* Exit status per rt/rsx/unix */
#ifdef rt11
   exit(8);
#endif
#ifdef rsx
   exit(4);
#endif
#else
   exit(0);
#endif
}

#ifdef TIMER
static int		time_first = TRUE;
static struct timeb	first_time;

long int
systime()
/*
 * Returns elapsed time in milliseconds
 */
{
	long		time, millisec;
	struct timeb	time_buffer;

	if (time_first) {
		/*
		 * This makes sure we can store enough milliseconds in 32 bits
		 */
		ftime(&first_time);
		time_first = FALSE;
	}
	ftime(&time_buffer);
	time = time_buffer.time - first_time.time;
	millisec = time_buffer.millitm - first_time.millitm;
	time *= 1000;
	return (time + millisec);
}
#endif	/* TIMER */

prtdoc ()
{
	register char *dp;

	for (dp = documentation; *dp; dp++) {
		printf ("%s\n", *dp);
	}
}

#ifndef unix
memcopy (out, in, size)
char *out;
char *in;
int size;
{
#ifdef AMIGA
   movmem (in, out, size);
#else
   while (size-- > 0) {
	*out++ = *in++;
   }
#endif
}
#endif
