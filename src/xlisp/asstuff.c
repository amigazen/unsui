/* asstuff.c - Amiga specific routines */

#include "xlisp.h"

#ifndef MANX
#define agetc getc	/* Not sure if this will work in all cases (fnf) */
#define aputc putc	/* Not sure if this will work in all cases (fnf) */
#endif

#define LBSIZE 200

/* external routines */
extern double ran();

/* external variables */
extern NODE *s_unbound,*true;
extern int prompt;
extern int errno;

/* line buffer variables */
static char lbuf[LBSIZE];
static int  lpos[LBSIZE];
static int lindex;
static int lcount;
static int lposition;

#define NEW 1006
static long xlispwindow;

/* osinit - initialize */
osinit(banner)
  char *banner;
{
    extern int Enable_Abort;

    Enable_Abort = 0;		/* Turn off ^C interrupt in case it's on */
    xlispwindow = Open("RAW:1/1/639/199/Xlisp by David Betz", NEW);
    while (*banner != '\000') {
	xputc (*banner++);
    }
    xputc ('\n');
    lposition = 0;
    lindex = 0;
    lcount = 0;
}

osfinish ()
{
    Close (xlispwindow);
}

/* osrand - return a random number between 0 and n-1 */
int osrand(n)
  int n;
{
    n = (int)(ran() * (double)n);
    return (n < 0 ? -n : n);
}

/* osgetc - get a character from the terminal */
int osgetc(fp)
  FILE *fp;
{
    int ch;

    /* check for input from a file other than stdin */
    if (fp != stdin)
	return ((int)agetc(fp));

    /* check for a buffered character */
    if (lcount--)
	return ((int)lbuf[lindex++]);

    /* get an input line */
    for (lcount = 0; ; )
	switch (ch = xgetc()) {
	case '\n':
	case '\r':
		lbuf[lcount++] = '\n';
		xputc('\r'); xputc('\n'); lposition = 0;
		lindex = 0; lcount--;
		return ((int)lbuf[lindex++]);
	case '\010':
	case '\177':
		if (lcount) {
		    lcount--;
		    while (lposition > lpos[lcount]) {
			xputc('\010'); xputc(' '); xputc('\010');
			lposition--;
		    }
		}
		break;
	case '\032':
		osflush();
		return (EOF);
	default:
		if (ch == '\t' || (ch >= 0x20 && ch < 0x7F)) {
		    lbuf[lcount] = ch;
		    lpos[lcount] = lposition;
		    if (ch == '\t')
			do {
			    xputc(' ');
			} while (++lposition & 7);
		    else {
			xputc(ch); lposition++;
		    }
		    lcount++;
		}
		else {
		    osflush();
		    switch (ch) {
		    case '\003':	xltoplevel();	/* control-c */
		    case '\007':	xlcleanup();	/* control-g */
		    case '\020':	xlcontinue();	/* control-p */
		    case '\032':	return (EOF);	/* control-z */
		    default:		return (ch);
		    }
		}
	}
}

/* osputc - put a character to the terminal */
osputc(ch,fp)
  int ch; FILE *fp;
{
    /* check for output to something other than stdout */
    if (fp != stdout)
	return (aputc(ch,fp));

    /* check for control characters */
    oscheck();

    /* output the character */
    if (ch == '\n') {
	xputc('\r'); xputc('\n');
	lposition = 0;
    }
    else {
	xputc(ch);
	lposition++;
   }
}

/* oscheck - check for control characters during execution */
oscheck()
{
    int ch;
    if (ch = xcheck())
	switch (ch) {
	case '\002':	osflush(); xlbreak("BREAK",s_unbound); break;
	case '\003':	osflush(); xltoplevel(); break;
	}
}

/* osflush - flush the input line buffer */
osflush()
{
    lindex = lcount = 0;
    osputc('\n',stdout);
    prompt = 1;
}

/* xgetc - get a character from the terminal without echo */
static int xgetc()
{
    char ch;

    Read (xlispwindow, &ch, 1);
    return (ch & 0xFF);
}

/* xputc - put a character to the terminal */
static xputc(ch)
  int ch;
{
    char chout;

    chout = ch;
    Write (xlispwindow, &chout, 1L);
}

/* xcheck - check for a character */
static int xcheck()
{
    if (WaitForChar (xlispwindow, 0L) == 0L)
	return (0);
    return (xgetc() & 0xFF);
}

/* xdos - execute a dos command */
NODE *xdos(args)
  NODE *args;
{
    char *cmd;
    cmd = xlmatch(STR,&args)->n_str;
    xllastarg(args);
    return (system(cmd) == -1 ? cvfixnum((FIXNUM)errno) : true);
}

int system (cmd)
char *cmd;
{
	return (Execute(cmd, 0L, xlispwindow));
}

double ran ()	/* Just punt for now, not in Manx C; FIXME!!*/
{
	static long seed = 654321;
	long lval;
	double dval;

	seed *= ((8 * (123456) - 3));
	lval = seed & 0xFFFF;
	dval = ((double) lval) / ((double) (0x10000));
	return (dval);
}
	
/* xgetkey - get a key from the keyboard */
NODE *xgetkey(args)
  NODE *args;
{
    xllastarg(args);
    return (cvfixnum((FIXNUM)xgetc()));
}

#ifdef DEADCODE	/* Dont' use this for now?  (fnf) */

/* xcursor - set the cursor position */
NODE *xcursor(args)
  NODE *args;
{
    int row,col;
    row = xlmatch(INT,&args)->n_int;
    col = xlmatch(INT,&args)->n_int;
    xllastarg(args);
    scr_curs(row,col);
    return (NIL);
}

/* xclear - clear the screen */
NODE *xclear(args)
  NODE *args;
{
    xllastarg(args);
    scr_clear();
    return (NIL);
}

/* xeol - clear to end of line */
NODE *xeol(args)
  NODE *args;
{
    xllastarg(args);
    scr_eol();
    return (NIL);
}


/* xeos - clear to end of screen */
NODE *xeos(args)
  NODE *args;
{
    xllastarg(args);
    scr_eos();
    return (NIL);
}

/* xlinsert - insert line */
NODE *xlinsert(args)
  NODE *args;
{
    xllastarg(args);
    scr_linsert();
    return (NIL);
}

/* xldelete - delete line */
NODE *xldelete(args)
  NODE *args;
{
    xllastarg(args);
    scr_ldelete();
    return (NIL);
}

/* xcinsert - insert character */
NODE *xcinsert(args)
  NODE *args;
{
    xllastarg(args);
    scr_cinsert();
    return (NIL);
}

/* xcdelete - delete character */
NODE *xcdelete(args)
  NODE *args;
{
    xllastarg(args);
    scr_cdelete();
    return (NIL);
}

/* xinverse - set/clear inverse video */
NODE *xinverse(args)
  NODE *args;
{
    NODE *val;
    val = xlarg(&args);
    xllastarg(args);
    scr_invers(val ? 1 : 0);
    return (NIL);
}

/* xline - draw a line */
NODE *xline(args)
  NODE *args;
{
    int x1,y1,x2,y2;
    x1 = xlmatch(INT,&args)->n_int;
    y1 = xlmatch(INT,&args)->n_int;
    x2 = xlmatch(INT,&args)->n_int;
    y2 = xlmatch(INT,&args)->n_int;
    xllastarg(args);
    line(x1,y1,x2,y2);
    return (NIL);
}

/* xpoint - draw a point */
NODE *xpoint(args)
  NODE *args;
{
    int x,y;
    x = xlmatch(INT,&args)->n_int;
    y = xlmatch(INT,&args)->n_int;
    xllastarg(args);
    point(x,y);
    return (NIL);
}

/* xcircle - draw a circle */
NODE *xcircle(args)
  NODE *args;
{
    int x,y,r;
    x = xlmatch(INT,&args)->n_int;
    y = xlmatch(INT,&args)->n_int;
    r = xlmatch(INT,&args)->n_int;
    xllastarg(args);
    circle(x,y,r);
    return (NIL);
}

/* xaspect - set the aspect ratio */
NODE *xaspect(args)
  NODE *args;
{
    int x,y;
    x = xlmatch(INT,&args)->n_int;
    y = xlmatch(INT,&args)->n_int;
    xllastarg(args);
    set_asp(x,y);
    return (NIL);
}

/* xcolors - setup the display colors */
NODE *xcolors(args)
  NODE *args;
{
    int c,p,b;
    c = xlmatch(INT,&args)->n_int;
    p = xlmatch(INT,&args)->n_int;
    b = xlmatch(INT,&args)->n_int;
    xllastarg(args);
    color(c);
    palette(p);
    ground(b);
    return (NIL);
}

/* xmode - set the display mode */
NODE *xmode(args)
  NODE *args;
{
    int m;
    m = xlmatch(INT,&args)->n_int;
    xllastarg(args);
    mode(m);
    return (NIL);
}

#endif DEADCODE

/* osfinit - initialize pc specific functions */
osfinit()
{
    xlsubr("DOS",		SUBR,	xdos);
    xlsubr("GET-KEY",		SUBR,	xgetkey);
#ifdef DEADCODE
    xlsubr("SET-CURSOR",	SUBR,	xcursor);
    xlsubr("CLEAR",		SUBR,	xclear);
    xlsubr("CLEAR-EOL",		SUBR,	xeol);
    xlsubr("CLEAR-EOS",		SUBR,	xeos);
    xlsubr("INSERT-LINE",	SUBR,	xlinsert);
    xlsubr("DELETE-LINE",	SUBR,	xldelete);
    xlsubr("INSERT-CHAR",	SUBR,	xcinsert);
    xlsubr("DELETE-CHAR",	SUBR,	xcdelete);
    xlsubr("SET-INVERSE",	SUBR,	xinverse);
    xlsubr("LINE", 		SUBR,	xline);
    xlsubr("POINT",		SUBR,	xpoint);
    xlsubr("CIRCLE",		SUBR,	xcircle);
    xlsubr("ASPECT-RATIO",	SUBR,	xaspect);
    xlsubr("COLORS",		SUBR,	xcolors);
    xlsubr("MODE", 		SUBR,	xmode);
#endif DEADCODE
}


