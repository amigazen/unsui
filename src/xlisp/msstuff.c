/* msstuff.c - ms-dos specific routines */

#include "xlisp.h"

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

/* osinit - initialize */
osinit(banner)
  char *banner;
{
    printf("%s\n",banner);
    lposition = 0;
    lindex = 0;
    lcount = 0;
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
	return (agetc(fp));

    /* check for a buffered character */
    if (lcount--)
	return (lbuf[lindex++]);

    /* get an input line */
    for (lcount = 0; ; )
	switch (ch = xgetc()) {
	case '\r':
		lbuf[lcount++] = '\n';
		xputc('\r'); xputc('\n'); lposition = 0;
		lindex = 0; lcount--;
		return (lbuf[lindex++]);
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
    return (bdos(7));
}

/* xputc - put a character to the terminal */
static xputc(ch)
  int ch;
{
    bdos(6,ch);
}

/* xcheck - check for a character */
static int xcheck()
{
    return (bdos(6,0xFF));
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

/* xgetkey - get a key from the keyboard */
NODE *xgetkey(args)
  NODE *args;
{
    xllastarg(args);
    return (cvfixnum((FIXNUM)xgetc()));
}

/* osfinit - initialize pc specific functions */
osfinit()
{
    xlsubr("DOS",		SUBR,	xdos);
    xlsubr("GET-KEY",		SUBR,	xgetkey);
}


