/* xlio - xlisp i/o routines */

#include "xlisp.h"

/* external variables */
extern int xlplevel;
extern int xlfsize;
extern NODE *xlstack;
extern NODE *s_stdin;
extern int xldebug;
extern int prompt;

/* xlgetc - get a character from a file or stream */
int xlgetc(fptr)
  NODE *fptr;
{
    NODE *lptr,*cptr;
    FILE *fp;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (consp(fptr)) {
	if ((lptr = car(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) ||
		(cptr = car(lptr)) == NIL || !fixp(cptr))
		xlfail("bad stream");
	    if (rplaca(fptr,cdr(lptr)) == NIL)
		rplacd(fptr,NIL);
	    ch = cptr->n_int;
	}
    }

    /* otherwise, check for a buffered file character */
    else if (ch = fptr->n_savech)
	fptr->n_savech = 0;

    /* otherwise, get a new character */
    else {

	/* get the file pointer */
	fp = fptr->n_fp;

	/* prompt if necessary */
	if (prompt && fp == stdin) {

	    /* print the debug level */
	    if (xldebug)
		printf("%d:",xldebug);

	    /* print the nesting level */
	    if (xlplevel > 0)
		printf("%d",xlplevel);

	    /* print the prompt */
	    printf("> ");
	    prompt = FALSE;
	}

	/* get the character */
	if (((ch = getc(fp)) == '\n' || ch == EOF) && fp == stdin)
	    prompt = TRUE;

	/* check for input abort */
	if (fp == stdin && ch == '\007') {
	    putchar('\n');
	    xlabort("input aborted");
	}
    }

    /* return the character */
    return (ch);
}

/* xlpeek - peek at a character from a file or stream */
int xlpeek(fptr)
  NODE *fptr;
{
    NODE *lptr,*cptr;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (consp(fptr)) {
	if ((lptr = car(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) ||
		(cptr = car(lptr)) == NIL || !fixp(cptr))
		xlfail("bad stream");
	    ch = cptr->n_int;
	}
    }

    /* otherwise, get the next file character and save it */
    else
	ch = fptr->n_savech = xlgetc(fptr);

    /* return the character */
    return (ch);
}

/* xlputc - put a character to a file or stream */
xlputc(fptr,ch)
  NODE *fptr; int ch;
{
    NODE *oldstk,lptr;

    /* count the character */
    xlfsize++;

    /* check for output to nil */
    if (fptr == NIL)
	;

    /* otherwise, check for output to a stream */
    else if (consp(fptr)) {
	oldstk = xlsave(&lptr,NULL);
	lptr.n_ptr = newnode(LIST);
	rplaca(lptr.n_ptr,newnode(INT));
	car(lptr.n_ptr)->n_int = ch;
	if (cdr(fptr))
	    rplacd(cdr(fptr),lptr.n_ptr);
	else
	    rplaca(fptr,lptr.n_ptr);
	rplacd(fptr,lptr.n_ptr);
	xlstack = oldstk;
    }

    /* otherwise, output the character to a file */
    else
	putc(ch,fptr->n_fp);
}

/* xlflush - flush the input buffer */
int xlflush()
{
    if (!prompt)
	while (xlgetc(s_stdin->n_symvalue) != '\n')
	    ;
}
