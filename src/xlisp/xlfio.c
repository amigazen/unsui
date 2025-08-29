/* xlfio.c - xlisp file i/o */

#include "xlisp.h"
#include "ctype.h"

/* external variables */
extern NODE *s_stdin,*s_stdout;
extern NODE *xlstack;
extern int xlfsize;
extern char buf[];

/* external routines */
extern FILE *fopen();

/* forward declarations */
FORWARD NODE *printit();
FORWARD NODE *flatsize();
FORWARD NODE *explode();
FORWARD NODE *implode();
FORWARD NODE *openit();
FORWARD NODE *getfile();

/* xread - read an expression */
NODE *xread(args)
  NODE *args;
{
    NODE *oldstk,fptr,eof,*val;

    /* create a new stack frame */
    oldstk = xlsave(&fptr,&eof,NULL);

    /* get file pointer and eof value */
    fptr.n_ptr = (args ? getfile(&args) : s_stdin->n_symvalue);
    eof.n_ptr = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* read an expression */
    if (!xlread(fptr.n_ptr,&val))
	val = eof.n_ptr;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the expression */
    return (val);
}

/* xprint - builtin function 'print' */
NODE *xprint(args)
  NODE *args;
{
    return (printit(args,TRUE,TRUE));
}

/* xprin1 - builtin function 'prin1' */
NODE *xprin1(args)
  NODE *args;
{
    return (printit(args,TRUE,FALSE));
}

/* xprinc - builtin function princ */
NODE *xprinc(args)
  NODE *args;
{
    return (printit(args,FALSE,FALSE));
}

/* xterpri - terminate the current print line */
NODE *xterpri(args)
  NODE *args;
{
    NODE *fptr;

    /* get file pointer */
    fptr = (args ? getfile(&args) : s_stdout->n_symvalue);
    xllastarg(args);

    /* terminate the print line and return nil */
    xlterpri(fptr);
    return (NIL);
}

/* printit - common print function */
LOCAL NODE *printit(args,pflag,tflag)
  NODE *args; int pflag,tflag;
{
    NODE *oldstk,fptr,val;

    /* create a new stack frame */
    oldstk = xlsave(&fptr,&val,NULL);

    /* get expression to print and file pointer */
    val.n_ptr = xlarg(&args);
    fptr.n_ptr = (args ? getfile(&args) : s_stdout->n_symvalue);
    xllastarg(args);

    /* print the value */
    xlprint(fptr.n_ptr,val.n_ptr,pflag);

    /* terminate the print line if necessary */
    if (tflag)
	xlterpri(fptr.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val.n_ptr);
}

/* xflatsize - compute the size of a printed representation using prin1 */
NODE *xflatsize(args)
  NODE *args;
{
    return (flatsize(args,TRUE));
}

/* xflatc - compute the size of a printed representation using princ */
NODE *xflatc(args)
  NODE *args;
{
    return (flatsize(args,FALSE));
}

/* flatsize - compute the size of a printed expression */
LOCAL NODE *flatsize(args,pflag)
  NODE *args; int pflag;
{
    NODE *oldstk,val;

    /* create a new stack frame */
    oldstk = xlsave(&val,NULL);

    /* get the expression */
    val.n_ptr = xlarg(&args);
    xllastarg(args);

    /* print the value to compute its size */
    xlfsize = 0;
    xlprint(NIL,val.n_ptr,pflag);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the length of the expression */
    val.n_ptr = newnode(INT);
    val.n_ptr->n_int = xlfsize;
    return (val.n_ptr);
}

/* xexplode - explode an expression */
NODE *xexplode(args)
  NODE *args;
{
    return (explode(args,TRUE));
}

/* xexplc - explode an expression using princ */
NODE *xexplc(args)
  NODE *args;
{
    return (explode(args,FALSE));
}

/* explode - internal explode routine */
LOCAL NODE *explode(args,pflag)
  NODE *args; int pflag;
{
    NODE *oldstk,val,strm;

    /* create a new stack frame */
    oldstk = xlsave(&val,&strm,NULL);

    /* get the expression */
    val.n_ptr = xlarg(&args);
    xllastarg(args);

    /* create a stream */
    strm.n_ptr = newnode(LIST);

    /* print the value into the stream */
    xlprint(strm.n_ptr,val.n_ptr,pflag);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the list of characters */
    return (car(strm.n_ptr));
}

/* ximplode - implode a list of characters into a symbol */
NODE *ximplode(args)
  NODE *args;
{
    return (implode(args,TRUE));
}

/* xmaknam - implode a list of characters into an uninterned symbol */
NODE *xmaknam(args)
  NODE *args;
{
    return (implode(args,FALSE));
}

/* implode - internal implode routine */
LOCAL NODE *implode(args,intflag)
  NODE *args; int intflag;
{
    NODE *list,*val;
    char *p;

    /* get the list */
    list = xlarg(&args);
    xllastarg(args);

    /* assemble the symbol's pname */
    for (p = buf; consp(list); list = cdr(list)) {
	if ((val = car(list)) == NIL || !fixp(val))
	    xlfail("bad character list");
	if ((int)(p - buf) < STRMAX)
	    *p++ = val->n_int;
    }
    *p = 0;

    /* create a symbol */
    val = (intflag ? xlenter(buf,DYNAMIC) : xlmakesym(buf,DYNAMIC));

    /* return the symbol */
    return (val);
}

/* xopeni - open an input file */
NODE *xopeni(args)
  NODE *args;
{
    return (openit(args,"r"));
}

/* xopeno - open an output file */
NODE *xopeno(args)
  NODE *args;
{
    return (openit(args,"w"));
}

/* openit - common file open routine */
LOCAL NODE *openit(args,mode)
  NODE *args; char *mode;
{
    NODE *fname,*val;
    FILE *fp;

    /* get the file name */
    fname = xlmatch(STR,&args);
    xllastarg(args);

    /* try to open the file */
    if ((fp = fopen(fname->n_str,mode)) != NULL) {
	val = newnode(FPTR);
	val->n_fp = fp;
	val->n_savech = 0;
    }
    else
	val = NIL;

    /* return the file pointer */
    return (val);
}

/* xclose - close a file */
NODE *xclose(args)
  NODE *args;
{
    NODE *fptr;

    /* get file pointer */
    fptr = xlmatch(FPTR,&args);
    xllastarg(args);

    /* make sure the file exists */
    if (fptr->n_fp == NULL)
	xlfail("file not open");

    /* close the file */
    fclose(fptr->n_fp);
    fptr->n_fp = NULL;

    /* return nil */
    return (NIL);
}

/* xrdchar - read a character from a file */
NODE *xrdchar(args)
  NODE *args;
{
    NODE *fptr,*val;
    int ch;

    /* get file pointer */
    fptr = (args ? getfile(&args) : s_stdin->n_symvalue);
    xllastarg(args);

    /* get character and check for eof */
    if ((ch = xlgetc(fptr)) == EOF)
	val = NIL;
    else {
	val = newnode(INT);
	val->n_int = ch;
    }

    /* return the character */
    return (val);
}

/* xpkchar - peek at a character from a file */
NODE *xpkchar(args)
  NODE *args;
{
    NODE *flag,*fptr,*val;
    int ch;

    /* peek flag and get file pointer */
    flag = (args ? xlarg(&args) : NIL);
    fptr = (args ? getfile(&args) : s_stdin->n_symvalue);
    xllastarg(args);

    /* skip leading white space and get a character */
    if (flag)
	while ((ch = xlpeek(fptr)) != EOF && isspace(ch))
	    xlgetc(fptr);
    else
	ch = xlpeek(fptr);

    /* check for eof */
    if (ch == EOF)
	val = NIL;
    else {
	val = newnode(INT);
	val->n_int = ch;
    }

    /* return the character */
    return (val);
}

/* xwrchar - write a character to a file */
NODE *xwrchar(args)
  NODE *args;
{
    NODE *fptr,*chr;

    /* get the character and file pointer */
    chr = xlmatch(INT,&args);
    fptr = (args ? getfile(&args) : s_stdout->n_symvalue);
    xllastarg(args);

    /* put character to the file */
    xlputc(fptr,chr->n_int);

    /* return the character */
    return (chr);
}

/* xreadline - read a line from a file */
NODE *xreadline(args)
  NODE *args;
{
    NODE *oldstk,fptr,str;
    char *p,*sptr;
    int len,ch;

    /* create a new stack frame */
    oldstk = xlsave(&fptr,&str,NULL);

    /* get file pointer */
    fptr.n_ptr = (args ? getfile(&args) : s_stdin->n_symvalue);
    xllastarg(args);

    /* make a string node */
    str.n_ptr = newnode(STR);
    str.n_ptr->n_strtype = DYNAMIC;

    /* get character and check for eof */
    len = 0; p = buf;
    while ((ch = xlgetc(fptr.n_ptr)) != EOF && ch != '\n') {

	/* check for buffer overflow */
	if ((int)(p - buf) == STRMAX) {
	    *p = 0;
 	    sptr = stralloc(len + STRMAX); *sptr = 0;
	    if (len) {
		strcpy(sptr,str.n_ptr->n_str);
		strfree(str.n_ptr->n_str);
	    }
	    str.n_ptr->n_str = sptr;
	    strcat(sptr,buf);
	    len += STRMAX;
	    p = buf;
	}

	/* store the character */
	*p++ = ch;
    }

    /* check for end of file */
    if (len == 0 && p == buf && ch == EOF) {
	xlstack = oldstk;
	return (NIL);
    }

    /* append the last substring */
    *p = 0;
    sptr = stralloc(len + (int)(p - buf)); *sptr = 0;
    if (len) {
	strcpy(sptr,str.n_ptr->n_str);
	strfree(str.n_ptr->n_str);
    }
    str.n_ptr->n_str = sptr;
    strcat(sptr,buf);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the string */
    return (str.n_ptr);
}

/* getfile - get a file or stream */
LOCAL NODE *getfile(pargs)
  NODE **pargs;
{
    NODE *arg;

    /* get a file or stream (cons) or nil */
    if (arg = xlarg(pargs)) {
	if (filep(arg)) {
	    if (arg->n_fp == NULL)
		xlfail("file not open");
	}
	else if (!consp(arg))
	    xlfail("bad argument type");
    }
    return (arg);
}
