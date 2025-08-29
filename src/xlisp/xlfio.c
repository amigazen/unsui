/* xlfio.c - xlisp file i/o */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/

#include "xlisp.h"

#ifdef MEGAMAX
overlay "io"
#endif

/* external variables */
extern NODE *s_stdin,*s_stdout,*true;
extern NODE ***xlstack;
extern int xlfsize;
extern char buf[];

/* external routines */
extern FILE *fopen();

/* forward declarations */
FORWARD NODE *printit();
FORWARD NODE *flatsize();
FORWARD NODE *openit();

/* xread - read an expression */
NODE *xread(args)
  NODE *args;
{
    NODE ***oldstk,*fptr,*eof,*rflag,*val;

    /* create a new stack frame */
    oldstk = xlsave(&fptr,&eof,NULL);

    /* get file pointer and eof value */
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdin));
    eof = (args ? xlarg(&args) : NIL);
    rflag = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* read an expression */
    if (!xlread(fptr,&val,rflag != NIL))
	val = eof;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the expression */
    return (val);
}

/* xprint - built-in function 'print' */
NODE *xprint(args)
  NODE *args;
{
    return (printit(args,TRUE,TRUE));
}

/* xprin1 - built-in function 'prin1' */
NODE *xprin1(args)
  NODE *args;
{
    return (printit(args,TRUE,FALSE));
}

/* xprinc - built-in function princ */
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
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdout));
    xllastarg(args);

    /* terminate the print line and return nil */
    xlterpri(fptr);
    return (NIL);
}

/* printit - common print function */
LOCAL NODE *printit(args,pflag,tflag)
  NODE *args; int pflag,tflag;
{
    NODE ***oldstk,*fptr,*val;

    /* create a new stack frame */
    oldstk = xlsave(&fptr,&val,NULL);

    /* get expression to print and file pointer */
    val = xlarg(&args);
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdout));
    xllastarg(args);

    /* print the value */
    xlprint(fptr,val,pflag);

    /* terminate the print line if necessary */
    if (tflag)
	xlterpri(fptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
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
    NODE ***oldstk,*val;

    /* create a new stack frame */
    oldstk = xlsave(&val,NULL);

    /* get the expression */
    val = xlarg(&args);
    xllastarg(args);

    /* print the value to compute its size */
    xlfsize = 0;
    xlprint(NIL,val,pflag);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the length of the expression */
    return (cvfixnum((FIXNUM)xlfsize));
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
    char *name;
    FILE *fp;

    /* get the file name */
    fname = xlarg(&args);
    xllastarg(args);

    /* get the name string */
    if (symbolp(fname))
	name = getstring(getpname(fname));
    else if (stringp(fname))
	name = getstring(fname);
    else
	xlfail("bad argument type",fname);

    /* try to open the file */
    if ((fp = fopen(name,mode)) != NULL)
	val = cvfile(fp);
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
    if (getfile(fptr) == NULL)
	xlfail("file not open");

    /* close the file */
    fclose(getfile(fptr));
    setfile(fptr,NULL);

    /* return nil */
    return (NIL);
}

/* xrdchar - read a character from a file */
NODE *xrdchar(args)
  NODE *args;
{
    NODE *fptr;
    int ch;

    /* get file pointer */
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdin));
    xllastarg(args);

    /* get character and check for eof */
    return ((ch = xlgetc(fptr)) == EOF ? NIL : cvfixnum((FIXNUM)ch));
}

/* xpkchar - peek at a character from a file */
NODE *xpkchar(args)
  NODE *args;
{
    NODE *flag,*fptr;
    int ch;

    /* peek flag and get file pointer */
    flag = (args ? xlarg(&args) : NIL);
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdin));
    xllastarg(args);

    /* skip leading white space and get a character */
    if (flag)
	while ((ch = xlpeek(fptr)) != EOF && isspace(ch))
	    xlgetc(fptr);
    else
	ch = xlpeek(fptr);

    /* return the character */
    return (ch == EOF ? NIL : cvfixnum((FIXNUM)ch));
}

/* xwrchar - write a character to a file */
NODE *xwrchar(args)
  NODE *args;
{
    NODE *fptr,*chr;

    /* get the character and file pointer */
    chr = xlmatch(INT,&args);
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdout));
    xllastarg(args);

    /* put character to the file */
    xlputc(fptr,(int)getfixnum(chr));

    /* return the character */
    return (chr);
}

/* xreadline - read a line from a file */
NODE *xreadline(args)
  NODE *args;
{
    NODE ***oldstk,*fptr,*str,*newstr;
    int len,blen,ch;
    char *p,*sptr;

    /* create a new stack frame */
    oldstk = xlsave(&fptr,&str,NULL);

    /* get file pointer */
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdin));
    xllastarg(args);

    /* get character and check for eof */
    len = blen = 0; p = buf;
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n') {

	/* check for buffer overflow */
	if (blen >= STRMAX) {
 	    newstr = newstring(len+STRMAX);
	    sptr = getstring(newstr); *sptr = 0;
	    if (str) strcat(sptr,getstring(str));
	    *p = 0; strcat(sptr,buf);
	    p = buf; blen = 0;
	    len += STRMAX;
	    str = newstr;
	}

	/* store the character */
	*p++ = ch; blen++;
    }

    /* check for end of file */
    if (len == 0 && p == buf && ch == EOF) {
	xlstack = oldstk;
	return (NIL);
    }

    /* append the last substring */
    if (str == NIL || blen) {
	newstr = newstring(len+blen);
	sptr = getstring(newstr); *sptr = 0;
	if (str) strcat(sptr,getstring(str));
	*p = 0; strcat(sptr,buf);
	str = newstr;
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the string */
    return (str);
}

