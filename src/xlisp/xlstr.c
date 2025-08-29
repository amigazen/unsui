/* xlstr - xlisp string builtin functions */

#include "xlisp.h"

/* external variables */
extern NODE *xlstack;

/* external procedures */
extern char *strcat();

/* xstrlen - length of a string */
NODE *xstrlen(args)
  NODE *args;
{
    NODE *val;
    int total;

    /* initialize */
    total = 0;

    /* loop over args and total */
    while (args)
	total += strlen(xlmatch(STR,&args)->n_str);

    /* create the value node */
    val = newnode(INT);
    val->n_int = total;

    /* return the total */
    return (val);
}

/* xstrcat - concatenate a bunch of strings */
NODE *xstrcat(args)
  NODE *args;
{
    NODE *oldstk,val,*p;
    char *str;
    int len;

    /* create a new stack frame */
    oldstk = xlsave(&val,NULL);

    /* find the length of the new string */
    for (p = args, len = 0; p; )
	len += strlen(xlmatch(STR,&p)->n_str);

    /* create the result string */
    val.n_ptr = newnode(STR);
    val.n_ptr->n_str = str = stralloc(len);
    *str = 0;

    /* combine the strings */
    while (args)
	strcat(str,xlmatch(STR,&args)->n_str);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new string */
    return (val.n_ptr);
}

/* xsubstr - return a substring */
NODE *xsubstr(args)
  NODE *args;
{
    NODE *oldstk,arg,src,val;
    int start,forlen,srclen;
    char *srcptr,*dstptr;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&src,&val,NULL);

    /* initialize */
    arg.n_ptr = args;
    
    /* get string and its length */
    src.n_ptr = xlmatch(STR,&arg.n_ptr);
    srcptr = src.n_ptr->n_str;
    srclen = strlen(srcptr);

    /* get starting pos -- must be present */
    start = xlmatch(INT,&arg.n_ptr)->n_int;

    /* get length -- if not present use remainder of string */
    forlen = (arg.n_ptr ? xlmatch(INT,&arg.n_ptr)->n_int : srclen);

    /* make sure there aren't any more arguments */
    xllastarg(arg.n_ptr);

    /* don't take more than exists */
    if (start + forlen > srclen)
	forlen = srclen - start + 1;

    /* if start beyond string -- return null string */
    if (start > srclen) {
	start = 1;
	forlen = 0; }
	
    /* create return node */
    val.n_ptr = newnode(STR);
    val.n_ptr->n_str = dstptr = stralloc(forlen);

    /* move string */
    for (srcptr += start-1; forlen--; *dstptr++ = *srcptr++)
	;
    *dstptr = 0;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the substring */
    return (val.n_ptr);
}

/* xascii - return ascii value */
NODE *xascii(args)
  NODE *args;
{
    NODE *val;

    /* build return node */
    val = newnode(INT);
    val->n_int = *(xlmatch(STR,&args)->n_str);

    /* make sure there aren't any more arguments */
    xllastarg(args);

    /* return the character */
    return (val);
}

/* xchr - convert an INT into a one character ascii string */
NODE *xchr(args)
  NODE *args;
{
    NODE *oldstk,val;
    char *sptr;

    /* create a new stack frame */
    oldstk = xlsave(&val,NULL);

    /* build return node */
    val.n_ptr = newnode(STR);
    val.n_ptr->n_str = sptr = stralloc(1);
    *sptr++ = xlmatch(INT,&args)->n_int;
    *sptr = 0;

    /* make sure there aren't any more arguments */
    xllastarg(args);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new string */
    return (val.n_ptr);
}

/* xatoi - convert an ascii string to an integer */
NODE *xatoi(args)
  NODE *args;
{
    NODE *val;
    int n;

    /* get the string and convert it */
    n = atoi(xlmatch(STR,&args)->n_str);

    /* make sure there aren't any more arguments */
    xllastarg(args);

    /* create the value node */
    val = newnode(INT);
    val->n_int = n;

    /* return the number */
    return (val);
}

/* xitoa - convert an integer to an ascii string */
NODE *xitoa(args)
  NODE *args;
{
    NODE *val;
    char buf[20];
    int n;

    /* get the integer */
    n = xlmatch(INT,&args)->n_int;
    xllastarg(args);

    /* convert it to ascii */
    sprintf(buf,"%d",n);

    /* create the value node */
    val = newnode(STR);
    val->n_str = strsave(buf);

    /* return the string */
    return (val);
}
