/* xlsubr - xlisp builtin function support routines */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/

#include "xlisp.h"

/* external variables */
extern NODE *k_test,*k_tnot,*s_eql;
extern NODE ***xlstack;

/* xlsubr - define a builtin function */
xlsubr(sname,type,subr)
  char *sname; int type; NODE *(*subr)();
{
    NODE *sym;

    /* enter the symbol */
    sym = xlsenter(sname);

    /* initialize the value */
    setvalue(sym,cvsubr(subr,type));
}

/* xlarg - get the next argument */
NODE *xlarg(pargs)
  NODE **pargs;
{
    NODE *arg;

    /* make sure the argument exists */
    if (!consp(*pargs))
	xlfail("too few arguments");

    /* get the argument value */
    arg = car(*pargs);

    /* move the argument pointer ahead */
    *pargs = cdr(*pargs);

    /* return the argument */
    return (arg);
}

/* xlmatch - get an argument and match its type */
NODE *xlmatch(type,pargs)
  int type; NODE **pargs;
{
    NODE *arg;

    /* get the argument */
    arg = xlarg(pargs);

    /* check its type */
    if (type == LIST) {
	if (arg && ntype(arg) != LIST)
	    xlerror("bad argument type",arg);
    }
    else {
	if (arg == NIL || ntype(arg) != type)
	    xlerror("bad argument type",arg);
    }

    /* return the argument */
    return (arg);
}

/* xlevarg - get the next argument and evaluate it */
NODE *xlevarg(pargs)
  NODE **pargs;
{
    NODE ***oldstk,*val;

    /* create a new stack frame */
    oldstk = xlsave(&val,NULL);

    /* get the argument */
    val = xlarg(pargs);

    /* evaluate the argument */
    val = xleval(val);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the argument */
    return (val);
}

/* xlevmatch - get an evaluated argument and match its type */
NODE *xlevmatch(type,pargs)
  int type; NODE **pargs;
{
    NODE *arg;

    /* get the argument */
    arg = xlevarg(pargs);

    /* check its type */
    if (type == LIST) {
	if (arg && ntype(arg) != LIST)
	    xlerror("bad argument type",arg);
    }
    else {
	if (arg == NIL || ntype(arg) != type)
	    xlerror("bad argument type",arg);
    }

    /* return the argument */
    return (arg);
}

/* xltest - get the :test or :test-not keyword argument */
void xltest(pfcn,ptresult,pargs)
  NODE **pfcn; int *ptresult; NODE **pargs;
{
    NODE *arg;

    /* default the argument to eql */
    if (!consp(*pargs)) {
	*pfcn = getvalue(s_eql);
	*ptresult = TRUE;
	return;
    }

    /* get the keyword */
    arg = car(*pargs);

    /* check the keyword */
    if (arg == k_test)
	*ptresult = TRUE;
    else if (arg == k_tnot)
	*ptresult = FALSE;
    else
	xlfail("expecting :test or :test-not");

    /* move the argument pointer ahead */
    *pargs = cdr(*pargs);

    /* make sure the argument exists */
    if (!consp(*pargs))
	xlfail("no value for keyword argument");

    /* get the argument value */
    *pfcn = car(*pargs);

    /* if its a symbol, get its value */
    if (symbolp(*pfcn))
	*pfcn = xleval(*pfcn);

    /* move the argument pointer ahead */
    *pargs = cdr(*pargs);
}

/* xlgetfile - get a file or stream */
NODE *xlgetfile(pargs)
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
	    xlerror("bad argument type",arg);
    }
    return (arg);
}

/* xllastarg - make sure the remainder of the argument list is empty */
xllastarg(args)
  NODE *args;
{
    if (args)
	xlfail("too many arguments");
}

/* eq - internal eq function */
int eq(arg1,arg2)
  NODE *arg1,*arg2;
{
    return (arg1 == arg2);
}

/* eql - internal eql function */
int eql(arg1,arg2)
  NODE *arg1,*arg2;
{
    if (eq(arg1,arg2))
	return (TRUE);
    else if (fixp(arg1) && fixp(arg2))
	return (arg1->n_int == arg2->n_int);
    else if (floatp(arg1) && floatp(arg2))
	return (arg1->n_float == arg2->n_float);
    else if (stringp(arg1) && stringp(arg2))
	return (strcmp(arg1->n_str,arg2->n_str) == 0);
    else
	return (FALSE);
}

/* equal - internal equal function */
int equal(arg1,arg2)
  NODE *arg1,*arg2;
{
    /* compare the arguments */
    if (eql(arg1,arg2))
	return (TRUE);
    else if (consp(arg1) && consp(arg2))
	return (equal(car(arg1),car(arg2)) && equal(cdr(arg1),cdr(arg2)));
    else
	return (FALSE);
}

