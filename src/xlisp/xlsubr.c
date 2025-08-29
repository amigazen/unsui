/* xlsubr - xlisp builtin function support routines */

#include "xlisp.h"

/* external variables */
extern NODE *k_test,*k_tnot,*s_eql;
extern NODE *xlstack;

/* xlsubr - define a builtin function */
xlsubr(sname,type,subr)
  char *sname; int type; NODE *(*subr)();
{
    NODE *sym;

    /* enter the symbol */
    sym = xlsenter(sname);

    /* initialize the value */
    sym->n_symvalue = newnode(type);
    sym->n_symvalue->n_subr = subr;
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

    /* make sure its not a keyword */
    if (symbolp(arg) && *car(arg->n_symplist)->n_str == ':')
	xlfail("too few arguments");

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
	    xlfail("bad argument type");
    }
    else {
	if (arg == NIL || ntype(arg) != type)
	    xlfail("bad argument type");
    }

    /* return the argument */
    return (arg);
}

/* xlevarg - get the next argument and evaluate it */
NODE *xlevarg(pargs)
  NODE **pargs;
{
    NODE *oldstk,val;

    /* create a new stack frame */
    oldstk = xlsave(&val,NULL);

    /* get the argument */
    val.n_ptr = xlarg(pargs);

    /* evaluate the argument */
    val.n_ptr = xleval(val.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the argument */
    return (val.n_ptr);
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
	    xlfail("bad argument type");
    }
    else {
	if (arg == NIL || ntype(arg) != type)
	    xlfail("bad argument type");
    }

    /* return the argument */
    return (arg);
}

/* xltest - get the :test or :test-not keyword argument */
xltest(pfcn,ptresult,pargs)
  NODE **pfcn; int *ptresult; NODE **pargs;
{
    NODE *arg;

    /* default the argument to eql */
    if (!consp(*pargs)) {
	*pfcn = s_eql->n_symvalue;
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

/* xllastarg - make sure the remainder of the argument list is empty */
xllastarg(args)
  NODE *args;
{
    if (args)
	xlfail("too many arguments");
}

/* assign - assign a value to a symbol */
assign(sym,val)
  NODE *sym,*val;
{
    NODE *lptr;

    /* check for a current object */
    if ((lptr = xlobsym(sym)) != NIL)
	rplaca(lptr,val);
    else
	sym->n_symvalue = val;
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
