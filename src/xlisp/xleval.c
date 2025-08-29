/* xleval - xlisp evaluator */

#include "xlisp.h"

/* external variables */
extern NODE *xlstack,*xlenv,*xlnewenv;
extern NODE *s_lambda,*s_macro;
extern NODE *k_optional,*k_rest,*k_aux;
extern NODE *s_evalhook,*s_applyhook;
extern NODE *s_unbound;
extern NODE *s_stdout;

/* forward declarations */
FORWARD NODE *xlxeval();
FORWARD NODE *evalhook();
FORWARD NODE *evform();
FORWARD NODE *evsym();
FORWARD NODE *evfun();

/* xleval - evaluate an xlisp expression (checking for *evalhook*) */
NODE *xleval(expr)
  NODE *expr;
{
    return (s_evalhook->n_symvalue ? evalhook(expr) : xlxeval(expr));
}

/* xlxeval - evaluate an xlisp expression (bypassing *evalhook*) */
NODE *xlxeval(expr)
  NODE *expr;
{
    /* evaluate nil to itself */
    if (expr == NIL)
	return (NIL);

    /* add trace entry */
    xltpush(expr);

    /* check type of value */
    if (consp(expr))
	expr = evform(expr);
    else if (symbolp(expr))
	expr = evsym(expr);

    /* remove trace entry */
    xltpop();

    /* return the value */
    return (expr);
}

/* xlapply - apply a function to a list of arguments */
NODE *xlapply(fun,args)
  NODE *fun,*args;
{
    NODE *val;

    /* check for a null function */
    if (fun == NIL)
	xlfail("bad function");

    /* evaluate the function */
    if (subrp(fun))
	val = (*fun->n_subr)(args);
    else if (consp(fun)) {
	if (car(fun) != s_lambda)
	    xlfail("bad function type");
	val = evfun(fun,args);
    }
    else
	xlfail("bad function");

    /* return the result value */
    return (val);
}

/* evform - evaluate a form */
LOCAL NODE *evform(expr)
  NODE *expr;
{
    NODE *oldstk,fun,args,*val,*type;

    /* create a stack frame */
    oldstk = xlsave(&fun,&args,NULL);

    /* get the function and the argument list */
    fun.n_ptr = car(expr);
    args.n_ptr = cdr(expr);

    /* evaluate the first expression */
    if ((fun.n_ptr = xleval(fun.n_ptr)) == NIL)
	xlfail("bad function");

    /* evaluate the function */
    if (subrp(fun.n_ptr) || fsubrp(fun.n_ptr)) {
	if (subrp(fun.n_ptr))
	    args.n_ptr = xlevlist(args.n_ptr);
	val = (*fun.n_ptr->n_subr)(args.n_ptr);
    }
    else if (consp(fun.n_ptr)) {
	if ((type = car(fun.n_ptr)) == s_lambda) {
	    args.n_ptr = xlevlist(args.n_ptr);
	    val = evfun(fun.n_ptr,args.n_ptr);
	}
	else if (type == s_macro) {
	    args.n_ptr = evfun(fun.n_ptr,args.n_ptr);
	    val = xleval(args.n_ptr);
	}
	else
	    xlfail("bad function type");
    }
    else if (objectp(fun.n_ptr))
	val = xlsend(fun.n_ptr,args.n_ptr);
    else
	xlfail("bad function");

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* evalhook - call the evalhook function */
LOCAL NODE *evalhook(expr)
  NODE *expr;
{
    NODE *oldstk,*oldenv,fun,args,*val;

    /* create a new stack frame */
    oldstk = xlsave(&fun,&args,NULL);

    /* get the hook function */
    fun.n_ptr = s_evalhook->n_symvalue;

    /* make an argument list */
    args.n_ptr = newnode(LIST);
    rplaca(args.n_ptr,expr);

    /* rebind the hook functions to nil */
    oldenv = xlenv;
    xlsbind(s_evalhook,NIL);
    xlsbind(s_applyhook,NIL);

    /* call the hook function */
    val = xlapply(fun.n_ptr,args.n_ptr);

    /* unbind the symbols */
    xlunbind(oldenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* xlevlist - evaluate a list of arguments */
NODE *xlevlist(args)
  NODE *args;
{
    NODE *oldstk,src,dst,*new,*last,*val;

    /* create a stack frame */
    oldstk = xlsave(&src,&dst,NULL);

    /* initialize */
    src.n_ptr = args;

    /* evaluate each argument */
    for (val = NIL; src.n_ptr; src.n_ptr = cdr(src.n_ptr)) {

	/* check this entry */
	if (!consp(src.n_ptr))
	    xlfail("bad argument list");

	/* allocate a new list entry */
	new = newnode(LIST);
	if (val)
	    rplacd(last,new);
	else
	    val = dst.n_ptr = new;
	rplaca(new,xleval(car(src.n_ptr)));
	last = new;
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new list */
    return (val);
}

/* evsym - evaluate a symbol */
LOCAL NODE *evsym(sym)
  NODE *sym;
{
    NODE *p;

    /* check for a reference to an instance variable */
    if ((p = xlobsym(sym)) != NIL)
	return (car(p));

    /* get the value of the variable */
    while ((p = sym->n_symvalue) == s_unbound)
	xlunbound(sym);

    /* return the value */
    return (p);
}

/* xlunbound - signal an unbound variable error */
xlunbound(sym)
  NODE *sym;
{
    xlcerror("try evaluating symbol again","unbound variable",sym);
}

/* evfun - evaluate a function */
LOCAL NODE *evfun(fun,args)
  NODE *fun,*args;
{
    NODE *oldstk,*oldenv,*oldnewenv,cptr,*fargs,*val;

    /* create a stack frame */
    oldstk = xlsave(&cptr,NULL);

    /* skip the function type */
    if ((fun = cdr(fun)) == NIL || !consp(fun))
	xlfail("bad function definition");

    /* get the formal argument list */
    if ((fargs = car(fun)) && !consp(fargs))
	xlfail("bad formal argument list");

    /* bind the formal parameters */
    oldnewenv = xlnewenv; oldenv = xlnewenv = xlenv;
    xlabind(fargs,args);
    xlfixbindings();

    /* execute the code */
    for (cptr.n_ptr = cdr(fun); cptr.n_ptr != NIL; )
	val = xlevarg(&cptr.n_ptr);

    /* restore the environment */
    xlunbind(oldenv); xlnewenv = oldnewenv;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* xlabind - bind the arguments for a function */
xlabind(fargs,aargs)
  NODE *fargs,*aargs;
{
    NODE *arg;

    /* evaluate and bind each required argument */
    while (consp(fargs) && !iskeyword(arg = car(fargs)) && consp(aargs)) {

	/* bind the formal variable to the argument value */
	xlbind(arg,car(aargs));

	/* move the argument list pointers ahead */
	fargs = cdr(fargs);
	aargs = cdr(aargs);
    }

    /* check for the '&optional' keyword */
    if (consp(fargs) && car(fargs) == k_optional) {
	fargs = cdr(fargs);

	/* bind the arguments that were supplied */
	while (consp(fargs) && !iskeyword(arg = car(fargs)) && consp(aargs)) {

	    /* bind the formal variable to the argument value */
	    xlbind(arg,car(aargs));

	    /* move the argument list pointers ahead */
	    fargs = cdr(fargs);
	    aargs = cdr(aargs);
	}

	/* bind the rest to nil */
	while (consp(fargs) && !iskeyword(arg = car(fargs))) {

	    /* bind the formal variable to nil */
	    xlbind(arg,NIL);

	    /* move the argument list pointer ahead */
	    fargs = cdr(fargs);
	}
    }

    /* check for the '&rest' keyword */
    if (consp(fargs) && car(fargs) == k_rest) {
	fargs = cdr(fargs);
	if (consp(fargs) && (arg = car(fargs)) && !iskeyword(arg))
	    xlbind(arg,aargs);
	else
	    xlfail("symbol missing after &rest");
	fargs = cdr(fargs);
	aargs = NIL;
    }

    /* check for the '&aux' keyword */
    if (consp(fargs) && car(fargs) == k_aux)
	while ((fargs = cdr(fargs)) != NIL && consp(fargs))
	    xlbind(car(fargs),NIL);

    /* make sure the correct number of arguments were supplied */
    if (fargs != aargs)
	xlfail(fargs ? "too few arguments" : "too many arguments");
}

/* iskeyword - check to see if a symbol is a keyword */
LOCAL int iskeyword(sym)
  NODE *sym;
{
    return (sym == k_optional || sym == k_rest || sym == k_aux);
}

/* xlsave - save nodes on the stack */
NODE *xlsave(n)
  NODE *n;
{
    NODE **nptr,*oldstk;

    /* save the old stack pointer */
    oldstk = xlstack;

    /* save each node */
    for (nptr = &n; *nptr != NULL; nptr++) {
	rplaca(*nptr,NIL);
	rplacd(*nptr,xlstack);
	xlstack = *nptr;
    }

    /* return the old stack pointer */
    return (oldstk);
}
