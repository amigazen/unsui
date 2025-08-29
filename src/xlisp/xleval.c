/* xleval - xlisp evaluator */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/

#include "xlisp.h"

/* external variables */
extern int xlsample;
extern NODE ***xlstack,***xlstkbase,*xlenv;
extern NODE *s_lambda,*s_macro;
extern NODE *k_optional,*k_rest,*k_aux;
extern NODE *s_evalhook,*s_applyhook;
extern NODE *s_unbound;
extern NODE *s_stdout;

/* trace variables */
extern NODE **trace_stack;
extern int xltrace;

/* forward declarations */
FORWARD NODE *xlxeval();
FORWARD NODE *evalhook();
FORWARD NODE *evform();
FORWARD NODE *evfun();

/* xleval - evaluate an xlisp expression (checking for *evalhook*) */
NODE *xleval(expr)
  NODE *expr;
{
    /* check for control codes */
    if (--xlsample <= 0) {
	xlsample = SAMPLE;
	oscheck();
    }

    /* check for *evalhook* */
    if (getvalue(s_evalhook))
	return (evalhook(expr));

    /* add trace entry */
    if (++xltrace < TDEPTH)
	trace_stack[xltrace] = expr;

    /* check type of value */
    if (consp(expr))
	expr = evform(expr);
    else if (symbolp(expr))
	expr = xlgetvalue(expr);

    /* remove trace entry */
    --xltrace;

    /* return the value */
    return (expr);
}

/* xlxeval - evaluate an xlisp expression (bypassing *evalhook*) */
NODE *xlxeval(expr)
  NODE *expr;
{
    /* check type of value */
    if (consp(expr))
	expr = evform(expr);
    else if (symbolp(expr))
	expr = xlgetvalue(expr);

    /* return the value */
    return (expr);
}

/* xlapply - apply a function to a list of arguments */
NODE *xlapply(fun,args)
  NODE *fun,*args;
{
    NODE *env,*val;

    /* check for a null function */
    if (fun == NIL)
	xlfail("bad function");

    /* evaluate the function */
    if (subrp(fun))
	val = (*getsubr(fun))(args);
    else if (consp(fun)) {
	if (consp(car(fun))) {
	    env = cdr(fun);
	    fun = car(fun);
	}
	else
	    env = xlenv;
	if (car(fun) != s_lambda)
	    xlfail("bad function type");
	val = evfun(fun,args,env);
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
    NODE ***oldstk,*fun,*args,*env,*val,*type;

    /* create a stack frame */
    oldstk = xlsave(&fun,&args,NULL);

    /* get the function and the argument list */
    fun = car(expr);
    args = cdr(expr);

    /* evaluate the first expression */
    if ((fun = xleval(fun)) == NIL)
	xlfail("bad function");

    /* evaluate the function */
    if (subrp(fun) || fsubrp(fun)) {
	if (subrp(fun))
	    args = xlevlist(args);
	val = (*getsubr(fun))(args);
    }
    else if (consp(fun)) {
	if (consp(car(fun))) {
	    env = cdr(fun);
	    fun = car(fun);
	}
	else
	    env = xlenv;
	if ((type = car(fun)) == s_lambda) {
	    args = xlevlist(args);
	    val = evfun(fun,args,env);
	}
	else if (type == s_macro) {
	    args = evfun(fun,args,env);
	    val = xleval(args);
	}
	else
	    xlfail("bad function type");
    }
    else if (objectp(fun))
	val = xlsend(fun,args);
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
    NODE ***oldstk,*ehook,*ahook,*args,*val;

    /* create a new stack frame */
    oldstk = xlsave(&ehook,&ahook,&args,NULL);

    /* make an argument list */
    args = consa(expr);
    rplacd(args,consa(xlenv));

    /* rebind the hook functions to nil */
    ehook = getvalue(s_evalhook);
    setvalue(s_evalhook,NIL);
    ahook = getvalue(s_applyhook);
    setvalue(s_applyhook,NIL);

    /* call the hook function */
    val = xlapply(ehook,args);

    /* unbind the symbols */
    setvalue(s_evalhook,ehook);
    setvalue(s_applyhook,ahook);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* xlevlist - evaluate a list of arguments */
NODE *xlevlist(args)
  NODE *args;
{
    NODE ***oldstk,*src,*dst,*new,*val;
    NODE *last = NIL;

    /* create a stack frame */
    oldstk = xlsave(&src,&dst,NULL);

    /* initialize */
    src = args;

    /* evaluate each argument */
    for (val = NIL; src; src = cdr(src)) {

	/* check this entry */
	if (!consp(src))
	    xlfail("bad argument list");

	/* allocate a new list entry */
	new = consa(NIL);
	if (val)
	    rplacd(last,new);
	else
	    val = dst = new;
	rplaca(new,xleval(car(src)));
	last = new;
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new list */
    return (val);
}

/* xlunbound - signal an unbound variable error */
xlunbound(sym)
  NODE *sym;
{
    xlcerror("try evaluating symbol again","unbound variable",sym);
}

/* evfun - evaluate a function */
LOCAL NODE *evfun(fun,args,env)
  NODE *fun,*args,*env;
{
    NODE ***oldstk,*oldenv,*newenv,*cptr,*fargs,*val;

    /* create a stack frame */
    oldstk = xlsave(&oldenv,&newenv,&cptr,NULL);

    /* skip the function type */
    if ((fun = cdr(fun)) == NIL || !consp(fun))
	xlfail("bad function definition");

    /* get the formal argument list */
    if ((fargs = car(fun)) && !consp(fargs))
	xlfail("bad formal argument list");

    /* create a new environment frame */
    newenv = xlframe(env);
    oldenv = xlenv;

    /* bind the formal parameters */
    xlabind(fargs,args,newenv);
    xlenv = newenv;

    /* execute the code */
    for (cptr = cdr(fun); cptr; )
	val = xlevarg(&cptr);

    /* restore the environment */
    xlenv = oldenv;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* xlabind - bind the arguments for a function */
xlabind(fargs,aargs,env)
  NODE *fargs,*aargs,*env;
{
    NODE *arg;

    /* evaluate and bind each required argument */
    while (consp(fargs) && !iskeyword(arg = car(fargs)) && consp(aargs)) {

	/* bind the formal variable to the argument value */
	xlbind(arg,car(aargs),env);

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
	    xlbind(arg,car(aargs),env);

	    /* move the argument list pointers ahead */
	    fargs = cdr(fargs);
	    aargs = cdr(aargs);
	}

	/* bind the rest to nil */
	while (consp(fargs) && !iskeyword(arg = car(fargs))) {

	    /* bind the formal variable to nil */
	    xlbind(arg,NIL,env);

	    /* move the argument list pointer ahead */
	    fargs = cdr(fargs);
	}
    }

    /* check for the '&rest' keyword */
    if (consp(fargs) && car(fargs) == k_rest) {
	fargs = cdr(fargs);
	if (consp(fargs) && (arg = car(fargs)) && !iskeyword(arg))
	    xlbind(arg,aargs,env);
	else
	    xlfail("symbol missing after &rest");
	fargs = cdr(fargs);
	aargs = NIL;
    }

    /* check for the '&aux' keyword */
    if (consp(fargs) && car(fargs) == k_aux)
	while ((fargs = cdr(fargs)) != NIL && consp(fargs))
	    xlbind(car(fargs),NIL,env);

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
NODE ***xlsave(n)
  NODE **n;
{
    NODE ***oldstk,***nptr;

    /* save the old stack pointer */
    oldstk = xlstack;

    /* save each node pointer */
    for (nptr = &n; *nptr; nptr++) {
	if (xlstack <= xlstkbase)
	    xlabort("evaluation stack overflow");
	*--xlstack = *nptr;
	**nptr = NIL;
    }

    /* return the old stack pointer */
    return (oldstk);
}

