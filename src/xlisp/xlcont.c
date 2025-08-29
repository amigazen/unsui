/* xlcont - xlisp control built-in functions */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/

#include "xlisp.h"

/* external variables */
extern NODE ***xlstack,*xlenv,*xlvalue;
extern NODE *s_unbound;
extern NODE *s_evalhook,*s_applyhook;
extern NODE *true;

/* external routines */
extern NODE *xlxeval();

/* forward declarations */
FORWARD NODE *let();
FORWARD NODE *prog();
FORWARD NODE *progx();
FORWARD NODE *doloop();

/* xcond - built-in function 'cond' */
NODE *xcond(args)
  NODE *args;
{
    NODE ***oldstk,*arg,*list,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&list,NULL);

    /* initialize */
    arg = args;

    /* initialize the return value */
    val = NIL;

    /* find a predicate that is true */
    while (arg) {

	/* get the next conditional */
	list = xlmatch(LIST,&arg);

	/* evaluate the predicate part */
	if (val = xlevarg(&list)) {

	    /* evaluate each expression */
	    while (list)
		val = xlevarg(&list);

	    /* exit the loop */
	    break;
	}
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* xcase - built-in function 'case' */
NODE *xcase(args)
  NODE *args;
{
    NODE ***oldstk,*key,*arg,*clause,*list,*val;

    /* create a new stack frame */
    oldstk = xlsave(&key,&arg,&clause,NULL);

    /* initialize */
    arg = args;

    /* get the key expression */
    key = xlevarg(&arg);

    /* initialize the return value */
    val = NIL;

    /* find a case that matches */
    while (arg) {

	/* get the next case clause */
	clause = xlmatch(LIST,&arg);

	/* compare the key list against the key */
	if ((list = xlarg(&clause)) == true ||
            (listp(list) && keypresent(key,list)) ||
            eql(key,list)) {

	    /* evaluate each expression */
	    while (clause)
		val = xlevarg(&clause);

	    /* exit the loop */
	    break;
	}
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* keypresent - check for the presence of a key in a list */
LOCAL int keypresent(key,list)
  NODE *key,*list;
{
    for (; consp(list); list = cdr(list))
	if (eql(car(list),key))
	    return (TRUE);
    return (FALSE);
}

/* xand - built-in function 'and' */
NODE *xand(args)
  NODE *args;
{
    NODE ***oldstk,*arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg = args;
    val = true;

    /* evaluate each argument */
    while (arg)

	/* get the next argument */
	if ((val = xlevarg(&arg)) == NIL)
	    break;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* xor - built-in function 'or' */
NODE *xor(args)
  NODE *args;
{
    NODE ***oldstk,*arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg = args;
    val = NIL;

    /* evaluate each argument */
    while (arg)
	if ((val = xlevarg(&arg)))
	    break;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* xif - built-in function 'if' */
NODE *xif(args)
  NODE *args;
{
    NODE ***oldstk,*testexpr,*thenexpr,*elseexpr,*val;

    /* create a new stack frame */
    oldstk = xlsave(&testexpr,&thenexpr,&elseexpr,NULL);

    /* get the test expression, then clause and else clause */
    testexpr = xlarg(&args);
    thenexpr = xlarg(&args);
    elseexpr = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* evaluate the appropriate clause */
    val = xleval(xleval(testexpr) ? thenexpr : elseexpr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the last value */
    return (val);
}

/* xlet - built-in function 'let' */
NODE *xlet(args)
  NODE *args;
{
    return (let(args,TRUE));
}

/* xletstar - built-in function 'let*' */
NODE *xletstar(args)
  NODE *args;
{
    return (let(args,FALSE));
}

/* let - common let routine */
LOCAL NODE *let(args,pflag)
  NODE *args; int pflag;
{
    NODE ***oldstk,*newenv,*arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&newenv,&arg,NULL);

    /* initialize */
    arg = args;

    /* create a new environment frame */
    newenv = xlframe(xlenv);

    /* get the list of bindings and bind the symbols */
    if (!pflag) xlenv = newenv;
    dobindings(xlmatch(LIST,&arg),newenv);
    if (pflag) xlenv = newenv;

    /* execute the code */
    for (val = NIL; arg; )
	val = xlevarg(&arg);

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xprog - built-in function 'prog' */
NODE *xprog(args)
  NODE *args;
{
    return (prog(args,TRUE));
}

/* xprogstar - built-in function 'prog*' */
NODE *xprogstar(args)
  NODE *args;
{
    return (prog(args,FALSE));
}

/* prog - common prog routine */
LOCAL NODE *prog(args,pflag)
  NODE *args; int pflag;
{
    NODE ***oldstk,*newenv,*arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&newenv,&arg,NULL);

    /* initialize */
    arg = args;

    /* create a new environment frame */
    newenv = xlframe(xlenv);

    /* get the list of bindings and bind the symbols */
    if (!pflag) xlenv = newenv;
    dobindings(xlmatch(LIST,&arg),newenv);
    if (pflag) xlenv = newenv;

    /* execute the code */
    tagblock(arg,&val);

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xgo - built-in function 'go' */
NODE *xgo(args)
  NODE *args;
{
    NODE *label;

    /* get the target label */
    label = xlarg(&args);
    xllastarg(args);

    /* transfer to the label */
    xlgo(label);
}

/* xreturn - built-in function 'return' */
NODE *xreturn(args)
  NODE *args;
{
    NODE *val;

    /* get the return value */
    val = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* return from the inner most block */
    xlreturn(val);
}

/* xprog1 - built-in function 'prog1' */
NODE *xprog1(args)
  NODE *args;
{
    return (progx(args,1));
}

/* xprog2 - built-in function 'prog2' */
NODE *xprog2(args)
  NODE *args;
{
    return (progx(args,2));
}

/* progx - common progx code */
LOCAL NODE *progx(args,n)
  NODE *args; int n;
{
    NODE ***oldstk,*arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&val,NULL);

    /* initialize */
    arg = args;

    /* evaluate the first n expressions */
    while (n--)
	val = xlevarg(&arg);

    /* evaluate each remaining argument */
    while (arg)
	xlevarg(&arg);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the last test expression value */
    return (val);
}

/* xprogn - built-in function 'progn' */
NODE *xprogn(args)
  NODE *args;
{
    NODE ***oldstk,*arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg = args;

    /* evaluate each remaining argument */
    for (val = NIL; arg; )
	val = xlevarg(&arg);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the last test expression value */
    return (val);
}

/* xdo - built-in function 'do' */
NODE *xdo(args)
  NODE *args;
{
    return (doloop(args,TRUE));
}

/* xdostar - built-in function 'do*' */
NODE *xdostar(args)
  NODE *args;
{
    return (doloop(args,FALSE));
}

/* doloop - common do routine */
LOCAL NODE *doloop(args,pflag)
  NODE *args; int pflag;
{
    NODE ***oldstk,*newenv,*arg,*blist,*clist,*test,*rval;
    int rbreak;

    /* create a new stack frame */
    oldstk = xlsave(&newenv,&arg,&blist,&clist,&test,NULL);

    /* initialize */
    arg = args;

    /* get the list of bindings */
    blist = xlmatch(LIST,&arg);

    /* create a new environment frame */
    newenv = xlframe(xlenv);

    /* bind the symbols */
    if (!pflag) xlenv = newenv;
    dobindings(blist,newenv);
    if (pflag) xlenv = newenv;

    /* get the exit test and result forms */
    clist = xlmatch(LIST,&arg);
    test = xlarg(&clist);

    /* execute the loop as long as the test is false */
    rbreak = FALSE;
    while (xleval(test) == NIL) {

	/* execute the body of the loop */
	if (tagblock(arg,&rval)) {
	    rbreak = TRUE;
	    break;
	}

	/* update the looping variables */
	doupdates(blist,pflag);
    }

    /* evaluate the result expression */
    if (!rbreak)
	for (rval = NIL; consp(clist); )
	    rval = xlevarg(&clist);

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (rval);
}

/* xdolist - built-in function 'dolist' */
NODE *xdolist(args)
  NODE *args;
{
    NODE ***oldstk,*arg,*clist,*sym,*list,*val,*rval;
    int rbreak;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&clist,&sym,&list,&val,NULL);

    /* initialize */
    arg = args;

    /* get the control list (sym list result-expr) */
    clist = xlmatch(LIST,&arg);
    sym = xlmatch(SYM,&clist);
    list = xlevmatch(LIST,&clist);
    val = (clist ? xlarg(&clist) : NIL);

    /* initialize the local environment */
    xlenv = xlframe(xlenv);
    xlbind(sym,NIL,xlenv);

    /* loop through the list */
    rbreak = FALSE;
    for (; consp(list); list = cdr(list)) {

	/* bind the symbol to the next list element */
	xlsetvalue(sym,car(list));

	/* execute the loop body */
	if (tagblock(arg,&rval)) {
	    rbreak = TRUE;
	    break;
	}
    }

    /* evaluate the result expression */
    if (!rbreak) {
	xlsetvalue(sym,NIL);
	rval = xleval(val);
    }

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (rval);
}

/* xdotimes - built-in function 'dotimes' */
NODE *xdotimes(args)
  NODE *args;
{
    NODE ***oldstk,*arg,*clist,*sym,*val,*rval;
    int rbreak,cnt,i;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&clist,&sym,&val,NULL);

    /* initialize */
    arg = args;

    /* get the control list (sym list result-expr) */
    clist = xlmatch(LIST,&arg);
    sym = xlmatch(SYM,&clist);
    cnt = getfixnum(xlevmatch(INT,&clist));
    val = (clist ? xlarg(&clist) : NIL);

    /* initialize the local environment */
    xlenv = xlframe(xlenv);
    xlbind(sym,NIL,xlenv);

    /* loop through for each value from zero to cnt-1 */
    rbreak = FALSE;
    for (i = 0; i < cnt; i++) {

	/* bind the symbol to the next list element */
	xlsetvalue(sym,cvfixnum((FIXNUM)i));

	/* execute the loop body */
	if (tagblock(arg,&rval)) {
	    rbreak = TRUE;
	    break;
	}
    }

    /* evaluate the result expression */
    if (!rbreak) {
	xlsetvalue(sym,cvfixnum((FIXNUM)cnt));
	rval = xleval(val);
    }

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (rval);
}

/* xcatch - built-in function 'catch' */
NODE *xcatch(args)
  NODE *args;
{
    NODE ***oldstk,*tag,*arg,*val;
    CONTEXT cntxt;

    /* create a new stack frame */
    oldstk = xlsave(&tag,&arg,NULL);

    /* initialize */
    tag = xlevarg(&args);
    arg = args;
    val = NIL;

    /* establish an execution context */
    xlbegin(&cntxt,CF_THROW,tag);

    /* check for 'throw' */
    if (setjmp(cntxt.c_jmpbuf))
	val = xlvalue;

    /* otherwise, evaluate the remainder of the arguments */
    else {
	while (arg)
	    val = xlevarg(&arg);
    }
    xlend(&cntxt);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xthrow - built-in function 'throw' */
NODE *xthrow(args)
  NODE *args;
{
    NODE *tag,*val;

    /* get the tag and value */
    tag = xlarg(&args);
    val = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* throw the tag */
    xlthrow(tag,val);
}

/* xerror - built-in function 'error' */
NODE *xerror(args)
  NODE *args;
{
    char *emsg; NODE *arg;

    /* get the error message and the argument */
    emsg = getstring(xlmatch(STR,&args));
    arg = (args ? xlarg(&args) : s_unbound);
    xllastarg(args);

    /* signal the error */
    xlerror(emsg,arg);
}

/* xcerror - built-in function 'cerror' */
NODE *xcerror(args)
  NODE *args;
{
    char *cmsg,*emsg; NODE *arg;

    /* get the correction message, the error message, and the argument */
    cmsg = getstring(xlmatch(STR,&args));
    emsg = getstring(xlmatch(STR,&args));
    arg = (args ? xlarg(&args) : s_unbound);
    xllastarg(args);

    /* signal the error */
    xlcerror(cmsg,emsg,arg);

    /* return nil */
    return (NIL);
}

/* xbreak - built-in function 'break' */
NODE *xbreak(args)
  NODE *args;
{
    char *emsg; NODE *arg;

    /* get the error message */
    emsg = (args ? getstring(xlmatch(STR,&args)) : "**BREAK**");
    arg = (args ? xlarg(&args) : s_unbound);
    xllastarg(args);

    /* enter the break loop */
    xlbreak(emsg,arg);

    /* return nil */
    return (NIL);
}

/* xcleanup - built-in function 'clean-up' */
NODE *xcleanup(args)
  NODE *args;
{
    xllastarg(args);
    xlcleanup();
}

/* xcontinue - built-in function 'continue' */
NODE *xcontinue(args)
  NODE *args;
{
    xllastarg(args);
    xlcontinue();
}

/* xerrset - built-in function 'errset' */
NODE *xerrset(args)
  NODE *args;
{
    NODE ***oldstk,*expr,*flag,*val;
    CONTEXT cntxt;

    /* create a new stack frame */
    oldstk = xlsave(&expr,&flag,NULL);

    /* get the expression and the print flag */
    expr = xlarg(&args);
    flag = (args ? xlarg(&args) : true);
    xllastarg(args);

    /* establish an execution context */
    xlbegin(&cntxt,CF_ERROR,flag);

    /* check for error */
    if (setjmp(cntxt.c_jmpbuf))
	val = NIL;

    /* otherwise, evaluate the expression */
    else {
	expr = xleval(expr);
	val = consa(expr);
    }
    xlend(&cntxt);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xevalhook - eval hook function */
NODE *xevalhook(args)
  NODE *args;
{
    NODE ***oldstk,*expr,*ehook,*ahook,*env,*newehook,*newahook,*newenv,*val;

    /* create a new stack frame */
    oldstk = xlsave(&expr,&ehook,&ahook,&env,&newehook,&newahook,&newenv,NULL);

    /* get the expression, the new hook functions and the environment */
    expr = xlarg(&args);
    newehook = xlarg(&args);
    newahook = xlarg(&args);
    newenv = (args ? xlarg(&args) : xlenv);
    xllastarg(args);

    /* bind *evalhook* and *applyhook* to the hook functions */
    ehook = getvalue(s_evalhook);
    setvalue(s_evalhook,newehook);
    ahook = getvalue(s_applyhook);
    setvalue(s_applyhook,newahook);
    env = xlenv;
    xlenv = newenv;

    /* evaluate the expression (bypassing *evalhook*) */
    val = xlxeval(expr);

    /* unbind the hook variables */
    setvalue(s_evalhook,ehook);
    setvalue(s_applyhook,ahook);
    xlenv = env;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* dobindings - handle bindings for let/let*, prog/prog*, do/do* */
LOCAL dobindings(blist,env)
  NODE *blist,*env;
{
    NODE ***oldstk,*list,*bnd,*sym,*val;

    /* create a new stack frame */
    oldstk = xlsave(&list,&bnd,&sym,&val,NULL);

    /* bind each symbol in the list of bindings */
    for (list = blist; consp(list); list = cdr(list)) {

	/* get the next binding */
	bnd = car(list);

	/* handle a symbol */
	if (symbolp(bnd)) {
	    sym = bnd;
	    val = NIL;
	}

	/* handle a list of the form (symbol expr) */
	else if (consp(bnd)) {
	    sym = xlmatch(SYM,&bnd);
	    val = xlevarg(&bnd);
	}
	else
	    xlfail("bad binding");

	/* bind the value to the symbol */
	xlbind(sym,val,env);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;
}

/* doupdates - handle updates for do/do* */
doupdates(blist,pflag)
  NODE *blist; int pflag;
{
    NODE ***oldstk,*plist,*list,*bnd,*sym,*val;

    /* create a new stack frame */
    oldstk = xlsave(&plist,&list,&bnd,&sym,&val,NULL);

    /* bind each symbol in the list of bindings */
    for (list = blist; consp(list); list = cdr(list)) {

	/* get the next binding */
	bnd = car(list);

	/* handle a list of the form (symbol expr) */
	if (consp(bnd)) {
	    sym = xlmatch(SYM,&bnd);
	    bnd = cdr(bnd);
	    if (bnd) {
		val = xlevarg(&bnd);
		if (pflag) {
		    plist = consd(plist);
		    rplaca(plist,cons(sym,val));
		}
		else
		    xlsetvalue(sym,val);
	    }
	}
    }

    /* set the values for parallel updates */
    for (; plist; plist = cdr(plist))
	xlsetvalue(car(car(plist)),cdr(car(plist)));

    /* restore the previous stack frame */
    xlstack = oldstk;
}

/* tagblock - execute code within a block and tagbody */
int tagblock(code,pval)
  NODE *code,**pval;
{
    NODE ***oldstk,*arg;
    CONTEXT cntxt;
    int type,sts;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg = code;

    /* establish an execution context */
    xlbegin(&cntxt,CF_GO|CF_RETURN,arg);

    /* check for a 'return' */
    if ((type = setjmp(cntxt.c_jmpbuf)) == CF_RETURN) {
	*pval = xlvalue;
	sts = TRUE;
    }

    /* otherwise, enter the body */
    else {

	/* check for a 'go' */
	if (type == CF_GO)
	    arg = xlvalue;

	/* evaluate each expression in the body */
	while (consp(arg))
	    if (consp(car(arg)))
		xlevarg(&arg);
	    else
		arg = cdr(arg);

	/* fell out the bottom of the loop */
	*pval = NIL;
	sts = FALSE;
    }
    xlend(&cntxt);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return status */
    return (sts);
}

