/* xlcont - xlisp control built-in functions */

#include "xlisp.h"

/* external variables */
extern NODE *xlstack,*xlenv,*xlnewenv,*xlvalue;
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
    NODE *oldstk,arg,list,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&list,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* initialize the return value */
    val = NIL;

    /* find a predicate that is true */
    while (arg.n_ptr) {

	/* get the next conditional */
	list.n_ptr = xlmatch(LIST,&arg.n_ptr);

	/* evaluate the predicate part */
	if (xlevarg(&list.n_ptr)) {

	    /* evaluate each expression */
	    while (list.n_ptr)
		val = xlevarg(&list.n_ptr);

	    /* exit the loop */
	    break;
	}
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* xand - built-in function 'and' */
NODE *xand(args)
  NODE *args;
{
    NODE *oldstk,arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg.n_ptr = args;
    val = true;

    /* evaluate each argument */
    while (arg.n_ptr)

	/* get the next argument */
	if ((val = xlevarg(&arg.n_ptr)) == NIL)
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
    NODE *oldstk,arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg.n_ptr = args;
    val = NIL;

    /* evaluate each argument */
    while (arg.n_ptr)
	if ((val = xlevarg(&arg.n_ptr)))
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
    NODE *oldstk,testexpr,thenexpr,elseexpr,*val;

    /* create a new stack frame */
    oldstk = xlsave(&testexpr,&thenexpr,&elseexpr,NULL);

    /* get the test expression, then clause and else clause */
    testexpr.n_ptr = xlarg(&args);
    thenexpr.n_ptr = xlarg(&args);
    elseexpr.n_ptr = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* evaluate the appropriate clause */
    val = xleval(xleval(testexpr.n_ptr) ? thenexpr.n_ptr : elseexpr.n_ptr);

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
    NODE *oldstk,*oldenv,*oldnewenv,arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* get the list of bindings and bind the symbols */
    oldnewenv = xlnewenv; oldenv = xlnewenv = xlenv;
    dobindings(xlmatch(LIST,&arg.n_ptr),pflag);

    /* execute the code */
    for (val = NIL; arg.n_ptr; )
	val = xlevarg(&arg.n_ptr);

    /* unbind the arguments */
    xlunbind(oldenv); xlnewenv = oldnewenv;

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
    NODE *oldstk,*oldenv,*oldnewenv,arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* get the list of bindings and bind the symbols */
    oldnewenv = xlnewenv; oldenv = xlnewenv = xlenv;
    dobindings(xlmatch(LIST,&arg.n_ptr),pflag);

    /* execute the code */
    tagblock(arg.n_ptr,&val);

    /* unbind the arguments */
    xlunbind(oldenv); xlnewenv = oldnewenv;

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
    NODE *oldstk,arg,val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&val,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* evaluate the first n expressions */
    while (n--)
	val.n_ptr = xlevarg(&arg.n_ptr);

    /* evaluate each remaining argument */
    while (arg.n_ptr)
	xlevarg(&arg.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the last test expression value */
    return (val.n_ptr);
}

/* xprogn - built-in function 'progn' */
NODE *xprogn(args)
  NODE *args;
{
    NODE *oldstk,arg,*val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* evaluate each remaining argument */
    for (val = NIL; arg.n_ptr; )
	val = xlevarg(&arg.n_ptr);

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
    NODE *oldstk,*oldenv,*oldnewenv,arg,blist,clist,test,*rval;
    int rbreak;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&blist,&clist,&test,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* get the list of bindings and bind the symbols */
    blist.n_ptr = xlmatch(LIST,&arg.n_ptr);
    oldnewenv = xlnewenv; oldenv = xlnewenv = xlenv;
    dobindings(blist.n_ptr,pflag);

    /* get the exit test and result forms */
    clist.n_ptr = xlmatch(LIST,&arg.n_ptr);
    test.n_ptr = xlarg(&clist.n_ptr);

    /* execute the loop as long as the test is false */
    rbreak = FALSE;
    while (xleval(test.n_ptr) == NIL) {

	/* execute the body of the loop */
	if (tagblock(arg.n_ptr,&rval)) {
	    rbreak = TRUE;
	    break;
	}

	/* update the looping variables */
	doupdates(blist.n_ptr,pflag);
    }

    /* evaluate the result expression */
    if (!rbreak)
	for (rval = NIL; consp(clist.n_ptr); )
	    rval = xlevarg(&clist.n_ptr);

    /* unbind the arguments */
    xlunbind(oldenv); xlnewenv = oldnewenv;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (rval);
}

/* xdolist - built-in function 'dolist' */
NODE *xdolist(args)
  NODE *args;
{
    NODE *oldstk,*oldenv,arg,clist,sym,list,val,*rval;
    int rbreak;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&clist,&sym,&list,&val,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* get the control list (sym list result-expr) */
    clist.n_ptr = xlmatch(LIST,&arg.n_ptr);
    sym.n_ptr = xlmatch(SYM,&clist.n_ptr);
    list.n_ptr = xlevmatch(LIST,&clist.n_ptr);
    val.n_ptr = (clist.n_ptr ? xlarg(&clist.n_ptr) : NIL);

    /* initialize the local environment */
    oldenv = xlenv;
    xlsbind(sym.n_ptr,NIL);

    /* loop through the list */
    rbreak = FALSE;
    for (; consp(list.n_ptr); list.n_ptr = cdr(list.n_ptr)) {

	/* bind the symbol to the next list element */
	sym.n_ptr->n_symvalue = car(list.n_ptr);

	/* execute the loop body */
	if (tagblock(arg.n_ptr,&rval)) {
	    rbreak = TRUE;
	    break;
	}
    }

    /* evaluate the result expression */
    if (!rbreak) {
	sym.n_ptr->n_symvalue = NIL;
	rval = xleval(val.n_ptr);
    }

    /* unbind the arguments */
    xlunbind(oldenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (rval);
}

/* xdotimes - built-in function 'dotimes' */
NODE *xdotimes(args)
  NODE *args;
{
    NODE *oldstk,*oldenv,arg,clist,sym,val,*rval;
    int rbreak,cnt,i;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&clist,&sym,&val,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* get the control list (sym list result-expr) */
    clist.n_ptr = xlmatch(LIST,&arg.n_ptr);
    sym.n_ptr = xlmatch(SYM,&clist.n_ptr);
    cnt = xlevmatch(INT,&clist.n_ptr)->n_int;
    val.n_ptr = (clist.n_ptr ? xlarg(&clist.n_ptr) : NIL);

    /* initialize the local environment */
    oldenv = xlenv;
    xlsbind(sym.n_ptr,NIL);

    /* loop through for each value from zero to cnt-1 */
    rbreak = FALSE;
    for (i = 0; i < cnt; i++) {

	/* bind the symbol to the next list element */
	sym.n_ptr->n_symvalue = newnode(INT);
	sym.n_ptr->n_symvalue->n_int = i;

	/* execute the loop body */
	if (tagblock(arg.n_ptr,&rval)) {
	    rbreak = TRUE;
	    break;
	}
    }

    /* evaluate the result expression */
    if (!rbreak) {
	sym.n_ptr->n_symvalue = newnode(INT);
	sym.n_ptr->n_symvalue->n_int = cnt;
	rval = xleval(val.n_ptr);
    }

    /* unbind the arguments */
    xlunbind(oldenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (rval);
}

/* xcatch - built-in function 'catch' */
NODE *xcatch(args)
  NODE *args;
{
    NODE *oldstk,tag,arg,*val;
    CONTEXT cntxt;

    /* create a new stack frame */
    oldstk = xlsave(&tag,&arg,NULL);

    /* initialize */
    tag.n_ptr = xlevarg(&args);
    arg.n_ptr = args;
    val = NIL;

    /* establish an execution context */
    xlbegin(&cntxt,CF_THROW,tag.n_ptr);

    /* check for 'throw' */
    if (setjmp(cntxt.c_jmpbuf))
	val = xlvalue;

    /* otherwise, evaluate the remainder of the arguments */
    else {
	while (arg.n_ptr)
	    val = xlevarg(&arg.n_ptr);
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
    emsg = xlmatch(STR,&args)->n_str;
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
    cmsg = xlmatch(STR,&args)->n_str;
    emsg = xlmatch(STR,&args)->n_str;
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
    emsg = (args ? xlmatch(STR,&args)->n_str : "**BREAK**");
    arg = (args ? xlarg(&args) : s_unbound);
    xllastarg(args);

    /* enter the break loop */
    xlbreak(emsg,arg);

    /* return nil */
    return (NIL);
}

/* xerrset - built-in function 'errset' */
NODE *xerrset(args)
  NODE *args;
{
    NODE *oldstk,expr,flag,*val;
    CONTEXT cntxt;

    /* create a new stack frame */
    oldstk = xlsave(&expr,&flag,NULL);

    /* get the expression and the print flag */
    expr.n_ptr = xlarg(&args);
    flag.n_ptr = (args ? xlarg(&args) : true);
    xllastarg(args);

    /* establish an execution context */
    xlbegin(&cntxt,CF_ERROR,flag.n_ptr);

    /* check for error */
    if (setjmp(cntxt.c_jmpbuf))
	val = NIL;

    /* otherwise, evaluate the expression */
    else {
	expr.n_ptr = xleval(expr.n_ptr);
	val = newnode(LIST);
	rplaca(val,expr.n_ptr);
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
    NODE *oldstk,*oldenv,expr,ehook,ahook,*val;

    /* create a new stack frame */
    oldstk = xlsave(&expr,&ehook,&ahook,NULL);

    /* get the expression and the hook functions */
    expr.n_ptr = xlarg(&args);
    ehook.n_ptr = xlarg(&args);
    ahook.n_ptr = xlarg(&args);
    xllastarg(args);

    /* bind *evalhook* and *applyhook* to the hook functions */
    oldenv = xlenv;
    xlsbind(s_evalhook,ehook.n_ptr);
    xlsbind(s_applyhook,ahook.n_ptr);

    /* evaluate the expression (bypassing *evalhook*) */
    val = xlxeval(expr.n_ptr);

    /* unbind the hook variables */
    xlunbind(oldenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* dobindings - handle bindings for let/let*, prog/prog*, do/do* */
LOCAL dobindings(blist,pflag)
  NODE *blist; int pflag;
{
    NODE *oldstk,list,bnd,sym,val;

    /* create a new stack frame */
    oldstk = xlsave(&list,&bnd,&sym,&val,NULL);

   /* bind each symbol in the list of bindings */
    for (list.n_ptr = blist; consp(list.n_ptr); list.n_ptr = cdr(list.n_ptr)) {

	/* get the next binding */
	bnd.n_ptr = car(list.n_ptr);

	/* handle a symbol */
	if (symbolp(bnd.n_ptr)) {
	    sym.n_ptr = bnd.n_ptr;
	    val.n_ptr = NIL;
	}

	/* handle a list of the form (symbol expr) */
	else if (consp(bnd.n_ptr)) {
	    sym.n_ptr = xlmatch(SYM,&bnd.n_ptr);
	    val.n_ptr = xlevarg(&bnd.n_ptr);
	}
	else
	    xlfail("bad binding");

	/* bind the value to the symbol */
	if (pflag)
	    xlbind(sym.n_ptr,val.n_ptr);
	else
	    xlsbind(sym.n_ptr,val.n_ptr);
    }

    /* fix the bindings on a parallel let */
    if (pflag)
	xlfixbindings();

    /* restore the previous stack frame */
    xlstack = oldstk;
}

/* doupdates - handle updates for do/do* */
doupdates(blist,pflag)
  NODE *blist; int pflag;
{
    NODE *oldstk,*oldenv,*oldnewenv,list,bnd,sym,val;

    /* create a new stack frame */
    oldstk = xlsave(&list,&bnd,&sym,&val,NULL);

    /* initialize the local environment */
    if (pflag) {
	oldenv = xlenv; oldnewenv = xlnewenv;
    }

    /* bind each symbol in the list of bindings */
    for (list.n_ptr = blist; consp(list.n_ptr); list.n_ptr = cdr(list.n_ptr)) {

	/* get the next binding */
	bnd.n_ptr = car(list.n_ptr);

	/* handle a list of the form (symbol expr) */
	if (consp(bnd.n_ptr)) {
	    sym.n_ptr = xlmatch(SYM,&bnd.n_ptr);
	    bnd.n_ptr = cdr(bnd.n_ptr);
	    if (bnd.n_ptr) {
		val.n_ptr = xlevarg(&bnd.n_ptr);
		if (pflag)
		    xlbind(sym.n_ptr,val.n_ptr);
		else
		    sym.n_ptr->n_symvalue = val.n_ptr;
	    }
	}
    }

    /* fix the bindings on a parallel let */
    if (pflag) {
	xlfixbindings();
	xlenv = oldenv; xlnewenv = oldnewenv;
    }

    /* restore the previous stack frame */
    xlstack = oldstk;
}

/* tagblock - execute code within a block and tagbody */
int tagblock(code,pval)
  NODE *code,**pval;
{
    NODE *oldstk,arg;
    CONTEXT cntxt;
    int type,sts;

    /* create a new stack frame */
    oldstk = xlsave(&arg,NULL);

    /* initialize */
    arg.n_ptr = code;

    /* establish an execution context */
    xlbegin(&cntxt,CF_GO|CF_RETURN,arg.n_ptr);

    /* check for a 'return' */
    if ((type = setjmp(cntxt.c_jmpbuf)) == CF_RETURN) {
	*pval = xlvalue;
	sts = TRUE;
    }

    /* otherwise, enter the body */
    else {

	/* check for a 'go' */
	if (type == CF_GO)
	    arg.n_ptr = xlvalue;

	/* evaluate each expression in the body */
	while (consp(arg.n_ptr))
	    if (consp(car(arg.n_ptr)))
		xlevarg(&arg.n_ptr);
	    else
		arg.n_ptr = cdr(arg.n_ptr);
	
	/* indicate that we fell through the bottom of the tagbody */
	*pval = NIL;
	sts = FALSE;
    }
    xlend(&cntxt);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return status */
    return (sts);
}
