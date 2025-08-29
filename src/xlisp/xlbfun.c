/* xlbfun.c - xlisp basic builtin functions */

#include "xlisp.h"

/* external variables */
extern NODE *xlstack;
extern NODE *s_lambda,*s_macro;
extern NODE *s_comma,*s_comat;
extern NODE *s_unbound;
extern char gsprefix[];
extern int gsnumber;

/* forward declarations */
FORWARD NODE *bquote1();
FORWARD NODE *defun();
FORWARD NODE *makesymbol();

/* xeval - the builtin function 'eval' */
NODE *xeval(args)
  NODE *args;
{
    NODE *oldstk,expr,*val;

    /* create a new stack frame */
    oldstk = xlsave(&expr,NULL);

    /* get the expression to evaluate */
    expr.n_ptr = xlarg(&args);
    xllastarg(args);

    /* evaluate the expression */
    val = xleval(expr.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the expression evaluated */
    return (val);
}

/* xapply - the builtin function 'apply' */
NODE *xapply(args)
  NODE *args;
{
    NODE *oldstk,fun,arglist,*val;

    /* create a new stack frame */
    oldstk = xlsave(&fun,&arglist,NULL);

    /* get the function and argument list */
    fun.n_ptr = xlarg(&args);
    arglist.n_ptr = xlarg(&args);
    xllastarg(args);

    /* if the function is a symbol, get its value */
    if (symbolp(fun.n_ptr))
	fun.n_ptr = xleval(fun.n_ptr);

    /* apply the function to the arguments */
    val = xlapply(fun.n_ptr,arglist.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the expression evaluated */
    return (val);
}

/* xfuncall - the builtin function 'funcall' */
NODE *xfuncall(args)
  NODE *args;
{
    NODE *oldstk,fun,arglist,*val;

    /* create a new stack frame */
    oldstk = xlsave(&fun,&arglist,NULL);

    /* get the function and argument list */
    fun.n_ptr = xlarg(&args);
    arglist.n_ptr = args;

    /* if the function is a symbol, get its value */
    if (symbolp(fun.n_ptr))
	fun.n_ptr = xleval(fun.n_ptr);

    /* apply the function to the arguments */
    val = xlapply(fun.n_ptr,arglist.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the expression evaluated */
    return (val);
}

/* xquote - builtin function to quote an expression */
NODE *xquote(args)
  NODE *args;
{
    NODE *arg;

    /* get the argument */
    arg = xlarg(&args);
    xllastarg(args);

    /* return the quoted expression */
    return (arg);
}

/* xbquote - back quote function */
NODE *xbquote(args)
  NODE *args;
{
    NODE *oldstk,expr,*val;

    /* create a new stack frame */
    oldstk = xlsave(&expr,NULL);

    /* get the expression */
    expr.n_ptr = xlarg(&args);
    xllastarg(args);

    /* fill in the template */
    val = bquote1(expr.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* bquote1 - back quote helper function */
LOCAL NODE *bquote1(expr)
  NODE *expr;
{
    NODE *oldstk,val,list,*last,*new;

    /* handle atoms */
    if (atom(expr))
	val.n_ptr = expr;

    /* handle (comma <expr>) */
    else if (car(expr) == s_comma) {
	if (atom(cdr(expr)))
	    xlfail("bad comma expression");
	val.n_ptr = xleval(car(cdr(expr)));
    }

    /* handle ((comma-at <expr>) ... ) */
    else if (consp(car(expr)) && car(car(expr)) == s_comat) {
	oldstk = xlsave(&list,&val,NULL);
	if (atom(cdr(car(expr))))
	    xlfail("bad comma-at expression");
	list.n_ptr = xleval(car(cdr(car(expr))));
	for (last = NIL; consp(list.n_ptr); list.n_ptr = cdr(list.n_ptr)) {
	    new = newnode(LIST);
	    rplaca(new,car(list.n_ptr));
	    if (last)
		rplacd(last,new);
	    else
		val.n_ptr = new;
	    last = new;
	}
	if (last)
	    rplacd(last,bquote1(cdr(expr)));
	else
	    val.n_ptr = bquote1(cdr(expr));
	xlstack = oldstk;
    }

    /* handle any other list */
    else {
	oldstk = xlsave(&val,NULL);
	val.n_ptr = newnode(LIST);
	rplaca(val.n_ptr,bquote1(car(expr)));
	rplacd(val.n_ptr,bquote1(cdr(expr)));
	xlstack = oldstk;
    }

    /* return the result */
    return (val.n_ptr);
}

/* xset - builtin function set */
NODE *xset(args)
  NODE *args;
{
    NODE *sym,*val;

    /* get the symbol and new value */
    sym = xlmatch(SYM,&args);
    val = xlarg(&args);
    xllastarg(args);

    /* assign the symbol the value of argument 2 and the return value */
    assign(sym,val);

    /* return the result value */
    return (val);
}

/* xsetq - builtin function setq */
NODE *xsetq(args)
  NODE *args;
{
    NODE *oldstk,arg,sym,val;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&sym,&val,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* handle each pair of arguments */
    while (arg.n_ptr) {
	sym.n_ptr = xlmatch(SYM,&arg.n_ptr);
	val.n_ptr = xlevarg(&arg.n_ptr);
	assign(sym.n_ptr,val.n_ptr);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val.n_ptr);
}

/* xdefun - builtin function 'defun' */
NODE *xdefun(args)
  NODE *args;
{
    return (defun(args,s_lambda));
}

/* xdefmacro - builtin function 'defmacro' */
NODE *xdefmacro(args)
  NODE *args;
{
    return (defun(args,s_macro));
}

/* defun - internal function definition routine */
LOCAL NODE *defun(args,type)
  NODE *args,*type;
{
    NODE *oldstk,sym,fargs,fun;

    /* create a new stack frame */
    oldstk = xlsave(&sym,&fargs,&fun,NULL);

    /* get the function symbol and formal argument list */
    sym.n_ptr = xlmatch(SYM,&args);
    fargs.n_ptr = xlmatch(LIST,&args);

    /* create a new function definition */
    fun.n_ptr = newnode(LIST);
    rplaca(fun.n_ptr,type);
    rplacd(fun.n_ptr,newnode(LIST));
    rplaca(cdr(fun.n_ptr),fargs.n_ptr);
    rplacd(cdr(fun.n_ptr),args);

    /* make the symbol point to a new function definition */
    assign(sym.n_ptr,fun.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the function symbol */
    return (sym.n_ptr);
}

/* xgensym - generate a symbol */
NODE *xgensym(args)
  NODE *args;
{
    char sym[STRMAX+1];
    NODE *x;

    /* get the prefix or number */
    if (args) {
	x = xlarg(&args);
	switch (ntype(x)) {
	case STR:
		strcpy(gsprefix,x->n_str);
		break;
	case INT:
		gsnumber = x->n_int;
		break;
	default:
		xlfail("bad argument type");
	}
    }
    xllastarg(args);

    /* create the pname of the new symbol */
    sprintf(sym,"%s%d",gsprefix,gsnumber++);

    /* make a symbol with this print name */
    return (xlmakesym(sym,DYNAMIC));
}

/* xmakesymbol - make a new uninterned symbol */
NODE *xmakesymbol(args)
  NODE *args;
{
    return (makesymbol(args,FALSE));
}

/* xintern - make a new interned symbol */
NODE *xintern(args)
  NODE *args;
{
    return (makesymbol(args,TRUE));
}

/* makesymbol - make a new symbol */
LOCAL NODE *makesymbol(args,iflag)
  NODE *args; int iflag;
{
    NODE *oldstk,pname,*val;
    char *str;

    /* create a new stack frame */
    oldstk = xlsave(&pname,NULL);

    /* get the print name of the symbol to intern */
    pname.n_ptr = xlmatch(STR,&args);
    xllastarg(args);

    /* make the symbol */
    str = pname.n_ptr->n_str;
    val = (iflag ? xlenter(str,DYNAMIC) : xlmakesym(str,DYNAMIC));

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the symbol */
    return (val);
}

/* xsymname - get the print name of a symbol */
NODE *xsymname(args)
  NODE *args;
{
    NODE *sym;

    /* get the symbol */
    sym = xlmatch(SYM,&args);
    xllastarg(args);

    /* return the print name */
    return (car(sym->n_symplist));
}

/* xsymvalue - get the print value of a symbol */
NODE *xsymvalue(args)
  NODE *args;
{
    NODE *sym;

    /* get the symbol */
    sym = xlmatch(SYM,&args);
    xllastarg(args);

    /* check for an unbound symbol */
    while (sym->n_symvalue == s_unbound)
	xlunbound(sym);

    /* return the value */
    return (sym->n_symvalue);
}

/* xsymplist - get the property list of a symbol */
NODE *xsymplist(args)
  NODE *args;
{
    NODE *sym;

    /* get the symbol */
    sym = xlmatch(SYM,&args);
    xllastarg(args);

    /* return the property list */
    return (cdr(sym->n_symplist));
}

/* xget - get the value of a property */
NODE *xget(args)
  NODE *args;
{
    NODE *sym,*prp;

    /* get the symbol and property */
    sym = xlmatch(SYM,&args);
    prp = xlmatch(SYM,&args);
    xllastarg(args);

    /* retrieve the property value */
    return (xlgetprop(sym,prp));
}

/* xremprop - remove a property value from a property list */
NODE *xremprop(args)
  NODE *args;
{
    NODE *sym,*prp;

    /* get the symbol and property */
    sym = xlmatch(SYM,&args);
    prp = xlmatch(SYM,&args);
    xllastarg(args);

    /* remove the property */
    xlremprop(sym,prp);

    /* return nil */
    return (NIL);
}
