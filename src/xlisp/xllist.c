/* xllist - xlisp built-in list functions */

#include "xlisp.h"

#ifdef MEGAMAX
overlay "overflow"
#endif

/* external variables */
extern NODE *xlstack;
extern NODE *s_unbound;
extern NODE *true;

/* external routines */
extern int eq(),eql(),equal();

/* forward declarations */
FORWARD NODE *cxr();
FORWARD NODE *nth(),*assoc();
FORWARD NODE *subst(),*sublis(),*map();
FORWARD NODE *cequal();

/* xcar - return the car of a list */
NODE *xcar(args)
  NODE *args;
{
    return (cxr(args,"a"));
}

/* xcdr - return the cdr of a list */
NODE *xcdr(args)
  NODE *args;
{
    return (cxr(args,"d"));
}

/* xcaar - return the caar of a list */
NODE *xcaar(args)
  NODE *args;
{
    return (cxr(args,"aa"));
}

/* xcadr - return the cadr of a list */
NODE *xcadr(args)
  NODE *args;
{
    return (cxr(args,"da"));
}

/* xcdar - return the cdar of a list */
NODE *xcdar(args)
  NODE *args;
{
    return (cxr(args,"ad"));
}

/* xcddr - return the cddr of a list */
NODE *xcddr(args)
  NODE *args;
{
    return (cxr(args,"dd"));
}

/* cxr - common car/cdr routine */
LOCAL NODE *cxr(args,adstr)
  NODE *args; char *adstr;
{
    NODE *list;

    /* get the list */
    list = xlmatch(LIST,&args);
    xllastarg(args);

    /* perform the car/cdr operations */
    while (*adstr && consp(list))
	list = (*adstr++ == 'a' ? car(list) : cdr(list));

    /* make sure the operation succeeded */
    if (*adstr && list)
	xlfail("bad argument");

    /* return the result */
    return (list);
}

/* xcons - construct a new list cell */
NODE *xcons(args)
  NODE *args;
{
    NODE *arg1,*arg2,*val;

    /* get the two arguments */
    arg1 = xlarg(&args);
    arg2 = xlarg(&args);
    xllastarg(args);

    /* construct a new list element */
    val = newnode(LIST);
    rplaca(val,arg1);
    rplacd(val,arg2);

    /* return the list */
    return (val);
}

/* xlist - built a list of the arguments */
NODE *xlist(args)
  NODE *args;
{
    NODE *oldstk,arg,list,val,*last,*lptr;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&list,&val,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* evaluate and append each argument */
    for (last = NIL; arg.n_ptr != NIL; last = lptr) {

	/* evaluate the next argument */
	val.n_ptr = xlarg(&arg.n_ptr);

	/* append this argument to the end of the list */
	lptr = newnode(LIST);
	if (last == NIL)
	    list.n_ptr = lptr;
	else
	    rplacd(last,lptr);
	rplaca(lptr,val.n_ptr);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the list */
    return (list.n_ptr);
}

/* xappend - built-in function append */
NODE *xappend(args)
  NODE *args;
{
    NODE *oldstk,arg,list,last,val,*lptr;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&list,&last,&val,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* evaluate and append each argument */
    while (arg.n_ptr) {

	/* evaluate the next argument */
	list.n_ptr = xlmatch(LIST,&arg.n_ptr);

	/* append each element of this list to the result list */
	while (consp(list.n_ptr)) {

	    /* append this element */
	    lptr = newnode(LIST);
	    if (last.n_ptr == NIL)
		val.n_ptr = lptr;
	    else
		rplacd(last.n_ptr,lptr);
	    rplaca(lptr,car(list.n_ptr));

	    /* save the new last element */
	    last.n_ptr = lptr;

	    /* move to the next element */
	    list.n_ptr = cdr(list.n_ptr);
	}
    }

    /* restore previous stack frame */
    xlstack = oldstk;

    /* return the list */
    return (val.n_ptr);
}

/* xreverse - built-in function reverse */
NODE *xreverse(args)
  NODE *args;
{
    NODE *oldstk,list,val,*lptr;

    /* create a new stack frame */
    oldstk = xlsave(&list,&val,NULL);

    /* get the list to reverse */
    list.n_ptr = xlmatch(LIST,&args);
    xllastarg(args);

    /* append each element of this list to the result list */
    while (consp(list.n_ptr)) {

	/* append this element */
	lptr = newnode(LIST);
	rplaca(lptr,car(list.n_ptr));
	rplacd(lptr,val.n_ptr);
	val.n_ptr = lptr;

	/* move to the next element */
	list.n_ptr = cdr(list.n_ptr);
    }

    /* restore previous stack frame */
    xlstack = oldstk;

    /* return the list */
    return (val.n_ptr);
}

/* xlast - return the last cons of a list */
NODE *xlast(args)
  NODE *args;
{
    NODE *list;

    /* get the list */
    list = xlmatch(LIST,&args);
    xllastarg(args);

    /* find the last cons */
    while (consp(list) && cdr(list))
	list = cdr(list);

    /* return the last element */
    return (list);
}

/* xmember - built-in function 'member' */
NODE *xmember(args)
  NODE *args;
{
    NODE *oldstk,x,list,fcn,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave(&x,&list,&fcn,NULL);

    /* get the expression to look for and the list */
    x.n_ptr = xlarg(&args);
    list.n_ptr = xlmatch(LIST,&args);
    xltest(&fcn.n_ptr,&tresult,&args);
    xllastarg(args);

    /* look for the expression */
    for (val = NIL; consp(list.n_ptr); list.n_ptr = cdr(list.n_ptr))
	if (dotest(x.n_ptr,car(list.n_ptr),fcn.n_ptr) == tresult) {
	    val = list.n_ptr;
	    break;
	}

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xassoc - built-in function 'assoc' */
NODE *xassoc(args)
  NODE *args;
{
    NODE *oldstk,x,alist,fcn,*pair,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave(&x,&alist,&fcn,NULL);

    /* get the expression to look for and the association list */
    x.n_ptr = xlarg(&args);
    alist.n_ptr = xlmatch(LIST,&args);
    xltest(&fcn.n_ptr,&tresult,&args);
    xllastarg(args);

    /* look for the expression */
    for (val = NIL; consp(alist.n_ptr); alist.n_ptr = cdr(alist.n_ptr))
	if ((pair = car(alist.n_ptr)) && consp(pair))
	    if (dotest(x.n_ptr,car(pair),fcn.n_ptr) == tresult) {
		val = pair;
		break;
	    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xsubst - substitute one expression for another */
NODE *xsubst(args)
  NODE *args;
{
    NODE *oldstk,to,from,expr,fcn,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave(&to,&from,&expr,&fcn,NULL);

    /* get the to value, the from value and the expression */
    to.n_ptr = xlarg(&args);
    from.n_ptr = xlarg(&args);
    expr.n_ptr = xlarg(&args);
    xltest(&fcn.n_ptr,&tresult,&args);
    xllastarg(args);

    /* do the substitution */
    val = subst(to.n_ptr,from.n_ptr,expr.n_ptr,fcn.n_ptr,tresult);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* subst - substitute one expression for another */
LOCAL NODE *subst(to,from,expr,fcn,tresult)
  NODE *to,*from,*expr,*fcn; int tresult;
{
    NODE *oldstk,carval,cdrval,*val;

    if (dotest(expr,from,fcn) == tresult)
	val = to;
    else if (consp(expr)) {
	oldstk = xlsave(&carval,&cdrval,NULL);
	carval.n_ptr = subst(to,from,car(expr),fcn,tresult);
	cdrval.n_ptr = subst(to,from,cdr(expr),fcn,tresult);
	val = newnode(LIST);
	rplaca(val,carval.n_ptr);
	rplacd(val,cdrval.n_ptr);
	xlstack = oldstk;
    }
    else
	val = expr;
    return (val);
}

/* xsublis - substitute using an association list */
NODE *xsublis(args)
  NODE *args;
{
    NODE *oldstk,alist,expr,fcn,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave(&alist,&expr,&fcn,NULL);

    /* get the assocation list and the expression */
    alist.n_ptr = xlmatch(LIST,&args);
    expr.n_ptr = xlarg(&args);
    xltest(&fcn.n_ptr,&tresult,&args);
    xllastarg(args);

    /* do the substitution */
    val = sublis(alist.n_ptr,expr.n_ptr,fcn.n_ptr,tresult);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* sublis - substitute using an association list */
LOCAL NODE *sublis(alist,expr,fcn,tresult)
  NODE *alist,*expr,*fcn; int tresult;
{
    NODE *oldstk,carval,cdrval,*val;

    if (val = assoc(expr,alist,fcn,tresult))
	val = cdr(val);
    else if (consp(expr)) {
	oldstk = xlsave(&carval,&cdrval,NULL);
	carval.n_ptr = sublis(alist,car(expr),fcn,tresult);
	cdrval.n_ptr = sublis(alist,cdr(expr),fcn,tresult);
	val = newnode(LIST);
	rplaca(val,carval.n_ptr);
	rplacd(val,cdrval.n_ptr);
	xlstack = oldstk;
    }
    else
	val = expr;
    return (val);
}

/* assoc - find a pair in an association list */
LOCAL NODE *assoc(expr,alist,fcn,tresult)
  NODE *expr,*alist,*fcn; int tresult;
{
    NODE *pair;

    for (; consp(alist); alist = cdr(alist))
	if ((pair = car(alist)) && consp(pair))
	    if (dotest(expr,car(pair),fcn) == tresult)
		return (pair);
    return (NIL);
}

/* xremove - built-in function 'remove' */
NODE *xremove(args)
  NODE *args;
{
    NODE *oldstk,x,list,fcn,val,*p,*last;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave(&x,&list,&fcn,&val,NULL);

    /* get the expression to remove and the list */
    x.n_ptr = xlarg(&args);
    list.n_ptr = xlmatch(LIST,&args);
    xltest(&fcn.n_ptr,&tresult,&args);
    xllastarg(args);

    /* remove matches */
    while (consp(list.n_ptr)) {

	/* check to see if this element should be deleted */
	if (dotest(x.n_ptr,car(list.n_ptr),fcn.n_ptr) != tresult) {
	    p = newnode(LIST);
	    rplaca(p,car(list.n_ptr));
	    if (val.n_ptr) rplacd(last,p);
	    else val.n_ptr = p;
	    last = p;
	}

	/* move to the next element */
	list.n_ptr = cdr(list.n_ptr);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the updated list */
    return (val.n_ptr);
}

/* dotest - call a test function */
int dotest(arg1,arg2,fcn)
  NODE *arg1,*arg2,*fcn;
{
    NODE *oldstk,args,*val;

    /* create a new stack frame */
    oldstk = xlsave(&args,NULL);

    /* build an argument list */
    args.n_ptr = newnode(LIST);
    rplaca(args.n_ptr,arg1);
    rplacd(args.n_ptr,newnode(LIST));
    rplaca(cdr(args.n_ptr),arg2);

    /* apply the test function */
    val = xlapply(fcn,args.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result of the test */
    return (val != NIL);
}

/* xnth - return the nth element of a list */
NODE *xnth(args)
  NODE *args;
{
    return (nth(args,FALSE));
}

/* xnthcdr - return the nth cdr of a list */
NODE *xnthcdr(args)
  NODE *args;
{
    return (nth(args,TRUE));
}

/* nth - internal nth function */
LOCAL NODE *nth(args,cdrflag)
  NODE *args; int cdrflag;
{
    NODE *list;
    int n;

    /* get n and the list */
    if ((n = xlmatch(INT,&args)->n_int) < 0)
	xlfail("bad argument");
    if ((list = xlmatch(LIST,&args)) == NIL)
	xlfail("bad argument");
    xllastarg(args);

    /* find the nth element */
    for (; n > 0 && consp(list); n--)
	list = cdr(list);

    /* return the list beginning at the nth element */
    return (cdrflag || !consp(list) ? list : car(list));
}

/* xlength - return the length of a list */
NODE *xlength(args)
  NODE *args;
{
    NODE *list,*val;
    int n;

    /* get the list */
    list = xlmatch(LIST,&args);
    xllastarg(args);

    /* find the length */
    for (n = 0; consp(list); n++)
	list = cdr(list);

    /* create the value node */
    val = newnode(INT);
    val->n_int = n;

    /* return the length */
    return (val);
}

/* xmapc - built-in function 'mapc' */
NODE *xmapc(args)
  NODE *args;
{
    return (map(args,TRUE,FALSE));
}

/* xmapcar - built-in function 'mapcar' */
NODE *xmapcar(args)
  NODE *args;
{
    return (map(args,TRUE,TRUE));
}

/* xmapl - built-in function 'mapl' */
NODE *xmapl(args)
  NODE *args;
{
    return (map(args,FALSE,FALSE));
}

/* xmaplist - built-in function 'maplist' */
NODE *xmaplist(args)
  NODE *args;
{
    return (map(args,FALSE,TRUE));
}

/* map - internal mapping function */
LOCAL NODE *map(args,carflag,valflag)
  NODE *args; int carflag,valflag;
{
    NODE *oldstk,fcn,lists,arglist,val,*last,*p,*x,*y;

    /* create a new stack frame */
    oldstk = xlsave(&fcn,&lists,&arglist,&val,NULL);

    /* get the function to apply and the first list */
    fcn.n_ptr = xlarg(&args);
    lists.n_ptr = xlmatch(LIST,&args);

    /* save the first list if not saving function values */
    if (!valflag)
	val.n_ptr = lists.n_ptr;

    /* set up the list of argument lists */
    p = newnode(LIST);
    rplaca(p,lists.n_ptr);
    lists.n_ptr = p;

    /* get the remaining argument lists */
    while (args) {
	p = newnode(LIST);
	rplacd(p,lists.n_ptr);
	lists.n_ptr = p;
	rplaca(p,xlmatch(LIST,&args));
    }

    /* if the function is a symbol, get its value */
    if (symbolp(fcn.n_ptr))
	fcn.n_ptr = xleval(fcn.n_ptr);

    /* loop through each of the argument lists */
    for (;;) {

	/* build an argument list from the sublists */
	arglist.n_ptr = NIL;
	for (x = lists.n_ptr; x && (y = car(x)) && consp(y); x = cdr(x)) {
	    p = newnode(LIST);
	    rplacd(p,arglist.n_ptr);
	    arglist.n_ptr = p;
	    rplaca(p,carflag ? car(y) : y);
	    rplaca(x,cdr(y));
	}

	/* quit if any of the lists were empty */
	if (x) break;

	/* apply the function to the arguments */
	if (valflag) {
	    p = newnode(LIST);
	    if (val.n_ptr) rplacd(last,p);
	    else val.n_ptr = p;
	    rplaca(p,xlapply(fcn.n_ptr,arglist.n_ptr));
	    last = p;
	}
	else
	    xlapply(fcn.n_ptr,arglist.n_ptr);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the last test expression value */
    return (val.n_ptr);
}

/* xrplca - replace the car of a list node */
NODE *xrplca(args)
  NODE *args;
{
    NODE *list,*newcar;

    /* get the list and the new car */
    if ((list = xlmatch(LIST,&args)) == NIL)
	xlfail("bad argument");
    newcar = xlarg(&args);
    xllastarg(args);

    /* replace the car */
    rplaca(list,newcar);

    /* return the list node that was modified */
    return (list);
}

/* xrplcd - replace the cdr of a list node */
NODE *xrplcd(args)
  NODE *args;
{
    NODE *list,*newcdr;

    /* get the list and the new cdr */
    if ((list = xlmatch(LIST,&args)) == NIL)
	xlfail("bad argument");
    newcdr = xlarg(&args);
    xllastarg(args);

    /* replace the cdr */
    rplacd(list,newcdr);

    /* return the list node that was modified */
    return (list);
}

/* xnconc - destructively append lists */
NODE *xnconc(args)
  NODE *args;
{
    NODE *list,*last,*val;

    /* concatenate each argument */
    for (val = NIL; args; ) {

	/* concatenate this list */
	if (list = xlmatch(LIST,&args)) {

	    /* check for this being the first non-empty list */
	    if (val)
		rplacd(last,list);
	    else
		val = list;

	    /* find the end of the list */
	    while (consp(cdr(list)))
		list = cdr(list);

	    /* save the new last element */
	    last = list;
	}
    }

    /* return the list */
    return (val);
}

/* xdelete - built-in function 'delete' */
NODE *xdelete(args)
  NODE *args;
{
    NODE *oldstk,x,list,fcn,*last,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave(&x,&list,&fcn,NULL);

    /* get the expression to delete and the list */
    x.n_ptr = xlarg(&args);
    list.n_ptr = xlmatch(LIST,&args);
    xltest(&fcn.n_ptr,&tresult,&args);
    xllastarg(args);

    /* delete leading matches */
    while (consp(list.n_ptr)) {
	if (dotest(x.n_ptr,car(list.n_ptr),fcn.n_ptr) != tresult)
	    break;
	list.n_ptr = cdr(list.n_ptr);
    }
    val = last = list.n_ptr;

    /* delete embedded matches */
    if (consp(list.n_ptr)) {

	/* skip the first non-matching element */
	list.n_ptr = cdr(list.n_ptr);

	/* look for embedded matches */
	while (consp(list.n_ptr)) {

	    /* check to see if this element should be deleted */
	    if (dotest(x.n_ptr,car(list.n_ptr),fcn.n_ptr) == tresult)
		rplacd(last,cdr(list.n_ptr));
	    else
		last = list.n_ptr;

	    /* move to the next element */
	    list.n_ptr = cdr(list.n_ptr);
 	}
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the updated list */
    return (val);
}

/* xatom - is this an atom? */
NODE *xatom(args)
  NODE *args;
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (atom(arg) ? true : NIL);
}

/* xsymbolp - is this an symbol? */
NODE *xsymbolp(args)
  NODE *args;
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (arg == NIL || symbolp(arg) ? true : NIL);
}

/* xnumberp - is this an number? */
NODE *xnumberp(args)
  NODE *args;
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (fixp(arg) ? true : NIL);
}

/* xboundp - is this a value bound to this symbol? */
NODE *xboundp(args)
  NODE *args;
{
    NODE *sym;
    sym = xlmatch(SYM,&args);
    xllastarg(args);
    return (sym->n_symvalue == s_unbound ? NIL : true);
}

/* xnull - is this null? */
NODE *xnull(args)
  NODE *args;
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (null(arg) ? true : NIL);
}

/* xlistp - is this a list? */
NODE *xlistp(args)
  NODE *args;
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (listp(arg) ? true : NIL);
}

/* xconsp - is this a cons? */
NODE *xconsp(args)
  NODE *args;
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (consp(arg) ? true : NIL);
}

/* xeq - are these equal? */
NODE *xeq(args)
  NODE *args;
{
    return (cequal(args,eq));
}

/* xeql - are these equal? */
NODE *xeql(args)
  NODE *args;
{
    return (cequal(args,eql));
}

/* xequal - are these equal? */
NODE *xequal(args)
  NODE *args;
{
    return (cequal(args,equal));
}

/* cequal - common eq/eql/equal function */
LOCAL NODE *cequal(args,fcn)
  NODE *args; int (*fcn)();
{
    NODE *arg1,*arg2;

    /* get the two arguments */
    arg1 = xlarg(&args);
    arg2 = xlarg(&args);
    xllastarg(args);

    /* compare the arguments */
    return ((*fcn)(arg1,arg2) ? true : NIL);
}
