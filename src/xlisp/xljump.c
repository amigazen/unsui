/* xljump - execution context routines */

#include "xlisp.h"

/* external variables */
extern CONTEXT *xlcontext;
extern NODE *xlvalue;
extern NODE *xlstack,*xlenv,*xlnewenv;
extern int xltrace,xldebug;

/* xlbegin - beginning of an execution context */
xlbegin(cptr,flags,expr)
  CONTEXT *cptr; int flags; NODE *expr;
{
    cptr->c_flags = flags;
    cptr->c_expr = expr;
    cptr->c_xlstack = xlstack;
    cptr->c_xlenv = xlenv;
    cptr->c_xlnewenv = xlnewenv;
    cptr->c_xltrace = xltrace;
    cptr->c_xlcontext = xlcontext;
    xlcontext = cptr;
}

/* xlend - end of an execution context */
xlend(cptr)
  CONTEXT *cptr;
{
    xlcontext = cptr->c_xlcontext;
}

/* xljump - jump to a saved execution context */
xljump(cptr,type,val)
  CONTEXT *cptr; int type; NODE *val;
{
    /* restore the state */
    xlvalue = val;
    xlstack = cptr->c_xlstack;
    xlunbind(cptr->c_xlenv);
    xlnewenv = cptr->c_xlnewenv;
    xltrace = cptr->c_xltrace;

    /* call the handler */
    longjmp(cptr->c_jmpbuf,type);
}

/* xlgo - go to a label */
xlgo(label)
  NODE *label;
{
    CONTEXT *cptr;
    NODE *p;

    /* find a tagbody context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_GO)
	    for (p = cptr->c_expr; consp(p); p = cdr(p))
		if (car(p) == label)
		    xljump(cptr,CF_GO,p);
    xlfail("no target for go");
}

/* xlreturn - return from a block */
xlreturn(val)
  NODE *val;
{
    CONTEXT *cptr;

    /* find a block context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_RETURN)
	    xljump(cptr,CF_RETURN,val);
    xlfail("no target for return");
}

/* xlthrow - throw to a catch */
xlthrow(tag,val)
  NODE *tag,*val;
{
    CONTEXT *cptr;

    /* find a catch context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if ((cptr->c_flags & CF_THROW) && cptr->c_expr == tag)
	    xljump(cptr,CF_THROW,val);
    xlfail("no target for throw");
}

/* xlsignal - signal an error */
xlsignal(emsg,arg)
  char *emsg; NODE *arg;
{
    CONTEXT *cptr;

    /* find an error catcher */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_ERROR) {
	    if (cptr->c_expr)
		xlerrprint("error",NULL,emsg,arg);
	    xljump(cptr,CF_ERROR,NIL);
	}
    xlfail("no target for error");
}
