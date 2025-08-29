/* xldebug - xlisp debugging support */

#include "xlisp.h"

/* external variables */
extern long total;
extern int xldebug;
extern int xltrace;
extern NODE *s_unbound;
extern NODE *s_stdin,*s_stdout;
extern NODE *s_tracenable,*s_tlimit,*s_breakenable;
extern NODE *s_continue,*s_quit;
extern NODE *xlstack;
extern NODE *true;
extern NODE **trace_stack;

/* external routines */
extern char *malloc();

/* forward declarations */
FORWARD NODE *stacktop();

/* xlfail - xlisp error handler */
xlfail(emsg)
  char *emsg;
{
    xlerror(emsg,stacktop());
}

/* xlabort - xlisp serious error handler */
xlabort(emsg)
  char *emsg;
{
    xlsignal(emsg,s_unbound);
}

/* xlbreak - enter a break loop */
xlbreak(emsg,arg)
  char *emsg; NODE *arg;
{
    breakloop("break",NULL,emsg,arg,TRUE);
}

/* xlerror - handle a fatal error */
xlerror(emsg,arg)
  char *emsg; NODE *arg;
{
    doerror(NULL,emsg,arg,FALSE);
}

/* xlcerror - handle a recoverable error */
xlcerror(cmsg,emsg,arg)
  char *cmsg,*emsg; NODE *arg;
{
    doerror(cmsg,emsg,arg,TRUE);
}

/* xlerrprint - print an error message */
xlerrprint(hdr,cmsg,emsg,arg)
  char *hdr,*cmsg,*emsg; NODE *arg;
{
    printf("%s: %s",hdr,emsg);
    if (arg != s_unbound) { printf(" - "); stdprint(arg); }
    else printf("\n");
    if (cmsg) printf("if continued: %s\n",cmsg);
}

/* doerror - handle xlisp errors */
LOCAL doerror(cmsg,emsg,arg,cflag)
  char *cmsg,*emsg; NODE *arg; int cflag;
{
    /* make sure the break loop is enabled */
    if (s_breakenable->n_symvalue == NIL)
	xlsignal(emsg,arg);

    /* call the debug read-eval-print loop */
    breakloop("error",cmsg,emsg,arg,cflag);
}

/* breakloop - the debug read-eval-print loop */
LOCAL int breakloop(hdr,cmsg,emsg,arg,cflag)
  char *hdr,*cmsg,*emsg; NODE *arg; int cflag;
{
    NODE *oldstk,expr,*val;
    CONTEXT cntxt;

    /* increment the debug level */
    xldebug++;

    /* flush the input buffer */
    xlflush();

    /* print the error message */
    xlerrprint(hdr,cmsg,emsg,arg);

    /* do the back trace */
    if (s_tracenable->n_symvalue) {
	val = s_tlimit->n_symvalue;
	xlbaktrace(fixp(val) ? val->n_int : -1);
    }

    /* create a new stack frame */
    oldstk = xlsave(&expr,NULL);

    /* debug command processing loop */
    xlbegin(&cntxt,CF_ERROR,true);
    while (TRUE) {

	/* setup the continue trap */
	if (setjmp(cntxt.c_jmpbuf)) {
	    xlflush();
	    continue;
	}

	/* read an expression and check for eof */
	if (!xlread(s_stdin->n_symvalue,&expr.n_ptr)) {
	    expr.n_ptr = s_quit;
	    break;
	}

	/* check for commands */
	if (expr.n_ptr == s_continue) {
	    if (cflag) break;
	    else xlabort("this error can't be continued");
	}
	else if (expr.n_ptr == s_quit)
	    break;

	/* evaluate the expression */
	expr.n_ptr = xleval(expr.n_ptr);

	/* print it */
	xlprint(s_stdout->n_symvalue,expr.n_ptr,TRUE);
	xlterpri(s_stdout->n_symvalue);
    }
    xlend(&cntxt);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* decrement the debug level */
    xldebug--;

    /* continue the next higher break loop on quit */
    if (expr.n_ptr == s_quit)
	xlsignal("quit from break loop",s_unbound);
}

/* tpush - add an entry to the trace stack */
xltpush(nptr)
    NODE *nptr;
{
    if (++xltrace < TDEPTH)
	trace_stack[xltrace] = nptr;
}

/* tpop - pop an entry from the trace stack */
xltpop()
{
    xltrace--;
}

/* stacktop - return the top node on the stack */
LOCAL NODE *stacktop()
{
    return (xltrace >= 0 && xltrace < TDEPTH ? trace_stack[xltrace] : s_unbound);
}

/* baktrace - do a back trace */
xlbaktrace(n)
  int n;
{
    int i;

    for (i = xltrace; (n < 0 || n--) && i >= 0; i--)
	if (i < TDEPTH)
	    stdprint(trace_stack[i]);
}

/* xldinit - debug initialization routine */
xldinit()
{
    if ((trace_stack = (NODE **) malloc(TSTKSIZE)) == NULL)
	xlabort("insufficient memory");
    total += (long) TSTKSIZE;
    xltrace = -1;
    xldebug = 0;
}
