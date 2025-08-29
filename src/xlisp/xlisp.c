/* xlisp - an experimental version of lisp that supports object-oriented
           programming */

#include "xlisp.h"

/* define the banner line string */
#define BANNER	"XLISP version 1.4 - 14-FEB-1985, by David Betz"

/* external variables */
extern NODE *s_stdin,*s_stdout;
extern NODE *s_evalhook,*s_applyhook;
extern NODE *true;

/* main - the main routine */
main()
/*
main(argc,argv)
  int argc; char *argv[];
*/
{
    NODE expr;
    CONTEXT cntxt;
    int i;

    /* print the banner line */
#ifdef MEGAMAX
    _autowin(BANNER);
#else
    printf("%s\n",BANNER);
#endif

    /* setup initialization error handler */
    xlbegin(&cntxt,CF_ERROR,(NODE *) 1);
    if (setjmp(cntxt.c_jmpbuf)) {
	printf("fatal initialization error\n");
	exit();
    }

    /* initialize xlisp */
    xlinit();
    xlend(&cntxt);

    /* reset the error handler */
    xlbegin(&cntxt,CF_ERROR,true);

    /* load "init.lsp" */
    if (setjmp(cntxt.c_jmpbuf) == 0)
	xlload("init",FALSE,FALSE);

    /* load any files mentioned on the command line */
/**
    if (setjmp(cntxt.c_jmpbuf) == 0)
	for (i = 1; i < argc; i++)
	    if (!xlload(argv[i],TRUE,FALSE)) xlfail("can't load file");
**/

    /* create a new stack frame */
    xlsave(&expr,NULL);

    /* main command processing loop */
    while (TRUE) {

	/* setup the error return */
	if (setjmp(cntxt.c_jmpbuf)) {
	    s_evalhook->n_symvalue = NIL;
	    s_applyhook->n_symvalue = NIL;
	    xlflush();
	}

	/* read an expression */
	if (!xlread(s_stdin->n_symvalue,&expr.n_ptr))
	    break;

	/* evaluate the expression */
	expr.n_ptr = xleval(expr.n_ptr);

	/* print it */
	stdprint(expr.n_ptr);
    }
    xlend(&cntxt);
}

/* stdprint - print to standard output */
stdprint(expr)
  NODE *expr;
{
    xlprint(s_stdout->n_symvalue,expr,TRUE);
    xlterpri(s_stdout->n_symvalue);
}
