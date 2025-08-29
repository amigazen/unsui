/* xlisp - a small implementation of lisp with object-oriented programming */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/

#include "xlisp.h"

/* define the banner line string */
#define BANNER	"XLISP version 1.6, Copyright (c) 1985, by David Betz"

/* external variables */
extern NODE *s_stdin,*s_stdout;
extern NODE *s_evalhook,*s_applyhook;
extern int xldebug;
extern NODE *true;

/* main - the main routine */
main(argc,argv)
  int argc; char *argv[];
{
    CONTEXT cntxt;
    NODE *expr;
    int i;

    /* initialize and print the banner line */
    osinit(BANNER);

    /* setup initialization error handler */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_ERROR,(NODE *) 1);
    if (setjmp(cntxt.c_jmpbuf)) {
	printf("fatal initialization error\n");
	osfinish();
	exit();
    }

    /* initialize xlisp */
    xlinit();
    xlend(&cntxt);

    /* reset the error handler */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_ERROR,true);

    /* load "init.lsp" */
    if (setjmp(cntxt.c_jmpbuf) == 0)
	xlload("init.lsp",FALSE,FALSE);

    /* load any files mentioned on the command line */
#ifndef MEGAMAX
    if (setjmp(cntxt.c_jmpbuf) == 0)
	for (i = 1; i < argc; i++)
	    if (!xlload(argv[i],TRUE,FALSE))
		xlfail("can't load file");
#endif

    /* create a new stack frame */
    xlsave(&expr,NULL);

    /* main command processing loop */
    while (TRUE) {

	/* setup the error return */
	if (i = setjmp(cntxt.c_jmpbuf)) {
	    if (i == CF_TOPLEVEL)
		stdputstr("[ back to the top level ]\n");
	    setvalue(s_evalhook,NIL);
	    setvalue(s_applyhook,NIL);
	    xldebug = 0;
	    xlflush();
	}

	/* read an expression */
	if (!xlread(getvalue(s_stdin),&expr,FALSE))
	    break;

	/* evaluate the expression */
	expr = xleval(expr);

	/* print it */
	stdprint(expr);
    }
    xlend(&cntxt);
    osfinish ();
    exit ();
}

/* stdprint - print to standard output */
stdprint(expr)
  NODE *expr;
{
    xlprint(getvalue(s_stdout),expr,TRUE);
    xlterpri(getvalue(s_stdout));
}

/* stdputstr - print a string to standard output */
stdputstr(str)
  char *str;
{
    xlputstr(getvalue(s_stdout),str);
}

