/* xlsys.c - xlisp builtin system functions */

#include "xlisp.h"

/* external variables */
extern NODE *xlstack;
extern int anodes;

/* external symbols */
extern NODE *a_subr,*a_fsubr;
extern NODE *a_list,*a_sym,*a_int,*a_str,*a_obj,*a_fptr;
extern NODE *true;

/* xload - direct input from a file */
NODE *xload(args)
  NODE *args;
{
    NODE *oldstk,fname,*val;
    int vflag,pflag;

    /* create a new stack frame */
    oldstk = xlsave(&fname,NULL);

    /* get the file name, verbose flag and print flag */
    fname.n_ptr = xlmatch(STR,&args);
    vflag = (args ? xlarg(&args) != NIL : TRUE);
    pflag = (args ? xlarg(&args) != NIL : FALSE);
    xllastarg(args);

    /* load the file */
    val = (xlload(fname.n_ptr->n_str,vflag,pflag) ? true : NIL);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the status */
    return (val);
}

/* xgc - xlisp function to force garbage collection */
NODE *xgc(args)
  NODE *args;
{
    /* make sure there aren't any arguments */
    xllastarg(args);

    /* garbage collect */
    gc();

    /* return nil */
    return (NIL);
}

/* xexpand - xlisp function to force memory expansion */
NODE *xexpand(args)
  NODE *args;
{
    NODE *val;
    int n,i;

    /* get the new number to allocate */
    n = (args ? xlmatch(INT,&args)->n_int : 1);
    xllastarg(args);

    /* allocate more segments */
    for (i = 0; i < n; i++)
	if (!addseg())
	    break;

    /* return the number of segments added */
    val = newnode(INT);
    val->n_int = i;
    return (val);
}

/* xalloc - xlisp function to set the number of nodes to allocate */
NODE *xalloc(args)
  NODE *args;
{
    NODE *val;
    int n,oldn;

    /* get the new number to allocate */
    n = xlmatch(INT,&args)->n_int;

    /* make sure there aren't any more arguments */
    xllastarg(args);

    /* set the new number of nodes to allocate */
    oldn = anodes;
    anodes = n;

    /* return the old number */
    val = newnode(INT);
    val->n_int = oldn;
    return (val);
}

/* xmem - xlisp function to print memory statistics */
NODE *xmem(args)
  NODE *args;
{
    /* make sure there aren't any arguments */
    xllastarg(args);

    /* print the statistics */
    stats();

    /* return nil */
    return (NIL);
}

/* xtype - return type of a thing */
NODE *xtype(args)
    NODE *args;
{
    NODE *arg;

    if (!(arg = xlarg(&args)))
	return (NIL);

    switch (ntype(arg)) {
	case SUBR:	return (a_subr);
	case FSUBR:	return (a_fsubr);
	case LIST:	return (a_list);
	case SYM:	return (a_sym);
	case INT:	return (a_int);
	case STR:	return (a_str);
	case OBJ:	return (a_obj);
	case FPTR:	return (a_fptr);
	default:	xlfail("bad node type");
    }
}

/* xbaktrace - print the trace back stack */
NODE *xbaktrace(args)
  NODE *args;
{
    int n;

    n = (args ? xlmatch(INT,&args)->n_int : -1);
    xllastarg(args);
    xlbaktrace(n);
    return (NIL);
}

/* xexit - get out of xlisp */
NODE *xexit(args)
  NODE *args;
{
    xllastarg(args);
    exit();
}
