/* xlinit.c - xlisp initialization module */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/

#include "xlisp.h"

/* external variables */
extern NODE *true,*s_dot;
extern NODE *s_quote,*s_function,*s_bquote,*s_comma,*s_comat;
extern NODE *s_lambda,*s_macro;
extern NODE *s_stdin,*s_stdout;
extern NODE *s_evalhook,*s_applyhook;
extern NODE *s_tracenable,*s_tlimit,*s_breakenable;
extern NODE *s_car,*s_cdr,*s_nth,*s_get,*s_svalue,*s_splist,*s_aref,*s_eql;
extern NODE *s_rtable,*k_wspace,*k_const,*k_nmacro,*k_tmacro;
extern NODE *k_test,*k_tnot,*k_optional,*k_rest,*k_aux;
extern NODE *a_subr,*a_fsubr;
extern NODE *a_list,*a_sym,*a_int,*a_float,*a_str,*a_obj,*a_fptr,*a_vect;
extern struct fdef ftab[];

/* xlinit - xlisp initialization routine */
xlinit()
{
    struct fdef *fptr;
    NODE *sym;

    /* initialize xlisp (must be in this order) */
    xlminit();	/* initialize xldmem.c */
    xlsinit();	/* initialize xlsym.c */
    xldinit();	/* initialize xldbug.c */
    xloinit();	/* initialize xlobj.c */

    /* enter the builtin functions */
    for (fptr = ftab; fptr->f_name; fptr++)
	xlsubr(fptr->f_name,fptr->f_type,fptr->f_fcn);

    /* enter operating system specific functions */
    osfinit();

    /* enter the 't' symbol */
    true = xlsenter("T");
    setvalue(true,true);

    /* enter some important symbols */
    s_dot	= xlsenter(".");
    s_quote	= xlsenter("QUOTE");
    s_function	= xlsenter("FUNCTION");
    s_bquote	= xlsenter("BACKQUOTE");
    s_comma	= xlsenter("COMMA");
    s_comat	= xlsenter("COMMA-AT");
    s_lambda	= xlsenter("LAMBDA");
    s_macro	= xlsenter("MACRO");
    s_eql	= xlsenter("EQL");

    /* enter setf place specifiers */
    s_car	= xlsenter("CAR");
    s_cdr	= xlsenter("CDR");
    s_nth	= xlsenter("NTH");
    s_get	= xlsenter("GET");
    s_svalue	= xlsenter("SYMBOL-VALUE");
    s_splist	= xlsenter("SYMBOL-PLIST");
    s_aref	= xlsenter("AREF");

    /* enter the readtable variable and keywords */
    s_rtable	= xlsenter("*READTABLE*");
    k_wspace	= xlsenter(":WHITE-SPACE");
    k_const	= xlsenter(":CONSTITUENT");
    k_nmacro	= xlsenter(":NMACRO");
    k_tmacro	= xlsenter(":TMACRO");
    xlrinit();
 
    /* enter parameter list keywords */
    k_test	= xlsenter(":TEST");
    k_tnot	= xlsenter(":TEST-NOT");

    /* enter lambda list keywords */
    k_optional	= xlsenter("&OPTIONAL");
    k_rest	= xlsenter("&REST");
    k_aux	= xlsenter("&AUX");

    /* enter *standard-input* and *standard-output* */
    s_stdin = xlsenter("*STANDARD-INPUT*");
    setvalue(s_stdin,cvfile(stdin));
    s_stdout = xlsenter("*STANDARD-OUTPUT*");
    setvalue(s_stdout,cvfile(stdout));

    /* enter the eval and apply hook variables */
    s_evalhook = xlsenter("*EVALHOOK*");
    setvalue(s_evalhook,NIL);
    s_applyhook = xlsenter("*APPLYHOOK*");
    setvalue(s_applyhook,NIL);

    /* enter the error traceback and the error break enable flags */
    s_tracenable = xlsenter("*TRACENABLE*");
    setvalue(s_tracenable,NIL);
    s_tlimit = xlsenter("*TRACELIMIT*");
    setvalue(s_tlimit,NIL);
    s_breakenable = xlsenter("*BREAKENABLE*");
    setvalue(s_breakenable,true);

    /* enter a copyright notice into the oblist */
    sym = xlsenter("**Copyright-1985-by-David-Betz**");
    setvalue(sym,true);

    /* enter type names */
    a_subr	= xlsenter(":SUBR");
    a_fsubr	= xlsenter(":FSUBR");
    a_list	= xlsenter(":CONS");
    a_sym	= xlsenter(":SYMBOL");
    a_int	= xlsenter(":FIXNUM");
    a_float	= xlsenter(":FLONUM");
    a_str	= xlsenter(":STRING");
    a_obj	= xlsenter(":OBJECT");
    a_fptr	= xlsenter(":FILE");
    a_vect	= xlsenter(":ARRAY");
}

