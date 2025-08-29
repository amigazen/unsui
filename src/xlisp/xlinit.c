/* xlinit.c - xlisp initialization module */

#include "xlisp.h"

/* external variables */
extern NODE *true;
extern NODE *s_quote,*s_function,*s_bquote,*s_comma,*s_comat;
extern NODE *s_lambda,*s_macro;
extern NODE *s_stdin,*s_stdout;
extern NODE *s_evalhook,*s_applyhook;
extern NODE *s_tracenable,*s_tlimit,*s_breakenable;
extern NODE *s_continue,*s_quit;
extern NODE *s_car,*s_cdr,*s_get,*s_svalue,*s_splist,*s_eql;
extern NODE *k_test,*k_tnot,*k_optional,*k_rest,*k_aux;
extern NODE *a_subr,*a_fsubr;
extern NODE *a_list,*a_sym,*a_int,*a_str,*a_obj,*a_fptr;
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

    /* enter the 't' symbol */
    true = xlsenter("t");
    true->n_symvalue = true;

    /* enter some important symbols */
    s_quote	= xlsenter("quote");
    s_function	= xlsenter("function");
    s_bquote	= xlsenter("backquote");
    s_comma	= xlsenter("comma");
    s_comat	= xlsenter("comma-at");
    s_lambda	= xlsenter("lambda");
    s_macro	= xlsenter("macro");
    s_eql	= xlsenter("eql");
    s_continue	= xlsenter("continue");
    s_quit	= xlsenter("quit");

    /* enter setf place specifiers */
    s_car	= xlsenter("car");
    s_cdr	= xlsenter("cdr");
    s_get	= xlsenter("get");
    s_svalue	= xlsenter("symbol-value");
    s_splist	= xlsenter("symbol-plist");

    /* enter parameter list keywords */
    k_test	= xlsenter(":test");
    k_tnot	= xlsenter(":test-not");

    /* enter lambda list keywords */
    k_optional	= xlsenter("&optional");
    k_rest	= xlsenter("&rest");
    k_aux	= xlsenter("&aux");

    /* enter *standard-input* and *standard-output* */
    s_stdin = xlsenter("*standard-input*");
    s_stdin->n_symvalue = newnode(FPTR);
    s_stdin->n_symvalue->n_fp = stdin;
    s_stdin->n_symvalue->n_savech = 0;
    s_stdout = xlsenter("*standard-output*");
    s_stdout->n_symvalue = newnode(FPTR);
    s_stdout->n_symvalue->n_fp = stdout;
    s_stdout->n_symvalue->n_savech = 0;

    /* enter the eval and apply hook variables */
    s_evalhook = xlsenter("*evalhook*");
    s_evalhook->n_symvalue = NIL;
    s_applyhook = xlsenter("*applyhook*");
    s_applyhook->n_symvalue = NIL;

    /* enter the error traceback and the error break enable flags */
    s_tracenable = xlsenter("*tracenable*");
    s_tracenable->n_symvalue = NIL;
    s_tlimit = xlsenter("*tracelimit*");
    s_tlimit->n_symvalue = NIL;
    s_breakenable = xlsenter("*breakenable*");
    s_breakenable->n_symvalue = true;

    /* enter a copyright notice into the oblist */
    sym = xlsenter("**Copyright-1985-by-David-Betz**");
    sym->n_symvalue = true;

    /* enter type names */
    a_subr	= xlsenter("SUBR");
    a_fsubr	= xlsenter("FSUBR");
    a_list	= xlsenter("LIST");
    a_sym	= xlsenter("SYM");
    a_int	= xlsenter("INT");
    a_str	= xlsenter("STR");
    a_obj	= xlsenter("OBJ");
    a_fptr	= xlsenter("FPTR");
}
