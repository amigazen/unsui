/* xlbind - xlisp symbol binding routines */

#include "xlisp.h"

/* external variables */
extern NODE *xlenv,*xlnewenv;

/* xlsbind - bind a value to a symbol sequentially */
xlsbind(sym,val)
  NODE *sym,*val;
{
    NODE *ptr;

    /* create a new environment list entry */
    ptr = newnode(LIST);
    rplacd(ptr,xlenv);
    xlenv = ptr;

    /* create a new variable binding */
    rplaca(ptr,newnode(LIST));
    rplaca(car(ptr),sym);
    rplacd(car(ptr),sym->n_symvalue);
    sym->n_symvalue = val;
}

/* xlbind - bind a value to a symbol in parallel */
xlbind(sym,val)
  NODE *sym,*val;
{
    NODE *ptr;

    /* create a new environment list entry */
    ptr = newnode(LIST);
    rplacd(ptr,xlnewenv);
    xlnewenv = ptr;

    /* create a new variable binding */
    rplaca(ptr,newnode(LIST));
    rplaca(car(ptr),sym);
    rplacd(car(ptr),val);
}

/* xlfixbindings - make a new set of bindings visible */
xlfixbindings()
{
    NODE *eptr,*bnd,*sym,*oldvalue;

    /* fix the bound value of each symbol in the environment chain */
    for (eptr = xlnewenv; eptr != xlenv; eptr = cdr(eptr)) {
	bnd = car(eptr);
	sym = car(bnd);
	oldvalue = sym->n_symvalue;
	sym->n_symvalue = cdr(bnd);
	rplacd(bnd,oldvalue);
    }
    xlenv = xlnewenv;
}

/* xlunbind - unbind symbols bound in this environment */
xlunbind(env)
  NODE *env;
{
    NODE *bnd;

    /* unbind each symbol in the environment chain */
    for (; xlenv != env; xlenv = cdr(xlenv))
	if (bnd = car(xlenv))
	    car(bnd)->n_symvalue = cdr(bnd);
}
