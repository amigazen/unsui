/* xlsetf - set field function */

#include "xlisp.h"

/* external variables */
extern NODE *s_car,*s_cdr,*s_get,*s_svalue,*s_splist;
extern NODE *xlstack;

/* xsetf - built-in function 'setf' */
NODE *xsetf(args)
  NODE *args;
{
    NODE *oldstk,arg,place,value;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&place,&value,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* handle each pair of arguments */
    while (arg.n_ptr) {

	/* get place and value */
	place.n_ptr = xlarg(&arg.n_ptr);
	value.n_ptr = xlevarg(&arg.n_ptr);

	/* check the place form */
	if (symbolp(place.n_ptr))
	    assign(place.n_ptr,value.n_ptr);
	else if (consp(place.n_ptr))
	    placeform(place.n_ptr,value.n_ptr);
	else
	    xlfail("bad place form");
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (value.n_ptr);
}

/* placeform - handle a place form other than a symbol */
LOCAL placeform(place,value)
  NODE *place,*value;
{
    NODE *fun,*oldstk,arg1,arg2;

    /* check the function name */
    if ((fun = xlmatch(SYM,&place)) == s_get) {
	oldstk = xlsave(&arg1,&arg2,NULL);
	arg1.n_ptr = xlevmatch(SYM,&place);
	arg2.n_ptr = xlevmatch(SYM,&place);
	xllastarg(place);
	xlputprop(arg1.n_ptr,value,arg2.n_ptr);
	xlstack = oldstk;
    }
    else if (fun == s_svalue || fun == s_splist) {
	oldstk = xlsave(&arg1,NULL);
	arg1.n_ptr = xlevmatch(SYM,&place);
	xllastarg(place);
	if (fun == s_svalue)
	    arg1.n_ptr->n_symvalue = value;
	else
	    rplacd(arg1.n_ptr->n_symplist,value);
	xlstack = oldstk;
    }
    else if (fun == s_car || fun == s_cdr) {
	oldstk = xlsave(&arg1,NULL);
	arg1.n_ptr = xlevmatch(LIST,&place);
	xllastarg(place);
	if (consp(arg1.n_ptr))
	    if (fun == s_car)
		rplaca(arg1.n_ptr,value);
	    else
		rplacd(arg1.n_ptr,value);
	xlstack = oldstk;
    }
    else
	xlfail("bad place form");
}
