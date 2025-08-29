/* xlmath - xlisp builtin arithmetic functions */

#include "xlisp.h"

/* external variables */
extern NODE *xlstack;
extern NODE *true;

/* forward declarations */
FORWARD NODE *unary();
FORWARD NODE *binary();
FORWARD NODE *predicate();
FORWARD NODE *compare();

/* xadd - builtin function for addition */
NODE *xadd(args)
  NODE *args;
{
    return (binary(args,'+'));
}

/* xsub - builtin function for subtraction */
NODE *xsub(args)
  NODE *args;
{
    return (binary(args,'-'));
}

/* xmul - builtin function for multiplication */
NODE *xmul(args)
  NODE *args;
{
    return (binary(args,'*'));
}

/* xdiv - builtin function for division */
NODE *xdiv(args)
  NODE *args;
{
    return (binary(args,'/'));
}

/* xrem - builtin function for remainder */
NODE *xrem(args)
  NODE *args;
{
    return (binary(args,'%'));
}

/* xmin - builtin function for minimum */
NODE *xmin(args)
  NODE *args;
{
    return (binary(args,'m'));
}

/* xmax - builtin function for maximum */
NODE *xmax(args)
  NODE *args;
{
    return (binary(args,'M'));
}

/* xbitand - builtin function for bitwise and */
NODE *xbitand(args)
  NODE *args;
{
    return (binary(args,'&'));
}

/* xbitior - builtin function for bitwise inclusive or */
NODE *xbitior(args)
  NODE *args;
{
    return (binary(args,'|'));
}

/* xbitxor - builtin function for bitwise exclusive or */
NODE *xbitxor(args)
  NODE *args;
{
    return (binary(args,'^'));
}

/* binary - handle binary operations */
LOCAL NODE *binary(args,fcn)
  NODE *args; int fcn;
{
    int ival,iarg;
    NODE *val;

    /* get the first argument */
    ival = xlmatch(INT,&args)->n_int;

    /* treat '-' with a single argument as a special case */
    if (fcn == '-' && args == NIL)
	ival = -ival;

    /* handle each remaining argument */
    while (args) {

	/* get the next argument */
	iarg = xlmatch(INT,&args)->n_int;

	/* accumulate the result value */
	switch (fcn) {
	case '+':	ival += iarg; break;
	case '-':	ival -= iarg; break;
	case '*':	ival *= iarg; break;
	case '/':	ival /= iarg; break;
	case '%':	ival %= iarg; break;
	case 'M':	if (iarg > ival) ival = iarg; break;
	case 'm':	if (iarg < ival) ival = iarg; break;
	case '&':	ival &= iarg; break;
	case '|':	ival |= iarg; break;
	case '^':	ival ^= iarg; break;
	}
    }

    /* initialize value */
    val = newnode(INT);
    val->n_int = ival;

    /* return the result value */
    return (val);
}

/* xbitnot - bitwise not */
NODE *xbitnot(args)
  NODE *args;
{
    return (unary(args,'~'));
}

/* xabs - builtin function for absolute value */
NODE *xabs(args)
  NODE *args;
{
    return (unary(args,'A'));
}

/* xadd1 - builtin function for adding one */
NODE *xadd1(args)
  NODE *args;
{
    return (unary(args,'+'));
}

/* xsub1 - builtin function for subtracting one */
NODE *xsub1(args)
  NODE *args;
{
    return (unary(args,'-'));
}

/* unary - handle unary operations */
LOCAL NODE *unary(args,fcn)
  NODE *args; int fcn;
{
    NODE *val;
    int ival;

    /* get the argument */
    ival = xlmatch(INT,&args)->n_int;
    xllastarg(args);

    /* compute the result */
    switch (fcn) {
    case '~':	ival = ~ival; break;
    case 'A':	if (ival < 0) ival = -ival; break;
    case '+':	ival++; break;
    case '-':	ival--; break;
    }

    /* convert the value  */
    val = newnode(INT);
    val->n_int = ival;

    /* return the result value */
    return (val);
}

/* xminusp - is this number negative? */
NODE *xminusp(args)
  NODE *args;
{
    return (predicate(args,'-'));
}

/* xzerop - is this number zero? */
NODE *xzerop(args)
  NODE *args;
{
    return (predicate(args,'Z'));
}

/* xplusp - is this number positive? */
NODE *xplusp(args)
  NODE *args;
{
    return (predicate(args,'+'));
}

/* xevenp - is this number even? */
NODE *xevenp(args)
  NODE *args;
{
    return (predicate(args,'E'));
}

/* xoddp - is this number odd? */
NODE *xoddp(args)
  NODE *args;
{
    return (predicate(args,'O'));
}

/* predicate - handle a predicate function */
LOCAL NODE *predicate(args,fcn)
  NODE *args; int fcn;
{
    NODE *val;
    int ival;

    /* get the argument */
    ival = xlmatch(INT,&args)->n_int;
    xllastarg(args);

    /* compute the result */
    switch (fcn) {
    case '-':	ival = (ival < 0); break;
    case 'Z':	ival = (ival == 0); break;
    case '+':	ival = (ival > 0); break;
    case 'E':	ival = ((ival & 1) == 0); break;
    case 'O':	ival = ((ival & 1) != 0); break;
    }

    /* return the result value */
    return (ival ? true : NIL);
}

/* xlss - builtin function for < */
NODE *xlss(args)
  NODE *args;
{
    return (compare(args,'<'));
}

/* xleq - builtin function for <= */
NODE *xleq(args)
  NODE *args;
{
    return (compare(args,'L'));
}

/* equ - builtin function for = */
NODE *xequ(args)
  NODE *args;
{
    return (compare(args,'='));
}

/* xneq - builtin function for /= */
NODE *xneq(args)
  NODE *args;
{
    return (compare(args,'#'));
}

/* xgeq - builtin function for >= */
NODE *xgeq(args)
  NODE *args;
{
    return (compare(args,'G'));
}

/* xgtr - builtin function for > */
NODE *xgtr(args)
  NODE *args;
{
    return (compare(args,'>'));
}

/* compare - common compare function */
LOCAL NODE *compare(args,fcn)
  NODE *args; int fcn;
{
    NODE *arg1,*arg2;
    int cmp;

    /* get the two arguments */
    arg1 = xlarg(&args);
    arg2 = xlarg(&args);
    xllastarg(args);

    /* do the compare */
    if (stringp(arg1) && stringp(arg2))
	cmp = strcmp(arg1->n_str,arg2->n_str);
    else if (fixp(arg1) && fixp(arg2))
	cmp = arg1->n_int - arg2->n_int;
    else
	cmp = (int)(arg1 - arg2);

    /* compute result of the compare */
    switch (fcn) {
    case '<':	cmp = (cmp < 0); break;
    case 'L':	cmp = (cmp <= 0); break;
    case '=':	cmp = (cmp == 0); break;
    case '#':	cmp = (cmp != 0); break;
    case 'G':	cmp = (cmp >= 0); break;
    case '>':	cmp = (cmp > 0); break;
    }

    /* return the result */
    return (cmp ? true : NIL);
}
