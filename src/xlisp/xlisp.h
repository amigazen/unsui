/* xlisp - a small subset of lisp */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/

/* system specific definitions */
#define AZTEC_SM

#include <stdio.h>
#include <ctype.h>
#ifndef MEGAMAX
#include <setjmp.h>
#endif

/* NNODES	number of nodes to allocate in each request (1000) */
/* TDEPTH	trace stack depth (500) */
/* EDEPTH	evaluation stack depth (1000) */
/* FORWARD	type of a forward declaration () */
/* LOCAL	type of a local function (static) */
/* AFMT		printf format for addresses ("%x") */
/* FIXNUM	data type for fixed point numbers (long) */
/* ITYPE	fixed point input conversion routine type (long atol()) */
/* ICNV		fixed point input conversion routine (atol) */
/* IFMT		printf format for fixed point numbers ("%ld") */
/* FLONUM	data type for floating point numbers (float) */
/* SYSTEM	enable the control-d command */

/* absolute value macros */
#ifndef abs
#define abs(n)	((n) < 0 ? -(n) : (n))
#endif
#ifndef fabs
#define fabs(n)	((n) < 0.0 ? -(n) : (n))
#endif

/* for the MegaMax compiler */
#ifdef MEGAMAX
#define LOCAL
#define AFMT		"%lx"
#endif

/* for the AZTEC C compiler - small model */
#ifdef AZTEC_SM
#define SYSTEM
#define NIL		0
#endif

/* for the AZTEC C compiler - large model */
#ifdef AZTEC_LM
#define FLONUM		double
#define SYSTEM
#define NIL		0L
#endif

/* default important definitions */
#ifndef NNODES
#define NNODES		1000
#endif
#ifndef TDEPTH
#define TDEPTH		500
#endif
#ifndef EDEPTH
#define EDEPTH		1000
#endif
#ifndef FORWARD
#define FORWARD
#endif
#ifndef LOCAL
#define LOCAL		static
#endif
#ifndef AFMT
#define AFMT		"%x"
#endif
#ifndef FIXNUM
#define FIXNUM		long
#endif
#ifndef ITYPE
#define ITYPE		long atol()
#endif
#ifndef ICNV
#define ICNV(n)		atol(n)
#endif
#ifndef IFMT
#define IFMT		"%ld"
#endif
#ifndef FLONUM
#define FLONUM		float
#endif

/* useful definitions */
#define TRUE	1
#define FALSE	0
#ifndef NIL
#define NIL	(NODE *)0
#endif

/* program limits */
#define STRMAX		100		/* maximum length of a string constant */
#define HSIZE		199		/* symbol hash table size */
#define SAMPLE		100		/* control character sample rate */
	
/* node types */
#define FREE	0
#define SUBR	1
#define FSUBR	2
#define LIST	3
#define SYM	4
#define INT	5
#define STR	6
#define OBJ	7
#define FPTR	8
#define FLOAT	9
#define VECT	10

/* node flags */
#define MARK	1
#define LEFT	2

/* string types */
#define DYNAMIC	0
#define STATIC	1

/* new node access macros */
#define ntype(x)	((x)->n_type)

/* type predicates */
#define atom(x)		((x) == NIL || (x)->n_type != LIST)
#define null(x)		((x) == NIL)
#define listp(x)	((x) == NIL || (x)->n_type == LIST)
#define consp(x)	((x) && (x)->n_type == LIST)
#define subrp(x)	((x) && (x)->n_type == SUBR)
#define fsubrp(x)	((x) && (x)->n_type == FSUBR)
#define stringp(x)	((x) && (x)->n_type == STR)
#define symbolp(x)	((x) && (x)->n_type == SYM)
#define filep(x)	((x) && (x)->n_type == FPTR)
#define objectp(x)	((x) && (x)->n_type == OBJ)
#define fixp(x)		((x) && (x)->n_type == INT)
#define floatp(x)	((x) && (x)->n_type == FLOAT)
#define vectorp(x)	((x) && (x)->n_type == VECT)

/* cons access macros */
#define car(x)		((x)->n_car)
#define cdr(x)		((x)->n_cdr)
#define rplaca(x,y)	((x)->n_car = (y))
#define rplacd(x,y)	((x)->n_cdr = (y))

/* symbol access macros */
#define getvalue(x)	((x)->n_symvalue)
#define setvalue(x,v)	((x)->n_symvalue = (v))
#define getplist(x)	((x)->n_symplist->n_cdr)
#define setplist(x,v)	((x)->n_symplist->n_cdr = (v))
#define getpname(x)	((x)->n_symplist->n_car)

/* vector access macros */
#define getsize(x)	((x)->n_vsize)
#define getelement(x,i)	((x)->n_vdata[i])
#define setelement(x,i,v) ((x)->n_vdata[i] = (v))

/* object access macros */
#define getclass(x)	((x)->n_vdata[0])
#define getivar(x,i)	((x)->n_vdata[i+1])
#define setivar(x,i,v)	((x)->n_vdata[i+1] = (v))

/* subr/fsubr access macros */
#define getsubr(x)	((x)->n_subr)

/* fixnum/flonum access macros */
#define getfixnum(x)	((x)->n_int)
#define getflonum(x)	((x)->n_float)

/* string access macros */
#define getstring(x)	((x)->n_str)
#define setstring(x,v)	((x)->n_str = (v))

/* file access macros */
#define getfile(x)	((x)->n_fp)
#define setfile(x,v)	((x)->n_fp = (v))
#define getsavech(x)	((x)->n_savech)
#define setsavech(x,v)	((x)->n_savech = (v))

/* symbol node */
#define n_symplist	n_info.n_xsym.xsy_plist
#define n_symvalue	n_info.n_xsym.xsy_value

/* subr/fsubr node */
#define n_subr		n_info.n_xsubr.xsu_subr

/* list node */
#define n_car		n_info.n_xlist.xl_car
#define n_cdr		n_info.n_xlist.xl_cdr

/* integer node */
#define n_int		n_info.n_xint.xi_int

/* float node */
#define n_float		n_info.n_xfloat.xf_float

/* string node */
#define n_str		n_info.n_xstr.xst_str
#define n_strtype	n_info.n_xstr.xst_type

/* file pointer node */
#define n_fp		n_info.n_xfptr.xf_fp
#define n_savech	n_info.n_xfptr.xf_savech

/* vector/object node */
#define n_vsize		n_info.n_xvect.xv_size
#define n_vdata		n_info.n_xvect.xv_data

/* node structure */
typedef struct node {
    char n_type;		/* type of node */
    char n_flags;		/* flag bits */
    union {			/* value */
	struct xsym {		/* symbol node */
	    struct node *xsy_plist;	/* symbol plist - (name . plist) */
	    struct node *xsy_value;	/* the current value */
	} n_xsym;
	struct xsubr {		/* subr/fsubr node */
	    struct node *(*xsu_subr)();	/* pointer to an internal routine */
	} n_xsubr;
	struct xlist {		/* list node (cons) */
	    struct node *xl_car;	/* the car pointer */
	    struct node *xl_cdr;	/* the cdr pointer */
	} n_xlist;
	struct xint {		/* integer node */
	    FIXNUM xi_int;		/* integer value */
	} n_xint;
	struct xfloat {		/* float node */
	    FLONUM xf_float;		/* float value */
	} n_xfloat;
	struct xstr {		/* string node */
	    int xst_type;		/* string type */
	    char *xst_str;		/* string pointer */
	} n_xstr;
	struct xfptr {		/* file pointer node */
	    FILE *xf_fp;		/* the file pointer */
	    int xf_savech;		/* lookahead character for input files */
	} n_xfptr;
	struct xvect {		/* vector node */
	    int xv_size;		/* vector size */
	    struct node **xv_data;	/* vector data */
	} n_xvect;
    } n_info;
} NODE;

/* execution context flags */
#define CF_GO		1
#define CF_RETURN	2
#define CF_THROW	4
#define CF_ERROR	8
#define CF_CLEANUP	16
#define CF_CONTINUE	32
#define CF_TOPLEVEL	64

/* execution context */
typedef struct context {
    int c_flags;			/* context type flags */
    struct node *c_expr;		/* expression (type dependant) */
    jmp_buf c_jmpbuf;			/* longjmp context */
    struct context *c_xlcontext;	/* old value of xlcontext */
    struct node ***c_xlstack;		/* old value of xlstack */
    struct node *c_xlenv;		/* old value of xlenv */
    int c_xltrace;			/* old value of xltrace */
} CONTEXT;

/* function table entry structure */
struct fdef {
    char *f_name;			/* function name */
    int f_type;				/* function type SUBR/FSUBR */
    struct node *(*f_fcn)();		/* function code */
};

/* memory segment structure definition */
struct segment {
    int sg_size;
    struct segment *sg_next;
    struct node sg_nodes[1];
};

/* external procedure declarations */
extern struct node ***xlsave();		/* generate a stack frame */
extern struct node *xleval();		/* evaluate an expression */
extern struct node *xlapply();		/* apply a function to arguments */
extern struct node *xlevlist();		/* evaluate a list of arguments */
extern struct node *xlarg();		/* fetch an argument */
extern struct node *xlevarg();		/* fetch and evaluate an argument */
extern struct node *xlmatch();		/* fetch an typed argument */
extern struct node *xlevmatch();	/* fetch and evaluate a typed arg */
extern struct node *xlgetfile();	/* fetch a file/stream argument */
extern struct node *xlsend();		/* send a message to an object */
extern struct node *xlenter();		/* enter a symbol */
extern struct node *xlsenter();		/* enter a symbol with a static pname */
extern struct node *xlmakesym();	/* make an uninterned symbol */
extern struct node *xlframe();		/* establish a new environment frame */
extern struct node *xlgetvalue();	/* get value of a symbol (checked) */
extern struct node *xlxgetvalue();	/* get value of a symbol */
extern struct node *xlygetvalue();	/* get value of a symbol (no ivars) */

extern struct node *cons();		/* (cons x y) */
extern struct node *consa();		/* (cons x nil) */
extern struct node *consd();		/* (cons nil x) */

extern struct node *cvsymbol();		/* convert a string to a symbol */
extern struct node *cvcsymbol();	/* (same but constant string) */
extern struct node *cvstring();		/* convert a string */
extern struct node *cvcstring();	/* (same but constant string) */
extern struct node *cvfile();		/* convert a FILE * to a file */
extern struct node *cvsubr();		/* convert a function to a subr/fsubr */
extern struct node *cvfixnum();		/* convert a fixnum */
extern struct node *cvflonum();		/* convert a flonum */

extern struct node *newstring();	/* create a new string */
extern struct node *newvector();	/* create a new vector */
extern struct node *newobject();	/* create a new object */

extern struct node *xlgetprop();	/* get the value of a property */
extern char *xlsymname();		/* get the print name of a symbol */


