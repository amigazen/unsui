/* xlisp - a small subset of lisp */

/* system specific definitions */
#define UNIX

#ifdef AZTEC
#include "stdio.h"
#include "setjmp.h"
#else
#include <stdio.h>
#ifdef AMIGA
struct JMP_BUF {		/* Early versions of Lattice C for the */
	long jmpret;		/* AMIGA did not have <setjmp.h>, but */
	long jmp_d1;		/* did have the corresponding library */
	long jmp_d2;		/* functions.  This declaration is */
	long jmp_d3;		/* compatible with the library functions. */
	long jmp_d4;
	long jmp_d5;
	long jmp_d6;
	long jmp_d7;
	long jmp_a1;
	long jmp_a2;
	long jmp_a3;
	long jmp_a4;
	long jmp_a5;
	long jmp_a6;
	long jmp_a7;
};
typedef JMP_BUF jmp_buf[1];
#else
#include <setjmp.h>
#include <ctype.h>
#endif
#endif

/* NNODES	number of nodes to allocate in each request */
/* TDEPTH	trace stack depth */
/* FORWARD	type of a forward declaration (usually "") */
/* LOCAL	type of a local function (usually "static") */

/* for the Computer Innovations compiler */
#ifdef CI
#define NNODES		1000
#define TDEPTH		500
#endif

/* for the CPM68K compiler */
#ifdef CPM68K
#define NNODES		1000
#define TDEPTH		500
#define LOCAL
#define AFMT		"%lx"
#undef NULL
#define NULL		(char *)0
#endif

/* for the DeSmet compiler */
#ifdef DESMET
#define NNODES		1000
#define TDEPTH		500
#define LOCAL
#define getc(fp)	getcx(fp)
#define putc(ch,fp)	putcx(ch,fp)
#define EOF		-1
#endif

/* for the MegaMax compiler */
#ifdef MEGAMAX
#define NNODES		200
#define TDEPTH		100
#define LOCAL
#define AFMT		"%lx"
#define TSTKSIZE	(4 * TDEPTH)
#endif

/* for the VAX-11 C compiler */
#ifdef vms
#define NNODES		2000
#define TDEPTH		1000
#endif

/* for the DECUS C compiler */
#ifdef decus
#define NNODES		200
#define TDEPTH		100
#define FORWARD		extern
#endif

/* for unix compilers */
#ifdef unix
#define NNODES		200
#define TDEPTH		100
#endif

/* for the AZTEC C compiler */
#ifdef AZTEC
#define NNODES		200
#define TDEPTH		100
#define getc(fp)	agetc(fp)
#define putc(ch,fp)	aputc(ch,fp)
#endif

/* default important definitions */
#ifndef NNODES
#define NNODES		200
#endif
#ifndef TDEPTH
#define TDEPTH		100
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
#ifndef TSTKSIZE
#define TSTKSIZE	(sizeof(NODE *) * TDEPTH)
#endif

/* useful definitions */
#define TRUE	1
#define FALSE	0
#define NIL	(NODE *)0

/* program limits */
#define STRMAX		100		/* maximum length of a string constant */
	
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

/* node flags */
#define MARK	1
#define LEFT	2

/* string types */
#define DYNAMIC	0
#define STATIC	1

/* new node access macros */
#define ntype(x)	((x)->n_type)
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
#define car(x)		((x)->n_car)
#define cdr(x)		((x)->n_cdr)
#define rplaca(x,y)	((x)->n_car = (y))
#define rplacd(x,y)	((x)->n_cdr = (y))

/* symbol node */
#define n_symplist	n_info.n_xsym.xsy_plist
#define n_symvalue	n_info.n_xsym.xsy_value

/* subr/fsubr node */
#define n_subr		n_info.n_xsubr.xsu_subr

/* list node */
#define n_car		n_info.n_xlist.xl_car
#define n_cdr		n_info.n_xlist.xl_cdr
#define n_ptr		n_info.n_xlist.xl_car

/* integer node */
#define n_int		n_info.n_xint.xi_int

/* string node */
#define n_str		n_info.n_xstr.xst_str
#define n_strtype	n_info.n_xstr.xst_type

/* object node */
#define n_obclass	n_info.n_xobj.xo_obclass
#define n_obdata	n_info.n_xobj.xo_obdata

/* file pointer node */
#define n_fp		n_info.n_xfptr.xf_fp
#define n_savech	n_info.n_xfptr.xf_savech

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
	    int xi_int;			/* integer value */
	} n_xint;
	struct xstr {		/* string node */
	    int xst_type;		/* string type */
	    char *xst_str;		/* string pointer */
	} n_xstr;
	struct xobj {		/* object node */
	    struct node *xo_obclass;	/* class of object */
	    struct node *xo_obdata;	/* instance data */
	} n_xobj;
	struct xfptr {		/* file pointer node */
	    FILE *xf_fp;		/* the file pointer */
	    int xf_savech;		/* lookahead character for input files */
	} n_xfptr;
    } n_info;
} NODE;

/* execution context flags */
#define CF_GO		1
#define CF_RETURN	2
#define CF_THROW	4
#define CF_ERROR	8

/* execution context */
typedef struct context {
    int c_flags;			/* context type flags */
    struct node *c_expr;		/* expression (type dependant) */
    jmp_buf c_jmpbuf;			/* longjmp context */
    struct context *c_xlcontext;	/* old value of xlcontext */
    struct node *c_xlstack;		/* old value of xlstack */
    struct node *c_xlenv,*c_xlnewenv;	/* old values of xlenv and xlnewenv */
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
extern struct node *xleval();		/* evaluate an expression */
extern struct node *xlapply();		/* apply a function to arguments */
extern struct node *xlevlist();		/* evaluate a list of arguments */
extern struct node *xlarg();		/* fetch an argument */
extern struct node *xlevarg();		/* fetch and evaluate an argument */
extern struct node *xlmatch();		/* fetch an typed argument */
extern struct node *xlevmatch();	/* fetch and evaluate a typed arg */
extern struct node *xlsend();		/* send a message to an object */
extern struct node *xlenter();		/* enter a symbol */
extern struct node *xlsenter();		/* enter a symbol with a static pname */
extern struct node *xlintern();		/* intern a symbol */
extern struct node *xlmakesym();	/* make an uninterned symbol */
extern struct node *xlsave();		/* generate a stack frame */
extern struct node *xlobsym();		/* find an object's class or instance
					   variable */
extern struct node *xlgetprop();	/* get the value of a property */
extern char *xlsymname();		/* get the print name of a symbol */

extern struct node *newnode();		/* allocate a new node */
extern char *stralloc();		/* allocate string space */
extern char *strsave();			/* make a safe copy of a string */
