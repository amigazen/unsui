/* xlglobals - xlisp global variables */

#include "xlisp.h"

/* symbols */
NODE *true = NIL;
NODE *s_quote = NIL, *s_function = NIL;
NODE *s_bquote = NIL, *s_comma = NIL, *s_comat = NIL;
NODE *s_evalhook = NIL, *s_applyhook = NIL;
NODE *s_lambda = NIL, *s_macro = NIL;
NODE *s_stdin = NIL, *s_stdout = NIL;
NODE *s_tracenable = NIL, *s_tlimit = NIL, *s_breakenable = NIL;
NODE *s_continue = NIL, *s_quit = NIL;
NODE *s_car = NIL, *s_cdr = NIL;
NODE *s_get = NIL, *s_svalue = NIL, *s_splist = NIL;
NODE *s_eql = NIL, *k_test = NIL, *k_tnot = NIL;
NODE *k_optional = NIL, *k_rest = NIL, *k_aux = NIL;
NODE *a_subr = NIL, *a_fsubr = NIL;
NODE *a_list = NIL, *a_sym = NIL, *a_int = NIL;
NODE *a_str = NIL, *a_obj = NIL, *a_fptr = NIL;
NODE *oblist = NIL, *keylist = NIL, *s_unbound = NIL;

/* evaluation variables */
NODE *xlstack = NIL;
NODE *xlenv = NIL;
NODE *xlnewenv = NIL;

/* exception handling variables */
CONTEXT *xlcontext = NULL;	/* current exception handler */
NODE *xlvalue = NIL;		/* exception value */

/* debugging variables */
int xldebug = 0;		/* debug level */
int xltrace = -1;		/* trace stack pointer */
NODE **trace_stack = NULL;	/* trace stack */

/* gensym variables */
char gsprefix[STRMAX+1] = { 'G',0 };	/* gensym prefix string */
int gsnumber = 1;		/* gensym number */

/* i/o variables */
int xlplevel = 0;		/* prompt nesting level */
int xlfsize = 0;		/* flat size of current print call */
int prompt = TRUE;		/* input prompt flag */

/* dynamic memory variables */
long total = 0L;		/* total memory in use */
int anodes = 0;			/* number of nodes to allocate */
int nnodes = 0;			/* number of nodes allocated */
int nsegs = 0;			/* number of segments allocated */
int nfree = 0;			/* number of nodes free */
int gccalls = 0;		/* number of gc calls */
struct segment *segs = NULL;	/* list of allocated segments */
NODE *fnodes = NIL;		/* list of free nodes */

/* object programming variables */
NODE *self = NIL, *class = NIL, *object = NIL;
NODE *new = NIL, *isnew = NIL, *msgcls = NIL, *msgclass = NIL;
int varcnt = 0;

/* general purpose string buffer */
char buf[STRMAX+1] = { 0 };
