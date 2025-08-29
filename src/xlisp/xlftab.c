/* xlftab.c - xlisp function table */

#include "xlisp.h"

/* external functions */
extern NODE
    *xeval(),*xapply(),*xfuncall(),*xquote(),*xbquote(),
    *xset(),*xsetq(),*xsetf(),*xdefun(),*xdefmacro(),
    *xgensym(),*xmakesymbol(),*xintern(),
    *xsymname(),*xsymvalue(),*xsymplist(),*xget(),*xremprop(),
    *xcar(),*xcaar(),*xcadr(),*xcdr(),*xcdar(),*xcddr(),
    *xcons(),*xlist(),*xappend(),*xreverse(),*xlast(),*xnth(),*xnthcdr(),
    *xmember(),*xassoc(),*xsubst(),*xsublis(),*xremove(),*xlength(),
    *xmapc(),*xmapcar(),*xmapl(),*xmaplist(),
    *xrplca(),*xrplcd(),*xnconc(),*xdelete(),
    *xatom(),*xsymbolp(),*xnumberp(),*xboundp(),*xnull(),*xlistp(),*xconsp(),
    *xeq(),*xeql(),*xequal(),
    *xcond(),*xand(),*xor(),*xlet(),*xletstar(),*xif(),
    *xprog(),*xprogstar(),*xprog1(),*xprog2(),*xprogn(),*xgo(),*xreturn(),
    *xcatch(),*xthrow(),
    *xerror(),*xcerror(),*xbreak(),*xerrset(),*xbaktrace(),*xevalhook(),
    *xdo(),*xdostar(),*xdolist(),*xdotimes(),
    *xadd(),*xsub(),*xmul(),*xdiv(),*xrem(),*xmin(),*xmax(),*xabs(),
    *xadd1(),*xsub1(),*xbitand(),*xbitior(),*xbitxor(),*xbitnot(),
    *xminusp(),*xzerop(),*xplusp(),*xevenp(),*xoddp(),
    *xlss(),*xleq(),*xequ(),*xneq(),*xgeq(),*xgtr(),
    *xstrlen(),*xstrcat(),*xsubstr(),*xascii(),*xchr(),*xatoi(),*xitoa(),
    *xread(),*xprint(),*xprin1(),*xprinc(),*xterpri(),
    *xflatsize(),*xflatc(),*xexplode(),*xexplc(),*ximplode(),*xmaknam(),
    *xopeni(),*xopeno(),*xclose(),*xrdchar(),*xpkchar(),*xwrchar(),*xreadline(),
    *xload(),*xgc(),*xexpand(),*xalloc(),*xmem(),*xtype(),*xexit();

/* the function table */
struct fdef ftab[] = {

	/* evaluator functions */
{	"eval",		SUBR,	xeval		},
{	"apply",	SUBR,	xapply		},
{	"funcall",	SUBR,	xfuncall	},
{	"quote",	FSUBR,	xquote		},
{	"function",	FSUBR,	xquote		},
{	"backquote",	FSUBR,	xbquote		},

	/* symbol functions */
{	"set",		SUBR,	xset		},
{	"setq",		FSUBR,	xsetq		},
{	"setf",		FSUBR,	xsetf		},
{	"defun",	FSUBR,	xdefun		},
{	"defmacro",	FSUBR,	xdefmacro	},
{	"gensym",	SUBR,	xgensym		},
{	"make-symbol",	SUBR,	xmakesymbol	},
{	"intern",	SUBR,	xintern		},
{	"symbol-name",	SUBR,	xsymname	},
{	"symbol-value",	SUBR,	xsymvalue	},
{	"symbol-plist",	SUBR,	xsymplist	},
{	"get",		SUBR,	xget		},
{	"remprop",	SUBR,	xremprop	},

	/* list functions */
{	"car",		SUBR,	xcar		},
{	"caar",		SUBR,	xcaar		},
{	"cadr",		SUBR,	xcadr		},
{	"cdr",		SUBR,	xcdr		},
{	"cdar",		SUBR,	xcdar		},
{	"cddr",		SUBR,	xcddr		},
{	"cons",		SUBR,	xcons		},
{	"list",		SUBR,	xlist		},
{	"append",	SUBR,	xappend		},
{	"reverse",	SUBR,	xreverse	},
{	"last",		SUBR,	xlast		},
{	"nth",		SUBR,	xnth		},
{	"nthcdr",	SUBR,	xnthcdr		},
{	"member",	SUBR,	xmember		},
{	"assoc",	SUBR,	xassoc		},
{	"subst",	SUBR,	xsubst		},
{	"sublis",	SUBR,	xsublis		},
{	"remove",	SUBR,	xremove		},
{	"length",	SUBR,	xlength		},
{	"mapc",		SUBR,	xmapc		},
{	"mapcar",	SUBR,	xmapcar		},
{	"mapl",		SUBR,	xmapl		},
{	"maplist",	SUBR,	xmaplist	},

	/* destructive list functions */
{	"rplaca",	SUBR,	xrplca		},
{	"rplacd",	SUBR,	xrplcd		},
{	"nconc",	SUBR,	xnconc		},
{	"delete",	SUBR,	xdelete		},

	/* predicate functions */
{	"atom",		SUBR,	xatom		},
{	"symbolp",	SUBR,	xsymbolp	},
{	"numberp",	SUBR,	xnumberp	},
{	"boundp",	SUBR,	xboundp		},
{	"null",		SUBR,	xnull		},
{	"not",		SUBR,	xnull		},
{	"listp",	SUBR,	xlistp		},
{	"consp",	SUBR,	xconsp		},
{	"minusp",	SUBR,	xminusp		},
{	"zerop",	SUBR,	xzerop		},
{	"plusp",	SUBR,	xplusp		},
{	"evenp",	SUBR,	xevenp		},
{	"oddp",		SUBR,	xoddp		},
{	"eq",		SUBR,	xeq		},
{	"eql",		SUBR,	xeql		},
{	"equal",	SUBR,	xequal		},

	/* control functions */
{	"cond",		FSUBR,	xcond		},
{	"and",		FSUBR,	xand		},
{	"or",		FSUBR,	xor		},
{	"let",		FSUBR,	xlet		},
{	"let*",		FSUBR,	xletstar	},
{	"if",		FSUBR,	xif		},
{	"prog",		FSUBR,	xprog		},
{	"prog*",	FSUBR,	xprogstar	},
{	"prog1",	FSUBR,	xprog1		},
{	"prog2",	FSUBR,	xprog2		},
{	"progn",	FSUBR,	xprogn		},
{	"go",		FSUBR,	xgo		},
{	"return",	SUBR,	xreturn		},
{	"do",		FSUBR,	xdo		},
{	"do*",		FSUBR,	xdostar		},
{	"dolist",	FSUBR,	xdolist		},
{	"dotimes",	FSUBR,	xdotimes	},
{	"catch",	FSUBR,	xcatch		},
{	"throw",	SUBR,	xthrow		},

	/* debugging and error handling functions */
{	"error",	SUBR,	xerror		},
{	"cerror",	SUBR,	xcerror		},
{	"break",	SUBR,	xbreak		},
{	"errset",	FSUBR,	xerrset		},
{	"baktrace",	SUBR,	xbaktrace	},
{	"evalhook",	SUBR,	xevalhook	},

	/* arithmetic functions */
{	"+",		SUBR,	xadd		},
{	"-",		SUBR,	xsub		},
{	"*",		SUBR,	xmul		},
{	"/",		SUBR,	xdiv		},
{	"1+",		SUBR,	xadd1		},
{	"1-",		SUBR,	xsub1		},
{	"rem",		SUBR,	xrem		},
{	"min",		SUBR,	xmin		},
{	"max",		SUBR,	xmax		},
{	"abs",		SUBR,	xabs		},

	/* bitwise logical functions */
{	"bit-and",	SUBR,	xbitand		},
{	"bit-ior",	SUBR,	xbitior		},
{	"bit-xor",	SUBR,	xbitxor		},
{	"bit-not",	SUBR,	xbitnot		},

	/* numeric comparison functions */
{	"<",		SUBR,	xlss		},
{	"<=",		SUBR,	xleq		},
{	"=",		SUBR,	xequ		},
{	"/=",		SUBR,	xneq		},
{	">=",		SUBR,	xgeq		},
{	">",		SUBR,	xgtr		},

	/* string functions */
{	"strlen",	SUBR,	xstrlen		},
{	"strcat",	SUBR,	xstrcat		},
{	"substr",	SUBR,	xsubstr		},
{	"ascii",	SUBR,	xascii		},
{	"chr",		SUBR,	xchr		},
{	"atoi",		SUBR,	xatoi		},
{	"itoa",		SUBR,	xitoa		},

	/* I/O functions */
{	"read",		SUBR,	xread		},
{	"print",	SUBR,	xprint		},
{	"prin1",	SUBR,	xprin1		},
{	"princ",	SUBR,	xprinc		},
{	"terpri",	SUBR,	xterpri		},
{	"flatsize",	SUBR,	xflatsize	},
{	"flatc",	SUBR,	xflatc		},
{	"explode",	SUBR,	xexplode	},
{	"explodec",	SUBR,	xexplc		},
{	"implode",	SUBR,	ximplode	},
{	"maknam",	SUBR,	xmaknam		},

	/* file I/O functions */
{	"openi",	SUBR,	xopeni		},
{	"openo",	SUBR,	xopeno		},
{	"close",	SUBR,	xclose		},
{	"read-char",	SUBR,	xrdchar		},
{	"peek-char",	SUBR,	xpkchar		},
{	"write-char",	SUBR,	xwrchar		},
{	"readline",	SUBR,	xreadline	},

	/* system functions */
{	"load",		SUBR,	xload		},
{	"gc",		SUBR,	xgc		},
{	"expand",	SUBR,	xexpand		},
{	"alloc",	SUBR,	xalloc		},
{	"mem",		SUBR,	xmem		},
{	"type",		SUBR,	xtype		},
{	"exit",		SUBR,	xexit		},

{	0					}
};
