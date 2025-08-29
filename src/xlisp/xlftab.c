/* xlftab.c - xlisp function table */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/

#include "xlisp.h"

/* external functions */
extern NODE
    *xeval(),*xapply(),*xfuncall(),*xquote(),*xfunction(),*xbquote(),
    *xlambda(),*xset(),*xsetq(),*xsetf(),*xdefun(),*xdefmacro(),
    *xgensym(),*xmakesymbol(),*xintern(),
    *xsymname(),*xsymvalue(),*xsymplist(),*xget(),*xputprop(),*xremprop(),
    *xhash(),*xmkarray(),*xaref(),
    *xcar(),*xcdr(),
    *xcaar(),*xcadr(),*xcdar(),*xcddr(),
    *xcaaar(),*xcaadr(),*xcadar(),*xcaddr(),
    *xcdaar(),*xcdadr(),*xcddar(),*xcdddr(),
    *xcaaaar(),*xcaaadr(),*xcaadar(),*xcaaddr(),
    *xcadaar(),*xcadadr(),*xcaddar(),*xcadddr(),
    *xcdaaar(),*xcdaadr(),*xcdadar(),*xcdaddr(),
    *xcddaar(),*xcddadr(),*xcdddar(),*xcddddr(),
    *xcons(),*xlist(),*xappend(),*xreverse(),*xlast(),*xnth(),*xnthcdr(),
    *xmember(),*xassoc(),*xsubst(),*xsublis(),*xremove(),*xlength(),
    *xmapc(),*xmapcar(),*xmapl(),*xmaplist(),
    *xrplca(),*xrplcd(),*xnconc(),*xdelete(),
    *xatom(),*xsymbolp(),*xnumberp(),*xboundp(),*xnull(),*xlistp(),*xconsp(),
    *xeq(),*xeql(),*xequal(),
    *xcond(),*xcase(),*xand(),*xor(),*xlet(),*xletstar(),*xif(),
    *xprog(),*xprogstar(),*xprog1(),*xprog2(),*xprogn(),*xgo(),*xreturn(),
    *xcatch(),*xthrow(),
    *xerror(),*xcerror(),*xbreak(),*xcleanup(),*xcontinue(),*xerrset(),
    *xbaktrace(),*xevalhook(),
    *xdo(),*xdostar(),*xdolist(),*xdotimes(),
    *xminusp(),*xzerop(),*xplusp(),*xevenp(),*xoddp(),
    *xfix(),*xfloat(),
    *xadd(),*xsub(),*xmul(),*xdiv(),*xrem(),*xmin(),*xmax(),*xabs(),
    *xadd1(),*xsub1(),*xbitand(),*xbitior(),*xbitxor(),*xbitnot(),
    *xsin(),*xcos(),*xtan(),*xexpt(),*xexp(),*xsqrt(),*xrand(),
    *xlss(),*xleq(),*xequ(),*xneq(),*xgeq(),*xgtr(),
    *xstrcat(),*xsubstr(),*xstring(),*xchar(),
    *xread(),*xprint(),*xprin1(),*xprinc(),*xterpri(),
    *xflatsize(),*xflatc(),
    *xopeni(),*xopeno(),*xclose(),*xrdchar(),*xpkchar(),*xwrchar(),*xreadline(),
    *xload(),*xgc(),*xexpand(),*xalloc(),*xmem(),*xtype(),*xexit();


/* the function table */
struct fdef ftab[] = {

	/* evaluator functions */
{	"EVAL",		SUBR,	xeval		},
{	"APPLY",	SUBR,	xapply		},
{	"FUNCALL",	SUBR,	xfuncall	},
{	"QUOTE",	FSUBR,	xquote		},
{	"FUNCTION",	FSUBR,	xfunction	},
{	"BACKQUOTE",	FSUBR,	xbquote		},
{	"LAMBDA",	FSUBR,	xlambda		},

	/* symbol functions */
{	"SET",		SUBR,	xset		},
{	"SETQ",		FSUBR,	xsetq		},
{	"SETF",		FSUBR,	xsetf		},
{	"DEFUN",	FSUBR,	xdefun		},
{	"DEFMACRO",	FSUBR,	xdefmacro	},
{	"GENSYM",	SUBR,	xgensym		},
{	"MAKE-SYMBOL",	SUBR,	xmakesymbol	},
{	"INTERN",	SUBR,	xintern		},
{	"SYMBOL-NAME",	SUBR,	xsymname	},
{	"SYMBOL-VALUE",	SUBR,	xsymvalue	},
{	"SYMBOL-PLIST",	SUBR,	xsymplist	},
{	"GET",		SUBR,	xget		},
{	"PUTPROP",	SUBR,	xputprop	},
{	"REMPROP",	SUBR,	xremprop	},
{	"HASH",		SUBR,	xhash		},

	/* array functions */
{	"MAKE-ARRAY",	SUBR,	xmkarray	},
{	"AREF",		SUBR,	xaref		},

	/* list functions */
{	"CAR",		SUBR,	xcar		},
{	"CDR",		SUBR,	xcdr		},

{	"CAAR",		SUBR,	xcaar		},
{	"CADR",		SUBR,	xcadr		},
{	"CDAR",		SUBR,	xcdar		},
{	"CDDR",		SUBR,	xcddr		},

{	"CAAAR",	SUBR,	xcaaar		},
{	"CAADR",	SUBR,	xcaadr		},
{	"CADAR",	SUBR,	xcadar		},
{	"CADDR",	SUBR,	xcaddr		},
{	"CDAAR",	SUBR,	xcdaar		},
{	"CDADR",	SUBR,	xcdadr		},
{	"CDDAR",	SUBR,	xcddar		},
{	"CDDDR",	SUBR,	xcdddr		},

{	"CAAAAR",	SUBR,	xcaaaar		},
{	"CAAADR",	SUBR,	xcaaadr		},
{	"CAADAR",	SUBR,	xcaadar		},
{	"CAADDR",	SUBR,	xcaaddr		},
{	"CADAAR",	SUBR,	xcadaar		},
{	"CADADR",	SUBR,	xcadadr		},
{	"CADDAR",	SUBR,	xcaddar		},
{	"CADDDR",	SUBR,	xcadddr		},
{	"CDAAAR",	SUBR,	xcdaaar		},
{	"CDAADR",	SUBR,	xcdaadr		},
{	"CDADAR",	SUBR,	xcdadar		},
{	"CDADDR",	SUBR,	xcdaddr		},
{	"CDDAAR",	SUBR,	xcddaar		},
{	"CDDADR",	SUBR,	xcddadr		},
{	"CDDDAR",	SUBR,	xcdddar		},
{	"CDDDDR",	SUBR,	xcddddr		},

{	"CONS",		SUBR,	xcons		},
{	"LIST",		SUBR,	xlist		},
{	"APPEND",	SUBR,	xappend		},
{	"REVERSE",	SUBR,	xreverse	},
{	"LAST",		SUBR,	xlast		},
{	"NTH",		SUBR,	xnth		},
{	"NTHCDR",	SUBR,	xnthcdr		},
{	"MEMBER",	SUBR,	xmember		},
{	"ASSOC",	SUBR,	xassoc		},
{	"SUBST",	SUBR,	xsubst		},
{	"SUBLIS",	SUBR,	xsublis		},
{	"REMOVE",	SUBR,	xremove		},
{	"LENGTH",	SUBR,	xlength		},
{	"MAPC",		SUBR,	xmapc		},
{	"MAPCAR",	SUBR,	xmapcar		},
{	"MAPL",		SUBR,	xmapl		},
{	"MAPLIST",	SUBR,	xmaplist	},

	/* destructive list functions */
{	"RPLACA",	SUBR,	xrplca		},
{	"RPLACD",	SUBR,	xrplcd		},
{	"NCONC",	SUBR,	xnconc		},
{	"DELETE",	SUBR,	xdelete		},

	/* predicate functions */
{	"ATOM",		SUBR,	xatom		},
{	"SYMBOLP",	SUBR,	xsymbolp	},
{	"NUMBERP",	SUBR,	xnumberp	},
{	"BOUNDP",	SUBR,	xboundp		},
{	"NULL",		SUBR,	xnull		},
{	"NOT",		SUBR,	xnull		},
{	"LISTP",	SUBR,	xlistp		},
{	"CONSP",	SUBR,	xconsp		},
{	"MINUSP",	SUBR,	xminusp		},
{	"ZEROP",	SUBR,	xzerop		},
{	"PLUSP",	SUBR,	xplusp		},
{	"EVENP",	SUBR,	xevenp		},
{	"ODDP",		SUBR,	xoddp		},
{	"EQ",		SUBR,	xeq		},
{	"EQL",		SUBR,	xeql		},
{	"EQUAL",	SUBR,	xequal		},

	/* control functions */
{	"COND",		FSUBR,	xcond		},
{	"CASE",		FSUBR,	xcase		},
{	"AND",		FSUBR,	xand		},
{	"OR",		FSUBR,	xor		},
{	"LET",		FSUBR,	xlet		},
{	"LET*",		FSUBR,	xletstar	},
{	"IF",		FSUBR,	xif		},
{	"PROG",		FSUBR,	xprog		},
{	"PROG*",	FSUBR,	xprogstar	},
{	"PROG1",	FSUBR,	xprog1		},
{	"PROG2",	FSUBR,	xprog2		},
{	"PROGN",	FSUBR,	xprogn		},
{	"GO",		FSUBR,	xgo		},
{	"RETURN",	SUBR,	xreturn		},
{	"DO",		FSUBR,	xdo		},
{	"DO*",		FSUBR,	xdostar		},
{	"DOLIST",	FSUBR,	xdolist		},
{	"DOTIMES",	FSUBR,	xdotimes	},
{	"CATCH",	FSUBR,	xcatch		},
{	"THROW",	SUBR,	xthrow		},

	/* debugging and error handling functions */
{	"ERROR",	SUBR,	xerror		},
{	"CERROR",	SUBR,	xcerror		},
{	"BREAK",	SUBR,	xbreak		},
{	"CLEAN-UP",	SUBR,	xcleanup	},
{	"CONTINUE",	SUBR,	xcontinue	},
{	"ERRSET",	FSUBR,	xerrset		},
{	"BAKTRACE",	SUBR,	xbaktrace	},
{	"EVALHOOK",	SUBR,	xevalhook	},

	/* arithmetic functions */
{	"TRUNCATE",	SUBR,	xfix		},
{	"FLOAT",	SUBR,	xfloat		},
{	"+",		SUBR,	xadd		},
{	"-",		SUBR,	xsub		},
{	"*",		SUBR,	xmul		},
{	"/",		SUBR,	xdiv		},
{	"1+",		SUBR,	xadd1		},
{	"1-",		SUBR,	xsub1		},
{	"REM",		SUBR,	xrem		},
{	"MIN",		SUBR,	xmin		},
{	"MAX",		SUBR,	xmax		},
{	"ABS",		SUBR,	xabs		},
{	"SIN",		SUBR,	xsin		},
{	"COS",		SUBR,	xcos		},
{	"TAN",		SUBR,	xtan		},
{	"EXPT",		SUBR,	xexpt		},
{	"EXP",		SUBR,	xexp		},
{	"SQRT",		SUBR,	xsqrt		},
{	"RANDOM",	SUBR,	xrand		},

	/* bitwise logical functions */
{	"BIT-AND",	SUBR,	xbitand		},
{	"BIT-IOR",	SUBR,	xbitior		},
{	"BIT-XOR",	SUBR,	xbitxor		},
{	"BIT-NOT",	SUBR,	xbitnot		},

	/* numeric comparison functions */
{	"<",		SUBR,	xlss		},
{	"<=",		SUBR,	xleq		},
{	"=",		SUBR,	xequ		},
{	"/=",		SUBR,	xneq		},
{	">=",		SUBR,	xgeq		},
{	">",		SUBR,	xgtr		},

	/* string functions */
{	"STRCAT",	SUBR,	xstrcat		},
{	"SUBSTR",	SUBR,	xsubstr		},
{	"STRING",	SUBR,	xstring		},
{	"CHAR",		SUBR,	xchar		},

	/* I/O functions */
{	"READ",		SUBR,	xread		},
{	"PRINT",	SUBR,	xprint		},
{	"PRIN1",	SUBR,	xprin1		},
{	"PRINC",	SUBR,	xprinc		},
{	"TERPRI",	SUBR,	xterpri		},
{	"FLATSIZE",	SUBR,	xflatsize	},
{	"FLATC",	SUBR,	xflatc		},

	/* file I/O functions */
{	"OPENI",	SUBR,	xopeni		},
{	"OPENO",	SUBR,	xopeno		},
{	"CLOSE",	SUBR,	xclose		},
{	"READ-CHAR",	SUBR,	xrdchar		},
{	"PEEK-CHAR",	SUBR,	xpkchar		},
{	"WRITE-CHAR",	SUBR,	xwrchar		},
{	"READ-LINE",	SUBR,	xreadline	},

	/* system functions */
{	"LOAD",		SUBR,	xload		},
{	"GC",		SUBR,	xgc		},
{	"EXPAND",	SUBR,	xexpand		},
{	"ALLOC",	SUBR,	xalloc		},
{	"MEM",		SUBR,	xmem		},
{	"TYPE-OF",	SUBR,	xtype		},
{	"EXIT",		SUBR,	xexit		},

{	0					}
};


