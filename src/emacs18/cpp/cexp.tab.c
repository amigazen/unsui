
/*  A Bison parser, made from cexp.y  */

#define YYBISON 1  /* Identify Bison output.  */

#define	INT	258
#define	CHAR	259
#define	NAME	260
#define	ERROR	261
#define	OR	262
#define	AND	263
#define	EQUAL	264
#define	NOTEQUAL	265
#define	LEQ	266
#define	GEQ	267
#define	LSH	268
#define	RSH	269
#define	UNARY	270

#line 99 "cexp.y"

#include <setjmp.h>
/* #define YYDEBUG 1 */

  static int yylex ();
  static yyerror ();
  int expression_value;

  static jmp_buf parse_return_error;

  /* some external tables of character types */
  extern unsigned char is_idstart[], is_idchar[];


#line 114 "cexp.y"
typedef union {
  long lval;
  int voidval;
  char *sval;
} YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __STDC__
#define const
#endif



#define	YYFINAL		59
#define	YYFLAG		-32768
#define	YYNTBASE	33

#define YYTRANSLATE(x) ((unsigned)(x) <= 270 ? yytranslate[x] : 36)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    27,     2,     2,     2,    25,    12,     2,    29,
    30,    23,    21,     7,    22,     2,    24,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    32,     2,    15,
     2,    16,    31,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,    11,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,    10,     2,    28,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     8,     9,    13,    14,    17,    18,    19,    20,    26
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     8,    11,    14,    17,    21,    25,    29,
    33,    37,    41,    45,    49,    53,    57,    61,    65,    69,
    73,    77,    81,    85,    89,    93,    99,   101,   103
};

#endif

static const short yyrhs[] = {    34,
     0,    35,     0,    34,     7,    35,     0,    22,    35,     0,
    27,    35,     0,    28,    35,     0,    29,    34,    30,     0,
    35,    23,    35,     0,    35,    24,    35,     0,    35,    25,
    35,     0,    35,    21,    35,     0,    35,    22,    35,     0,
    35,    19,    35,     0,    35,    20,    35,     0,    35,    13,
    35,     0,    35,    14,    35,     0,    35,    17,    35,     0,
    35,    18,    35,     0,    35,    15,    35,     0,    35,    16,
    35,     0,    35,    12,    35,     0,    35,    11,    35,     0,
    35,    10,    35,     0,    35,     9,    35,     0,    35,     8,
    35,     0,    35,    31,    35,    32,    35,     0,     3,     0,
     4,     0,     5,     0
};

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   140,   145,   146,   151,   153,   155,   157,   162,   164,   166,
   168,   170,   172,   174,   176,   178,   180,   182,   184,   186,
   188,   190,   192,   194,   196,   198,   200,   202,   204
};

static const char * const yytname[] = {   "$","error","$illegal.","INT","CHAR",
"NAME","ERROR","','","OR","AND","'|'","'^'","'&'","EQUAL","NOTEQUAL","'<'","'>'",
"LEQ","GEQ","LSH","RSH","'+'","'-'","'*'","'/'","'%'","UNARY","'!'","'~'","'('",
"')'","'?'","':'","start","exp1","exp",""
};
#endif

static const short yyr1[] = {     0,
    33,    34,    34,    35,    35,    35,    35,    35,    35,    35,
    35,    35,    35,    35,    35,    35,    35,    35,    35,    35,
    35,    35,    35,    35,    35,    35,    35,    35,    35
};

static const short yyr2[] = {     0,
     1,     1,     3,     2,     2,     2,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     5,     1,     1,     1
};

static const short yydefact[] = {     0,
    27,    28,    29,     0,     0,     0,     0,     1,     2,     4,
     5,     6,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     7,     3,    25,    24,    23,    22,    21,
    15,    16,    19,    20,    17,    18,    13,    14,    11,    12,
     8,     9,    10,     0,     0,    26,     0,     0,     0
};

static const short yydefgoto[] = {    57,
     8,     9
};

static const short yypact[] = {    27,
-32768,-32768,-32768,    27,    27,    27,    27,-    3,    74,-   28,
-   28,-   28,    45,    27,    27,    27,    27,    27,    27,    27,
    27,    27,    27,    27,    27,    27,    27,    27,    27,    27,
    27,    27,    27,-32768,    74,    97,   119,    22,   139,   158,
   175,   175,   188,   188,   188,   188,   193,   193,   197,   197,
-   28,-   28,-   28,    49,    27,    74,     5,     6,-32768
};

static const short yypgoto[] = {-32768,
     0,-    4
};


#define	YYLAST		228


static const short yytable[] = {    10,
    11,    12,    33,    14,    58,    59,    13,     0,     0,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    49,    50,    51,    52,    53,    54,     1,
     2,     3,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,     0,     4,     0,
    56,    14,    33,     5,     6,     7,    15,    16,    17,    18,
    19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
    29,    30,    31,    32,    34,     0,     0,     0,     0,    33,
    55,    15,    16,    17,    18,    19,    20,    21,    22,    23,
    24,    25,    26,    27,    28,    29,    30,    31,    32,     0,
     0,     0,     0,     0,    33,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,     0,     0,     0,     0,     0,    33,    17,    18,
    19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
    29,    30,    31,    32,     0,     0,     0,     0,     0,    33,
    19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
    29,    30,    31,    32,     0,     0,     0,     0,     0,    33,
    20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
    30,    31,    32,     0,     0,     0,     0,     0,    33,    22,
    23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
     0,     0,     0,     0,     0,    33,    26,    27,    28,    29,
    30,    31,    32,    28,    29,    30,    31,    32,    33,    30,
    31,    32,     0,    33,     0,     0,     0,    33
};

static const short yycheck[] = {     4,
     5,     6,    31,     7,     0,     0,     7,-    1,-    1,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,    26,    27,    28,    29,    30,    31,    32,    33,     3,
     4,     5,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,    23,    24,    25,-    1,    22,-    1,
    55,     7,    31,    27,    28,    29,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    25,    30,-    1,-    1,-    1,-    1,    31,
    32,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,-    1,
-    1,-    1,-    1,-    1,    31,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
    24,    25,-    1,-    1,-    1,-    1,-    1,    31,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    25,-    1,-    1,-    1,-    1,-    1,    31,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    25,-    1,-    1,-    1,-    1,-    1,    31,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    24,    25,-    1,-    1,-    1,-    1,-    1,    31,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
-    1,-    1,-    1,-    1,-    1,    31,    19,    20,    21,    22,
    23,    24,    25,    21,    22,    23,    24,    25,    31,    23,
    24,    25,-    1,    31,-    1,-    1,-    1,    31
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 169 "bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
#ifdef YYLSP_NEEDED
		 &yyls1, size * sizeof (*yylsp),
#endif
		 &yystacksize);

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 141 "cexp.y"
{ expression_value = yyvsp[0].lval; ;
    break;}
case 3:
#line 147 "cexp.y"
{ yyval.lval = yyvsp[0].lval; ;
    break;}
case 4:
#line 152 "cexp.y"
{ yyval.lval = - yyvsp[0].lval; ;
    break;}
case 5:
#line 154 "cexp.y"
{ yyval.lval = ! yyvsp[0].lval; ;
    break;}
case 6:
#line 156 "cexp.y"
{ yyval.lval = ~ yyvsp[0].lval; ;
    break;}
case 7:
#line 158 "cexp.y"
{ yyval.lval = yyvsp[-1].lval; ;
    break;}
case 8:
#line 163 "cexp.y"
{ yyval.lval = yyvsp[-2].lval * yyvsp[0].lval; ;
    break;}
case 9:
#line 165 "cexp.y"
{ yyval.lval = yyvsp[-2].lval / yyvsp[0].lval; ;
    break;}
case 10:
#line 167 "cexp.y"
{ yyval.lval = yyvsp[-2].lval % yyvsp[0].lval; ;
    break;}
case 11:
#line 169 "cexp.y"
{ yyval.lval = yyvsp[-2].lval + yyvsp[0].lval; ;
    break;}
case 12:
#line 171 "cexp.y"
{ yyval.lval = yyvsp[-2].lval - yyvsp[0].lval; ;
    break;}
case 13:
#line 173 "cexp.y"
{ yyval.lval = yyvsp[-2].lval << yyvsp[0].lval; ;
    break;}
case 14:
#line 175 "cexp.y"
{ yyval.lval = yyvsp[-2].lval >> yyvsp[0].lval; ;
    break;}
case 15:
#line 177 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval == yyvsp[0].lval); ;
    break;}
case 16:
#line 179 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval != yyvsp[0].lval); ;
    break;}
case 17:
#line 181 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval <= yyvsp[0].lval); ;
    break;}
case 18:
#line 183 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval >= yyvsp[0].lval); ;
    break;}
case 19:
#line 185 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval < yyvsp[0].lval); ;
    break;}
case 20:
#line 187 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval > yyvsp[0].lval); ;
    break;}
case 21:
#line 189 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval & yyvsp[0].lval); ;
    break;}
case 22:
#line 191 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval ^ yyvsp[0].lval); ;
    break;}
case 23:
#line 193 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval | yyvsp[0].lval); ;
    break;}
case 24:
#line 195 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval && yyvsp[0].lval); ;
    break;}
case 25:
#line 197 "cexp.y"
{ yyval.lval = (yyvsp[-2].lval || yyvsp[0].lval); ;
    break;}
case 26:
#line 199 "cexp.y"
{ yyval.lval = yyvsp[-4].lval ? yyvsp[-2].lval : yyvsp[0].lval; ;
    break;}
case 27:
#line 201 "cexp.y"
{ yyval.lval = yylval.lval; ;
    break;}
case 28:
#line 203 "cexp.y"
{ yyval.lval = yylval.lval; ;
    break;}
case 29:
#line 205 "cexp.y"
{ yyval.lval = 0; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 442 "bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 207 "cexp.y"


/* During parsing of a C expression, the pointer to the next character
   is in this variable.  */

static char *lexptr;

/* Take care of parsing a number (anything that starts with a digit).
   Set yylval and return the token type; update lexptr.
   LEN is the number of characters in it.  */

/* maybe needs to actually deal with floating point numbers */

static int
parse_number (olen)
     int olen;
{
  register char *p = lexptr;
  register long n = 0;
  register int c;
  register int base = 10;
  register len = olen;
  char *err_copy;

  extern double atof ();

  for (c = 0; c < len; c++)
    if (p[c] == '.') {
      /* It's a float since it contains a point.  */
      yyerror ("floating point numbers not allowed in #if expressions");
      return ERROR;
      
/* ****************
	 yylval.dval = atof (p);
	 lexptr += len;
	 return FLOAT;
		 ****************  */
    }
  
  if (len >= 3 && (!strncmp (p, "0x", 2) || !strncmp (p, "0X", 2))) {
    p += 2;
    base = 16;
    len -= 2;
  }
  else if (*p == '0')
    base = 8;
  
  while (len-- > 0) {
    c = *p++;
    n *= base;
    if (c >= '0' && c <= '9')
      n += c - '0';
    else {
      if (c >= 'A' && c <= 'Z') c += 'a' - 'A';
      if (base == 16 && c >= 'a' && c <= 'f')
	n += c - 'a' + 10;
      else if (len == 0 && c == 'l')
	;
      else {
	yyerror ("Invalid number in #if expression");
	return ERROR;
      }
    }
  }

  lexptr = p;
  yylval.lval = n;
  return INT;
}

struct token {
  char *operator;
  int token;
};

#define NULL 0

static struct token tokentab2[] = {
  {"&&", AND},
  {"||", OR},
  {"<<", LSH},
  {">>", RSH},
  {"==", EQUAL},
  {"!=", NOTEQUAL},
  {"<=", LEQ},
  {">=", GEQ},
  {NULL, ERROR}
};

/* Read one token, getting characters through lexptr.  */

static int
yylex ()
{
  register int c;
  register int namelen;
  register char *tokstart;
  register struct token *toktab;

 retry:

  tokstart = lexptr;
  c = *tokstart;
  /* See if it is a special token of length 2.  */
  for (toktab = tokentab2; toktab->operator != NULL; toktab++)
    if (c == *toktab->operator && tokstart[1] == toktab->operator[1]) {
      lexptr += 2;
      return toktab->token;
    }

  switch (c) {
  case 0:
    return 0;
    
  case ' ':
  case '\t':
  case '\n':
    lexptr++;
    goto retry;
    
  case '\'':
    lexptr++;
    c = *lexptr++;
    if (c == '\\')
      c = parse_escape (&lexptr);
    yylval.lval = c;
    c = *lexptr++;
    if (c != '\'') {
      yyerror ("Invalid character constant in #if");
      return ERROR;
    }
    
    return CHAR;

  case '/':			/* possible comment */
    if (*lexptr != '*')
      return c;
    for (;;) {
      while (*lexptr != '\0') {
	if (*lexptr++ == '*' && *lexptr == '/') {
	  lexptr++;
	  goto retry;
	}
      }
    }

    /* some of these chars are invalid in constant expressions;
       maybe do something about them later */
  case '+':
  case '-':
  case '*':
  case '%':
  case '|':
  case '&':
  case '^':
  case '~':
  case '!':
  case '@':
  case '<':
  case '>':
  case '(':
  case ')':
  case '[':
  case ']':
  case '.':
  case '?':
  case ':':
  case '=':
  case '{':
  case '}':
  case ',':
    lexptr++;
    return c;
    
  case '"':
    yyerror ("double quoted strings not allowed in #if expressions");
    return ERROR;
  }
  if (c >= '0' && c <= '9') {
    /* It's a number */
    for (namelen = 0;
	 c = tokstart[namelen], is_idchar[c] || c == '.'; 
	 namelen++)
      ;
    return parse_number (namelen);
  }
  
  if (!is_idstart[c]) {
    yyerror ("Invalid token in expression");
    return ERROR;
  }
  
  /* It is a name.  See how long it is.  */
  
  for (namelen = 0; is_idchar[tokstart[namelen]]; namelen++)
    ;
  
  lexptr += namelen;
  return NAME;
}


/* Parse a C escape sequence.  STRING_PTR points to a variable
   containing a pointer to the string to parse.  That pointer
   is updated past the characters we use.  The value of the
   escape sequence is returned.

   A negative value means the sequence \ newline was seen,
   which is supposed to be equivalent to nothing at all.

   If \ is followed by a null character, we return a negative
   value and leave the string pointer pointing at the null character.

   If \ is followed by 000, we return 0 and leave the string pointer
   after the zeros.  A value of 0 does not mean end of string.  */

static int
parse_escape (string_ptr)
     char **string_ptr;
{
  register int c = *(*string_ptr)++;
  switch (c)
    {
    case 'a':
      return '\a';
    case 'b':
      return '\b';
    case 'e':
      return 033;
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case '\n':
      return -2;
    case 0:
      (*string_ptr)--;
      return 0;
    case '^':
      c = *(*string_ptr)++;
      if (c == '\\')
	c = parse_escape (string_ptr);
      if (c == '?')
	return 0177;
      return (c & 0200) | (c & 037);
      
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      {
	register int i = c - '0';
	register int count = 0;
	while (++count < 3)
	  {
	    if ((c = *(*string_ptr)++) >= '0' && c <= '7')
	      {
		i *= 8;
		i += c - '0';
	      }
	    else
	      {
		(*string_ptr)--;
		break;
	      }
	  }
	return i;
      }
    default:
      return c;
    }
}

static
yyerror (s)
     char *s;
{
  error (s);
  longjmp (parse_return_error, 1);
}

/* This page contains the entry point to this file.  */

/* Parse STRING as an expression, and complain if this fails
   to use up all of the contents of STRING.  */
int
parse_c_expression (string)
     char *string;
{
  lexptr = string;
  
  if (lexptr == 0 || *lexptr == 0) {
    error ("empty #if expression");
    return 0;			/* don't include the #if group */
  }

  /* if there is some sort of scanning error, just return 0 and assume
     the parsing routine has printed an error message somewhere.
     there is surely a better thing to do than this.     */
  if (setjmp(parse_return_error))
    return 0;

  if (yyparse ())
    return 0;			/* actually this is never reached
				   the way things stand. */
  if (*lexptr)
    error ("Junk after end of expression.");

  return expression_value;	/* set by yyparse() */
}

#ifdef TEST_EXP_READER
/* main program, for testing purposes. */
main()
{
  int n;
  char buf[1024];
  extern int yydebug;
/*
  yydebug = 1;
*/
  initialize_random_junk ();

  for (;;) {
    printf("enter expression: ");
    n = 0;
    while ((buf[n] = getchar()) != '\n')
      n++;
    buf[n] = '\0';
    printf("parser returned %d\n", parse_c_expression(buf));
  }
}

/* table to tell if char can be part of a C identifier. */
char is_idchar[256];
/* table to tell if char can be first char of a c identifier. */
char is_idstart[256];
/* table to tell if c is horizontal space.  isspace() thinks that
   newline is space; this is not a good idea for this program. */
char is_hor_space[256];

/*
 * initialize random junk in the hash table and maybe other places
 */
initialize_random_junk()
{
  register int i;

  /*
   * Set up is_idchar and is_idstart tables.  These should be
   * faster than saying (is_alpha(c) || c == '_'), etc.
   * Must do set up these things before calling any routines tthat
   * refer to them.
   */
  for (i = 'a'; i <= 'z'; i++) {
    ++is_idchar[i - 'a' + 'A'];
    ++is_idchar[i];
    ++is_idstart[i - 'a' + 'A'];
    ++is_idstart[i];
  }
  for (i = '0'; i <= '9'; i++)
    ++is_idchar[i];
  ++is_idchar['_'];
  ++is_idstart['_'];

  /* horizontal space table */
  ++is_hor_space[' '];
  ++is_hor_space['\t'];
}

error (msg)
{
  printf("error: %s\n", msg);
}
#endif
