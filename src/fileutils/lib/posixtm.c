
/*  A Bison parser, made from posixtm.y  */

#define	DIGIT	258

#line 19 "posixtm.y"

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#ifdef sparc
#include <alloca.h>
#else
#ifdef _AIX
 #pragma alloca
#else
char *alloca ();
#endif
#endif
#endif

#include <stdio.h>
#include <sys/types.h>
#include <time.h>

#define YYDEBUG 1

/* Lexical analyzer's current scan position in the input string. */
static char *curpos;

/* The return value. */
static struct tm t;

time_t mktime ();

#define zzparse posixtime_zzparse
static int zzlex ();
static int zzerror ();

#ifndef YYLTYPE
typedef
  struct zzltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  zzltype;

#define YYLTYPE zzltype
#endif

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __STDC__
#define const
#endif



#define	YYFINAL		15
#define	YYFLAG		-32768
#define	YYNTBASE	5

#define YYTRANSLATE(x) ((unsigned)(x) <= 258 ? zztranslate[x] : 9)

static const char zztranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     4,     2,     2,     2,     2,
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
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3
};

static const short zzprhs[] = {     0,
     0,     7,     9,    12,    13,    14,    17
};

static const short zzrhs[] = {     8,
     8,     8,     8,     6,     7,     0,     8,     0,     8,     8,
     0,     0,     0,     4,     8,     0,     3,     3,     0
};

#if YYDEBUG != 0
static const short zzrline[] = { 0,
    56,    85,    92,    99,   110,   113,   122
};

static const char * const zztname[] = {   "$",
"error","$illegal.","DIGIT","'.'","date","year","seconds","digitpair",""
};
#endif

static const short zzr1[] = {     0,
     5,     6,     6,     6,     7,     7,     8
};

static const short zzr2[] = {     0,
     6,     1,     2,     0,     0,     2,     2
};

static const short zzdefact[] = {     0,
     0,     0,     7,     0,     0,     4,     5,     2,     0,     1,
     3,     6,     0,     0,     0
};

static const short zzdefgoto[] = {    13,
     7,    10,     2
};

static const short zzpact[] = {     2,
     5,     2,-32768,     2,     2,     2,-    3,     2,     2,-32768,
-32768,-32768,     9,    10,-32768
};

static const short zzpgoto[] = {-32768,
-32768,-32768,-    2
};


#define	YYLAST		10


static const short zztable[] = {     4,
     9,     5,     6,     8,     1,    11,    12,     3,    14,    15
};

static const short zzcheck[] = {     2,
     4,     4,     5,     6,     3,     8,     9,     3,     0,     0
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
#else /* Not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
#include <alloca.h>
#else /* Not sparc */
#ifdef MSDOS
#include <malloc.h>
#endif /* MSDOS */
#endif /* Not sparc.  */
#endif /* Not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define zzerrok		(zzerrstatus = 0)
#define zzclearin	(zzchar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto zzerrlab1
/* Like YYERROR except do call zzerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto zzerrlab
#define YYRECOVERING()  (!!zzerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (zzchar == YYEMPTY && zzlen == 1)				\
    { zzchar = (token), zzlval = (value);			\
      zzchar1 = YYTRANSLATE (zzchar);				\
      YYPOPSTACK;						\
      goto zzbackup;						\
    }								\
  else								\
    { zzerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		zzlex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		zzlex(&zzlval, &zzlloc)
#else
#define YYLEX		zzlex(&zzlval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	zzchar;			/*  the lookahead symbol		*/
YYSTYPE	zzlval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE zzlloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int zznerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int zzdebug;			/*  nonzero means print parse trace	*/
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

#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__zz_bcopy (from, to, count)
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
__zz_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif

#line 160 "bison.simple"
int
zzparse()
{
  register int zzstate;
  register int zzn;
  register short *zzssp;
  register YYSTYPE *zzvsp;
  int zzerrstatus;	/*  number of tokens to shift before error messages enabled */
  int zzchar1;		/*  lookahead token as an internal (translated) token number */

  short	zzssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE zzvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *zzss = zzssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *zzvs = zzvsa;	/*  to allow zzoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE *zzls = zzlsa;
  YYLTYPE *zzlsp;
  YYLTYPE zzlsa[YYINITDEPTH];	/*  the location stack			*/

#define YYPOPSTACK   (zzvsp--, zzsp--, zzlsp--)
#else
#define YYPOPSTACK   (zzvsp--, zzsp--)
#endif

  int zzstacksize = YYINITDEPTH;

#ifdef YYPURE
  int zzchar;
  YYSTYPE zzlval;
  int zznerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE zzlloc;
#endif
#endif

  YYSTYPE zzval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int zzlen;

#if YYDEBUG != 0
  if (zzdebug)
    fprintf(stderr, "Starting parse\n");
#endif

  zzstate = 0;
  zzerrstatus = 0;
  zznerrs = 0;
  zzchar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.  */

  zzssp = zzss - 1;
  zzvsp = zzvs;
#ifdef YYLSP_NEEDED
  zzlsp = zzls;
#endif

/* Push a new state, which is found in  zzstate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
zznewstate:

  *++zzssp = zzstate;

  if (zzssp >= zzss + zzstacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *zzvs1 = zzvs;
      short *zzss1 = zzss;
#ifdef YYLSP_NEEDED
      YYLTYPE *zzls1 = zzls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = zzssp - zzss + 1;

#ifdef zzoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
      zzoverflow("parser stack overflow",
		 &zzss1, size * sizeof (*zzssp),
		 &zzvs1, size * sizeof (*zzvsp),
#ifdef YYLSP_NEEDED
		 &zzls1, size * sizeof (*zzlsp),
#endif
		 &zzstacksize);

      zzss = zzss1; zzvs = zzvs1;
#ifdef YYLSP_NEEDED
      zzls = zzls1;
#endif
#else /* no zzoverflow */
      /* Extend the stack our own way.  */
      if (zzstacksize >= YYMAXDEPTH)
	{
	  zzerror("parser stack overflow");
	  return 2;
	}
      zzstacksize *= 2;
      if (zzstacksize > YYMAXDEPTH)
	zzstacksize = YYMAXDEPTH;
      zzss = (short *) alloca (zzstacksize * sizeof (*zzssp));
      __zz_bcopy ((char *)zzss1, (char *)zzss, size * sizeof (*zzssp));
      zzvs = (YYSTYPE *) alloca (zzstacksize * sizeof (*zzvsp));
      __zz_bcopy ((char *)zzvs1, (char *)zzvs, size * sizeof (*zzvsp));
#ifdef YYLSP_NEEDED
      zzls = (YYLTYPE *) alloca (zzstacksize * sizeof (*zzlsp));
      __zz_bcopy ((char *)zzls1, (char *)zzls, size * sizeof (*zzlsp));
#endif
#endif /* no zzoverflow */

      zzssp = zzss + size - 1;
      zzvsp = zzvs + size - 1;
#ifdef YYLSP_NEEDED
      zzlsp = zzls + size - 1;
#endif

#if YYDEBUG != 0
      if (zzdebug)
	fprintf(stderr, "Stack size increased to %d\n", zzstacksize);
#endif

      if (zzssp >= zzss + zzstacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (zzdebug)
    fprintf(stderr, "Entering state %d\n", zzstate);
#endif

 zzbackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* zzresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  zzn = zzpact[zzstate];
  if (zzn == YYFLAG)
    goto zzdefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* zzchar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (zzchar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (zzdebug)
	fprintf(stderr, "Reading a token: ");
#endif
      zzchar = YYLEX;
    }

  /* Convert token to internal form (in zzchar1) for indexing tables with */

  if (zzchar <= 0)		/* This means end of input. */
    {
      zzchar1 = 0;
      zzchar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (zzdebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      zzchar1 = YYTRANSLATE(zzchar);

#if YYDEBUG != 0
      if (zzdebug)
	fprintf(stderr, "Next token is %d (%s)\n", zzchar, zztname[zzchar1]);
#endif
    }

  zzn += zzchar1;
  if (zzn < 0 || zzn > YYLAST || zzcheck[zzn] != zzchar1)
    goto zzdefault;

  zzn = zztable[zzn];

  /* zzn is what to do for this token type in this state.
     Negative => reduce, -zzn is rule number.
     Positive => shift, zzn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (zzn < 0)
    {
      if (zzn == YYFLAG)
	goto zzerrlab;
      zzn = -zzn;
      goto zzreduce;
    }
  else if (zzn == 0)
    goto zzerrlab;

  if (zzn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (zzdebug)
    fprintf(stderr, "Shifting token %d (%s), ", zzchar, zztname[zzchar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (zzchar != YYEOF)
    zzchar = YYEMPTY;

  *++zzvsp = zzlval;
#ifdef YYLSP_NEEDED
  *++zzlsp = zzlloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (zzerrstatus) zzerrstatus--;

  zzstate = zzn;
  goto zznewstate;

/* Do the default action for the current state.  */
zzdefault:

  zzn = zzdefact[zzstate];
  if (zzn == 0)
    goto zzerrlab;

/* Do a reduction.  zzn is the number of a rule to reduce with.  */
zzreduce:
  zzlen = zzr2[zzn];
  zzval = zzvsp[1-zzlen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (zzdebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       zzn, zzrline[zzn]);

      /* Print the symboles being reduced, and their result.  */
      for (i = zzprhs[zzn]; zzrhs[i] > 0; i++)
	fprintf (stderr, "%s ", zztname[zzrhs[i]]);
      fprintf (stderr, " -> %s\n", zztname[zzr1[zzn]]);
    }
#endif


  switch (zzn) {

case 1:
#line 62 "posixtm.y"
{
	         if (zzvsp[-5] >= 1 && zzvsp[-5] <= 12)
		   t.tm_mon = zzvsp[-5] - 1;
		 else {
		   YYABORT;
		 }
		 if (zzvsp[-4] >= 1 && zzvsp[-4] <= 31)
		   t.tm_mday = zzvsp[-4];
		 else {
		   YYABORT;
		 }
		 if (zzvsp[-3] >= 0 && zzvsp[-3] <= 23)
		   t.tm_hour = zzvsp[-3];
		 else {
		   YYABORT;
		 }
		 if (zzvsp[-2] >= 0 && zzvsp[-2] <= 59)
		   t.tm_min = zzvsp[-2];
		 else {
		   YYABORT;
		 }
	       ;
    break;}
case 2:
#line 85 "posixtm.y"
{
                   t.tm_year = zzvsp[0];
		   /* Deduce the century based on the year.
		      See POSIX.2 section 4.63.3.  */
		   if (zzvsp[0] <= 68)
		     t.tm_year += 100;
		 ;
    break;}
case 3:
#line 92 "posixtm.y"
{
                            t.tm_year = zzvsp[-1] * 100 + zzvsp[0];
			    if (t.tm_year < 1900) {
			      YYABORT;
			    } else
			      t.tm_year -= 1900;
			  ;
    break;}
case 4:
#line 99 "posixtm.y"
{
                    time_t now;
		    struct tm *tmp;

                    /* Use current year.  */
                    time (&now);
		    tmp = localtime (&now);
		    t.tm_year = tmp->tm_year;
		  ;
    break;}
case 5:
#line 110 "posixtm.y"
{
                        t.tm_sec = 0;
		      ;
    break;}
case 6:
#line 113 "posixtm.y"
{
	                  if (zzvsp[0] >= 0 && zzvsp[0] <= 61)
			    t.tm_sec = zzvsp[0];
			  else {
			    YYABORT;
			  }
			;
    break;}
case 7:
#line 122 "posixtm.y"
{
                          zzval = zzvsp[-1] * 10 + zzvsp[0];
			;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 423 "bison.simple"

  zzvsp -= zzlen;
  zzssp -= zzlen;
#ifdef YYLSP_NEEDED
  zzlsp -= zzlen;
#endif

#if YYDEBUG != 0
  if (zzdebug)
    {
      short *ssp1 = zzss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != zzssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++zzvsp = zzval;

#ifdef YYLSP_NEEDED
  zzlsp++;
  if (zzlen == 0)
    {
      zzlsp->first_line = zzlloc.first_line;
      zzlsp->first_column = zzlloc.first_column;
      zzlsp->last_line = (zzlsp-1)->last_line;
      zzlsp->last_column = (zzlsp-1)->last_column;
      zzlsp->text = 0;
    }
  else
    {
      zzlsp->last_line = (zzlsp+zzlen-1)->last_line;
      zzlsp->last_column = (zzlsp+zzlen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  zzn = zzr1[zzn];

  zzstate = zzpgoto[zzn - YYNTBASE] + *zzssp;
  if (zzstate >= 0 && zzstate <= YYLAST && zzcheck[zzstate] == *zzssp)
    zzstate = zztable[zzstate];
  else
    zzstate = zzdefgoto[zzn - YYNTBASE];

  goto zznewstate;

zzerrlab:   /* here on detecting error */

  if (! zzerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++zznerrs;

#ifdef YYERROR_VERBOSE
      zzn = zzpact[zzstate];

      if (zzn > YYFLAG && zzn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  for (x = 0; x < (sizeof(zztname) / sizeof(char *)); x++)
	    if (zzcheck[x + zzn] == x)
	      size += strlen(zztname[x]) + 15, count++;
	  msg = (char *) xmalloc(size + 15);
	  strcpy(msg, "parse error");

	  if (count < 5)
	    {
	      count = 0;
	      for (x = 0; x < (sizeof(zztname) / sizeof(char *)); x++)
		if (zzcheck[x + zzn] == x)
		  {
		    strcat(msg, count == 0 ? ", expecting `" : " or `");
		    strcat(msg, zztname[x]);
		    strcat(msg, "'");
		    count++;
		  }
	    }
	  zzerror(msg);
	  free(msg);
	}
      else
#endif /* YYERROR_VERBOSE */
	zzerror("parse error");
    }

zzerrlab1:   /* here on error raised explicitly by an action */

  if (zzerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (zzchar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (zzdebug)
	fprintf(stderr, "Discarding token %d (%s).\n", zzchar, zztname[zzchar1]);
#endif

      zzchar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  zzerrstatus = 3;		/* Each real token shifted decrements this */

  goto zzerrhandle;

zzerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  zzn = zzdefact[zzstate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (zzn) goto zzdefault;
#endif

zzerrpop:   /* pop the current state because it cannot handle the error token */

  if (zzssp == zzss) YYABORT;
  zzvsp--;
  zzstate = *--zzssp;
#ifdef YYLSP_NEEDED
  zzlsp--;
#endif

#if YYDEBUG != 0
  if (zzdebug)
    {
      short *ssp1 = zzss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != zzssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

zzerrhandle:

  zzn = zzpact[zzstate];
  if (zzn == YYFLAG)
    goto zzerrdefault;

  zzn += YYTERROR;
  if (zzn < 0 || zzn > YYLAST || zzcheck[zzn] != YYTERROR)
    goto zzerrdefault;

  zzn = zztable[zzn];
  if (zzn < 0)
    {
      if (zzn == YYFLAG)
	goto zzerrpop;
      zzn = -zzn;
      goto zzreduce;
    }
  else if (zzn == 0)
    goto zzerrpop;

  if (zzn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (zzdebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++zzvsp = zzlval;
#ifdef YYLSP_NEEDED
  *++zzlsp = zzlloc;
#endif

  zzstate = zzn;
  goto zznewstate;
}
#line 126 "posixtm.y"

static int
zzlex ()
{
  char ch = *curpos++;

  if (ch >= '0' && ch <= '9')
    {
      zzlval = ch - '0';
      return DIGIT;
    }
  else if (ch == '.' || ch == 0)
    return ch;
  else
    return '?';			/* Cause an error.  */
}

static int
zzerror ()
{
  return 0;
}

/* Parse a POSIX-style date and return it, or (time_t)-1 for an error.  */

time_t
posixtime (s)
     char *s;
{
  curpos = s;
  /* Let mktime decide whether it is daylight savings time.  */
  t.tm_isdst = -1;
  if (zzparse ())
    return (time_t)-1;
  else
    return mktime (&t);
}

/* Parse a POSIX-style date and return it, or NULL for an error.  */

struct tm *
posixtm (s)
     char *s;
{
  if (posixtime (s) == -1)
    return NULL;
  return &t;
}
