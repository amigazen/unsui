
/*  A Bison parser, made from web2c.yacc  */

#define	array_tok	258
#define	begin_tok	259
#define	case_tok	260
#define	const_tok	261
#define	do_tok	262
#define	downto_tok	263
#define	else_tok	264
#define	end_tok	265
#define	file_tok	266
#define	for_tok	267
#define	function_tok	268
#define	goto_tok	269
#define	if_tok	270
#define	label_tok	271
#define	of_tok	272
#define	procedure_tok	273
#define	program_tok	274
#define	record_tok	275
#define	repeat_tok	276
#define	then_tok	277
#define	to_tok	278
#define	type_tok	279
#define	until_tok	280
#define	var_tok	281
#define	while_tok	282
#define	integer_tok	283
#define	real_tok	284
#define	others_tok	285
#define	r_num_tok	286
#define	i_num_tok	287
#define	string_literal_tok	288
#define	single_char_tok	289
#define	assign_tok	290
#define	two_dots_tok	291
#define	unknown_tok	292
#define	undef_id_tok	293
#define	var_id_tok	294
#define	proc_id_tok	295
#define	proc_param_tok	296
#define	fun_id_tok	297
#define	fun_param_tok	298
#define	const_id_tok	299
#define	type_id_tok	300
#define	hhb0_tok	301
#define	hhb1_tok	302
#define	field_id_tok	303
#define	define_tok	304
#define	field_tok	305
#define	break_tok	306
#define	not_eq_tok	307
#define	less_eq_tok	308
#define	great_eq_tok	309
#define	or_tok	310
#define	unary_plus_tok	311
#define	unary_minus_tok	312
#define	div_tok	313
#define	mod_tok	314
#define	and_tok	315
#define	not_tok	316

#line 17 "web2c.yacc"

#include "web2c.h"

#define	symbol(x)	sym_table[x].id
#define	MAX_ARGS	50

static char function_return_type[50], for_stack[300], control_var[50],
            relation[3];
static char arg_type[MAX_ARGS][30];
static int last_type = -1, ids_typed;
char my_routine[100];	/* Name of routine being parsed, if any */
static char array_bounds[80], array_offset[80];
static int uses_mem, uses_eqtb, lower_sym, upper_sym;
static FILE *orig_std;
boolean doing_statements = FALSE;
static boolean var_formals = FALSE;
static int param_id_list[MAX_ARGS], ids_paramed=0;

extern char conditional[], temp[], *std_header;
extern int tex, mf, strict_for;
extern boolean ansi;
extern FILE *coerce;
extern char coerce_name[];
extern boolean debug;

/* Forward refs */
#ifdef	ANSI
static long labs(long x);
static void compute_array_bounds(void);
static void fixup_var_list(void);
static void do_proc_args(void);
static void gen_function_head(void);
static boolean doreturn(char *label);
extern int yylex(void);
#else	/* Not ANSI */
static long labs();
static void compute_array_bounds(), fixup_var_list();
static void do_proc_args(), gen_function_head();
static boolean doreturn();
#endif	/* Not ANSI */


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

#define	YYACCEPT	return(0)
#define	YYABORT	return(1)
#define	YYERROR	goto yyerrlab
#ifndef YYSTYPE
#define YYSTYPE int
#endif
#ifndef stdin
#include <stdio.h>
#endif

#ifndef __STDC__
#define const
#endif



#define	YYFINAL		417
#define	YYFLAG		-32768
#define	YYNTBASE	78

#define YYTRANSLATE(x) ((unsigned)(x) <= 316 ? yytranslate[x] : 242)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    70,
    71,    63,    58,    72,    59,    77,    64,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    76,    69,    54,
    52,    55,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    74,     2,    75,    73,     2,     2,     2,     2,     2,     2,
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
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    49,    50,    51,    53,    56,    57,    60,
    61,    62,    65,    66,    67,    68
};

static const short yyrline[] = {     0,
    64,    68,    72,    76,    77,    80,    85,    90,    95,   100,
   105,   110,   115,   125,   132,   135,   140,   156,   161,   192,
   193,   195,   199,   200,   203,   207,   208,   212,   213,   216,
   219,   223,   228,   236,   240,   242,   246,   260,   278,   282,
   283,   286,   287,   290,   293,   299,   303,   315,   316,   319,
   347,   350,   353,   354,   357,   363,   371,   382,   385,   386,
   389,   394,   405,   406,   410,   412,   421,   425,   427,   431,
   432,   435,   437,   443,   456,   459,   460,   463,   475,   487,
   490,   493,   494,   497,   498,   501,   508,   513,   517,   518,
   521,   535,   549,   565,   566,   571,   575,   576,   579,   581,
   585,   588,   600,   604,   612,   618,   623,   633,   636,   637,
   640,   641,   654,   655,   655,   656,   658,   659,   662,   665,
   676,   682,   687,   687,   697,   703,   708,   710,   711,   714,
   717,   720,   722,   726,   727,   730,   731,   735,   744,   746,
   750,   751,   752,   753,   754,   758,   761,   761,   764,   766,
   777,   777,   783,   785,   789,   790,   793,   795,   797,   815,
   817,   821,   822,   824,   827,   829,   830,   831,   832,   833,
   834,   835,   836,   837,   838,   839,   840,   841,   842,   843,
   844,   845,   846,   847,   848,   849,   850,   851,   852,   853,
   854,   855,   858,   859,   863,   864,   866,   870,   872,   874,
   875,   876,   878,   881,   883,   885,   889,   890,   893,   895,
   899,   900,   903,   905,   911,   914,   916,   930,   933,   934,
   935,   938,   939,   942,   943,   946,   949,   950,   953,   956,
   958,   960,   964,   968,   969,   972,   976,   977,   980,   985,
   989,   990,   993,   994,   995,   998,  1003,  1004,  1007,  1009,
  1014,  1017,  1026,  1032,  1049,  1065,  1069,  1072,  1077,  1081,
  1084,  1089
};

static const char * const yytname[] = {     0,
"error","$illegal.","array_tok","begin_tok","case_tok","const_tok","do_tok","downto_tok","else_tok","end_tok",
"file_tok","for_tok","function_tok","goto_tok","if_tok","label_tok","of_tok","procedure_tok","program_tok","record_tok",
"repeat_tok","then_tok","to_tok","type_tok","until_tok","var_tok","while_tok","integer_tok","real_tok","others_tok",
"r_num_tok","i_num_tok","string_literal_tok","single_char_tok","assign_tok","two_dots_tok","unknown_tok","undef_id_tok","var_id_tok","proc_id_tok",
"proc_param_tok","fun_id_tok","fun_param_tok","const_id_tok","type_id_tok","hhb0_tok","hhb1_tok","field_id_tok","define_tok","field_tok",
"break_tok","'='","not_eq_tok","'<'","'>'","less_eq_tok","great_eq_tok","'+'","'-'","or_tok",
"unary_plus_tok","unary_minus_tok","'*'","'/'","div_tok","mod_tok","and_tok","not_tok","';'","'('",
"')'","','","'^'","'['","']'","':'","'.'","PROGRAM"
};

static const short yyr1[] = {     0,
    79,    80,    78,    81,    81,    82,    82,    82,    82,    82,
    82,    82,    82,    82,    83,    85,    86,    87,    84,    88,
    89,    88,    90,    90,    91,    92,    92,    93,    93,    95,
    96,    94,    97,    97,    97,    97,    98,    98,    99,   100,
   100,   101,   101,   103,   104,   105,   102,   106,   106,   107,
   107,   108,   109,   109,   110,   110,   111,   112,   112,   112,
   112,   113,   114,   114,   115,   115,   116,   118,   117,   119,
   119,   121,   122,   120,   120,   123,   123,   124,   124,   126,
   125,   127,   127,   128,   128,   130,   131,   129,   132,   132,
   133,   133,   133,   134,   135,   134,   136,   136,   137,   137,
   138,   140,   139,   141,   139,   142,   143,   142,   144,   144,
   146,   145,   148,   147,   149,   147,   150,   150,   151,   153,
   154,   155,   152,   156,   157,   158,   152,   159,   159,   160,
   161,   163,   162,   164,   164,   165,   165,   166,   167,   167,
   168,   168,   168,   168,   168,   170,   169,   171,   169,   173,
   172,   172,   174,   174,   175,   175,   177,   176,   176,   176,
   176,   178,   179,   178,   180,   181,   180,   182,   180,   183,
   180,   184,   180,   185,   180,   186,   180,   187,   180,   188,
   180,   189,   180,   190,   180,   191,   180,   192,   180,   193,
   180,   194,   180,   180,   195,   195,   195,   197,   196,   196,
   196,   196,   198,   196,   200,   199,   201,   202,   201,   203,
   204,   204,   205,   205,   206,   205,   207,   208,   209,   209,
   209,   210,   210,   211,   211,   213,   214,   212,   216,   215,
   218,   219,   217,   220,   220,   221,   222,   222,   223,   223,
   224,   224,   225,   225,   225,   227,   228,   226,   230,   231,
   229,   233,   234,   235,   232,   236,   238,   239,   237,   240,
   241,   237
};

static const short yyr2[] = {     0,
     0,     0,    10,     0,     2,     4,     4,     4,     6,     4,
     6,     4,     6,     4,     3,     0,     0,     0,     8,     0,
     0,     4,     1,     3,     1,     0,     2,     1,     2,     0,
     0,     6,     1,     1,     1,     1,     1,     1,     1,     0,
     2,     1,     2,     0,     0,     0,     7,     1,     1,     1,
     1,     3,     0,     1,     2,     1,     1,     1,     1,     1,
     1,     2,     6,     8,     1,     1,     1,     0,     4,     1,
     3,     0,     0,     5,     0,     1,     3,     1,     1,     0,
     4,     0,     2,     1,     2,     0,     0,     6,     1,     3,
     1,     1,     1,     0,     0,     5,     1,     2,     2,     2,
     2,     0,     5,     0,     5,     0,     0,     4,     1,     3,
     0,     4,     0,     2,     0,     3,     1,     1,     2,     0,
     0,     0,     9,     0,     0,     0,     9,     1,     1,     1,
     3,     0,     4,     1,     3,     1,     3,     1,     1,     1,
     1,     1,     1,     1,     1,     0,     4,     0,     4,     0,
     3,     1,     1,     1,     1,     2,     0,     4,     2,     2,
     2,     1,     0,     4,     2,     0,     4,     0,     4,     0,
     4,     0,     4,     0,     4,     0,     4,     0,     4,     0,
     4,     0,     4,     0,     4,     0,     4,     0,     4,     0,
     4,     0,     4,     1,     1,     1,     1,     0,     4,     1,
     1,     1,     0,     3,     0,     4,     1,     0,     4,     2,
     2,     0,     1,     1,     0,     3,     2,     0,     1,     1,
     1,     1,     1,     1,     2,     0,     0,     6,     0,     3,
     0,     0,     7,     1,     3,     3,     1,     3,     1,     1,
     1,     2,     1,     1,     1,     0,     0,     6,     0,     0,
     6,     0,     0,     0,     9,     1,     0,     0,     5,     0,
     0,     5
};

static const short yydefact[] = {     4,
     0,     0,     0,     5,     1,     0,     0,     0,     0,     0,
     0,     0,    20,    15,     0,     0,     0,     0,     0,     0,
    21,    26,     8,     7,     0,    10,     0,    53,    12,    14,
     6,     0,    30,    40,     0,     0,    56,    54,     0,     0,
     0,    25,     0,    23,    27,    28,     0,    44,    82,     9,
    11,    13,    55,    53,    22,     0,    29,    31,    41,    42,
     0,    86,     2,    52,    24,     0,    43,    45,    83,    84,
     0,     0,     0,     0,    85,    91,    92,    93,     0,    89,
     0,     0,    94,    97,     0,    16,     0,    16,    34,    33,
    37,    38,    39,     0,    35,    36,    46,     0,    87,   120,
   128,   129,   124,   102,   117,   118,   104,    95,     3,    98,
    99,   101,    20,   100,   119,    32,    53,    90,    53,   106,
   106,   106,   106,   218,    26,     0,     0,    68,    57,     0,
     0,    48,    50,    51,    49,    61,    58,    59,    60,     0,
   107,     0,     0,     0,     0,   132,   231,   252,     0,   226,
   249,   246,   138,   214,   152,   213,   215,   153,   154,   145,
   219,     0,   134,     0,   136,   139,   141,     0,     0,   142,
   143,   144,   140,   220,   222,   224,   223,   221,   243,   244,
   245,    40,    53,    80,    72,    62,    47,    88,   113,   121,
   125,   103,   105,   218,     0,     0,   217,     0,   218,     0,
     0,     0,     0,   218,   218,   146,   148,   229,   225,    17,
    66,    65,     0,    53,     0,    70,     0,   115,     0,   109,
   111,    53,    53,     0,   202,   203,   195,   196,   197,   198,
   201,   200,     0,     0,   194,   256,     0,   227,     0,   247,
   157,     0,   151,   155,   205,   216,    96,   135,   137,     0,
     0,   218,    82,    53,     0,    81,    69,    72,    78,    79,
     0,    76,   111,   113,   108,   114,     0,   130,   122,   126,
   133,     0,     0,   232,   174,   176,   180,   182,   184,   186,
   166,   168,   190,   170,   192,   172,   178,   188,   165,   253,
     0,   250,     0,     0,   160,   161,   159,   156,     0,   147,
   149,   230,    18,     0,    53,    71,     0,    73,   116,   110,
     0,     0,     0,   204,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   218,     0,   218,     0,   212,     0,   207,     0,     0,
    67,    63,    77,    53,     0,   123,   127,   199,   240,   239,
     0,   234,     0,   237,   175,   177,   181,   183,   185,   187,
   167,   169,   191,   171,   193,   173,   179,   189,   257,     0,
   228,   251,   248,   163,   162,   158,     0,   210,   206,   208,
   218,    19,    53,    74,   112,   241,     0,   233,     0,   218,
     0,     0,   254,     0,   211,     0,     0,    64,   242,   235,
   238,   236,   258,   261,   218,     0,   209,   131,     0,     0,
   255,   164,   259,   262,     0,     0,     0
};

static const short yydefgoto[] = {   415,
    13,    72,     1,     4,     5,   112,   113,   253,   339,    22,
    32,    43,    44,    34,    45,    46,    47,    66,   231,    95,
    96,    49,    59,    60,    61,    74,   117,   268,   132,   133,
    40,    41,   134,   135,   136,   137,   213,   342,   138,   185,
   215,   216,   217,   344,   261,   262,   139,   214,    63,    69,
    70,    71,   119,    79,    80,   109,   124,    83,    84,    85,
    86,   122,   123,   142,   189,   219,   266,   267,   220,   221,
   263,   107,    87,    88,   120,   222,   312,   121,   223,   313,
   103,   269,   382,   161,   194,   162,   163,   164,   165,   166,
   167,   250,   251,   232,   201,   169,   243,   244,   294,   376,
   394,   336,   323,   324,   326,   328,   317,   318,   329,   319,
   320,   321,   322,   330,   325,   327,   234,   235,   273,   272,
   246,   299,   337,   396,   338,   378,   170,   202,   171,   172,
   173,   174,   175,   176,   198,   291,   209,   252,   177,   195,
   316,   351,   352,   353,   354,   388,   178,   179,   200,   293,
   180,   199,   333,   181,   196,   331,   405,   237,   370,   391,
   409,   392,   410
};

static const short yypact[] = {-32768,
    -8,    18,    19,-32768,-32768,    15,    41,    59,   125,   132,
   144,   150,   181,-32768,   129,    35,    47,   -21,   130,   131,
-32768,   198,-32768,-32768,   134,-32768,   136,   -11,-32768,-32768,
-32768,   182,-32768,   189,   146,   147,-32768,-32768,   148,   188,
   187,-32768,    75,-32768,   191,-32768,   193,-32768,   199,-32768,
-32768,-32768,-32768,   -11,-32768,   182,-32768,-32768,   194,-32768,
   195,-32768,-32768,-32768,-32768,   174,-32768,-32768,    13,-32768,
    20,    22,   118,   175,-32768,-32768,-32768,-32768,   -48,-32768,
    44,   120,    16,-32768,   165,-32768,   176,-32768,-32768,-32768,
-32768,-32768,-32768,   177,-32768,-32768,-32768,    20,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   181,-32768,-32768,-32768,    10,-32768,    10,   180,
   180,   180,   180,   152,   198,   173,   234,-32768,-32768,   211,
   186,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   190,
-32768,   184,   185,   196,   200,-32768,-32768,-32768,   226,-32768,
-32768,-32768,-32768,-32768,    91,-32768,-32768,-32768,-32768,-32768,
-32768,    -5,-32768,   192,-32768,-32768,-32768,   227,   228,-32768,
-32768,-32768,-32768,-32768,-32768,   261,-32768,-32768,-32768,-32768,
-32768,   189,   -22,-32768,    -4,-32768,-32768,-32768,   238,-32768,
-32768,-32768,-32768,   152,   210,   235,-32768,   210,   152,   210,
    95,   203,   202,   152,   197,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    99,    10,    -2,-32768,   -12,-32768,   109,-32768,
-32768,    10,    10,     5,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    55,   210,-32768,-32768,   240,   323,    -9,   323,
-32768,   107,    95,-32768,-32768,-32768,-32768,-32768,-32768,   210,
   210,   152,   199,   -22,   259,-32768,-32768,    -4,-32768,-32768,
   -34,-32768,-32768,   238,-32768,-32768,    20,-32768,-32768,-32768,
-32768,   203,   210,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   269,-32768,   270,   210,-32768,-32768,-32768,-32768,   210,   323,
   323,-32768,-32768,   217,    10,-32768,   -12,-32768,-32768,-32768,
   -19,   229,   231,-32768,   303,   151,   210,   210,   210,   210,
   210,   210,   210,   210,   210,   210,   210,   210,   210,   210,
   210,   152,   210,   152,   255,   230,   114,-32768,   295,   284,
-32768,-32768,-32768,    10,   257,-32768,-32768,-32768,-32768,-32768,
     7,-32768,     1,-32768,   339,   339,   339,   339,   339,   339,
    60,    60,    60,-32768,-32768,-32768,-32768,-32768,    36,   296,
-32768,   323,-32768,-32768,-32768,-32768,   272,-32768,-32768,-32768,
   152,-32768,    10,-32768,-32768,-32768,    17,-32768,   151,   197,
   282,   308,-32768,   210,-32768,   210,     9,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   152,   279,-32768,-32768,   210,   210,
-32768,-32768,   323,   323,   317,   324,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,-32768,   237,-32768,-32768,-32768,   213,
-32768,-32768,   267,   204,-32768,   283,-32768,-32768,   268,-32768,
-32768,   158,-32768,   288,-32768,-32768,-32768,  -116,-32768,   -24,
-32768,   294,-32768,-32768,-32768,-32768,    96,   -32,-32768,-32768,
-32768,    94,-32768,-32768,-32768,    42,-32768,-32768,   100,-32768,
   302,-32768,-32768,    97,   274,-32768,-32768,-32768,   290,-32768,
-32768,-32768,-32768,    54,-32768,-32768,   102,-32768,   121,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   161,-32768,-32768,-32768,  -185,  -186,-32768,  -203,-32768,
-32768,-32768,-32768,  -124,-32768,-32768,-32768,   157,-32768,-32768,
-32768,  -188,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   135,-32768,-32768,-32768,    12,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    14,-32768,    21,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768
};


#define	YYLAST		410


static const short yytable[] = {   168,
   131,   249,   140,    39,   203,   -75,   233,   257,   224,   238,
     2,   240,   126,   239,   271,   292,   386,   248,   408,   108,
   127,    37,   211,    98,     7,   259,   399,    99,    81,   128,
    28,     8,    37,    82,    81,   260,     9,   307,    38,    82,
     3,   308,    10,  -260,    11,   289,   349,    29,   350,    38,
   -86,   -86,    98,    37,   129,     6,   345,    76,    77,   204,
   -86,   300,   301,   204,   -75,   302,   258,    78,    12,   168,
    38,   274,   389,   204,   168,   387,   390,   204,    15,   168,
   168,   100,   130,    14,   315,   101,   102,   275,   276,   277,
   278,   279,   280,   281,   282,   283,    16,   256,   284,   285,
   286,   287,   288,    24,    25,   335,   275,   276,   277,   278,
   279,   280,   281,   282,   283,    26,    27,   284,   285,   286,
   287,   288,   284,   285,   286,   287,   288,   168,   355,   356,
   357,   358,   359,   360,   361,   362,   363,   364,   365,   366,
   367,   368,   369,    55,   372,   371,    56,   373,    89,    90,
    91,    92,   295,   296,   297,   146,   147,   104,   212,   105,
   106,    93,    17,   148,  -150,   149,   150,  -150,   241,    18,
   254,   242,   151,   255,   143,   144,   145,   264,   152,   265,
   349,    19,   350,   153,   379,   380,   402,    20,   341,   154,
   155,   156,   157,   158,   159,   397,    21,    23,    30,    31,
   146,   147,   160,    33,    35,   406,    36,   168,   148,   168,
   149,   150,    48,    42,    50,    51,    52,   151,   411,    53,
   413,   414,    54,   152,    62,    73,    97,   384,   -30,   212,
    58,   -44,    68,   111,   154,   155,   156,   157,   158,   159,
    89,    90,    91,    92,   114,   116,   183,   160,   155,   141,
   184,   225,   226,    93,   187,   186,   168,   197,   188,   190,
   191,   206,   207,   218,   192,   168,   341,   205,   193,   208,
   227,   228,   245,   236,   290,   305,   334,   229,   247,   230,
   168,   275,   276,   277,   278,   279,   280,   281,   282,   283,
   332,   340,   284,   285,   286,   287,   288,   346,   381,   347,
   383,   385,   393,   395,   403,   377,   275,   276,   277,   278,
   279,   280,   281,   282,   283,   404,   416,   284,   285,   286,
   287,   288,    65,   417,   115,   125,   374,    57,   182,   375,
   275,   276,   277,   278,   279,   280,   281,   282,   283,   210,
    94,   284,   285,   286,   287,   288,    67,    64,   343,   304,
   398,   306,   303,   412,   275,   276,   277,   278,   279,   280,
   281,   282,   283,   311,   309,   284,   285,   286,   287,   288,
    75,   118,   110,   348,   275,   276,   277,   278,   279,   280,
   281,   282,   283,   270,   310,   284,   285,   286,   287,   288,
-32768,-32768,-32768,-32768,-32768,-32768,   281,   282,   283,   298,
   400,   284,   285,   286,   287,   288,   314,   407,     0,   401
};

static const short yycheck[] = {   124,
   117,   205,   119,    28,    10,    10,   195,    10,   194,   198,
    19,   200,     3,   199,    10,    25,    10,   204,    10,     4,
    11,    44,    45,    72,     6,    38,    10,    76,    13,    20,
    52,    13,    44,    18,    13,    48,    18,    72,    61,    18,
    49,    76,    24,     8,    26,   234,    30,    69,    32,    61,
    38,    39,    72,    44,    45,    38,    76,    38,    39,    69,
    48,   250,   251,    69,    69,   252,    69,    48,    50,   194,
    61,    17,    72,    69,   199,    69,    76,    69,    38,   204,
   205,    38,    73,    69,   273,    42,    43,    52,    53,    54,
    55,    56,    57,    58,    59,    60,    38,   214,    63,    64,
    65,    66,    67,    69,    70,   294,    52,    53,    54,    55,
    56,    57,    58,    59,    60,    69,    70,    63,    64,    65,
    66,    67,    63,    64,    65,    66,    67,   252,   317,   318,
   319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
   329,   330,   331,    69,   333,   332,    72,   334,    31,    32,
    33,    34,    46,    47,    48,     4,     5,    38,   183,    40,
    41,    44,    38,    12,    74,    14,    15,    77,    74,    38,
    72,    77,    21,    75,   121,   122,   123,    69,    27,    71,
    30,    38,    32,    32,    71,    72,   390,    38,   305,    38,
    39,    40,    41,    42,    43,   381,    16,    69,    69,    69,
     4,     5,    51,     6,    71,   394,    71,   332,    12,   334,
    14,    15,    24,    32,    69,    69,    69,    21,   405,    32,
   409,   410,    36,    27,    26,    52,    52,   344,    38,   254,
    38,    38,    38,    69,    38,    39,    40,    41,    42,    43,
    31,    32,    33,    34,    69,    69,    74,    51,    39,    70,
    17,    42,    43,    44,    69,    45,   381,    32,    69,    76,
    76,    35,    35,    26,    69,   390,   383,    76,    69,     9,
    61,    62,    70,    39,    35,    17,     7,    68,    77,    70,
   405,    52,    53,    54,    55,    56,    57,    58,    59,    60,
    22,    75,    63,    64,    65,    66,    67,    69,     4,    69,
    17,    45,     7,    32,    23,    76,    52,    53,    54,    55,
    56,    57,    58,    59,    60,     8,     0,    63,    64,    65,
    66,    67,    56,     0,    88,   113,    72,    45,   125,    75,
    52,    53,    54,    55,    56,    57,    58,    59,    60,   182,
    73,    63,    64,    65,    66,    67,    59,    54,   307,   254,
   383,   258,   253,    75,    52,    53,    54,    55,    56,    57,
    58,    59,    60,   267,   263,    63,    64,    65,    66,    67,
    69,    98,    83,    71,    52,    53,    54,    55,    56,    57,
    58,    59,    60,   223,   264,    63,    64,    65,    66,    67,
    52,    53,    54,    55,    56,    57,    58,    59,    60,   243,
   387,    63,    64,    65,    66,    67,   272,   396,    -1,   389
};
#define YYPURE 1

#line 2 "bison.simple"

/* Skeleton output parser for bison,
   copyright (C) 1984 Bob Corbett and Richard Stallman

                       NO WARRANTY

  BECAUSE THIS PROGRAM IS LICENSED FREE OF CHARGE, WE PROVIDE ABSOLUTELY
NO WARRANTY, TO THE EXTENT PERMITTED BY APPLICABLE STATE LAW.  EXCEPT
WHEN OTHERWISE STATED IN WRITING, FREE SOFTWARE FOUNDATION, INC,
RICHARD M. STALLMAN AND/OR OTHER PARTIES PROVIDE THIS PROGRAM "AS IS"
WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY
AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
CORRECTION.

 IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW WILL RICHARD M.
STALLMAN, THE FREE SOFTWARE FOUNDATION, INC., AND/OR ANY OTHER PARTY
WHO MAY MODIFY AND REDISTRIBUTE THIS PROGRAM AS PERMITTED BELOW, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY LOST PROFITS, LOST MONIES, OR
OTHER SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR
DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY THIRD PARTIES OR
A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS) THIS
PROGRAM, EVEN IF YOU HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES, OR FOR ANY CLAIM BY ANY OTHER PARTY.

                GENERAL PUBLIC LICENSE TO COPY

  1. You may copy and distribute verbatim copies of this source file
as you receive it, in any medium, provided that you conspicuously and
appropriately publish on each copy a valid copyright notice "Copyright
(C) 1985 Free Software Foundation, Inc."; and include following the
copyright notice a verbatim copy of the above disclaimer of warranty
and of this License.  You may charge a distribution fee for the
physical act of transferring a copy.

  2. You may modify your copy or copies of this source file or
any portion of it, and copy and distribute such modifications under
the terms of Paragraph 1 above, provided that you also do the following:

    a) cause the modified files to carry prominent notices stating
    that you changed the files and the date of any change; and

    b) cause the whole of any work that you distribute or publish,
    that in whole or in part contains or is a derivative of this
    program or any part thereof, to be licensed at no charge to all
    third parties on terms identical to those contained in this
    License Agreement (except that you may choose to grant more extensive
    warranty protection to some or all third parties, at your option).

    c) You may charge a distribution fee for the physical act of
    transferring a copy, and you may at your option offer warranty
    protection in exchange for a fee.

Mere aggregation of another unrelated program with this program (or its
derivative) on a volume of a storage or distribution medium does not bring
the other program under the scope of these terms.

  3. You may copy and distribute this program (or a portion or derivative
of it, under Paragraph 2) in object code or executable form under the terms
of Paragraphs 1 and 2 above provided that you also do one of the following:

    a) accompany it with the complete corresponding machine-readable
    source code, which must be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    b) accompany it with a written offer, valid for at least three
    years, to give any third party free (except for a nominal
    shipping charge) a complete machine-readable copy of the
    corresponding source code, to be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    c) accompany it with the information you received as to where the
    corresponding source code may be obtained.  (This alternative is
    allowed only for noncommercial distribution and only if you
    received the program in object code or executable form alone.)

For an executable file, complete source code means all the source code for
all modules it contains; but, as a special exception, it need not include
source code for modules which are standard libraries that accompany the
operating system on which the executable file runs.

  4. You may not copy, sublicense, distribute or transfer this program
except as expressly provided under this License Agreement.  Any attempt
otherwise to copy, sublicense, distribute or transfer this program is void and
your rights to use the program under this License agreement shall be
automatically terminated.  However, parties who have received computer
software programs from you with this License Agreement will not have
their licenses terminated so long as such parties remain in full compliance.

  5. If you wish to incorporate parts of this program into other free
programs whose distribution conditions are different, write to the Free
Software Foundation at 675 Mass Ave, Cambridge, MA 02139.  We have not yet
worked out a simple rule that can be stated here, but we will often permit
this.  We will be guided by the two goals of preserving the free status of
all derivatives of our free software and of promoting the sharing and reuse of
software.


In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#ifdef AMIGA

#define bzero(b, length) memset((b),'\0',(length))
#define bcopy(b1, b2, length) memcpy((b1),(b2),(length))
#define bcmp(b1, b2, length)  memcmp((b1),(b2),(length))

void
memory_full ()
{
  printf ("Memory exhausted.\n");
  exit(1);
}


char *
xmalloc (size)
     int size;
{
  extern char *malloc ();
  register char *ptr = malloc (size);
  if (ptr != 0) return (ptr);
  memory_full ();
  /*NOTREACHED*/
}

char *
xrealloc (old, size)
     char *old;
     int size;
{
  extern char *realloc ();
  register char *ptr = realloc (old, size);
  if (ptr != 0) return (ptr);
  memory_full ();
  /*NOTREACHED*/
}

char *
xcalloc (number, size)
     int number, size;
{
  extern char *malloc ();
  register int total = number * size;
  register char *ptr = malloc (total);
  if (ptr != 0)
    {
      if (total > 100)
        bzero (ptr, total);
      else {
        /* It's not too long, so loop, zeroing by longs.
           It must be safe because malloc values are always well aligned.  */
        register long *zp = (long *) ptr;
        register long *zl = (long *) (ptr + total - 4);
        register int i = total - 4;
        while (zp < zl)
          *zp++ = 0;
        if (i < 0)
          i = 0;
        while (i < total)
          ptr[i++] = 0;
      }
      return ptr;
    }
  memory_full ();
  /*NOTREACHED*/
}

/*
        alloca -- (mostly) portable public-domain implementation -- D A Gwyn

        last edit:      86/05/30        rms
           include config.h, since on VMS it renames some symbols.
           Use xmalloc instead of malloc.

        This implementation of the PWB library alloca() function,
        which is used to allocate space off the run-time stack so
        that it is automatically reclaimed upon procedure exit,
        was inspired by discussions with J. Q. Johnson of Cornell.

        It should work under any C implementation that uses an
        actual procedure stack (as opposed to a linked list of
        frames).  There are some preprocessor constants that can
        be defined when compiling for your specific system, for
        improved efficiency; however, the defaults should be okay.

        The general concept of this implementation is to keep
        track of all alloca()-allocated blocks, and reclaim any
        that are found to be deeper in the stack than the current
        invocation.  This heuristic does not reclaim storage as
        soon as it becomes invalid, but it will do so eventually.

        As a special case, alloca(0) reclaims storage without
        allocating any.  It is a good idea to use alloca(0) in
        your main control loop, etc. to force garbage collection.
*/

typedef char    *pointer;               /* generic pointer type */

extern void     free();
extern pointer  xmalloc();

/*
        Define STACK_DIRECTION if you know the direction of stack
        growth for your system; otherwise it will be automatically
        deduced at run-time.

        STACK_DIRECTION > 0 => grows toward higher addresses
        STACK_DIRECTION < 0 => grows toward lower addresses
        STACK_DIRECTION = 0 => direction of growth unknown
*/

#define STACK_DIRECTION -1

#define STACK_DIR       STACK_DIRECTION /* known at compile-time */

/*
        An "alloca header" is used to:
        (a) chain together all alloca()ed blocks;
        (b) keep track of stack depth.

        It is very important that sizeof(header) agree with malloc()
        alignment chunk size.  The following default should work okay.
*/

#ifndef ALIGN_SIZE
#define ALIGN_SIZE      sizeof(double)
#endif

typedef union hdr
{
  char  align[ALIGN_SIZE];      /* to force sizeof(header) */
  struct
    {
      union hdr *next;          /* for chaining headers */
      char *deep;               /* for stack depth measure */
    } h;
} header;

/*
        alloca( size ) returns a pointer to at least `size' bytes of
        storage which will be automatically reclaimed upon exit from
        the procedure that called alloca().  Originally, this space
        was supposed to be taken from the current stack frame of the
        caller, but that method cannot be made to work for some
        implementations of C, for example under Gould's UTX/32.
*/

static header *last_alloca_header = NULL; /* -> last alloca header */

pointer
alloca (size)                   /* returns pointer to storage */
     unsigned   size;           /* # bytes to allocate */
{
  auto char     probe;          /* probes stack depth: */
  register char *depth = &probe;

                                /* Reclaim garbage, defined as all alloca()ed storage that
                                   was allocated from deeper in the stack than currently. */

  {
    register header     *hp;    /* traverses linked list */

    for (hp = last_alloca_header; hp != NULL;)
      if (STACK_DIR > 0 && hp->h.deep > depth
          || STACK_DIR < 0 && hp->h.deep < depth)
        {
          register header       *np = hp->h.next;

          free ((pointer) hp);  /* collect garbage */

          hp = np;              /* -> next header */
        }
      else
        break;                  /* rest are not deeper */

    last_alloca_header = hp;    /* -> last valid storage */
  }

  if (size == 0)
    return NULL;                /* no allocation required */

  /* Allocate combined header + user data storage. */

  {
    register pointer    new = xmalloc (sizeof (header) + size);
    /* address of header */

    ((header *)new)->h.next = last_alloca_header;
    ((header *)new)->h.deep = depth;

    last_alloca_header = (header *)new;

    /* User storage begins just after header. */

    return (pointer)((char *)new + sizeof(header));
  }
}
#endif



#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         -2
#define YYEOF           0
#define YYFAIL          goto yyerrlab;

#define YYTERROR        1

#ifndef YYIMPURE
#define YYLEX           yylex()
#endif

#ifndef YYPURE
#define YYLEX           yylex(&yylval, &yylloc)
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYIMPURE

int     yychar;                 /*  the lookahead symbol                */
YYSTYPE yylval;                 /*  the semantic value of the           */
                                /*  lookahead symbol                    */

YYLTYPE yylloc;                 /*  location data for the lookahead     */
                                /*  symbol                              */

int yynerr;                     /*  number of parse errors so far       */

#ifdef YYDEBUG
int yydebug = 0;                /*  nonzero means print parse trace     */
#endif

#endif  /* YYIMPURE */


/*  YYMAXDEPTH indicates the initial size of the parser's stacks        */

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 200
#endif

/*  YYMAXLIMIT is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#ifndef YYMAXLIMIT
#define YYMAXLIMIT 10000
#endif


#line 165 "bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  YYLTYPE *yylsp;
  int yyerrstatus;      /*  number of tokens to shift before error messages enabled */
  int yychar1;          /*  lookahead token as an internal (translated) token number */

  short yyssa[YYMAXDEPTH];      /*  the state stack                     */
  YYSTYPE yyvsa[YYMAXDEPTH];    /*  the semantic value stack            */
  YYLTYPE yylsa[YYMAXDEPTH];    /*  the location stack                  */

  short *yyss = yyssa;          /*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;        /*  to allow yyoverflow to reallocate them elsewhere */
  YYLTYPE *yyls = yylsa;

  int yymaxdepth = YYMAXDEPTH;

#ifndef YYPURE
  int yychar;
  YYSTYPE yylval;
  YYLTYPE yylloc;
#endif

#ifdef YYDEBUG
  extern int yydebug;
#endif


  YYSTYPE yyval;                /*  the variable used to return         */
                                /*  semantic values from the action     */
                                /*  routines                            */

  int yylen;

#ifdef YYDEBUG
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerr = 0;
  yychar = YYEMPTY;             /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
  yylsp = yyls;

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yymaxdepth - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      YYLTYPE *yyls1 = yyls;
      short *yyss1 = yyss;

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
         the data in use in that stack, in bytes.  */
      yyoverflow("parser stack overflow",
                 &yyss1, size * sizeof (*yyssp),
                 &yyvs1, size * sizeof (*yyvsp),
                 &yyls1, size * sizeof (*yylsp),
                 &yymaxdepth);

      yyss = yyss1; yyvs = yyvs1; yyls = yyls1;
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yymaxdepth >= YYMAXLIMIT)
        yyerror("parser stack overflow");
      yymaxdepth *= 2;
      if (yymaxdepth > YYMAXLIMIT)
        yymaxdepth = YYMAXLIMIT;
      yyss = (short *) alloca (yymaxdepth * sizeof (*yyssp));
      bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yymaxdepth * sizeof (*yyvsp));
      bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yymaxdepth * sizeof (*yylsp));
      bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#ifdef YYDEBUG
      if (yydebug)
        fprintf(stderr, "Stack size increased to %d\n", yymaxdepth);
#endif

      if (yyssp >= yyss + yymaxdepth - 1)
        YYERROR;
    }

#ifdef YYDEBUG
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
yyresume:

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#ifdef YYDEBUG
      if (yydebug)
        fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)              /* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;           /* Don't call YYLEX any more */

#ifdef YYDEBUG
      if (yydebug)
        fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#ifdef YYDEBUG
      if (yydebug)
        fprintf(stderr, "Next token is %d (%s)\n", yychar, yytname[yychar1]);
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

#ifdef YYDEBUG
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

#ifdef YYDEBUG
  if (yydebug)
    {
      if (yylen == 1)
        fprintf (stderr, "Reducing 1 value via line %d, ",
                 yyrline[yyn]);
      else
        fprintf (stderr, "Reducing %d values via line %d, ",
                 yylen, yyrline[yyn]);
    }
#endif


  switch (yyn) {

case 1:
#line 66 "web2c.yacc"
{block_level++;
			 printf("#include \"%s\"\n", std_header);;
    break;}
case 2:
#line 70 "web2c.yacc"
{printf("\n#include \"%s\"\n", coerce_name); ;
    break;}
case 3:
#line 73 "web2c.yacc"
{YYACCEPT;;
    break;}
case 6:
#line 81 "web2c.yacc"
{
			    ii = add_to_table(last_id); 
			    sym_table[ii].typ = field_id_tok;
			;
    break;}
case 7:
#line 86 "web2c.yacc"
{
			    ii = add_to_table(last_id); 
			    sym_table[ii].typ = fun_id_tok;
			;
    break;}
case 8:
#line 91 "web2c.yacc"
{
			    ii = add_to_table(last_id); 
			    sym_table[ii].typ = const_id_tok;
			;
    break;}
case 9:
#line 96 "web2c.yacc"
{
			    ii = add_to_table(last_id); 
			    sym_table[ii].typ = fun_param_tok;
			;
    break;}
case 10:
#line 101 "web2c.yacc"
{
			    ii = add_to_table(last_id); 
			    sym_table[ii].typ = proc_id_tok;
			;
    break;}
case 11:
#line 106 "web2c.yacc"
{
			    ii = add_to_table(last_id); 
			    sym_table[ii].typ = proc_param_tok;
			;
    break;}
case 12:
#line 111 "web2c.yacc"
{
			    ii = add_to_table(last_id); 
			    sym_table[ii].typ = type_id_tok;
			;
    break;}
case 13:
#line 117 "web2c.yacc"
{
			    ii = add_to_table(last_id); 
			    sym_table[ii].typ = type_id_tok;
			    sym_table[ii].val = lower_bound;
			    sym_table[ii].val_sym = lower_sym;
			    sym_table[ii].upper = upper_bound;
			    sym_table[ii].upper_sym = upper_sym;
			;
    break;}
case 14:
#line 126 "web2c.yacc"
{
			    ii = add_to_table(last_id); 
			    sym_table[ii].typ = var_id_tok;
			;
    break;}
case 16:
#line 136 "web2c.yacc"
{	if (block_level > 0) my_output("{");
			indent++; block_level++;
		      ;
    break;}
case 17:
#line 141 "web2c.yacc"
{if (block_level == 2) {
			    if (strcmp(function_return_type, "void")) {
			      my_output("register");
			      my_output(function_return_type);
			      my_output("Result;");
			    }
			    if (tex) {
			      (void) sprintf(safe_string, "%s_regmem",
						my_routine);
			      my_output(safe_string);
			      indent_line();
			    }
			 }
			;
    break;}
case 18:
#line 156 "web2c.yacc"
{if (block_level == 1)
				puts("\n#include \"coerce.h\"");
			 doing_statements = TRUE;
			;
    break;}
case 19:
#line 161 "web2c.yacc"
{if (block_level == 2) {
			    if (strcmp(function_return_type,"void")) {
			      my_output("return(Result)");
			      semicolon();
			     }
			     if (tex) {
			       if (uses_mem && uses_eqtb)
				(void) fprintf(coerce,
	"#define %s_regmem register memoryword *mem=zmem, *eqtb=zeqtb;\n",
				   my_routine);
			       else if (uses_mem)
				(void) fprintf(coerce,
			"#define %s_regmem register memoryword *mem=zmem;\n",
				   my_routine);
			       else if (uses_eqtb)
				(void) fprintf(coerce,
			"#define %s_regmem register memoryword *eqtb=zeqtb;\n",
				   my_routine);
			       else
				(void) fprintf(coerce,
				   "#define %s_regmem\n",
				   my_routine);
			    }
			    my_routine[0] = '\0';
			 }
			 indent--; block_level--;
			 my_output("}"); new_line();
			 doing_statements = FALSE;
			;
    break;}
case 21:
#line 194 "web2c.yacc"
{ my_output("/*"); ;
    break;}
case 22:
#line 196 "web2c.yacc"
{ my_output("*/"); ;
    break;}
case 25:
#line 204 "web2c.yacc"
{ my_output(temp); ;
    break;}
case 27:
#line 209 "web2c.yacc"
{ indent_line(); ;
    break;}
case 30:
#line 217 "web2c.yacc"
{ new_line(); my_output("#define"); ;
    break;}
case 31:
#line 219 "web2c.yacc"
{ ii=add_to_table(last_id);
				  sym_table[ii].typ = const_id_tok;
				  my_output(last_id);
				;
    break;}
case 32:
#line 224 "web2c.yacc"
{ sym_table[ii].val=last_i_num;
				  new_line(); ;
    break;}
case 33:
#line 229 "web2c.yacc"
{
				  (void) sscanf(temp, "%ld", &last_i_num);
				  if (labs((long) last_i_num) > 32767)
				      (void) strcat(temp, "L");
				  my_output(temp);
				  yyval = ex_32;
				;
    break;}
case 34:
#line 237 "web2c.yacc"
{ my_output(temp);
				  yyval = ex_real;
				;
    break;}
case 35:
#line 241 "web2c.yacc"
{ yyval = 0; ;
    break;}
case 36:
#line 243 "web2c.yacc"
{ yyval = ex_32; ;
    break;}
case 37:
#line 247 "web2c.yacc"
{ int i, j; char s[132];
	  			  j = 1;
				  s[0] = '"';
	  			  for (i=1; yytext[i-1]!=0; i++) {
	  			    if (yytext[i] == '\\' || yytext[i] == '"')
					s[j++]='\\';
	    			    else if (yytext[i] == '\'') i++;
	    			    s[j++] = yytext[i];
				  }
	    			  s[j-1] = '"';
				  s[j] = 0;
				  my_output(s);
				;
    break;}
case 38:
#line 261 "web2c.yacc"
{ char s[5];
				  s[0]='\'';
	    			  if (yytext[1] == '\\' || yytext[1] == '\'') {
	  				s[2] = yytext[1];
					s[1] = '\\';
					s[3] = '\'';
					s[4] = '\0';
				  }
	  			  else {
					s[1] = yytext[1];
					s[2]='\'';
					s[3]='\0';
				  }
	  			  my_output(s);
				;
    break;}
case 39:
#line 279 "web2c.yacc"
{ my_output(last_id); ;
    break;}
case 44:
#line 291 "web2c.yacc"
{ my_output("typedef"); ;
    break;}
case 45:
#line 293 "web2c.yacc"
{ ii = add_to_table(last_id);
				  sym_table[ii].typ = type_id_tok;
				  (void) strcpy(safe_string, last_id);
				  last_type = ii;
				;
    break;}
case 46:
#line 299 "web2c.yacc"
{
				  array_bounds[0] = 0;
				  array_offset[0] = 0;
				;
    break;}
case 47:
#line 304 "web2c.yacc"
{ if (*array_offset) {
			fprintf(stderr, "Cannot typedef arrays with offsets\n");
					exit(1);
				  }
				  my_output(safe_string);
				  my_output(array_bounds);
				  semicolon();
				  last_type = -1;
				;
    break;}
case 50:
#line 320 "web2c.yacc"
{ if (last_type >= 0) {
					sym_table[ii].val = lower_bound;
					sym_table[ii].val_sym = lower_sym;
			 		sym_table[ii].upper = upper_bound;
					sym_table[ii].upper_sym = upper_sym;
					ii= -1;
				  }
/* The following code says: if the bounds are known at translation time
 * on an integral type, then we select the smallest type of data which
 * can represent it in ANSI C.  We only use unsigned types when necessary.
 */
				  if (lower_sym == -1 && upper_sym == -1) {
				    if (lower_bound>= -127 && upper_bound<=127)
					my_output("schar");
				    else if (lower_bound >= 0
				      && upper_bound <= 255)
					my_output("unsigned char");
	  			    else if (lower_bound >= -32767
				      && upper_bound <= 32767)
					my_output("short");
	  			    else if (lower_bound >= 0
				      && upper_bound <= 65535)
					my_output("unsigned short");
	  			    else my_output("integer");
				  }
				  else my_output("integer");
				;
    break;}
case 55:
#line 358 "web2c.yacc"
{lower_bound = upper_bound;
				 lower_sym = upper_sym;
				 (void) sscanf(temp, "%ld", &upper_bound);
				 upper_sym = -1; /* no sym table entry */
				;
    break;}
case 56:
#line 364 "web2c.yacc"
{ lower_bound = upper_bound;
				  lower_sym = upper_sym;
				  upper_bound = sym_table[l_s].val;
				  upper_sym = l_s;
				;
    break;}
case 57:
#line 372 "web2c.yacc"
{if (last_type >= 0) {
	    sym_table[last_type].var_not_needed = sym_table[l_s].var_not_needed;
	    sym_table[last_type].upper = sym_table[l_s].upper;
	    sym_table[last_type].upper_sym = sym_table[l_s].upper_sym;
	    sym_table[last_type].val = sym_table[l_s].val;
	    sym_table[last_type].val_sym = sym_table[l_s].val_sym;
	 }
	 my_output(last_id); ;
    break;}
case 58:
#line 383 "web2c.yacc"
{if (last_type >= 0)
				    sym_table[last_type].var_not_needed = TRUE;;
    break;}
case 60:
#line 387 "web2c.yacc"
{if (last_type >= 0)
				    sym_table[last_type].var_not_needed = TRUE;;
    break;}
case 61:
#line 390 "web2c.yacc"
{if (last_type >= 0)
				    sym_table[last_type].var_not_needed = TRUE;;
    break;}
case 62:
#line 395 "web2c.yacc"
{if (last_type >= 0) {
	    sym_table[last_type].var_not_needed = sym_table[l_s].var_not_needed;
	    sym_table[last_type].upper = sym_table[l_s].upper;
	    sym_table[last_type].upper_sym = sym_table[l_s].upper_sym;
	    sym_table[last_type].val = sym_table[l_s].val;
	    sym_table[last_type].val_sym = sym_table[l_s].val_sym;
	 }
	 my_output(last_id); my_output("*"); ;
    break;}
case 65:
#line 411 "web2c.yacc"
{ compute_array_bounds(); ;
    break;}
case 66:
#line 413 "web2c.yacc"
{ lower_bound = sym_table[l_s].val;
				  lower_sym = sym_table[l_s].val_sym;
				  upper_bound = sym_table[l_s].upper;
				  upper_sym = sym_table[l_s].upper_sym;
				  compute_array_bounds();
				;
    break;}
case 68:
#line 426 "web2c.yacc"
{ my_output("struct"); my_output("{"); indent++;;
    break;}
case 69:
#line 428 "web2c.yacc"
{ indent--; my_output("}"); semicolon(); ;
    break;}
case 72:
#line 436 "web2c.yacc"
{ field_list[0] = 0; ;
    break;}
case 73:
#line 438 "web2c.yacc"
{
				  /*array_bounds[0] = 0;
				  array_offset[0] = 0;*/
				;
    break;}
case 74:
#line 443 "web2c.yacc"
{ int i=0, j; char ltemp[80];
				  while(field_list[i++] == '!') {
					j = 0;
					while (field_list[i])
					    ltemp[j++] = field_list[i++];
					i++;
					if (field_list[i] == '!')
						ltemp[j++] = ',';
					ltemp[j] = 0;
					my_output(ltemp);
				  }
				  semicolon();
				;
    break;}
case 78:
#line 464 "web2c.yacc"
{ int i=0, j=0;
				  while (field_list[i] == '!')
					while(field_list[i++]);
				  ii = add_to_table(last_id);
				  sym_table[ii].typ = field_id_tok;
				  field_list[i++] = '!';
				  while (last_id[j])
					field_list[i++] = last_id[j++];
				  field_list[i++] = 0;
				  field_list[i++] = 0;
				;
    break;}
case 79:
#line 476 "web2c.yacc"
{ int i=0, j=0;
				  while (field_list[i] == '!')
					while(field_list[i++]);
				  field_list[i++] = '!';
				  while (last_id[j])
					field_list[i++] = last_id[j++];
				  field_list[i++] = 0;
				  field_list[i++] = 0;
				;
    break;}
case 80:
#line 488 "web2c.yacc"
{ my_output("file_ptr /* of "); ;
    break;}
case 81:
#line 490 "web2c.yacc"
{ my_output("*/"); ;
    break;}
case 86:
#line 502 "web2c.yacc"
{ var_list[0] = 0;
				  array_bounds[0] = 0;
				  array_offset[0] = 0;
				  var_formals = FALSE;
				  ids_paramed = 0;
				;
    break;}
case 87:
#line 509 "web2c.yacc"
{
				  array_bounds[0] = 0;	
				  array_offset[0] = 0;
				;
    break;}
case 88:
#line 514 "web2c.yacc"
{ fixup_var_list(); ;
    break;}
case 91:
#line 522 "web2c.yacc"
{ int i=0, j=0;
				  ii = add_to_table(last_id);
				  sym_table[ii].typ = var_id_tok;
				  sym_table[ii].var_formal = var_formals;
				  param_id_list[ids_paramed++] = ii;
	  			  while (var_list[i] == '!')
					while(var_list[i++]);
				  var_list[i++] = '!';
				  while (last_id[j])
					var_list[i++] = last_id[j++];
	  			  var_list[i++] = 0;
				  var_list[i++] = 0;
				;
    break;}
case 92:
#line 536 "web2c.yacc"
{ int i=0, j=0;
				  ii = add_to_table(last_id);
	  			  sym_table[ii].typ = var_id_tok;
				  sym_table[ii].var_formal = var_formals;
				  param_id_list[ids_paramed++] = ii;
	  			  while (var_list[i] == '!')
					while (var_list[i++]);
	  			  var_list[i++] = '!';
				  while (last_id[j])
					var_list[i++] = last_id[j++];
	  			  var_list[i++] = 0;
				  var_list[i++] = 0;
				;
    break;}
case 93:
#line 550 "web2c.yacc"
{ int i=0, j=0;
				  ii = add_to_table(last_id);
	  			  sym_table[ii].typ = var_id_tok;
				  sym_table[ii].var_formal = var_formals;
				  param_id_list[ids_paramed++] = ii;
	  			  while (var_list[i] == '!')
					while(var_list[i++]);
	  			  var_list[i++] = '!';
				  while (last_id[j])
					var_list[i++] = last_id[j++];
				  var_list[i++] = 0;
				  var_list[i++] = 0;
				;
    break;}
case 95:
#line 567 "web2c.yacc"
{ my_output("void main_body() {");
				  indent++;
			      	  new_line();
                                ;
    break;}
case 96:
#line 572 "web2c.yacc"
{ indent--; my_output("}"); new_line(); ;
    break;}
case 99:
#line 580 "web2c.yacc"
{ indent_line(); remove_locals(); ;
    break;}
case 100:
#line 582 "web2c.yacc"
{ indent_line(); remove_locals(); ;
    break;}
case 102:
#line 589 "web2c.yacc"
{ ii = add_to_table(last_id);
				  if (debug)
				  (void) fprintf(stderr, "%3d Procedure %s\n",
					pf_count++, last_id);
				  sym_table[ii].typ = proc_id_tok;
				  (void) strcpy(my_routine, last_id);
				  uses_eqtb = uses_mem = FALSE;
				  my_output("void");
				  orig_std = std;
				  std = 0;
				;
    break;}
case 103:
#line 601 "web2c.yacc"
{(void) strcpy(function_return_type, "void");
				 do_proc_args();
				 gen_function_head();;
    break;}
case 104:
#line 605 "web2c.yacc"
{ ii = l_s; 
				  if (debug)
				  (void) fprintf(stderr, "%3d Procedure %s\n",
					pf_count++, last_id);
				  (void) strcpy(my_routine, last_id);
				  my_output("void");
				;
    break;}
case 105:
#line 613 "web2c.yacc"
{(void) strcpy(function_return_type, "void");
				 do_proc_args();
				 gen_function_head();;
    break;}
case 106:
#line 619 "web2c.yacc"
{ (void) strcpy(z_id, last_id);
				  mark();
				  ids_paramed = 0;
				;
    break;}
case 107:
#line 624 "web2c.yacc"
{ if (ansi) (void) strcpy(z_id, last_id);
				  else (void) sprintf(z_id, "z%s", last_id);
				  ids_paramed = 0;
 				  if (sym_table[ii].typ == proc_id_tok)
				  	sym_table[ii].typ = proc_param_tok;
				  else if (sym_table[ii].typ == fun_id_tok)
					sym_table[ii].typ = fun_param_tok;
				  mark();
				;
    break;}
case 111:
#line 640 "web2c.yacc"
{ ids_typed = ids_paramed;;
    break;}
case 112:
#line 642 "web2c.yacc"
{int i, need_var;
				 i = search_table(last_id);
				 need_var = !sym_table[i].var_not_needed;
				 for (i=ids_typed; i<ids_paramed; i++) {
					(void) strcpy(arg_type[i], last_id);
		if (need_var && sym_table[param_id_list[i]].var_formal)
					(void) strcat(arg_type[i], " *");
		else sym_table[param_id_list[i]].var_formal = FALSE;
				 }
				;
    break;}
case 113:
#line 654 "web2c.yacc"
{var_formals = 0;;
    break;}
case 115:
#line 655 "web2c.yacc"
{var_formals = 1;;
    break;}
case 120:
#line 666 "web2c.yacc"
{ orig_std = std;
				  std = 0;
				  ii = add_to_table(last_id);
				  if (debug)
				  (void) fprintf(stderr, "%3d Function %s\n",
					pf_count++, last_id);
	  			  sym_table[ii].typ = fun_id_tok;
				  (void) strcpy(my_routine, last_id);
				  uses_eqtb = uses_mem = FALSE;
				;
    break;}
case 121:
#line 677 "web2c.yacc"
{ normal();
				  array_bounds[0] = 0;
				  array_offset[0] = 0;
				;
    break;}
case 122:
#line 682 "web2c.yacc"
{(void) strcpy(function_return_type, yytext);
			     do_proc_args();
			     gen_function_head();
			    ;
    break;}
case 124:
#line 689 "web2c.yacc"
{ orig_std = std;
				  std = 0;
				  ii = l_s;
				  (void) fprintf(stderr, "%3d Function %s\n",
					pf_count++, last_id);
				  (void) strcpy(my_routine, last_id);
				  uses_eqtb = uses_mem = FALSE;
				;
    break;}
case 125:
#line 698 "web2c.yacc"
{ normal();
				  array_bounds[0] = 0;
				  array_offset[0] = 0;
				;
    break;}
case 126:
#line 703 "web2c.yacc"
{(void) strcpy(function_return_type, yytext);
			     do_proc_args();
			     gen_function_head();
			    ;
    break;}
case 132:
#line 721 "web2c.yacc"
{my_output("{"); indent++; new_line();;
    break;}
case 133:
#line 723 "web2c.yacc"
{ indent--; my_output("}"); new_line(); ;
    break;}
case 138:
#line 736 "web2c.yacc"
{if (!doreturn(temp)) {
				    (void) sprintf(safe_string, "lab%s:",
					temp);
				    my_output(safe_string);
				 }
				;
    break;}
case 139:
#line 745 "web2c.yacc"
{ semicolon(); ;
    break;}
case 140:
#line 747 "web2c.yacc"
{ semicolon(); ;
    break;}
case 145:
#line 755 "web2c.yacc"
{my_output("break");;
    break;}
case 146:
#line 759 "web2c.yacc"
{ my_output("="); ;
    break;}
case 148:
#line 762 "web2c.yacc"
{ my_output("Result ="); ;
    break;}
case 150:
#line 767 "web2c.yacc"
{ if (strcmp(last_id, "mem") == 0)
					uses_mem = 1;
				  else if (strcmp(last_id, "eqtb") == 0)
					uses_eqtb = 1;
				  if (sym_table[l_s].var_formal)
					(void) putchar('*');
				  my_output(last_id);
				  yyval = ex_32;
				;
    break;}
case 152:
#line 778 "web2c.yacc"
{ if (sym_table[l_s].var_formal)
					(void) putchar('*');
				  my_output(last_id); yyval = ex_32; ;
    break;}
case 153:
#line 784 "web2c.yacc"
{ yyval = ex_32; ;
    break;}
case 154:
#line 786 "web2c.yacc"
{ yyval = ex_32; ;
    break;}
case 157:
#line 794 "web2c.yacc"
{ my_output("["); ;
    break;}
case 158:
#line 796 "web2c.yacc"
{ my_output("]"); ;
    break;}
case 159:
#line 798 "web2c.yacc"
{if (tex || mf) {
				   if (strcmp(last_id, "int")==0)
					my_output(".cint");
				   else if (strcmp(last_id, "lh")==0)
					my_output(".v.LH");
				   else if (strcmp(last_id, "rh")==0)
					my_output(".v.RH");
				   else {
				     (void)sprintf(safe_string, ".%s", last_id);
				     my_output(safe_string);
				   }
				 }
				 else {
				    (void) sprintf(safe_string, ".%s", last_id);
				    my_output(safe_string);
				 }
				;
    break;}
case 160:
#line 816 "web2c.yacc"
{ my_output(".hh.b0");;
    break;}
case 161:
#line 818 "web2c.yacc"
{ my_output(".hh.b1");;
    break;}
case 163:
#line 823 "web2c.yacc"
{ my_output("][");;
    break;}
case 165:
#line 828 "web2c.yacc"
{ yyval = yyvsp[0]; ;
    break;}
case 166:
#line 829 "web2c.yacc"
{my_output("+");;
    break;}
case 167:
#line 830 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 168:
#line 831 "web2c.yacc"
{my_output("-");;
    break;}
case 169:
#line 832 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 170:
#line 833 "web2c.yacc"
{my_output("*");;
    break;}
case 171:
#line 834 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 172:
#line 835 "web2c.yacc"
{my_output("/");;
    break;}
case 173:
#line 836 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 174:
#line 837 "web2c.yacc"
{my_output("==");;
    break;}
case 175:
#line 838 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 176:
#line 839 "web2c.yacc"
{my_output("!=");;
    break;}
case 177:
#line 840 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 178:
#line 841 "web2c.yacc"
{my_output("%");;
    break;}
case 179:
#line 842 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 180:
#line 843 "web2c.yacc"
{my_output("<");;
    break;}
case 181:
#line 844 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 182:
#line 845 "web2c.yacc"
{my_output(">");;
    break;}
case 183:
#line 846 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 184:
#line 847 "web2c.yacc"
{my_output("<=");;
    break;}
case 185:
#line 848 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 186:
#line 849 "web2c.yacc"
{my_output(">=");;
    break;}
case 187:
#line 850 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 188:
#line 851 "web2c.yacc"
{my_output("&&");;
    break;}
case 189:
#line 852 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 190:
#line 853 "web2c.yacc"
{my_output("||");;
    break;}
case 191:
#line 854 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]);;
    break;}
case 192:
#line 856 "web2c.yacc"
{ my_output("/ ((double)"); ;
    break;}
case 193:
#line 858 "web2c.yacc"
{yyval = max(yyvsp[-3], yyvsp[0]); my_output(")"); ;
    break;}
case 194:
#line 860 "web2c.yacc"
{ yyval = yyvsp[0]; ;
    break;}
case 196:
#line 865 "web2c.yacc"
{ my_output("- (integer)"); ;
    break;}
case 197:
#line 867 "web2c.yacc"
{ my_output("!"); ;
    break;}
case 198:
#line 871 "web2c.yacc"
{ my_output("("); ;
    break;}
case 199:
#line 873 "web2c.yacc"
{ my_output(")"); yyval = yyvsp[-3]; ;
    break;}
case 202:
#line 877 "web2c.yacc"
{ my_output(last_id); my_output("()"); ;
    break;}
case 203:
#line 879 "web2c.yacc"
{ my_output(last_id); ;
    break;}
case 205:
#line 884 "web2c.yacc"
{ my_output("("); ;
    break;}
case 206:
#line 886 "web2c.yacc"
{ my_output(")"); ;
    break;}
case 208:
#line 891 "web2c.yacc"
{ my_output(","); ;
    break;}
case 213:
#line 904 "web2c.yacc"
{ my_output(last_id); my_output("()"); ;
    break;}
case 214:
#line 906 "web2c.yacc"
{ my_output(last_id);
				  ii = add_to_table(last_id);
	  			  sym_table[ii].typ = proc_id_tok;
				  my_output("()");
				;
    break;}
case 215:
#line 912 "web2c.yacc"
{ my_output(last_id); ;
    break;}
case 217:
#line 917 "web2c.yacc"
{if (doreturn(temp)) {
				    if (strcmp(function_return_type,"void"))
					my_output("return(Result)");
				    else
					my_output("return");
				 } else {
				     (void) sprintf(safe_string, "goto lab%s",
					temp);
				     my_output(safe_string);
				 }
				;
    break;}
case 226:
#line 947 "web2c.yacc"
{ my_output("if"); my_output("("); ;
    break;}
case 227:
#line 949 "web2c.yacc"
{ my_output(")"); new_line();;
    break;}
case 229:
#line 954 "web2c.yacc"
{ my_output("else"); ;
    break;}
case 231:
#line 959 "web2c.yacc"
{ my_output("switch"); my_output("("); ;
    break;}
case 232:
#line 961 "web2c.yacc"
{ my_output(")"); indent_line();
				  my_output("{"); indent++;
				;
    break;}
case 233:
#line 965 "web2c.yacc"
{ indent--; my_output("}"); new_line(); ;
    break;}
case 236:
#line 973 "web2c.yacc"
{ my_output("break"); semicolon(); ;
    break;}
case 239:
#line 981 "web2c.yacc"
{ my_output("case"); 
				  my_output(temp);
				  my_output(":"); indent_line();
				;
    break;}
case 240:
#line 986 "web2c.yacc"
{ my_output("default:"); indent_line(); ;
    break;}
case 246:
#line 999 "web2c.yacc"
{ my_output("while");
				  my_output("(");
				;
    break;}
case 247:
#line 1003 "web2c.yacc"
{ my_output(")"); ;
    break;}
case 249:
#line 1008 "web2c.yacc"
{ my_output("do"); my_output("{"); indent++; ;
    break;}
case 250:
#line 1010 "web2c.yacc"
{ indent--; my_output("}"); 
				  my_output("while"); my_output("( ! (");
				;
    break;}
case 251:
#line 1014 "web2c.yacc"
{ my_output(") )"); ;
    break;}
case 252:
#line 1018 "web2c.yacc"
{
				  my_output("{");
				  my_output("register");
				  my_output("integer");
				  if (strict_for)
					my_output("for_begin,");
				  my_output("for_end;");
				 ;
    break;}
case 253:
#line 1027 "web2c.yacc"
{ if (strict_for)
					my_output("for_begin");
				  else
					my_output(control_var);
				  my_output("="); ;
    break;}
case 254:
#line 1033 "web2c.yacc"
{ my_output("; if (");
				  if (strict_for) my_output("for_begin");
				  else my_output(control_var);
				  my_output(relation);
				  my_output("for_end)");
				  if (strict_for) {
					my_output("{");
					my_output(control_var);
					my_output("=");
					my_output("for_begin");
					semicolon();
				  }
				  my_output("do"); 
				  indent++; 
				  new_line();;
    break;}
case 255:
#line 1049 "web2c.yacc"
{
				  char *top = rindex(for_stack, '#');
				  indent--; new_line();
				  my_output("while"); 
				  my_output("("); 
				  my_output(top+1); 
				  my_output(")"); 
				  my_output(";");
				  my_output("}");
				  if (strict_for)
					my_output("}");
				  *top=0;
				  new_line();
				;
    break;}
case 256:
#line 1066 "web2c.yacc"
{(void) strcpy(control_var, last_id); ;
    break;}
case 257:
#line 1070 "web2c.yacc"
{ my_output(";"); ;
    break;}
case 258:
#line 1072 "web2c.yacc"
{ 
				  (void) strcpy(relation, "<=");
				  my_output("for_end");
				  my_output("="); ;
    break;}
case 259:
#line 1077 "web2c.yacc"
{ 
				  (void) sprintf(for_stack + strlen(for_stack),
				    "#%s++ < for_end", control_var);
				;
    break;}
case 260:
#line 1082 "web2c.yacc"
{ my_output(";"); ;
    break;}
case 261:
#line 1084 "web2c.yacc"
{
				  (void) strcpy(relation, ">=");
				  my_output("for_end");
				  my_output("="); ;
    break;}
case 262:
#line 1089 "web2c.yacc"
{ 
				  (void) sprintf(for_stack + strlen(for_stack),
				    "#%s-- > for_end", control_var);
				;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 303 "bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#ifdef YYDEBUG
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
      ++yynerr;
      yyerror("parse error");
    }

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
        YYERROR;

#ifdef YYDEBUG
      if (yydebug)
        fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;              /* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYERROR;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#ifdef YYDEBUG
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

#ifdef YYDEBUG
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
#line 1094 "web2c.yacc"


static void compute_array_bounds()
{
    long lb;
    char tmp[200];

    if (lower_sym == -1) {	/* lower is a constant */
	lb = lower_bound - 1;
	if (lb==0) lb = -1;	/* Treat lower_bound==1 as if lower_bound==0 */
	if (upper_sym == -1)	/* both constants */
	    (void) sprintf(tmp, "[%ld]", upper_bound - lb);
	else {			/* upper a symbol, lower constant */
	    if (lb < 0)
		(void) sprintf(tmp, "[%s + %ld]",
				symbol(upper_sym), (-lb));
	    else
		(void) sprintf(tmp, "[%s - %ld]",
				symbol(upper_sym), lb);
	}
	if (lower_bound < 0 || lower_bound > 1) {
	    if (*array_bounds) {
		fprintf(stderr, "Cannot handle offset in second dimension\n");
		exit(1);
	    }
	    if (lower_bound < 0) {
		(void) sprintf(array_offset, "+%ld", -lower_bound);
	    } else {
		(void) sprintf(array_offset, "-%ld", lower_bound);
	    }
	}
	(void) strcat(array_bounds, tmp);
    }
    else {			/* lower is a symbol */
	if (upper_sym != -1)	/* both are symbols */
	    (void) sprintf(tmp, "[%s - %s + 1]", symbol(upper_sym),
		symbol(lower_sym));
	else {			/* upper constant, lower symbol */
	    (void) sprintf(tmp, "[%ld - %s]", upper_bound + 1,
		symbol(lower_sym));
	}
	if (*array_bounds) {
	    fprintf(stderr, "Cannot handle symbolic offset in second dimension\n");
	    exit(1);
	}
	(void) sprintf(array_offset, "- (int)(%s)", symbol(lower_sym));
	(void) strcat(array_bounds, tmp);
    }
}

static void fixup_var_list()
{
    int i, j;
    char output_string[100], real_symbol[100];

    for (i=0; var_list[i++] == '!'; ) {
	for (j=0; real_symbol[j++] = var_list[i++];);
	if (*array_offset) {
	    (void) fprintf(std, "\n#define %s (%s %s)\n  ",
	        real_symbol, next_temp, array_offset);
	    (void) strcpy(real_symbol, next_temp);
	    find_next_temp();
	}
	(void) sprintf(output_string, "%s%s%c",
	    real_symbol, array_bounds, (var_list[i]=='!' ? ',' : ' '));
	my_output(output_string);
    }
    semicolon();
}


/*
 * If we're not processing TeX, we return 0 (false).  Otherwise,
 * return 1 if the label is "10" and we're not in one of four TeX
 * routines where the line labeled "10" isn't the end of the routine.
 * Otherwise, return 0.
 */
static boolean doreturn(label)
char *label;
{
    if (!tex) return(FALSE);
    if (strcmp(label, "10")) return(FALSE);
    if (strcmp(my_routine, "macrocall") == 0) return(FALSE);
    if (strcmp(my_routine, "hpack") == 0) return(FALSE);
    if (strcmp(my_routine, "vpackage") == 0) return(FALSE);
    if (strcmp(my_routine, "trybreak") == 0) return(FALSE);
    return(TRUE);
}


/* Return the absolute value of a long */
static long labs(x)
long x;
{
    if (x < 0L) return(-x);
    return(x);
}

static void do_proc_args()
{
    int i;

    if (ansi) {
	fprintf(coerce, "%s %s(", function_return_type, z_id);
	if (ids_paramed == 0) fprintf(coerce, "void");
	for (i=0; i<ids_paramed; i++) {
	    if (i > 0) putc(',', coerce);
	    fprintf(coerce, "%s %s",
		arg_type[i],
		symbol(param_id_list[i]));
	}
	fprintf(coerce, ");\n");
    } else
	fprintf(coerce, "%s %s();\n", function_return_type, z_id);
}

static void gen_function_head()
{
    int i;

    if (strcmp(my_routine, z_id)) {
	fprintf(coerce, "#define %s(", my_routine);
	for (i=0; i<ids_paramed; i++) {
	    if (i > 0)
		fprintf(coerce, ", %s", symbol(param_id_list[i]));
	    else
		fprintf(coerce, "%s", symbol(param_id_list[i]));
	}
	fprintf(coerce, ") %s(", z_id);
	for (i=0; i<ids_paramed; i++) {
	    if (i > 0)
		fprintf(coerce, ", (%s) %s(%s)",
		    arg_type[i],
		    sym_table[param_id_list[i]].var_formal?"&":"",
		    symbol(param_id_list[i]));
	    else
		fprintf(coerce, "(%s) %s(%s)",
		    arg_type[i],
		    sym_table[param_id_list[i]].var_formal?"&":"",
		    symbol(param_id_list[i]));
	}
	fprintf(coerce, ")\n");
    }
    std = orig_std;
    my_output(z_id);
    my_output("(");
    if (ansi) {
	for (i=0; i<ids_paramed; i++) {
	    if (i > 0) my_output(",");
	    my_output(arg_type[i]);
	    my_output(symbol(param_id_list[i]));
	}
	my_output(")");
	indent_line();
    } else {	/* Not ansi */
	for (i=0; i<ids_paramed; i++) {
	    if (i > 0) my_output(",");
	    my_output(symbol(param_id_list[i]));
	}
	my_output(")");
	indent_line();
	for (i=0; i<ids_paramed; i++) {
	    my_output(arg_type[i]);
	    my_output(symbol(param_id_list[i]));
	    semicolon();
	}
    }
}
