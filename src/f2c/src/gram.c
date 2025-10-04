# define SEOS 1
# define SCOMMENT 2
# define SLABEL 3
# define SUNKNOWN 4
# define SHOLLERITH 5
# define SICON 6
# define SRCON 7
# define SDCON 8
# define SBITCON 9
# define SOCTCON 10
# define SHEXCON 11
# define STRUE 12
# define SFALSE 13
# define SNAME 14
# define SNAMEEQ 15
# define SFIELD 16
# define SSCALE 17
# define SINCLUDE 18
# define SLET 19
# define SASSIGN 20
# define SAUTOMATIC 21
# define SBACKSPACE 22
# define SBLOCK 23
# define SCALL 24
# define SCHARACTER 25
# define SCLOSE 26
# define SCOMMON 27
# define SCOMPLEX 28
# define SCONTINUE 29
# define SDATA 30
# define SDCOMPLEX 31
# define SDIMENSION 32
# define SDO 33
# define SDOUBLE 34
# define SELSE 35
# define SELSEIF 36
# define SEND 37
# define SENDFILE 38
# define SENDIF 39
# define SENTRY 40
# define SEQUIV 41
# define SEXTERNAL 42
# define SFORMAT 43
# define SFUNCTION 44
# define SGOTO 45
# define SASGOTO 46
# define SCOMPGOTO 47
# define SARITHIF 48
# define SLOGIF 49
# define SIMPLICIT 50
# define SINQUIRE 51
# define SINTEGER 52
# define SINTRINSIC 53
# define SLOGICAL 54
# define SNAMELIST 55
# define SOPEN 56
# define SPARAM 57
# define SPAUSE 58
# define SPRINT 59
# define SPROGRAM 60
# define SPUNCH 61
# define SREAD 62
# define SREAL 63
# define SRETURN 64
# define SREWIND 65
# define SSAVE 66
# define SSTATIC 67
# define SSTOP 68
# define SSUBROUTINE 69
# define STHEN 70
# define STO 71
# define SUNDEFINED 72
# define SWRITE 73
# define SLPAR 74
# define SRPAR 75
# define SEQUALS 76
# define SCOLON 77
# define SCOMMA 78
# define SCURRENCY 79
# define SPLUS 80
# define SMINUS 81
# define SSTAR 82
# define SSLASH 83
# define SPOWER 84
# define SCONCAT 85
# define SAND 86
# define SOR 87
# define SNEQV 88
# define SEQV 89
# define SNOT 90
# define SEQ 91
# define SLT 92
# define SGT 93
# define SLE 94
# define SGE 95
# define SNE 96
# define SENDDO 97
# define SWHILE 98
# define SSLASHD 99
# define SBYTE 100

/* # line 125 "gram.in" */
#include "defs.h"
#include "p1defs.h"

static int nstars;			/* Number of labels in an
					   alternate return CALL */
static int datagripe;
static int ndim;
static int vartype;
int new_dcl;
static ftnint varleng;
static struct Dims dims[MAXDIM+1];
extern struct Labelblock **labarray;	/* Labels in an alternate
						   return CALL */
extern int maxlablist;

/* The next two variables are used to verify that each statement might be reached
   during runtime.   lastwasbranch   is tested only in the defintion of the
   stat:   nonterminal. */

int lastwasbranch = NO;
static int thiswasbranch = NO;
extern ftnint yystno;
extern flag intonly;
static chainp datastack;
extern long laststfcn, thisstno;
extern int can_include;	/* for netlib */

#define ESNULL (Extsym *)0
#define NPNULL (Namep)0
#define LBNULL (struct Listblock *)0

 static void
pop_datastack(Void) {
	chainp d0 = datastack;
	if (d0->datap)
		curdtp = (chainp)d0->datap;
	datastack = d0->nextp;
	d0->nextp = 0;
	frchain(&d0);
	}


/* # line 170 "gram.in" */
typedef union 	{
	int ival;
	ftnint lval;
	char *charpval;
	chainp chval;
	tagptr tagval;
	expptr expval;
	struct Labelblock *labval;
	struct Nameblock *namval;
	struct Eqvchain *eqvval;
	Extsym *extval;
	} YYSTYPE;
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
typedef int yytabelem;
extern yytabelem yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256
yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 20,
	1, 38,
	-2, 229,
-1, 24,
	1, 42,
	-2, 229,
-1, 123,
	6, 241,
	-2, 229,
-1, 151,
	1, 245,
	-2, 189,
-1, 175,
	1, 266,
	78, 266,
	-2, 189,
-1, 224,
	77, 174,
	-2, 140,
-1, 246,
	74, 229,
	-2, 226,
-1, 272,
	1, 287,
	-2, 144,
-1, 276,
	1, 296,
	78, 296,
	-2, 146,
-1, 329,
	77, 175,
	-2, 142,
-1, 359,
	1, 268,
	14, 268,
	74, 268,
	78, 268,
	-2, 190,
-1, 437,
	91, 0,
	92, 0,
	93, 0,
	94, 0,
	95, 0,
	96, 0,
	-2, 154,
-1, 454,
	1, 290,
	78, 290,
	-2, 144,
-1, 456,
	1, 292,
	78, 292,
	-2, 144,
-1, 458,
	1, 294,
	78, 294,
	-2, 144,
-1, 460,
	1, 297,
	78, 297,
	-2, 145,
-1, 505,
	78, 290,
	-2, 144,
	};
# define YYNPROD 302
# define YYLAST 1385
yytabelem yyact[]={

 238, 275, 472, 318, 317, 413, 421, 298, 471, 305,
 414, 398, 387, 358, 129, 267, 357, 327, 293, 400,
 253, 202, 296, 230, 399, 223, 304, 100,   5, 117,
  17, 265, 185, 204, 314, 274, 200, 211, 104, 271,
 119, 337, 203, 184, 261, 121, 237, 120, 107, 235,
 196, 102, 112, 122, 397, 513, 104, 105, 166, 167,
 335, 336, 337, 343, 342, 341, 340, 339, 191, 344,
 346, 345, 348, 347, 349, 396, 312, 158, 106, 158,
 310, 273, 166, 167, 259, 260, 261, 262, 115, 130,
 279, 397, 131, 132, 133, 134, 536, 136, 522, 476,
 484, 138, 466, 166, 167, 335, 336, 337, 343, 342,
 341, 340, 339, 540, 344, 346, 345, 348, 347, 349,
 103,  95, 319, 156, 295, 156, 335, 336, 337, 118,
 187, 188, 259, 260, 261, 104,  96,  97,  98, 526,
 186, 523, 410, 101, 231, 241, 241, 409, 195, 194,
 528, 212, 485, 447, 481, 158, 463, 220, 258, 158,
 244, 240, 242, 166, 167, 335, 336, 337, 214,  99,
 221, 219, 216, 158, 166, 167, 335, 336, 337, 343,
 342, 341, 215, 104, 158, 344, 346, 345, 348, 347,
 349, 166, 167, 335, 336, 337, 343, 103, 103, 103,
 103, 156, 190, 101, 121, 156, 120, 193, 462, 104,
 197, 198, 199, 461, 372, 321, 322, 207, 278, 156,
 301, 189, 289, 300, 325, 316, 329, 276, 276, 331,
 156, 453, 334, 197, 217, 218, 351, 309, 311, 444,
 353, 354, 334, 269, 290, 355, 350, 266, 429, 246,
 258, 248, 482, 158, 251, 483, 287, 288, 363, 158,
 158, 158, 158, 158, 258, 258, 469, 324, 378, 470,
 291, 280, 281, 282, 356, 464, 334, 295, 465, 379,
 451,   4, 377, 428, 258, 166, 167, 259, 260, 261,
 262, 103, 233, 448, 294, 376, 447, 270, 373, 156,
 401, 374, 375, 208, 178, 156, 156, 156, 156, 156,
 118, 393, 315, 382, 320, 113, 418, 407, 197, 419,
 408, 391, 334, 334, 392, 351, 334, 276, 111, 425,
 334, 151, 334, 175, 411, 231, 433, 434, 435, 436,
 437, 438, 439, 440, 441, 442, 404, 157, 110, 402,
 360, 406, 334, 361, 334, 334, 334, 432, 389, 381,
 109, 108, 158, 258, 334, 423, 420, 258, 258, 258,
 258, 258, 388, 236, 383, 384, 332, 495, 222, 333,
 334, 224, 449, 423, 371, 537, 533, 532, 452, 166,
 167, 259, 260, 261, 262, 531, 527, 473, 401, 530,
 525, 403, 197, 166, 167, 259, 260, 261, 156, 475,
 493, 468, 417, 467, 427, 104, 297, 446, 422, 137,
 480, 477, 487, 104, 489, 491, 276, 276, 276, 150,
 241, 497, 430, 385, 334, 334, 334, 334, 334, 334,
 334, 334, 334, 334, 404, 498, 496, 402, 404, 488,
 155, 258, 155, 494, 486, 192, 308, 503, 455, 457,
 459, 501, 286, 500, 247, 506, 507, 508, 243, 452,
 272, 272, 473, 227, 334, 307, 502, 401, 509, 512,
 224, 201, 511, 269, 213, 517, 210, 516, 334, 518,
 334, 480, 334, 521, 294, 519, 515, 241, 334, 403,
 524, 514, 249, 403, 529, 172, 263, 171, 173, 177,
 142, 276, 276, 276, 330, 535, 534, 492,  30, 104,
 264, 315, 415, 404, 510, 538, 402, 352, 155, 388,
 334, 283, 155, 209, 245, 226, 334,  93,   6, 334,
 539, 250,  82, 455, 457, 459, 155, 268,  81,  80,
  79, 174, 124,  78, 541, 504,  77, 155,  76,  60,
  49,  48,  45, 504, 504, 504,  33, 114, 206, 412,
 380, 205, 395, 394, 299, 303, 479, 306, 403, 135,
 390, 313, 116, 504,  26,  25, 359, 520,  24,  23,
 320, 166, 167, 259, 260, 261, 262,  22,  21, 386,
 362, 285,   9,   8,   7,   2, 366, 367, 368, 369,
 370, 302,  20, 165,  51, 490, 352, 292, 229, 328,
 326, 416,  92, 256,  53, 338, 155,  19,  55,  37,
 225,   3, 155, 155, 155, 155, 155,   1,   0,   0,
 268, 460,   0, 268, 268,   0, 166, 167, 335, 336,
 337, 343, 342, 341, 340, 339,   0, 344, 346, 345,
 348, 347, 349, 166, 167, 335, 336, 337, 343, 454,
 456, 458,   0,   0, 344, 346, 345, 348, 347, 349,
   0, 306,   0, 445,   0,   0,   0,   0, 166, 167,
 335, 336, 337, 343, 342, 341, 340, 339, 352, 344,
 346, 345, 348, 347, 349, 443,   0,   0,   0, 450,
 166, 167, 335, 336, 337, 343, 342, 341, 340, 339,
   0, 344, 346, 345, 348, 347, 349, 166, 167, 335,
 336, 337, 343, 342,   0, 155,   0, 499, 344, 346,
 345, 348, 347, 349,   0,   0, 268,   0,   0,   0,
   0,   0, 431,   0, 505, 456, 458, 166, 167, 335,
 336, 337, 343, 342, 341, 340, 339,   0, 344, 346,
 345, 348, 347, 349,   0,   0,   0,   0,   0,   0,
 424,   0, 478,   0, 306, 166, 167, 335, 336, 337,
 343, 342, 341, 340, 339,   0, 344, 346, 345, 348,
 347, 349, 166, 167, 335, 336, 337, 343, 342, 341,
 340, 339,   0, 344, 346, 345, 348, 347, 349,   0,
   0,   0,   0,   0,   0,   0,   0, 268, 161, 162,
 163, 164, 170, 169, 168, 159, 160, 104,   0,   0,
   0,   0,  12,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0, 306,  10,  56,  46,  73,
  86,  14,  61,  70,  91,  38,  66,  47,  42,  68,
  72,  31,  67,  35,  34,  11,  88,  36,  18,  41,
  39,  28,  16,  57,  58,  59,  50,  54,  43,  89,
  64,  40,  69,  44,  90,  29,  62,  85,  13,   0,
  83,  65,  52,  87,  27,  74,  63,  15,  73,   0,
  71,  84,  70,   0,   0,  66,   0,   0,  68,  72,
   0,  67, 161, 162, 163, 164, 170, 169, 168, 159,
 160, 104,   0,   0,   0,  32,   0,   0,  75,  64,
   0,  69,   0,   0,   0,   0,   0,   0,   0,   0,
  65,   0,   0,   0,  74,   0,   0,   0,   0,  71,
 161, 162, 163, 164, 170, 169, 168, 159, 160, 104,
   0, 161, 162, 163, 164, 170, 169, 168, 159, 160,
 104,   0,   0,   0,   0,   0,   0,  75,   0,   0,
   0, 234,   0,   0,   0,   0,   0, 166, 167, 364,
   0, 365,   0,   0,   0,   0,   0, 239, 161, 162,
 163, 164, 170, 169, 168, 159, 160, 104,   0, 161,
 162, 163, 164, 170, 169, 168, 159, 160, 104, 234,
 228,   0,   0,   0,   0, 166, 167, 232,   0,   0,
 234,   0,   0,   0,   0, 239, 166, 167, 474,   0,
   0,   0,   0,   0,  94,   0, 239, 161, 162, 163,
 164, 170, 169, 168, 159, 160, 104,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 234,   0,   0,
   0,   0,   0, 166, 167, 232, 123,   0, 234, 126,
 127, 128,   0, 239, 166, 167, 426,   0,   0,   0,
   0, 139, 140,   0, 239, 141,   0, 143, 144, 145,
   0,   0, 146, 147, 148,   0, 149,   0,   0,   0,
   0,   0,   0,   0,   0,   0, 234,   0,   0,   0,
   0,   0, 166, 167,   0,   0, 179, 180, 181, 182,
 183,   0, 239, 161, 162, 163, 164, 170, 169, 168,
 159, 160, 104,   0, 161, 162, 163, 164, 170, 169,
 168, 159, 160, 104,   0, 161, 162, 163, 164, 170,
 169, 168, 159, 160, 104, 257, 161, 162, 163, 164,
 170, 169, 168, 159, 160, 104,   0, 161, 162, 163,
 164, 170, 169, 168, 159, 160, 104, 161, 162, 163,
 164, 170, 169, 168, 159, 160, 104,   0,   0,   0,
   0,   0, 277,   0,   0,   0,   0,   0, 166, 167,
   0,   0,   0, 323,   0,   0,   0,   0, 239, 166,
 167,   0,   0,   0, 252,   0,   0,   0,   0, 239,
 166, 167, 254,   0, 255, 154,   0,   0,   0,   0,
   0, 166, 167, 152,   0, 153, 252,   0,   0,   0,
   0,   0, 166, 167, 284,   0, 154,   0,   0,   0,
   0,   0, 166, 167, 176, 161, 162, 163, 164, 170,
 169, 168, 159, 160, 104, 161, 162, 163, 164, 170,
 169, 168, 159, 160, 104,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
  56,  46,   0,  86,   0,  61,   0,  91,   0,   0,
  47,   0,   0,   0, 405,   0,   0,   0,   0,  88,
 166, 167,   0,   0, 252,   0,  57,  58,  59,  50,
 166, 167,  89,   0,   0,   0,   0,  90,   0,  62,
  85,   0,   0,  83,   0,  52,  87,   0,   0,  63,
   0, 125,   0,   0,  84 };
yytabelem yypact[]={

-1000,  25, 537, 838,-1000,-1000,-1000,-1000,-1000,-1000,
 532,-1000,-1000,-1000,-1000,-1000,-1000, 125, 505, -21,
 283, 282, 270, 250,  65, 237,   5, 121,-1000,-1000,
-1000,-1000,-1000,1311,-1000,-1000,-1000,   7,-1000,-1000,
-1000,-1000,-1000,-1000,-1000, 505,-1000,-1000,-1000,-1000,
-1000, 436,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,1171, 431,1192, 431,
 226,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000, 505, 505, 505, 505,-1000,
 505,-1000, 381,-1000,-1000, 505,-1000, -38, 505, 505,
 505, 407,-1000,-1000,-1000, 505, 225,-1000,-1000,-1000,
-1000, 519, 412,  65,-1000,-1000, 410,-1000,-1000,-1000,
-1000, 121, 505, 505, 407,-1000,-1000, 302, 406, 529,
-1000, 399, 955,1052,1052, 394, 528, 505, 390, 505,
-1000,-1000,-1000,-1000,1160,-1000,-1000,   2,1280,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,1160, 169, 219,-1000,-1000,1138,1138,-1000,
-1000,-1000,-1000,1182, 388,-1000,-1000, 381, 381, 505,
-1000,-1000, 195, 342,-1000,  65,-1000, 342,-1000,-1000,
-1000, 505,-1000, 401,-1000, 382, 887,  -3, 121,  -7,
 505, 528,  24,1052,1149,-1000, 505,-1000,-1000,-1000,
-1000,-1000,1052,-1000,1052, 443,-1000,1052,-1000, 301,
-1000, 722, 528,-1000,1052,-1000,-1000,-1000,1052,1052,
-1000, 722,-1000,1052,-1000,-1000,  65, 528,-1000, 511,
 275,-1000,1280,-1000,-1000,-1000, 917,-1000,1280,1280,
1280,1280,1280, -40, 309, 136, 409,-1000,-1000, 409,
 409,-1000, 217, 204, 190, 722,-1000,1138,-1000,-1000,
-1000,-1000,-1000,   2,-1000,-1000, 358,-1000,-1000, 381,
-1000,-1000, 246,-1000,-1000,-1000,   7,-1000, -24,1270,
 505,-1000, 242,-1000,  64,-1000,-1000, 401, 508,-1000,
 505,-1000,-1000, 241,-1000, 290,-1000,-1000,-1000, 344,
 289, 705, 722,1014,-1000, 722, 339, 206, 170, 722,
 505, 677,-1000,1003,1052,1052,1052,1052,1052,1052,
1052,1052,1052,1052,-1000,-1000,-1000,-1000,-1000,-1000,
-1000, 630, 161, -43, 583, 608, 343, 218,-1000,-1000,
-1000,1160, 205, 722,-1000,-1000,  50, -40, -40, -40,
 323,-1000, 409, 136, 153, 136,1138,1138,1138, 566,
 135, 130,  78,-1000,-1000,-1000, 200,-1000,  26,-1000,
 342,-1000,  42,-1000, 191, 966,-1000,1270,-1000,-1000,
  17, 823,-1000,-1000,-1000,1052,-1000,-1000, 505,-1000,
 401,  76, 177,-1000,  19,-1000,  74,-1000,-1000, 505,
1052,  65,1052,1052, 447,-1000, 335, 303,1052,1052,
-1000, 528,-1000,  44, -43, -43, -43, 111,  94,  94,
 647, 583,  83,-1000,1052,-1000, 528, 528,  65,-1000,
   2,-1000,-1000, 409,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,1138,1138,1138,-1000, 515, 510,   7,-1000,-1000,
 966,-1000,-1000, -22,-1000,-1000,1270,-1000,-1000,-1000,
-1000, 401,-1000, 508, 508, 505,-1000, 722,  24,  23,
  63, 722,-1000,-1000,-1000,1052, 325, 722,  61, 321,
  75,-1000,1052, 324, 307, 321, 320, 312, 311,-1000,
-1000,-1000,-1000, 966,-1000,-1000,  13, 310,-1000,-1000,
-1000,-1000,-1000,1052,-1000,-1000, 528,-1000,-1000, 722,
-1000,-1000,-1000,-1000,-1000, 722,-1000,-1000, 722,  35,
 528,-1000 };
yytabelem yypgo[]={

   0, 637, 631,  13, 630,  81,  15,  30, 629, 628,
 627,  10,   0, 625, 624, 623,  14, 622,   9,  26,
 621, 620, 619,   3,   4, 618,  68, 617, 615,  31,
  39,  35,  25, 101,  18, 614,  50, 373,   1, 292,
  17, 347, 254,   2,  19,  24,  23,  49,  46, 613,
 612,  40,  32,  43, 611, 605, 604, 603, 602,1054,
 121, 601, 599,  12, 598, 597, 589, 588, 585, 584,
 582,  53, 581,  27, 580,  22,  42,   7,  37,   6,
  36, 579,  21, 576, 574,  11,  29,  34, 573, 572,
   8,  16,  33, 571, 569, 568,   5, 567, 518, 566,
 562, 561, 560, 559, 558, 429, 556, 553, 551, 550,
 549, 548,  90, 542, 541,  20 };
yytabelem yyr1[]={

   0,   1,   1,  55,  55,  55,  55,  55,  55,  55,
   2,  56,  56,  56,  56,  56,  56,  56,  60,  52,
  33,  53,  53,  61,  61,  62,  62,  63,  63,  26,
  26,  26,  27,  27,  34,  34,  17,  57,  57,  57,
  57,  57,  57,  57,  57,  57,  57,  57,  57,  10,
  10,  10,  74,   7,   8,   9,   9,   9,   9,   9,
   9,   9,   9,   9,   9,   9,   9,  16,  16,  16,
  50,  50,  50,  50,  51,  51,  64,  64,  65,  65,
  66,  66,  80,  54,  54,  67,  67,  81,  82,  76,
  83,  84,  77,  77,  85,  85,  45,  45,  45,  70,
  70,  86,  86,  72,  72,  87,  36,  18,  18,  19,
  19,  75,  75,  89,  88,  88,  90,  90,  43,  43,
  91,  91,   3,  68,  68,  92,  92,  95,  93,  94,
  94,  96,  96,  11,  69,  69,  97,  20,  20,  71,
  21,  21,  22,  22,  38,  38,  38,  39,  39,  39,
  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,
  39,  12,  12,  13,  13,  13,  13,  13,  13,  37,
  37,  37,  37,  32,  40,  40,  44,  44,  48,  48,
  48,  48,  48,  48,  48,  47,  49,  49,  49,  41,
  41,  42,  42,  42,  42,  42,  42,  42,  42,  58,
  58,  58,  58,  58,  58,  58,  58,  58,  99,  23,
  24,  24,  98,  98,  98,  98,  98,  98,  98,  98,
  98,  98,  98,   4, 100, 101, 101, 101, 101,  73,
  73,  35,  25,  25,  46,  46,  14,  14,  28,  28,
  59,  78,  79, 102, 103, 103, 103, 103, 103, 103,
 103, 103, 103, 103, 103, 103, 103, 103, 104, 111,
 111, 111, 106, 113, 113, 113, 108, 108, 105, 105,
 114, 114, 115, 115, 115, 115, 115, 115,  15, 107,
 109, 110, 110,  29,  29,   6,   6,  30,  30,  30,
  31,  31,  31,  31,  31,  31,   5,   5,   5,   5,
   5, 112 };
yytabelem yyr2[]={

   0,   0,   3,   2,   2,   2,   3,   3,   2,   1,
   1,   3,   4,   3,   4,   4,   5,   3,   0,   1,
   1,   0,   1,   2,   3,   1,   3,   1,   3,   0,
   2,   3,   1,   3,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   2,   1,   5,   7,
   5,   5,   0,   2,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   0,   4,   6,
   3,   4,   5,   3,   1,   3,   3,   3,   3,   3,
   3,   3,   3,   1,   3,   3,   3,   0,   6,   0,
   0,   0,   2,   3,   1,   3,   1,   2,   1,   1,
   3,   1,   1,   1,   3,   3,   2,   1,   5,   1,
   3,   0,   3,   0,   2,   3,   1,   3,   1,   1,
   1,   3,   1,   3,   3,   4,   1,   0,   2,   1,
   3,   1,   3,   1,   1,   2,   4,   1,   3,   0,
   0,   1,   1,   3,   1,   3,   1,   1,   1,   3,
   3,   3,   3,   2,   3,   3,   3,   3,   3,   2,
   3,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   2,   4,   5,   5,   0,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   5,   1,   1,   1,   1,
   3,   1,   1,   3,   3,   3,   3,   2,   3,   1,
   7,   4,   1,   2,   2,   6,   2,   2,   5,   3,
   1,   4,   4,   5,   2,   1,   1,  10,   1,   3,
   4,   3,   3,   1,   1,   3,   3,   7,   7,   0,
   1,   3,   1,   3,   1,   2,   1,   1,   1,   3,
   0,   0,   0,   1,   2,   2,   2,   2,   2,   2,
   2,   3,   4,   4,   2,   3,   1,   3,   3,   1,
   1,   1,   3,   1,   1,   1,   1,   1,   3,   3,
   1,   3,   1,   1,   1,   2,   2,   2,   1,   3,
   3,   4,   4,   1,   3,   1,   5,   1,   1,   1,
   3,   3,   3,   3,   3,   3,   1,   3,   5,   5,
   5,   0 };
yytabelem yychk[]={

-1000,  -1, -55,  -2, 256,   3,   1, -56, -57, -58,
  18,  37,   4,  60,  23,  69,  44,  -7,  40, -10,
 -50, -64, -65, -66, -67, -68, -69,  66,  43,  57,
 -98,  33,  97, -99,  36,  35,  39,  -8,  27,  42,
  53,  41,  30,  50,  55,-100,  20,  29,-101,-102,
  48, -35,  64, -14,  49,  -9,  19,  45,  46,  47,
-103,  24,  58,  68,  52,  63,  28,  34,  31,  54,
  25,  72,  32,  21,  67, 100,-104,-106,-107,-109,
-110,-111,-113,  62,  73,  59,  22,  65,  38,  51,
  56,  26, -17,   5, -59, -60, -60, -60, -60,  44,
 -73,  78, -52, -33,  14,  78,  99, -73,  78,  78,
  78,  78, -73,  78, -97,  83, -70, -86, -33, -51,
  85,  83, -71, -59, -98,  70, -59, -59, -59, -16,
  82, -71, -71, -71, -71, -81, -71, -37, -33, -59,
 -59, -59,  74, -59, -59, -59, -59, -59, -59, -59,
-105, -42,  82,  84,  74, -37, -48, -41, -12,  12,
  13,   5,   6,   7,   8, -49,  80,  81,  11,  10,
   9,-105,  74,-105,-108, -42,  82,-105,  78, -59,
 -59, -59, -59, -59, -53, -52, -53, -52, -52, -60,
 -33, -26,  74, -33, -76, -51, -36, -33, -33, -33,
 -80,  74, -82, -76, -92, -93, -95, -33,  78,  14,
  74, -78, -73,  74, -78, -36, -51, -33, -33, -80,
 -82, -92,  76, -32,  74,  -4,   6,  74,  75, -25,
 -46, -38,  82, -39,  74, -47, -37, -48, -12,  90,
 -40, -38, -40,  74,  -3,   6, -33,  74, -33, -41,
-114, -42,  74,-115,  82,  84, -15,  15, -12,  82,
  83,  84,  85, -41, -41, -29,  78,  -6, -37,  74,
  78, -30, -39,  -5, -31, -38, -47,  74, -30,-112,
-112,-112,-112, -41,  82, -61,  74, -26, -26, -52,
 -71,  75, -27, -34, -33,  82, -75,  74, -77, -84,
 -73, -75, -54, -37, -19, -18, -37,  74,  74,  -7,
  83, -86,  83, -72, -87, -33,  -3, -24, -23,  98,
 -33, -38, -38,  74, -36, -38, -21, -40, -22, -38,
  71, -38,  75,  78, -12,  82,  83,  84, -13,  89,
  88,  87,  86,  85,  91,  93,  92,  95,  94,  96,
  -3, -38, -39, -38, -38, -38, -73, -91,  -3,  75,
  75,  78, -41, -38,  82,  84, -41, -41, -41, -41,
 -41,  75,  78, -29, -29, -29,  78,  78,  78, -38,
 -39,  -5, -31,-112,-112,  75, -62, -63,  14, -26,
 -74,  75,  78, -16, -88, -89,  99,  78, -85, -45,
 -44, -12, -47, -33, -48,  74, -36,  75,  78,  83,
  78, -19, -94, -96, -11,  14, -20, -33,  75,  78,
  76, -79,  74,  76,  75, -79,  82,  75,  77,  78,
 -33,  75, -46, -38, -38, -38, -38, -38, -38, -38,
 -38, -38, -38,  75,  78,  75,  74,  78,  75,-115,
 -41,  75,  -6,  78, -39,  -5, -39,  -5, -39,  -5,
  75,  78,  78,  78,  75,  78,  76, -75, -34,  75,
  78, -90, -43, -38,  82, -85,  82, -44, -37, -83,
 -18,  78,  75,  78,  81,  78, -87, -38, -73, -38,
 -28, -38,  70,  75, -32,  74, -40, -38,  -3, -39,
 -91,  -3, -73, -23, -33, -39, -23, -23, -23, -63,
  14, -16, -90,  77, -45, -44, -77, -23, -96, -11,
 -33, -24,  75,  78, -79,  75,  78,  75,  75, -38,
  75,  75,  75,  75, -43, -38,  83,  75, -38,  -3,
  78,  -3 };
yytabelem yydef[]={

   1,  -2,   0,   0,   9,  10,   2,   3,   4,   5,
   0, 240,   8,  18,  18,  18,  18, 229,   0,  37,
  -2,  39,  40,  41,  -2,  43,  44,  45,  47, 139,
 199, 240, 202,   0, 240, 240, 240,  67, 139, 139,
 139, 139,  87, 139, 134,   0, 240, 240, 215, 216,
 240, 218, 240, 240, 240,  54, 224, 240, 240, 240,
 243, 240, 236, 237,  55,  56,  57,  58,  59,  60,
  61,  62,  63,  64,  65,  66,   0,   0,   0,   0,
 256, 240, 240, 240, 240, 240, 259, 260, 261, 263,
 264, 265,   6,  36,   7,  21,  21,   0,   0,  18,
   0, 230,  29,  19,  20,   0,  89,   0, 230,   0,
   0,   0,  89, 127, 135,   0,  46,  99, 101, 102,
  74,   0,   0,  -2, 203, 204,   0, 206, 207,  53,
 241,   0,   0,   0,   0,  89, 127,   0, 169,   0,
 214,   0,   0, 174, 174,   0,   0,   0,   0,   0,
 244,  -2, 246, 247,   0, 191, 192,   0,   0, 178,
 179, 180, 181, 182, 183, 184, 161, 162, 186, 187,
 188, 248,   0, 249, 250,  -2, 267, 254,   0, 301,
 301, 301, 301,   0,  11,  22,  13,  29,  29,   0,
 139,  17,   0, 111,  91, 229,  73, 111,  77,  79,
  81,   0,  86,   0, 124, 126,   0,   0,   0,   0,
   0,   0,   0,   0,   0,  70,   0,  76,  78,  80,
  85, 123,   0, 170,  -2,   0, 223,   0, 219,   0,
 232, 234,   0, 144,   0, 146, 147, 148,   0,   0,
 221, 175, 222,   0, 225, 122,  -2,   0, 231, 272,
   0, 189,   0, 270, 273, 274,   0, 278,   0,   0,
   0,   0,   0, 197, 272, 251,   0, 283, 285,   0,
   0, 255,  -2, 288, 289,   0,  -2,   0, 257, 258,
 262, 279, 280, 301, 301,  12,   0,  14,  15,  29,
  52,  30,   0,  32,  34,  35,  67, 113,   0,   0,
   0, 106,   0,  83,   0, 109, 107,   0,   0, 128,
   0, 100,  75,   0, 103,   0, 242, 201, 210,   0,
   0,   0, 242,   0,  71, 212,   0,   0, 141,  -2,
   0,   0, 220,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0, 163, 164, 165, 166, 167, 168,
 235,   0, 144, 153, 159,   0,   0,   0, 120,  -2,
 269,   0,   0, 275, 276, 277, 193, 194, 195, 196,
 198, 268,   0, 253,   0, 252,   0,   0,   0,   0,
 144,   0,   0, 281, 282,  23,   0,  25,  27,  16,
 111,  31,   0,  50,   0,   0,  51,   0,  92,  94,
  96,   0,  98, 176, 177,   0,  72,  82,   0,  90,
   0,   0,   0, 129, 131, 133, 136, 137,  48,   0,
   0, 229,   0,   0,   0,  68,   0, 171, 174,   0,
 213,   0, 233, 149, 150, 151, 152,  -2, 155, 156,
 157, 158, 160, 145,   0, 208,   0,   0, 229, 271,
 272, 190, 284,   0,  -2, 291,  -2, 293,  -2, 295,
  -2,   0,   0,   0,  24,   0,   0,  67,  33, 112,
   0, 114, 116, 119, 118,  93,   0,  97,  84,  91,
 110,   0, 125,   0,   0,   0, 104, 105,   0,   0,
 209, 238, 205, 242, 172, 174,   0, 143,   0, 144,
   0, 121,   0,   0, 169,  -2,   0,   0,   0,  26,
  28,  49, 115,   0,  95,  96,   0,   0, 130, 132,
 138, 200, 211,   0,  69, 173,   0, 185, 227, 228,
 286, 298, 299, 300, 117, 119,  88, 108, 239,   0,
   0, 217 };
# ifdef YYDEBUG
# include "y.debug"
# endif

# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
yytabelem yyerrflag = 0;  /* error recovery flag */

yyparse()
{	yytabelem yys[YYMAXDEPTH];
	int yyj, yym;
	register YYSTYPE *yypvt;
	register int yystate, yyn;
	register yytabelem *yyps;
	register YYSTYPE *yypv;
	register yytabelem *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

yystack:    /* put a state and value onto the stack */
#ifdef YYDEBUG
	if(yydebug >= 3)
		if(yychar < 0 || yytoknames[yychar] == 0)
			printf("char %d in %s", yychar, yystates[yystate]);
		else
			printf("%s in %s", yytoknames[yychar], yystates[yystate]);
#endif
	if( ++yyps >= &yys[YYMAXDEPTH] ) {
		yyerror( "yacc stack overflow" );
		return(1);
	}
	*yyps = yystate;
	++yypv;
	*yypv = yyval;
yynewstate:
	yyn = yypact[yystate];
	if(yyn <= YYFLAG) goto yydefault; /* simple state */
	if(yychar<0) {
		yychar = yylex();
#ifdef YYDEBUG
		if(yydebug >= 2) {
			if(yychar <= 0)
				printf("lex EOF\n");
			else if(yytoknames[yychar])
				printf("lex %s\n", yytoknames[yychar]);
			else
				printf("lex (%c)\n", yychar);
		}
#endif
		if(yychar < 0)
			yychar = 0;
	}
	if((yyn += yychar) < 0 || yyn >= YYLAST)
		goto yydefault;
	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
	}
yydefault:
	/* default state action */
	if( (yyn=yydef[yystate]) == -2 ) {
		if(yychar < 0) {
			yychar = yylex();
#ifdef YYDEBUG
			if(yydebug >= 2)
				if(yychar < 0)
					printf("lex EOF\n");
				else
					printf("lex %s\n", yytoknames[yychar]);
#endif
			if(yychar < 0)
				yychar = 0;
		}
		/* look through exception table */
		for(yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate);
			yyxi += 2 ) ; /* VOID */
		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
		}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
	}
	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */
		switch( yyerrflag ){
		case 0:   /* brand new error */
#ifdef YYDEBUG
			yyerror("syntax error\n%s", yystates[yystate]);
			if(yytoknames[yychar])
				yyerror("saw %s\n", yytoknames[yychar]);
			else if(yychar >= ' ' && yychar < '\177')
				yyerror("saw `%c'\n", yychar);
			else if(yychar == 0)
				yyerror("saw EOF\n");
			else
				yyerror("saw char 0%o\n", yychar);
#else
			yyerror( "syntax error" );
#endif
yyerrlab:
			++yynerrs;
		case 1:
		case 2: /* incompletely recovered error ... try again */
			yyerrflag = 3;
			/* find a state where "error" is a legal shift action */
			while ( yyps >= yys ) {
				yyn = yypact[*yyps] + YYERRCODE;
				if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
					yystate = yyact[yyn];  /* simulate a shift of "error" */
					goto yystack;
				}
				yyn = yypact[*yyps];
				/* the current yyps has no shift onn "error", pop stack */
#ifdef YYDEBUG
				if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
				--yyps;
				--yypv;
			}
			/* there is no state on the stack with an error shift ... abort */
yyabort:
			return(1);
		case 3:  /* no shift yet; clobber input char */
#ifdef YYDEBUG
			if( yydebug ) {
				printf("error recovery discards ");
				if(yytoknames[yychar])
					printf("%s\n", yytoknames[yychar]);
				else if(yychar >= ' ' && yychar < '\177')
					printf("`%c'\n", yychar);
				else if(yychar == 0)
					printf("EOF\n");
				else
					printf("char 0%o\n", yychar);
			}
#endif
			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */
		}
	}
	/* reduction by production yyn */
#ifdef YYDEBUG
	if(yydebug) {	char *s;
		printf("reduce %d in:\n\t", yyn);
		for(s = yystates[yystate]; *s; s++) {
			putchar(*s);
			if(*s == '\n' && *(s+1))
				putchar('\t');
		}
	}
#endif
	yyps -= yyr2[yyn];
	yypvt = yypv;
	yypv -= yyr2[yyn];
	yyval = yypv[1];
	yym=yyn;
	/* consult goto table to find next state */
	yyn = yyr1[yyn];
	yyj = yypgo[yyn] + *yyps + 1;
	if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
	switch(yym){

case 3:
/* # line 218 "gram.in" */
{
/* stat:   is the nonterminal for Fortran statements */

		  lastwasbranch = NO; } break;
case 5:
/* # line 224 "gram.in" */
{ /* forbid further statement function definitions... */
		  if (parstate == INDATA && laststfcn != thisstno)
			parstate = INEXEC;
		  thisstno++;
		  if(yypvt[-1].labval && (yypvt[-1].labval->labelno==dorange))
			enddo(yypvt[-1].labval->labelno);
		  if(lastwasbranch && thislabel==NULL)
			warn("statement cannot be reached");
		  lastwasbranch = thiswasbranch;
		  thiswasbranch = NO;
		  if(yypvt[-1].labval)
			{
			if(yypvt[-1].labval->labtype == LABFORMAT)
				err("label already that of a format");
			else
				yypvt[-1].labval->labtype = LABEXEC;
			}
		  freetemps();
		} break;
case 6:
/* # line 244 "gram.in" */
{ if (can_include)
			doinclude( yypvt[-0].charpval );
		  else {
			fprintf(diagfile, "Cannot open file %s\n", yypvt[-0].charpval);
			done(1);
			}
		} break;
case 7:
/* # line 252 "gram.in" */
{ if (yypvt[-2].labval)
			lastwasbranch = NO;
		  endproc(); /* lastwasbranch = NO; -- set in endproc() */
		} break;
case 8:
/* # line 257 "gram.in" */
{ unclassifiable();

/* flline flushes the current line, ignoring the rest of the text there */

		  flline(); } break;
case 9:
/* # line 263 "gram.in" */
{ flline();  needkwd = NO;  inioctl = NO;
		  yyerrok; yyclearin; } break;
case 10:
/* # line 268 "gram.in" */
{
		if(yystno != 0)
			{
			yyval.labval = thislabel =  mklabel(yystno);
			if( ! headerdone ) {
				if (procclass == CLUNKNOWN)
					procclass = CLMAIN;
				puthead(CNULL, procclass);
				}
			if(thislabel->labdefined)
				execerr("label %s already defined",
					convic(thislabel->stateno) );
			else	{
				if(thislabel->blklevel!=0 && thislabel->blklevel<blklevel
				    && thislabel->labtype!=LABFORMAT)
					warn1("there is a branch to label %s from outside block",
					      convic( (ftnint) (thislabel->stateno) ) );
				thislabel->blklevel = blklevel;
				thislabel->labdefined = YES;
				if(thislabel->labtype != LABFORMAT)
					p1_label((long)(thislabel - labeltab));
				}
			}
		else    yyval.labval = thislabel = NULL;
		} break;
case 11:
/* # line 296 "gram.in" */
{startproc(yypvt[-0].extval, CLMAIN); } break;
case 12:
/* # line 298 "gram.in" */
{	warn("ignoring arguments to main program");
			/* hashclear(); */
			startproc(yypvt[-1].extval, CLMAIN); } break;
case 13:
/* # line 302 "gram.in" */
{ if(yypvt[-0].extval) NO66("named BLOCKDATA");
		  startproc(yypvt[-0].extval, CLBLOCK); } break;
case 14:
/* # line 305 "gram.in" */
{ entrypt(CLPROC, TYSUBR, (ftnint) 0,  yypvt[-1].extval, yypvt[-0].chval); } break;
case 15:
/* # line 307 "gram.in" */
{ entrypt(CLPROC, TYUNKNOWN, (ftnint) 0, yypvt[-1].extval, yypvt[-0].chval); } break;
case 16:
/* # line 309 "gram.in" */
{ entrypt(CLPROC, yypvt[-4].ival, varleng, yypvt[-1].extval, yypvt[-0].chval); } break;
case 17:
/* # line 311 "gram.in" */
{ if(parstate==OUTSIDE || procclass==CLMAIN
			|| procclass==CLBLOCK)
				execerr("misplaced entry statement", CNULL);
		  entrypt(CLENTRY, 0, (ftnint) 0, yypvt[-1].extval, yypvt[-0].chval);
		} break;
case 18:
/* # line 319 "gram.in" */
{ newproc(); } break;
case 19:
/* # line 323 "gram.in" */
{ yyval.extval = newentry(yypvt[-0].namval, 1); } break;
case 20:
/* # line 327 "gram.in" */
{ yyval.namval = mkname(token); } break;
case 21:
/* # line 330 "gram.in" */
{ yyval.extval = NULL; } break;
case 29:
/* # line 348 "gram.in" */
{ yyval.chval = 0; } break;
case 30:
/* # line 350 "gram.in" */
{ NO66(" () argument list");
		  yyval.chval = 0; } break;
case 31:
/* # line 353 "gram.in" */
{yyval.chval = yypvt[-1].chval; } break;
case 32:
/* # line 357 "gram.in" */
{ yyval.chval = (yypvt[-0].namval ? mkchain((char *)yypvt[-0].namval,CHNULL) : CHNULL ); } break;
case 33:
/* # line 359 "gram.in" */
{ if(yypvt[-0].namval) yypvt[-2].chval = yyval.chval = mkchain((char *)yypvt[-0].namval, yypvt[-2].chval); } break;
case 34:
/* # line 363 "gram.in" */
{ if(yypvt[-0].namval->vstg!=STGUNKNOWN && yypvt[-0].namval->vstg!=STGARG)
			dclerr("name declared as argument after use", yypvt[-0].namval);
		  yypvt[-0].namval->vstg = STGARG;
		} break;
case 35:
/* # line 368 "gram.in" */
{ NO66("altenate return argument");

/* substars   means that '*'ed formal parameters should be replaced.
   This is used to specify alternate return labels; in theory, only
   parameter slots which have '*' should accept the statement labels.
   This compiler chooses to ignore the '*'s in the formal declaration, and
   always return the proper value anyway.

   This variable is only referred to in   proc.c   */

		  yyval.namval = 0;  substars = YES; } break;
case 36:
/* # line 384 "gram.in" */
{
		char *s;
		s = copyn(toklen+1, token);
		s[toklen] = '\0';
		yyval.charpval = s;
		} break;
case 45:
/* # line 400 "gram.in" */
{ NO66("SAVE statement");
		  saveall = YES; } break;
case 46:
/* # line 403 "gram.in" */
{ NO66("SAVE statement"); } break;
case 47:
/* # line 405 "gram.in" */
{ fmtstmt(thislabel); setfmt(thislabel); } break;
case 48:
/* # line 407 "gram.in" */
{ NO66("PARAMETER statement"); } break;
case 49:
/* # line 411 "gram.in" */
{ settype(yypvt[-4].namval, yypvt[-6].ival, yypvt[-0].lval);
		  if(ndim>0) setbound(yypvt[-4].namval,ndim,dims);
		} break;
case 50:
/* # line 415 "gram.in" */
{ settype(yypvt[-2].namval, yypvt[-4].ival, yypvt[-0].lval);
		  if(ndim>0) setbound(yypvt[-2].namval,ndim,dims);
		} break;
case 51:
/* # line 419 "gram.in" */
{ if (new_dcl == 2) {
			err("attempt to give DATA in type-declaration");
			new_dcl = 1;
			}
		} break;
case 52:
/* # line 426 "gram.in" */
{ new_dcl = 2; } break;
case 53:
/* # line 429 "gram.in" */
{ varleng = yypvt[-0].lval; } break;
case 54:
/* # line 433 "gram.in" */
{ varleng = (yypvt[-0].ival<0 || ONEOF(yypvt[-0].ival,M(TYLOGICAL)|M(TYLONG))
				? 0 : typesize[yypvt[-0].ival]);
		  vartype = yypvt[-0].ival; } break;
case 55:
/* # line 438 "gram.in" */
{ yyval.ival = TYLONG; } break;
case 56:
/* # line 439 "gram.in" */
{ yyval.ival = tyreal; } break;
case 57:
/* # line 440 "gram.in" */
{ ++complex_seen; yyval.ival = tycomplex; } break;
case 58:
/* # line 441 "gram.in" */
{ yyval.ival = TYDREAL; } break;
case 59:
/* # line 442 "gram.in" */
{ ++dcomplex_seen; NOEXT("DOUBLE COMPLEX statement"); yyval.ival = TYDCOMPLEX; } break;
case 60:
/* # line 443 "gram.in" */
{ yyval.ival = TYLOGICAL; } break;
case 61:
/* # line 444 "gram.in" */
{ NO66("CHARACTER statement"); yyval.ival = TYCHAR; } break;
case 62:
/* # line 445 "gram.in" */
{ yyval.ival = TYUNKNOWN; } break;
case 63:
/* # line 446 "gram.in" */
{ yyval.ival = TYUNKNOWN; } break;
case 64:
/* # line 447 "gram.in" */
{ NOEXT("AUTOMATIC statement"); yyval.ival = - STGAUTO; } break;
case 65:
/* # line 448 "gram.in" */
{ NOEXT("STATIC statement"); yyval.ival = - STGBSS; } break;
case 66:
/* # line 449 "gram.in" */
{ yyval.ival = TYINT1; } break;
case 67:
/* # line 453 "gram.in" */
{ yyval.lval = varleng; } break;
case 68:
/* # line 455 "gram.in" */
{
		expptr p;
		p = yypvt[-1].expval;
		NO66("length specification *n");
		if( ! ISICON(p) || p->constblock.Const.ci <= 0 )
			{
			yyval.lval = 0;
			dclerr("length must be a positive integer constant",
				NPNULL);
			}
		else {
			if (vartype == TYCHAR)
				yyval.lval = p->constblock.Const.ci;
			else switch((int)p->constblock.Const.ci) {
				case 1:	yyval.lval = 1; break;
				case 2: yyval.lval = typesize[TYSHORT];	break;
				case 4: yyval.lval = typesize[TYLONG];	break;
				case 8: yyval.lval = typesize[TYDREAL];	break;
				case 16: yyval.lval = typesize[TYDCOMPLEX]; break;
				default:
					dclerr("invalid length",NPNULL);
					yyval.lval = varleng;
				}
			}
		} break;
case 69:
/* # line 481 "gram.in" */
{ NO66("length specification *(*)"); yyval.lval = -1; } break;
case 70:
/* # line 485 "gram.in" */
{ incomm( yyval.extval = comblock("") , yypvt[-0].namval ); } break;
case 71:
/* # line 487 "gram.in" */
{ yyval.extval = yypvt[-1].extval;  incomm(yypvt[-1].extval, yypvt[-0].namval); } break;
case 72:
/* # line 489 "gram.in" */
{ yyval.extval = yypvt[-2].extval;  incomm(yypvt[-2].extval, yypvt[-0].namval); } break;
case 73:
/* # line 491 "gram.in" */
{ incomm(yypvt[-2].extval, yypvt[-0].namval); } break;
case 74:
/* # line 495 "gram.in" */
{ yyval.extval = comblock(""); } break;
case 75:
/* # line 497 "gram.in" */
{ yyval.extval = comblock(token); } break;
case 76:
/* # line 501 "gram.in" */
{ setext(yypvt[-0].namval); } break;
case 77:
/* # line 503 "gram.in" */
{ setext(yypvt[-0].namval); } break;
case 78:
/* # line 507 "gram.in" */
{ NO66("INTRINSIC statement"); setintr(yypvt[-0].namval); } break;
case 79:
/* # line 509 "gram.in" */
{ setintr(yypvt[-0].namval); } break;
case 82:
/* # line 517 "gram.in" */
{
		struct Equivblock *p;
		if(nequiv >= maxequiv)
			many("equivalences", 'q', maxequiv);
		p  =  & eqvclass[nequiv++];
		p->eqvinit = NO;
		p->eqvbottom = 0;
		p->eqvtop = 0;
		p->equivs = yypvt[-1].eqvval;
		} break;
case 83:
/* # line 530 "gram.in" */
{ yyval.eqvval=ALLOC(Eqvchain);
		  yyval.eqvval->eqvitem.eqvlhs = (struct Primblock *)yypvt[-0].expval;
		} break;
case 84:
/* # line 534 "gram.in" */
{ yyval.eqvval=ALLOC(Eqvchain);
		  yyval.eqvval->eqvitem.eqvlhs = (struct Primblock *) yypvt[-0].expval;
		  yyval.eqvval->eqvnextp = yypvt[-2].eqvval;
		} break;
case 87:
/* # line 545 "gram.in" */
{ if(parstate == OUTSIDE)
			{
			newproc();
			startproc(ESNULL, CLMAIN);
			}
		  if(parstate < INDATA)
			{
			enddcl();
			parstate = INDATA;
			datagripe = 1;
			}
		} break;
case 88:
/* # line 560 "gram.in" */
{ ftnint junk;
		  if(nextdata(&junk) != NULL)
			err("too few initializers");
		  frdata(yypvt[-4].chval);
		  frrpl();
		} break;
case 89:
/* # line 568 "gram.in" */
{ frchain(&datastack); curdtp = 0; } break;
case 90:
/* # line 570 "gram.in" */
{ pop_datastack(); } break;
case 91:
/* # line 572 "gram.in" */
{ toomanyinit = NO; } break;
case 94:
/* # line 577 "gram.in" */
{ dataval(ENULL, yypvt[-0].expval); } break;
case 95:
/* # line 579 "gram.in" */
{ dataval(yypvt[-2].expval, yypvt[-0].expval); } break;
case 97:
/* # line 584 "gram.in" */
{ if( yypvt[-1].ival==OPMINUS && ISCONST(yypvt[-0].expval) )
			consnegop((Constp)yypvt[-0].expval);
		  yyval.expval = yypvt[-0].expval;
		} break;
case 101:
/* # line 596 "gram.in" */
{ int k;
		  yypvt[-0].namval->vsave = YES;
		  k = yypvt[-0].namval->vstg;
		if( ! ONEOF(k, M(STGUNKNOWN)|M(STGBSS)|M(STGINIT)) )
			dclerr("can only save static variables", yypvt[-0].namval);
		} break;
case 105:
/* # line 610 "gram.in" */
{ if(yypvt[-2].namval->vclass == CLUNKNOWN)
			make_param((struct Paramblock *)yypvt[-2].namval, yypvt[-0].expval);
		  else dclerr("cannot make into parameter", yypvt[-2].namval);
		} break;
case 106:
/* # line 617 "gram.in" */
{ if(ndim>0) setbound(yypvt[-1].namval, ndim, dims); } break;
case 107:
/* # line 621 "gram.in" */
{ Namep np;
		  np = ( (struct Primblock *) yypvt[-0].expval) -> namep;
		  vardcl(np);
		  if(np->vstg == STGCOMMON)
			extsymtab[np->vardesc.varno].extinit = YES;
		  else if(np->vstg==STGEQUIV)
			eqvclass[np->vardesc.varno].eqvinit = YES;
		  else if(np->vstg!=STGINIT && np->vstg!=STGBSS)
			dclerr("inconsistent storage classes", np);
		  yyval.chval = mkchain((char *)yypvt[-0].expval, CHNULL);
		} break;
case 108:
/* # line 633 "gram.in" */
{ chainp p; struct Impldoblock *q;
		pop_datastack();
		q = ALLOC(Impldoblock);
		q->tag = TIMPLDO;
		(q->varnp = (Namep) (yypvt[-1].chval->datap))->vimpldovar = 1;
		p = yypvt[-1].chval->nextp;
		if(p)  { q->implb = (expptr)(p->datap); p = p->nextp; }
		if(p)  { q->impub = (expptr)(p->datap); p = p->nextp; }
		if(p)  { q->impstep = (expptr)(p->datap); }
		frchain( & (yypvt[-1].chval) );
		yyval.chval = mkchain((char *)q, CHNULL);
		q->datalist = hookup(yypvt[-3].chval, yyval.chval);
		} break;
case 109:
/* # line 649 "gram.in" */
{ if (!datastack)
			curdtp = 0;
		  datastack = mkchain((char *)curdtp, datastack);
		  curdtp = yypvt[-0].chval; curdtelt = 0;
		  } break;
case 110:
/* # line 655 "gram.in" */
{ yyval.chval = hookup(yypvt[-2].chval, yypvt[-0].chval); } break;
case 111:
/* # line 659 "gram.in" */
{ ndim = 0; } break;
case 113:
/* # line 663 "gram.in" */
{ ndim = 0; } break;
case 116:
/* # line 668 "gram.in" */
{
		  if(ndim == maxdim)
			err("too many dimensions");
		  else if(ndim < maxdim)
			{ dims[ndim].lb = 0;
			  dims[ndim].ub = yypvt[-0].expval;
			}
		  ++ndim;
		} break;
case 117:
/* # line 678 "gram.in" */
{
		  if(ndim == maxdim)
			err("too many dimensions");
		  else if(ndim < maxdim)
			{ dims[ndim].lb = yypvt[-2].expval;
			  dims[ndim].ub = yypvt[-0].expval;
			}
		  ++ndim;
		} break;
case 118:
/* # line 690 "gram.in" */
{ yyval.expval = 0; } break;
case 120:
/* # line 695 "gram.in" */
{ nstars = 1; labarray[0] = yypvt[-0].labval; } break;
case 121:
/* # line 697 "gram.in" */
{ if(nstars < maxlablist)  labarray[nstars++] = yypvt[-0].labval; } break;
case 122:
/* # line 701 "gram.in" */
{ yyval.labval = execlab( convci(toklen, token) ); } break;
case 123:
/* # line 705 "gram.in" */
{ NO66("IMPLICIT statement"); } break;
case 126:
/* # line 711 "gram.in" */
{ if (vartype != TYUNKNOWN)
			dclerr("-- expected letter range",NPNULL);
		  setimpl(vartype, varleng, 'a', 'z'); } break;
case 127:
/* # line 716 "gram.in" */
{ needkwd = 1; } break;
case 131:
/* # line 725 "gram.in" */
{ setimpl(vartype, varleng, yypvt[-0].ival, yypvt[-0].ival); } break;
case 132:
/* # line 727 "gram.in" */
{ setimpl(vartype, varleng, yypvt[-2].ival, yypvt[-0].ival); } break;
case 133:
/* # line 731 "gram.in" */
{ if(toklen!=1 || token[0]<'a' || token[0]>'z')
			{
			dclerr("implicit item must be single letter", NPNULL);
			yyval.ival = 0;
			}
		  else yyval.ival = token[0];
		} break;
case 136:
/* # line 745 "gram.in" */
{
		if(yypvt[-2].namval->vclass == CLUNKNOWN)
			{
			yypvt[-2].namval->vclass = CLNAMELIST;
			yypvt[-2].namval->vtype = TYINT;
			yypvt[-2].namval->vstg = STGBSS;
			yypvt[-2].namval->varxptr.namelist = yypvt[-0].chval;
			yypvt[-2].namval->vardesc.varno = ++lastvarno;
			}
		else dclerr("cannot be a namelist name", yypvt[-2].namval);
		} break;
case 137:
/* # line 759 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].namval, CHNULL); } break;
case 138:
/* # line 761 "gram.in" */
{ yyval.chval = hookup(yypvt[-2].chval, mkchain((char *)yypvt[-0].namval, CHNULL)); } break;
case 139:
/* # line 765 "gram.in" */
{ switch(parstate)
			{
			case OUTSIDE:	newproc();
					startproc(ESNULL, CLMAIN);
			case INSIDE:	parstate = INDCL;
			case INDCL:	break;

			case INDATA:
				if (datagripe) {
					errstr(
				"Statement order error: declaration after DATA",
						CNULL);
					datagripe = 0;
					}
				break;

			default:
				dclerr("declaration among executables", NPNULL);
			}
		} break;
case 140:
/* # line 787 "gram.in" */
{ yyval.chval = 0; } break;
case 141:
/* # line 789 "gram.in" */
{ yyval.chval = revchain(yypvt[-0].chval); } break;
case 142:
/* # line 793 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].expval, CHNULL); } break;
case 143:
/* # line 795 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].expval, yypvt[-2].chval); } break;
case 145:
/* # line 800 "gram.in" */
{ yyval.expval = yypvt[-1].expval; if (yyval.expval->tag == TPRIM)
					yyval.expval->primblock.parenused = 1; } break;
case 149:
/* # line 808 "gram.in" */
{ yyval.expval = mkexpr(yypvt[-1].ival, yypvt[-2].expval, yypvt[-0].expval); } break;
case 150:
/* # line 810 "gram.in" */
{ yyval.expval = mkexpr(OPSTAR, yypvt[-2].expval, yypvt[-0].expval); } break;
case 151:
/* # line 812 "gram.in" */
{ yyval.expval = mkexpr(OPSLASH, yypvt[-2].expval, yypvt[-0].expval); } break;
case 152:
/* # line 814 "gram.in" */
{ yyval.expval = mkexpr(OPPOWER, yypvt[-2].expval, yypvt[-0].expval); } break;
case 153:
/* # line 816 "gram.in" */
{ if(yypvt[-1].ival == OPMINUS)
			yyval.expval = mkexpr(OPNEG, yypvt[-0].expval, ENULL);
		  else 	yyval.expval = yypvt[-0].expval;
		} break;
case 154:
/* # line 821 "gram.in" */
{ yyval.expval = mkexpr(yypvt[-1].ival, yypvt[-2].expval, yypvt[-0].expval); } break;
case 155:
/* # line 823 "gram.in" */
{ NO66(".EQV. operator");
		  yyval.expval = mkexpr(OPEQV, yypvt[-2].expval,yypvt[-0].expval); } break;
case 156:
/* # line 826 "gram.in" */
{ NO66(".NEQV. operator");
		  yyval.expval = mkexpr(OPNEQV, yypvt[-2].expval, yypvt[-0].expval); } break;
case 157:
/* # line 829 "gram.in" */
{ yyval.expval = mkexpr(OPOR, yypvt[-2].expval, yypvt[-0].expval); } break;
case 158:
/* # line 831 "gram.in" */
{ yyval.expval = mkexpr(OPAND, yypvt[-2].expval, yypvt[-0].expval); } break;
case 159:
/* # line 833 "gram.in" */
{ yyval.expval = mkexpr(OPNOT, yypvt[-0].expval, ENULL); } break;
case 160:
/* # line 835 "gram.in" */
{ NO66("concatenation operator //");
		  yyval.expval = mkexpr(OPCONCAT, yypvt[-2].expval, yypvt[-0].expval); } break;
case 161:
/* # line 839 "gram.in" */
{ yyval.ival = OPPLUS; } break;
case 162:
/* # line 840 "gram.in" */
{ yyval.ival = OPMINUS; } break;
case 163:
/* # line 843 "gram.in" */
{ yyval.ival = OPEQ; } break;
case 164:
/* # line 844 "gram.in" */
{ yyval.ival = OPGT; } break;
case 165:
/* # line 845 "gram.in" */
{ yyval.ival = OPLT; } break;
case 166:
/* # line 846 "gram.in" */
{ yyval.ival = OPGE; } break;
case 167:
/* # line 847 "gram.in" */
{ yyval.ival = OPLE; } break;
case 168:
/* # line 848 "gram.in" */
{ yyval.ival = OPNE; } break;
case 169:
/* # line 852 "gram.in" */
{ yyval.expval = mkprim(yypvt[-0].namval, LBNULL, CHNULL); } break;
case 170:
/* # line 854 "gram.in" */
{ NO66("substring operator :");
		  yyval.expval = mkprim(yypvt[-1].namval, LBNULL, yypvt[-0].chval); } break;
case 171:
/* # line 857 "gram.in" */
{ yyval.expval = mkprim(yypvt[-3].namval, mklist(yypvt[-1].chval), CHNULL); } break;
case 172:
/* # line 859 "gram.in" */
{ NO66("substring operator :");
		  yyval.expval = mkprim(yypvt[-4].namval, mklist(yypvt[-2].chval), yypvt[-0].chval); } break;
case 173:
/* # line 864 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-3].expval, mkchain((char *)yypvt[-1].expval,CHNULL)); } break;
case 174:
/* # line 868 "gram.in" */
{ yyval.expval = 0; } break;
case 176:
/* # line 873 "gram.in" */
{ if(yypvt[-0].namval->vclass == CLPARAM)
			yyval.expval = (expptr) cpexpr(
				( (struct Paramblock *) (yypvt[-0].namval) ) -> paramval);
		} break;
case 178:
/* # line 880 "gram.in" */
{ yyval.expval = mklogcon(1); } break;
case 179:
/* # line 881 "gram.in" */
{ yyval.expval = mklogcon(0); } break;
case 180:
/* # line 882 "gram.in" */
{ yyval.expval = mkstrcon(toklen, token); } break;
case 181:
/* # line 883 "gram.in" */
 { yyval.expval = mkintcon( convci(toklen, token) ); } break;
case 182:
/* # line 884 "gram.in" */
 { yyval.expval = mkrealcon(tyreal, token); } break;
case 183:
/* # line 885 "gram.in" */
 { yyval.expval = mkrealcon(TYDREAL, token); } break;
case 185:
/* # line 890 "gram.in" */
{ yyval.expval = mkcxcon(yypvt[-3].expval,yypvt[-1].expval); } break;
case 186:
/* # line 894 "gram.in" */
{ NOEXT("hex constant");
		  yyval.expval = mkbitcon(4, toklen, token); } break;
case 187:
/* # line 897 "gram.in" */
{ NOEXT("octal constant");
		  yyval.expval = mkbitcon(3, toklen, token); } break;
case 188:
/* # line 900 "gram.in" */
{ NOEXT("binary constant");
		  yyval.expval = mkbitcon(1, toklen, token); } break;
case 190:
/* # line 906 "gram.in" */
{ yyval.expval = yypvt[-1].expval; } break;
case 193:
/* # line 912 "gram.in" */
{ yyval.expval = mkexpr(yypvt[-1].ival, yypvt[-2].expval, yypvt[-0].expval); } break;
case 194:
/* # line 914 "gram.in" */
{ yyval.expval = mkexpr(OPSTAR, yypvt[-2].expval, yypvt[-0].expval); } break;
case 195:
/* # line 916 "gram.in" */
{ yyval.expval = mkexpr(OPSLASH, yypvt[-2].expval, yypvt[-0].expval); } break;
case 196:
/* # line 918 "gram.in" */
{ yyval.expval = mkexpr(OPPOWER, yypvt[-2].expval, yypvt[-0].expval); } break;
case 197:
/* # line 920 "gram.in" */
{ if(yypvt[-1].ival == OPMINUS)
			yyval.expval = mkexpr(OPNEG, yypvt[-0].expval, ENULL);
		  else	yyval.expval = yypvt[-0].expval;
		} break;
case 198:
/* # line 925 "gram.in" */
{ NO66("concatenation operator //");
		  yyval.expval = mkexpr(OPCONCAT, yypvt[-2].expval, yypvt[-0].expval); } break;
case 200:
/* # line 930 "gram.in" */
{
		if(yypvt[-3].labval->labdefined)
			execerr("no backward DO loops", CNULL);
		yypvt[-3].labval->blklevel = blklevel+1;
		exdo(yypvt[-3].labval->labelno, NPNULL, yypvt[-0].chval);
		} break;
case 201:
/* # line 937 "gram.in" */
{
		exdo((int)(ctls - ctlstack - 2), NPNULL, yypvt[-0].chval);
		NOEXT("DO without label");
		} break;
case 202:
/* # line 942 "gram.in" */
{ exenddo(NPNULL); } break;
case 203:
/* # line 944 "gram.in" */
{ exendif();  thiswasbranch = NO; } break;
case 205:
/* # line 947 "gram.in" */
{ exelif(yypvt[-2].expval); lastwasbranch = NO; } break;
case 206:
/* # line 949 "gram.in" */
{ exelse(); lastwasbranch = NO; } break;
case 207:
/* # line 951 "gram.in" */
{ exendif(); lastwasbranch = NO; } break;
case 208:
/* # line 955 "gram.in" */
{ exif(yypvt[-1].expval); } break;
case 209:
/* # line 959 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-2].namval, yypvt[-0].chval); } break;
case 211:
/* # line 964 "gram.in" */
{ yyval.chval = mkchain(CNULL, (chainp)yypvt[-1].expval); } break;
case 212:
/* # line 968 "gram.in" */
{ exequals((struct Primblock *)yypvt[-2].expval, yypvt[-0].expval); } break;
case 213:
/* # line 970 "gram.in" */
{ exassign(yypvt[-0].namval, yypvt[-2].labval); } break;
case 216:
/* # line 974 "gram.in" */
{ inioctl = NO; } break;
case 217:
/* # line 976 "gram.in" */
{ exarif(yypvt[-6].expval, yypvt[-4].labval, yypvt[-2].labval, yypvt[-0].labval);  thiswasbranch = YES; } break;
case 218:
/* # line 978 "gram.in" */
{ excall(yypvt[-0].namval, LBNULL, 0, labarray); } break;
case 219:
/* # line 980 "gram.in" */
{ excall(yypvt[-2].namval, LBNULL, 0, labarray); } break;
case 220:
/* # line 982 "gram.in" */
{ if(nstars < maxlablist)
			excall(yypvt[-3].namval, mklist(revchain(yypvt[-1].chval)), nstars, labarray);
		  else
			many("alternate returns", 'l', maxlablist);
		} break;
case 221:
/* # line 988 "gram.in" */
{ exreturn(yypvt[-0].expval);  thiswasbranch = YES; } break;
case 222:
/* # line 990 "gram.in" */
{ exstop(yypvt[-2].ival, yypvt[-0].expval);  thiswasbranch = yypvt[-2].ival; } break;
case 223:
/* # line 994 "gram.in" */
{ yyval.labval = mklabel( convci(toklen, token) ); } break;
case 224:
/* # line 998 "gram.in" */
{ if(parstate == OUTSIDE)
			{
			newproc();
			startproc(ESNULL, CLMAIN);
			}
		} break;
case 225:
/* # line 1007 "gram.in" */
{ exgoto(yypvt[-0].labval);  thiswasbranch = YES; } break;
case 226:
/* # line 1009 "gram.in" */
{ exasgoto(yypvt[-0].namval);  thiswasbranch = YES; } break;
case 227:
/* # line 1011 "gram.in" */
{ exasgoto(yypvt[-4].namval);  thiswasbranch = YES; } break;
case 228:
/* # line 1013 "gram.in" */
{ if(nstars < maxlablist)
			putcmgo(putx(fixtype(yypvt[-0].expval)), nstars, labarray);
		  else
			many("labels in computed GOTO list", 'l', maxlablist);
		} break;
case 231:
/* # line 1025 "gram.in" */
{ nstars = 0; yyval.namval = yypvt[-0].namval; } break;
case 232:
/* # line 1029 "gram.in" */
{ yyval.chval = yypvt[-0].expval ? mkchain((char *)yypvt[-0].expval,CHNULL) : CHNULL; } break;
case 233:
/* # line 1031 "gram.in" */
{ yyval.chval = yypvt[-0].expval ? mkchain((char *)yypvt[-0].expval, yypvt[-2].chval) : yypvt[-2].chval; } break;
case 235:
/* # line 1036 "gram.in" */
{ if(nstars < maxlablist) labarray[nstars++] = yypvt[-0].labval; yyval.expval = 0; } break;
case 236:
/* # line 1040 "gram.in" */
{ yyval.ival = 0; } break;
case 237:
/* # line 1042 "gram.in" */
{ yyval.ival = 2; } break;
case 238:
/* # line 1046 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].expval, CHNULL); } break;
case 239:
/* # line 1048 "gram.in" */
{ yyval.chval = hookup(yypvt[-2].chval, mkchain((char *)yypvt[-0].expval,CHNULL) ); } break;
case 240:
/* # line 1052 "gram.in" */
{ if(parstate == OUTSIDE)
			{
			newproc();
			startproc(ESNULL, CLMAIN);
			}

/* This next statement depends on the ordering of the state table encoding */

		  if(parstate < INDATA) enddcl();
		} break;
case 241:
/* # line 1065 "gram.in" */
{ intonly = YES; } break;
case 242:
/* # line 1069 "gram.in" */
{ intonly = NO; } break;
case 243:
/* # line 1074 "gram.in" */
{ endio(); } break;
case 245:
/* # line 1079 "gram.in" */
{ ioclause(IOSUNIT, yypvt[-0].expval); endioctl(); } break;
case 246:
/* # line 1081 "gram.in" */
{ ioclause(IOSUNIT, ENULL); endioctl(); } break;
case 247:
/* # line 1083 "gram.in" */
{ ioclause(IOSUNIT, IOSTDERR); endioctl(); } break;
case 249:
/* # line 1086 "gram.in" */
{ doio(CHNULL); } break;
case 250:
/* # line 1088 "gram.in" */
{ doio(CHNULL); } break;
case 251:
/* # line 1090 "gram.in" */
{ doio(revchain(yypvt[-0].chval)); } break;
case 252:
/* # line 1092 "gram.in" */
{ doio(revchain(yypvt[-0].chval)); } break;
case 253:
/* # line 1094 "gram.in" */
{ doio(revchain(yypvt[-0].chval)); } break;
case 254:
/* # line 1096 "gram.in" */
{ doio(CHNULL); } break;
case 255:
/* # line 1098 "gram.in" */
{ doio(revchain(yypvt[-0].chval)); } break;
case 256:
/* # line 1100 "gram.in" */
{ doio(CHNULL); } break;
case 257:
/* # line 1102 "gram.in" */
{ doio(revchain(yypvt[-0].chval)); } break;
case 259:
/* # line 1109 "gram.in" */
{ iostmt = IOBACKSPACE; } break;
case 260:
/* # line 1111 "gram.in" */
{ iostmt = IOREWIND; } break;
case 261:
/* # line 1113 "gram.in" */
{ iostmt = IOENDFILE; } break;
case 263:
/* # line 1120 "gram.in" */
{ iostmt = IOINQUIRE; } break;
case 264:
/* # line 1122 "gram.in" */
{ iostmt = IOOPEN; } break;
case 265:
/* # line 1124 "gram.in" */
{ iostmt = IOCLOSE; } break;
case 266:
/* # line 1128 "gram.in" */
{
		ioclause(IOSUNIT, ENULL);
		ioclause(IOSFMT, yypvt[-0].expval);
		endioctl();
		} break;
case 267:
/* # line 1134 "gram.in" */
{
		ioclause(IOSUNIT, ENULL);
		ioclause(IOSFMT, ENULL);
		endioctl();
		} break;
case 268:
/* # line 1142 "gram.in" */
{
		  ioclause(IOSUNIT, yypvt[-1].expval);
		  endioctl();
		} break;
case 269:
/* # line 1147 "gram.in" */
{ endioctl(); } break;
case 272:
/* # line 1155 "gram.in" */
{ ioclause(IOSPOSITIONAL, yypvt[-0].expval); } break;
case 273:
/* # line 1157 "gram.in" */
{ ioclause(IOSPOSITIONAL, ENULL); } break;
case 274:
/* # line 1159 "gram.in" */
{ ioclause(IOSPOSITIONAL, IOSTDERR); } break;
case 275:
/* # line 1161 "gram.in" */
{ ioclause(yypvt[-1].ival, yypvt[-0].expval); } break;
case 276:
/* # line 1163 "gram.in" */
{ ioclause(yypvt[-1].ival, ENULL); } break;
case 277:
/* # line 1165 "gram.in" */
{ ioclause(yypvt[-1].ival, IOSTDERR); } break;
case 278:
/* # line 1169 "gram.in" */
{ yyval.ival = iocname(); } break;
case 279:
/* # line 1173 "gram.in" */
{ iostmt = IOREAD; } break;
case 280:
/* # line 1177 "gram.in" */
{ iostmt = IOWRITE; } break;
case 281:
/* # line 1181 "gram.in" */
{
		iostmt = IOWRITE;
		ioclause(IOSUNIT, ENULL);
		ioclause(IOSFMT, yypvt[-1].expval);
		endioctl();
		} break;
case 282:
/* # line 1188 "gram.in" */
{
		iostmt = IOWRITE;
		ioclause(IOSUNIT, ENULL);
		ioclause(IOSFMT, ENULL);
		endioctl();
		} break;
case 283:
/* # line 1197 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].tagval, CHNULL); } break;
case 284:
/* # line 1199 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].tagval, yypvt[-2].chval); } break;
case 285:
/* # line 1203 "gram.in" */
{ yyval.tagval = (tagptr) yypvt[-0].expval; } break;
case 286:
/* # line 1205 "gram.in" */
{ yyval.tagval = (tagptr) mkiodo(yypvt[-1].chval,revchain(yypvt[-3].chval)); } break;
case 287:
/* # line 1209 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].expval, CHNULL); } break;
case 288:
/* # line 1211 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].tagval, CHNULL); } break;
case 290:
/* # line 1216 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].expval, mkchain((char *)yypvt[-2].expval, CHNULL) ); } break;
case 291:
/* # line 1218 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].tagval, mkchain((char *)yypvt[-2].expval, CHNULL) ); } break;
case 292:
/* # line 1220 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].expval, mkchain((char *)yypvt[-2].tagval, CHNULL) ); } break;
case 293:
/* # line 1222 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].tagval, mkchain((char *)yypvt[-2].tagval, CHNULL) ); } break;
case 294:
/* # line 1224 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].expval, yypvt[-2].chval); } break;
case 295:
/* # line 1226 "gram.in" */
{ yyval.chval = mkchain((char *)yypvt[-0].tagval, yypvt[-2].chval); } break;
case 296:
/* # line 1230 "gram.in" */
{ yyval.tagval = (tagptr) yypvt[-0].expval; } break;
case 297:
/* # line 1232 "gram.in" */
{ yyval.tagval = (tagptr) yypvt[-1].expval; } break;
case 298:
/* # line 1234 "gram.in" */
{ yyval.tagval = (tagptr) mkiodo(yypvt[-1].chval, mkchain((char *)yypvt[-3].expval, CHNULL) ); } break;
case 299:
/* # line 1236 "gram.in" */
{ yyval.tagval = (tagptr) mkiodo(yypvt[-1].chval, mkchain((char *)yypvt[-3].tagval, CHNULL) ); } break;
case 300:
/* # line 1238 "gram.in" */
{ yyval.tagval = (tagptr) mkiodo(yypvt[-1].chval, revchain(yypvt[-3].chval)); } break;
case 301:
/* # line 1242 "gram.in" */
{ startioctl(); } break;
	}
	goto yystack;  /* stack new state and value */
}
