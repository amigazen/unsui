#include	<stdio.h>
#include	<ctype.h>

/*
 * A bug in the current Lattice C compiler for the AMIGA prevents enums from
 * working properly.
 */
#ifdef AMIGA
#  define NOENUMS	/* Define NOENUMS to not use enumerations */
#endif

#define	min(a,b)	(a<b?a:b)
#define	max(a,b)	(a>b?a:b)
#define	skipbl(p)	{while(*p == ' ' || *p == '\t')p++;}
#define	skipnbl(p)	{while(*p != ' ' && *p != '\t' && *p != '\n')p++;}
#define	CHARNULL	((char *)NULL)
#define	NO		0
#define	YES		1
#define	COMMAND		'.'
#define	PAGENUM		'%'
#define	HUGE		10000
#define	MAXIN		256
#define	MAXOUT		256
#define	MAXFN		64	/* length of included filename */
#define	MAXCHARS	14
#define	MAXMAC		50
#define	MAXPB		50
#define	MAXTABS		20
#define	PAGLEN		66
#define	PAPERSIZE	65
#define	M1DEF		3
#define	M2DEF		1
#define	M3DEF		1
#define	M4DEF		3
#define	PAGEWIDTH	60
#define	ARABIC		0
#define	ROMAN		1
#define	ENGLISH		2

struct linelink{
	char *lineptr;
	struct linelink *lastline;
	};
struct	macro{
	char macnam[3];
	struct linelink *macend;
	};
struct	envir{
	short plval;
	short m1val;
	short m2val;
	short m3val;
	short m4val;
	short bottom;
	char *evenhdr,*oddhdr;
	char *evenftr,*oddftr;
	char comchr;
	char tabchr;
	char ubchr;
	short fill;
	short adjust;
	short numtyp;
	short lsval;
	short llval;
	short inval;
	short tival;
	short poval;
	short ceval;
	short ulval;
	short bdval;
	short litval;
	short blval;
	short skpval;
	short tabpos[MAXTABS];
	struct envir *lastenv;
	};
struct cmdents{
	char cmdname[3];
	short notredefd;
	};
#ifdef NOENUMS
#  define ADJ		00
#  define ARA		01
#  define BLD		02
#  define BLN		03
#  define BPG		04
#  define BRE		05
#  define CMC		06
#  define CEN		07
#  define DFN		08
#  define EFO		09
#  define ENG		10
#  define EHD		11
#  define FIL		12
#  define FOT		13
#  define HED		14
#  define IND		15
#  define INX		16
#  define LIT		17
#  define LNL		18
#  define LNS		19
#  define M1		20
#  define M2		21
#  define M3		22
#  define M4		23
#  define NAD		24
#  define NED		25
#  define NFL		26
#  define OFO		27
#  define OHD		28
#  define PGL		29
#  define POF		30
#  define PGT		31
#  define RNV		32
#  define REF		33
#  define ROM		34
#  define RPG		35
#  define SNV		36
#  define SKP		37
#  define SOU		38
#  define SPA		39
#  define TCL		40
#  define TCH		41
#  define TMI		42
#  define UBC		43
#  define UDL		44
#  define MAC		45
#  define UNKNOWN	46
#  define CMDNUM int
#else
#  define CMDNUM enum cmdnum
CMDNUM		{ADJ, ARA, BLD, BLN, BPG, BRE, CMC, CEN, DFN, EFO, ENG, EHD,
		FIL, FOT, HED, IND, INX, LIT, LNL, LNS, M1, M2, M3, M4, NAD,
		NED, NFL, OFO, OHD, PGL, POF, PGT, RNV, REF, ROM, RPG, SNV, SKP,
		SOU, SPA, TCL, TCH, TMI, UBC, UDL, MAC, UNKNOWN};
#endif
extern	struct macro macros[MAXMAC];
extern	short maccnt;
extern	char *pbptr[MAXMAC];
extern	short pblev;
extern	char outbuf[MAXOUT];
extern	char *outp;
extern	short outw;
extern	short outwds;
extern	short pages;
extern	short pausecount;
extern	short curpag;
extern	short newpag;
extern	short lineno;
extern	short peekno;
extern	short indline;
extern	short respage;
extern	char trapmac[];
extern	char blnkhdr[];
extern	struct envir env;
extern	struct envir *curenv;
extern	struct cmdents builtins[];
extern	short echodir;
extern	char *progname;
extern	char *filename;
extern	short fileline;
extern	short ttyfd;
extern	FILE *indfp;
extern	char *nomem;
