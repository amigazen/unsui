#ifndef INITEXTERN
#  define INITEXTERN extern
#endif

#if !defined(AMIGA) || !defined(LATTICE)
#  define STDARGS
#  define FARREF
#else
#  define STDARGS __stdargs
#  define FARREF  __far
#endif

/*  Nur in "config.h" angegebene Defines koennen (fast) problemlos
 *  geaendert werden.
 */
#include "config.h"



/* (br) enable/disable some of my changes: */
/* vvvvvvvvvvvv */
/* Don't change any of these defines, change is not tested !!!! */
/* ^^^^^^^^^^^^ */
/* ... and the format of the fmt-file is possible changed */
#define FONTPTR		/* Use pointer in charbase[], ... */
#define FONTSTRUCT	/* put fontinfo in a array of structs
			   instead of many separate arrays. */
#undef  ORIGTRIE	/* shorter Trie or original def.  */
#undef	OLD_MODE	/* `mode' is 1,102,203 or 1,2,3?  */
#define INP_PTR		/* curinput is ptr in inputstack[] */
#define PARAM_PTR	/* changes mit parameter stack */
#define HYPHSTRUCT	/* hyphenation exception in struct? */
#define NEW_HELP	/* helpX() macros array or array ptr assignment? */


/* (br) GNU C erlaubt das Reservieren von "globalen" Registervariablen, dabei
 * muss nur darauf geachtet werden, dass alle Teile nochmal mit dieser Option
 * uebersetzt wurden (sonst bekommen die Register evtl. einen anderen Wert!!)
 */

#ifndef __GNUC__
#  undef REG_A5
#else
#  define REG_A5
#endif

/* Leider ist memoryword noch nicht bekannt, es werden jedoch schon `inline'
 * Funktionen in den Headerfiles definiert, deshalb muss schon hier die
 * Variable deklariert werden und spaeter (in coerce.h) auf (memoryword *)
 * gecastet werden.
 */
#ifdef REG_A5
register void *zzzaa __asm__("a5");
#endif


#include "texmf.h"	/* "../common/texmf.h" */

extern char banner[];
EXTERN schar user_interaction ;		/* interaction mode from arguments */
EXTERN long  user_language ;		/* language from args or cnf-file  */

#ifdef MLTEX
EXTERN char is_ML_TeX;		/* set, if ML-TeX */
#endif
#ifdef TEXXET
EXTERN char is_TeX_XeT;		/* set, if TeX--XeT */
#endif


#ifdef BIG
#define maxhalfword 524287L	/* from bigtex.diff: 262143L */
#define hashsize 9500
#define hashprime 7919
#define hyphsize 607
#else
#define maxhalfword 65535L
#define hashsize 3500		/* changed, original: 3000 */
#define hashprime 2551
#define hyphsize 307
#endif

INITEXTERN long memmax;
#define memmin 0		/* sollte immer 0 sein */
INITEXTERN long bufsize;
INITEXTERN long errorline;
INITEXTERN long halferrorline;
INITEXTERN long maxprintline;
INITEXTERN long stacksize;
INITEXTERN long maxinopen;
INITEXTERN long fontmax;
INITEXTERN long fontmemsize;
INITEXTERN long paramsize;	/* 60 */
INITEXTERN long nestsize;	/* 40 */
INITEXTERN long maxstrings;
INITEXTERN long stringvacancies;
INITEXTERN long poolsize;
INITEXTERN long savesize;
INITEXTERN long triesize;
INITEXTERN long trieopsize;		/* 500 */
#define negtrieopsize (-trieopsize)	/* -500 */
#define mintrieop 0
#define maxtrieop 32767L		/* 500 */
INITEXTERN long dvibufsize;
#define poolname "tex.pool" 
INITEXTERN long memtop;
#define membot 0 

#include "texdefs.h"

typedef unsigned char ASCIIcode;
typedef unsigned char eightbits;
typedef integer poolpointer;
typedef integer strnumber;
typedef unsigned char packedASCIIcode;
typedef integer scaled;
typedef integer nonnegativeinteger;
typedef schar smallnumber;
typedef unsigned char quarterword;

/* long_halfword...	wird verwendet fuer Funktionswerte, die danach als
 *			Indizes o.ae. weiterverwendet werden.  Dadurch
 *			entfaellt zuerst impliziter cast auf (halfword) und
 *			danach als Index auf ([unsigned] long).
 */
#ifdef BIG
typedef integer halfword;
typedef integer long_halfword;
#else
typedef unsigned short halfword;
typedef unsigned long  long_halfword;
#endif

typedef schar twochoices;
typedef schar fourchoices;

#include "memory.h"

typedef schar glueord;

typedef struct {
  short modefield;
  halfword headfield, tailfield;
  integer pgfield, mlfield;
  memoryword auxfield;
#if 0  /* 3.1415 */
  quarterword lhmfield, rhmfield;
#endif
#ifdef TEXXET
  halfword LRs_field;
#endif
} liststaterecord;

typedef schar groupcode;

typedef struct {
  quarterword statefield, indexfield;
  halfword startfield, locfield, limitfield, namefield;
} instaterecord;

typedef SMALLhalfword internalfontnumber ;	/* integer */
typedef integer fontindex;
typedef integer dviindex;
typedef SMALLhalfword trieopcode ;	/* integer */
#ifdef BIGTRIE
typedef halfword triepointer ;		/* integer */
#else
typedef SMALLhalfword triepointer ;	/* integer */
#endif
typedef SMALLhalfword hyphpointer ;	/* integer */

#ifdef TEXXET
EXTERN smallnumber cur_dir;
EXTERN halfword LR_ptr;
#endif

EXTERN ASCIIcode xord[256];
EXTERN ASCIIcode xchr[256];
EXTERN boolean printable[256];
#ifdef ERW_CODEPAGE
EXTERN boolean codepage_given;
#endif
EXTERN char nameoffile[FILENAMESIZE + 1] ;
EXTERN integer namelength;
INITEXTERN ASCIIcode *buffer;			/* [bufsize + 1] */
EXTERN integer first;
EXTERN integer last;
EXTERN integer maxbufstack;
INITEXTERN packedASCIIcode *strpool;		/* [poolsize + 1] */
INITEXTERN poolpointer *strstart;		/* [maxstrings + 1] */
EXTERN poolpointer poolptr  ; 
EXTERN strnumber strptr  ; 
EXTERN poolpointer initpoolptr  ; 
EXTERN strnumber initstrptr  ; 

EXTERN alphafile logfile  ; 
EXTERN schar selector ;		/* integer */
EXTERN schar dig[23]  ; 
EXTERN integer tally  ; 
EXTERN integer termoffset  ; 
EXTERN integer fileoffset  ; 
INITEXTERN ASCIIcode *trickbuf;                /* [errorline + 1] */
EXTERN integer trickcount  ; 
EXTERN integer firstcount  ; 
#ifndef ERW_INTERACTION
EXTERN schar interaction ;		/* integer */
#endif
EXTERN boolean deletionsallowed ;
EXTERN boolean setboxallowed ;
EXTERN schar history ;			/* integer */
EXTERN schar errorcount ;
#ifdef NEW_HELP
EXTERN short *helpptr;
#else
/* in helpline werden nur die "festen" Strings eingetragen, deshalb... */
EXTERN /* strnumber */ unsigned short helpline[6];
EXTERN schar helpptr;
#endif
EXTERN boolean useerrhelp;
EXTERN short interrupt  ; 		/* changed from integer (br) */
EXTERN boolean OKtointerrupt  ; 
EXTERN boolean aritherror  ; 
EXTERN scaled remainder  ; 
EXTERN halfword tempptr  ; 
#define zmem (zzzaa - (int)(memmin))
#ifndef REG_A5
INITEXTERN memoryword *zzzaa;                /* [memmax - memmin + 1] */
#endif
EXTERN halfword lomemmax  ; 
EXTERN halfword himemmin  ; 
EXTERN integer varused, dynused  ; 
EXTERN halfword avail  ; 
EXTERN halfword memend  ; 
EXTERN halfword rover  ; 
#ifdef DEBUG
#define freearr (zzzab - (int)(memmin))
#define wasfree (zzzac - (int)(memmin))
INITEXTERN boolean *zzzab;                /* [memmax - memmin + 1] */
INITEXTERN boolean *zzzac;                /* [memmax - memmin + 1] */
EXTERN halfword wasmemend, waslomax, washimin  ; 
EXTERN boolean panicking  ; 
#endif /* DEBUG */

EXTERN integer fontinshortdisplay  ; 
EXTERN integer depththreshold  ; 
EXTERN integer breadthmax  ; 
EXTERN liststaterecord *nest;		/* [nestsize + 1] */
EXTERN integer nestptr  ; 
EXTERN integer maxneststack  ; 
EXTERN liststaterecord curlist  ; 
EXTERN short shownmode  ; 
EXTERN integer oldsetting  ; 


/* Note:  activebase = 1, ... it's faster without -activebase ! */
/* #define zeqtb (zzzad - (int)(activebase))			*/
/* EXTERN MEDmemoryword zzzad[eqtbsize - activebase + 1];	*/

#ifdef EQTB_ALLOC	/* siehe auch init.c! */

INITEXTERN MEDmemoryword *zeqtb;
INITEXTERN quarterword *zzzae;
INITEXTERN twohalves *zzzaf;
/* zzzae = &xeqlevel[intbase] */
#define xeqlevel (zzzae)
/* zzzaf = &hash[hashbase] */
#define hash (zzzaf)

#else

EXTERN MEDmemoryword FARREF zeqtb[eqtbsize + 1];
EXTERN quarterword FARREF zzzae[eqtbsize - intbase + 1];
EXTERN twohalves FARREF zzzaf[undefinedcontrolsequence - hashbase + 1];
#define xeqlevel (zzzae - (long)(intbase))
#define hash (zzzaf - (long)(hashbase))

#endif

EXTERN halfword hashused;
EXTERN boolean nonewcontrolsequence;
EXTERN integer cscount;
INITEXTERN MEDmemoryword *savestack;                /* [savesize + 1] */
EXTERN integer saveptr;
EXTERN integer maxsavestack;
EXTERN quarterword curlevel;
EXTERN groupcode curgroup;
EXTERN integer curboundary;
/* EXTERN integer magset; */ /* local nach preparemag() */

EXTERN eightbits curcmd;
EXTERN halfword curchr;
EXTERN halfword curcs;
EXTERN halfword curtok;

INITEXTERN instaterecord *inputstack;                /* [stacksize + 1] */
#ifdef INP_PTR
EXTERN instaterecord *curinput_ptr;
# define curinput (*curinput_ptr)
#else
EXTERN instaterecord curinput;
#endif
EXTERN integer inputptr;
EXTERN integer maxinstack;
EXTERN integer baseptr;

EXTERN integer inopen;
EXTERN integer openparens  ; 
INITEXTERN alphafile *inputfile;                /* [maxinopen + 1] */
EXTERN integer line  ; 
INITEXTERN integer *linestack;                /* [maxinopen + 1] */
EXTERN schar scannerstatus  ; 
EXTERN halfword warningindex  ; 
EXTERN halfword defref;
EXTERN halfword *paramstack;			/* [paramsize + 1] */
EXTERN integer paramptr;
EXTERN integer maxparamstack;
EXTERN integer alignstate;
EXTERN halfword parloc;
EXTERN halfword partoken;
EXTERN boolean forceeof;
EXTERN halfword zzzag[splitbotmarkcode - topmarkcode + 1];
#define curmark (zzzag - (long)(topmarkcode))
EXTERN schar longstate;
EXTERN halfword pstack[9];
EXTERN integer curval;
EXTERN schar curvallevel;
EXTERN smallnumber radix;
EXTERN glueord curorder;
EXTERN alphafile readfile[16];
EXTERN schar readopen[17];
EXTERN halfword condptr;
EXTERN schar iflimit;
EXTERN smallnumber curif;
EXTERN integer ifline;
EXTERN integer skipline;
EXTERN strnumber curname;
EXTERN strnumber curarea;
EXTERN strnumber curext;
EXTERN poolpointer areadelimiter;
EXTERN poolpointer extdelimiter;
EXTERN integer formatdefaultlength;
EXTERN ccharpointer TEXformatdefault;
EXTERN boolean nameinprogress;
EXTERN strnumber jobname;
EXTERN boolean logopened;
EXTERN bytefile dvifile;
EXTERN strnumber outputfilename;
EXTERN strnumber logname;

INITEXTERN SMALLmemoryword *fontinfo;		/* [fontmemsize + 1] */
EXTERN fontindex fmemptr;
EXTERN internalfontnumber fontptr;

#ifdef FONTSTRUCT
	/* see also: defs.h !!!! */
	/* struct sollte so gross sein, dass Groesse eine 2er Potenz ist */
INITEXTERN struct font_desc {
#  ifdef FONTPTR
	SMALLmemoryword *charbase;
	SMALLmemoryword *widthbase;
	SMALLmemoryword *heightbase;
	SMALLmemoryword *depthbase;
	SMALLmemoryword *italicbase;
	SMALLmemoryword *ligkernbase;
	SMALLmemoryword *kernbase;
#  else
	integer charbase;
	integer widthbase;
	integer heightbase;
	integer depthbase;
	integer italicbase;
	integer ligkernbase;
	integer kernbase;
#  endif
	integer extenbase;
	integer parambase;

	eightbits	fontbc, fontec;
	fourquarters	fontcheck;
	SMALLhalfword	fontparams;
/* Achtung fuer BIG/normal: damit hat der struct die Groesse von 64 Bytes */
	/*halfword*/ long fontglue;
	integer		hyphenchar, skewchar;
	fontindex	bcharlabel;
	short		fontbchar, fontfalsebchar;
} *fontdesc;		/* [fontmax + 1] */

#else

INITEXTERN fourquarters *fontcheck;		/* [fontmax + 1] */
INITEXTERN SMALLhalfword *fontparams;		/* [fontmax + 1] */
#ifdef FONTPTR
INITEXTERN struct fnt_bcec {
  eightbits bc;
  eightbits ec;
} *font_bcec;					/* [fontmax + 1] */
#else
INITEXTERN eightbits *fontbc;			/* [fontmax + 1] */
INITEXTERN eightbits *fontec;			/* [fontmax + 1] */
#endif
INITEXTERN halfword *fontglue;			/* [fontmax + 1] */
INITEXTERN integer *hyphenchar;			/* [fontmax + 1] */
INITEXTERN integer *skewchar;			/* [fontmax + 1] */
INITEXTERN fontindex *bcharlabel;		/* [fontmax + 1] */
INITEXTERN short *fontbchar;			/* [fontmax + 1] */
INITEXTERN short *fontfalsebchar;		/* [fontmax + 1] */
#ifdef FONTPTR
INITEXTERN SMALLmemoryword **charbase;		/* [fontmax + 1] */
INITEXTERN SMALLmemoryword **widthbase;		/* [fontmax + 1] */
INITEXTERN SMALLmemoryword **heightbase;	/* [fontmax + 1] */
INITEXTERN SMALLmemoryword **depthbase;		/* [fontmax + 1] */
INITEXTERN SMALLmemoryword **italicbase;	/* [fontmax + 1] */
INITEXTERN SMALLmemoryword **ligkernbase;	/* [fontmax + 1] */
INITEXTERN SMALLmemoryword **kernbase;		/* [fontmax + 1] */
#else
INITEXTERN integer *charbase;			/* [fontmax + 1] */
INITEXTERN integer *widthbase;			/* [fontmax + 1] */
INITEXTERN integer *heightbase;			/* [fontmax + 1] */
INITEXTERN integer *depthbase;			/* [fontmax + 1] */
INITEXTERN integer *italicbase;			/* [fontmax + 1] */
INITEXTERN integer *ligkernbase;		/* [fontmax + 1] */
INITEXTERN integer *kernbase;			/* [fontmax + 1] */
#endif
INITEXTERN integer *extenbase;			/* [fontmax + 1] */
INITEXTERN integer *parambase;			/* [fontmax + 1] */
#endif	/* FONTSTRUCT */

INITEXTERN boolean *fontused;			/* [fontmax + 1] */
INITEXTERN scaled *fontsize;			/* [fontmax + 1] */
INITEXTERN scaled *fontdsize;			/* [fontmax + 1] */
INITEXTERN strnumber *fontname;			/* [fontmax + 1] */
INITEXTERN strnumber *fontarea;			/* [fontmax + 1] */

EXTERN fourquarters nullcharacter;
EXTERN integer totalpages;
EXTERN scaled maxv;
EXTERN scaled maxh;
EXTERN integer maxpush;
EXTERN integer lastbop;
EXTERN integer deadcycles;
EXTERN boolean doingleaders;
#if 0
EXTERN quarterword c, f  ; 
EXTERN scaled ruleht, ruledp, rulewd  ; 
EXTERN halfword g  ; 
EXTERN integer lq, lr  ; 
#endif
INITEXTERN eightbits *dvibuf;                /* [dvibufsize + 1] */
EXTERN dviindex halfbuf;
EXTERN dviindex dvilimit;
EXTERN dviindex dviptr;
EXTERN integer dvioffset;
EXTERN integer dvigone;
#if 0 /* nach shipout.c */
EXTERN halfword downptr, rightptr; 
EXTERN scaled dvih, dviv;
EXTERN scaled curh, curv;
EXTERN internalfontnumber dvif;
EXTERN integer curs;
#endif
EXTERN scaled zzzah[filll - normal + 1], zzzai[filll - normal + 1];
#define totalstretch (zzzah - (int)(normal))
#define totalshrink (zzzai - (int)(normal))
EXTERN integer lastbadness  ; 
EXTERN halfword adjusttail  ; 
EXTERN integer packbeginline  ; 
EXTERN twohalves emptyfield  ; 
EXTERN fourquarters nulldelimiter  ; 

EXTERN internalfontnumber curf  ; 
EXTERN quarterword curc  ; 
EXTERN fourquarters curi  ; 

EXTERN halfword curalign  ; 
EXTERN halfword curspan  ; 
EXTERN halfword curloop  ; 
EXTERN halfword alignptr  ; 
EXTERN halfword curhead, curtail  ; 
EXTERN halfword justbox  ; 
EXTERN halfword passive  ; 
EXTERN halfword printednode  ; 
EXTERN halfword passnumber  ; 
EXTERN scaled activewidth[7]  ; 
EXTERN scaled curactivewidth[7]  ; 
EXTERN scaled background[7]  ; 
EXTERN scaled breakwidth[7]  ; 
EXTERN boolean noshrinkerroryet  ; 
EXTERN halfword curp  ;
#if 0		/* local in linebreak() */
EXTERN boolean secondpass  ; 
#endif
EXTERN boolean finalpass  ; 
EXTERN integer threshold  ; 
EXTERN integer minimaldemerits[4]  ; 
EXTERN integer minimumdemerits  ; 
EXTERN halfword bestplace[4]  ; 
EXTERN halfword bestplline[4]  ; 
EXTERN scaled discwidth  ; 
EXTERN halfword easyline  ; 
EXTERN halfword lastspecialline  ; 
EXTERN scaled firstwidth  ; 
EXTERN scaled secondwidth  ; 
EXTERN scaled firstindent  ; 
EXTERN scaled secondindent  ; 
EXTERN halfword bestbet  ; 
EXTERN integer fewestdemerits  ; 
EXTERN halfword bestline  ; 
EXTERN integer actuallooseness  ; 
EXTERN integer linediff  ; 
EXTERN short hc[66]  ; 
EXTERN smallnumber hn  ; 
EXTERN halfword ha, hb  ; 
EXTERN internalfontnumber hf  ; 
EXTERN short hu[64]  ; 
EXTERN integer hyfchar  ; 
EXTERN ASCIIcode curlang  ; 
#if 1  /* TeX 3.1415 */
EXTERN ASCIIcode initcurlang  ; 
#endif
EXTERN integer lhyf, rhyf  ;
#if 1  /* TeX 3.1415 */
EXTERN integer initlhyf, initrhyf  ;
#endif
#if 1  /* TeX 3.141 */
EXTERN halfword hyf_bchar;
#endif
EXTERN schar hyf[65]  ; 
EXTERN halfword initlist  ; 
EXTERN boolean initlig  ; 
EXTERN boolean initlft  ; 
EXTERN smallnumber hyphenpassed  ;
#if 0	/* (br) made local in mainctrl(), reconstitute() */
EXTERN halfword curl, curr  ; 
EXTERN halfword curq  ; 
EXTERN halfword ligstack  ; 
#endif
EXTERN boolean ligaturepresent  ; 
EXTERN boolean lfthit, rthit  ; 
INITEXTERN halfword *trietrl;                /* [triesize + 1] */
INITEXTERN halfword *trietro;                /* [triesize + 1] */
INITEXTERN quarterword *trietrc;                /* [triesize + 1] */
EXTERN SMALLhalfword opstart[256];	/* integer */

#ifdef HYPHSTRUCT
EXTERN struct hyph_exc {
	strnumber word;
	halfword  list;
} hyph_exc[hyphsize + 1];
#define hyphword(x)	(hyph_exc[x].word)
#define hyphlist(x)	(hyph_exc[x].list)
#else
EXTERN strnumber hyphword[hyphsize + 1]  ; 
EXTERN halfword hyphlist[hyphsize + 1]  ; 
#define hyphword(x)	(hyphword[x])
#define hyphlist(x)	(hyphlist[x])
#endif

EXTERN hyphpointer hyphcount;

#ifdef HYPHSTRUCT
EXTERN struct hyph_op {
	smallnumber distance;
	smallnumber num;
	trieopcode  next;
} *hyph_op;		/* [trieopsize + 1] */
#define hyfdistance(x)	(hyph_op[x].distance)
#define hyfnum(x)	(hyph_op[x].num)
#define hyfnext(x)	(hyph_op[x].next)
#else
EXTERN smallnumber *hyfdistance;	/* [trieopsize + 1] */
EXTERN smallnumber *hyfnum;		/* [trieopsize + 1] */
EXTERN trieopcode *hyfnext;		/* [trieopsize + 1] ; quarterword */
#define hyfdistance(x)	(hyfdistance[x])
#define hyfnum(x)	(hyfnum[x])
#define hyfnext(x)	(hyfnext[x])
#endif

#ifdef INITEX
EXTERN SMALLhalfword *zzzaj; /* [trieopsize - negtrieopsize + 1] ; integer */
#define trieophash (zzzaj - (long)(negtrieopsize))
EXTERN ASCIIcode *trieoplang;	/* [trieopsize + 1] */
EXTERN trieopcode *trieopval;	/* [trieopsize + 1] */
EXTERN trieopcode trieused[256];
EXTERN integer trieopptr;
#endif /* INITEX */

EXTERN trieopcode maxopused  ; 
EXTERN boolean smallop  ; 

#ifdef INITEX
INITEXTERN packedASCIIcode *triec;                /* [triesize + 1] */
#ifdef ORIGTRIE
INITEXTERN trieopcode *trieo;                /* [triesize + 1] */
INITEXTERN boolean *trietaken;                /* [triesize + 1] */
#else
typedef struct {
  unsigned trietaken:1;
  unsigned trieo:15;
} trie_struct;
INITEXTERN trie_struct *ztrie;                /* [triesize + 1] */
#endif
INITEXTERN triepointer *triel;                /* [triesize + 1] */
INITEXTERN triepointer *trier;                /* [triesize + 1] */
EXTERN triepointer trieptr  ; 
INITEXTERN triepointer *triehash;                /* [triesize + 1] */
EXTERN triepointer triemin[256]  ; 
EXTERN triepointer triemax  ; 
EXTERN boolean trienotready  ; 
#endif /* INITEX */

EXTERN scaled bestheightplusdepth  ; 
EXTERN halfword pagetail  ; 
EXTERN schar pagecontents  ; 
EXTERN scaled pagemaxdepth  ; 
EXTERN halfword bestpagebreak  ; 
EXTERN integer leastpagecost  ; 
EXTERN scaled bestsize  ; 
EXTERN scaled pagesofar[8]  ; 
EXTERN halfword lastglue  ; 
EXTERN integer lastpenalty  ; 
EXTERN scaled lastkern  ; 
EXTERN integer insertpenalties  ; 
EXTERN boolean outputactive  ;
#if 0	/* local in mainctrl() und appspace() */
EXTERN internalfontnumber mainf  ; 
EXTERN fourquarters maini  ; 
EXTERN fourquarters mainj  ; 
EXTERN fontindex maink  ;
EXTERN halfword mainp  ;
EXTERN integer mains  ;
#endif
#if 0	/* (br) made local in mainctrl() */
EXTERN halfword bchar  ; 
EXTERN halfword falsebchar  ; 
EXTERN boolean cancelboundary  ; 
EXTERN boolean insdisc  ;
#endif
#if 0	/* (br) made local */
EXTERN halfword curbox  ;
#endif
EXTERN halfword aftertoken  ; 
EXTERN boolean longhelpseen  ; 
EXTERN strnumber formatident  ; 
EXTERN wordfile fmtfile  ; 
EXTERN integer readyalready  ; 
EXTERN alphafile writefile[16]  ; 
EXTERN boolean writeopen[18]  ; 
EXTERN halfword writeloc  ; 
EXTERN poolpointer editnamestart  ;
EXTERN integer editnamelength, editline;

#include "coerce.h"

/* -- end -- */
