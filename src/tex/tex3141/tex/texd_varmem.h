/* texd.h with variable memory size hack - for TeX engine 3.141 only */

#undef	TRIP
#undef	TRAP
#define	STAT
#undef	DEBUG
#include "tex.h"

#define VARMEM
#define DCC_NP 13

#ifdef IN_EXTRA_C
char *dcc_pname[DCC_NP] = { "memmax" , "bufsize", "stacksize", "maxopenin", "fontmax",
			    "fontmemsize", "maxstrings", "poolsize", "savesize",
                            "triesize", "dvibufsize", "memtop", "trieopsize" };

int dcc_pval[DCC_NP] = { 30000, 3000, 300, 6, 75,
                         72000, 7500, 32000, 500,
                         8000, 1000, 30000, 800 }; 
#else
extern int dcc_pval[DCC_NP];
#endif

#define dcc_texstrings 24000

#define memmax (dcc_pval[0]) 
#define memmin 0 
#define bufsize (dcc_pval[1])
#define errorline 79 
#define halferrorline 50 
#define maxprintline 79 
#define stacksize (dcc_pval[2]) 
#define maxinopen (dcc_pval[3]) 
#define fontmax (dcc_pval[4])
#define fontmemsize (dcc_pval[5])
#define paramsize 100 
#define nestsize 40 
#define maxstrings (dcc_pval[6]) 
#define stringvacancies ((dcc_pval[7]) - dcc_texstrings) 
#define poolsize (dcc_pval[7])
#define savesize (dcc_pval[8])
#define triesize (dcc_pval[9])
#define trieopsize (dcc_pval[12]) 
#define negtrieopsize (-(trieopsize)) 
#define dvibufsize (8 * dcc_pval[10]) 
#define poolname "tex.pool" 
#define memtop dcc_pval[11]
typedef unsigned char ASCIIcode  ; 
typedef unsigned char eightbits  ; 
typedef integer poolpointer  ; 
typedef integer strnumber  ; 
typedef unsigned char packedASCIIcode  ; 
typedef integer scaled  ; 
typedef integer nonnegativeinteger  ; 
typedef schar smallnumber  ; 
typedef short quarterword  ; 
typedef integer halfword  ; 
typedef schar twochoices  ; 
typedef schar fourchoices  ; 
#include "memory.h"
typedef schar glueord  ; 
typedef struct {
    short modefield ; 
  halfword headfield, tailfield ; 
  integer pgfield, mlfield ; 
  memoryword auxfield ; 
  quarterword lhmfield, rhmfield ; 
} liststaterecord  ; 
typedef schar groupcode  ; 
typedef struct {
    quarterword statefield, indexfield ; 
  halfword startfield, locfield, limitfield, namefield ; 
} instaterecord  ; 
typedef integer internalfontnumber  ; 
typedef integer fontindex  ; 
typedef integer dviindex  ; 
typedef integer triepointer  ; 
typedef short hyphpointer  ; 
EXTERN integer bad  ; 
EXTERN ASCIIcode xord[256]  ; 
EXTERN ASCIIcode xchr[256]  ; 
EXTERN char nameoffile[FILENAMESIZE + 1], realnameoffile[FILENAMESIZE + 1]  ; 
EXTERN integer namelength  ; 
EXTERN ASCIIcode *buffer;
EXTERN integer first  ; 
EXTERN integer last  ; 
EXTERN integer maxbufstack  ; 
EXTERN packedASCIIcode *strpool; 
EXTERN poolpointer *strstart  ; 
EXTERN poolpointer poolptr  ; 
EXTERN strnumber strptr  ; 
EXTERN poolpointer initpoolptr  ; 
EXTERN strnumber initstrptr  ; 
#ifdef INITEX
EXTERN alphafile poolfile  ; 
#endif /* INITEX */
EXTERN alphafile logfile  ; 
EXTERN schar selector  ; 
EXTERN schar dig[23]  ; 
EXTERN integer tally  ; 
EXTERN integer termoffset  ; 
EXTERN integer fileoffset  ; 
EXTERN ASCIIcode trickbuf[errorline + 1]  ; 
EXTERN integer trickcount  ; 
EXTERN integer firstcount  ; 
EXTERN schar interaction  ; 
EXTERN boolean deletionsallowed  ; 
EXTERN boolean setboxallowed  ; 
EXTERN schar history  ; 
EXTERN schar errorcount  ; 
EXTERN strnumber helpline[6]  ; 
EXTERN schar helpptr  ; 
EXTERN boolean useerrhelp  ; 
EXTERN integer interrupt  ; 
EXTERN boolean OKtointerrupt  ; 
EXTERN boolean aritherror  ; 
EXTERN scaled remainder  ; 
EXTERN halfword tempptr  ; 
#define zmem (zzzaa - (int)(memmin))
EXTERN memoryword *zzzaa ; 
EXTERN halfword lomemmax  ; 
EXTERN halfword himemmin  ; 
EXTERN integer varused, dynused  ; 
EXTERN halfword avail  ; 
EXTERN halfword memend  ; 
EXTERN halfword rover  ; 
#ifdef DEBUG
EXTERN boolean 
#define freearr (zzzab - (int)(memmin))
  *zzzab  ; 
EXTERN boolean 
#define wasfree (zzzac - (int)(memmin))
  *zzzac;
EXTERN halfword wasmemend, waslomax, washimin  ; 
EXTERN boolean panicking  ; 
#endif /* DEBUG */
EXTERN integer fontinshortdisplay  ; 
EXTERN integer depththreshold  ; 
EXTERN integer breadthmax  ; 
EXTERN liststaterecord nest[nestsize + 1]  ; 
EXTERN integer nestptr  ; 
EXTERN integer maxneststack  ; 
EXTERN liststaterecord curlist  ; 
EXTERN short shownmode  ; 
EXTERN schar oldsetting  ; 
EXTERN memoryword zeqtb[13507]  ; 
EXTERN quarterword 
#define xeqlevel (zzzad -12663)
  zzzad[844]  ; 
EXTERN twohalves 
#define hash (zzzae -514)
  zzzae[9767]  ; 
EXTERN halfword hashused  ; 
EXTERN boolean nonewcontrolsequence  ; 
EXTERN integer cscount  ; 
EXTERN memoryword *savestack;
EXTERN integer saveptr  ; 
EXTERN integer maxsavestack  ; 
EXTERN quarterword curlevel  ; 
EXTERN groupcode curgroup  ; 
EXTERN integer curboundary  ; 
EXTERN integer magset  ; 
EXTERN eightbits curcmd  ; 
EXTERN halfword curchr  ; 
EXTERN halfword curcs  ; 
EXTERN halfword curtok  ; 
EXTERN instaterecord *inputstack;
EXTERN integer inputptr  ; 
EXTERN integer maxinstack  ; 
EXTERN instaterecord curinput  ; 
EXTERN integer inopen  ; 
EXTERN integer openparens  ; 
EXTERN alphafile *inputfile; 
EXTERN integer line  ; 
EXTERN integer *linestack;
EXTERN schar scannerstatus  ; 
EXTERN halfword warningindex  ; 
EXTERN halfword defref  ; 
EXTERN halfword paramstack[paramsize + 1]  ; 
EXTERN integer paramptr  ; 
EXTERN integer maxparamstack  ; 
EXTERN integer alignstate  ; 
EXTERN integer baseptr  ; 
EXTERN halfword parloc  ; 
EXTERN halfword partoken  ; 
EXTERN boolean forceeof  ; 
EXTERN halfword curmark[5]  ; 
EXTERN schar longstate  ; 
EXTERN halfword pstack[9]  ; 
EXTERN integer curval  ; 
EXTERN schar curvallevel  ; 
EXTERN smallnumber radix  ; 
EXTERN glueord curorder  ; 
EXTERN alphafile readfile[16]  ; 
EXTERN schar readopen[17]  ; 
EXTERN halfword condptr  ; 
EXTERN schar iflimit  ; 
EXTERN smallnumber curif  ; 
EXTERN integer ifline  ; 
EXTERN integer skipline  ; 
EXTERN strnumber curname  ; 
EXTERN strnumber curarea  ; 
EXTERN strnumber curext  ; 
EXTERN poolpointer areadelimiter  ; 
EXTERN poolpointer extdelimiter  ; 
EXTERN ccharpointer TEXformatdefault  ; 
EXTERN boolean nameinprogress  ; 
EXTERN strnumber jobname  ; 
EXTERN boolean logopened  ; 
EXTERN bytefile dvifile  ; 
EXTERN strnumber outputfilename  ; 
EXTERN strnumber logname  ; 
EXTERN bytefile tfmfile  ; 
EXTERN memoryword *fontinfo  ; 
EXTERN fontindex fmemptr  ; 
EXTERN internalfontnumber fontptr  ; 
EXTERN fourquarters *fontcheck ; 
EXTERN scaled *fontsize; 
EXTERN scaled *fontdsize; 
EXTERN halfword *fontparams; 
EXTERN strnumber *fontname;
EXTERN strnumber *fontarea;
EXTERN eightbits *fontbc;
EXTERN eightbits *fontec;
EXTERN halfword *fontglue;
EXTERN boolean *fontused;
EXTERN integer *hyphenchar;
EXTERN integer *skewchar;
EXTERN fontindex *bcharlabel;
EXTERN short *fontbchar;
EXTERN short *fontfalsebchar;
EXTERN integer *charbase  ; 
EXTERN integer *widthbase  ; 
EXTERN integer *heightbase  ; 
EXTERN integer *depthbase  ; 
EXTERN integer *italicbase  ; 
EXTERN integer *ligkernbase  ; 
EXTERN integer *kernbase  ; 
EXTERN integer *extenbase  ; 
EXTERN integer *parambase  ; 
EXTERN fourquarters nullcharacter  ; 
EXTERN integer totalpages  ; 
EXTERN scaled maxv  ; 
EXTERN scaled maxh  ; 
EXTERN integer maxpush  ; 
EXTERN integer lastbop  ; 
EXTERN integer deadcycles  ; 
EXTERN boolean doingleaders  ; 
EXTERN quarterword c, f  ; 
EXTERN scaled ruleht, ruledp, rulewd  ; 
EXTERN halfword g  ; 
EXTERN integer lq, lr  ; 
EXTERN eightbits *dvibuf;
EXTERN dviindex halfbuf  ; 
EXTERN dviindex dvilimit  ; 
EXTERN dviindex dviptr  ; 
EXTERN integer dvioffset  ; 
EXTERN integer dvigone  ; 
EXTERN halfword downptr, rightptr  ; 
EXTERN scaled dvih, dviv  ; 
EXTERN scaled curh, curv  ; 
EXTERN internalfontnumber dvif  ; 
EXTERN integer curs  ; 
EXTERN scaled totalstretch[4], totalshrink[4]  ; 
EXTERN integer lastbadness  ; 
EXTERN halfword adjusttail  ; 
EXTERN integer packbeginline  ; 
EXTERN twohalves emptyfield  ; 
EXTERN fourquarters nulldelimiter  ; 
EXTERN halfword curmlist  ; 
EXTERN smallnumber curstyle  ; 
EXTERN smallnumber cursize  ; 
EXTERN scaled curmu  ; 
EXTERN boolean mlistpenalties  ; 
EXTERN internalfontnumber curf  ; 
EXTERN quarterword curc  ; 
EXTERN fourquarters curi  ; 
EXTERN integer magicoffset  ; 
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
EXTERN boolean secondpass  ; 
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
EXTERN integer lhyf, rhyf  ; 
EXTERN halfword hyfbchar  ; 
EXTERN schar hyf[65]  ; 
EXTERN halfword initlist  ; 
EXTERN boolean initlig  ; 
EXTERN boolean initlft  ; 
EXTERN smallnumber hyphenpassed  ; 
EXTERN halfword curl, curr  ; 
EXTERN halfword curq  ; 
EXTERN halfword ligstack  ; 
EXTERN boolean ligaturepresent  ; 
EXTERN boolean lfthit, rthit  ; 
EXTERN twohalves *trie;
EXTERN smallnumber *hyfdistance;
EXTERN smallnumber *hyfnum;
EXTERN quarterword *hyfnext; 
EXTERN integer opstart[256]  ; 
EXTERN strnumber hyphword[608]  ; 
EXTERN halfword hyphlist[608]  ; 
EXTERN hyphpointer hyphcount  ; 
#ifdef INITEX
EXTERN integer 
#define trieophash (zzzaf - (int)(negtrieopsize))
  *zzzaf  ; 
EXTERN quarterword trieused[256]  ; 
EXTERN ASCIIcode *trieoplang  ; 
EXTERN quarterword *trieopval  ; 
EXTERN integer trieopptr  ; 
#endif /* INITEX */
#ifdef INITEX
EXTERN packedASCIIcode *triec; 
EXTERN quarterword *trieo;
EXTERN triepointer *triel; 
EXTERN triepointer *trier;
EXTERN triepointer trieptr  ; 
EXTERN triepointer *triehash;
#endif /* INITEX */
#ifdef INITEX
EXTERN boolean *trietaken;
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
EXTERN internalfontnumber mainf  ; 
EXTERN fourquarters maini  ; 
EXTERN fourquarters mainj  ; 
EXTERN fontindex maink  ; 
EXTERN halfword mainp  ; 
EXTERN integer mains  ; 
EXTERN halfword bchar  ; 
EXTERN halfword falsebchar  ; 
EXTERN boolean cancelboundary  ; 
EXTERN boolean insdisc  ; 
EXTERN halfword curbox  ; 
EXTERN halfword aftertoken  ; 
EXTERN boolean longhelpseen  ; 
EXTERN strnumber formatident  ; 
EXTERN wordfile fmtfile  ; 
EXTERN integer readyalready  ; 
EXTERN alphafile writefile[16]  ; 
EXTERN boolean writeopen[18]  ; 
EXTERN halfword writeloc  ; 
EXTERN poolpointer editnamestart  ; 
EXTERN integer editnamelength, editline, tfmtemp  ; 

#include "coerce.h"
