/* texdefs.h */

#define noprint 16 
#define termonly 17 
#define logonly 18 
#define termandlog 19 
#define pseudo 20 
#define newstring 21 
#define maxselector 21 
#define batchmode 0 
#define nonstopmode 1 
#define scrollmode 2 
#define errorstopmode 3 
#define spotless 0 
#define warningissued 1 
#define errormessageissued 2 
#define fatalerrorstop 3 
#define unity 65536L 
#define two 131072L 
#define infbad 10000 
#define minquarterword 0
#define maxquarterword 255
#define minhalfword 0L 
#if 0  /* nach texd.h verschoben */
#define maxhalfword 65535L
#endif
#define emptyflag maxhalfword 
#define nodesize info 
#define font ztype 
#define character subtype 
#define hlistnode 0 
#define boxnodesize 7 
#define widthoffset 1 
#define depthoffset 2 
#define heightoffset 3 
#define listoffset 5 
#define normal 0 
#define stretching 1 
#define shrinking 2 
#define glueoffset 6 
#define vlistnode 1 
#define rulenode 2 
#define rulenodesize 4 
#define nullflag -1073741824L 
#define insnode 3 
#define insnodesize 5 
#define marknode 4 
#define smallnodesize 2 
#define adjustnode 5 
#define ligaturenode 6 
#define discnode 7 
#define whatsitnode 8 
#define mathnode 9 
#define before 0 
#define after 1 
#define gluenode 10 
#define condmathglue 98 
#define muglue 99 
#define aleaders 100 
#define cleaders 101 
#define xleaders 102 
#define gluespecsize 4 
#define fil 1 
#define fill 2 
#define filll 3 
#define kernnode 11 
#define explicit 1 
#define acckern 2 
#define penaltynode 12 
#define infpenalty infbad 
#define ejectpenalty ( - (integer) infpenalty ) 
#define unsetnode 13 
#define zeroglue membot 
#define filglue ( zeroglue + gluespecsize ) 
#define fillglue ( filglue + gluespecsize ) 
#define ssglue ( fillglue + gluespecsize ) 
#define filnegglue ( ssglue + gluespecsize ) 
#define lomemstatmax ( filnegglue + gluespecsize - 1 ) 
#define pageinshead memtop 
#define contribhead ( memtop - 1 ) 
#define pagehead ( memtop - 2 ) 
#define temphead ( memtop - 3 ) 
#define holdhead ( memtop - 4 ) 
#define adjusthead ( memtop - 5 ) 
#define active ( memtop - 7 ) 
#define alignhead ( memtop - 8 ) 
#define endspan ( memtop - 9 ) 
#define omittemplate ( memtop - 10 ) 
#define nulllist ( memtop - 11 ) 
#define ligtrick ( memtop - 12 ) 
#define garbage ( memtop - 12 ) 
#define backuphead ( memtop - 13 ) 
#define himemstatmin ( memtop - 13 ) 
#define himemstatusage 14 
#define tokenrefcount info 


#define relax		0
#define left_brace	1
#define right_brace	2
#define math_shift	3
#define tab_mark	4
#define car_ret		5
#define out_param	5
#define mac_param	6
#define sup_mark	7
#define sub_mark	8
#define endv		9
#define spacer		10
#define letter		11
#define otherchar	12
#define par_end		13
#define begin_match	13
#define stop		14
#define end_match	14
#define delim_num	15
#define charnum		16
#define math_char_num	17
#define mark		18
#define xray		19
#define make_box	20
#define hmove		21
#define vmove		22
#define un_hbox		23
#define un_vbox		24
#define remove_item	25
#define hskip		26
#define vskip		27
#define mskip		28
#define kern		29
#define mkern		30
#define leader_ship	31
#define halign		32
#define valign		33
#define no_align	34
#define vrule		35
#define hrule		36
#define insert		37
#define vadjust		38
#define ignorespaces	39
#define afterassignment	40
#define aftergroup	41
#define break_penalty	42
#define start_par	43
#define ital_corr	44
#define accent		45
#define math_accent	46
#define discretionary	47
#define eq_no		48
#define left_right	49
#define math_comp	50
#define limit_switch	51
#define above		52
#define math_style	53
#define math_choice	54
#define non_script	55
#define vcenter		56
#define caseshift	57
#define message		58
#define extension	59
#define instream	60
#define begin_group	61
#define end_group	62
#define omit		63
#define exspace		64
#define noboundary	65
#define radical		66
#define end_cs_name	67
#define min_internal	68

#define chargiven	68
#define math_given	69
#define last_item	70

#define max_non_prefixed_command	70

/* 209. */
#define toks_register	71
#define assign_toks	72
#define assign_int	73
#define assign_dimen	74
#define assign_glue	75
#define assign_mu_glue	76
#define assign_font_dimen	77
#define assign_font_int	78
#define set_aux		79
#define set_prev_graf	80
#define set_page_dimen	81
#define set_page_int	82
#define set_box_dimen	83
#define set_shape	84
#define def_code	85
#define def_family	86
#define set_font	87
#define def_font	88
#define register_cmd	89	/* original: register */
#define max_internal	89	/* unused so far */

#define advance		90
#define multiply	91
#define divide		92
#define prefix		93
#define let		94
#define shorthand_def	95
#define read_to_cs	96
#define def		97
#define set_box		98
#define hyph_data	99
#define set_interaction	100

#ifdef MLTEX
#define char_sub_def	101
#define maxcommand	101
#else
#define maxcommand	100
#endif



#if 0
/* Achtung: hmode, mmode ist auch von maxcommand abhaengig ! */
/* TODO: bisher ist noch vmode/hmode/mmode direkt als Zahl im
 * Text, muss noch ersetzt werden
 */
#ifdef MLTEX		/* delete this if you have made TODO */
#define vmode (1)
#define hmode (vmode + 100 + 1)
#define mmode (hmode + 100 + 1)
#else
#define vmode (1)
#define hmode (vmode + maxcommand + 1)
#define mmode (hmode + maxcommand + 1)
#endif

typedef short	mode_type;

#else
/* Damit geht `mode' von -3..+3 */

#define	vmode	1
#define	hmode	2
#define mmode	3

typedef schar	mode_type;	/* wichtig: signed !!! */

#endif


/* 210. */
#define undefinedcs	(maxcommand + 1)
#define expandafter	(maxcommand + 2)
#define noexpand	(maxcommand + 3)
#define input		(maxcommand + 4)
#define iftest		(maxcommand + 5)
#define fiorelse	(maxcommand + 6)
#define csname		(maxcommand + 7)
#define convert		(maxcommand + 8)
#define the		(maxcommand + 9)
#define topbotmark	(maxcommand + 10)
#define call		(maxcommand + 11)
#define longcall	(maxcommand + 12)
#define outercall	(maxcommand + 13)
#define longoutercall	(maxcommand + 14)
#define endtemplate	(maxcommand + 15)
#define dontexpand	(maxcommand + 16)
#define glueref		(maxcommand + 17)
#define shaperef	(maxcommand + 18)
#define boxref		(maxcommand + 19)
#define data		(maxcommand + 20)

/* 212. */
#define ignoredepth ( -65536000L ) 


#define levelzero 0 
#define levelone ( levelzero + 1 ) 


#define activebase 1 
#define singlebase ( activebase + 256 ) 
#ifdef MLTEX
#define char_sub_base ( singlebase + 256 )
#define nullcs ( char_sub_base + 256 )
#else
#define nullcs ( singlebase + 256 )
#endif
#define hashbase ( nullcs + 1 ) 
#define frozencontrolsequence ( hashbase + hashsize ) 
#define frozenprotection ( frozencontrolsequence ) 
#define frozencr ( frozencontrolsequence + 1 ) 
#define frozenendgroup ( frozencontrolsequence + 2 ) 
#define frozenright ( frozencontrolsequence + 3 ) 
#define frozenfi ( frozencontrolsequence + 4 ) 
#define frozenendtemplate ( frozencontrolsequence + 5 ) 
#define frozenendv ( frozencontrolsequence + 6 ) 
#define frozenrelax ( frozencontrolsequence + 7 ) 
#define endwrite ( frozencontrolsequence + 8 ) 
#define frozendontexpand ( frozencontrolsequence + 9 ) 
#define frozennullfont ( frozencontrolsequence + 10 ) 
#define fontidbase ( frozennullfont - 0 ) 
#define undefinedcontrolsequence ( frozennullfont + 257 ) 
#define gluebase ( undefinedcontrolsequence + 1 ) 


#define lineskipcode 0 
#define baselineskipcode 1 
#define parskipcode 2 
#define abovedisplayskipcode 3 
#define belowdisplayskipcode 4 
#define abovedisplayshortskipcode 5 
#define belowdisplayshortskipcode 6 
#define leftskipcode 7 
#define rightskipcode 8 
#define topskipcode 9 
#define splittopskipcode 10 
#define tabskipcode 11 
#define spaceskipcode 12 
#define xspaceskipcode 13 
#define parfillskipcode 14 
#define thinmuskipcode 15 
#define medmuskipcode 16 
#define thickmuskipcode 17 
#define gluepars 18 
#define skipbase ( gluebase + gluepars ) 
#define muskipbase ( skipbase + 256 ) 
#define localbase ( muskipbase + 256 ) 

#define parshapeloc localbase 
#define outputroutineloc ( localbase + 1 ) 
#define everyparloc ( localbase + 2 ) 
#define everymathloc ( localbase + 3 ) 
#define everydisplayloc ( localbase + 4 ) 
#define everyhboxloc ( localbase + 5 ) 
#define everyvboxloc ( localbase + 6 ) 
#define everyjobloc ( localbase + 7 ) 
#define everycrloc ( localbase + 8 ) 
#define errhelploc ( localbase + 9 ) 
#define toksbase ( localbase + 10 ) 
#define boxbase ( toksbase + 256 ) 
#define curfontloc ( boxbase + 256 ) 
#define mathfontbase ( curfontloc + 1 ) 
#define catcodebase ( mathfontbase + 48 ) 
#define lccodebase ( catcodebase + 256 ) 
#define uccodebase ( lccodebase + 256 ) 
#define sfcodebase ( uccodebase + 256 ) 
#define mathcodebase ( sfcodebase + 256 ) 
#define intbase ( mathcodebase + 256 ) 

#define nullfont 0 
#define varcode 28672 
#define pretolerancecode 0 
#define tolerancecode 1 
#define linepenaltycode 2 
#define hyphenpenaltycode 3 
#define exhyphenpenaltycode 4 
#define clubpenaltycode 5 
#define widowpenaltycode 6 
#define displaywidowpenaltycode 7 
#define brokenpenaltycode 8 
#define binoppenaltycode 9 
#define relpenaltycode 10 
#define predisplaypenaltycode 11 
#define postdisplaypenaltycode 12 
#define interlinepenaltycode 13 
#define doublehyphendemeritscode 14 
#define finalhyphendemeritscode 15 
#define adjdemeritscode 16 
#define magcode 17 
#define delimiterfactorcode 18 
#define loosenesscode 19 
#define timecode 20 
#define daycode 21 
#define monthcode 22 
#define yearcode 23 
#define showboxbreadthcode 24 
#define showboxdepthcode 25 
#define hbadnesscode 26 
#define vbadnesscode 27 
#define pausingcode 28 
#define tracingonlinecode 29 
#define tracingmacroscode 30 
#define tracingstatscode 31 
#define tracingparagraphscode 32 
#define tracingpagescode 33 
#define tracingoutputcode 34 
#define tracinglostcharscode 35 
#define tracingcommandscode 36 
#define tracingrestorescode 37 
#define uchyphcode 38 
#define outputpenaltycode 39 
#define maxdeadcyclescode 40 
#define hangaftercode 41 
#define floatingpenaltycode 42 
#define globaldefscode 43 
#define curfamcode 44 
#define escapecharcode 45 
#define defaulthyphencharcode 46 
#define defaultskewcharcode 47 
#define endlinecharcode 48 
#define newlinecharcode 49 
#define languagecode 50 
#define lefthyphenmincode 51 
#define righthyphenmincode 52 
#define holdinginsertscode 53 
#define errorcontextlinescode 54
#ifdef MLTEX
#define char_sub_def_max_code		55
#define tracing_char_sub_def_code	56
#ifdef ERW_INTERACTION
#define interactionmodecode	57
#define intpars			58
#else
#define intpars				57
#endif
#else 
#ifdef ERW_INTERACTION
#define interactionmodecode	55
#define intpars			56
#else
#define intpars 55
#endif
#endif
#define countbase ( intbase + intpars ) 
#define delcodebase ( countbase + 256 ) 
#define dimenbase ( delcodebase + 256 ) 
#define parindentcode 0 
#define mathsurroundcode 1 
#define lineskiplimitcode 2 
#define hsizecode 3 
#define vsizecode 4 
#define maxdepthcode 5 
#define splitmaxdepthcode 6 
#define boxmaxdepthcode 7 
#define hfuzzcode 8 
#define vfuzzcode 9 
#define delimitershortfallcode 10 
#define nulldelimiterspacecode 11 
#define scriptspacecode 12 
#define predisplaysizecode 13 
#define displaywidthcode 14 
#define displayindentcode 15 
#define overfullrulecode 16 
#define hangindentcode 17 
#define hoffsetcode 18 
#define voffsetcode 19 
#define emergencystretchcode 20 
#define dimenpars 21 
#define scaledbase ( dimenbase + dimenpars ) 
#define eqtbsize ( scaledbase + 255 ) 
#define restoreoldvalue 0 
#define restorezero 1 
#define inserttoken 2 
#define levelboundary 3 
#define bottomlevel 0 
#define simplegroup 1 
#define hboxgroup 2 
#define adjustedhboxgroup 3 
#define vboxgroup 4 
#define vtopgroup 5 
#define aligngroup 6 
#define noaligngroup 7 
#define outputgroup 8 
#define mathgroup 9 
#define discgroup 10 
#define insertgroup 11 
#define vcentergroup 12 
#define mathchoicegroup 13 
#define semisimplegroup 14 
#define mathshiftgroup 15 
#define mathleftgroup 16 
#define maxgroupcode 16 
#define cstokenflag 4095 
#define leftbracetoken 256 
#define leftbracelimit 512 
#define rightbracetoken 512 
#define rightbracelimit 768 
#define mathshifttoken 768 
#define tabtoken 1024 
#define outparamtoken 1280 
#define spacetoken 2592 
#define lettertoken 2816 
#define othertoken 3072 
#define matchtoken 3328 
#define endmatchtoken 3584 
#define skipping 1 
#define defining 2 
#define matching 3 
#define aligning 4 
#define absorbing 5 
#define tokenlist 0 
#define parameter 0 
#define utemplate 1 
#define vtemplate 2 
#define backedup 3 
#define inserted 4 
#define macro 5 
#define outputtext 6 
#define everypartext 7 
#define everymathtext 8 
#define everydisplaytext 9 
#define everyhboxtext 10 
#define everyvboxtext 11 
#define everyjobtext 12 
#define everycrtext 13 
#define marktext 14 
#define writetext 15 
#define noexpandflag 257 
#define topmarkcode 0 
#define firstmarkcode 1 
#define botmarkcode 2 
#define splitfirstmarkcode 3 
#define splitbotmarkcode 4 
#define intval 0 
#define dimenval 1 
#define glueval 2 
#define muval 3 
#define identval 4 
#define tokval 5 
#define inputlinenocode ( glueval + 1 ) 
#define badnesscode ( glueval + 2 ) 
#define maxdimen 1073741823L 
#define octaltoken ( othertoken + 39 ) 
#define hextoken ( othertoken + 34 ) 
#define alphatoken ( othertoken + 96 ) 
#define pointtoken ( othertoken + 46 ) 
#define continentalpointtoken ( othertoken + 44 ) 
#define infinity 2147483647L 
#define zerotoken ( othertoken + 48 ) 
#define Atoken ( lettertoken + 65 ) 
#define otherAtoken ( othertoken + 65 ) 
#define defaultrule 26214 
#define numbercode 0 
#define romannumeralcode 1 
#define stringcode 2 
#define meaningcode 3 
#define fontnamecode 4 
#define jobnamecode 5 
#define closed 2 
#define justopen 1 
#define ifcharcode 0 
#define ifcatcode 1 
#define ifintcode 2 
#define ifdimcode 3 
#define ifoddcode 4 
#define ifvmodecode 5 
#define ifhmodecode 6 
#define ifmmodecode 7 
#define ifinnercode 8 
#define ifvoidcode 9 
#define ifhboxcode 10 
#define ifvboxcode 11 
#define ifxcode 12 
#define ifeofcode 13 
#define iftruecode 14 
#define iffalsecode 15 
#define ifcasecode 16 
#define ifnodesize 2 
#define ifcode 1 
#define ficode 2 
#define elsecode 3 
#define orcode 4 

#define stopflag 128 
#define kernflag 128 
#define slantcode 1 
#define spacecode 2 
#define spacestretchcode 3 
#define spaceshrinkcode 4 
#define xheightcode 5 
#define quadcode 6 
#define extraspacecode 7 
#define nonchar 256 
#define nonaddress 0x7fffffffL			/* fontmemsize */
#define kernbaseoffset (256L * 128)

#define setchar0 0 
#define set1 128 
#define setrule 132 
#define putrule 137 
#define nop 138 
#define bop 139 
#define eop 140 
#define push 141 
#define pop 142 
#define right1 143 
#define w0 147 
#define w1 148 
#define x0 152 
#define x1 153 
#define down1 157 
#define y0 161 
#define y1 162 
#define z0 166 
#define z1 167 
#define fntnum0 171 
#define fnt1 235 
#define xxx1 239 
#define xxx4 242 
#define fntdef1 243 
#define pre 247 
#define post 248 
#define postpost 249

#define idbyte 2


#define movementnodesize 3 
#define noadsize 4 
#define mathchar 1 
#define subbox 2 
#define submlist 3 
#define mathtextchar 4 
#define ordnoad ( unsetnode + 3 ) 
#define opnoad ( ordnoad + 1 ) 
#define binnoad ( ordnoad + 2 ) 
#define relnoad ( ordnoad + 3 ) 
#define opennoad ( ordnoad + 4 ) 
#define closenoad ( ordnoad + 5 ) 
#define punctnoad ( ordnoad + 6 ) 
#define innernoad ( ordnoad + 7 ) 
#define limits 1 
#define nolimits 2 
#define radicalnoad ( innernoad + 1 ) 
#define radicalnoadsize 5 
#define fractionnoad ( radicalnoad + 1 ) 
#define fractionnoadsize 6 
#define defaultcode 1073741824L 
#define undernoad ( fractionnoad + 1 ) 
#define overnoad ( undernoad + 1 ) 
#define accentnoad ( overnoad + 1 ) 
#define accentnoadsize 5 
#define vcenternoad ( accentnoad + 1 ) 
#define leftnoad ( vcenternoad + 1 ) 
#define rightnoad ( leftnoad + 1 ) 
#define stylenode ( unsetnode + 1 ) 
#define stylenodesize 3 
#define displaystyle 0 
#define textstyle 2 
#define scriptstyle 4 
#define scriptscriptstyle 6 
#define cramped 1 
#define choicenode ( unsetnode + 2 ) 
#define textsize 0 
#define scriptsize 16 
#define scriptscriptsize 32 
#define totalmathsyparams 22 
#define totalmathexparams 13 
#define alignstacknodesize 5 
#define spancode 256 
#define crcode 257 
#define crcrcode ( crcode + 1 ) 
#define endtemplatetoken ( cstokenflag + frozenendtemplate ) 
#define spannodesize 2 
#define activenodesize 3 
#define unhyphenated 0 
#define hyphenated 1 
#define lastactive active 
#define passivenodesize 2 
#define deltanodesize 7 
#define deltanode 2 
#define awfulbad 1073741823L 

#define insertsonly 1 
#define boxthere 2 
#define pageinsnodesize 4 
#define inserting 0 
#define splitup 1 

/* 1341. */
#define writenodesize	2
#define opennodesize	3

#define opennode	0
#define writenode	1
#define closenode	2
#define specialnode	3
#define languagenode	4

/* other defs,  hand-coded */

#include "defs.h"

/* end */
