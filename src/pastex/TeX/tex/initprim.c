#define EXTERN extern
#include "texd.h"

#ifdef INITEX

static long_halfword
primitive ( strnumber s, quarterword c, halfword o )
{ primitive_regmem 
  register poolpointer k;
  register smallnumber j;
  register smallnumber l;
  register long_halfword r_curval;

  if ( s < 256 ) 
    r_curval = s + singlebase;
  else {
    k = strstart [ s ];
    l = strstart [ s + 1 ] - k;
    for( j = 0 ; j <= l - 1 ; j++ )
      buffer [ j ] = strpool [ k + j ];
    r_curval = idlookup ( 0 , l );
    flushstring;
    ztext ( r_curval ) = s;
  }
  eqlevel ( r_curval ) = levelone;
  eqtype ( r_curval ) = c;
  equiv ( r_curval ) = o;
  return r_curval;
}


struct prim_desc_str {
  unsigned short s;		/* is strnumber */
  quarterword c;
  unsigned short o;		/* is halfword */
} prim_desc[] = {
	372,	assign_glue,	gluebase + lineskipcode,
	373,	assign_glue,	gluebase + baselineskipcode,
	374,	assign_glue,	gluebase + parskipcode,
	375,	assign_glue,	gluebase + abovedisplayskipcode,
	376,	assign_glue,	gluebase + belowdisplayskipcode,
	377,	assign_glue,	gluebase + abovedisplayshortskipcode,
	378,	assign_glue,	gluebase + belowdisplayshortskipcode,
	379,	assign_glue,	gluebase + leftskipcode,
	380,	assign_glue,	gluebase + rightskipcode,
	381,	assign_glue,	gluebase + topskipcode,
	382,	assign_glue,	gluebase + splittopskipcode,
	383,	assign_glue,	gluebase + tabskipcode,
	384,	assign_glue,	gluebase + spaceskipcode,
	385,	assign_glue,	gluebase + xspaceskipcode,
	386,	assign_glue,	gluebase + parfillskipcode,
	387,	assign_mu_glue,	gluebase + thinmuskipcode,
	388,	assign_mu_glue,	gluebase + medmuskipcode,
	389,	assign_mu_glue,	gluebase + thickmuskipcode,
	394,	assign_toks,	outputroutineloc,
	395,	assign_toks,	everyparloc,
	396,	assign_toks,	everymathloc,
	397,	assign_toks,	everydisplayloc,
	398,	assign_toks,	everyhboxloc,
	399,	assign_toks,	everyvboxloc,
	400,	assign_toks,	everyjobloc,
	401,	assign_toks,	everycrloc,
	402,	assign_toks,	errhelploc,
	416,	assign_int,	intbase + pretolerancecode,
	417,	assign_int,	intbase + tolerancecode,
	418,	assign_int,	intbase + linepenaltycode,
	419,	assign_int,	intbase + hyphenpenaltycode,
	420,	assign_int,	intbase + exhyphenpenaltycode,
	421,	assign_int,	intbase + clubpenaltycode,
	422,	assign_int,	intbase + widowpenaltycode,
	423,	assign_int,	intbase + displaywidowpenaltycode,
	424,	assign_int,	intbase + brokenpenaltycode,
	425,	assign_int,	intbase + binoppenaltycode,
	426,	assign_int,	intbase + relpenaltycode,
	427,	assign_int,	intbase + predisplaypenaltycode,
	428,	assign_int,	intbase + postdisplaypenaltycode,
	429,	assign_int,	intbase + interlinepenaltycode,
	430,	assign_int,	intbase + doublehyphendemeritscode,
	431,	assign_int,	intbase + finalhyphendemeritscode,
	432,	assign_int,	intbase + adjdemeritscode,
	433,	assign_int,	intbase + magcode,
	434,	assign_int,	intbase + delimiterfactorcode,
	435,	assign_int,	intbase + loosenesscode,
	436,	assign_int,	intbase + timecode,
	437,	assign_int,	intbase + daycode,
	438,	assign_int,	intbase + monthcode,
	439,	assign_int,	intbase + yearcode,
	440,	assign_int,	intbase + showboxbreadthcode,
	441,	assign_int,	intbase + showboxdepthcode,
	442,	assign_int,	intbase + hbadnesscode,
	443,	assign_int,	intbase + vbadnesscode,
	444,	assign_int,	intbase + pausingcode,
	445,	assign_int,	intbase + tracingonlinecode,
	446,	assign_int,	intbase + tracingmacroscode,
	447,	assign_int,	intbase + tracingstatscode,
	448,	assign_int,	intbase + tracingparagraphscode,
	449,	assign_int,	intbase + tracingpagescode,
	450,	assign_int,	intbase + tracingoutputcode,
	451,	assign_int,	intbase + tracinglostcharscode,
	452,	assign_int,	intbase + tracingcommandscode,
	453,	assign_int,	intbase + tracingrestorescode,
	454,	assign_int,	intbase + uchyphcode,
	455,	assign_int,	intbase + outputpenaltycode,
	456,	assign_int,	intbase + maxdeadcyclescode,
	457,	assign_int,	intbase + hangaftercode,
	458,	assign_int,	intbase + floatingpenaltycode,
	459,	assign_int,	intbase + globaldefscode,
	460,	assign_int,	intbase + curfamcode,
	461,	assign_int,	intbase + escapecharcode,
	462,	assign_int,	intbase + defaulthyphencharcode,
	463,	assign_int,	intbase + defaultskewcharcode,
	464,	assign_int,	intbase + endlinecharcode,
	465,	assign_int,	intbase + newlinecharcode,
	466,	assign_int,	intbase + languagecode,
	467,	assign_int,	intbase + lefthyphenmincode,
	468,	assign_int,	intbase + righthyphenmincode,
	469,	assign_int,	intbase + holdinginsertscode,
	470,	assign_int,	intbase + errorcontextlinescode,
#if 0 /* MLTEX */
	STR_CHARSUBDEFMAX,	assign_int, intbase + char_sub_def_max_code,
	STR_TRACING_CHARSUBDEF,	assign_int, intbase + tracing_char_sub_def_code,
#endif
	474,	assign_dimen,	dimenbase + parindentcode,
	475,	assign_dimen,	dimenbase + mathsurroundcode,
	476,	assign_dimen,	dimenbase + lineskiplimitcode,
	477,	assign_dimen,	dimenbase + hsizecode,
	478,	assign_dimen,	dimenbase + vsizecode,
	479,	assign_dimen,	dimenbase + maxdepthcode,
	480,	assign_dimen,	dimenbase + splitmaxdepthcode,
	481,	assign_dimen,	dimenbase + boxmaxdepthcode,
	482,	assign_dimen,	dimenbase + hfuzzcode,
	483,	assign_dimen,	dimenbase + vfuzzcode,
	484,	assign_dimen,	dimenbase + delimitershortfallcode,
	485,	assign_dimen,	dimenbase + nulldelimiterspacecode,
	486,	assign_dimen,	dimenbase + scriptspacecode,
	487,	assign_dimen,	dimenbase + predisplaysizecode,
	488,	assign_dimen,	dimenbase + displaywidthcode,
	489,	assign_dimen,	dimenbase + displayindentcode,
	490,	assign_dimen,	dimenbase + overfullrulecode,
	491,	assign_dimen,	dimenbase + hangindentcode,
	492,	assign_dimen,	dimenbase + hoffsetcode,
	493,	assign_dimen,	dimenbase + voffsetcode,
	494,	assign_dimen,	dimenbase + emergencystretchcode,
	32,	exspace,	0,
	47,	ital_corr,	0,
	504,	accent,		0,
	505,	advance,	0,
	506,	afterassignment,	0,
	507,	aftergroup,		0,
	508,	begin_group,	0,
	509,	charnum,	0,
	500,	csname,		0,
	510,	delim_num,	0,
	511,	divide,		0,
	STR_ENDCSNAME,	end_cs_name,	0,
	STR_ENDGROUP,	end_group,	0,
   /* frozenendgroup ... */
	0,	1,	0,
	513,	expandafter,	0,
	514,	def_font,	0,
	515,	assign_font_dimen,	0,
	516,	halign,		0,
	517,	hrule,		0,
	518,	ignorespaces,	0,
	STR_INSERT,	insert,	0,
	348,	mark,		0,
	519,	math_accent,	0,
	520,	math_char_num,	0,
	521,	math_choice,	0,
	522,	multiply,	0,
	523,	no_align,	0,
	524,	noboundary,	0,
	525,	noexpand,	0,
	332,	non_script,	0,
	526,	omit,		0,
	404,	set_shape,	0,
	527,	break_penalty,	0,
	528,	set_prev_graf,	0,
	529,	radical,	0,
	STR_READ,	read_to_cs,	0,
	531,	relax,	256,
  /* frozenrelax ... */
	0,	2,	0,
	532,	set_box,	0,
	533,	the,		0,
	403,	toks_register,	0,
	349,	vadjust,	0,
	534,	valign,		0,
	535,	vcenter,	0,
	536,	vrule,		0,
#if 0  /* MLTEX */
	STR_CHARSUBDEF, char_sub_def, 0,
#endif
	STR_PAR,	par_end,	256,
  /* parloc, partoken ... */
	0,	3,	0,
	625,	input,	0,
	626,	input,	1,
	627,	topbotmark,	topmarkcode,
	628,	topbotmark,	firstmarkcode,
	629,	topbotmark,	botmarkcode,
	630,	topbotmark,	splitfirstmarkcode,
	631,	topbotmark,	splitbotmarkcode,
	472,	register_cmd,	intval,
	496,	register_cmd,	dimenval,
	STR_SKIP,	register_cmd,	glueval,
	392,	register_cmd,	muval,

	STR_SPACEFACTOR,	set_aux,	hmode,
	STR_PREVDEPTH,		set_aux,	vmode,
	STR_DEADCYCLES,		set_page_int,	0,
	STR_INSERTPENALTIES,	set_page_int,	1,

	STR_WD,			set_box_dimen,	widthoffset,
	STR_HT,			set_box_dimen,	heightoffset,
	STR_DP,			set_box_dimen,	depthoffset,

	STR_LASTPENALTY,	last_item,	intval,
	STR_LASTKERN,		last_item,	dimenval,
	STR_LASTSKIP,		last_item,	glueval,
	STR_INPUTLINENO,	last_item,	inputlinenocode,
	STR_BADNESS,		last_item,	badnesscode,

	STR_NUMBER,		convert,	numbercode,
	STR_ROMANNUMERAL,	convert,	romannumeralcode,
	STR_STRING,		convert,	stringcode,
	STR_MEANING,		convert,	meaningcode,
	STR_FONTNAME,		convert,	fontnamecode,
	STR_JOBNAME,		convert,	jobnamecode,

	STR_IF,		iftest,	ifcharcode,
	STR_IFCAT,	iftest,	ifcatcode,
	STR_IFNUM,	iftest,	ifintcode,
	STR_IFDIM,	iftest,	ifdimcode,
	STR_IFODD,	iftest,	ifoddcode,
	STR_IFVMODE,	iftest,	ifvmodecode,
	STR_IFHMODE,	iftest,	ifhmodecode,
	STR_IFMMODE,	iftest,	ifmmodecode,
	STR_IFINNER,	iftest,	ifinnercode,
	STR_IFVOID,	iftest,	ifvoidcode,
	STR_IFHBOX,	iftest,	ifhboxcode,
	STR_IFVBOX,	iftest,	ifvboxcode,
	STR_IFX,	iftest,	ifxcode,
	STR_IFEOF,	iftest,	ifeofcode,
	STR_IFTRUE,	iftest,	iftruecode,
	STR_IFFALSE,	iftest,	iffalsecode,
	STR_IFCASE,	iftest,	ifcasecode,
	STR_FI,		fiorelse, ficode,
  /* frozenfi... */
	0,	4,	0,
	STR_OR,		fiorelse,	orcode,
	STR_ELSE,	fiorelse,	elsecode,

	STR_NULLFONT,	set_font,	nullfont,
  /* frozennullfont... */
	0,	5,	0,

	STR_SPAN,	tab_mark,	spancode,
	STR_CR,		car_ret,	crcode,
  /* frozencr... */
	0,		6,	0,
	STR_CRCR,	car_ret,	crcrcode,
  /* frozenendtemplate... */
	0,		7,	0,

	STR_PAGEGOAL,		set_page_dimen,	0,
	STR_PAGETOTAL,		set_page_dimen,	1,
	STR_PAGESTRETCH,	set_page_dimen,	2,
	STR_PAGEFILSTRETCH,	set_page_dimen,	3,
	STR_PAGEFILLSTRETCH,	set_page_dimen,	4,
	STR_PAGEFILLLSTRETCH,	set_page_dimen,	5,
	STR_PAGESHRINK,		set_page_dimen,	6,
	STR_PAGEDEPTH,		set_page_dimen,	7,

	STR_END,	stop,	0,
	STR_DUMP,	stop,	1,

	STR_HSKIP,	hskip,	4,
	STR_HFIL,	hskip,	0,
	STR_HFILL,	hskip,	1,
	STR_HSS,	hskip,	2,
	STR_HFILNEG,	hskip,	3,
	STR_VSKIP,	vskip,	4,
	STR_VFIL,	vskip,	0,
	STR_VFILL,	vskip,	1,
	STR_VSS,	vskip,	2,
	STR_VFILNEG,	vskip,	3,

	333,	mskip,	5,
	337,	kern,	explicit,
	339,	mkern,	muglue,

	STR_MOVELEFT,	hmove,	1,
	STR_MOVERIGHT,	hmove,	0,
	STR_RAISE,	vmove,	1,
	STR_LOWER,	vmove,	0,
	STR_BOX,	make_box,	0,
	STR_COPY,	make_box,	1,
	STR_LASTBOX,	make_box,	2,
	STR_VSPLIT,	make_box,	3,
	STR_VTOP,	make_box,	4,
	STR_VBOX,	make_box,	4 + vmode,
	STR_HBOX,	make_box,	4 + hmode,
	STR_SHIPOUT,	leader_ship,	aleaders - 1,
	STR_LEADERS,	leader_ship,	aleaders,
	STR_CLEADERS,	leader_ship,	cleaders,
	STR_XLEADERS,	leader_ship,	xleaders,

	STR_INDENT,	start_par,	1,
	STR_NOINDENT,	start_par,	0,

	STR_UNPENALTY,	remove_item,	penaltynode,
	STR_UNKERN,	remove_item,	kernnode,
	STR_UNSKIP,	remove_item,	gluenode,
	STR_UNHBOX,	un_hbox,	0,
	STR_UNHCOPY,	un_hbox,	1,
	STR_UNVBOX,	un_vbox,	0,
	STR_UNVCOPY,	un_vbox,	1,

	45,	discretionary,	1,
	346,	discretionary,	0,

	STR_EQNO,	eq_no,	0,
	STR_LEQNO,	eq_no,	1,

	STR_MATHORD,	math_comp,	ordnoad,
	STR_MATHOP,	math_comp,	opnoad,
	STR_MATHBIN,	math_comp,	binnoad,
	STR_MATHREL,	math_comp,	relnoad,
	STR_MATHOPEN,	math_comp,	opennoad,
	STR_MATHCLOSE,	math_comp,	closenoad,
	STR_MATHPUNCT,	math_comp,	punctnoad,
	STR_MATHINNER,	math_comp,	innernoad,
	STR_OVERLINE,	math_comp,	overnoad,
	STR_UNDERLINE,	math_comp,	undernoad,

	STR_DISPLAYLIMITS,	limit_switch,	normal,
	STR_LIMITS,		limit_switch,	limits,
	STR_NOLIMITS,		limit_switch,	nolimits,

	STR_DISPLAYSTYLE,	math_style,	displaystyle,
	STR_TEXTSTYLE,		math_style,	textstyle,
	STR_SCRIPTSTYLE,	math_style,	scriptstyle,
	STR_SCRIPTSCRIPTSTYLE,	math_style,	scriptscriptstyle,

	STR_ABOVE,		above,	0,
	STR_OVER,		above,	1,
	STR_ATOP,		above,	2,
	STR_ABOVEWITHDELIMS,	above,	3,
	STR_OVERWITHDELIMS,	above,	4,
	STR_ATOPWITHDELIMS,	above,	5,

	STR_LEFT,	left_right,	leftnoad,
	STR_RIGHT,	left_right,	rightnoad,
  /* frozenright... */
	0,	8,	0,

	STR_LONG,	prefix,	1,
	STR_OUTER,	prefix,	2,
	STR_GLOBAL,	prefix,	4,
	STR_DEF,	def,	0,
	STR_GDEF,	def,	1,
	STR_EDEF,	def,	2,
	STR_XDEF,	def,	3,

	STR_LET,		let,	normal,
	STR_FUTURELET,		let,	normal + 1,

	STR_CHARDEF,		shorthand_def,	0,
	STR_MATHCHARDEF,	shorthand_def,	1,
	STR_COUNTDEF,		shorthand_def,	2,
	STR_DIMENDEF,		shorthand_def,	3,
	STR_SKIPDEF,		shorthand_def,	4,
	STR_MUSKIPDEF,		shorthand_def,	5,
	STR_TOKSDEF,		shorthand_def,	6,

	411,	def_code,	catcodebase,
	415,	def_code,	mathcodebase,
	412,	def_code,	lccodebase,
	413,	def_code,	uccodebase,
	414,	def_code,	sfcodebase,
	473,	def_code,	delcodebase,
	408,	def_family,	mathfontbase,
	409,	def_family,	mathfontbase + scriptsize,
	410,	def_family,	mathfontbase + scriptscriptsize,

	STR_HYPHENATION,	hyph_data,	0,
	STR_PATTERNS,		hyph_data,	1,

	STR_HYPHENCHAR,		assign_font_int,	0,
	STR_SKEWCHAR,		assign_font_int,	1,
	272,		set_interaction,	batchmode,
	273,		set_interaction,	nonstopmode,
	274,		set_interaction,	scrollmode,
	STR_ERRORSTOPMODE,	set_interaction,	errorstopmode,

	STR_OPENIN,		instream,	1,
	STR_CLOSEIN,		instream,	0,

	STR_MESSAGE,		message,	0,
	STR_ERRMESSAGE,		message,	1,

	STR_LOWERCASE,		caseshift,	lccodebase,
	STR_UPPERCASE,		caseshift,	uccodebase,

	STR_SHOW,		xray,	0,
	STR_SHOWBOX,		xray,	1,
	STR_SHOWTHE,		xray,	2,
	STR_SHOWLISTS,		xray,	3,

	STR_OPENOUT,		extension,	opennode,
	STR_WRITE,		extension,	writenode,
  /* writeloc... */
	0,			9,	0,
	STR_CLOSEOUT,		extension,	closenode,
	STR_SPECIAL,		extension,	specialnode,
	STR_IMMEDIATE,		extension,	4,
	STR_SETLANGUAGE,	extension,	5,
	0,			0,	0	/* end, phuuuuu */
};


void initprim ( void )
{ initprim_regmem
  register long_halfword r_curval = 0;

  struct prim_desc_str *p = prim_desc;

  nonewcontrolsequence = false;

  while( true ) {
    if( p->s == 0 ) {		/* exception, if strnum = 0 */
	switch( p->c ) {	/* decide, from eq_type.. */
	case 0:
	  goto endprim;		/* ----------- end of loop ------>>>> */
	case 1:
	  ztext ( frozenendgroup ) = STR_ENDGROUP;
	  eqtb [ frozenendgroup ] = eqtb [ r_curval ] ; 
	  break;
	case 2:
	  ztext ( frozenrelax ) = 531 ; 
	  eqtb [ frozenrelax ] = eqtb [ r_curval ] ; 
	  break;
	case 3:
	  parloc = r_curval;
	  partoken = cstokenflag + parloc ; 
	  break;
	case 4:
	  ztext ( frozenfi ) = STR_FI;
	  eqtb [ frozenfi ] = eqtb [ r_curval ];
	  break;
	case 5:
	  ztext ( frozennullfont ) = STR_NULLFONT;
	  eqtb [ frozennullfont ] = eqtb [ r_curval ];
	  break;
	case 6:
	  ztext ( frozencr ) = STR_CR;
	  eqtb [ frozencr ] = eqtb [ r_curval ];
	  break;
	case 7:		/* not necessary, but .... for safety */
	  ztext ( frozenendtemplate ) = STR_ENDTEMPLATE;
	  ztext ( frozenendv ) = STR_ENDTEMPLATE;
	  eqtype ( frozenendv ) = endv;
	  equiv ( frozenendv ) = nulllist;
	  eqlevel ( frozenendv ) = levelone;
	  eqtb [ frozenendtemplate ] = eqtb[frozenendv];
	  eqtype ( frozenendtemplate ) = endtemplate;
	  break;
	case 8:
	  ztext ( frozenright ) = STR_RIGHT;
	  eqtb [ frozenright ] = eqtb [ r_curval ];
	  break;
	case 9:
	  writeloc = r_curval;
	  break;
	default:
	  fprintf(stderr, "Sorry, internal error in `initprim()'...\n");
      }
      p++;
      continue;
    }
    r_curval = primitive( (strnumber)p->s, p->c, (halfword)p->o );
    p++;
  }

endprim:

#ifdef ERW_INTERACTION
  primitive(STR_INTERACTION_MODE, assign_int, intbase + interactionmodecode);
#endif


#ifdef MLTEX
  /* Only, if User has enabled ML-TeX, we define these primitives */
  /* This is the only place, where we need the distinction TeX != MLTeX */
  if( is_ML_TeX ) {
    primitive(STR_CHARSUBDEFMAX, assign_int,
		intbase + char_sub_def_max_code);
    primitive(STR_TRACING_CHARSUBDEF, assign_int,
		intbase + tracing_char_sub_def_code);
    primitive(STR_CHARSUBDEF, char_sub_def, 0);
  }
#endif

#ifdef TEXXET
  if( is_TeX_XeT ) {
    primitive(STR_BEGINL, valign, begin_L_code);
    primitive(STR_BEGINR, valign, begin_R_code);
    primitive(STR_ENDL, valign, end_L_code);
    primitive(STR_ENDR, valign, end_R_code);
  }
#endif

  nonewcontrolsequence = true;
}

#endif /* INITEX */
