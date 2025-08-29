/* @<Constants...@>= */

#include "help.h"

/* Nummern der String-Konstanten: ... */
#define STR_ENDWRITE			260 /* 1286 */

#define STR_OPENOUT			265 /* 1276 */
#define STR_CLOSEOUT			266 /* 1277 */
#define STR_SPECIAL			267 /* 1278 */
#define STR_IMMEDIATE			268 /* 1279 */
#define STR_SETLANGUAGE			269 /* 1280 */
#define STR_WRITE			590 /* */

#define STR_INITEX			271 /* 1248 */

#define STR_SHOW			312 /* 1230 */
#define STR_SHOWBOX			313 /* 1231 */
#define STR_SHOWTHE			314 /* 1232 */
#define STR_SHOWLISTS			315 /* 1233 */

#define STR_LOWERCASE			285 /* 1228 */
#define STR_UPPERCASE			286 /* 1229 */

#define STR_MESSAGE			324 /* 1221 */
#define STR_ERRMESSAGE			325 /* 1222 */

#define STR_OPENIN			322 /* 1219 */
#define STR_CLOSEIN			323 /* 1220 */

#define STR_ERRORSTOPMODE		263 /* 1218 */

#define STR_AT				341 /* 1211 */
#define STR_SCALED			342 /* 1212 */

#define STR_FONT_			289 /* 1210 */

#define STR_HYPHENCHAR			304 /* 1208 */
#define STR_SKEWCHAR			305 /* 1209 */

#define STR_BY				291 /* 1199 */

#define STR_CHARDEF			350 /* 1186 */
#define STR_MATHCHARDEF			351 /* 1187 */
#define STR_COUNTDEF			352 /* 1188 */
#define STR_DIMENDEF			353 /* 1189 */
#define STR_SKIPDEF			354 /* 1190 */
#define STR_MUSKIPDEF			355 /* 1191 */
#define STR_TOKSDEF			356 /* 1192 */

#define STR_LET				357 /* 1184 */
#define STR_FUTURELET			358 /* 1185 */

#define STR_INACCESSIBLE		347 /* 1183 */

#define STR_LONG			539 /* 1164 */
#define STR_OUTER			540 /* 1165 */
#define STR_GLOBAL			541 /* 1166 */
#define STR_DEF				542 /* 1167 */
#define STR_GDEF			543 /* 1168 */
#define STR_EDEF			544 /* 1169 */
#define STR_XDEF			545 /* 1170 */

#define STR_ABOVE			558 /* 1140 */
#define STR_OVER			559 /* 1141 */
#define STR_ATOP			560 /* 1142 */
#define STR_ABOVEWITHDELIMS		561 /* 1143 */
#define STR_OVERWITHDELIMS		562 /* 1144 */
#define STR_ATOPWITHDELIMS		563 /* 1145 */

#define STR_FIL				309 /* */
#define STR_PLUS_			310 /* */
#define STR_MINUS_			311 /* */

/* --- */
/* STR ohne Aenderung muessen noch ueberprueft werden, ob alle
   Vorkommen ersetzt wurden
 */

#define STR_INSERT			327 /* */
#define STR_MU				334 /* */
#define STR_KERN			337
#define STR_MATH			340
#define STR_DISCRETIONARY		346
#define STR_SKIP			391 /* */
#define STR_PT				393 /* */

#define STR_BOX				405 /* */

#define STR_ENDCSNAME			501 /* */
#define STR_ENDGROUP			512 /* */
#define STR_HRULE			517
#define STR_MATHACCENT			519
#define STR_NOALIGN			523
#define STR_OMIT			526
#define STR_PENALTY			527
#define STR_PREVGRAF			528
#define STR_READ			530 /* */
#define STR_THE				533
#define STR_PAR				593 /* */

#define STR_SPACEFACTOR			256 /* 664 */
#define STR_PREVDEPTH			371 /* 665 */
#define STR_DEADCYCLES			495 /* 666 */
#define STR_INSERTPENALTIES		497 /* 667 */

#define STR_WD				499 /* 668 */
#define STR_HT				502 /* 669 */
#define STR_DP				503 /* 670 */

#define STR_LASTPENALTY			556 /* 671 */
#define STR_LASTKERN			557 /* 672 */
#define STR_LASTSKIP			567 /* 673 */
#define STR_INPUTLINENO			587 /* 674 */
#define STR_BADNESS			588 /* 675 */

#define STR_TRUE			407 /* 700 */
#define STR_EM				546 /* 704 */
#define STR_EX				547 /* 705 */
#define STR_IN				548 /* 711 */
#define STR_PC				549 /* 712 */
#define STR_CM				550 /* 713 */
#define STR_MM				551 /* 714 */
#define STR_BP				552 /* 715 */
#define STR_DD				553 /* 716 */
#define STR_CC				554 /* 717 */
#define STR_SP				555 /* 718 */

#define STR_PLUS			537 /* 726 */
#define STR_MINUS			538 /* 727 */
#define STR_WIDTH			564 /* 728 */
#define STR_HEIGHT			565 /* 729 */
#define STR_DEPTH			566 /* 730 */

#define STR_NUMBER			365 /* 731 */
#define STR_ROMANNUMERAL		366 /* 732 */
#define STR_STRING			367 /* 733 */
#define STR_MEANING			368 /* 734 */
#define STR_FONTNAME			369 /* 735 */
#define STR_JOBNAME			370 /* 736 */

#define STR_AT_				406 /* 737 */

#define STR_IF				594 /* 752 */
#define STR_IFCAT			595 /* 753 */
#define STR_IFNUM			596 /* 754 */
#define STR_IFDIM			597 /* 755 */
#define STR_IFODD			598 /* 756 */
#define STR_IFVMODE			599 /* 757 */
#define STR_IFHMODE			600 /* 758 */
#define STR_IFMMODE			601 /* 759 */
#define STR_IFINNER			602 /* 760 */
#define STR_IFVOID			603 /* 761 */
#define STR_IFHBOX			604 /* 762 */
#define STR_IFVBOX			605 /* 763 */
#define STR_IFX				606 /* 764 */
#define STR_IFEOF			607 /* 765 */
#define STR_IFTRUE			608 /* 766 */
#define STR_IFFALSE			609 /* 767 */
#define STR_IFCASE			610 /* 768 */
#define STR_FI				611 /* 769 */
#define STR_OR				612 /* 770 */
#define STR_ELSE			613 /* 771 */

#define STR_DOT_FMT			359 /* 779 */
#define STR_DOT_TEX			360 /* 784 */
#define STR_DOT_DVI			361 /* 787 */
#define STR_TEXPUT			471 /* 789 */

#define STR_DOT_LOG			264 /* 790 */
#define STR_NULLFONT			303 /* 794 */
#define STR_DOT_TFM			326 /* 804 */

#define STR_TO				290 /* 835 */
#define STR_SPREAD			297 /* 836 */

#define STR_DISPLAYSTYLE		299 /* 854 */
#define STR_TEXTSTYLE			300 /* 855 */
#define STR_SCRIPTSTYLE			301 /* 856 */
#define STR_SCRIPTSCRIPTSTYLE		302 /* 857 */

#define STR_MATHORD			276 /* 859 */
#define STR_MATHOP			277 /* 860 */
#define STR_MATHBIN			278 /* 861 */
#define STR_MATHREL			279 /* 862 */
#define STR_MATHOPEN			280 /* 863 */
#define STR_MATHCLOSE			281 /* 864 */
#define STR_MATHPUNCT			282 /* 865 */
#define STR_MATHINNER			283 /* 866 */
#define STR_OVERLINE			259 /* 867 */
#define STR_UNDERLINE			261 /* 868 */

#define STR_LEFT			292 /* 869 */
#define STR_RIGHT			293 /* 870 */

#define STR_LIMITS			295 /* 871 */
#define STR_NOLIMITS			296 /* 872 */

#define STR_SPAN			306 /* 891 */
#define STR_CR				307 /* 892 */
#define STR_CRCR			308 /* 893 */
#define STR_ENDTEMPLATE			316 /* 894 */

#define STR_HYPHENATION			284 /* 934 */
#define STR_PATTERNS			270 /* 946 */

#define STR_VSPLIT			294 /* 958 */
#define STR_VBOX			298 /* 960 */

#define STR_END				343 /* 1018 */
#define STR_DUMP			344 /* 1019 */

#define STR_PAGEGOAL			317 /* 963 */
#define STR_PAGETOTAL			318 /* 964 */
#define STR_PAGESTRETCH			319 /* 965 */
#define STR_PAGEFILSTRETCH		320 /* 966 */
#define STR_PAGEFILLSTRETCH		321 /* 967 */
#define STR_PAGEFILLLSTRETCH		328 /* 968 */
#define STR_PAGESHRINK			329 /* 969 */
#define STR_PAGEDEPTH			330 /* 970 */

#define STR_HSKIP			589 /* 1020 */
#define STR_HFIL			591 /* 1021 */
#define STR_HFILL			592 /* 1022 */
#define STR_HSS				614 /* 1023 */
#define STR_HFILNEG			615 /* 1024 */
#define STR_VSKIP			616 /* 1025 */
#define STR_VFIL			617 /* 1026 */
#define STR_VFILL			618 /* 1027 */
#define STR_VSS				619 /* 1028 */
#define STR_VFILNEG			620 /* 1029 */

#define STR_MOVELEFT			575 /* 1047 */
#define STR_MOVERIGHT			576 /* 1048 */
#define STR_RAISE			577 /* 1049 */
#define STR_LOWER			578 /* 1050 */
#define STR_COPY			579 /* 1051 */
#define STR_LASTBOX			580 /* 1052 */
#define STR_VTOP			581 /* 1053 */
#define STR_HBOX			582 /* 1054 */
#define STR_SHIPOUT			583 /* 1055 */
#define STR_LEADERS			584 /* 1056 */
#define STR_CLEADERS			585 /* 1057 */
#define STR_XLEADERS			586 /* 1058 */

#define STR_INDENT			336 /* 1073 */
#define STR_NOINDENT			338 /* 1074 */

#define STR_UNPENALTY			568 /* 1083 */
#define STR_UNKERN			569 /* 1084 */
#define STR_UNSKIP			570 /* 1085 */
#define STR_UNHBOX			571 /* 1086 */
#define STR_UNHCOPY			572 /* 1087 */
#define STR_UNVBOX			573 /* 1088 */
#define STR_UNVCOPY			574 /* 1089 */

#define STR_EQNO			362 /* 1120 */
#define STR_LEQNO			363 /* 1121 */
#define STR_DISPLAYLIMITS		364 /* 1122 */

/* */
/* STR_MAX_ALL = last string in tex.pool */
#define	STR_MAX_ALL		631 /* 914 */ /* 1290 */

#ifdef MLTEX
#define STR_CHARSUBDEFMAX		(STR_MAX_ALL+1)
#define STR_TRACING_CHARSUBDEF		(STR_CHARSUBDEFMAX+1)
#define STR_CHARSUBDEF			(STR_TRACING_CHARSUBDEF+1)

#  ifdef ERW_INTERACTION
#define STR_INTERACTION_MODE	(STR_CHARSUBDEF+1)
#define STR_MAX			(STR_INTERACTION_MODE+1)
#  else
#define STR_MAX	(STR_CHARSUBDEF+1)
#  endif
#else
#  ifdef ERW_INTERACTION
#define STR_INTERACTION_MODE	(STR_MAX_ALL+1)
#define STR_MAX			(STR_INTERACTION_MODE+1)
#  else
#define STR_MAX	(STR_MAX_ALL+1)
#  endif
#endif

#ifdef TEXXET
#define	STR_BEGINL	(STR_MAX+1)
#define STR_BEGINR	(STR_BEGINL+1)
#define STR_ENDL	(STR_BEGINR+1)
#define STR_ENDR	(STR_ENDL+1)
#endif


#ifdef MLTEX
# if 0
#  define effective_char(inchr) \
    ( (is_ML_TeX && char_list_exists(inchr) && (inchr) <= char_sub_def_max) \
	? (eqtb[char_sub_base + (inchr)].qqqq.b1) \
	: (inchr) )
# endif
#else
#  define effective_char(inchr) (inchr)
#endif

/* bei aelteren GCC's wird *10 mit mulX #10,dX uebersetzt */
#if 0
/* besser als "muls #10,d0" : */
#define TEN_MULT(X)	((((X) << 2) + (X)) << 1)
#else
#define TEN_MULT(X)	(10 * (X))
#endif


#ifdef FONTSTRUCT
#define charbase(f)	fontdesc[f].charbase
#define widthbase(f)	fontdesc[f].widthbase
#define heightbase(f)	fontdesc[f].heightbase
#define depthbase(f)	fontdesc[f].depthbase
#define italicbase(f)	fontdesc[f].italicbase
#define ligkernbase(f)	fontdesc[f].ligkernbase
#define kernbase(f)	fontdesc[f].kernbase
#define extenbase(f)	fontdesc[f].extenbase
#define parambase(f)	fontdesc[f].parambase

#define fontbc(f)	fontdesc[f].fontbc
#define fontec(f)	fontdesc[f].fontec
#define fontcheck(f)	fontdesc[f].fontcheck
#define fontparams(f)	fontdesc[f].fontparams
#define fontglue(f)	fontdesc[f].fontglue
#define hyphenchar(f)	fontdesc[f].hyphenchar
#define skewchar(f)	fontdesc[f].skewchar
#define bcharlabel(f)	fontdesc[f].bcharlabel
#define fontbchar(f)	fontdesc[f].fontbchar
#define fontfalsebchar(f)	fontdesc[f].fontfalsebchar

#else

#define charbase(f)	charbase[f]
#define widthbase(f)	widthbase[f]
#define heightbase(f)	heightbase[f]
#define depthbase(f)	depthbase[f]
#define italicbase(f)	italicbase[f]
#define ligkernbase(f)	ligkernbase[f]
#define kernbase(f)	kernbase[f]
#define extenbase(f)	extenbase[f]
#define parambase(f)	parambase[f]

#ifdef FONTPTR
#define fontbc(f)	font_bcec[f].bc
#define fontec(f)	font_bcec[f].ec
#else
#define fontbc(f)	fontbc[f]
#define fontec(f)	fontec[f]
#endif
#define fontcheck(f)	fontcheck[f]
#define fontparams(f)	fontparams[f]
#define fontglue(f)	fontglue[f]
#define hyphenchar(f)	hyphenchar[f]
#define skewchar(f)	skewchar[f]
#define bcharlabel(f)	bcharlabel[f]
#define fontbchar(f)	fontbchar[f]
#define fontfalsebchar(f)	fontfalsebchar[f]

#endif

#define fontused(f)	fontused[f]
#define fontsize(f)	fontsize[f]
#define fontdsize(f)	fontdsize[f]
#define fontname(f)	fontname[f]
#define fontarea(f)	fontarea[f]



#ifdef TEXXET
#define push_LR(ptr) \
  do { \
    register long_halfword LR_tmp = getavail(); \
    info(LR_tmp) = LR_type(ptr); \
    link(LR_tmp) = LR_ptr; \
    LR_ptr = LR_tmp; \
  } while(0)

#define pop_LR \
  do { \
    register long_halfword LR_tmp = LR_ptr; \
    LR_ptr = link(LR_tmp); \
    freeavail(LR_tmp); \
  } while(0)
#endif


#define minquarterword 0
#define tokentype   (curinput.indexfield)
#define paramstart   (curinput.limitfield)

#ifdef NEW_HELP

#define help0 \
   do { static short tmp[] = { 0 }; helpptr = tmp; } while(0)
#define zhelp1(s1) \
   do { static short tmp[] = { s1 , 0 }; helpptr = tmp; } while(0)
#define zhelp2(s2,s1) \
   do { static short tmp[] = { s2 , s1 , 0 }; helpptr = tmp; } while(0)
#define zhelp3(s3,s2,s1) \
   do { static short tmp[] = { s3 , s2 , s1 , 0 }; helpptr = tmp; } while(0)
#define zhelp4(s4,s3,s2,s1) \
   do { static short tmp[] = { s4 , s3 , s2 , s1 , 0 }; \
        helpptr = tmp; } while(0)
#define zhelp5(s5,s4,s3,s2,s1) \
   do { static short tmp[] = { s5 , s4 , s3 , s2 , s1 , 0 }; \
        helpptr = tmp; } while(0)
#define zhelp6(s6,s5,s4,s3,s2,s1) \
   do { static short tmp[] = { s6 , s5 , s4 , s3 , s2 , s1 , 0 }; \
        helpptr = tmp; } while(0)

#else

#define help0   helpptr = 0
#define zhelp1(s1)   do { helpptr = 1; helpline[0] = s1; } while(0)
#define zhelp2(s2,s1) \
   do { helpptr = 2; helpline[0] = s1; helpline[1] = s2; } while(0)
#define zhelp3(s3,s2,s1) \
   do { helpptr = 3; helpline[0] = s1; helpline[1] = s2;\
        helpline[2] = s3; } while(0)
#define zhelp4(s4,s3,s2,s1) \
   do { helpptr = 4; helpline[0] = s1; helpline[1] = s2; helpline[2] = s3; \
     helpline[3] = s4; } while(0)
#define zhelp5(s5,s4,s3,s2,s1) \
   do { helpptr = 5; helpline[0] = s1; helpline[1] = s2; helpline[2] = s3; \
     helpline[3] = s4; helpline[4] = s5;} while (0)
#define zhelp6(s6,s5,s4,s3,s2,s1) \
   do { helpptr = 6; helpline[0] = s1; helpline[1] = s2; helpline[2] = s3; \
     helpline[3] = s4; helpline[4] = s5; helpline[5] = s6;} while(0)

#endif


#define link(s) (mem[s].hh.v.RH)  /* the |link| field of a memory word */
#define info(s) (mem[s].hh.v.LH)  /* the |info| field of a memory word */

#define isempty(s)  (link(s) == emptyflag) /* tests for empty node */
  /* left link in doubly-linked list of empty nodes */
#define llink(s)    info((s)+1)
  /* right link in doubly-linked list of empty nodes */
#define rlink(s)    link((s)+1)


#define length(s) (strstart[(s)+1]-strstart[s])
        /* the number of characters in string number |s| */
#define curlength  (poolptr - strstart[strptr])

  /* put |ASCII_code| # at the end of |str_pool| */
#define appendchar(c) \
  strpool[poolptr++] = (c)
  /* forget the last character in the pool */
#define flushchar  decr(poolptr)
  /* make sure that the pool hasn't overflowed */
#define strroom(s) \
  do { if (poolptr + (s) > poolsize) \
      overflow(0, poolsize-initpoolptr);  /* "pool size" */ \
  } while(0)

#define flushstring  do { decr(strptr); poolptr=strstart[strptr]; } while(0)


#define str_eq_str(s,t) \
  ( (length(s) == length(t)) && \
    !strncmp( (char *) &strpool[strstart[(s)]], \
	      (char *) &strpool[strstart[(t)]], length(s)) )


	/* Achtung: freeavail direkt verwendet in flush_node_list() */
#ifdef STAT
  /* single-word node liberation */
#define freeavail(s) \
  do { \
    register halfword *availPTR = &avail; \
    link(s) = *availPTR; *availPTR = (s); \
    decr(dynused); \
  } while(0)
#else
#define freeavail(s) \
  do { \
    register halfword *availPTR = &avail; \
    link(s) = *availPTR; *availPTR = (s); \
  } while(0)
#endif

#ifdef STAT
#define fastgetavail(s)  \
  do { register halfword *availPTR = &avail; \
    (s)=*availPTR; /*avoid |get_avail| if possible, to save time */ \
    if( (s)==0 ) { \
      (s)=getavail(); \
    } else { \
       *availPTR=link(s); link(s)=0; \
       incr(dynused); \
    } \
  } while(0)
#else
#define fastgetavail(s)  \
  do { (s)=avail; /*avoid |get_avail| if possible, to save time */ \
  if( (s)==0 ) { (s)=getavail(); \
  } else { avail=link(s); link(s)=0; } } while(0)
#endif

#define ztype(s)  mem[s].hh.b0 /* identifies what kind of node this is */
#define subtype(s) mem[s].hh.b1 /* secondary identification in some cases */
	/* (br) added this for all functions new...() with const s1,s2 */
#define set_type_subtype(s,s1,s2) \
	mem[s].hh.w.LH = ((s1) * 256) + (s2)

#define ischarnode(s) ((s)>=himemmin)
  /* does the argument point to a |char_node|? */
	/* (br) added this all functions new...() */
#define set_font_character(s,s1,s2)	set_type_subtype((s),(s1),(s2))

#define width(s) mem[(s)+widthoffset].cint /* width of the box, in sp */
#define depth(s) mem[(s)+depthoffset].cint /* depth of the box, in sp */
#define height(s) mem[(s)+heightoffset].cint /* height of the box, in sp */
#define shiftamount(s) mem[(s)+4].cint /* repositioning distance, in sp */
#define listptr(s) link((s)+listoffset)
   /* beginning of the list inside the box */
#define glueorder(s) subtype((s)+listoffset)
   /* applicable order of infinity */
#define gluesign(s) ztype((s)+listoffset)
   /* stretching or shrinking */ 
#define glueset(s) mem[(s)+glueoffset].gr
   /* a word of type |glue_ratio| for glue setting */
	/* (br) added this all functions new...() */
#define set_gluesign_order(s,s1,s2) \
	set_type_subtype((s)+listoffset,(s1),(s2))


#define isrunning(s)  ((s)==nullflag)  /* tests for a running dimension */

#define floatcost(s)   mem[(s)+1].cint /* the |floating_penalty| to be used */
#define insptr(s)      info((s)+4) /* the vertical list to be inserted */
#define splittopptr(s) link((s)+4) /* the |split_top_skip| to be used */

#define markptr(s) mem[(s)+1].cint /* head of the token list for a mark */
#define adjustptr(s) markptr(s)

#define ligchar(s) (s)+1   /* the word where the ligature is to be found */
#define ligptr(s) link(ligchar(s))  /* the list of characters */

#define replacecount(s) subtype(s) /* how many subsequent nodes to replace */
	/* (br) added this all functions new...() */
#define set_type_replacecount(s,s1,s2)	set_type_subtype((s),(s1),(s2))
#define prebreak(s) llink(s) /* text that precedes a discretionary break */
#define postbreak(s) rlink(s) /* text that follows a discretionary break */

#define precedesbreak(s) (ztype(s)<mathnode)
#define nondiscardable(s) (ztype(s)<mathnode)

#define glueptr(s) llink(s) /* pointer to a glue specification */
#define leaderptr(s) rlink(s) /* pointer to box or rule node for leaders */

#define gluerefcount(s) link(s) /* reference count of a glue specification */
#define stretch(s) mem[(s)+2].cint /*the stretchability of this glob of glue*/
#define shrink(s)  mem[(s)+3].cint /*the shrinkability of this glob of glue*/

#define stretchorder(s) ztype(s)  /* order of infinity for stretching */
#define shrinkorder(s) subtype(s) /* order of infinity for shrinking */
	/* (br) added this all functions new...() */
#define set_stretch_shrinkorder(s,s1,s2)	set_type_subtype(s,(s1),(s2))
	 
#define penalty(s) mem[(s)+1].cint /*the added cost of breaking a list here*/

#define glueshrink(s) shiftamount(s) /* total shrink in an unset node */
#define spancount(s) subtype(s) /* indicates the number of spanned columns */

#define gluestretch(s) mem[(s)+glueoffset].cint
    /* total stretch in an unset node */


#define fastdeleteglueref(s) \
  do { if (gluerefcount(s)==0) freenode(s,gluespecsize); \
       else decr(gluerefcount(s)); } while(0)

#define addtokenref(s) incr(tokenrefcount(s))
   /* new reference to a token list */
#define addglueref(s)  incr(gluerefcount(s))
   /* new reference to a glue spec */

#define tailappend(s) \
   do { \
     register halfword *tailfPTR = &(curlist.tailfield); \
     link( (*tailfPTR) ) = (s); \
     *tailfPTR = link( (*tailfPTR) ); \
   } while(0)


#define eqlevelfield(s)  (s).hh.b1
#define eqtypefield(s)   (s).hh.b0
#define equivfield(s)    (s).hh.v.RH
#define eqlevel(s)  eqlevelfield(eqtb[s]) /* level of definition */
#define eqtype(s)   eqtypefield(eqtb[s])  /* command code for equivalent */
#define equiv(s)    equivfield(eqtb[s])   /* equivalent value */


#define skip(s)    equiv((s)+skipbase) /*|mem| location of glue specification*/
#define muskip(s)  equiv((s)+muskipbase) /*|mem| location of math glue spec*/
#define gluepar(s) equiv((s)+gluebase) /*|mem| location of glue specification*/

#define lineskip      gluepar(lineskipcode)
#define baselineskip  gluepar(baselineskipcode)
#define parskip       gluepar(parskipcode)
#define abovedisplayskip  gluepar(abovedisplayskipcode)
#define belowdisplayskip  gluepar(belowdisplayskipcode)
#define abovedisplayshortskip  gluepar(abovedisplayshortskipcode)
#define belowdisplayshortskip  gluepar(belowdisplayshortskipcode)
#define leftskip  gluepar(leftskipcode)
#define rightskip  gluepar(rightskipcode)
#define topskip  gluepar(topskipcode)
#define splittopskip  gluepar(splittopskipcode)
#define tabskip  gluepar(tabskipcode)
#define spaceskip  gluepar(spaceskipcode)
#define xspaceskip  gluepar(xspaceskipcode)
#define parfillskip  gluepar(parfillskipcode)
#define thinmuskip  gluepar(thinmuskipcode)
#define medmuskip  gluepar(medmuskipcode)
#define thickmuskip  gluepar(thickmuskipcode)

#define parshapeptr    equiv(parshapeloc)
#define outputroutine  equiv(outputroutineloc)
#define everypar   equiv(everyparloc)
#define everymath  equiv(everymathloc)
#define everydisplay  equiv(everydisplayloc)
#define everyhbox  equiv(everyhboxloc)
#define everyvbox  equiv(everyvboxloc)
#define everyjob  equiv(everyjobloc)
#define everycr  equiv(everycrloc)
#define errhelp  equiv(errhelploc)
#define toks(s)  equiv((s)+toksbase)
#define box(s)  equiv((s)+boxbase)
#define curfont  equiv(curfontloc)
#define famfnt(s)  equiv((s)+mathfontbase)
#define catcode(s)  equiv((s)+catcodebase)
#define lccode(s)  equiv((s)+lccodebase)
#define uccode(s)  equiv((s)+uccodebase)
#define sfcode(s)  equiv((s)+sfcodebase)
#define mathcode(s)  equiv((s)+mathcodebase)
  /* Note: |mathcode(c)| is the true math code plus |minhalfword| */

#define delcode(s)  eqtb[(s)+delcodebase].cint
#define count(s)  eqtb[(s)+countbase].cint
#define intpar(s)  eqtb[(s)+intbase].cint  /* an integer parameter */
#define pretolerance  intpar(pretolerancecode)
#define tolerance  intpar(tolerancecode)
#define linepenalty  intpar(linepenaltycode)
#define hyphenpenalty  intpar(hyphenpenaltycode)
#define exhyphenpenalty  intpar(exhyphenpenaltycode)
#define clubpenalty  intpar(clubpenaltycode)
#define widowpenalty  intpar(widowpenaltycode)
#define displaywidowpenalty  intpar(displaywidowpenaltycode)
#define brokenpenalty  intpar(brokenpenaltycode)
#define binoppenalty  intpar(binoppenaltycode)
#define relpenalty  intpar(relpenaltycode)
#define predisplaypenalty  intpar(predisplaypenaltycode)
#define postdisplaypenalty  intpar(postdisplaypenaltycode)
#define interlinepenalty  intpar(interlinepenaltycode)
#define doublehyphendemerits  intpar(doublehyphendemeritscode)
#define finalhyphendemerits  intpar(finalhyphendemeritscode)
#define adjdemerits  intpar(adjdemeritscode)
#define mag  intpar(magcode)
#define delimiterfactor  intpar(delimiterfactorcode)
#define looseness  intpar(loosenesscode)
#define ztime  intpar(timecode)
#define zday  intpar(daycode)
#define zmonth  intpar(monthcode)
#define zyear  intpar(yearcode)
#define showboxbreadth  intpar(showboxbreadthcode)
#define showboxdepth  intpar(showboxdepthcode)
#define hbadness  intpar(hbadnesscode)
#define vbadness  intpar(vbadnesscode)
#define pausing  intpar(pausingcode)
#define tracingonline  intpar(tracingonlinecode)
#define tracingmacros  intpar(tracingmacroscode)
#define tracingstats  intpar(tracingstatscode)
#define tracingparagraphs  intpar(tracingparagraphscode)
#define tracingpages  intpar(tracingpagescode)
#define tracingoutput  intpar(tracingoutputcode)
#define tracinglostchars  intpar(tracinglostcharscode)
#define tracingcommands  intpar(tracingcommandscode)
#define tracingrestores  intpar(tracingrestorescode)
#define uchyph  intpar(uchyphcode)
#define outputpenalty  intpar(outputpenaltycode)
#define maxdeadcycles  intpar(maxdeadcyclescode)
#define hangafter  intpar(hangaftercode)
#define floatingpenalty  intpar(floatingpenaltycode)
#define globaldefs  intpar(globaldefscode)
#define curfam  intpar(curfamcode)
#define escapechar  intpar(escapecharcode)
#define defaulthyphenchar  intpar(defaulthyphencharcode)
#define defaultskewchar  intpar(defaultskewcharcode)
#define endlinechar  intpar(endlinecharcode)
#define newlinechar  intpar(newlinecharcode)
#define language  intpar(languagecode)
#define lefthyphenmin  intpar(lefthyphenmincode)
#define righthyphenmin  intpar(righthyphenmincode)
#define holdinginserts  intpar(holdinginsertscode)
#define errorcontextlines  intpar(errorcontextlinescode)
#ifdef MLTEX
#define char_sub_def_max	intpar(char_sub_def_max_code)
#define tracing_char_sub_def	intpar(tracing_char_sub_def_code)
#endif
#ifdef ERW_INTERACTION
#define interaction	intpar(interactionmodecode)
#endif

#define dimen(s)     eqtb[(s)+scaledbase].cint
#define dimenpar(s)  eqtb[(s)+dimenbase].cint /* a scaled quantity */
#define parindent     dimenpar(parindentcode)
#define mathsurround  dimenpar(mathsurroundcode)
#define lineskiplimit  dimenpar(lineskiplimitcode)
#define hsize  dimenpar(hsizecode)
#define vsize  dimenpar(vsizecode)
#define maxdepth  dimenpar(maxdepthcode)
#define splitmaxdepth  dimenpar(splitmaxdepthcode)
#define boxmaxdepth  dimenpar(boxmaxdepthcode)
#define hfuzz  dimenpar(hfuzzcode)
#define vfuzz  dimenpar(vfuzzcode)
#define delimitershortfall  dimenpar(delimitershortfallcode)
#define nulldelimiterspace  dimenpar(nulldelimiterspacecode)
#define scriptspace  dimenpar(scriptspacecode)
#define predisplaysize  dimenpar(predisplaysizecode)
#define displaywidth  dimenpar(displaywidthcode)
#define displayindent  dimenpar(displayindentcode)
#define overfullrule  dimenpar(overfullrulecode)
#define hangindent  dimenpar(hangindentcode)
#define hoffset  dimenpar(hoffsetcode)
#define voffset  dimenpar(voffsetcode)
#define emergencystretch  dimenpar(emergencystretchcode)


#define next(s) hash[s].v.LH
    /* link for coalesced lists */
#define ztext(s) hash[s].v.RH
    /* string number for control sequence name */
/* #define text(s) ztext(h) */
#define hashisfull (hashused==hashbase)
   /* test if all positions are occupied */
#define fontidtext(s) ztext((s)+fontidbase)
   /* a frozen font identifier's name */

#define savetype(s)  savestack[s].hh.b0
  /* classifies a |savestack| entry */
#define savelevel(s) savestack[s].hh.b1
  /* saved level for regions 5 and 6, or group code */
#define saveindex(s) savestack[s].hh.v.RH
  /* |eqtb| location or |savestack| location */

#define saved(s)  savestack[(s)+saveptr].cint

#define endlinecharinactive()  ((endlinechar < 0) || (endlinechar > 255))

#define storenewtoken(s) \
  do { register long_halfword q; \
       q = getavail(); link(p)=q; info(q)=(s); \
       p=q; /* |link(p)| is |null| */ } while(0)
#define faststorenewtoken(s) \
  do { register long_halfword q; \
       fastgetavail(q); link(p)=q; info(q)=(s); \
       p=q;  /* |link(p)| is |null| */ } while(0)


#define topmark  curmark[topmarkcode]
#define firstmark  curmark[firstmarkcode]
#define botmark  curmark[botmarkcode]
#define splitfirstmark  curmark[splitfirstmarkcode]
#define splitbotmark  curmark[splitbotmarkcode]


#define iflinefield(s) mem[(s)+1].cint

#define getxtokenoractivechar \
  do { \
    r_curcmd = getxtoken(); \
    if ( r_curcmd == relax && curchr == noexpandflag ) { \
      r_curcmd = 13;   /* active_char */ \
      curchr = curtok - cstokenflag - activebase; \
    } \
  } while(0)


#define dviout(s) \
  do { dvibuf[dviptr] = (s); incr(dviptr); \
       if (dviptr == dvilimit) dviswap(); } while(0)

#define location(s) mem[(s)+2].cint
  /* \.{DVI} byte number for a movement command */


#define nucleus(s) ((s)+1) /* the |nucleus| field of a noad */
#define supscr(s)  ((s)+2) /* the |supscr| field of a noad */
#define subscr(s)  ((s)+3) /* the |subscr| field of a noad */
#define mathtype(s) link(s) /* a |halfword| in |mem| */
#define fam(s)       font(s) /* a |quarterword| in |mem| */

#define leftdelimiter(s)  ((s)+4) /* first delimiter field of a noad */
#define rightdelimiter(s) ((s)+5)
   /* second delimiter field of a fraction noad */
#define smallfam(s)  mem[s].qqqq.b0 /* |fam| for ``small'' delimiter */
#define smallchar(s)  mem[s].qqqq.b1 /* |character| for ``small'' delimiter */
#define largefam(s)   mem[s].qqqq.b2 /* |fam| for ``large'' delimiter */
#define largechar(s)  mem[s].qqqq.b3 /* |character| for ``large'' delimiter */
#define thickness(s)   width(s)  /* |thickness| field in a fraction noad */
#define numerator(s)   supscr(s) /* |numerator| field in a fraction noad */
#define denominator(s) subscr(s) /* |denominator| field in a fraction noad */

#define accentchr(s)  ((s)+4)   /* the |accent_chr| field of an accent noad */
#define delimiter(s) nucleus(s) /* |delimiter| field in left and right noads */
#define scriptsallowed(s) ((ztype(s)>=ordnoad)&&(ztype(s)<leftnoad))

#define displaymlist(s)      info((s)+1)
   /* mlist to be used in display style */
#define textmlist(s)         link((s)+1)
   /* mlist to be used in text style */
#define scriptmlist(s)       info((s)+2)
   /* mlist to be used in script style */
#define scriptscriptmlist(s) link((s)+2)
   /* mlist to be used in scriptscript style */


#if 0
#define skipbyte(s) ((s).b0)
#define nextchar(s) ((s).b1)
#define opbyte(s)   ((s).b2)
#define rembyte(s)  ((s).b3)
#else
#define skipbyte(s) *((quarterword *) &((s).b0))
#define nextchar(s) *( ((quarterword *) &((s).b0)) + 1 )
#define opbyte(s)   *( ((quarterword *) &((s).b0)) + 2 )
#define rembyte(s)  ((s).b3)
  /* *( ((quarterword *) &((s).b0)) + 3 ) */
#endif

#define exttop(s)  ((s).b0) /* |top| piece in a recipe */
#define extmid(s)  ((s).b1) /* |mid| piece in a recipe */
#define extbot(s)  ((s).b2) /* |bot| piece in a recipe */
#define extrep(s)  ((s).b3) /* |rep| piece in a recipe */


#ifdef MLTEX
#define char_list_exists(s) (equiv((s)+char_sub_base) > 0) /* min_halfword */
#endif

		/* CharInfo() nur verwendet in readfontinfo() */
#ifdef FONTPTR
#ifdef MLTEX
#define zcharinfo(s1,s2)   (*(charbase(s1)+effective_char(s2))).qqqq
#else
#define zcharinfo(s1,s2)   (*(charbase(s1)+(s2))).qqqq
#endif
#define CharInfo(s1,s2)   (*(charbase(s1)+(s2))).qqqq
#define zcharwidth(s1,s2)  (*(widthbase(s1)+(s2).b0)).cint
#else
#ifdef MLTEX
#define zcharinfo(s1,s2)   fontinfo[charbase(s1)+effective_char(s2)].qqqq
#else
#define zcharinfo(s1,s2)   fontinfo[charbase(s1)+(s2)].qqqq
#endif
#define CharInfo(s1,s2)   fontinfo[charbase(s1)+(s2)].qqqq
#define zcharwidth(s1,s2)  fontinfo[widthbase(s1)+((s2).b0)].cint
#endif

#if 0
#define charexists(s)          (((s).b0) > minquarterword)
#else		/* minquarterword = 0 */
#define charexists(s)          (((s).cint) > 0x00ffffff)
#endif

#ifdef FONTPTR
#define zcharitalic(s1,s2) (*(italicbase(s1)+(((s2).b2)/4))).cint
#define zcharheight(s1,s2) (*(heightbase(s1)+((s2)/16))).cint
#define zchardepth(s1,s2)  (*(depthbase(s1)+((s2)%16))).cint
#else
#define zcharitalic(s1,s2) fontinfo[italicbase(s1) + (((s2).b2)/4)].cint
#define zcharheight(s1,s2) fontinfo[heightbase(s1) + ((s2)/16)].cint
#define zchardepth(s1,s2)  fontinfo[depthbase(s1) + ((s2)%16)].cint
#endif
#if 0
#define heightdepth(s)         ((s).b1)
#else
#define heightdepth(s)	(*( ((quarterword *) &((s).b0)) + 1 ))
#endif

   /* Tag field in char_info word: ... */
#define notag	0	/* wird nirgends verwendet */
#define ligtag	1
#define listtag 2
#define exttag	3
#define chartag(s)             (((s).b2) % 4)
#define mychartag(s)       ((unsigned short) ((s).cint & 0x00000300))
				/* Vergleich dann mit (..tag << 8) */

#ifdef FONTPTR

#define zcharkern(s1,s2) \
   (*(kernbase(s1) + *( ((unsigned short *) &((s2).b0)) + 1 ))).cint

#define zligkernstart(s1,s2)   (ligkernbase(s1)+rembyte(s2))
   /* beginning of lig/kern program */

#define zligkernrestart(s1,s2) \
   (ligkernbase(s1)+ *( ((unsigned short *) &((s2).b0)) + 1) + \
	(32768L-kernbaseoffset))

#else

#define zcharkern(s1,s2) \
   fontinfo[kernbase(s1) + *( ((unsigned short *) &((s2).b0)) + 1 )].cint
				/* not +2, because sizeof(u_short) = 2 !! */
	/* fontinfo[kernbase(s1) + 256*opbyte(s2)+rembyte(s2)].cint */

#define zligkernstart(s1,s2)   (ligkernbase(s1)+rembyte(s2))
   /* beginning of lig/kern program */

#define zligkernrestart(s1,s2) \
   (ligkernbase(s1)+ *( ((unsigned short *) &((s2).b0)) + 1) + \
	(32768L-kernbaseoffset))
  /* (ligkernbase(s1)+(256*opbyte(s2))+rembyte(s2)+(32768L-kernbaseoffset)) */

#endif


#define zparam(s1,s2)  fontinfo[(s1) + parambase(s2)].cint
#define param(s1,s2)       zparam(s1,s2)

#define slant(s)  param(slantcode,s)
   /* slant to the right, per unit distance upward*/
#define space(s)  param(spacecode,s) /* normal space between words*/
#define spacestretch(s)  param(spacestretchcode,s) /* stretch between words*/
#define spaceshrink(s)  param(spaceshrinkcode,s) /* shrink between words*/
#define xheight(s)  param(xheightcode,s) /* one ex*/
#define quad(s) param(quadcode,s) /* one em*/
#define extraspace(s)  param(extraspacecode,s)
   /* additional space at end of sentence*/

#define zmathsy(s1,s2) fontinfo[(s1)+parambase(famfnt((s2)+2))].cint
#define mathsy(s1,s2)  zmathsy((s1),(s2))
#define mathxheight(s) mathsy(5,s)   /* height of `\.x' */
#define mathquad(s)    mathsy(6,s) /* \.{18mu} */
#define num1(s)    mathsy(8,s) /* numerator shift-up in display styles */
#define num2(s)    mathsy(9,s) /* numerator shift-up in non-display, non-\.{\\atop} */
#define num3(s)    mathsy(10,s) /* numerator shift-up in non-display \.{\\atop} */
#define denom1(s)    mathsy(11,s) /* denominator shift-down in display styles */
#define denom2(s)    mathsy(12,s) /* denominator shift-down in non-display styles */
#define sup1(s)    mathsy(13,s) /* superscript shift-up in uncramped display style */
#define sup2(s)    mathsy(14,s) /* superscript shift-up in uncramped non-display */
#define sup3(s)   mathsy(15,s) /* superscript shift-up in cramped styles */
#define sub1(s)    mathsy(16,s) /* subscript shift-down if superscript is absent */
#define sub2(s)    mathsy(17,s) /* subscript shift-down if superscript is present */
#define supdrop(s)    mathsy(18,s) /* superscript baseline below top of large box */
#define subdrop(s)    mathsy(19,s) /* subscript baseline below bottom of large box */
#define delim1(s)    mathsy(20,s)
  /* size of \.{\\atopwithdelims} delimiters in display styles */
#define delim2(s)    mathsy(21,s)
  /* size of \.{\\atopwithdelims} delimiters in non-displays */
#define axisheight(s)    mathsy(22,s)
  /* height of fraction lines above the baseline */

#define mathex(s) fontinfo[(s)+parambase(famfnt(cursize+3))].cint
#define defaultrulethickness  mathex(8) /* thickness of \.{\\over} bars */
#define bigopspacing1  mathex(9) /* minimum clearance above a displayed op */
#define bigopspacing2  mathex(10) /* minimum clearance below a displayed op */
#define bigopspacing3  mathex(11) /* minimum baselineskip above displayed op */
#define bigopspacing4  mathex(12) /* minimum baselineskip below displayed op */
#define bigopspacing5  mathex(13) /* padding above and below displayed limits */

#define crampedstyle(s)  (((s)&~1)+cramped)  /* cramp the style */
#if 1
#define substyle(s)  ((((s)/2)&~1)+scriptstyle+cramped) /* smaller and cramped */
#define supstyle(s)  ((((s)/2)&~1)+scriptstyle+((s)&1)) /* smaller */
#define numstyle(s)  ((s)+2-(((s)/3)&~1)) /* smaller unless already script-script */
#define denomstyle(s)  (((s)&~1)+cramped+2-(((s)/3)&~1)) /* smaller, cramped */
#else
#define substyle(s)  (2*((s)/4)+scriptstyle+cramped) /* smaller and cramped */
#define supstyle(s)  (2*((s)/4)+scriptstyle+((s)%2)) /* smaller */
#define numstyle(s)  ((s)+2-2*((s)/6)) /* smaller unless already script-script */
#define denomstyle(s)  (2*((s)/2)+cramped+2-2*((s)/6)) /* smaller, cramped */
#endif

#define newhlist(s) mem[nucleus(s)].cint   /* the translation of an mlist */


#define upart(s)  mem[(s)+heightoffset].cint /* pointer to \<u_j> token list */
#define vpart(s)  mem[(s)+depthoffset].cint /* pointer to \<v_j> token list */
#define extrainfo(s)  info((s)+listoffset) /* info to remember during template */

#define preamble  link(alignhead) /* the current preamble list */


#define fitness(s)  subtype(s)
   /* |very_loose_fit..tight_fit| on final line for this break */
#define breaknode(s)  rlink(s) /* pointer to the corresponding passive node */
#define zlinenumber(s)  llink(s) /* line that begins at this breakpoint */
#define totaldemerits(s)  mem[(s)+2].cint
   /* the quantity that \TeX\ minimizes */

#define curbreak(s)  rlink(s)
   /* in passive node, points to position of this breakpoint */
#define prevbreak(s)  llink(s)
   /* points to passive node that should precede this one */
#define serial(s)  info(s)  /* serial number for symbolic identification */


/* -------- BEGIN: Achtung, die Makros werden direkt in trybreak() verwendet */
#define converttobreakwidth(s)  \
  mem[prevr+(s)].cint = mem[prevr+(s)].cint-curactivewidth[s]+breakwidth[(s)-1]
#define storebreakwidth(s)  activewidth[s]=breakwidth[(s)-1]
#define newdeltatobreakwidth(s) \
  mem[q+(s)].cint = breakwidth[(s)-1]-curactivewidth[s]

#define newdeltafrombreakwidth(s) \
  mem[q+(s)].cint = curactivewidth[s]-breakwidth[(s)-1]

#define combinetwodeltas(s) \
  mem[prevr+(s)].cint = mem[prevr+(s)].cint+mem[r+(s)].cint
#define downdatewidth(s) \
  curactivewidth[s] = curactivewidth[s] - mem[prevr+(s)].cint

#define updateactive(s)  activewidth[s]=activewidth[s]+mem[r+(s)].cint
/* -------- END: Achtung, die Makros werden direkt in trybreak() verwendet */


#define brokenptr(s)  link((s)+1)
  /* an insertion for this class will break here if anywhere */
#define brokenins(s)  info((s)+1) /* this insertion might break at |broken_ptr| */
#define lastinsptr(s)  link((s)+2) /* the most recent insertion for this |subtype| */
#define bestinsptr(s)  info((s)+2) /* the optimum most recent insertion */


#ifdef TEXXET
#define L_code	2
#define begin_L_code	(L_code+before)
#define end_L_code	(L_code+after)

#define R_code	4
#define begin_R_code	(R_code+before)
#define end_R_code	(R_code+after)

#define end_LR(s)	odd(subtype(s))
#define end_LR_type(s)	(subtype(s)+after-before)
#define begin_LR_type(s)	(subtype(s)-after+before)

#define edge_node	stylenode
#define edge_node_size	stylenodesize
#define edge_dist(s)	depth(s)

#define LR_save	curlist.LRs_field

#define R_node	unsetnode
#define left_to_right	0
#define right_to_left	1
#define reflected	(1-cur_dir)

#define safe_info(p)	(((p) == 0L) ? 0L : info(p))
#endif


#define whatlang(s) link((s)+1) /* language number, in the range |0..255| */
#define whatlhm(s) ztype((s)+1) /* minimum left fragment, in the range |1..63| */
#define whatrhm(s) subtype((s)+1) /* minimum right fragment, in the range |1..63| */
#define writetokens(s)  link((s)+1) /* reference count of token list to write */
#define writestream(s)  info((s)+1) /* stream number (0 to 17) */
#define openname(s)  link((s)+1) /* string number of file name to open */
#define openarea(s)  info((s)+2) /* string number of file area for |open_name| */
#define openext(s)  link((s)+2) /* string number of file extension for |open_name| */

/* -- end -- */
