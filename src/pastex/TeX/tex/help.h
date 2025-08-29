/* help.h */

/* --- often used print texts (*no* help texts) --- */
#define STR_H_MISSING_LEFTBRACE	257 /* 653 scanning tex3 tex7 */
#define STR_H_IMPROPER		258 /* 676 scanning tex5 tex6 tex8 */
#define STR_H_YOUCANTUSE	287 /* 681 scanning tex6 tex7(2) tex8 */
#define STR_H_AFTER		288 /* 682 scanning tex7 */
/* --- end --- */


extern char *help_messages[];

/* buildbox.c */

#define STR_H_YOU_SHOULD_LEADERS	1 /* 1060-1062 buildbox */

#define STR_H_SORRY_LASTBOX_VOID	2 /* 1063 buildbox */

#define STR_H_SORRY_CANTTAKETHINGS	3 /* 1064 buildbox tex7(3) */
#define STR_H_THIS_LASTBOX_VOID		4 /* 1065 buildbox */

#define STR_H_IM_WORKING_VSPLIT		5 /* 1067,8 buildbox */

#define STR_H_IWAS_EXPECT_HVBOX		6 /* 1070-2 buildbox */

/* dump.c */

#define STR_H_DUMPISANONO		7 /* 1250 dump */

/* gettoken.c */

#define STR_H_AFUNNY_SYMBOL		8 /* 610,1 gettoken */

/* itex.c */

#define STR_H_ALL_PATTERNS		9 /* 647 itex */
#define STR_H_SEE_APP_H			10 /* 649 itex(3) */

/* overflow.c */

#define STR_H_IF_NEED_CAPACITY		11 /* 287,8 overflow */

/* math.c */

#define STR_H_SOMEWHERE_IN_MATH		12 /* 878-881 math */

#define STR_H_SORRY_TEXTFONT2		13 /* 1152-1154 math */

#define STR_H_SORRY_TEXTFONT3		14 /* 1156-1158 math */

#define STR_H_THEDOLLARTHATI		15 /* 1160,1 math */

/* scanning.c */

#define STR_H_A_LEFTBRACE_MANDATORY	16 /* 654-657 scanning */

#define STR_H_IM_GOING_MU_PT		17 /* 659 scanning */

#define STR_H_ANUMBER_SHOULD_HERE	18 /* 661-3 scanning */

#define STR_H_YOU_REFER_SPACEFACTOR	19 /* 677-9 scanning */
#define STR_H_IM_FORGETTING_ZERO	20 /* 680 scanning(2) */

#define STR_H_I_CHANGED_TO_ZERO		21 /* 685 scanning(5) */

#define STR_H_REGISTERNUMBER_BETWEEN	22 /* 684 scanning */
#define STR_H_CHARNUMBER_BETWEEN	23 /* 687 scanning */
#define STR_H_SINCE_I_EXPECT_BETWEEN	24 /* 689 scanning */
#define STR_H_NUMERICMATH_BETWEEN	25 /* 691 scanning */
#define STR_H_NUMERICDEL_BETWEEN	26 /* 693 scanning */

#define STR_H_ONECHAR_CS_BELONGS	27 /* 695,6 scanning */

#define STR_H_ICAN_ONLY_GOUPTO		28 /* 698,9 scanning */

#define STR_H_I_DDDONT_GO		29 /* 703 scanning */

#define STR_H_UNIT_IN_MATHGLUE		30 /* 707 scanning */
#define STR_H_DIMENSIONS_EMEX		31 /* 720-2 scanning */

#define STR_H_TO_RECOVER_GRACEF		32 /* 708-710 scanning(2) */

#define STR_H_ICANT_WORK_BIGGER		33 /* 724,5 scanning */

#define STR_H_I_LOOKING_FOR_CS		34 /* 811,2 scanning */

#define STR_H_TO_INCREASE_FNTP		35 /* 815,6 scanning */

/* shipout.c */

#define STR_H_ICANT_HANDLE_THAT		36 /* 1005 shipout tex8 */
#define STR_H_ONTHISPAGE_WRITE		37 /* 1288 shipout */

#define STR_H_THE_PAGE_JUST		38 /* 827,8 shipout */

/* tex8.c */

#define	STR_H_YOUVE_CLOSED_MORE		39 /* 1038,9 tex8 */

#define	STR_H_YOUR_SNEAKY_OUTPUT	40 /* 1004 tex8 */

#define	STR_H_YOUR_OUTPUT_COMMANDS	41 /* 1007-9 tex8 */

#define	STR_H_IM_GUESSING_ENDALIGN	42 /* 1118 tex7 */

#define STR_H_ILLPRETENDLONG		43 /* 1176 tex7 */
#define STR_H_ILLPRETENDGLOBAL		44 /* 1173 tex7 */

#define STR_H_YOUSHOULDREAD		45 /* 1193,4 tex8 */

#define STR_H_IMGOINGTOUSENULL		46 /* 1198 tex7 */

/* tex7.c */

#define STR_H_TOPUTAHORIZONTAL		47 /* 1076,7 tex7 */

#define STR_H_IMCHANGINGTOINSERT	48 /* 1079 tex7 */

#define STR_H_TRY_VSKIPLASTSKIP		49 /* 1080 tex7 */
#define STR_H_TRY_KERNLASTSKIP		50 /* 1081 tex7 */
#define STR_H_PERHAPSYOUCAN_OUTPUT	51 /* 1082 tex7 */

#define STR_H_SORRYPANDORA		52 /* 1091-3 tex7 */

#define STR_H_SORRY_THIRDPARTOFDISC	53 /* 1095,6 tex7 */

#define STR_H_WOW_INEVERTHOUGHT		54 /* 1098,9 tex7 */

#define STR_H_DISCLISTS_MUST		55 /* 1101 tex7 */

#define STR_H_IVEPUTIN_TOFIX		56 /* 1104-6 tex7 */

#define STR_H_ICANTFIGURE_TABMARK	57 /* 1108 tex7(2) */
#define STR_H_HERE_AMPERSAND		58 /* 1109-10 tex7 */
#define STR_H_UPABOVE_PREVALIGN		59 /* 1111-3 tex7(2) */
#define STR_H_ORCRORSPAN		60 /* 1114 tex7 */

#define STR_H_IEXPECT_NOALIGN		61 /* 1115 tex7 */
#define STR_H_AN_ALIGNMENT		62 /* 1116 tex7(2) */
#define STR_H_IEXPECT_OMIT		63 /* 1117 tex7 */

#define STR_H_IMIGNORING_CSNAME		64 /* 1119 tex7 */
#define STR_H_IMIGNORING_LIMITS		65 /* 1124 tex7 */

#define STR_H_IWASEXP_BRACE		66 /* 1126-31 tex7 */

#define STR_H_IMCHANGING_ACCENT		67 /* 1134,5 tex7 */

#define STR_H_ITREAT_XUP1UP2		68 /* 1137 tex7 */
#define STR_H_ITREAT_XDOWN1DOWN2	69 /* 1139 tex7 */

#define STR_H_IM_IGNORING_FRACTION	70 /* 1147-9 tex7 */

#define STR_H_IM_IGNORING_RIGHT		71 /* 1150 tex7 */

#define STR_H_PLEASEDONTSAYDEF		72 /* 1178-82 tex7 */

#define STR_H_IMFORGETTING		73 /* 1203 tex7 */

#define STR_H_ICANTCARRYOUT		74 /* 1201,2 tex7 */

#define STR_H_IALLOWONLYVALUES		75 /* 1205 tex7 */
#define STR_H_IALLOWONLYNONNEG		76 /* 1206 tex7 */

#define STR_H_ICANONLYHANDLE		77 /* 1215,6 tex7 */

#define STR_H_THATWASANOTHER		78 /* 1223 tex7 */

#define STR_H_THISERRORMESSAGE		79 /* 1224-7 tex7 */

#define STR_H_THISISNTERR		80 /* 1234-6 tex7(2) */
#define STR_H_ANDTYPEITRACING		81 /* 1237,8 tex7 */

#define STR_H_ICAN_MAGRATIO		82 /* 546,7 tex7 */

#define STR_H_THEMAGRATIO		83 /* 549 tex7(2) */

/* tex6.c */

#define STR_H_HYPH_EXC_ONLY		84 /* 936,7 tex6 */

#define STR_H_LETTERSINHYPH		85 /* 939,40 tex6 */

#define STR_H_THEBOX_VSPLIT		86 /* 955 tex6 */
#define STR_H_SHRINKABLEGLUE		87 /* 956,7 tex6(2) */

#define STR_H_THEBOX_SPLITHBOX		88 /* 961,2 tex6 */

#define STR_H_TUTTUT_INSERT		89 /* 983,4 tex6 */
#define STR_H_PROCEED_DISCRADCONT	90 /* 985 tex6(2) */

#define STR_H_THEPAGE_INFINITELY	91 /* 988 tex6 */

#define STR_H_THECORRECTIONGLUE		92 /* 992,3 tex6 */

#define STR_H_YOUSHOULDNTUSEBOX		93 /* 997 tex6 */

#define STR_H_IVE_OUTPUT_AWRY		94 /* 1000-2 tex6 */

#define STR_H_IVE_BEGINMATH		95 /* 1011,2 tex6 */

#define STR_H_SORRY_NOTPROGRAMMED	96 /* 1014-7 tex6 */

#define STR_H_IVE_INSERTED_SOMETHING	97 /* 1030-4 tex6 */

#define STR_H_THINGS_MIXEDUP		98 /* 1036 tex6 */

#define STR_H_IVE_GROUPCLOSING		99 /* 1042-6 tex6 */

#define STR_H_SINCETHE_OFFENSIVE	100 /* 915 tex5 tex6(3) */

/* tex0.c */

#define STR_H_IHAVEJUSTDELETED		101 /* 277,8 tex0 */

#define STR_H_SORRY_DONTKNOWHELP	102 /* 279 tex0 */
#define STR_H_SORRY_GAVEHELP		103 /* 281 tex0 */
#define STR_H_MAYBE_ASK_HUMAN		104 /* 280 tex0(2) */
#define STR_H_ANERRORMIGHT		105 /* 282,3 tex0 */

#define STR_H_IMBROKEN_FIX		106 /* 290 tex0 */

#define STR_H_ONEOFYOURFAUXPAS		107 /* 292,3 tex0 */

#define STR_H_YOURANG			108 /* 295-7 tex0 */

/* fatalerror() */

#define STR_H_FE_ENDOFFILE		109 /* 261 tex0 */
#define STR_H_FE_INTERWOVEN		110 /* 591 gettoken tex2 tex5(2)*/
#define STR_H_FE_JOBAB_NO		111 /* 613 gettoken */
#define STR_H_FE_CANNOTREAD		112 /* 749 tex3 */
#define STR_H_FE_JOBAB_FILE		113 /* 786 tex3 */

/* tex2.c */

#define STR_H_AFORBIDDEN_SKIPPED	114 /* 596 tex2 */
#define STR_H_THISKIND_HAPPENS_IF	115 /* 597,8 tex2(2) */
#define STR_H_THEFILEENDEDWHILE		116 /* 599 tex2 */

#define STR_H_ISUSPECT_FORGOTTEN	117 /* 604-607 tex2 */

#define STR_H_THECSATTHEEND		118 /* 616-620 tex2 */

#define STR_H_THECSMARKED		119 /* 623,624 tex2 */

#define STR_H_IVERUNACROSS		120 /* 635-640 tex2 */

#define STR_H_ISUSPECT_CAUSING		121 /* 643-645 tex2(2) */

#define STR_H_IFYOUSAY_DEFA		122 /* 648-651 tex2 */

#define STR_H_IMIGNORING_IF		123 /* 773 tex2 tex3 */

/* tex5.c */

#define STR_H_DISPLAYSCANUSE		124 /* 888,9 tex5(2) */
#define STR_H_SOIVE_FORMULAS		125 /* 890 tex5 */

#define STR_H_THERESHOULD_TAB		126 /* 897-898 tex5(2) */
#define STR_H_NONESOIVEPUT		127 /* 899 tex5 */
#define STR_H_MORETHANONE_SOIM		128 /* 901 tex5 */

#define STR_H_YOUHAVE_SPANORTAB		129 /* 904-6 tex5 */

#define STR_H_THEPARAGRAPHJUST		130 /* 911-4 tex5 */

/* tex4.c */
/* tex3b.c */

#define STR_H_IWASNT_READSIZE		131 /* 799-803 tex3b */

#define STR_H_IMAFRAID_FONT		132 /* 806-809 tex3b */

/* tex3.c */

#define STR_H_WHEREWAS_LEFTBRACE	133 /* 738,9 tex3 */

#define STR_H_IMGOING_TABSIGN		134 /* 741 tex3 */

#define STR_H_IVEINSERTED_DIGIT		135 /* 743,4 tex3 */

#define STR_H_YOUMEANT_TAB		136 /* 746-8 tex3 */

#define STR_H_THISREADHASUNBALANCED	137 /* 751 tex3 */

#define STR_H_IWASEXPECTING_DIDNT	138 /* 777 tex3 */

/* TeX 3.141 */

#define STR_H_SORRY_SETBOXNOTALLOW	139 /* tex8 */

/* -- end -- */
