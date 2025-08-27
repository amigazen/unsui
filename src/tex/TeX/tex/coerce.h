/* With GNU C we can store `zmem' in a global register */
/* (see texd.h for more) */

#ifdef REG_A5
#  define mem ((memoryword *)zmem)
#  define REGMEM_ZMEM
#else
#  define REGMEM_ZMEM register memoryword *mem=zmem;
#endif

#define REGMEM_ZEQTB register MEDmemoryword * CONST eqtb=zeqtb;



void init_arrays(int debugflag);	/* defined in init.c, used in texmf.c */



/* texmf.c: */
extern void topenin(void);
extern void get_date_and_time(integer *, integer *, integer *, integer *);
extern void calledit(ASCIIcode *, poolpointer, integer, integer);

/* init.c: */

void initialize(void);
#define initialize_regmem REGMEM_ZMEM REGMEM_ZEQTB
void println(void);
#define println_regmem
void printchar(ASCIIcode s);
#define printchar_regmem REGMEM_ZEQTB
void c_print(char *s);
#define c_print_regmem
void print(integer s);
#define print_regmem REGMEM_ZEQTB

#ifdef ERW_CODEPAGE
#  define slowprint(s) print(s)
#else
   void slowprint(integer s);
#  define slowprint_regmem REGMEM_ZEQTB
#endif

void printnl(strnumber s);
void c_printnl(char *s);
#define printnl_regmem
void c_printesc(char *s);
void printesc(strnumber s);
#define printesc_regmem REGMEM_ZEQTB
void printthedigs(eightbits k);
#define printthedigs_regmem
void printint(integer n);
#define printint_regmem
void printcs(integer p);
#define printcs_regmem REGMEM_ZEQTB
void sprintcs(halfword p);
#define sprintcs_regmem
void printfilename(integer n, integer a, integer e);
#define printfilename_regmem
void printsize(integer s);
#define printsize_regmem
void printwritewhatsit(strnumber s, halfword p);
#define printwritewhatsit_regmem REGMEM_ZMEM
VOLATILE void jumpout(void);
#define jumpout_regmem
void print_err(char *s);
#ifdef ERW_INTERACTION
#define printerr_regmem REGMEM_ZEQTB
#else
#define printerr_regmem
#endif
void p_print_err(strnumber s);

void error(void);
#ifdef ERW_INTERACTION
#define error_regmem REGMEM_ZEQTB
#else
#define error_regmem
#endif
VOLATILE void fatalerror(strnumber s);
#define fatalerror_regmem
VOLATILE void STDARGS overflow(strnumber s, integer n);
#define overflow_regmem
void confusion(char *s /*strnumber s*/);
#define confusion_regmem
boolean initterminal(void);
#define initterminal_regmem
strnumber makestring(void);
#define makestring_regmem
#if 0
boolean zstreqbuf();
#define streqbuf(s, k) zstreqbuf((strnumber) (s), (integer) (k))
#define streqbuf_regmem
boolean zstreqstr();
#define streqstr(s, t) zstreqstr((strnumber) (s), (strnumber) (t))
#define streqstr_regmem
#endif

boolean getstringsstarted(void);
#define getstringsstarted_regmem

void printtwo(integer n);
#define printtwo_regmem
void printhex(integer n);
#define printhex_regmem
void printromanint(integer n);
#define printromanint_regmem
void printcurrentstring(void);
#define printcurrentstring_regmem
void terminput(char *s /*integer s*/);
#define terminput_regmem
void interror(integer n);
#define interror_regmem
void normalizeselector(void);
#ifdef ERW_INTERACTION
#define normalizeselector_regmem REGMEM_ZEQTB
#else
#define normalizeselector_regmem
#endif
VOLATILE void succumb(void);
#ifdef ERW_INTERACTION
#define succumb_regmem REGMEM_ZEQTB
#else
#define succumb_regmem
#endif
void pauseforinstructions(void);
#define pauseforinstructions_regmem

integer half(integer x);
#define half_regmem
scaled rounddecimals(smallnumber k);
#define rounddecimals_regmem
void printscaled(scaled s);
#define printscaled_regmem
scaled multandadd(integer n, scaled x, scaled y, scaled maxanswer);
#define multandadd_regmem
scaled xovern(scaled x, integer n);
#define xovern_regmem
scaled xnoverd(scaled xs, integer n, integer d);
#define xnoverd_regmem
long_halfword badness(scaled t, scaled s);
#define badness_regmem

void printword(memoryword w);
#define printword_regmem
void showtokenlist(integer p, integer q, integer l);
#define showtokenlist_regmem REGMEM_ZMEM
void STDARGS runaway(void);
#define runaway_regmem REGMEM_ZMEM
long_halfword STDARGS getavail(void);
#define getavail_regmem REGMEM_ZMEM
void STDARGS flushlist(long_halfword p);
#define flushlist_regmem REGMEM_ZMEM
long_halfword STDARGS getnode(integer s);
#define getnode_regmem REGMEM_ZMEM
void STDARGS freenode(long_halfword p, long_halfword s);
#define freenode_regmem REGMEM_ZMEM

void sortavail(void);
#define sortavail_regmem REGMEM_ZMEM

long_halfword newnullbox(void);
#define newnullbox_regmem REGMEM_ZMEM
long_halfword newrule(void);
#define newrule_regmem REGMEM_ZMEM
long_halfword newligature(quarterword f, quarterword c, halfword q);
#define newligature_regmem REGMEM_ZMEM
long_halfword newligitem(quarterword c);
#define newligitem_regmem REGMEM_ZMEM
long_halfword newdisc(void);
#define newdisc_regmem REGMEM_ZMEM
long_halfword newmath(scaled w, smallnumber s);
#define newmath_regmem REGMEM_ZMEM
long_halfword newspec(halfword p);
#define newspec_regmem REGMEM_ZMEM
long_halfword newparamglue(smallnumber n);
#define newparamglue_regmem REGMEM_ZMEM REGMEM_ZEQTB
long_halfword newglue(halfword q);
#define newglue_regmem REGMEM_ZMEM
long_halfword newskipparam(smallnumber n);
#define newskipparam_regmem REGMEM_ZMEM REGMEM_ZEQTB
long_halfword newkern(scaled w);
#define newkern_regmem REGMEM_ZMEM
long_halfword newpenalty(integer m);
#define newpenalty_regmem REGMEM_ZMEM
#ifdef TEXXET
long_halfword new_edge(smallnumber s, scaled w);
#define new_edge_regmem REGMEM_ZMEM
long_halfword reverse(halfword this_box, halfword t);
#define reverse_regmem REGMEM_ZMEM
#endif
void checkmem(boolean printlocs);
#define checkmem_regmem
void searchmem(halfword p);
#define searchmem_regmem REGMEM_ZEQTB
void shortdisplay(integer p);
#define shortdisplay_regmem REGMEM_ZMEM
void printfontandchar(integer p);
#define printfontandchar_regmem REGMEM_ZMEM
void printmark(integer p);
#define printmark_regmem REGMEM_ZMEM
void printruledimen(scaled d);
#define printruledimen_regmem
void printglue(scaled d, integer order, strnumber s);
#define printglue_regmem
void printspec(integer p, strnumber s);
#define printspec_regmem REGMEM_ZMEM
void printfamandchar(halfword p);
#define printfamandchar_regmem REGMEM_ZMEM
void printdelimiter(halfword p);
#define printdelimiter_regmem REGMEM_ZMEM
void printsubsidiarydata(halfword p, ASCIIcode c);
#define printsubsidiarydata_regmem REGMEM_ZMEM
void printstyle(integer c);
#define printstyle_regmem
void printskipparam(integer n);
#define printskipparam_regmem
void shownodelist(integer p);
#define shownodelist_regmem REGMEM_ZMEM

void showbox(halfword p);
#define showbox_regmem REGMEM_ZEQTB
void deletetokenref(halfword p);
#define deletetokenref_regmem REGMEM_ZMEM
void deleteglueref(halfword p);
#define deleteglueref_regmem REGMEM_ZMEM
void flushnodelist(halfword p);
#define flushnodelist_regmem REGMEM_ZMEM
halfword copynodelist(halfword p);
#define copynodelist_regmem REGMEM_ZMEM
void printmode(integer m);
#define printmode_regmem
void pushnest(void);
#define pushnest_regmem
void popnest(void);
#define popnest_regmem REGMEM_ZMEM
void showactivities(void);
#define showactivities_regmem REGMEM_ZMEM REGMEM_ZEQTB
void printparam(integer n);
#define printparam_regmem
void begindiagnostic(void);
#define begindiagnostic_regmem REGMEM_ZEQTB
void enddiagnostic(boolean blankline);
#define enddiagnostic_regmem
void printlengthparam(integer n);
#define printlengthparam_regmem
void printcmdchr(quarterword cmd, halfword chrcode);
#define printcmdchr_regmem
void showeqtb(halfword n);
#define showeqtb_regmem REGMEM_ZMEM REGMEM_ZEQTB
halfword idlookup(integer j, integer l);
#define idlookup_regmem

/*void*/ long_halfword primitive(strnumber s, quarterword c, halfword o);
#define primitive_regmem REGMEM_ZEQTB

void newsavelevel(groupcode c);
#define newsavelevel_regmem
void eqdestroy(MEDmemoryword w);
#define eqdestroy_regmem REGMEM_ZMEM
void eqsave(halfword p, quarterword l);
#define eqsave_regmem REGMEM_ZEQTB
void eqdefine(halfword p, quarterword t, halfword e);
#define eqdefine_regmem REGMEM_ZEQTB
void eqworddefine(halfword p, integer w);
#define eqworddefine_regmem REGMEM_ZEQTB
void geqdefine(halfword p, quarterword t, halfword e);
#define geqdefine_regmem REGMEM_ZEQTB
void geqworddefine(halfword p, integer w);
#define geqworddefine_regmem REGMEM_ZEQTB
void saveforafter(halfword t);
#define saveforafter_regmem
void restoretrace(halfword p, char *s /*strnumber s*/);
#define restoretrace_regmem
void unsave(void);
#define unsave_regmem REGMEM_ZEQTB

void preparemag(void);
#define preparemag_regmem REGMEM_ZEQTB
void tokenshow(halfword p);
#define tokenshow_regmem REGMEM_ZMEM
void printmeaning(void);
#define printmeaning_regmem
void showcurcmdchr(void);
#define showcurcmdchr_regmem
void showcontext(void);
#define showcontext_regmem REGMEM_ZMEM REGMEM_ZEQTB
void begintokenlist(halfword p, quarterword t);
#define begintokenlist_regmem REGMEM_ZMEM REGMEM_ZEQTB
void endtokenlist(void);
#define endtokenlist_regmem
void backinput(void);
#define backinput_regmem REGMEM_ZMEM
void backerror(void);
#define backerror_regmem
void inserror(void);
#define inserror_regmem
void beginfilereading(void);
#define beginfilereading_regmem
void endfilereading(void);
#define endfilereading_regmem
void clearforerrorprompt(void);
#define clearforerrorprompt_regmem
void checkoutervalidity(void);
#define checkoutervalidity_regmem REGMEM_ZMEM

/* void */ long_halfword getnext(void);		/* eightbits */
#define getnext_regmem /* REGMEM_ZMEM */ REGMEM_ZEQTB
/* void */ long_halfword gettoken(void);	/* eightbits */
#define gettoken_regmem

void firmuptheline(void);
#ifdef ERW_INTERACTION
#define firmuptheline_regmem REGMEM_ZEQTB
#else
#define firmuptheline_regmem
#endif

void macrocall(void);
#define macrocall_regmem REGMEM_ZMEM REGMEM_ZEQTB
void insertrelax(void);
#define insertrelax_regmem
void expand(void);
#define expand_regmem REGMEM_ZMEM REGMEM_ZEQTB

/* void */ long_halfword getxtoken(void);	/* eightbits */
long_halfword getxnbtoken(boolean);		/* eightbits */
#define getxtoken_regmem
/* void */ long_halfword xtoken(void);		/* eightbits */
#define xtoken_regmem

void scanleftbrace(void);
#define scanleftbrace_regmem
void scanoptionalequals(void);
#define scanoptionalequals_regmem
boolean scankeyword(strnumber s);
#define scankeyword_regmem REGMEM_ZMEM
void muerror(void);
#define muerror_regmem
/*void*/ integer scaneightbitint(void);
#define scaneightbitint_regmem
/*void*/ integer scancharnum(void);
#define scancharnum_regmem
/*void*/ integer scanfourbitint(void);
#define scanfourbitint_regmem
/*void*/ integer scanfifteenbitint(void);
#define scanfifteenbitint_regmem
/*void*/ integer scantwentysevenbitint(void);
#define scantwentysevenbitint_regmem
/*void*/ integer scanfontident(void);
#define scanfontident_regmem REGMEM_ZEQTB
/*void*/ integer findfontdimen(boolean writing);
#define findfontdimen_regmem
/*void*/ integer scansomethinginternal(smallnumber level, boolean negative);
#define scansomethinginternal_regmem REGMEM_ZMEM REGMEM_ZEQTB
integer scanint(void);
#define scanint_regmem
/*void*/ integer scandimen(boolean mu, boolean inf, boolean shortcut);
#define scandimen_regmem REGMEM_ZMEM REGMEM_ZEQTB
/*void*/ long_halfword scanglue(smallnumber level);
#define scanglue_regmem REGMEM_ZMEM
long_halfword scanrulespec(void);

#define scanrulespec_regmem REGMEM_ZMEM
long_halfword strtoks(poolpointer p);
#define strtoks_regmem REGMEM_ZMEM
long_halfword thetoks(void);
#define thetoks_regmem REGMEM_ZMEM
void insthetoks(void);
#define insthetoks_regmem REGMEM_ZMEM
void convtoks(void);
#define convtoks_regmem REGMEM_ZMEM
long_halfword scantoks(boolean macrodef, boolean xpand);
#define scantoks_regmem REGMEM_ZMEM
void readtoks(integer n, halfword r);
#define readtoks_regmem  REGMEM_ZMEM REGMEM_ZEQTB
void passtext(void);
#define passtext_regmem
void changeiflimit(smallnumber l, halfword p);
#define changeiflimit_regmem REGMEM_ZMEM
void conditional(void);
#define conditional_regmem REGMEM_ZMEM REGMEM_ZEQTB
void beginname(void);
#define beginname_regmem
boolean morename(ASCIIcode c);
#define morename_regmem
void endname(void);
#define endname_regmem
void packfilename(strnumber n, strnumber a, strnumber e);
#define packfilename_regmem
void packbufferedname(smallnumber n, integer a, integer b);
#define packbufferedname_regmem
strnumber makenamestring(void);
#define makenamestring_regmem

#if 0
  strnumber amakenamestring(alphafile);
  strnumber bmakenamestring(bytefile);
  strnumber wmakenamestring(wordfile);
#else
# define amakenamestring(afile) (makenamestring())
# define bmakenamestring(bfile) (makenamestring())
# define wmakenamestring(wfile) (makenamestring())
#endif
#define amakenamestring_regmem
#define bmakenamestring_regmem
#define wmakenamestring_regmem

void scanfilename(void);
#define scanfilename_regmem
void packjobname(strnumber s);
#define packjobname_regmem
void promptfilename(char *s /*strnumber s*/, strnumber e);
#ifdef ERW_INTERACTION
#define promptfilename_regmem REGMEM_ZEQTB
#else
#define promptfilename_regmem
#endif
void openlogfile(void);
#define openlogfile_regmem REGMEM_ZEQTB
void startinput(void);
#define startinput_regmem REGMEM_ZEQTB

internalfontnumber readfontinfo(halfword u, strnumber nom, strnumber aire,
				scaled s);
#define readfontinfo_regmem REGMEM_ZEQTB

void charwarning(internalfontnumber f, eightbits c);
#define charwarning_regmem REGMEM_ZEQTB
long_halfword newcharacter(internalfontnumber f, eightbits c);
#define newcharacter_regmem REGMEM_ZMEM
void dviswap(void);
#define dviswap_regmem
void dvifour(integer x);
#define dvifour_regmem
void dvipop(integer l);
#define dvipop_regmem
void dvifontdef(internalfontnumber f );
#define dvifontdef_regmem
/* void movement(scaled w, eightbits o); */
#define movement_regmem REGMEM_ZMEM
/* void prunemovements(integer l); */
#define prunemovements_regmem REGMEM_ZMEM
/* void specialout(halfword p); */
#define specialout_regmem REGMEM_ZMEM
/* void writeout(halfword p); */
#define writeout_regmem REGMEM_ZMEM

void outwhat(halfword p);
#define outwhat_regmem REGMEM_ZMEM

/* void hlistout(void); */
#define hlistout_regmem REGMEM_ZMEM
/* void vlistout(void); */
#define vlistout_regmem REGMEM_ZMEM

void shipout(halfword p);
#define shipout_regmem REGMEM_ZMEM REGMEM_ZEQTB

void scanspec(groupcode c, boolean threecodes);
#define scanspec_regmem
long_halfword hpack(halfword p, scaled w, smallnumber m);
#define hpack_regmem REGMEM_ZMEM REGMEM_ZEQTB
long_halfword vpackage(halfword p, scaled h, smallnumber m, scaled l);
#define vpackage_regmem REGMEM_ZMEM REGMEM_ZEQTB
void appendtovlist(halfword b);
#define appendtovlist_regmem REGMEM_ZMEM REGMEM_ZEQTB
long_halfword newnoad(void);
#define newnoad_regmem REGMEM_ZMEM
long_halfword newstyle(smallnumber s);
#define newstyle_regmem REGMEM_ZMEM
long_halfword newchoice(void);
#define newchoice_regmem REGMEM_ZMEM

#if 0	/* (br) unnoetig, nur wegen PASCAL eingefuehrt */
void showinfo(void);
#define showinfo_regmem REGMEM_ZMEM
#endif

long_halfword fractionrule(scaled t);
#define fractionrule_regmem REGMEM_ZMEM
long_halfword overbar(halfword b, scaled k, scaled t);
#define overbar_regmem REGMEM_ZMEM
long_halfword charbox(internalfontnumber f, quarterword c);
#define charbox_regmem REGMEM_ZMEM
void stackintobox(halfword b, internalfontnumber f, quarterword c);
#define stackintobox_regmem REGMEM_ZMEM
scaled heightplusdepth(internalfontnumber f, quarterword c);
#define heightplusdepth_regmem
long_halfword vardelimiter(halfword d, smallnumber s, scaled v);
#define vardelimiter_regmem REGMEM_ZMEM REGMEM_ZEQTB
long_halfword rebox(long_halfword b, scaled w);
#define rebox_regmem REGMEM_ZMEM
long_halfword mathglue(halfword g, scaled m);
#define mathglue_regmem REGMEM_ZMEM
void mathkern(halfword p, scaled m);
#define mathkern_regmem REGMEM_ZMEM
void flushmath(void);
#define flushmath_regmem REGMEM_ZMEM

#if 0
halfword zcleanbox();
#define cleanbox(p, s) zcleanbox((halfword) (p), (smallnumber) (s))
#endif
#define cleanbox_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0	/* math.c */
void zfetch();
#define fetch(a) zfetch((halfword) (a))
#endif
#define fetch_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0	/* math.c */
void zmakeover();
#define makeover(q) zmakeover((halfword) (q))
#endif
#define makeover_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0	/* math.c */
void zmakeunder();
#define makeunder(q) zmakeunder((halfword) (q))
#endif
#define makeunder_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0	/* math.c */
void zmakevcenter();
#define makevcenter(q) zmakevcenter((halfword) (q))
#endif
#define makevcenter_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0	/* math.c */
void zmakeradical();
#define makeradical(q) zmakeradical((halfword) (q))
#endif
#define makeradical_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0	/* math.c */
void zmakemathaccent();
#define makemathaccent(q) zmakemathaccent((halfword) (q))
#endif
#define makemathaccent_regmem REGMEM_ZMEM
#if 0	/* math.c */
void zmakefraction();
#define makefraction(q) zmakefraction((halfword) (q))
#endif
#define makefraction_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0	/* math.c */
scaled zmakeop();
#define makeop(q) zmakeop((halfword) (q))
#endif
#define makeop_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0	/* math.c */
void zmakeord();
#define makeord(q) zmakeord((halfword) (q))
#endif
#define makeord_regmem REGMEM_ZMEM
#if 0	/* math.c */
void zmakescripts();
#define makescripts(q, delta) zmakescripts((halfword) (q), (scaled) (delta))
#endif
#define makescripts_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0
smallnumber zmakeleftright();
#define makeleftright(q, style, maxd, maxh) \
  zmakeleftright((halfword) (q), (smallnumber) (style), \
		(scaled) (maxd), (scaled) (maxh))
#endif
#define makeleftright_regmem REGMEM_ZMEM REGMEM_ZEQTB
#if 0
void mlisttohlist(smallnumber);		/* math.c */
#endif
#define mlisttohlist_regmem REGMEM_ZMEM REGMEM_ZEQTB

void pushalignment(void);
#define pushalignment_regmem REGMEM_ZMEM
void popalignment(void);
#define popalignment_regmem REGMEM_ZMEM
void getpreambletoken(void);
#define getpreambletoken_regmem REGMEM_ZEQTB
void initalign(void);
#define initalign_regmem REGMEM_ZMEM REGMEM_ZEQTB
void initspan(halfword p);
#define initspan_regmem
void initrow(void);
#define initrow_regmem REGMEM_ZMEM
void initcol(void);
#define initcol_regmem REGMEM_ZMEM
boolean fincol(void);
#define fincol_regmem REGMEM_ZMEM
void finrow(void);
#define finrow_regmem REGMEM_ZMEM REGMEM_ZEQTB
void finalign(void);
#define finalign_regmem REGMEM_ZMEM REGMEM_ZEQTB
void alignpeek(void);
#define alignpeek_regmem
long_halfword finiteshrink(halfword p);
#define finiteshrink_regmem REGMEM_ZMEM

void trybreak(integer pi, smallnumber breaktype);
#define trybreak_regmem REGMEM_ZMEM REGMEM_ZEQTB

void postlinebreak(integer finalwidowpenalty);
#define postlinebreak_regmem REGMEM_ZMEM REGMEM_ZEQTB
smallnumber reconstitute(smallnumber j, smallnumber n, halfword bchar,
			  halfword hchar);
#define reconstitute_regmem REGMEM_ZMEM
void hyphenate(void);
#define hyphenate_regmem REGMEM_ZMEM

trieopcode newtrieop(smallnumber d, smallnumber n, trieopcode v);
#define newtrieop_regmem
triepointer trienode(triepointer p);
#define trienode_regmem
triepointer compresstrie(triepointer p);
#define compresstrie_regmem
void firstfit(triepointer p);
#define firstfit_regmem
void triepack(triepointer p);
#define triepack_regmem
void triefix(triepointer p);
#define triefix_regmem
void newpatterns(void);
#define newpatterns_regmem REGMEM_ZMEM REGMEM_ZEQTB
void inittrie(void);
#define inittrie_regmem

void linebreak(integer finalwidowpenalty);
#define linebreak_regmem REGMEM_ZMEM REGMEM_ZEQTB

void newhyphexceptions(void);
#define newhyphexceptions_regmem REGMEM_ZMEM REGMEM_ZEQTB
halfword prunepagetop(halfword p);
#define prunepagetop_regmem REGMEM_ZMEM
halfword vertbreak(halfword p, scaled h, scaled d);
#define vertbreak_regmem REGMEM_ZMEM
long_halfword vsplit(eightbits n, scaled h);
#define vsplit_regmem REGMEM_ZMEM REGMEM_ZEQTB
void printtotals(void);
#define printtotals_regmem
void freezepagespecs(smallnumber s);
#define freezepagespecs_regmem REGMEM_ZEQTB
void boxerror(eightbits n);
#define boxerror_regmem REGMEM_ZEQTB
void ensurevbox(eightbits n);
#define ensurevbox_regmem REGMEM_ZMEM REGMEM_ZEQTB
void fireup(halfword c);
#define fireup_regmem REGMEM_ZMEM REGMEM_ZEQTB
void buildpage(void);
#define buildpage_regmem REGMEM_ZMEM REGMEM_ZEQTB

void appspace(void);
#define appspace_regmem REGMEM_ZMEM REGMEM_ZEQTB

void insertdollarsign(void);
#define insertdollarsign_regmem
void youcant(void);
#define youcant_regmem
void reportillegalcase(void);
#define reportillegalcase_regmem
boolean privileged(void);
#define privileged_regmem
boolean itsallover(void);
#define itsallover_regmem REGMEM_ZMEM REGMEM_ZEQTB
void appendglue(void);
#define appendglue_regmem REGMEM_ZMEM
void appendkern(void);
#define appendkern_regmem REGMEM_ZMEM
void offsave(void);
#define offsave_regmem REGMEM_ZMEM
void extrarightbrace(void);
#define extrarightbrace_regmem
void normalparagraph(void);
#define normalparagraph_regmem REGMEM_ZEQTB

void boxend(integer boxcontext, long_halfword curbox /*zusaetzlich*/);
#define boxend_regmem REGMEM_ZMEM
void beginbox(integer boxcontext);
#define beginbox_regmem REGMEM_ZMEM REGMEM_ZEQTB
void scanbox(integer boxcontext);
#define scanbox_regmem
void package(smallnumber c);
#define package_regmem REGMEM_ZMEM REGMEM_ZEQTB

smallnumber normmin(integer h);
#define normmin_regmem
void newgraf(boolean indented);
#define newgraf_regmem REGMEM_ZMEM REGMEM_ZEQTB
void indentinhmode(boolean indented /*void*/);
#define indentinhmode_regmem REGMEM_ZMEM REGMEM_ZEQTB
void headforvmode(void);
#define headforvmode_regmem
void endgraf(void);
#define endgraf_regmem REGMEM_ZEQTB
void begininsertoradjust(void);
#define begininsertoradjust_regmem
void makemark(void);
#define makemark_regmem REGMEM_ZMEM
void appendpenalty(void);
#define appendpenalty_regmem REGMEM_ZMEM
void deletelast(void);
#define deletelast_regmem REGMEM_ZMEM
void unpackage(void);
#define unpackage_regmem REGMEM_ZMEM REGMEM_ZEQTB
void appenditaliccorrection(void);
#define appenditaliccorrection_regmem REGMEM_ZMEM
void appenddiscretionary(void);
#define appenddiscretionary_regmem REGMEM_ZMEM REGMEM_ZEQTB
void builddiscretionary(void);
#define builddiscretionary_regmem REGMEM_ZMEM
void makeaccent(void);
#define makeaccent_regmem REGMEM_ZMEM REGMEM_ZEQTB
void alignerror(void);
#define alignerror_regmem
void noalignerror(void);
#define noalignerror_regmem
void omiterror(void);
#define omiterror_regmem
void doendv(void);
#define doendv_regmem
void cserror(void);
#define cserror_regmem
void pushmath(groupcode c);
#define pushmath_regmem
#define gointoordinarymath_regmem REGMEM_ZEQTB
void initmath(void);
#define initmath_regmem REGMEM_ZMEM REGMEM_ZEQTB
void starteqno(void);
#define starteqno_regmem
void scanmath(halfword p);
#define scanmath_regmem REGMEM_ZMEM REGMEM_ZEQTB
void setmathchar(integer c);
#define setmathchar_regmem REGMEM_ZMEM REGMEM_ZEQTB
void mathlimitswitch(void);
#define mathlimitswitch_regmem REGMEM_ZMEM
void scandelimiter(halfword p, boolean r);
#define scandelimiter_regmem REGMEM_ZMEM REGMEM_ZEQTB
void mathradical(void);
#define mathradical_regmem REGMEM_ZMEM
void mathac(void);
#define mathac_regmem REGMEM_ZMEM REGMEM_ZEQTB
void appendchoices(void);
#define appendchoices_regmem REGMEM_ZMEM
halfword finmlist(halfword p);
#define finmlist_regmem REGMEM_ZMEM
void buildchoices(void);
#define buildchoices_regmem REGMEM_ZMEM
void subsup(void);
#define subsup_regmem REGMEM_ZMEM
void mathfraction(void);
#define mathfraction_regmem REGMEM_ZMEM
void mathleftright(void);
#define mathleftright_regmem REGMEM_ZMEM

void check_that_dollar_follows(void);
void aftermath(void);
#define aftermath_regmem REGMEM_ZMEM REGMEM_ZEQTB

void resumeafterdisplay(void);
#ifdef ERW_INTERACTION
#define resumeafterdisplay_regmem REGMEM_ZEQTB
#else
#define resumeafterdisplay_regmem
#endif
void getrtoken(void);
#define getrtoken_regmem
/* void trapzeroglue(void); */
long_halfword trapzeroglue(long_halfword);
#define trapzeroglue_regmem REGMEM_ZMEM
void doregistercommand(smallnumber a);
#define doregistercommand_regmem REGMEM_ZMEM REGMEM_ZEQTB
void alteraux(void);
#define alteraux_regmem
void alterprevgraf(void);
#define alterprevgraf_regmem
void alterpagesofar(void);
#define alterpagesofar_regmem
void alterinteger(void);
#define alterinteger_regmem
void alterboxdimen(void);
#define alterboxdimen_regmem REGMEM_ZMEM REGMEM_ZEQTB
void newfont(smallnumber a);
#define newfont_regmem REGMEM_ZEQTB
void newinteraction(void);
#ifdef ERW_INTERACTION
#define newinteraction_regmem REGMEM_ZEQTB
#else
#define newinteraction_regmem
#endif

int call_new_patterns(void);
#define call_new_patterns_regmem REGMEM_ZEQTB

void prefixedcommand(void);
#define prefixedcommand_regmem REGMEM_ZMEM REGMEM_ZEQTB

void doassignments(void);
#define doassignments_regmem
void openorclosein(void);
#define openorclosein_regmem
void issuemessage(void);
#define issuemessage_regmem REGMEM_ZMEM REGMEM_ZEQTB
void shiftcase(void);
#define shiftcase_regmem REGMEM_ZMEM REGMEM_ZEQTB
void showwhatever(void);
#define showwhatever_regmem REGMEM_ZMEM REGMEM_ZEQTB

void storefmtfile(void);
#define storefmtfile_regmem REGMEM_ZMEM REGMEM_ZEQTB

void newwhatsit(smallnumber s, smallnumber w);
#define newwhatsit_regmem REGMEM_ZMEM
void newwritewhatsit(smallnumber w);
#define newwritewhatsit_regmem REGMEM_ZMEM
void doextension(void);
#define doextension_regmem REGMEM_ZMEM REGMEM_ZEQTB
void fixlanguage(void);
#define fixlanguage_regmem REGMEM_ZMEM REGMEM_ZEQTB
void handlerightbrace(void);
#define handlerightbrace_regmem REGMEM_ZMEM REGMEM_ZEQTB

void maincontrol(void);
#define maincontrol_regmem REGMEM_ZMEM REGMEM_ZEQTB

void giveerrhelp(void);
#define giveerrhelp_regmem REGMEM_ZEQTB
boolean openfmtfile(void);
#define openfmtfile_regmem
boolean loadfmtfile(void);
#define loadfmtfile_regmem REGMEM_ZMEM REGMEM_ZEQTB
void closefilesandterminate(void);
#define closefilesandterminate_regmem REGMEM_ZEQTB

void finalcleanup(void);
#ifdef ERW_INTERACTION
#define finalcleanup_regmem REGMEM_ZMEM REGMEM_ZEQTB
#else
#define finalcleanup_regmem REGMEM_ZMEM
#endif

void initprim(void);
#define initprim_regmem REGMEM_ZEQTB

void debughelp(void);
#define debughelp_regmem REGMEM_ZMEM REGMEM_ZEQTB

void texbody(void);
#define texbody_regmem REGMEM_ZEQTB

#ifdef MLTEX
#ifndef effective_char
integer effective_char(integer in_chr);
#define effective_char_regmem REGMEM_ZEQTB
#endif

halfword substitute_char_list(halfword p, quarterword c, quarterword f);
#define substitute_char_list_regmem REGMEM_ZEQTB
#endif

/*
 * The C compiler ignores most unnecessary casts (i.e., casts of something
 * to its own type).  However, for structures, it doesn't.  Therefore,
 * we have to redefine these two macros so that they don't try to cast
 * the argument (a memoryword) as a memoryword.
 */
#if 0
#undef	eqdestroy
#define	eqdestroy(x)	zeqdestroy(x)
#undef	printword
#define	printword(x)	zprintword(x)
#endif
