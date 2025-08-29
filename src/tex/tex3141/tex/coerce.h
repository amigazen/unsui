void initialize(void);
#define initialize_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void println(void);
#define println_regmem
void printchar(ASCIIcode s);
#define printchar_regmem register memoryword *eqtb=zeqtb;
void print(integer s);
#define print_regmem register memoryword *eqtb=zeqtb;
void slowprint(integer s);
#define slowprint_regmem
void printnl(strnumber s);
#define printnl_regmem
void printesc(strnumber s);
#define printesc_regmem register memoryword *eqtb=zeqtb;
void printthedigs(eightbits k);
#define printthedigs_regmem
void printint(integer n);
#define printint_regmem
void printcs(integer p);
#define printcs_regmem register memoryword *eqtb=zeqtb;
void sprintcs(halfword p);
#define sprintcs_regmem
void printfilename(integer n,integer a,integer e);
#define printfilename_regmem
void printsize(integer s);
#define printsize_regmem
void printwritewhatsit(strnumber s,halfword p);
#define printwritewhatsit_regmem register memoryword *mem=zmem;
void jumpout(void);
#define jumpout_regmem
void error(void);
#define error_regmem
void fatalerror(strnumber s);
#define fatalerror_regmem
void overflow(strnumber s,integer n);
#define overflow_regmem
void confusion(strnumber s);
#define confusion_regmem
boolean initterminal(void);
#define initterminal_regmem
strnumber makestring(void);
#define makestring_regmem
boolean streqbuf(strnumber s,integer k);
#define streqbuf_regmem
boolean streqstr(strnumber s,strnumber t);
#define streqstr_regmem
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
void terminput(void);
#define terminput_regmem
void interror(integer n);
#define interror_regmem
void normalizeselector(void);
#define normalizeselector_regmem
void pauseforinstructions(void);
#define pauseforinstructions_regmem
integer half(integer x);
#define half_regmem
scaled rounddecimals(smallnumber k);
#define rounddecimals_regmem
void printscaled(scaled s);
#define printscaled_regmem
scaled multandadd(integer n,scaled x,scaled y,scaled maxanswer);
#define multandadd_regmem
scaled xovern(scaled x,integer n);
#define xovern_regmem
scaled xnoverd(scaled x,integer n,integer d);
#define xnoverd_regmem
halfword badness(scaled t,scaled s);
#define badness_regmem
void printword(memoryword w);
#define printword_regmem
void showtokenlist(integer p,integer q,integer l);
#define showtokenlist_regmem register memoryword *mem=zmem;
void runaway(void);
#define runaway_regmem register memoryword *mem=zmem;
halfword getavail(void);
#define getavail_regmem register memoryword *mem=zmem;
void flushlist(halfword p);
#define flushlist_regmem register memoryword *mem=zmem;
halfword getnode(integer s);
#define getnode_regmem register memoryword *mem=zmem;
void freenode(halfword p,halfword s);
#define freenode_regmem register memoryword *mem=zmem;
void sortavail(void);
#define sortavail_regmem register memoryword *mem=zmem;
halfword newnullbox(void);
#define newnullbox_regmem register memoryword *mem=zmem;
halfword newrule(void);
#define newrule_regmem register memoryword *mem=zmem;
halfword newligature(quarterword f,quarterword c,halfword q);
#define newligature_regmem register memoryword *mem=zmem;
halfword newligitem(quarterword c);
#define newligitem_regmem register memoryword *mem=zmem;
halfword newdisc(void);
#define newdisc_regmem register memoryword *mem=zmem;
halfword newmath(scaled w,smallnumber s);
#define newmath_regmem register memoryword *mem=zmem;
halfword newspec(halfword p);
#define newspec_regmem register memoryword *mem=zmem;
halfword newparamglue(smallnumber n);
#define newparamglue_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
halfword newglue(halfword q);
#define newglue_regmem register memoryword *mem=zmem;
halfword newskipparam(smallnumber n);
#define newskipparam_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
halfword newkern(scaled w);
#define newkern_regmem register memoryword *mem=zmem;
halfword newpenalty(integer m);
#define newpenalty_regmem register memoryword *mem=zmem;
void checkmem(boolean printlocs);
#define checkmem_regmem register memoryword *mem=zmem;
void searchmem(halfword p);
#define searchmem_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void shortdisplay(integer p);
#define shortdisplay_regmem register memoryword *mem=zmem;
void printfontandchar(integer p);
#define printfontandchar_regmem register memoryword *mem=zmem;
void printmark(integer p);
#define printmark_regmem register memoryword *mem=zmem;
void printruledimen(scaled d);
#define printruledimen_regmem
void printglue(scaled d,integer order,strnumber s);
#define printglue_regmem
void printspec(integer p,strnumber s);
#define printspec_regmem register memoryword *mem=zmem;
void printfamandchar(halfword p);
#define printfamandchar_regmem register memoryword *mem=zmem;
void printdelimiter(halfword p);
#define printdelimiter_regmem register memoryword *mem=zmem;
void printsubsidiarydata(halfword p,ASCIIcode c);
#define printsubsidiarydata_regmem register memoryword *mem=zmem;
void printstyle(integer c);
#define printstyle_regmem
void printskipparam(integer n);
#define printskipparam_regmem
void shownodelist(integer p);
#define shownodelist_regmem register memoryword *mem=zmem;
void showbox(halfword p);
#define showbox_regmem register memoryword *eqtb=zeqtb;
void deletetokenref(halfword p);
#define deletetokenref_regmem register memoryword *mem=zmem;
void deleteglueref(halfword p);
#define deleteglueref_regmem register memoryword *mem=zmem;
void flushnodelist(halfword p);
#define flushnodelist_regmem register memoryword *mem=zmem;
halfword copynodelist(halfword p);
#define copynodelist_regmem register memoryword *mem=zmem;
void printmode(integer m);
#define printmode_regmem
void pushnest(void);
#define pushnest_regmem
void popnest(void);
#define popnest_regmem register memoryword *mem=zmem;
void showactivities(void);
#define showactivities_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void printparam(integer n);
#define printparam_regmem
void begindiagnostic(void);
#define begindiagnostic_regmem register memoryword *eqtb=zeqtb;
void enddiagnostic(boolean blankline);
#define enddiagnostic_regmem
void printlengthparam(integer n);
#define printlengthparam_regmem
void printcmdchr(quarterword cmd,halfword chrcode);
#define printcmdchr_regmem
void showeqtb(halfword n);
#define showeqtb_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
halfword idlookup(integer j,integer l);
#define idlookup_regmem
void primitive(strnumber s,quarterword c,halfword o);
#define primitive_regmem register memoryword *eqtb=zeqtb;
void newsavelevel(groupcode c);
#define newsavelevel_regmem
void eqdestroy(memoryword w);
#define eqdestroy_regmem register memoryword *mem=zmem;
void eqsave(halfword p,quarterword l);
#define eqsave_regmem register memoryword *eqtb=zeqtb;
void eqdefine(halfword p,quarterword t,halfword e);
#define eqdefine_regmem register memoryword *eqtb=zeqtb;
void eqworddefine(halfword p,integer w);
#define eqworddefine_regmem register memoryword *eqtb=zeqtb;
void geqdefine(halfword p,quarterword t,halfword e);
#define geqdefine_regmem register memoryword *eqtb=zeqtb;
void geqworddefine(halfword p,integer w);
#define geqworddefine_regmem register memoryword *eqtb=zeqtb;
void saveforafter(halfword t);
#define saveforafter_regmem
void restoretrace(halfword p,strnumber s);
#define restoretrace_regmem
void unsave(void);
#define unsave_regmem register memoryword *eqtb=zeqtb;
void preparemag(void);
#define preparemag_regmem register memoryword *eqtb=zeqtb;
void tokenshow(halfword p);
#define tokenshow_regmem register memoryword *mem=zmem;
void printmeaning(void);
#define printmeaning_regmem
void showcurcmdchr(void);
#define showcurcmdchr_regmem
void showcontext(void);
#define showcontext_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void begintokenlist(halfword p,quarterword t);
#define begintokenlist_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void endtokenlist(void);
#define endtokenlist_regmem
void backinput(void);
#define backinput_regmem register memoryword *mem=zmem;
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
#define checkoutervalidity_regmem register memoryword *mem=zmem;
void getnext(void);
#define getnext_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void firmuptheline(void);
#define firmuptheline_regmem register memoryword *eqtb=zeqtb;
void gettoken(void);
#define gettoken_regmem
void macrocall(void);
#define macrocall_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void insertrelax(void);
#define insertrelax_regmem
void expand(void);
#define expand_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void getxtoken(void);
#define getxtoken_regmem
void xtoken(void);
#define xtoken_regmem
void scanleftbrace(void);
#define scanleftbrace_regmem
void scanoptionalequals(void);
#define scanoptionalequals_regmem
boolean scankeyword(strnumber s);
#define scankeyword_regmem register memoryword *mem=zmem;
void muerror(void);
#define muerror_regmem
void scaneightbitint(void);
#define scaneightbitint_regmem
void scancharnum(void);
#define scancharnum_regmem
void scanfourbitint(void);
#define scanfourbitint_regmem
void scanfifteenbitint(void);
#define scanfifteenbitint_regmem
void scantwentysevenbitint(void);
#define scantwentysevenbitint_regmem
void scanfontident(void);
#define scanfontident_regmem register memoryword *eqtb=zeqtb;
void findfontdimen(boolean writing);
#define findfontdimen_regmem
void scansomethinginternal(smallnumber level,boolean negative);
#define scansomethinginternal_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void scanint(void);
#define scanint_regmem
void scandimen(boolean mu,boolean inf,boolean shortcut);
#define scandimen_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void scanglue(smallnumber level);
#define scanglue_regmem register memoryword *mem=zmem;
halfword scanrulespec(void);
#define scanrulespec_regmem register memoryword *mem=zmem;
halfword strtoks(poolpointer b);
#define strtoks_regmem register memoryword *mem=zmem;
halfword thetoks(void);
#define thetoks_regmem register memoryword *mem=zmem;
void insthetoks(void);
#define insthetoks_regmem register memoryword *mem=zmem;
void convtoks(void);
#define convtoks_regmem register memoryword *mem=zmem;
halfword scantoks(boolean macrodef,boolean xpand);
#define scantoks_regmem register memoryword *mem=zmem;
void readtoks(integer n,halfword r);
#define readtoks_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void passtext(void);
#define passtext_regmem
void changeiflimit(smallnumber l,halfword p);
#define changeiflimit_regmem register memoryword *mem=zmem;
void conditional(void);
#define conditional_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void beginname(void);
#define beginname_regmem
boolean morename(ASCIIcode c);
#define morename_regmem
void endname(void);
#define endname_regmem
void packfilename(strnumber n,strnumber a,strnumber e);
#define packfilename_regmem
void packbufferedname(smallnumber n,integer a,integer b);
#define packbufferedname_regmem
strnumber makenamestring(void);
#define makenamestring_regmem
void scanfilename(void);
#define scanfilename_regmem
void packjobname(strnumber s);
#define packjobname_regmem
void promptfilename(strnumber s,strnumber e);
#define promptfilename_regmem
void openlogfile(void);
#define openlogfile_regmem register memoryword *eqtb=zeqtb;
void startinput(void);
#define startinput_regmem register memoryword *eqtb=zeqtb;
internalfontnumber readfontinfo(halfword u,strnumber nom,strnumber aire,scaled s);
#define readfontinfo_regmem register memoryword *eqtb=zeqtb;
void charwarning(internalfontnumber f,eightbits c);
#define charwarning_regmem register memoryword *eqtb=zeqtb;
halfword newcharacter(internalfontnumber f,eightbits c);
#define newcharacter_regmem register memoryword *mem=zmem;
void dviswap(void);
#define dviswap_regmem
void dvifour(integer x);
#define dvifour_regmem
void dvipop(integer l);
#define dvipop_regmem
void dvifontdef(internalfontnumber f);
#define dvifontdef_regmem
void movement(scaled w,eightbits o);
#define movement_regmem register memoryword *mem=zmem;
void prunemovements(integer l);
#define prunemovements_regmem register memoryword *mem=zmem;
void specialout(halfword p);
#define specialout_regmem register memoryword *mem=zmem;
void writeout(halfword p);
#define writeout_regmem register memoryword *mem=zmem;
void outwhat(halfword p);
#define outwhat_regmem register memoryword *mem=zmem;
void hlistout(void);
#define hlistout_regmem register memoryword *mem=zmem;
void vlistout(void);
#define vlistout_regmem register memoryword *mem=zmem;
void shipout(halfword p);
#define shipout_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void scanspec(groupcode c,boolean threecodes);
#define scanspec_regmem
halfword hpack(halfword p,scaled w,smallnumber m);
#define hpack_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
halfword vpackage(halfword p,scaled h,smallnumber m,scaled l);
#define vpackage_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void appendtovlist(halfword b);
#define appendtovlist_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
halfword newnoad(void);
#define newnoad_regmem register memoryword *mem=zmem;
halfword newstyle(smallnumber s);
#define newstyle_regmem register memoryword *mem=zmem;
halfword newchoice(void);
#define newchoice_regmem register memoryword *mem=zmem;
void showinfo(void);
#define showinfo_regmem register memoryword *mem=zmem;
halfword fractionrule(scaled t);
#define fractionrule_regmem register memoryword *mem=zmem;
halfword overbar(halfword b,scaled k,scaled t);
#define overbar_regmem register memoryword *mem=zmem;
halfword charbox(internalfontnumber f,quarterword c);
#define charbox_regmem register memoryword *mem=zmem;
void stackintobox(halfword b,internalfontnumber f,quarterword c);
#define stackintobox_regmem register memoryword *mem=zmem;
scaled heightplusdepth(internalfontnumber f,quarterword c);
#define heightplusdepth_regmem
halfword vardelimiter(halfword d,smallnumber s,scaled v);
#define vardelimiter_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
halfword rebox(halfword b,scaled w);
#define rebox_regmem register memoryword *mem=zmem;
halfword mathglue(halfword g,scaled m);
#define mathglue_regmem register memoryword *mem=zmem;
void mathkern(halfword p,scaled m);
#define mathkern_regmem register memoryword *mem=zmem;
void flushmath(void);
#define flushmath_regmem register memoryword *mem=zmem;
halfword cleanbox(halfword p,smallnumber s);
#define cleanbox_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void fetch(halfword a);
#define fetch_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void makeover(halfword q);
#define makeover_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void makeunder(halfword q);
#define makeunder_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void makevcenter(halfword q);
#define makevcenter_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void makeradical(halfword q);
#define makeradical_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void makemathaccent(halfword q);
#define makemathaccent_regmem register memoryword *mem=zmem;
void makefraction(halfword q);
#define makefraction_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
scaled makeop(halfword q);
#define makeop_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void makeord(halfword q);
#define makeord_regmem register memoryword *mem=zmem;
void makescripts(halfword q,scaled delta);
#define makescripts_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
smallnumber makeleftright(halfword q,smallnumber style,scaled maxd,scaled maxh);
#define makeleftright_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void mlisttohlist(void);
#define mlisttohlist_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void pushalignment(void);
#define pushalignment_regmem register memoryword *mem=zmem;
void popalignment(void);
#define popalignment_regmem register memoryword *mem=zmem;
void getpreambletoken(void);
#define getpreambletoken_regmem register memoryword *eqtb=zeqtb;
void initalign(void);
#define initalign_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void initspan(halfword p);
#define initspan_regmem
void initrow(void);
#define initrow_regmem register memoryword *mem=zmem;
void initcol(void);
#define initcol_regmem register memoryword *mem=zmem;
boolean fincol(void);
#define fincol_regmem register memoryword *mem=zmem;
void finrow(void);
#define finrow_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void finalign(void);
#define finalign_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void alignpeek(void);
#define alignpeek_regmem
halfword finiteshrink(halfword p);
#define finiteshrink_regmem register memoryword *mem=zmem;
void trybreak(integer pi,smallnumber breaktype);
#define trybreak_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void postlinebreak(integer finalwidowpenalty);
#define postlinebreak_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
smallnumber reconstitute(smallnumber j,smallnumber n,halfword bchar,halfword hchar);
#define reconstitute_regmem register memoryword *mem=zmem;
void hyphenate(void);
#define hyphenate_regmem register memoryword *mem=zmem;
quarterword newtrieop(smallnumber d,smallnumber n,quarterword v);
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
#define newpatterns_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void inittrie(void);
#define inittrie_regmem
void linebreak(integer finalwidowpenalty);
#define linebreak_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void newhyphexceptions(void);
#define newhyphexceptions_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
halfword prunepagetop(halfword p);
#define prunepagetop_regmem register memoryword *mem=zmem;
halfword vertbreak(halfword p,scaled h,scaled d);
#define vertbreak_regmem register memoryword *mem=zmem;
halfword vsplit(eightbits n,scaled h);
#define vsplit_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void printtotals(void);
#define printtotals_regmem
void freezepagespecs(smallnumber s);
#define freezepagespecs_regmem register memoryword *eqtb=zeqtb;
void boxerror(eightbits n);
#define boxerror_regmem register memoryword *eqtb=zeqtb;
void ensurevbox(eightbits n);
#define ensurevbox_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void fireup(halfword c);
#define fireup_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void buildpage(void);
#define buildpage_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void appspace(void);
#define appspace_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void insertdollarsign(void);
#define insertdollarsign_regmem
void youcant(void);
#define youcant_regmem
void reportillegalcase(void);
#define reportillegalcase_regmem
boolean privileged(void);
#define privileged_regmem
boolean itsallover(void);
#define itsallover_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void appendglue(void);
#define appendglue_regmem register memoryword *mem=zmem;
void appendkern(void);
#define appendkern_regmem register memoryword *mem=zmem;
void offsave(void);
#define offsave_regmem register memoryword *mem=zmem;
void extrarightbrace(void);
#define extrarightbrace_regmem
void normalparagraph(void);
#define normalparagraph_regmem register memoryword *eqtb=zeqtb;
void boxend(integer boxcontext);
#define boxend_regmem register memoryword *mem=zmem;
void beginbox(integer boxcontext);
#define beginbox_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void scanbox(integer boxcontext);
#define scanbox_regmem
void package(smallnumber c);
#define package_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
smallnumber normmin(integer h);
#define normmin_regmem
void newgraf(boolean indented);
#define newgraf_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void indentinhmode(void);
#define indentinhmode_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void headforvmode(void);
#define headforvmode_regmem
void endgraf(void);
#define endgraf_regmem register memoryword *eqtb=zeqtb;
void begininsertoradjust(void);
#define begininsertoradjust_regmem
void makemark(void);
#define makemark_regmem register memoryword *mem=zmem;
void appendpenalty(void);
#define appendpenalty_regmem register memoryword *mem=zmem;
void deletelast(void);
#define deletelast_regmem register memoryword *mem=zmem;
void unpackage(void);
#define unpackage_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void appenditaliccorrection(void);
#define appenditaliccorrection_regmem register memoryword *mem=zmem;
void appenddiscretionary(void);
#define appenddiscretionary_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void builddiscretionary(void);
#define builddiscretionary_regmem register memoryword *mem=zmem;
void makeaccent(void);
#define makeaccent_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
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
void initmath(void);
#define initmath_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void starteqno(void);
#define starteqno_regmem register memoryword *eqtb=zeqtb;
void scanmath(halfword p);
#define scanmath_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void setmathchar(integer c);
#define setmathchar_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void mathlimitswitch(void);
#define mathlimitswitch_regmem register memoryword *mem=zmem;
void scandelimiter(halfword p,boolean r);
#define scandelimiter_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void mathradical(void);
#define mathradical_regmem register memoryword *mem=zmem;
void mathac(void);
#define mathac_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void appendchoices(void);
#define appendchoices_regmem register memoryword *mem=zmem;
halfword finmlist(halfword p);
#define finmlist_regmem register memoryword *mem=zmem;
void buildchoices(void);
#define buildchoices_regmem register memoryword *mem=zmem;
void subsup(void);
#define subsup_regmem register memoryword *mem=zmem;
void mathfraction(void);
#define mathfraction_regmem register memoryword *mem=zmem;
void mathleftright(void);
#define mathleftright_regmem register memoryword *mem=zmem;
void aftermath(void);
#define aftermath_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void resumeafterdisplay(void);
#define resumeafterdisplay_regmem
void getrtoken(void);
#define getrtoken_regmem
void trapzeroglue(void);
#define trapzeroglue_regmem register memoryword *mem=zmem;
void doregistercommand(smallnumber a);
#define doregistercommand_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void alteraux(void);
#define alteraux_regmem
void alterprevgraf(void);
#define alterprevgraf_regmem
void alterpagesofar(void);
#define alterpagesofar_regmem
void alterinteger(void);
#define alterinteger_regmem
void alterboxdimen(void);
#define alterboxdimen_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void newfont(smallnumber a);
#define newfont_regmem register memoryword *eqtb=zeqtb;
void newinteraction(void);
#define newinteraction_regmem
void prefixedcommand(void);
#define prefixedcommand_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void doassignments(void);
#define doassignments_regmem
void openorclosein(void);
#define openorclosein_regmem
void issuemessage(void);
#define issuemessage_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void shiftcase(void);
#define shiftcase_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void showwhatever(void);
#define showwhatever_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void storefmtfile(void);
#define storefmtfile_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void newwhatsit(smallnumber s,smallnumber w);
#define newwhatsit_regmem register memoryword *mem=zmem;
void newwritewhatsit(smallnumber w);
#define newwritewhatsit_regmem register memoryword *mem=zmem;
void doextension(void);
#define doextension_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void fixlanguage(void);
#define fixlanguage_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void handlerightbrace(void);
#define handlerightbrace_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void maincontrol(void);
#define maincontrol_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void giveerrhelp(void);
#define giveerrhelp_regmem register memoryword *eqtb=zeqtb;
boolean openfmtfile(void);
#define openfmtfile_regmem
boolean loadfmtfile(void);
#define loadfmtfile_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void closefilesandterminate(void);
#define closefilesandterminate_regmem register memoryword *eqtb=zeqtb;
void finalcleanup(void);
#define finalcleanup_regmem register memoryword *mem=zmem;
void initprim(void);
#define initprim_regmem register memoryword *eqtb=zeqtb;
void debughelp(void);
#define debughelp_regmem register memoryword *mem=zmem, *eqtb=zeqtb;
void texbody(void);
#define texbody_regmem register memoryword *eqtb=zeqtb;
/*
 * The C compiler ignores most unnecessary casts (i.e., casts of something
 * to its own type).  However, for structures, it doesn't.  Therefore,
 * we have to redefine these two macros so that they don't try to cast
 * the argument (a memoryword) as a memoryword.
 */
#undef	eqdestroy
#define	eqdestroy(x)	zeqdestroy(x)
#undef	printword
#define	printword(x)	zprintword(x)
