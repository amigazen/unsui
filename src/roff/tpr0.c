/*
**	tpr - text formatter
**		Ken Yap, June 1981
*/
#include	"tpr.h"

struct macro macros[MAXMAC];
short maccnt	= 0;		/* counter for current macro */
char *pbptr[MAXMAC];		/* pointers to pushed back lines */
short pblev	= 0;		/* indicates level of macro nesting during collection */

char outbuf[MAXOUT];	/* lines to be filled collect here */
char *outp	= outbuf;	/* last char position in outbuf; init = 0 */
short outw	= 0;	/* width of text currenty in outbuf; init = 0 */
short outwds	= 0;	/* number of words in outbuf; init = 0 */

short pages	= 1;	/* pages between pauses with -s option */
short pausecount= 0;	/* decremented every page */
short curpag	= 0;
short newpag	= 1;
short lineno	= 0;
short peekno	= 1;
short indline	= 0;
short respage	= 0;
char trapmac[2]	= "";
char blnkhdr[]	= "\n";
struct envir env	= {
	PAGLEN, M1DEF, M2DEF, M3DEF, M4DEF, (PAGLEN-M3DEF-M4DEF),
	blnkhdr, blnkhdr,
	blnkhdr, blnkhdr,
	'.', '\t', ' ',
	YES, YES, ARABIC, 1, PAGEWIDTH, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	{ 8,16,24,32,40,48,56,64,72,80,88,96,104,112,120,0 },
	NULL
	};
struct envir *curenv	= NULL;
struct cmdents builtins[] ={
	{ "ad",YES },
	{ "ar",YES },
	{ "bd",YES },
	{ "bl",YES },
	{ "bp",YES },
	{ "br",YES },
	{ "cc",YES },
	{ "ce",YES },
	{ "de",YES },
	{ "ef",YES },
	{ "eg",YES },
	{ "eh",YES },
	{ "fi",YES },
	{ "fo",YES },
	{ "he",YES },
	{ "in",YES },
	{ "ix",YES },
	{ "li",YES },
	{ "ll",YES },
	{ "ls",YES },
	{ "m1",YES },
	{ "m2",YES },
	{ "m3",YES },
	{ "m4",YES },
	{ "na",YES },
	{ "ne",YES },
	{ "nf",YES },
	{ "of",YES },
	{ "oh",YES },
	{ "pl",YES },
	{ "po",YES },
	{ "pt",YES },
	{ "re",YES },
	{ "rf",YES },
	{ "ro",YES },
	{ "rp",YES },
	{ "se",YES },
	{ "sk",YES },
	{ "so",YES },
	{ "sp",YES },
	{ "ta",YES },
	{ "tc",YES },
	{ "ti",YES },
	{ "ub",YES },
	{ "ul",YES }
	};

short echodir	= 0;
char *progname;
char *filename	= "stdin";
short fileline	= 0;
short ttyfd	= -1;	/* for pause between pages */
FILE *indfp	= NULL;
char *nomem	= "Out of dynamic memory\n";	/* canned message */

/*
**	main
*/
main(argc,argv)
	short argc;
	char **argv;{
	FILE *file;
#ifdef unix
	extern char _sobuf[];
#endif
	
	progname = *argv;
	for(argc--, argv++; argc > 0 && **argv == '-' && (*argv)[1] != '\0'; argc--, argv++)
		setoptions(*argv);
#ifdef unix
	setbuf(stdout,_sobuf);
#endif
	if(argc == 0)
		tpr(stdin,"stdin");
	else
		for( ; argc > 0; argc--, argv++){
			if(**argv == '-' && (*argv)[1] == '\0')
				tpr(stdin,"stdin");
			else{
				if((file = fopen(*argv,"r")) == NULL){
					perror(*argv);
					continue;
					}
				tpr(file,*argv);
				fclose(file);
				}
			}
	if(lineno > 0)
		spc(HUGE);		/* flush last output */
	if(indfp != NULL)
		fclose(indfp);
	fflush(stdout);
}

setoptions(arg)
	char *arg;{
	register char c;
	int isatty(),open();
	FILE *fopen();

	while((c = *++arg) != '\0')
		switch(c){
			case 'd':	/* echo directives */
				echodir++;
				break;
			case 'e':	/* divert errors */
				close(2);
				if(creat(++arg,0600) < 0){
					open("/dev/tty",1);
					perror(arg);
					exit(1);
					}
				return;
			case 'i':	/* open index file */
				if((indfp = fopen(arg[1] == '\0' ? "index" : ++arg, "w")) == NULL)
					perror(arg);
				return;
			case 'n':	/* set initial page number */
				curpag = atoi(++arg);
				if(curpag < 0)
					curpag = 0;
				newpag = curpag;
				return;
			case 's':	/* pause every n pages */
				pages = atoi(++arg);
				if(pages <= 0)
					pages = 1;
				pausecount = pages;
#ifdef unix
				if(isatty(fileno(stdout)))
					ttyfd = open("/dev/tty",0);
#else
				ttyfd = fileno (stdout);
#endif
				return;
			default:
				fprintf(stderr,"Usage: %s [-d] [-efile] [-iindex] [-nN] [-sN] files\n",progname);
				exit(1);
			}
}

/*
**	tpr - here is the main routine for each file
**	the name is passed along so that the 'include' directive
**	can call 'tpr' recursively
*/
tpr(file,name)
	FILE *file;
	char *name;{
	register char *savename;
	register short saveline;
	char inbuf[MAXIN];
	char *ngetl();

	savename = filename;	/* save old name */
	filename = name;	/* make new one available for error routine */
	saveline = fileline;	/* and line number */
	fileline = 0;
	while(ngetl(inbuf,file) != CHARNULL){
		++fileline;
		if(*inbuf == env.comchr)	/* it's a command */
			cmd(inbuf,file);
		else			/* it's text */
			text(inbuf);
		}
	filename = savename;	/* restore name and linenumber */
	fileline = saveline;
}

/*
**	error - prints filename and linenumber
*/
error(msg,arg)
	char *msg,*arg;{

	fprintf(stderr,"At line %d in file %s: ",fileline,filename);
	fprintf(stderr,msg,arg);
}

/*
**	bold - bolden a line
*/
bold(buf,tbuf,tend)
	char *buf,*tbuf,*tend;{
	register char c;
	register char *p,*q;
	char *strcpy();
	
	p = buf;
	q = tbuf;
	while(*p != '\n' && q < tend){
		if(isprint(c = *p++)){
			*q++ = c;
			*q++ = '\b';
			}
		*q++ = c;
		}
	*q++ = '\n';
	*q = '\0';
	strcpy(buf,tbuf);	/* copy it back to buf */
}

/*
**	blnk - space n lines (to new page if necessary, cf spc)
*/
blnk(n)
	short n;{
	register short i;
	short nextline();
	
	linebreak();
	while(n > 0){
		if(lineno > env.bottom){
			pfoot();
			lineno = 0;
			peekno = nextline(lineno);
			}
		if(lineno == 0)
			newpage();
		i = min(n,env.bottom + 1 - lineno);
		skip(i);
		n -= i;
		lineno += i;
		peekno = nextline(lineno);
		}
	if(lineno > env.bottom)
		pfoot();
}

/*
**	linebreak - end current filled line
*/
linebreak(){
	
	if(outp != outbuf){
		*outp++ = '\n';
		*outp = '\0';
		put(outbuf);
		}
	outp = outbuf;
	outw = 0;
	outwds = 0;
}

/*
**	centre - centre a line by setting tival
*/
center(buf)
	char *buf;{
	short width();
	
	env.tival = max((env.llval + env.tival - width(buf))/2,0);
}

/*
**	cmd - perform formatting command
*/
cmd(buf,file)
	char *buf;
	FILE *file;{
	CMDNUM comtyp();
	CMDNUM ct;
	register short spval,val;
	register char *p;
	short macnum;
	char argtyp;
	char fn[MAXFN];
	FILE *fp;
	int strlen();
	short getval(),set();
	char *gettl();
	
	if(echodir)
		putdir(buf);
	ct = comtyp(buf,&macnum);
	val = 0;
	val = getval(buf,&argtyp);
	switch(ct){
		case SPA:
			spval = set(0,val,argtyp,1,0,HUGE);
			spc(spval);
			break;
		case IND:
		case INX:
			if(ct == IND)
				linebreak();
			env.inval = set(env.inval,val,argtyp,0,0,env.llval - 1);
			env.tival = env.inval;
			break;
		case TMI:
			linebreak();
			env.tival = set(env.tival,val,argtyp,0,0,env.llval);
			break;
		case CEN:
			linebreak();
			env.ceval = set(env.ceval,val,argtyp,1,0,HUGE);
			break;
		case UDL:
			env.ulval = set(env.ulval,val,argtyp,0,1,HUGE);
			break;
		case BLD:
			env.bdval = set(env.bdval,val,argtyp,0,1,HUGE);
			break;
		case FIL:
			linebreak();
			env.fill = YES;
			break;
		case NFL:
			linebreak();
			env.fill = NO;
			break;
		case BRE:
			linebreak();
			break;
		case BLN:
			env.blval = set(env.blval,val,argtyp,1,0,HUGE);
			blnk(env.blval);
			env.blval = 0;
			break;
		case NED:
			if(val > env.bottom - peekno + 1)
				spc(HUGE);
			break;
		case LNS:
			env.lsval = set(env.lsval,val,argtyp,1,1,HUGE);
			break;
		case LNL:
			env.llval = set(env.llval,val,argtyp,PAGEWIDTH,env.tival + 1,HUGE);
			break;
		case PGL:
			env.plval = set(env.plval,val,argtyp,PAGLEN,
				env.m1val + env.m2val + env.m3val + env.m4val + 1,HUGE);
			env.bottom = env.plval - env.m3val - env.m4val;
			break;
		case BPG:
			if(lineno > 0)
				spc(HUGE);
			curpag = set(curpag,val,argtyp,curpag + 1, - HUGE,HUGE);
			newpag = curpag;
			break;
		case HED:
			env.evenhdr = env.oddhdr = gettl(buf);
			break;
		case FOT:
			env.evenftr = env.oddftr = gettl(buf);
			break;
		case EHD:
			env.evenhdr = gettl(buf);
			break;
		case EFO:
			env.evenftr = gettl(buf);
			break;
		case OHD:
			env.oddhdr = gettl(buf);
			break;
		case OFO:
			env.oddftr = gettl(buf);
			break;
		case NAD:
			env.adjust = NO;
			break;
		case ADJ:
			env.adjust = YES;
			break;
		case ROM:
			env.numtyp = ROMAN;
			break;
		case ARA:
			env.numtyp = ARABIC;
			break;
		case ENG:
			env.numtyp = ENGLISH;
			break;
		case LIT:
			env.litval = set(env.litval,val,argtyp,1,0,HUGE);
			break;
		case M1:
			env.m1val = set(env.m1val,val,argtyp,M1DEF,
				0,env.plval - (env.m2val + env.m3val + env.m4val + 1));
			break;
		case M2:
			env.m2val = set(env.m2val,val,argtyp,M2DEF,
				0,env.plval - (env.m1val + env.m3val + env.m4val + 1));
			break;
		case M3:
			env.m3val = set(env.m3val,val,argtyp,M3DEF,
				0,env.plval - (env.m1val + env.m2val + env.m4val + 1));
			env.bottom = env.plval - env.m3val - env.m4val;
			break;
		case M4:
			env.m4val = set(env.m4val,val,argtyp,M4DEF,
				0,env.plval - (env.m1val + env.m2val + env.m3val + 1));
			env.bottom = env.plval - env.m3val - env.m4val;
			break;
		case CMC:
			if(argtyp != '\n')
				env.comchr = argtyp;
			else
				env.comchr = COMMAND;
			break;
		case POF:
			env.poval = set(env.poval,val,argtyp,0,0,PAPERSIZE);
			break;
		case SKP:
			env.skpval = set(env.skpval,val,argtyp,1,0,HUGE);
			break;
		case DFN:
			getmac(buf,file);
			break;
		case TCH:
			if(argtyp != '\n')
				env.tabchr = argtyp;
			else
				env.tabchr = '\t';
			break;
		case TCL:
			tabcol(buf);
			break;
		case UBC:
			if(argtyp != '\n')
				env.ubchr = argtyp;
			else
				env.ubchr = ' ';
			break;
		case REF:
			if(indfp != NULL)
				indline = set(indline,val,argtyp,1,0,HUGE);
			break;
		case RPG:
			respage += set(0,val,argtyp,1,0,HUGE);
			break;
		case PGT:
			settrap(buf);
			break;
		case RNV:
			if(val <= 0)
				val = 1;
			resenv(val);
			break;
		case SNV:
			savenv();
			break;
		case MAC:
			expand(macnum);
			break;
		case SOU:
			skipnbl(buf);
			skipbl(buf);
			for(p = fn; p < &fn[MAXFN - 1] && *buf != '\n'; )
				*p++ = *buf++;
			*p = '\0';
			if((fp = fopen(fn,"r")) == NULL)
				error("Cannot open %s\n",fn);
			else{
				tpr(fp,fn);
				fclose(fp);
				}
			break;
		case UNKNOWN:
			error("Unrecognised directive:\n%s",buf);
			return;
		}
}

/*
**	comtyp - decode command type
*/
CMDNUM comtyp(buf,macnum)
	char *buf;
	short *macnum;{
	register char a,b;
	register struct cmdents *p;
	register char *q;
	register short l,h,m;
	
	a = buf[1];
	b = buf[2];
	for(l = 0, h = (sizeof(builtins)/sizeof(builtins[0])) - 1; l <= h; ){
		p = &builtins[m = (l + h) / 2];
		if(a < p->cmdname[0] || a <= p->cmdname[0] && b < p->cmdname[1])
			h = m - 1;
		else if(a > p->cmdname[0] || a <= p->cmdname[0] && b > p->cmdname[1])
			l = m + 1;
		else
			break;
		}
	if(l <= h && p->notredefd)
		return((CMDNUM)m);
	for(l = maccnt - 1; l >= 0; l--){
		q = macros[l].macnam;
		if(a == *q++ && b == *q){
			*macnum = l;
			return(MAC);
			}
		}
	return(UNKNOWN);
}

#ifdef AMIGA
perror (arg)
char *arg;
{
    if (arg && *arg) {
	fprintf (stderr, "%s: ", arg);
    }
    fprintf (stderr, "<unknown error>\n");
}
#endif

