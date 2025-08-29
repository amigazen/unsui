#include	"tpr.h"

/*
**	expand - pushback macro definition onto input
*/
expand(macnum)
	short macnum;{
	register struct linelink *lp;
	
	for(lp = macros[macnum].macend; lp != NULL; lp = lp->lastline)
		pushback(lp->lineptr);
}

/*
**	getmac - collect macro
*/
getmac(buf,file)
	char *buf;
	FILE *file;{
	register char *p;
	register struct macro *mp;
	register struct linelink *lp;
	register short dotlev;
	CMDNUM ct;
	short macnum;
	char line[MAXIN];
	CMDNUM comtyp();
	char *strcpy();
	char *malloc();
	char *ngetl();
	
	skipnbl(buf);
	skipbl(buf);
	if(*buf == '\n'){
		error("Missing macro name\n",CHARNULL);
		return;
		}
	if(maccnt >= MAXMAC){
		error("Too many macro definitions",CHARNULL);
		return;
		}
	mp = &macros[maccnt];
	p = mp->macnam;
	if((ct = comtyp(buf - 1,&macnum)) != UNKNOWN && ct != MAC)
		builtins[(int)ct].notredefd = NO;
	*p++ = *buf++;	/* record name */
	*p++ = *buf++;
	*p = '\0';
	for(lp = mp->macend; lp != NULL; lp = lp->lastline, free(lp))
		free(lp->lineptr);
	mp->macend = NULL;
	dotlev = 1;
	do{
		if((p = ngetl(line,file)) == CHARNULL)
			break;	/* unexpected EOF */
		++fileline;
		if(*p++ == env.comchr){
			if(*p == '.')
				dotlev--;
			else if(comtyp(line,&macnum) == DFN)
				dotlev++;	/* included .de */
			}
		if(dotlev > 0){
			if((lp = (struct linelink *)malloc(sizeof(struct linelink))) == NULL){
				error(nomem,CHARNULL);
				break;
				}
			lp->lastline = mp->macend;
			mp->macend = lp;
			if((lp->lineptr = malloc((unsigned)(strlen(line) + 1))) == NULL){
				error(nomem,CHARNULL);
				break;
				}
			strcpy(lp->lineptr,line);
			}
		}while(dotlev > 0);
	maccnt++;
}

/*
**	getseg - puts out part of header
*/
char *getseg(buf,copy,copyend,term,pageno)
	char *buf,*copy,*copyend,term;
	short pageno;{
	register short i;
	register char *p;
	short itorom(),itoeng();
	
	p = buf;
	for( ;copy != copyend && *p != term && *p != '\0' && *p != '\n'; p++){
		if(*p == PAGENUM){
			switch(env.numtyp){
			    case ARABIC:
				sprintf(copy, "%d", pageno);
				i = strlen(copy);
				break;
			    case ROMAN:
				i = itorom(pageno,copy,min(MAXCHARS,(short)(copyend - copy)));
				break;
			    case ENGLISH:
				i = itoeng(pageno,copy,copyend);
				break;
				}
			copy += i;
			}
		else
			*copy++ = *p;
		}
	if(*p == term)
		p++;
	*copy = '\0';
	return(p);
}

/*
**	gettl - copy title from buf to ttl
*/
char *gettl(buf)
	char *buf;{
	register char *p,*q;
	char *strcpy();
	char *malloc();
	
	p = buf;
	skipnbl(p);
	skipbl(p);	/* find argument */
	if((q = malloc((unsigned)(strlen(p) + 1))) == NULL){
		error(nomem,CHARNULL);
		return(q);
		}
	strcpy(q,p);
	return(q);
}

/*
**	getval - evaluate optional numeric argument
*/
short getval(buf,argtyp)
	char *buf;
	char *argtyp;{
	int atoi();
	
	skipnbl(buf);
	skipbl(buf);	/* find argument */
	*argtyp = *buf;
	if(!isdigit(*buf))
		buf++;
	return(atoi(buf));
}

/*
**	getwrd - get a non - blank word from instr(i) to out, increment i
*/
char *getwrd(instr,out)
	char *instr,*out;{
	register char c;
	register char *p,*q;
	
	p = instr;
	q = out;
	while((*p == ' ' || *p == '\t') && *p != env.tabchr)
		p++;
	instr = p;
	if(*p == env.tabchr)
		*q++ = *p;
	else{
		while((c = *p) != '\0' && c != ' ' && c != '\t'
			&& c != '\n' && c != env.tabchr){
			*q++ = *p++;
			}
		}
	*q = '\0';
	return(p == instr ? NULL : p);
}

/*
**	itorom - converts integer to roman numerals
*/
short itorom(num,str,flen)
	char *str;
	short num,flen;{
	register short i,j;
	char *p;
	static short romval[]	= { 1000,500,100,50,10,5,1,0 };
	static short reltab[]	= { 2,1,2,1,2,1,1,0 };
	static char romlet[]	= "mdclxvi0";
	
	p = str;
	if(num < 0 && flen > 1){
		num = -num;
		*p++ = '-';
		}
	for(i = 0; num > 0; i++){
		while(num >= romval[i]){
			num -= romval[i];
			*p++ = romlet[i];
			}
		j = i + reltab[i];
		if(num >= (romval[i] - romval[j])){
			num -= (romval[i] - romval[j]);
			*p++ = romlet[j];
			*p++ = romlet[i];
			}
		}
	*p = '\0';
	return((short)(p - str));
}

/*
**	leadbl - delete leading blanks, set tival
*/
leadbl(buf)
	char *buf;{
	register char *p;
	
	linebreak();
	p = buf;
	skipnbl(buf);
	if(*buf != '\n')
		env.tival = (buf - p);
	while(*buf != '\0')		/* move line to left */
		*p++ = *buf++;
	*p = '\0';
}

/*
**	newpage - put out as many blank pages as necessary to satisfy
**	reserved page count 'respage'
*/
newpage(){

	phead();
	for( ; respage > 0; respage--){
		skip(env.bottom + 1 - lineno);
		pfoot();
		phead();
		}
}

/*
**	nextline - predicts the next line after n
*/
short nextline(n)
	register short n;{
	register short i;

	i = n + 1;
	if(i > env.bottom)
		i = env.m1val + env.m2val + 1;
	return(i);
}

/*
**	nextab - returns position of next tab stop
*/
short nextab(pos)
	short pos;{
	register short i,k;
	
	for(i =  0; i < MAXTABS; i++){
	       if(env.tabpos[i] == 0)
			break;
	       if(env.tabpos[i] > pos){
	               k = env.tabpos[i];
	               return(k);
	               }
	       }
	k = pos + 1;
	return(k);
}

/*
**	ngetl - gets line from input or pushback buffer
*/
char *ngetl(buf,file)
	char *buf;
	FILE *file;{
	char *fgets();
	char *strcpy();
	
	if(pblev <= 0)
		return(fgets(buf,MAXIN,file));
	else{
		pblev--;
		strcpy(buf,pbptr[pblev]);
		}
	return(buf);
}

/*
**	pfoot - put out page footer
*/
pfoot(){
	
	skip(env.m3val);
	if(env.m4val > 0){
		puttl(curpag % 2 ? env.oddftr : env.evenftr, curpag);
		skip(env.m4val - 1);
		}
}

/*
**	phead - put out page header
*/
phead(){
	short nextline();
	
	curpag = newpag;
	newpag++;
	if(ttyfd >= 0 && pausecount-- <= 0){
		fflush(stdout);
		waitfornl();
		pausecount = pages - 1;
		}
	if(trapmac[0] != '\0')
		springtrap();
	if(env.m1val > 0){
		skip(env.m1val - 1);
		puttl(curpag % 2 ? env.oddhdr : env.evenhdr, curpag);
		}
	skip(env.m2val);
	lineno = env.m1val + env.m2val + 1;
	peekno = nextline(lineno);
}

/*
**	pushback - pushback line onto input
*/
pushback(buf)
	char *buf;{

	if(pblev > MAXPB){
		error("Not enough pushback space\n",CHARNULL);
		return;	/* to catch stack overflow */
		}
	pbptr[pblev] = buf;
	pblev++;
}

/*
**	put - put out line with proper spacing and indenting
*/
put(buf)
	char *buf;{
	short nextab();
	register char c;
	register short col,i;
	short nextline();
	
	if(lineno == 0 || lineno > env.bottom)
		newpage();
	for(i = (env.tival + env.poval); i--; )
		putchar(' ');
	col = env.tival;
	env.tival = env.inval;
	for(; (c = *buf) != '\0'; buf++){
		if(c == env.ubchr)
			c = ' ';	/* put blanks instead of blank replacement */
		if(c == env.tabchr){
			i = nextab(col);	/* nextab wants last used column */
			for(; col < i; col++)
				putchar(' ');
			continue;
			}
		else if(c == '\b')
			col--;
		else
			col++;
		putchar(c);
		}
	skip(min(env.lsval - 1,env.bottom - lineno));
	lineno += env.lsval;
	peekno = nextline(lineno);
	if(lineno > env.bottom){
		pfoot();
		if(env.skpval > 0){
			skpage(env.skpval);
			env.skpval = 0;
			}
		}
}

/*
**	putdir - output a directive
*/
putdir(buf)
	char *buf;{

	fprintf(stderr,"%.10s",buf);	/* first 10 chars */
}

/*
**	puttl - put out title line with optional page number
*/
puttl(buf,pageno)
	char *buf;
	short pageno;{
	register char *p,*q;
	register short col,newcol;
	char copy[MAXOUT],term;
	short width();
	char *getseg();
	
	for(col = env.poval; col-- > 0; )
		tputc(' ');
	term = *buf;
	if(term == '\n'){
		tputc('\n');
		return;
		}
	col = 0;
	p = buf + 1;
	p = getseg(p,copy,&copy[MAXOUT],term,pageno);
	col += width(copy);
	for(q = copy; *q != '\0'; q++)
		tputc(*q);
	p = getseg(p,copy,&copy[MAXOUT],term,pageno);
	newcol = (env.llval - strlen(copy))/2;	/* start of centre */
	for(; col < newcol; col++)
		tputc(' ');
	col += width(copy);
	for(q = copy; *q != '\0'; q++)
		tputc(*q);
	p = getseg(p,copy,&copy[MAXOUT],term,pageno);
	newcol = env.llval - strlen(copy);	/* start of right */
	for(; col < newcol; col++)
		tputc(' ');
	for(q = copy; *q != '\0'; q++)
		tputc(*q);
	tputc('\n');
}

/*
**	putwrd - put a word in outbuf; includes margin justification
*/
putwrd(wrdbuf)
	char *wrdbuf;{
	char *strcpy();
	register short l,w,lnval,nextra;
	short width(), nextab();
	int strlen();
	
	lnval = env.llval - env.tival;
	if(*wrdbuf == env.tabchr){
		outw = nextab(outw + env.tival) - env.tival;
		/* because outw floats from the indent */
		/* and nextab is absolute */
		if(outp  != outbuf && outw > lnval){
			linebreak();
			outw = nextab(outw + env.tival) - env.tival;
			}
		*++outp = env.tabchr;
		outwds = 0;		/* adjust from next word */
		}
	else{
		w = width(wrdbuf);
		l = strlen(wrdbuf);
		if(outp != outbuf && (outw + w > lnval ||	/* too big */
			(char *)(outp + l) >= &outbuf[MAXOUT])){
			--outp;	/* we put in a blank earlier */
			if(env.adjust == YES){
				nextra = lnval - outw + 1;
				spread(outp,nextra,outwds);
				outp += nextra;
				}
			linebreak();		/* flush previous line */
			}
		strcpy(outp,wrdbuf);
		outp += l;
		*outp++ = ' ';		/* blank between words */
		outw += w + 1;			/* 1 for blank */
		outwds++;
		}
}

/*
**	resenv - restore environment n levels back
*/
resenv(n)
	short n;{
	register struct envir *ep,*tp;
	
	linebreak();		/* to flush any latent output */
	for(ep = curenv; ep != NULL && --n > 0; free(tp))
		ep = (tp = ep)->lastenv;
	if(ep != NULL){
		env = *ep;
		curenv = ep->lastenv;
		free(ep);
		}
}

/*
**	savenv - keep environment for later restoration
*/
savenv(){
	register struct envir *ep;
	char *malloc();
	
	if((ep = (struct envir *)malloc(sizeof(env))) == NULL){
		error(nomem,CHARNULL);
		return;
		}
	*ep = env;	/* structure copy */
	ep->lastenv = curenv;
	curenv = ep;
}

/*
**	set - set parameter and check range
*/
short set(param,val,argtyp,defval,minval,maxval)
	short param,val,defval,minval,maxval;
	char argtyp;{
	
	switch(argtyp){
		case '\n':
			param = defval;
			break;
		case '+':
			param = param + val;
			break;
		case '-':
			param = param - val;
			break;
		default:
			param = val;
		}
	param = min(param,maxval);
	param = max(param,minval);
	return(param);
}

/*
**	settrap - record macro name to be invoked on page crossing
*/
settrap(buf)
	char *buf;{
	register char *p;

	p = buf;
	skipnbl(p);
	skipbl(p);
	if(*p == '\n'){
		trapmac[0] = '\0';
		return;
		}
	trapmac[0] = *p++;
	trapmac[1] = *p;
}

/*
**	skip - output n blank lines
*/
skip(n)
	short n;{
	
	while(n-- > 0)
		putchar('\n');
}

/*
**	skpage - skip n pages
*/
skpage(n)
	short n;{
	short nextline();
	
	while(n-- > 0){
		newpage();
		skip(env.bottom + 1 - lineno);
		lineno = env.bottom + 1;
		peekno = nextline(lineno);
		pfoot();
		}
}

/*
**	spc - space n lines or to bottom of page (cf blnk)
*/
spc(n)
	short n;{
	short nextline();
	
	linebreak();
	if(lineno > env.bottom)
		return;
	if(lineno == 0)
		newpage();
	skip(min(n,env.bottom + 1 - lineno));
	lineno += n;
	peekno = nextline(lineno);
	if(lineno > env.bottom){
		pfoot();
		if(env.skpval > 0){
			skpage(env.skpval);
			env.skpval = 0;
			}
		}
}

/*
**	spread - spread words to justify right margin
*/
spread(ptr,nextra,nwrds)
	char *ptr;
	short nextra,nwrds;{
	register char *p,*q;
	register short nb,nholes;
	static short dir	= 0;
	
	if(nextra <= 0 || nwrds <= 1)
		return;
	dir = !dir;		/* reverse previous direction */
	nholes = nwrds - 1;
	p = ptr - 1;
	q = ptr + nextra;
	*q-- = '\0';
	while(p < q){
		if((*q = *p) == ' '){
			nb = dir ? (nextra - 1) / nholes + 1 :
				nextra / nholes;
			nextra -= nb;
			nholes--;
			while(nb-- > 0){
				*--q = ' ';
				}
			}
		p--;
		q--;
		}
}

/*
**	springtrap - invoke macro on page crossing
*/
springtrap(){
	static char mac[5]	= ".xx\n";

	mac[1] = trapmac[0];
	mac[2] = trapmac[1];
	trapmac[0] = '\0';	/* deactivate trap */
	pushback(mac);
}

/*
**	tabcol - enters pseudotab stops, checking validity
*/
tabcol(buf)
	char *buf;{
	int atoi();
	register short tp,incr,val;
	
	for(tp = 0; tp < MAXTABS - 1; tp++){
		skipnbl(buf);
		skipbl(buf);
		if(*buf == '\n')
			break;	/* end of list */
		incr = *buf++ == '+' ? YES : NO;
		val = atoi(buf);
		if(incr == YES && tp > 1)	/* relative tab */
			val = env.tabpos[tp - 1] + val;
		env.tabpos[tp] = val;
		if(val < 0 || (tp > 1 && val < env.tabpos[tp - 1]))
			tp--;
		}
	env.tabpos[tp] = 0;	/* end of list */
}

/*
**	text - process text lines
*/
text(inbuf)
	char *inbuf;{
	register char *p;
	char wrdbuf[MAXIN];
	char *getwrd();
	
	if(indline > 0){
		fprintf(indfp,"%d. %s",curpag,inbuf);
		indline--;
		}
	if(env.litval > 0){
		put(inbuf);
		env.litval--;
		return;
		}
	if(*inbuf == ' ' || *inbuf == '\n')
		leadbl(inbuf);	/* move left, set tival */
	if(env.ulval > 0){			/* underlining */
		underl(inbuf,wrdbuf,&wrdbuf[MAXIN]);
		env.ulval--;
		}
	if(env.bdval > 0){
		bold(inbuf,wrdbuf,&wrdbuf[MAXIN]);
		env.bdval--;
		}
	if(env.ceval > 0){			/* centering */
		center(inbuf);
		put(inbuf);
		env.ceval--;
		}
	else if(*inbuf == '\n')	/* all blank line */
		put(inbuf);
	else if(env.fill == NO)		/* unfilled text */
		put(inbuf);
	else
		for(p = inbuf; (p = getwrd(p,wrdbuf)) != NULL;)
			putwrd(wrdbuf);
}

/*
**	tputc  - intelligent filter routine to eliminate unnecessary blanks
*/
tputc(c)
	register char c;{
	static short curcol	= 0;
	static short lastcol	= 0;

	switch(c){
	    case ' ':
		curcol++;
		break;
	    case '\n':
		curcol = lastcol = 0;
		putchar(c);
		break;
	    default:
		while(lastcol++ < curcol)
			putchar(' ');
		putchar(c);
		curcol++;
		break;
		}
}

/*
**	underl - underline a line
*/
underl(buf,tbuf,tend)
	char *buf,*tbuf,*tend;{
	register char c;
	register char *p,*q;
	char *strcpy();
	
	p = buf;
	q = tbuf;
	while(*p != '\n' && q < tend){
		if(isalnum(c = *p++)){
			*q++ = '_';
			*q++ = '\b';
			}
		*q++ = c;
		}
	*q++ = '\n';
	*q = '\0';
	strcpy(buf,tbuf);	/* copy it back to buf */
}

/*
**	waitfornl - crude version to get newline from terminal
*/
waitfornl(){
	char c;
	int read();

	while(read(ttyfd,&c,1) > 0)
		if(c == '\n')
			break;
}


/*
**	width - compute width of character string
*/
short width(buf)
	char *buf;{
	register short i;
	register char c;
	
	for(i = 0; (c = *buf) != '\0'; buf++){
		if(c == '\b')
			i--;
		else if(c != '\n')
			i++;
		}
	return(i);
}
