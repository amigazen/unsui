/* xlread - xlisp expression input routine */

#include "xlisp.h"
#include "ctype.h"

/* external variables */
extern NODE *s_stdout,*true;
extern NODE *s_quote,*s_function,*s_bquote,*s_comma,*s_comat;
extern NODE *xlstack;
extern int xlplevel;

/* external routines */
extern FILE *fopen();

/* forward declarations */
FORWARD NODE *plist();
FORWARD NODE *pstring();
FORWARD NODE *pquote();
FORWARD NODE *pname();

/* xlload - load a file of xlisp expressions */
int xlload(name,vflag,pflag)
  char *name; int vflag,pflag;
{
    NODE *oldstk,fptr,expr;
    char fname[50];
    CONTEXT cntxt;
    int sts;

    /* create a new stack frame */
    oldstk = xlsave(&fptr,&expr,NULL);

    /* allocate a file node */
    fptr.n_ptr = newnode(FPTR);
    fptr.n_ptr->n_fp = NULL;
    fptr.n_ptr->n_savech = 0;

    /* create the file name and print the information line */
    strcpy(fname,name); strcat(fname,".lsp");
    if (vflag)
	printf("; loading \"%s\"\n",fname);

    /* open the file */
    if ((fptr.n_ptr->n_fp = fopen(fname,"r")) == NULL) {
	xlstack = oldstk;
	return (FALSE);
    }

    /* read, evaluate and possibly print each expression in the file */
    xlbegin(&cntxt,CF_ERROR,true);
    if (setjmp(cntxt.c_jmpbuf))
	sts = FALSE;
    else {
	while (xlread(fptr.n_ptr,&expr.n_ptr)) {
	    expr.n_ptr = xleval(expr.n_ptr);
	    if (pflag)
		stdprint(expr.n_ptr);
	}
	sts = TRUE;
    }
    xlend(&cntxt);

    /* close the file */
    fclose(fptr.n_ptr->n_fp);
    fptr.n_ptr->n_fp = NULL;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return status */
    return (sts);
}

/* xlread - read an xlisp expression */
int xlread(fptr,pval)
  NODE *fptr,**pval;
{
    /* initialize */
    xlplevel = 0;

    /* parse an expression */
    return (parse(fptr,pval));
}

/* parse - parse an xlisp expression */
LOCAL int parse(fptr,pval)
  NODE *fptr,**pval;
{
    int ch;

    /* keep looking for a node skipping comments */
    while (TRUE)

	/* check next character for type of node */
	switch (ch = nextch(fptr)) {
	case EOF:
		xlgetc(fptr);
		return (FALSE);
	case '\'':			/* a quoted expression */
		xlgetc(fptr);
		*pval = pquote(fptr,s_quote);
		return (TRUE);
	case '#':			/* a quoted function */
		xlgetc(fptr);
		if ((ch = xlgetc(fptr)) == '<')
		    xlfail("unreadable atom");
		else if (ch != '\'')
		    xlfail("expected quote after #");
		*pval = pquote(fptr,s_function);
		return (TRUE);
	case '`':			/* a back quoted expression */
		xlgetc(fptr);
		*pval = pquote(fptr,s_bquote);
		return (TRUE);
	case ',':			/* a comma or comma-at expression */
		xlgetc(fptr);
		if (xlpeek(fptr) == '@') {
		    xlgetc(fptr);
		    *pval = pquote(fptr,s_comat);
		}
		else
		    *pval = pquote(fptr,s_comma);
		return (TRUE);
	case '(':			/* a sublist */
		*pval = plist(fptr);
		return (TRUE);
	case ')':			/* closing paren - shouldn't happen */
		xlfail("extra right paren");
	case '.':			/* dot - shouldn't happen */
		xlfail("misplaced dot");
	case ';':			/* a comment */
		pcomment(fptr);
		break;
	case '"':			/* a string */
		*pval = pstring(fptr);
		return (TRUE);
	default:
		if (issym(ch))		/* a name */
		    *pval = pname(fptr);
		else
		    xlfail("invalid character");
		return (TRUE);
	}
}

/* pcomment - parse a comment */
LOCAL pcomment(fptr)
  NODE *fptr;
{
    int ch;

    /* skip to end of line */
    while ((ch = checkeof(fptr)) != EOF && ch != '\n')
	;
}

/* plist - parse a list */
LOCAL NODE *plist(fptr)
  NODE *fptr;
{
    NODE *oldstk,val,*lastnptr,*nptr,*p;
    int ch;

    /* increment the nesting level */
    xlplevel += 1;

    /* create a new stack frame */
    oldstk = xlsave(&val,NULL);

    /* skip the opening paren */
    xlgetc(fptr);

    /* keep appending nodes until a closing paren is found */
    lastnptr = NIL;
    for (lastnptr = NIL; (ch = nextch(fptr)) != ')'; lastnptr = nptr) {

	/* check for end of file */
	if (ch == EOF)
	    badeof(fptr);

	/* check for a dotted pair */
	if (ch == '.') {

	    /* skip the dot */
	    xlgetc(fptr);

	    /* make sure there's a node */
	    if (lastnptr == NIL)
		xlfail("invalid dotted pair");

	    /* parse the expression after the dot */
	    if (!parse(fptr,&p))
		badeof(fptr);
	    rplacd(lastnptr,p);

	    /* make sure its followed by a close paren */
	    if (nextch(fptr) != ')')
		xlfail("invalid dotted pair");

	    /* done with this list */
	    break;
	}

	/* allocate a new node and link it into the list */
	nptr = newnode(LIST);
	if (lastnptr == NIL)
	    val.n_ptr = nptr;
	else
	    rplacd(lastnptr,nptr);

	/* initialize the new node */
	if (!parse(fptr,&p))
	    badeof(fptr);
	rplaca(nptr,p);
    }

    /* skip the closing paren */
    xlgetc(fptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* decrement the nesting level */
    xlplevel -= 1;

    /* return successfully */
    return (val.n_ptr);
}

/* pstring - parse a string */
LOCAL NODE *pstring(fptr)
  NODE *fptr;
{
    NODE *oldstk,val;
    char sbuf[STRMAX+1];
    int ch,i,d1,d2,d3;

    /* create a new stack frame */
    oldstk = xlsave(&val,NULL);

    /* skip the opening quote */
    xlgetc(fptr);

    /* loop looking for a closing quote */
    for (i = 0; i < STRMAX && (ch = checkeof(fptr)) != '"'; i++) {
	switch (ch) {
	case EOF:
		badeof(fptr);
	case '\\':
		switch (ch = checkeof(fptr)) {
		case 'e':
			ch = '\033';
			break;
		case 'n':
			ch = '\n';
			break;
		case 'r':
			ch = '\r';
			break;
		case 't':
			ch = '\t';
			break;
		default:
			if (ch >= '0' && ch <= '7') {
			    d1 = ch - '0';
			    d2 = checkeof(fptr) - '0';
			    d3 = checkeof(fptr) - '0';
			    ch = (d1 << 6) + (d2 << 3) + d3;
			}
			break;
		}
	}
	sbuf[i] = ch;
    }
    sbuf[i] = 0;

    /* initialize the node */
    val.n_ptr = newnode(STR);
    val.n_ptr->n_str = strsave(sbuf);
    val.n_ptr->n_strtype = DYNAMIC;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new string */
    return (val.n_ptr);
}

/* pquote - parse a quoted expression */
LOCAL NODE *pquote(fptr,sym)
  NODE *fptr,*sym;
{
    NODE *oldstk,val,*p;

    /* create a new stack frame */
    oldstk = xlsave(&val,NULL);

    /* allocate two nodes */
    val.n_ptr = newnode(LIST);
    rplaca(val.n_ptr,sym);
    rplacd(val.n_ptr,newnode(LIST));

    /* initialize the second to point to the quoted expression */
    if (!parse(fptr,&p))
	badeof(fptr);
    rplaca(cdr(val.n_ptr),p);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the quoted expression */
    return (val.n_ptr);
}

/* pname - parse a symbol name */
LOCAL NODE *pname(fptr)
  NODE *fptr;
{
    char sname[STRMAX+1];
    NODE *val;
    int i;

    /* get symbol name */
    for (i = 0; i < STRMAX && issym(xlpeek(fptr)); )
	sname[i++] = xlgetc(fptr);
    sname[i] = 0;

    /* check for a number or enter the symbol into the oblist */
    return (isnumber(sname,&val) ? val : xlenter(sname,DYNAMIC));
}

/* nextch - look at the next non-blank character */
LOCAL int nextch(fptr)
  NODE *fptr;
{
    int ch;

    /* return and save the next non-blank character */
    while ((ch = xlpeek(fptr)) != EOF && isspace(ch))
	xlgetc(fptr);
    return (ch);
}

/* checkeof - get a character and check for end of file */
LOCAL int checkeof(fptr)
  NODE *fptr;
{
    int ch;

    if ((ch = xlgetc(fptr)) == EOF)
	badeof(fptr);
    return (ch);
}

/* badeof - unexpected eof */
LOCAL badeof(fptr)
  NODE *fptr;
{
    xlgetc(fptr);
    xlfail("unexpected EOF");
}

/* isnumber - check if this string is a number */
int isnumber(str,pval)
  char *str; NODE **pval;
{
    char *p;
    int d;

    /* initialize */
    p = str; d = 0;

    /* check for a sign */
    if (*p == '+' || *p == '-')
	p++;

    /* check for a string of digits */
    while (isdigit(*p))
	p++, d++;

    /* make sure there was at least one digit and this is the end */
    if (d == 0 || *p)
	return (FALSE);

    /* convert the string to an integer and return successfully */
    *pval = newnode(INT);
    (*pval)->n_int = atoi(*str == '+' ? ++str : str);
    return (TRUE);
}

/* issym - check whether a character if valid in a symbol name */
LOCAL int issym(ch)
  int ch;
{
    if (ch <= ' ' || ch >= 0177 ||
    	ch == '(' ||
    	ch == ')' ||
    	ch == ';' || 
	ch == ',' ||
	ch == '`' ||
    	ch == '"' ||
    	ch == '\'')
	return (FALSE);
    else
	return (TRUE);
}
