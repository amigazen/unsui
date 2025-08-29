/* xlobj - xlisp object functions */

#include "xlisp.h"

#ifdef MEGAMAX
overlay "overflow"
#endif

/* external variables */
extern NODE *xlstack;
extern NODE *xlenv,*xlnewenv;
extern NODE *s_stdout;
extern NODE *self;
extern NODE *class;
extern NODE *object;
extern NODE *new;
extern NODE *isnew;
extern NODE *msgcls;
extern NODE *msgclass;
extern int varcnt;

/* instance variable numbers for the class 'Class' */
#define MESSAGES	0	/* list of messages */
#define IVARS		1	/* list of instance variable names */
#define CVARS		2	/* list of class variable names */
#define CVALS		3	/* list of class variable values */
#define SUPERCLASS	4	/* pointer to the superclass */
#define IVARCNT		5	/* number of class instance variables */
#define IVARTOTAL	6	/* total number of instance variables */

/* number of instance variables for the class 'Class' */
#define CLASSSIZE	7

/* forward declarations */
FORWARD NODE *xlgetivar();
FORWARD NODE *xlsetivar();
FORWARD NODE *xlivar();
FORWARD NODE *xlcvar();
FORWARD NODE *findmsg();
FORWARD NODE *findvar();
FORWARD NODE *defvars();
FORWARD NODE *makelist();

/* xlclass - define a class */
NODE *xlclass(name,vcnt)
  char *name; int vcnt;
{
    NODE *sym,*cls;

    /* create the class */
    sym = xlsenter(name);
    cls = sym->n_symvalue = newnode(OBJ);
    cls->n_obclass = class;
    cls->n_obdata = makelist(CLASSSIZE);

    /* set the instance variable counts */
    if (vcnt > 0) {
	xlsetivar(cls,IVARCNT,newnode(INT))->n_int = vcnt;
	xlsetivar(cls,IVARTOTAL,newnode(INT))->n_int = vcnt;
    }

    /* set the superclass to 'Object' */
    xlsetivar(cls,SUPERCLASS,object);

    /* return the new class */
    return (cls);
}

/* xlmfind - find the message binding for a message to an object */
NODE *xlmfind(obj,msym)
  NODE *obj,*msym;
{
    return (findmsg(obj->n_obclass,msym));
}

/* xlxsend - send a message to an object */
NODE *xlxsend(obj,msg,args)
  NODE *obj,*msg,*args;
{
    NODE *oldstk,*oldenv,*oldnewenv,method,cptr,eargs,val,*isnewmsg;

    /* save the old environment */
    oldnewenv = xlnewenv; oldenv = xlnewenv = xlenv;

    /* create a new stack frame */
    oldstk = xlsave(&method,&cptr,&eargs,&val,NULL);

    /* get the method for this message */
    method.n_ptr = cdr(msg);

    /* make sure its a function or a subr */
    if (!subrp(method.n_ptr) && !consp(method.n_ptr))
	xlfail("bad method");

    /* bind the symbols 'self' and 'msgclass' */
    xlbind(self,obj);
    xlbind(msgclass,msgcls);

    /* evaluate the function call */
    eargs.n_ptr = xlevlist(args);
    if (subrp(method.n_ptr)) {
	xlfixbindings();
	val.n_ptr = (*method.n_ptr->n_subr)(eargs.n_ptr);
    }
    else {

	/* bind the formal arguments */
	xlabind(car(method.n_ptr),eargs.n_ptr);
	xlfixbindings();

	/* execute the code */
	cptr.n_ptr = cdr(method.n_ptr);
	while (cptr.n_ptr != NIL)
	    val.n_ptr = xlevarg(&cptr.n_ptr);
    }

    /* restore the environment */
    xlunbind(oldenv); xlnewenv = oldnewenv;

    /* after creating an object, send it the "isnew" message */
    if (car(msg) == new && val.n_ptr != NIL) {
	if ((isnewmsg = xlmfind(val.n_ptr,isnew)) == NIL)
	    xlfail("no method for the isnew message");
	val.n_ptr = xlxsend(val.n_ptr,isnewmsg,args);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val.n_ptr);
}

/* xlsend - send a message to an object (message in arg list) */
NODE *xlsend(obj,args)
  NODE *obj,*args;
{
    NODE *msg;

    /* find the message binding for this message */
    if ((msg = xlmfind(obj,xlevmatch(SYM,&args))) == NIL)
	xlfail("no method for this message");

    /* send the message */
    return (xlxsend(obj,msg,args));
}

/* xlobsym - find a class or instance variable for the current object */
NODE *xlobsym(sym)
  NODE *sym;
{
    NODE *obj;

    if ((obj = self->n_symvalue) != NIL && objectp(obj))
	return (findvar(obj,sym));
    else
	return (NIL);
}

/* mnew - create a new object instance */
LOCAL NODE *mnew()
{
    NODE *oldstk,obj,*cls;

    /* create a new stack frame */
    oldstk = xlsave(&obj,NULL);

    /* get the class */
    cls = self->n_symvalue;

    /* generate a new object */
    obj.n_ptr = newnode(OBJ);
    obj.n_ptr->n_obclass = cls;
    obj.n_ptr->n_obdata = makelist(getivcnt(cls,IVARTOTAL));

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new object */
    return (obj.n_ptr);
}

/* misnew - initialize a new class */
LOCAL NODE *misnew(args)
  NODE *args;
{
    NODE *oldstk,super,*obj;

    /* create a new stack frame */
    oldstk = xlsave(&super,NULL);

    /* get the superclass if there is one */
    if (args != NIL)
	super.n_ptr = xlmatch(OBJ,&args);
    else
	super.n_ptr = object;
    xllastarg(args);

    /* get the object */
    obj = self->n_symvalue;

    /* store the superclass */
    xlsetivar(obj,SUPERCLASS,super.n_ptr);
    xlsetivar(obj,IVARTOTAL,newnode(INT))->n_int =
        getivcnt(super.n_ptr,IVARTOTAL);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new object */
    return (obj);
}

/* xladdivar - enter an instance variable */
xladdivar(cls,var)
  NODE *cls; char *var;
{
    NODE *ivar,*lptr;

    /* find the 'ivars' instance variable */
    ivar = xlivar(cls,IVARS);

    /* add the instance variable */
    lptr = newnode(LIST);
    rplacd(lptr,car(ivar));
    rplaca(ivar,lptr);
    rplaca(lptr,xlsenter(var));
}

/* entermsg - add a message to a class */
LOCAL NODE *entermsg(cls,msg)
  NODE *cls,*msg;
{
    NODE *ivar,*lptr,*mptr;

    /* find the 'messages' instance variable */
    ivar = xlivar(cls,MESSAGES);

    /* lookup the message */
    for (lptr = car(ivar); lptr != NIL; lptr = cdr(lptr))
	if (car(mptr = car(lptr)) == msg)
	    return (mptr);

    /* allocate a new message entry if one wasn't found */
    lptr = newnode(LIST);
    rplacd(lptr,car(ivar));
    rplaca(ivar,lptr);
    rplaca(lptr,mptr = newnode(LIST));
    rplaca(mptr,msg);

    /* return the symbol node */
    return (mptr);
}

/* answer - define a method for answering a message */
LOCAL NODE *answer(args)
  NODE *args;
{
    NODE *oldstk,arg,msg,fargs,code;
    NODE *obj,*mptr,*fptr;

    /* create a new stack frame */
    oldstk = xlsave(&arg,&msg,&fargs,&code,NULL);

    /* initialize */
    arg.n_ptr = args;

    /* message symbol, formal argument list and code */
    msg.n_ptr = xlmatch(SYM,&arg.n_ptr);
    fargs.n_ptr = xlmatch(LIST,&arg.n_ptr);
    code.n_ptr = xlmatch(LIST,&arg.n_ptr);
    xllastarg(arg.n_ptr);

    /* get the object node */
    obj = self->n_symvalue;

    /* make a new message list entry */
    mptr = entermsg(obj,msg.n_ptr);

    /* setup the message node */
    rplacd(mptr,fptr = newnode(LIST));
    rplaca(fptr,fargs.n_ptr);
    rplacd(fptr,code.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the object */
    return (obj);
}

/* mivars - define the list of instance variables */
LOCAL NODE *mivars(args)
  NODE *args;
{
    NODE *cls,*super;
    int scnt;

    /* define the list of instance variables */
    cls = defvars(args,IVARS);

    /* get the superclass instance variable count */
    if ((super = xlgetivar(cls,SUPERCLASS)) != NIL)
	scnt = getivcnt(super,IVARTOTAL);
    else
	scnt = 0;

    /* save the number of instance variables */
    xlsetivar(cls,IVARCNT,newnode(INT))->n_int = varcnt;
    xlsetivar(cls,IVARTOTAL,newnode(INT))->n_int = scnt+varcnt;

    /* return the class */
    return (cls);
}

/* getivcnt - get the number of instance variables for a class */
LOCAL int getivcnt(cls,ivar)
  NODE *cls; int ivar;
{
    NODE *cnt;

    if ((cnt = xlgetivar(cls,ivar)) != NIL)
	if (fixp(cnt))
	    return (cnt->n_int);
	else
	    xlfail("bad value for instance variable count");
    else
	return (0);
}

/* mcvars - define the list of class variables */
LOCAL NODE *mcvars(args)
  NODE *args;
{
    NODE *cls;

    /* define the list of class variables */
    cls = defvars(args,CVARS);

    /* make a new list of values */
    xlsetivar(cls,CVALS,makelist(varcnt));

    /* return the class */
    return (cls);
}

/* defvars - define a class or instance variable list */
LOCAL NODE *defvars(args,varnum)
  NODE *args; int varnum;
{
    NODE *oldstk,vars,*vptr,*cls,*sym;

    /* create a new stack frame */
    oldstk = xlsave(&vars,NULL);

    /* get ivar list */
    vars.n_ptr = xlmatch(LIST,&args);
    xllastarg(args);

    /* get the class node */
    cls = self->n_symvalue;

    /* check each variable in the list */
    varcnt = 0;
    for (vptr = vars.n_ptr;
	 consp(vptr);
	 vptr = cdr(vptr)) {

	/* make sure this is a valid symbol in the list */
	if ((sym = car(vptr)) == NIL || !symbolp(sym))
	    xlfail("bad variable list");

	/* make sure its not already defined */
	if (checkvar(cls,sym))
	    xlfail("multiply defined variable");

	/* count the variable */
	varcnt++;
    }

    /* make sure the list ended properly */
    if (vptr != NIL)
	xlfail("bad variable list");

    /* define the new variable list */
    xlsetivar(cls,varnum,vars.n_ptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the class */
    return (cls);
}

/* xladdmsg - add a message to a class */
xladdmsg(cls,msg,code)
  NODE *cls; char *msg; NODE *(*code)();
{
    NODE *mptr;

    /* enter the message selector */
    mptr = entermsg(cls,xlsenter(msg));

    /* store the method for this message */
    rplacd(mptr,newnode(SUBR));
    cdr(mptr)->n_subr = code;
}

/* getclass - get the class of an object */
LOCAL NODE *getclass(args)
  NODE *args;
{
    /* make sure there aren't any arguments */
    xllastarg(args);

    /* return the object's class */
    return (self->n_symvalue->n_obclass);
}

/* obshow - show the instance variables of an object */
LOCAL NODE *obshow(args)
  NODE *args;
{
    NODE *fptr;

    /* get the file pointer */
    fptr = (args ? xlmatch(FPTR,&args) : s_stdout->n_symvalue);
    xllastarg(args);

    /* print the object's instance variables */
    xlprint(fptr,self->n_symvalue->n_obdata,TRUE);
    xlterpri(fptr);

    /* return the object */
    return (self->n_symvalue);
}

/* defisnew - default 'isnew' method */
LOCAL NODE *defisnew(args)
  NODE *args;
{
    /* make sure there aren't any arguments */
    xllastarg(args);

    /* return the object */
    return (self->n_symvalue);
}

/* sendsuper - send a message to an object's superclass */
LOCAL NODE *sendsuper(args)
  NODE *args;
{
    NODE *obj,*super,*msg;

    /* get the object */
    obj = self->n_symvalue;

    /* get the object's superclass */
    super = xlgetivar(obj->n_obclass,SUPERCLASS);

    /* find the message binding for this message */
    if ((msg = findmsg(super,xlmatch(SYM,&args))) == NIL)
	xlfail("no method for this message");

    /* send the message */
    return (xlxsend(obj,msg,args));
}

/* findmsg - find the message binding given an object and a class */
LOCAL NODE *findmsg(cls,sym)
  NODE *cls,*sym;
{
    NODE *lptr,*msg;

    /* start at the specified class */
    msgcls = cls;

    /* look for the message in the class or superclasses */
    while (msgcls != NIL) {

	/* lookup the message in this class */
	for (lptr = xlgetivar(msgcls,MESSAGES);
	     lptr != NIL;
	     lptr = cdr(lptr))
	    if ((msg = car(lptr)) != NIL && car(msg) == sym)
		return (msg);

	/* look in class's superclass */
	msgcls = xlgetivar(msgcls,SUPERCLASS);
    }

    /* message not found */
    return (NIL);
}

/* findvar - find a class or instance variable */
LOCAL NODE *findvar(obj,sym)
  NODE *obj,*sym;
{
    NODE *cls,*lptr;
    int base,varnum;
    int found;

    /* get the class of the object */
    cls = obj->n_obclass;

    /* get the total number of instance variables */
    base = getivcnt(cls,IVARTOTAL);

    /* find the variable */
    found = FALSE;
    for (; cls != NIL; cls = xlgetivar(cls,SUPERCLASS)) {

	/* get the number of instance variables for this class */
	if ((base -= getivcnt(cls,IVARCNT)) < 0)
	    xlfail("error finding instance variable");

	/* check for finding the class of the current message */
	if (!found && cls == msgclass->n_symvalue)
	    found = TRUE;

	/* lookup the instance variable */
	varnum = 0;
	for (lptr = xlgetivar(cls,IVARS);
    	     lptr != NIL;
    	     lptr = cdr(lptr))
	    if (found && car(lptr) == sym)
		return (xlivar(obj,base + varnum));
	    else
		varnum++;

	/* skip the class variables if the message class hasn't been found */
	if (!found)
	    continue;

	/* lookup the class variable */
	varnum = 0;
	for (lptr = xlgetivar(cls,CVARS);
    	     lptr != NIL;
    	     lptr = cdr(lptr))
	    if (car(lptr) == sym)
		return (xlcvar(cls,varnum));
	    else
		varnum++;
    }

    /* variable not found */
    return (NIL);
}

/* checkvar - check for an existing class or instance variable */
LOCAL int checkvar(cls,sym)
  NODE *cls,*sym;
{
    NODE *lptr;

    /* find the variable */
    for (; cls != NIL; cls = xlgetivar(cls,SUPERCLASS)) {

	/* lookup the instance variable */
	for (lptr = xlgetivar(cls,IVARS);
    	     lptr != NIL;
    	     lptr = cdr(lptr))
	    if (car(lptr) == sym)
		return (TRUE);

	/* lookup the class variable */
	for (lptr = xlgetivar(cls,CVARS);
    	     lptr != NIL;
    	     lptr = cdr(lptr))
	    if (car(lptr) == sym)
		return (TRUE);
    }

    /* variable not found */
    return (FALSE);
}

/* xlgetivar - get the value of an instance variable */
NODE *xlgetivar(obj,num)
  NODE *obj; int num;
{
    return (car(xlivar(obj,num)));
}

/* xlsetivar - set the value of an instance variable */
NODE *xlsetivar(obj,num,val)
  NODE *obj; int num; NODE *val;
{
    rplaca(xlivar(obj,num),val);
    return (val);
}

/* xlivar - get an instance variable */
NODE *xlivar(obj,num)
  NODE *obj; int num;
{
    NODE *ivar;

    /* get the instance variable */
    for (ivar = obj->n_obdata; num > 0; num--)
	if (ivar != NIL)
	    ivar = cdr(ivar);
	else
	    xlfail("bad instance variable list");

    /* return the instance variable */
    return (ivar);
}

/* xlcvar - get a class variable */
NODE *xlcvar(cls,num)
  NODE *cls; int num;
{
    NODE *cvar;

    /* get the class variable */
    for (cvar = xlgetivar(cls,CVALS); num > 0; num--)
	if (cvar != NIL)
	    cvar = cdr(cvar);
	else
	    xlfail("bad class variable list");

    /* return the class variable */
    return (cvar);
}

/* makelist - make a list of nodes */
LOCAL NODE *makelist(cnt)
  int cnt;
{
    NODE *oldstk,list,*lnew;

    /* create a new stack frame */
    oldstk = xlsave(&list,NULL);

    /* make the list */
    for (; cnt > 0; cnt--) {
	lnew = newnode(LIST);
	rplacd(lnew,list.n_ptr);
	list.n_ptr = lnew;
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the list */
    return (list.n_ptr);
}

/* xloinit - object function initialization routine */
xloinit()
{
    /* don't confuse the garbage collector */
    class = object = NIL;

    /* enter the object related symbols */
    new		= xlsenter("new");
    isnew	= xlsenter("isnew");
    self	= xlsenter("self");
    msgclass	= xlsenter("msgclass");

    /* create the 'Class' object */
    class = xlclass("Class",CLASSSIZE);
    class->n_obclass = class;

    /* create the 'Object' object */
    object = xlclass("Object",0);

    /* finish initializing 'class' */
    xlsetivar(class,SUPERCLASS,object);
    xladdivar(class,"ivartotal");	/* ivar number 6 */
    xladdivar(class,"ivarcnt");		/* ivar number 5 */
    xladdivar(class,"superclass");	/* ivar number 4 */
    xladdivar(class,"cvals");		/* ivar number 3 */
    xladdivar(class,"cvars");		/* ivar number 2 */
    xladdivar(class,"ivars");		/* ivar number 1 */
    xladdivar(class,"messages");	/* ivar number 0 */
    xladdmsg(class,"new",mnew);
    xladdmsg(class,"answer",answer);
    xladdmsg(class,"ivars",mivars);
    xladdmsg(class,"cvars",mcvars);
    xladdmsg(class,"isnew",misnew);

    /* finish initializing 'object' */
    xladdmsg(object,"class",getclass);
    xladdmsg(object,"show",obshow);
    xladdmsg(object,"isnew",defisnew);
    xladdmsg(object,"sendsuper",sendsuper);
}
