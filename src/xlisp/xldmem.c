/* xldmem - xlisp dynamic memory management routines */

#include "xlisp.h"

/* useful definitions */
#define ALLOCSIZE (sizeof(struct segment) + (anodes-1) * sizeof(NODE))

/* external variables */
extern NODE *oblist,*keylist;
extern NODE *xlstack;
extern NODE *xlenv,*xlnewenv;
extern long total;
extern int anodes,nnodes,nsegs,nfree,gccalls;
extern struct segment *segs;
extern NODE *fnodes;

/* external procedures */
extern char *malloc();
extern char *calloc();

/* newnode - allocate a new node */
NODE *newnode(type)
  int type;
{
    NODE *nnode;

    /* get a free node */
    if ((nnode = fnodes) == NIL) {
	gc();
	if ((nnode = fnodes) == NIL)
	    xlabort("insufficient node space");
    }

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    nfree -= 1;

    /* initialize the new node */
    nnode->n_type = type;
    rplacd(nnode,NIL);

    /* return the new node */
    return (nnode);
}

/* stralloc - allocate memory for a string adding a byte for the terminator */
char *stralloc(size)
  int size;
{
    char *sptr;

    /* allocate memory for the string copy */
    if ((sptr = malloc(size+1)) == NULL) {
	gc();
	if ((sptr = malloc(size+1)) == NULL)
	    xlfail("insufficient string space");
    }
    total += (long) (size+1);

    /* return the new string memory */
    return (sptr);
}

/* strsave - generate a dynamic copy of a string */
char *strsave(str)
  char *str;
{
    char *sptr;

    /* create a new string */
    sptr = stralloc(strlen(str));
    strcpy(sptr,str);

    /* return the new string */
    return (sptr);
}

/* strfree - free string memory */
strfree(str)
  char *str;
{
    total -= (long) (strlen(str)+1);
    free(str);
}

/* gc - garbage collect */
gc()
{
    NODE *p;

    /* mark all accessible nodes */
    mark(oblist); mark(keylist);
    mark(xlenv);
    mark(xlnewenv);

    /* mark the evaluation stack */
    for (p = xlstack; p; p = cdr(p))
	mark(car(p));

    /* sweep memory collecting all unmarked nodes */
    sweep();

    /* if there's still nothing available, allocate more memory */
    if (fnodes == NIL)
	addseg();

    /* count the gc call */
    gccalls++;
}

/* mark - mark all accessible nodes */
LOCAL mark(ptr)
  NODE *ptr;
{
    NODE *this,*prev,*tmp;

    /* just return on nil */
    if (ptr == NIL)
	return;

    /* initialize */
    prev = NIL;
    this = ptr;

    /* mark this list */
    while (TRUE) {

	/* descend as far as we can */
	while (TRUE) {

	    /* check for this node being marked */
	    if (this->n_flags & MARK)
		break;

	    /* mark it and its descendants */
	    else {

		/* mark the node */
		this->n_flags |= MARK;

		/* follow the left sublist if there is one */
		if (livecar(this)) {
		    this->n_flags |= LEFT;
		    tmp = prev;
		    prev = this;
		    this = car(prev);
		    rplaca(prev,tmp);
		}

		/* otherwise, follow the right sublist if there is one */
		else if (livecdr(this)) {
		    this->n_flags &= ~LEFT;
		    tmp = prev;
		    prev = this;
		    this = cdr(prev);
		    rplacd(prev,tmp);
		}
		else
		    break;
	    }
	}

	/* backup to a point where we can continue descending */
	while (TRUE) {

	    /* check for termination condition */
	    if (prev == NIL)
		return;

	    /* check for coming from the left side */
	    if (prev->n_flags & LEFT)
		if (livecdr(prev)) {
		    prev->n_flags &= ~LEFT;
		    tmp = car(prev);
		    rplaca(prev,this);
		    this = cdr(prev);
		    rplacd(prev,tmp);
		    break;
		}
		else {
		    tmp = prev;
		    prev = car(tmp);
		    rplaca(tmp,this);
		    this = tmp;
		}

	    /* otherwise, came from the right side */
	    else {
		tmp = prev;
		prev = cdr(tmp);
		rplacd(tmp,this);
		this = tmp;
	    }
	}
    }
}

/* sweep - sweep all unmarked nodes and add them to the free list */
LOCAL sweep()
{
    struct segment *seg;
    NODE *p;
    int n;

    /* empty the free list */
    fnodes = NIL;
    nfree = 0;

    /* add all unmarked nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	p = &seg->sg_nodes[0];
	for (n = seg->sg_size; n--; p++)
	    if (!(p->n_flags & MARK)) {
		switch (ntype(p)) {
		case STR:
			if (p->n_strtype == DYNAMIC && p->n_str != NULL)
			    strfree(p->n_str);
			break;
		case FPTR:
			if (p->n_fp)
			    fclose(p->n_fp);
			break;
		}
		p->n_type = FREE;
		p->n_flags = 0;
		rplaca(p,NIL);
		rplacd(p,fnodes);
		fnodes = p;
		nfree++;
	    }
	    else
		p->n_flags &= ~(MARK | LEFT);
    }
}

/* addseg - add a segment to the available memory */
int addseg()
{
    struct segment *newseg;
    NODE *p;
    int n;

    /* check for zero allocation */
    if (anodes == 0)
	return (FALSE);

    /* allocate a new segment */
    if ((newseg = (struct segment *) calloc(1,ALLOCSIZE)) != NULL) {

	/* initialize the new segment */
	newseg->sg_size = anodes;
	newseg->sg_next = segs;
	segs = newseg;

	/* add each new node to the free list */
	p = &newseg->sg_nodes[0];
	for (n = anodes; n--; ) {
	    rplacd(p,fnodes);
	    fnodes = p++;
	}

	/* update the statistics */
	total += (long) ALLOCSIZE;
	nnodes += anodes;
	nfree += anodes;
	nsegs++;

	/* return successfully */
	return (TRUE);
    }
    else
	return (FALSE);
}
 
/* livecar - do we need to follow the car? */
LOCAL int livecar(n)
  NODE *n;
{
    switch (ntype(n)) {
    case SUBR:
    case FSUBR:
    case INT:
    case STR:
    case FPTR:
	    return (FALSE);
    case SYM:
    case LIST:
    case OBJ:
	    return (car(n) != NIL);
    default:
	    printf("bad node type (%d) found during left scan\n",ntype(n));
	    exit();
    }
}

/* livecdr - do we need to follow the cdr? */
LOCAL int livecdr(n)
  NODE *n;
{
    switch (ntype(n)) {
    case SUBR:
    case FSUBR:
    case INT:
    case STR:
    case FPTR:
	    return (FALSE);
    case SYM:
    case LIST:
    case OBJ:
	    return (cdr(n) != NIL);
    default:
	    printf("bad node type (%d) found during right scan\n",ntype(n));
	    exit();
    }
}

/* stats - print memory statistics */
stats()
{
    printf("Nodes:       %d\n",nnodes);
    printf("Free nodes:  %d\n",nfree);
    printf("Segments:    %d\n",nsegs);
    printf("Allocate:    %d\n",anodes);
    printf("Total:       %ld\n",total);
    printf("Collections: %d\n",gccalls);
}

/* xlminit - initialize the dynamic memory module */
xlminit()
{
    /* initialize our internal variables */
    anodes = NNODES;
    total = 0L;
    nnodes = nsegs = nfree = gccalls = 0;
    fnodes = NIL;
    segs = NULL;

    /* initialize structures that are marked by the collector */
    xlstack = xlenv = xlnewenv = oblist = keylist = NIL;
}
