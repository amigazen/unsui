#include <stdio.h>
#include <stdlib.h>

#include <exec/types.h>
#include <utility/tagitem.h>
#include <intuition/intuition.h>

#include <intuition/classusr.h>
#include <intuition/imageclass.h>
#include <intuition/gadgetclass.h>
#include <intuition/cghooks.h>
#include <intuition/icclass.h>
#include <intuition/classes.h>

#include <clib/intuition_protos.h>
#include <clib/exec_protos.h>
#include <clib/utility_protos.h>
#include <clib/alib_protos.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/exec_pragmas.h>
#include <pragmas/utility_pragmas.h>

#include "globals.h"
#include "pgmodel.h"
#include "pgscroll.i"



extern struct  ExecBase	       *SysBase;
extern struct  IntuitionBase   *IntuitionBase;
extern struct  GfxBase         *GfxBase;
extern struct  Library         *UtilityBase;


static Object		* mymodel 	= NULL;	/* the repository of the "Current Value" */
static void		* MyModClass	= NULL;


static struct Gadget	* propg		= NULL;
static struct Gadget	* stringg	= NULL;
static struct Gadget	* rightarrowg	= NULL;
static struct Gadget	* leftarrowg	= NULL;
static struct Gadget	* mygadgets	= NULL;		/* linked list	*/

/* pictures for arrows	*/
static struct Image	* rightimage	= NULL;
static struct Image	* leftimage	= NULL;


/* some static layout and setup constants, for now	*/
#define SWIDTH		(3*8)
#define INITVAL		(0)	/* initial value of string and slider	*/
#define PWIDTH		(200)	/* width of horizontal propslider	*/


enum gadgetids {
    gUp = 50,
    gDown,
    gSlider,
    gString,
};


/****************************************************************/
/*  mapping tag lists						*/
/****************************************************************/

/* for IDCMPUPDATE	*/
struct TagItem	model2me[] = {
    {MYMODA_CURRVAL, ICSPECIAL_CODE },	/* put (16 bits of) currval into IntuiMessage.Code */
    {TAG_END, }
};

struct TagItem	slider2model[] = {
    {PGA_TOP, MYMODA_CURRVAL},
    {TAG_END, }
};

struct TagItem	model2slider[] = {
    {MYMODA_CURRVAL, PGA_TOP},
    {MYMODA_RANGE,   PGA_TOTAL },
    {TAG_END, }
};

struct TagItem	string2model[] = {
    {STRINGA_LongVal, MYMODA_CURRVAL},
    {TAG_END, }
};

struct TagItem	model2string[] = {
    {MYMODA_CURRVAL, STRINGA_LongVal},
    {TAG_END, }
};

struct TagItem	uparrow2model[] = {
    {GA_ID, MYMODA_INCRSTROBE},
    {TAG_END, }
};

struct TagItem	downarrow2model[] = {
    {GA_ID, MYMODA_DECRSTROBE},
    {TAG_END, }
};


/* a macro for the "adjacent" position to the right of a gadget,
 * but safe, if the reference gadget is NULL
 */
#define RIGHTBY( g )	( ((g)==NULL)? 20: ((g)->LeftEdge + (g)->Width ) )




int InitPGScrollGad(struct Window * w, struct DrawInfo * drinfo)
{
    struct Gadget	* tmpgad;
    Object		* ic;


    /* get images for the up and down arrows, sensitive
     * to depth and pen specs for current screen (we'll be
     * adding resolution/size selection later).
     */
    rightimage = (struct Image *) NewObject( NULL, "sysiclass",
			SYSIA_Size,	0,		/* normal "medium-res" for now */
			SYSIA_DrawInfo, drinfo,
			SYSIA_Which,	RIGHTIMAGE,
		    	TAG_END );

    leftimage = (struct Image *) NewObject( NULL, "sysiclass",
			SYSIA_Size,	0,		/* normal "medium-res" for now */
			SYSIA_Which,	LEFTIMAGE,
			SYSIA_DrawInfo, drinfo,
		    	TAG_END );


    /* get "model" object which is the repository of our "current
     * value" and is the hub of object interconnection.
     * This thing also is used to free icclass objects,
     * so we'd better make sure it got allocated.
     */
    MyModClass = initMyModClass();	/* private class	*/

    mymodel = (Object *) NewObject( MyModClass, NULL, 
			ICA_TARGET,	ICTARGET_IDCMP,	/* talk to me	*/
			ICA_MAP,	model2me,
			TAG_END );

    if ( ! mymodel ) {
      DispPGScrollGad( w );
      return FALSE;
    }

    /* make gadgets, link into list (easier starting with Beta 4) */
    tmpgad = (struct Gadget *) &mygadgets;


    stringg  = (struct Gadget *) NewObject(NULL, "strgclass",
			GA_ID,			gString,
			GA_Width,		SWIDTH,
			GA_Height,		8,		/* fix this	*/
			GA_Left,		w->BorderLeft+5,
			GA_RelBottom,		-w->BorderBottom+2,
			GA_BottomBorder,	TRUE,
			GA_Previous,		tmpgad,

			STRINGA_MaxChars,	5,
			STRINGA_LongVal,	INITVAL,
			STRINGA_Justification,	STRINGRIGHT,

			ICA_TARGET,		mymodel,
			ICA_MAP,		string2model,
			TAG_END );

    propg = (struct Gadget *) NewObject( NULL, "propgclass",
			GA_ID,			gSlider,
			GA_Left,		RIGHTBY(stringg)+2,
			GA_RelBottom,		-w->BorderBottom+1+2,
			GA_BottomBorder,	TRUE,
			GA_Height,		6,	/* normale border bottom mit size gadg */
			GA_Width,		PWIDTH,	/* height to be specified	*/
			GA_Previous,		tmpgad,

			PGA_Freedom,		FREEHORIZ,
			PGA_NewLook,		TRUE,
			PGA_Borderless,		TRUE,
			PGA_Top,		INITVAL,
			PGA_Total,		1,
			PGA_Visible,		1,		/* want an integer value slider	*/

			ICA_TARGET,		mymodel,
			ICA_MAP,		slider2model,
			TAG_END );


    leftarrowg = (struct Gadget *) NewObject( NULL, "buttongclass",
			GA_ID,			gDown,
			GA_Left,		RIGHTBY(propg) + 2,
			GA_RelBottom,		-w->BorderBottom+1,
			GA_BottomBorder,	TRUE,
			GA_Image,		leftimage,
			GA_Previous,		tmpgad,

			ICA_TARGET,		mymodel,
			ICA_MAP,		downarrow2model,
			TAG_END );

    /* get up/down arrow button gadgets	*/
    rightarrowg = (struct Gadget *) NewObject( NULL, "buttongclass",
			GA_ID,			gUp,
			GA_Left,		RIGHTBY(leftarrowg),
			GA_RelBottom,		-w->BorderBottom+1,
			GA_BottomBorder,	TRUE,
			GA_Image, 		rightimage,
			GA_Previous,		tmpgad,

			ICA_TARGET,		mymodel,
			ICA_MAP,		uparrow2model,
			TAG_END );

    /*
     * We now have all the gadgets talking to the model,
     * but we need to create some little IC nodes for
     * the model to update the string and propotional gadgets
     */
    ic = NewObject( NULL, ICCLASS,
			ICA_TARGET,		stringg,
			ICA_MAP,		model2string,
			TAG_END );
    DoMethod( mymodel, OM_ADDMEMBER, ic );

    ic = NewObject( NULL, ICCLASS,
			ICA_TARGET,		propg,
			ICA_MAP,		model2slider,
			TAG_END );
    DoMethod( mymodel, OM_ADDMEMBER, ic );


    AddGList( w, mygadgets, -1, 4, NULL );
    RefreshGList( mygadgets, w, NULL, 4 );


    return TRUE;	// alle ok!
}



void ChangePGScrollGad(struct Window * w, int range, int curval)
{
    /* although we're changing the attributes of
     * the model, if we want the gadgets attached 
     * to it to be able to refresh, we have to 
     * use SetGadgetAttr()s rather than SetAttrs()
     */

    SetAttrs( mymodel, 
		MYMODA_RANGE, range,
		MYMODA_CURRVAL, curval,
		MYMODA_LOWERB, 1,
		TAG_END );
    //RefreshGList( mygadgets, w, NULL, 4 );
    RefreshGList( rightarrowg, w, NULL, 4 );
    RefreshGList( stringg, w, NULL, 1 );

    /*
    SetGadgetAttrsA( (struct Gadget *)mymodel, w, NULL, setrangevaltags);
    SetGadgetAttrs( (struct Gadget *)mymodel, w, NULL,
    			MYMODA_RANGE, PROPRANGE,
			MYMODA_CURRVAL, PROPRANGE/2,
			TAG_END );
    */
}


void DispPGScrollGad(struct Window * w)
{
    RemoveGList( w, mygadgets, 4 );

    DisposeObject( mymodel );

    DisposeObject( propg );
    DisposeObject( stringg );
    DisposeObject( rightarrowg );
    DisposeObject( leftarrowg );

    mymodel   = NULL;
    mygadgets = NULL;
    
    propg = NULL;
    stringg = NULL;
    rightarrowg = NULL;
    leftarrowg = NULL;

    DisposeObject(rightimage);
    DisposeObject(leftimage);
    
    rightimage = NULL;
    leftimage  = NULL;

    if ( MyModClass ) {
	freeMyModClass( MyModClass );
	MyModClass = NULL;
    }
}


long GetValPgScrollGad(void)
{
  ULONG currval;

  GetAttr( MYMODA_CURRVAL, mymodel, &currval );
  
  return (long)currval;
}
