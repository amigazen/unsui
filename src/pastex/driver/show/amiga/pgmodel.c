/* mymodelclass.c -- :ts=8
 * Example of a simple subclass of "modelclass".
 * It maintains an integer "current value", which
 * it keeps between 0 and some specified maximum.
 */

/*
Copyright (c) 1989, 1990 Commodore-Amiga, Inc.

Executables based on this information may be used in software
for Commodore Amiga computers. All other rights reserved.
This information is provided "as is"; no warranties are made.
All use is at your own risk, and no liability or responsibility
is assumed.
*/

#include <stdio.h>
#include <dos.h>

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
#include <clib/utility_protos.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/utility_pragmas.h>

extern ULONG __stdargs DoSuperMethodA( struct IClass *cl, Object *obj, Msg message );
extern ULONG __stdargs DoMethod( Object *obj, unsigned long MethodID, ... );


#include "pgmodel.h"		/* attributes are defined there	*/

extern struct Library   * UtilityBase;
extern struct Library	* IntuitionBase;



#ifdef printf
#undef printf
#endif

#define printf kprintf
int kprintf(char * fmt, ...);

#define DK(x)	;
//#define DK(x)	x

/* private class	*/
#define MYCLASSID	(NULL)
#define SUPERCLASSID	MODELCLASS

struct MyModData	{
    long        mmd_RegA4;		/* Register A4 */
    ULONG	mmd_CurrentValue;
    ULONG	mmd_Range;		/* current value <= range-1	*/
    ULONG	mmd_LowerBorder;	/* current value >= lower border */
    ULONG	mmd_LastIncSecs;	/* timer tick of last INC operation */
    ULONG	mmd_LastIncMicros;	/* timer tick of last INC operation */
};

/*
 * Achtung!!
 *
 * Der Value liegt IMMER im Bereich zwischen 0..range-1 !!
 *
 * Der LowerBorder geht hier nicht ein, der ist nur fuer die ANZEIGE
 * im Integer-Gadget wichtig!
 *
 * Uups..brauch ich ihn ueberhaupt dort...
 *
 * Das Integer-Gadget muss ich noch durch ein eigenes ersetzen, das
 * immer was anderes anzeigt, als es eigentlich soll!
 * Und zwar soll es die Werte immer durch zwei Funktionen jagen...
 * ValueToShow() und ShowToValue().
 *
 * Dort geht dann entweder der LowerBorder ein (sprich es wird immer
 * der LowerBorder fuer die Anzeige addiert) oder aber es wird eine
 * Funktion verwendet, die eine Umwandlung Phy--Real Seitenzahlen macht.
 *
 */



static ULONG __stdargs dispatchMyMod( Class * cl, Object * o, Msg msg );
static int   setMyModAttrs( Class * cl, Object * o, struct opSet * msg );
static ULONG getMyModAttr( Class * cl, Object * o, struct opGet * msg );
static ULONG notifyAttrChanges( Object * o, void * ginfo, ULONG flags, ULONG tag1, ... );
static void  dumpTagList( UBYTE * str, struct TagItem * tags );


static ULONG __asm hookEntry( register __a0 struct Hook * h, 
			      register __a2 void * o, 
			      register __a1 void * msg )
{
  return ((*(ULONG (* __stdargs)())h->h_SubEntry)(h, o, msg));
}




Class	* initMyModClass(void)
{
    Class	*cl;

    if ( cl =  MakeClass( MYCLASSID, 
		SUPERCLASSID, NULL,		/* superclass is public      */
 		sizeof ( struct MyModData ),
		0 ))
    {
	/* initialize the cl_Dispatcher Hook	*/
	cl->cl_Dispatcher.h_Entry = (unsigned long (* )()) hookEntry;
	cl->cl_Dispatcher.h_SubEntry = (ULONG (* )()) dispatchMyMod;
	cl->cl_Dispatcher.h_Data = (VOID *) 0xFACE;	/* unused */
    }
    return ( cl );
}

int freeMyModClass( Class * cl )
{
    return ( FreeClass( cl )  );
}




static ULONG __stdargs dispatchMyMod( Class * cl, Object * o, Msg msg )
{
    Object	*newobj;
    LONG	oldval;
    ULONG	oldrng;
    LONG	oldlob;
    ULONG	notify_msg_flags = 0;
    ULONG	interim_flag;

    struct MyModData	*mmd;

    DK( printf("mymodel dispatcher, method ID %lx\n", msg->MethodID ) );

    mmd = INST_DATA( cl, o );

    switch ( msg->MethodID )
    {
    /* use superclass defaults for everything else */
    case OM_NEW:
	DK( printf("mymodel: OM_NEW\n") );
	if( newobj = (Object *) DoSuperMethodA( cl, o, msg ) )
	{
	    DK( printf("new model object at %lx\n", newobj ) );
	    /* initialize instance data (they start life as 0)	*/
	    mmd = INST_DATA( cl, newobj );
	    mmd->mmd_RegA4 = getreg(REG_A4);
	    setMyModAttrs( cl, newobj, (struct opSet *)msg );
	}
	return ( (ULONG) newobj );

    case OM_GET:
        putreg(REG_A4, mmd->mmd_RegA4);
	DK( printf("mymodel: OM_GET\n") );
	return ( (ULONG) getMyModAttr( cl, o, (struct opGet *)msg ) );

    case OM_SET:
    case OM_UPDATE:
        putreg(REG_A4, mmd->mmd_RegA4);
	DK( printf("mymod UPDATE/SET\n" ) );
	if ( ! DoSuperMethodA( cl, o, (Msg)ICM_CHECKLOOP ) ) 
	{
	    /* let the superclass see whatever it wants from OM_SET,
	     * such as ICA_TARGET, ICA_MAP, and so on.  For OM_NOTIFY,
	     * however, we control all traffic and issue notification
	     * specifically for the attributes we're interested in.
	     */
	    if ( msg->MethodID == OM_SET )
	    {
		DK( printf("mymod update is actually OM_SET\n"));
		DoSuperMethodA( cl, o, msg );
	    }
	    else
	    {
		/* these flags aren't present in the message of OM_SET	*/
		notify_msg_flags =  ((struct opUpdate *)msg)->opu_Flags;
	    }

	    /*
	     * I'll be wanting to know this is an "interim" message
	     * or a final report (which I always want to send, even
	     * if the value of mmd_CurrentValue hasn't changed).
	     */
	    interim_flag =  notify_msg_flags & OPUF_INTERIM;

	    /* Now set possibly new value of mmd_CurrentVal, and
	     * maybe a range change.
	     * Only send a notification message along for values of
	     * interest that have changed.
	     */

	    /* save 'em	*/
	    oldval = mmd->mmd_CurrentValue;
	    oldrng = mmd->mmd_Range;
	    oldlob = mmd->mmd_LowerBorder;

	    /* change 'em, only if changed (or if
	     * a "non-interim" message.
	     */
	    if ( setMyModAttrs( cl, o, (struct opSet *)msg ) || ! interim_flag )
	    {
	    	Tag	rangetag;
	    	Tag	currvaltag;
	    	Tag	lowbtag;

/* if condition is false, replace tag with TAG_IGNORE	*/
#define XTAG( expr, tagid ) ((expr)? (tagid): TAG_IGNORE)

		rangetag = (oldrng!=mmd->mmd_Range)? MYMODA_RANGE: TAG_IGNORE;
		if ( ! interim_flag || (oldval != mmd->mmd_CurrentValue) )
		{
		    currvaltag = MYMODA_CURRVAL;
		}
		else
		{
		    currvaltag = TAG_IGNORE;
		}
		
		lowbtag = (oldlob!=mmd->mmd_LowerBorder)? MYMODA_LOWERB: TAG_IGNORE;


		DK( printf("mymod: sending notification\n") );

		/* Pass along GInfo, if any, so gadgets can redraw
		 * themselves.  Pass along opu_Flags, so that the
		 * application will know the difference between
		 * and interim message and a final message
		 */
		notifyAttrChanges( o, ((struct opSet *)msg)->ops_GInfo,
			interim_flag,
			rangetag,	mmd->mmd_Range,
			currvaltag,	mmd->mmd_CurrentValue,
			lowbtag,	mmd->mmd_LowerBorder,
			TAG_END );
	    }
	    DK( else printf("setMyModAttrs returnes 'nochange'\n"));
	}
	DK( else printf("Loop Check violation!\n"));
	break;

    case OM_NOTIFY:
        putreg(REG_A4, mmd->mmd_RegA4);
    	DK( printf("mymod: forwarding OM_NOTIFY to superclass modelclass\n"));
    case OM_DISPOSE:
    default:
        putreg(REG_A4, mmd->mmd_RegA4);
	DK( printf("let superclass handle it\n"));
	return ( (ULONG) DoSuperMethodA( cl, o, msg ) );
    }
    return ( 1 );
}

static int setMyModAttrs( Class * cl, Object * o, struct opSet * msg )
{
    struct TagItem	*tags = msg->ops_AttrList;
    struct MyModData	*mmd;
    int			changes = FALSE;
    LONG		newval;
    LONG		newlob;
    ULONG		newrng;

    mmd = INST_DATA( cl, o );

    DK( printf("setMyModAttrs, object %lx\n", o ) );
    DK( dumpTagList( "mymod attr tags", tags ));

    newrng =  GetTagData( MYMODA_RANGE, mmd->mmd_Range, tags );

    if ( mmd->mmd_Range != newrng )
    {
	DK( printf( "mymod: range has changed value to %ld\n",
		mmd->mmd_Range ));
	mmd->mmd_Range =  newrng;
	changes = TRUE;
    }

    /* validity check	*/
    if ( mmd->mmd_Range == 0 )
    {
	mmd->mmd_Range = 1;
	changes = TRUE;
    }
    

    newlob = GetTagData( MYMODA_LOWERB, mmd->mmd_LowerBorder, tags );

    if ( mmd->mmd_LowerBorder != newlob )
    {
	DK( printf( "mymod: lower border has changed value to %ld\n",
		mmd->mmd_LowerBorder ));
	mmd->mmd_LowerBorder =  newlob;
	changes = TRUE;
    }


    DK( printf("lower border is %ld, range is %ld\n", mmd->mmd_LowerBorder, mmd->mmd_Range ) );

    /* start with original value	*/
    newval =  mmd->mmd_CurrentValue;

    DK( printf("original currval %ld\n", mmd->mmd_CurrentValue ) );

    /* increment/decrement in response to strobes	*/
    if ( GetTagData( MYMODA_INCRSTROBE, 0, tags ) > 0 )
    {
        ULONG secs, micros;
        ULONG diff;

	CurrentTime(&secs, &micros);
	
	diff = (secs - mmd->mmd_LastIncSecs) * 1000000 + micros - mmd->mmd_LastIncMicros;
	
	if (diff > 150000) {
	  mmd->mmd_LastIncSecs   = secs;
	  mmd->mmd_LastIncMicros = micros;
	  newval++;
	  DK( printf("strobe increment newval to %ld\n", newval ) );
	}
    }
    if ( GetTagData( MYMODA_DECRSTROBE, FALSE, tags ) > 0 )
    {
        ULONG secs, micros;
        ULONG diff;

	CurrentTime(&secs, &micros);
	
	diff = (secs - mmd->mmd_LastIncSecs) * 1000000 + micros - mmd->mmd_LastIncMicros;
	
	if (diff > 150000) {
	  mmd->mmd_LastIncSecs   = secs;
	  mmd->mmd_LastIncMicros = micros;
  	  if ( newval > 0) newval--;
	  DK( printf("strobe decrement newval to %ld\n", newval ) );
	}
    }

    /* look at "absolute" setting last	*/
    newval = GetTagData( MYMODA_CURRVAL, newval, tags );

    DK( printf("unconstrained newval %ld\n", newval ) );

    /* limit mmd_CurrentValue to mmd_Range-1	*/
    if ( newval < 0 ) newval = 0;
    if ( newval > (mmd->mmd_Range-1) )  newval = mmd->mmd_Range - 1;

    DK( printf( "final: new current value %ld\n", newval ));

    if ( mmd->mmd_CurrentValue != newval )
    {
    	mmd->mmd_CurrentValue = newval;
	DK( printf( "mymod: value has changed to %ld\n",
		mmd->mmd_CurrentValue ));
	changes = TRUE;
    }

    return ( changes );
}

static ULONG getMyModAttr( Class * cl, Object * o, struct opGet * msg )
{
    struct MyModData	*mmd;

    mmd = INST_DATA( cl, o );

    switch ( msg->opg_AttrID )
    {
    case MYMODA_CURRVAL:
	*msg->opg_Storage = mmd->mmd_CurrentValue;
	break;
    case MYMODA_RANGE:
	*msg->opg_Storage = mmd->mmd_Range;
	break;
    case MYMODA_LOWERB:
	*msg->opg_Storage = mmd->mmd_LowerBorder;
	break;
    default:
	/* I don't recognize this one, let the superclass try	*/
	return ( DoSuperMethodA( cl, o, (Msg)msg ) );
    }
    return ( TRUE );
}

/*
 * a convenient way to construct and send an OM_NOTIFY message
 */
static ULONG notifyAttrChanges( Object * o, void * ginfo, ULONG flags, ULONG tag1, ... )
{
    return ( DoMethod( o, OM_NOTIFY, &tag1, ginfo, flags ) );
}



/*
 * DEBUG
 */
static void dumpTagList( UBYTE * str, struct TagItem * tags )
{
    struct TagItem	*ti;
    struct TagItem	*NextTagItem();

    printf("\n");
    if ( str ) printf( "%s\n", str );
    printf("tags at %lx, prevtag %08lx - %08lx\n",
    	tags, tags[-1].ti_Tag, tags[-1].ti_Data );


    while ( ti = NextTagItem( &tags ) )
    {
	printf("%08lx - %08lx\n", ti->ti_Tag, ti->ti_Data );
    }
}
