/*  io.c */

/* All of the code in this file is Amiga-specific */

#include "less.h"

#include <ctype.h>
#include <string.h>
#include <intuition/intuition.h>
#include <dos.h>
#include <devices/conunit.h>

#include <intuition/intuitionbase.h>
extern struct IntuitionBase *IntuitionBase;
extern int sigs;

static struct FileHandle *ttyHandle;
static struct MsgPort *ttyPort; /* Port to send DOS Packets to */
static struct MsgPort *RdReplyPort; /* When a tty read completes */
static BPTR ConsoleHandle = 0;
static struct StandardPacket *RdPacket = NULL; /* Packet for read requests */
static struct ConUnit *conUnit; /* For resizing information */

public BOOL UseCLI = FALSE;	/* Has user requested we use CLI window? */

public int     nrow = 0;         /* Terminal size, rows.         */
public int     ncol;             /* Terminal size, columns.      */


/* Following function is required by MySprintf */
extern __asm void stuffChar
	( register __d0 char c, register __a3 char *SPFbuffer );

int Wind_Spec[4] = { 0, 0, 0, 0 };
/* User-supplied option specifying window size/position:
 *
 * [0]: Left edge X - coord.
 * [1]: Top edge Y - coord.
 * [2]: Right edge X - coord.
 * [3]: Bottom edge Y - coord.
 *
 * If element 2 or 3 is negative, it is taken to be a relative value to be
 * subtracted from the maximum screen size.  If the resulting window
 * would be smaller than the minimum, it is quietly enlarged to the
 * minimum, by expanding to the lower right as far as possible, and then
 * moving the window toward the upper left if necessary.
 */
#define W_MINWIDTH 200
#define W_MINHEIGHT 60

/* Prototypes for functions defined in io.c */

static void StartConRead __PROTO((void));
static int ConGetC __PROTO((void));
static int ConLookC __PROTO((void));
static void ConWrite __PROTO((char *data, int length));
static LONG findWindow ( void );
static void MySendPkt __PROTO((struct StandardPacket *pkt, struct MsgPort *RPort,
	long action, long PktArg[], long Nargs));
static LONG MyDoPkt __PROTO(( long action, long PktArg[], long Nargs ));


#include <exec/memory.h>

extern struct GfxBase *GfxBase;
extern struct MsgPort *CreatePort();

static char buffer; /* buffer for incoming console character */

/* asynchronous console read request--must be called once before
   any ConGetC requests
*/

static void StartConRead ( void )
{
    LONG PktArg[3];

	PktArg[0] = (LONG)(ttyHandle->fh_Arg1);
	PktArg[1] = (LONG)&buffer;
	PktArg[2] = 1;
	/* asynchronous posting of a read request */
	MySendPkt( RdPacket, RdReplyPort, ACTION_READ, PktArg, 3 );
}

/* Get a character from the console.
*/
static int ConGetC ()
{
    struct MsgPort *mp;
    struct StandardPacket *rddata;
    int temp;
    ULONG ReadMsgBit, MsgBits;

    mp = RdReplyPort;    /* get the read reply port */
    ReadMsgBit = 1 << mp->mp_SigBit;
    rddata = NULL;
    do /* Wait for a character or ^C */
    {
        MsgBits = Wait(ReadMsgBit | SIGBREAKF_CTRL_C);
        if ( MsgBits & ReadMsgBit )
            rddata = (struct StandardPacket *) GetMsg ( mp );
        else if ( MsgBits & SIGBREAKF_CTRL_C )
        {
            quit();
        }
    } while ( !rddata );

    /* We've got a character... */
    temp = buffer;   /* get the character */
    StartConRead ( ); /* set up next read */
    return temp;
}

/* See if a character is available at the console.
   If so, get it, else return -1.
*/
static int ConLookC ( void )
{
    struct MsgPort *mp;
    struct StandardPacket *rddata;
    int temp;

    mp = RdReplyPort;    /* get the read reply port */
    rddata = (struct StandardPacket *) GetMsg ( mp );
    if ( !rddata ) return -1;

    /* We've got a character... */
    temp = buffer;   /* get the character */
    StartConRead ( ); /* set up next read */
    return temp;
}

/* write a specified number of characters from a buffer to console device
*/
static void ConWrite ( char *data, int length )
{
	Write ( ConsoleHandle, data, length );
}


/*
 * This routine gets called once, to set up the
 * terminal channel.
 *
 * See The AmigaDOS Manual for details of how to derive parameters
 * from an AmigaDOS filehandle and for doing console packet IO.
 * The sample code on Fish Disk 56 is also instructive.
 */
void ttopen (void)
{
        int left, top, width, height;   /* window specification */
        struct Screen *wbsdata;
        long PktArg[1];
        static char LessWindowSpec[132];

		if ( ConsoleHandle ) return;

		/* Under AmigaDos 1.3, one can't seem to open * with MODE_READWRITE */
		if ( !called_from_WB && UseCLI )
			ConsoleHandle = Open ( "*", MODE_OLDFILE );
		if ( ConsoleHandle == 0 )
		{
        	wbsdata = MyFindWB();
        	if ( (left = Wind_Spec[0]) < 0 ) left += wbsdata->Width;
        	if ( left < 0 ) left = 0;
        	if ( left > wbsdata->Width )
            	left = wbsdata->Width - W_MINWIDTH;
        	if ( (top = Wind_Spec[1]) < 0 ) top += wbsdata->Height;
        	if ( top < 0 ) top = 0;
        	if ( top > wbsdata->Height )
            	top = wbsdata->Height - W_MINHEIGHT;

        	width = Wind_Spec[2];
        	if ( width <= 0 )
            	width += wbsdata->Width;
        	if ( width <= 0 )
            	width = 0;
        	height = Wind_Spec[3];
        	if ( height <= 0 )
            	height += wbsdata->Height;
        	if ( height <= 0 )
            	height = 0;

        	if ( width < W_MINWIDTH )
            	width = W_MINWIDTH;
        	if ( height < W_MINHEIGHT )
            	height = W_MINHEIGHT;

        	if ( left + width > wbsdata->Width )
            	width = wbsdata->Width - left;
        	if ( top + height > wbsdata->Height )
            	height = wbsdata->Height - top;
        	if ( width < W_MINWIDTH )
            	left = wbsdata->Width - W_MINWIDTH;
        	if ( height < W_MINHEIGHT )
            	top = wbsdata->Height - W_MINHEIGHT;
        	if ( left < 0 || top < 0 )
        	{
				MyFreeWB( wbsdata );
            	error ( "Window won't fit on screen" );
            	quit();
        	}

			MyFreeWB(wbsdata);
			MySprintf ( LessWindowSpec,
				"CON:%ld/%ld/%ld/%ld/%s: h for help", left, top,
				width, height, Ver );
			if ( IsV2 ) strcat ( LessWindowSpec, "/CLOSE" );
			ConsoleHandle = Open ( LessWindowSpec, MODE_OLDFILE );
		}

        if ( ConsoleHandle == 0 )
        {
            PrintFault ( IoErr(), "Can't open Less window" );
            quit();
        }
        ttyHandle = (struct FileHandle *)(BADDR(ConsoleHandle));
        ttyPort = ttyHandle->fh_Type;
        if ( !ttyPort )
        {
        	error ( "NULL ttyPort" );
        	quit();
        }
        RdReplyPort = CreatePort ( NULL, 0 );
		RdPacket = (struct StandardPacket *)
			AllocMem ( sizeof(struct StandardPacket), MEMF_PUBLIC | MEMF_CLEAR );
		if ( !RdReplyPort || !RdPacket )
		{
        	error ( "Out of Memory" );
			quit();
		}
		findWindow();

        StartConRead ( );
        /* enable report of window resizeing and close gadget */
    	PktArg[0] = 1;	/* set raw mode */
    	if ( ! MyDoPkt ( ACTION_SCREEN_MODE, PktArg, 1 ) )
    	{
    		error ( "Can't set raw mode" );
    		quit();
    	}
    	/* Enable reports; translate \n to \n\r for aux: */
        ConWrite( "\x1b[12;11{\x1b[20h", 13);
}

struct Screen *MyFindWB ( void )
{
    static struct Screen __aligned WBScreen, *wbsdata;

    if ( IsV2 )
        wbsdata = LockPubScreen("Workbench");
    else
    {
        if ( !GetScreenData ( &WBScreen, sizeof(struct Screen),
            WBENCHSCREEN, NULL ) )
            wbsdata = NULL;
        else
        	wbsdata = &WBScreen;
    }
    if ( !wbsdata )
    {
        error ( "Can't find Workbench screen" );
        quit();
    }
    return wbsdata;
}

void MyFreeWB ( struct Screen *wbsdata )
{
    if (IsV2)
        UnlockPubScreen(NULL, wbsdata);
}

/* Request size of window information.
   Modelled after sample code by C. Scheppner et al. on Fish Disk 56.
*/
void getrowcol (void)
{
    if ( ConsoleHandle == 0 ) ttopen();
    if ( conUnit )
    {
		/* 1.3 needs something done in the window so cu_* becomes  valid */
		lower_left();

		nrow = conUnit->cu_YMax + 1;
		ncol = conUnit->cu_XMax + 1;
	}
	else	/* for instance, if we're running aux: */
	{
		/* make something up... */
		nrow = 24;
		ncol = 80;
	}
}


/*
 * This function gets called just
 * before we go back home to the command interpreter.
 * On the Amiga it closes up the virtual terminal window.
 */
void ttclose (void)
{
	long PktArg[1];
   	char ch;

    /* disable raw mode report of window resizeing and close gadget */
    if ( ConsoleHandle )
    {
    	lower_left();
    	clear_eol();

    	/* Don't leave the console handing with an unfulfilled async
    	   read request.  We cancel it by forcing the console to send
    	   us some more-or-less known characters, and not renewing the
    	   read request when we've seen the last one.
    	*/
    	ConWrite( "\x1b[6n", 4 ); /* request device status */
    	while ( 1 )
    	{
    		WaitPort ( RdReplyPort );
        	while ( ! GetMsg ( RdReplyPort ) ) /* nothing */;
        	ch = buffer;
        	if ( ch == 'R' ) break;
        	StartConRead();
        }

    	/* Disable window sizing reporting and raw mode.  This only
    	   matters if we are running in a borrowed CLI window.  We
    	   leave the close gadget activated, since if there is a close
    	   gadget is was probably activated before we were called.
    	*/
    	ConWrite( "\x1b[12}", 5);
    	PktArg[0] = 0; /* Turn off raw mode */
    	MyDoPkt ( ACTION_SCREEN_MODE, PktArg, 1 );
    	Close ( ConsoleHandle );
    	ConsoleHandle = 0;
    	if ( RdReplyPort ) DeletePort ( RdReplyPort );
    	RdReplyPort = 0L;
    }
    if ( RdPacket ) FreeMem ( RdPacket, sizeof (struct StandardPacket) );
    RdPacket = NULL;
    nrow = 0;
}


/*
 * Read a character from the terminal,
 * performing no editing and doing conditional echo
 * but interpretting window resize and close gadget reports
 */
int do_echo = 1; /* echo flag */

int ttgetc (void)
{
        unsigned char c;        /* must be unsigned! */


        while ( (c = ConGetC()) == '\x9b' )
        {
            switch (c = ConGetC())
            {
            case '1': /* raw input event */
                if ( (c = ConGetC()) == '1' )
                    quit();  /* close gadget */
                else /*if ( c == '2' )  /* window resize */
                {
                    while ( (c = ConGetC()) != '|') /* nothing */;
                    winch();
                }
                break;
            case '?': /* Help key */
                if ( (c = ConGetC()) == '~' ) return 'h';
                break;
            case 'A':
            case 'T': /* Arrow up */
                c = 'b';
                break;
            case 'B':
            case 'S': /* Arrow down */
                c = ' ';
                break;
            case 'D': /* Arrow left */
                c = 'k';
                break;
            case 'C': /* Arrow right */
                c = '\r';
                break;
            case ' ': /* Shifted left or right */
                if ( (c = ConGetC()) == 'A' ) c = 'u';
                else c = 'd';
                break;
            default:
                continue;
            }
            break;
        }
        if ( c == 3 )
        {
            sigs |= S_INTERRUPT;
            psignals();
            /* no return */
        }
        if (do_echo)
                ttwrite(&c, 1);

        return ((int) c);
}

/*
 * Check for ^C in the tty window.  This routine will throw
 * away any characters waiting in the tty input buffer.  Returns when
 * there's nothing in the input queue or one of the following has been
 * recognized:
 *
 *  Close gadget    (exits)
 *  Window resize   (resets window and returns)
 *  ^C              (sets sigs and returns)
 */
int chk_sigs (void)
{
    int c;

    for (;;)
    {
        if ( (c = ConLookC()) < 0 ) return sigs;

        switch ( c )
        {
        case '\x9b': /* raw input event */
            if ( (c = ConGetC()) != '1' ) break; /* unexpected raw input */
            if ( (c = ConGetC()) == '1' )
                quit();  /* close gadget */
            else if ( c == '2' )  /* window resize */
            {
                while ( (c = ConGetC()) != '|') /* nothing */;
                winch();
                return sigs;
            }
            break;
        case 3:
            sigs |= S_INTERRUPT;
            return sigs;
        }
    }
}



/*
 * Write a buffer of characters to the display.
 */
void ttwrite (char *buffer, int length)
{
        ConWrite ( buffer, length );
}

/*
 * Write a string to the terminal
 */
void ttputs (char *s)
{
        ConWrite ( s, strlen(s) );
}

/* fake termcap output */
/* This takes the place of the original tputs(x,y,z), using only the
   first parameter
 */
void Tputs (char *s)
{
        flush();
        if ( s ) ConWrite ( s, strlen(s) );
}


/* An sprintf replacement that uses exec.library, so is much smaller that
   including the more general SAS link library routine.
*/
__stdargs void MySprintf ( char *buffer, char *fmt, ... )
{
	RawDoFmt ( fmt, (APTR)(&fmt + 1), stuffChar, (APTR)buffer );
}

/* The following code is adapted from C. Schepner's sample code on
   Fish Disk 56.
   Inits conUnit (global var), assuming ttyPort has been
   set to the message port of a console.
*/
static LONG findWindow ( void )
{
	struct InfoData *id;
	LONG myargs[1] ,nargs, res1;

	/* Alloc to insure longword alignment */
	id = (struct InfoData *)AllocMem(sizeof(struct InfoData),
		MEMF_PUBLIC|MEMF_CLEAR);
	if(! id) return(0);

	nargs = 0;
	(void) MyDoPkt ( ACTION_FLUSH, myargs, nargs );

	myargs[0] = MKBADDR(id);
	nargs = 1;
	res1 = MyDoPkt ( ACTION_DISK_INFO, myargs, nargs );
	conUnit = (struct ConUnit *)
		((struct IOStdReq *)id->id_InUse)->io_Unit;
	FreeMem ( id, sizeof(struct InfoData) );
	return res1;
}

static void MySendPkt ( struct StandardPacket *pkt, struct MsgPort *RPort,
	long action, long PktArg[], long Nargs )
{
	int count;
	long *pargs;

	if ( !ttyPort ) quit();
	pkt->sp_Msg.mn_Node.ln_Name = (char *)&(pkt->sp_Pkt);
	pkt->sp_Pkt.dp_Link = &(pkt->sp_Msg);
	pkt->sp_Pkt.dp_Port = RPort;
	pkt->sp_Pkt.dp_Type = action;
	pargs = &(pkt->sp_Pkt.dp_Arg1);
	for ( count = 0; count < Nargs; count++ )
	{
		pargs[count] = PktArg[count];
	}
	PutMsg ( ttyPort, (struct Message *)pkt );
}

static LONG MyDoPkt ( long action, long PktArg[], long Nargs )
{
	struct MsgPort *ReplyPort;
	struct StandardPacket *packet;
	LONG result;

	if ( !ttyPort ) quit();
	if ( !(ReplyPort = CreatePort ( NULL, 0 )) )
		quit();
	packet = (struct StandardPacket *)AllocMem ( sizeof (struct StandardPacket),
		MEMF_PUBLIC | MEMF_CLEAR );
	if ( !packet )
	{
		DeletePort ( ReplyPort );
		quit ();
	}
    MySendPkt ( packet, ReplyPort, action, PktArg, Nargs );
	WaitPort ( ReplyPort );
	GetMsg ( ReplyPort );
	result = packet->sp_Pkt.dp_Res1;
	FreeMem ( packet, sizeof(struct StandardPacket) );
	DeletePort ( ReplyPort );
	return result;
}
