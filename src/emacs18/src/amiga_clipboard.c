#include "config.h"
#undef NULL
#include "lisp.h"
#include "termchar.h"
#include "amiga.h"

#include <stdio.h>
#include <internal/devices.h>

#undef LONGBITS

#include <exec/types.h>
#include <exec/io.h>
#include <devices/clipboard.h>
#include <libraries/iffparse.h>
#include <utility/hooks.h>

#include <proto/exec.h>
#include <proto/iffparse.h>

#define  ID_FTXT	MAKE_ID('F','T','X','T')
#define  ID_CHRS	MAKE_ID('C','H','R','S')

/*
 * Text error messages for possible IFFERR_#? returns from various
 * IFF routines.  To get the index into this array, take your IFFERR code,
 * negate it, and subtract one.
 *  idx = -error - 1;
 */
static char *far ifferrormsgs[] = {
	"End of file (not an error).",
	"End of context (not an error).",
	"No lexical scope.",
	"Insufficient memory.",
	"Stream read error.",
	"Stream write error.",
	"Stream seek error.",
	"File is corrupt.",
	"IFF syntax error.",
	"Not an IFF file.",
	"Required call-back hook missing.",
	"Return to client.  You should never see this."
};

Lisp_Object amiga_new_clip;
static struct IFFHandle *far iff;
struct Library *IFFParseBase;

static struct IOClipReq *far ClipRequest;
static struct Hook cliphook;

/* added __interrupt flag this disables stack checking for this function
 * so we can compile with stack checking on.  -ch3/19/93. */
static ULONG __saveds __asm __interrupt
clip_change(	register __a0 struct Hook	*hook,
		register __a2 VOID		*object,
		register __a1 ULONG		*message )
{
    amiga_new_clip = 1;
    return 0;
}

static Lisp_Object clip_unwind(Lisp_Object dummy)
{
    CloseIFF (iff);
    CloseClipboard ((struct ClipboardHandle *) iff->iff_Stream);

    return Qnil;
}

static int clip_protect(void)
{
    int count = specpdl_ptr - specpdl;

    record_unwind_protect(clip_unwind, Qnil);

    return count;
}

static long clip_check(long err)
{
    if(err) error ("Clipboard IO failed, error %ld: %s\n",
		   err, ifferrormsgs[-err - 1]);
    return err;
}


static void cut(char *str, int size)
{
    int count;

    if (!(iff->iff_Stream = (ULONG) OpenClipboard (0)))
	error ("Clipboard open failed.");

    count = clip_protect();

    /* Open clipbaord */
    InitIFFasClip (iff);
    clip_check(OpenIFF (iff, IFFF_WRITE));

    /* Write data */
    clip_check(PushChunk(iff, ID_FTXT, ID_FORM, IFFSIZE_UNKNOWN));
    clip_check(PushChunk(iff, 0, ID_CHRS, IFFSIZE_UNKNOWN));
    if (WriteChunkBytes(iff, str, size) != size) clip_check(IFFERR_WRITE);
    clip_check(PopChunk(iff));
    clip_check(PopChunk(iff));

    /* & close */
    unbind_to (count);
}

DEFUN ("amiga-cut", Famiga_cut, Samiga_cut,
  1, 1, 0,
  "Copy string into Amiga clipboard.")
  (arg)
     Lisp_Object arg;
{
    struct Lisp_String *p;

    CHECK_STRING (arg, 0);

    p = XSTRING (arg);
    cut(p->data, p->size);

    return Qnil;
}

DEFUN ("amiga-paste", Famiga_paste, Samiga_paste,
  0, 0, 0,
  "Returns text currently in the Amiga clipboard, or NIL if there is none.")
  ()
{
    long err = 0;
    Lisp_Object result = Qnil;
    struct ContextNode  *cn;
    int count;

    if (!(iff->iff_Stream = (ULONG) OpenClipboard (0)))
	error ("Clipboard open failed.");

    count = clip_protect();

    /* Open clipbaord */
    InitIFFasClip (iff);
    clip_check(OpenIFF (iff, IFFF_READ));
    clip_check(StopChunk(iff, ID_FTXT, ID_CHRS));

    /* Find the first FTXT CHRS chunks */
    while (result == Qnil)
    {
	long err = ParseIFF(iff, IFFPARSE_SCAN);

	if (err == IFFERR_EOC) continue; /* enter next context */
	else if (err == IFFERR_EOF) break;
	else clip_check(err);

	/* We only asked to stop at FTXT CHRS chunks
	 * If no error we've hit a stop chunk
	 * Read the CHRS chunk data
	 */
	cn = CurrentChunk(iff);

	if ((cn) && (cn->cn_Type == ID_FTXT) && (cn->cn_ID == ID_CHRS))
	{
	    int size = cn->cn_Size, rlen;

	    result = make_string("", size);

	    if ((rlen = ReadChunkBytes(iff, XSTRING (result)->data, size)) != size)
		if (rlen < 0) clip_check(rlen);
		else clip_check(IFFERR_EOC);
	}
    }
    unbind_to (count);

    return result;
}

void syms_of_amiga_clipboard(void)
{
    DEFVAR_BOOL ("amiga-new-clip", &amiga_new_clip,
		 "Set to t every time a new clip is put in the Amiga clipboard");
    amiga_new_clip = 0;

    defsubr (&Samiga_cut);
    defsubr (&Samiga_paste);
}

void early_clipboard(void)
{
    IFFParseBase = 0;
}

void init_clipboard(void)
{
    /* Initialise IFF for clipboard */
    if (!(IFFParseBase = OpenLibrary("iffparse.library", 0)))
      _fail("iffparse.library is required");
    if (!(iff = AllocIFF())) no_memory();

    ClipRequest = (struct IOClipReq *)
	_device_open("clipboard.device", 0L, 0L, 0L, 0, sizeof(struct IOClipReq));
    if (!ClipRequest) _fail("clipboard.device missing !?");

    cliphook.h_Entry = (ULONG (*)())clip_change;
    ClipRequest->io_Command = CBD_CHANGEHOOK;
    ClipRequest->io_Length = 1; /* install */
    ClipRequest->io_Data = (APTR)&cliphook;
    DoIO((struct IORequest *)ClipRequest);
}

void cleanup_clipboard(void)
{
    if (ClipRequest)
    {
	cliphook.h_Entry = (ULONG (*)())clip_change;
	ClipRequest->io_Command = CBD_CHANGEHOOK;
	ClipRequest->io_Length = 0; /* remove */
	ClipRequest->io_Data = (APTR)&cliphook;
	DoIO((struct IORequest *)ClipRequest);
    }
    if (iff) FreeIFF(iff);
    if (IFFParseBase) CloseLibrary(IFFParseBase);
    _device_close((struct IORequest *)ClipRequest);
}
