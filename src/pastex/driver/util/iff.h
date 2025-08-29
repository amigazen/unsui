#ifndef LIBRARIES_IFF_H
#define LIBRARIES_IFF_H

/* C include file for the iff.library V18.4, 28-Feb-90 by Chris Weber.
   Should work for Lattice and Manx. */

#ifndef EXEC_TYPES_H
#include <exec/types.h>
#endif

#define IFFNAME "iff.library"
#define IFFVERSION 18L				/* Current library version */

typedef void *IFFFILE;				/* The IFF 'FileHandle' structure */


/************** FUNCTION DECLARATIONS ***********************************/

#ifdef OLD_AZTEC_C
#define __ARGS(a) ()
#define NO_PRAGMAS
#else
#define __ARGS(a) a
#endif

IFFFILE	OpenIFF      __ARGS((char *));
void    CloseIFF     __ARGS((IFFFILE));
void   *FindChunk    __ARGS((IFFFILE,LONG));	/* Was struct Chunk * */
struct BitMapHeader *GetBMHD __ARGS((IFFFILE));
LONG    GetColorTab  __ARGS((IFFFILE,WORD *));
BOOL    DecodePic    __ARGS((IFFFILE,struct BitMap *));
BOOL    SaveBitMap   __ARGS((char *,struct BitMap *,WORD *,LONG));
BOOL    SaveClip     __ARGS((char *,struct BitMap *,WORD *,LONG,LONG,LONG,LONG,LONG));
LONG    IFFError     __ARGS((void));
ULONG   GetViewModes __ARGS((IFFFILE));			/* ULONG since V18.1 */
APTR    NewOpenIFF   __ARGS((char *,LONG));				/* Since V16.1 */
BOOL    ModifyFrame  __ARGS((void *,struct BitMap *));	/* Since V18.1 */


/************** ERROR-CODES *********************************************/

#define IFF_BADTASK -1              /* IFFError() called by wrong task */

#define IFF_CANTOPENFILE 16         /* File not found */
#define IFF_READERROR 17            /* Error reading file */
#define IFF_NOMEM 18                /* Not enough memory */
#define IFF_NOTIFF 19               /* File is not an IFF file */
#define IFF_WRITEERROR 20           /* Error writing file */

#define IFF_NOILBM 24               /* IFF file is not of type ILBM */
#define IFF_NOBMHD 25               /* BMHD chunk not found */
#define IFF_NOBODY 26               /* BODY chunk not found */
#define IFF_TOOMANYPLANES 27        /* BODY has more planes than BitMap */
#define IFF_UNKNOWNCOMPRESSION 28   /* Unknown compression type */

#define IFF_NOANHD 29				/* ANHD chunk not found (since V18.1) */
#define IFF_NODLTA 30				/* DLTA chunk not found (since V18.1) */


/************** COMMON IFF IDs ******************************************/

#define MakeID(a,b,c,d) ((ULONG)(a)<<24L|(ULONG)(b)<<16L|(c)<<8|(d))

/* List of the most useful IDs, NOT complete (to be continued sometimes...) */

#define ID_FORM MakeID('F','O','R','M')
#define ID_PROP MakeID('P','R','O','P')
#define ID_LIST MakeID('L','I','S','T')
#define ID_CAT  MakeID('C','A','T',' ')

#define ID_ANIM MakeID('A','N','I','M')
#define ID_ANHD MakeID('A','N','H','D')
#define ID_ILBM MakeID('I','L','B','M')
#define ID_BMHD MakeID('B','M','H','D')
#define ID_BODY MakeID('B','O','D','Y')
#define ID_CAMG MakeID('C','A','M','G')
#define ID_CLUT MakeID('C','L','U','T')
#define ID_CMAP MakeID('C','M','A','P')
#define ID_CRNG MakeID('C','R','N','G')
#define ID_DLTA MakeID('D','L','T','A')
#define ID_SHAM MakeID('S','H','A','M')

#define ID_8SVX MakeID('8','S','V','X')
#define ID_ATAK MakeID('A','T','A','K')
#define ID_NAME MakeID('N','A','M','E')
#define ID_RLSE MakeID('R','L','S','E')
#define ID_VHDR MakeID('V','H','D','R')

#define FORM ID_FORM	/* Ancient compatibility only, don't use */
#define PROP ID_PROP
#define LIST ID_LIST
#define CAT  ID_CAT


/************** STRUCTURES **********************************************/

struct Chunk			/* Generic IFF chunk structure */
{
	LONG  ckID;
	LONG  ckSize;
	/* UBYTE ckData[1];	should be UBYTE ckData[ckSize] */
};

struct BitMapHeader		/* BMHD chunk for ILBM files */
{
	UWORD w,h;
	WORD  x,y;
	UBYTE nPlanes;
	UBYTE masking;
	UBYTE compression;
	UBYTE pad1;
	UWORD transparentColor;
	UBYTE xAspect,yAspect;
	WORD  pageWidth,pageHeight;
};

struct AnimHeader		/* ANHD chunk for ANIM files */
{
	UBYTE	Operation;
	UBYTE	Mask;
	UWORD	W;
	UWORD	H;
	WORD	X;
	WORD	Y;
	ULONG	AbsTime;
	ULONG	RelTime;
	UBYTE	Interleave;
	UBYTE	pad0;
	ULONG	Bits;
	UBYTE	pad[16];
};


/************** PRAGMAS FOR LATTICE C V5.x ******************************/

/* Pragmas generated with: 'fd2pragma iff_lib.fd iffpragmas.h' */

#ifndef NO_PRAGMAS
extern struct Library *IFFBase;
#pragma libcall IFFBase OpenIFF 1e 801
#pragma libcall IFFBase CloseIFF 24 901
#pragma libcall IFFBase FindChunk 2a 902
#pragma libcall IFFBase GetBMHD 30 901
#pragma libcall IFFBase GetColorTab 36 8902
#pragma libcall IFFBase DecodePic 3c 8902
#pragma libcall IFFBase SaveBitMap 42 a9804
/*#pragma libcall IFFBase SaveClip 48 210a9808*/
#pragma libcall IFFBase IFFError 4e 0
#pragma libcall IFFBase GetViewModes 54 901
#pragma libcall IFFBase NewOpenIFF 5a 802
#pragma libcall IFFBase ModifyFrame 60 8902
#endif

#endif /* !LIBRARIES_IFF_H */

