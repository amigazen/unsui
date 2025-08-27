/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heﬂmann
*/

enum	{	GAD_TRANSFER=1000,GAD_RENDER,GAD_INVERT,GAD_BASEDPI,GAD_MSGLIST,GAD_JUMP,GAD_SHOW,GAD_CLEAR,

		MEN_OPEN,MEN_SAVE,MEN_PUBSCREEN,MEN_ABOUT,MEN_QUIT
	};

enum	{	CYID_Transfer_Memory, CYID_Transfer_Disk, CYID_Transfer_None };
enum	{	CYID_Render_None, CYID_Render_Frame, CYID_Render_Clear };

enum	{	ERR_NO_INTUITION=1000,ERR_NO_GRAPHICS,ERR_NO_GADTOOLS,ERR_NO_ICON,
		ERR_NO_IFFPARSE,ERR_NO_UTILITY,ERR_NO_ASL,ERR_NO_MUI,ERR_NO_POOL,
		ERR_NO_GUI,ERR_ALREADY_RUNNING,ERR_NO_PORT,ERR_NO_MATHFFP,ERR_NO_MATHTRANS,

		ERR_READ_ERROR,ERR_NO_MEM,ERR_FILE_FORMAT_ERROR,ERR_WEIRD_COMPRESSION,
		ERR_WRONG_IMAGE_FORMAT,ERR_TOO_LARGE,ERR_TOO_SMALL,

		ERR_NO_POST,ERR_NO_NIL,
	};

#define BMHDB_CMAPOK	7
#define BMHDF_CMAPOK	(1 << BMHDB_CMAPOK)

#define MAX_FILENAME_LEN 256

#define PORTMASK(p)	(1L << ((struct MsgPort *)(p)) -> mp_SigBit)

struct ListEntry
{
	struct MinNode		 MinNode;
	STRPTR			 Title;
};

struct GreyImage
{
	LONG			 Width,
				 Height,
				 Index;
	UBYTE			**Lines;
	APTR			 Pool;
};

struct MapTable
{
	STRPTR			 Key;
	LONG			 ID;
};

extern struct ExecBase		*SysBase;
extern struct DosLibrary	*DOSBase;

extern struct IntuitionBase	*IntuitionBase;
extern struct GfxBase		*GfxBase;
extern struct Library		*IconBase,
				*GadToolsBase,
				*IFFParseBase,
				*UtilityBase,
				*AslBase,
/*
				*MathBase,
				*MathTransBase,
*/

				*MathIeeeDoubBasBase,
				*MathIeeeDoubTransBase,

				*MUIMasterBase,
				*DataTypesBase;

extern struct Process		*ThisProcess;
extern APTR			 OldPtr;

extern APTR			 Pool;

extern struct List		 MessageList;

extern struct MsgPort		*MainPort;

extern UBYTE			 Filter[256];

extern APTR			 AP_Application,
				 WI_Main,
				 CY_Transfer,
				 CY_Render,
				 CM_Invert,
				 ST_BaseDPI,
				 LV_Messages,
				 GA_Gauge,
				 BT_Jump,
				 BT_Show,
				 BT_Clear;

extern struct Screen		*PubScreen;

extern struct Process		*Father;
extern BOOL			 InvertChanged,
				 UseGUI;

extern struct config_struct	 Configuration;
extern struct NewMenu		 MenuTemplate[];

extern STRPTR			 CYA_Transfer[],
				 CYA_Render[];

extern UBYTE			 OrderedBckBrick[256],
				 OrderedFwdBrick[256],
				 OrderedHalftone[256],
				 OrderedVanilla[256],
				 OrderedHexagonalCluster[256],
				 OrderedSpiralDot[256],
				 OrderedHorizontal[256],
				 OrderedBckBrick8[256],
				 OrderedBckBrick4[256],
				 OrderedFwdBrick8[256],
				 OrderedFwdBrick4[256],
				 OrderedHalftone8[256],
				 OrderedHalftone4[256],
				 OrderedSpiralDot8[256],
				 OrderedSpiralDot4[256];

extern VOID			(* __regargs WriteLine)(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp);
extern VOID			(* __regargs ReadLine)(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp);

extern struct MapTable		 TransferTable[],
				 RenderTable[];

	/* Luminance.asm */

extern LONG			 LumR[256],
				 LumG[256],
				 LumB[256];
