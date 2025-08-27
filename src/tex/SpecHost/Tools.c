/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heßmann
*/

/*
** Tools.c
**
** Latest revision: 1 Jun 1995, by Giuseppe Ghibò
*/

#include "Global.h"

/* (ghi) BitMapTrack is a useful structure which allows to associate a
   boolean information with a BitMap structure. */

struct BitMapTrack {
	struct BitMapTrack *next;
	struct BitMapTrack *prev;
	struct BitMap *bmap;
	BOOL IsAllocBitMapped;
};

struct BitMapTrack *bmt = NULL;

STATIC VOID __regargs Add_BitMapTrack(struct BitMap *bmap, BOOL status);
STATIC VOID __regargs Del_BitMapTrack(struct BitMap *bmap);
STATIC BOOL __regargs Is_AllocBitMapped(struct BitMap *bmap);
STATIC struct BitMapTrack * __regargs Find_BitMapTrack(struct BitMap *bmap);

VOID __regargs
FreeVecPooled(APTR Mem)
{
	if(Mem)
	{
		ULONG *Data = Mem;

		LibFreePooled(Pool,&Data[-1],Data[-1]);
	}
}

APTR __regargs
AllocVecPooled(LONG Size,ULONG Flags)
{
	if(Size)
	{
		ULONG *Data;

		Size += sizeof(ULONG);

		if(Data = (ULONG *)LibAllocPooled(Pool,Size))
		{
			*Data++ = Size;

			if(Flags & MEMF_CLEAR)
				memset(Data,0,Size - sizeof(ULONG));

			return((APTR)Data);
		}
	}

	return(NULL);
}

VOID __regargs
ClearList(struct List *List)
{
	struct Node	*Node,
			*Next;

	Node = List -> lh_Head;

	while(Next = Node -> ln_Succ)
	{
		FreeVecPooled(Node);

		Node = Next;
	}

	NewList(List);
}

struct ListEntry * __regargs
NewEntry(STRPTR Title)
{
	struct ListEntry *Entry;

	if(Entry = (struct ListEntry *)AllocVecPooled(sizeof(struct ListEntry) + strlen(Title),MEMF_ANY))
	{
		Entry -> Title = (STRPTR)(Entry + 1);

		strcpy(Entry -> Title,Title);
	}

	return(Entry);
}

WORD __stdargs
ShowRequest(struct Window *Window,STRPTR Text,STRPTR Gadgets,...)
{
	WORD Result;

	if(AP_Application)
	{
		va_list VarArgs;

		va_start(VarArgs,Gadgets);
		Result = MUI_RequestA(AP_Application,WI_Main,NULL,"SpecialHost",Gadgets,Text,VarArgs);
		va_end(VarArgs);
	}
	else
	{
		struct EasyStruct	Easy;
		ULONG			IDCMP = NULL;
		va_list	 		VarArgs;

		Easy . es_StructSize	= sizeof(struct EasyStruct);
		Easy . es_Flags		= NULL;
		Easy . es_Title		= "SpecialHost";
		Easy . es_TextFormat	= Text;
		Easy . es_GadgetFormat	= Gadgets;

		va_start(VarArgs,Gadgets);
		Result = EasyRequestArgs(Window,&Easy,&IDCMP,VarArgs);
		va_end(VarArgs);
	}

	return(Result);
}

STRPTR __regargs
ShowError(LONG Primary,LONG Secondary,BOOL GetPrimary)
{
	STATIC struct { LONG Code; STRPTR Name; } LocalErrors[] =
	{
		ERR_NO_INTUITION,		"Error opening intuition.library v37",
		ERR_NO_GRAPHICS,		"Error opening graphics.library v37",
		ERR_NO_GADTOOLS,		"Error opening gadtools.library v37",
		ERR_NO_ICON,			"Error opening icon.library v37",
		ERR_NO_IFFPARSE,		"Error opening iffparse.library v37",
		ERR_NO_UTILITY,			"Error opening utility.library v37",
		ERR_NO_ASL,			"Error opening asl.library v37",
		ERR_NO_MUI,			"Error opening muimaster.library",
		ERR_NO_MATHFFP,			"Error opening mathieeedoubbas.library",
		ERR_NO_MATHTRANS,		"Error opening mathieeedoubtrans.library",
		ERR_NO_POOL,			"Error creating memory pool",
		ERR_NO_GUI,			"Error opening window",
		ERR_ALREADY_RUNNING,		"SpecialHost process already running",
		ERR_NO_PORT,			"Error opening message port",
		ERR_READ_ERROR,			"Error reading file",
		ERR_NO_MEM,			"Out of memory",
		ERR_FILE_FORMAT_ERROR,		"File format corrupt",
		ERR_WEIRD_COMPRESSION,		"Image compression type not supported",
		ERR_WRONG_IMAGE_FORMAT,		"Cannot handle picture file format",
		ERR_TOO_LARGE,			"Requested image size is too large",
		ERR_TOO_SMALL,			"Requested image size is too small",
		ERR_NO_NIL,			"Error opening NIL: stream",
		ERR_NO_POST,			"Error opening post.library v15",

		DTERROR_UNKNOWN_DATATYPE,	"Unknown file format",
		DTERROR_COULDNT_SAVE,		"Could not store file",
		DTERROR_COULDNT_OPEN,		"Could not open file",
		DTERROR_COULDNT_SEND_MESSAGE,	"Internal send message error",
		DTERROR_COULDNT_OPEN_CLIPBOARD,	"Could not open clipboard",
		DTERROR_UNKNOWN_COMPRESSION,	"Compression method unknown",
		DTERROR_NOT_ENOUGH_DATA,	"Not enough data available",
		DTERROR_INVALID_DATA,		"Invalid data",

		19999,				"Postscript error: File not found",
		20001,				"Postscript error: Dictionary full",
		20002,				"Postscript error: Dictionary stack overflow",
		20003,				"Postscript error: Dictionary stack underflow",
		20004,				"Postscript error: Exec stack overflow",
		20005,				"Postscript error: Interrupted",
		20006,				"Postscript error: Invalid access",
		20007,				"Postscript error: Invalid exit",
		20008,				"Postscript error: Invalid file access",
		20009,				"Postscript error: Invalid font",
		20010,				"Postscript error: Invalid restore",
		20011,				"Postscript error: Invalid stop",
		20012,				"Postscript error: I/O error",
		20013,				"Postscript error: Limit check trap",
		20014,				"Postscript error: No current point",
		20015,				"Postscript error: Range check trap",
		20016,				"Postscript error: Stack overflow",
		20017,				"Postscript error: Stack underflow",
		20018,				"Postscript error: Syntax error",
		20019,				"Postscript error: Timeout",
		20020,				"Postscript error: Typecheck",
		20021,				"Postscript error: Undefined",
		20022,				"Postscript error: Undefined filename",
		20023,				"Postscript error: Undefined result",
		20024,				"Postscript error: Unmatched mark",
		20025,				"Postscript error: Unregistered",
		20026,				"Postscript error: Virtual memory error",
		20027,				"Postscript error: Memory allocation error",
		20028,				"Postscript error: Killed",
		20029,				"Postscript error: Configuration",
		20030,				"Postscript error: Undefined resource",

		IFFERR_NOMEM,			"Out of memory",
		IFFERR_READ,			"File read error",
		IFFERR_WRITE,			"File write error",
		IFFERR_SEEK,			"File seek error",
		IFFERR_MANGLED,			"File structure damaged",
		IFFERR_NOTIFF,			"Not an IFF format file",

		0,				NULL
	};

	STRPTR	PrimaryError	= NULL,
		SecondaryError	= NULL;

	if(Primary)
	{
		LONG i;

		for(i = 0 ; LocalErrors[i] . Name ; i++)
		{
			if(LocalErrors[i] . Code == Primary)
			{
				PrimaryError = LocalErrors[i] . Name;

				break;
			}
		}

		if(!PrimaryError)
		{
			STATIC UBYTE __far Buffer[256];

			Fault(Primary,"",Buffer,256);

			PrimaryError = Buffer + 2;
		}

		if(GetPrimary)
			return(PrimaryError);
	}

	if(Secondary)
	{
		LONG i;

		for(i = 0 ; LocalErrors[i] . Name ; i++)
		{
			if(LocalErrors[i] . Code == Secondary)
			{
				SecondaryError = LocalErrors[i] . Name;

				break;
			}
		}

		if(!SecondaryError)
		{
			STATIC UBYTE __far Buffer[256];

			Fault(Secondary,"",Buffer,256);

			SecondaryError = Buffer + 2;
		}
	}

	if(PrimaryError)
	{
		if(ThisProcess -> pr_CLI)
		{
			if(SecondaryError)
				Printf("SpecialHost: %s, %s\a\n",PrimaryError,SecondaryError);
			else
				Printf("SpecialHost: %s\a\n",PrimaryError);
		}
		else
		{
			if(IntuitionBase)
			{
				if(SecondaryError)
					ShowRequest(NULL,"%s\n%s","Continue",PrimaryError,SecondaryError);
				else
					ShowRequest(NULL,"%s","Continue",PrimaryError);
			}
		}
	}
}

VOID __regargs
DeleteBitMap(struct BitMap *BitMap)
{
	if(GfxBase -> LibNode . lib_Version >= 39 && Is_AllocBitMapped(BitMap))
	{
		Del_BitMapTrack(BitMap);

		if (BitMap)
			FreeBitMap(BitMap);
	}
	else
	{
		if (BitMap)
		{
			LONG i;

			for(i = 0; i < BitMap -> Depth; i++)
			{
				if(BitMap -> Planes[i])
					FreeVec(BitMap -> Planes[i]);
			}
			FreeVecPooled(BitMap);
		}
	}
}

struct BitMap * __regargs
CreateBitMap(LONG Width,LONG Height,LONG Depth,ULONG Flags,struct BitMap *Friend)
{

	if(GfxBase -> LibNode . lib_Version < 39 || (Flags & BMAP_MEMF_ANY))
	{

	/* Code used for OS 2.1 (and for OS 3.0 if Flags contains BMAP_MEMF_ANY) */

		struct BitMap	*BitMap;
		LONG		 Plus;

		Width = (Width + 15) & ~15; /* (ghi) fixed for OS 2.1, word alignment */

		if(Depth > 8)
			Plus = (Depth - 8) * sizeof(PLANEPTR);
		else
			Plus = 0;

		if(BitMap = (struct BitMap *)AllocVecPooled(sizeof(struct BitMap) + Plus,MEMF_ANY | MEMF_CLEAR))
		{
			LONG i,PageSize;

			InitBitMap(BitMap,Depth,Width,Height);

			PageSize = BitMap -> BytesPerRow * BitMap -> Rows;

			for(i = 0 ; i < BitMap -> Depth ; i++)
			{
				if (Flags & BMAP_MEMF_ANY)
					BitMap -> Planes[i] = (PLANEPTR)AllocVec(PageSize,MEMF_ANY | ((Flags & BMF_CLEAR) ? MEMF_CLEAR : 0));
				else
					BitMap -> Planes[i] = (PLANEPTR)AllocVec(PageSize,MEMF_CHIP);

				if(!BitMap -> Planes[i])
				{
					LONG j;

					for(j = 0 ; j < i ; j++)
						FreeVec(BitMap -> Planes[j]);

					FreeVecPooled(BitMap);

					return(NULL);
				}
			}

			if((Flags & BMF_CLEAR) && !(Flags & BMAP_MEMF_ANY))
			{
				for (i = 0; i < BitMap -> Depth ; i++)
				{
					if (TypeOfMem(BitMap->Planes[0]) & MEMF_CHIP)
						BltBitMap(BitMap,0,0,BitMap,0,0,Width,Height,0x00,(1 << Depth) - 1,NULL);
					else
						memset(BitMap -> Planes[i], 0, PageSize);
				}
			}

//			Printf("BitMap Address = $%lx [MEMF_%s] %s\n",BitMap -> Planes[0], (TypeOfMem(BitMap -> Planes[0]) & MEMF_CHIP ? "CHIP" : "FAST"), ((Flags & BMAP_MEMF_ANY) ? "(BMAP_MEMF_ANY)" : ""));
			return(BitMap);
		}
	}
	else
	{
		struct BitMap *BitMap = AllocBitMap(Width,Height,Depth,Flags,Friend); /* OS 3.0 */
	
		if (BitMap)
			Add_BitMapTrack(BitMap, TRUE); /* (ghi) to keep track that memory was allocated by AllocBitMap() and need to be freed by FreeBitMap() */

		return (BitMap);
	}
}

VOID __regargs
DeleteTempLine(UBYTE *Line)
{
	FreeVecPooled(Line);
}

UBYTE * __regargs
CreateTempLine(LONG Width,LONG Height)
{
	return((UBYTE *)AllocVecPooled(((Width + 15) & ~15) * Height,MEMF_ANY));
}

VOID __regargs
DeleteTempRPort(struct RastPort *Temp)
{
	DeleteBitMap(Temp -> BitMap);

	FreeVecPooled(Temp);
}

struct RastPort * __regargs
CreateTempRPort(struct RastPort *Source)
{
	struct RastPort *Temp;

	if(Temp = (struct RastPort *)AllocVecPooled(sizeof(struct RastPort),MEMF_ANY))
	{
		LONG Width,Depth;

		CopyMem(Source,Temp,sizeof(struct RastPort));

		Temp -> Layer = NULL;

		if(GfxBase -> LibNode . lib_Version < 39)
		{
			Width	= Source -> BitMap -> BytesPerRow * 8;
			Depth	= Source -> BitMap -> Depth;
		}
		else
		{
			Width	= GetBitMapAttr(Source -> BitMap,BMA_WIDTH);
			Depth	= GetBitMapAttr(Source -> BitMap,BMA_DEPTH);
		}

		if(Temp -> BitMap = CreateBitMap(Width,1,Depth,NULL,Source -> BitMap))
			return(Temp);
		else
			FreeVecPooled(Temp);
	}

	return(NULL);
}

LONG __regargs
FileDateCheck(STRPTR File1,STRPTR File2,LONG *Error)
{
	struct FileInfoBlock	*FileInfo;
	LONG			 Result = 0;

	*Error = 0;

	if(FileInfo = (struct FileInfoBlock *)AllocDosObjectTags(DOS_FIB,TAG_DONE))
	{
		BPTR FileLock;

		if(FileLock = Lock(File1,ACCESS_READ))
		{
			if(Examine(FileLock,FileInfo))
			{
				struct DateStamp Date1;

				Date1 = FileInfo -> fib_Date;

				UnLock(FileLock);

				if(FileLock = Lock(File2,ACCESS_READ))
				{
					if(Examine(FileLock,FileInfo))
					{
						struct DateStamp Date2;

						Date2 = FileInfo -> fib_Date;

						Result = CompareDates(&Date1,&Date2);
					}
					else
						*Error = IoErr();
				}
				else
					*Error = IoErr();
			}
			else
				*Error = IoErr();

			if(FileLock)
				UnLock(FileLock);
		}
		else
			*Error = IoErr();

		FreeDosObject(DOS_FIB,FileInfo);
	}
	else
		*Error = ERR_NO_MEM;

	return(Result);
}

VOID __regargs
AddProtection(STRPTR Name,ULONG Mask)
{
	struct FileInfoBlock *FileInfo;

	if(FileInfo = (struct FileInfoBlock *)AllocDosObject(DOS_FIB,TAG_DONE))
	{
		BPTR FileLock;

		if(FileLock = Lock(Name,ACCESS_READ))
		{
			if(Examine(FileLock,FileInfo))
			{
				UnLock(FileLock);

				SetProtection(Name,FileInfo -> fib_Protection | Mask);
			}
			else
				UnLock(FileLock);
		}

		FreeDosObject(DOS_FIB,FileInfo);
	}
}

STATIC LONG	MaxProgress	= 1,
		LastProgress	= -1;

VOID __regargs
SetMaxProgress(LONG Count)
{
	if(GA_Gauge)
	{
		SetAttrs(GA_Gauge,
			MUIA_Gauge_Current,	0,
			MUIA_Gauge_Max,		Count,
		TAG_DONE);
	}
	else
	{
		if(LastProgress != 100 && LastProgress != -1)
			FPrintf(ThisProcess -> pr_COS,"\n");

		FPrintf(ThisProcess -> pr_COS,"\33[0 pWorking:   0%");
		Flush(ThisProcess -> pr_COS);

		MaxProgress	= Count;
		LastProgress	= 0;
	}
}

VOID __regargs
ShowProgress(LONG Count)
{
	if(GA_Gauge)
		set(GA_Gauge,MUIA_Gauge_Current,Count);
	else
	{
		if(ThisProcess -> pr_CLI)
		{
			Count = (100 * Count) / MaxProgress;

			if(Count < 0)
				Count = 0;
			else
			{
				if(Count > 100)
					Count = 100;
			}

			if(LastProgress != Count)
			{
				FPrintf(ThisProcess -> pr_COS,"\33[4D%3ld%%",Count);

				Flush(ThisProcess -> pr_COS);

				LastProgress = Count;

				if(LastProgress == 100)
				{
					FPrintf(ThisProcess -> pr_COS,"\n\33[ p");

					Flush(ThisProcess -> pr_COS);
				}
			}
		}
	}
}

LONG __regargs
GetMapCode(struct MapTable *Table,STRPTR Key)
{
	while(Table -> Key)
	{
		if(!Stricmp(Table -> Key,Key))
			return(Table -> ID);
		else
			Table++;
	}

	return(-1);
}


/* (ghi) InvertBitMap() inverts a BitMap in any memory (even FAST);
   Width and Height are the respectively the width and the height of
   the area to invert. These values may be less or equal of the true
   BitMap sizes. */

VOID __regargs InvertBitMap(struct BitMap *BitMap, LONG Width, LONG Height)
{
	if (TypeOfMem(BitMap -> Planes[0]) & MEMF_CHIP)
		BltBitMap(BitMap,0,0,BitMap,0,0,Width,Height,0x50,1,NULL);
	else
	{
		UWORD *p;
		LONG i,j,k;

		if (Width > (BitMap -> BytesPerRow << 3))
			Width = BitMap -> BytesPerRow << 3;

		if (Height > BitMap -> Rows)
			Height = BitMap -> Rows;

		for (i=0; i < BitMap -> Depth; i++)
		{
			p = (UWORD *) BitMap -> Planes[i];
			for (j = 0; j < Height; j++)
			{
				for (k = 0; k < (Width >> 4); k++)
				{
					p[k] = ~p[k];
				}
				p += BitMap -> BytesPerRow >> 1;
			}
		}

		k = Width % 16;
		if (k > 0)
		{
			UWORD mask = ((1 << k) - 1) << 16 - k;
			for (i=0; i < BitMap -> Depth; i++)
			{
				p = (UWORD *) BitMap -> Planes[i];
				for (j = 0; j < Height; j++)
				{
					p[Width >> 4] ^= mask;
					p += BitMap -> BytesPerRow >> 1;
				}
			}
		}	
	}
}

/* (ghi) Here follow some functions to keep track if a BitMap was allocated
   by AllocBitMap() or by AllocVec(). These functions are
   Add_BitMapTrack(), Del_BitMapTrack, Find_BitMapTrack() and
   IsAllocBitMapped(). */

STATIC VOID __regargs Add_BitMapTrack(struct BitMap *bmap, BOOL status)
{
	if (bmap == NULL)
		return;

	if (bmt == NULL) /* A NULL bmt indicates an empty list */
	{ 
		if (bmt = AllocVecPooled(sizeof(struct BitMapTrack), MEMF_ANY | MEMF_CLEAR))
		{
			bmt -> next = bmt -> prev = NULL;
			bmt -> bmap = bmap;
			bmt -> IsAllocBitMapped = status;
		}
		else
			return;
	}
	else
	{
		while (bmt -> next)
				bmt = bmt -> next; /* seek bmt to the last node */

		if (bmt -> next = AllocVecPooled(sizeof(struct BitMapTrack), MEMF_ANY | MEMF_CLEAR))
		{
			bmt -> next -> prev = bmt;
			bmt = bmt -> next; /* Link to next node */
			bmt -> next = NULL; /* last note */
			bmt -> bmap = bmap;
			bmt -> IsAllocBitMapped = status;
		}
		else
			return;
	}
}

STATIC BOOL __regargs Is_AllocBitMapped(struct BitMap *bmap)
{
	struct BitMapTrack *p;

	if (p = Find_BitMapTrack(bmap))
		return (p -> IsAllocBitMapped);
	else
		return (FALSE);
}

STATIC struct BitMapTrack * __regargs Find_BitMapTrack(struct BitMap *bmap)
{
	struct BitMapTrack *p = bmt;

	if (!p)
		return NULL;	/* (hes) */

	do
	{
		if (p -> bmap == bmap)
			return (p);
	} 
	while (p = p -> prev); /* scan backward */

	if (p = bmt -> next)
	{
		do
		{
			if (p -> bmap == bmap)
				return (p);
		}
		while (p = p -> next); /* scan forward */
	}

	return (NULL); /* not found */
}

STATIC VOID __regargs Del_BitMapTrack(struct BitMap *bmap)
{
	struct BitMapTrack *p = Find_BitMapTrack(bmap);

	if (!p)
		return;

        if (p -> next && p -> prev) /* p is a mid node */
	{
		p -> prev -> next = p -> next;
		p -> next -> prev = p -> prev;

		if (bmt == p)
			bmt = p -> next;

		FreeVecPooled(p);
	}
	else if (p -> next) /* there are no more nodes before p (p is the first node) */
	{
		p -> next -> prev = NULL; /* new first node */

		if (bmt == p)
			bmt = p -> next;

		FreeVecPooled(p);
	}
	else if (p -> prev) /* there are no more nodes after p (p is the last node) */
	{
		p -> prev -> next = NULL; /* new last node */

		if (bmt == p)
			bmt = p -> prev;

		FreeVecPooled(p);
	}
	else /* there are no more nodes after nor before p (p is the only node) */
	{
		FreeVecPooled(p);
		bmt = NULL;
	}
}
