/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heßmann
*/

#include "Global.h"

enum	{	CLUT_MONO,CLUT_RED,CLUT_GREEN,CLUT_BLUE,CLUT_HUE,CLUT_SAT };

#define ID_CLUT			MAKE_ID('C','L','U','T')
#define ID_DPI			MAKE_ID('D','P','I',' ')

#define CACHE_SIZE		16384

struct DPIHeader
{
	UWORD			DPI_X,
				DPI_Y;
};

struct CLUT
{
	ULONG			Type;
	ULONG			Reserved;
	UBYTE			LUT[256];
};

struct BufferHandle
{
	struct IFFHandle	*Handle;
	UBYTE			*Index,
				 Buffer[CACHE_SIZE];
	LONG			 Length;
	BOOL			 FromClip;
};

struct PictureInfo
{
	struct BitMapHeader	 BitMapHeader;
	ULONG			*ColourMap,
				 ViewModes;
	LONG			 NumColours;
	struct CLUT		*CLUT[CLUT_HUE];
	struct BitMap		*BitMap[3];
	UBYTE			*TempLine[3];
	struct GreyImage	*GreyImage;
	LONG			*Error;
	struct RastPort		*TempRPort,
				 DummyRPort;
	BOOL			 GotCLUT;
};

extern LONG				HookEntry();

STATIC BOOL __regargs			GetClipID(STRPTR Name,LONG *ID);
STATIC LONG __saveds			StreamHandler(struct Hook *Hook,struct IFFHandle *Handle,struct IFFStreamCmd *ActionPkt);

STATIC LONG __regargs			BufferFill(struct BufferHandle *Handle);
STATIC LONG __regargs			BufferGet(struct BufferHandle *Handle);
STATIC LONG __regargs			BufferRead(struct BufferHandle *Handle,UBYTE *Buffer,LONG Size);
STATIC LONG __regargs			BufferSeek(struct BufferHandle *Handle,LONG Size);
STATIC VOID __regargs			BufferClose(struct BufferHandle *Handle);
STATIC struct BufferHandle * __regargs	BufferOpen(STRPTR Name);

STATIC struct GreyImage * __regargs	CreateImage(LONG Width,LONG Height);
STATIC UBYTE * __regargs		AddLine(struct GreyImage *Image);
STATIC struct BufferHandle * __regargs	ReadImageHeader(STRPTR Name,BOOL PatchColour,struct PictureInfo *PictureInfo);
STATIC VOID __regargs			ReadHAM6Data(struct BufferHandle *Handle,struct PictureInfo *PictureInfo);
STATIC VOID __regargs			ReadHAM8Data(struct BufferHandle *Handle,struct PictureInfo *PictureInfo);
STATIC VOID __regargs			ReadVanillaData(struct BufferHandle *Handle,struct PictureInfo *PictureInfo);
STATIC VOID __regargs			ReadDeepData(struct BufferHandle *Handle,struct PictureInfo *PictureInfo);
STATIC VOID __regargs			ConvertHAM6Data(struct PictureInfo *PictureInfo);
STATIC VOID __regargs			ConvertHAM8Data(struct PictureInfo *PictureInfo);
STATIC VOID __regargs			ConvertVanillaData(struct PictureInfo *PictureInfo);
STATIC struct GreyImage * __regargs	ReadPicture(STRPTR Name,BOOL PatchColours,LONG *Error);
STATIC struct GreyImage * __regargs	ReadILBM(STRPTR Name,BOOL PatchColours,LONG *Error);
STATIC BOOL __regargs			GetPictureSize(STRPTR Name,LONG *Width,LONG *Height,LONG *Error);
STATIC BOOL __regargs			GetILBMSize(STRPTR Name,LONG *Width,LONG *Height,LONG *Error);
STATIC BOOL __regargs			GetPicture_DPI(STRPTR Name,LONG *DPI_X,LONG *DPI_Y,LONG *Error);
STATIC BOOL __regargs			GetILBM_DPI(STRPTR Name,LONG *DPI_X,LONG *DPI_Y,LONG *Error);

STATIC struct Hook StreamHook =
{
	{NULL},
	(ULONG (*)())HookEntry,
	(ULONG (*)())StreamHandler,
	NULL
};

STATIC BOOL __regargs
GetClipID(STRPTR Name,LONG *ID)
{
	if(!Strnicmp(Name,"CLIPBOARD:",10))
	{
		if(ID)
			StrToLong(Name + 10,ID);

		return(TRUE);
	}
	else
		return(FALSE);
}

STATIC LONG __saveds
StreamHandler(struct Hook *Hook,struct IFFHandle *Handle,struct IFFStreamCmd *ActionPkt)
{
	struct AsyncFile	*Stream	= (struct AsyncFile *)Handle -> iff_Stream;
	LONG			 Bytes	= ActionPkt -> sc_NBytes;

	switch(ActionPkt -> sc_Command)
	{
		case IFFCMD_READ:

			return(ReadAsync(Stream,ActionPkt -> sc_Buf,Bytes) != Bytes);

		case IFFCMD_WRITE:

			return(WriteAsync(Stream,ActionPkt -> sc_Buf,Bytes) != Bytes);

		case IFFCMD_SEEK:

			return(SeekAsync(Stream,Bytes,MODE_CURRENT) == -1);

		default:

			return(0);
	}
}

STATIC LONG __regargs
BufferFill(struct BufferHandle *Handle)
{
	Handle -> Index = Handle -> Buffer;

	return(Handle -> Length = ReadChunkBytes(Handle -> Handle,Handle -> Buffer,CACHE_SIZE));
}

STATIC LONG __regargs
BufferGet(struct BufferHandle *Handle)
{
	if(!Handle -> Length)
	{
		if(BufferFill(Handle) < 1)
			return(-1);
	}

	Handle -> Length--;

	return(*Handle -> Index++);
}

STATIC LONG __regargs
BufferRead(struct BufferHandle *Handle,UBYTE *Buffer,LONG Size)
{
	LONG BytesRead = 0,Count;

	while(Size)
	{
		if(!Handle -> Length)
		{
			if(BufferFill(Handle) < 1)
				return(BytesRead);
		}

		if(Size > Handle -> Length)
			Count = Handle -> Length;
		else
			Count = Size;

		CopyMem(Handle -> Index,Buffer,Count);

		BytesRead		+= Count;
		Size			-= Count;
		Buffer			+= Count;
		Handle -> Index		+= Count;
		Handle -> Length	-= Count;
	}

	return(BytesRead);
}

STATIC LONG __regargs
BufferSeek(struct BufferHandle *Handle,LONG Size)
{
	LONG BytesRead = 0,Count;

	while(Size)
	{
		if(!Handle -> Length)
		{
			if(BufferFill(Handle) < 1)
				return(BytesRead);
		}

		if(Size > Handle -> Length)
			Count = Handle -> Length;
		else
			Count = Size;

		BytesRead		+= Count;
		Size			-= Count;
		Handle -> Index		+= Count;
		Handle -> Length	-= Count;
	}

	return(BytesRead);
}

STATIC VOID __regargs
BufferClose(struct BufferHandle *Handle)
{
	CloseIFF(Handle -> Handle);

	if(Handle -> FromClip)
		CloseClipboard((struct ClipboardHandle *)Handle -> Handle -> iff_Stream);
	else
		CloseAsync((struct AsyncFile *)Handle -> Handle -> iff_Stream);

	FreeIFF(Handle -> Handle);

	FreeVecPooled(Handle);
}

STATIC struct BufferHandle * __regargs
BufferOpen(STRPTR Name)
{
	struct BufferHandle	*Handle;
	LONG			 Error;

	if(Handle = (struct BufferHandle *)AllocVecPooled(sizeof(struct BufferHandle),MEMF_ANY | MEMF_CLEAR))
	{
		if(Handle -> Handle = AllocIFF())
		{
			LONG ClipID;

			if(GetClipID(Name,&ClipID))
			{
				if(Handle -> Handle -> iff_Stream = (ULONG)OpenClipboard(ClipID))
				{
					InitIFFasClip(Handle -> Handle);

					if(!(Error = OpenIFF(Handle -> Handle,IFFF_READ)))
					{
						Handle -> Length	= 0;
						Handle -> FromClip	= TRUE;

						return(Handle);
					}

					CloseClipboard((struct ClipboardHandle *)Handle -> Handle -> iff_Stream);
				}
				else
					Error = IoErr();
			}
			else
			{
				if(Handle -> Handle -> iff_Stream = (ULONG)OpenAsync(Name,MODE_READ,2 * CACHE_SIZE))
				{
					InitIFF(Handle -> Handle,IFFF_FSEEK | IFFF_RSEEK,&StreamHook);

					if(!(Error = OpenIFF(Handle -> Handle,IFFF_READ)))
					{
						Handle -> Length	= 0;
						Handle -> FromClip	= FALSE;

						return(Handle);
					}

					CloseAsync((struct AsyncFile *)Handle -> Handle -> iff_Stream);
				}
				else
					Error = IoErr();
			}

			FreeIFF(Handle -> Handle);
		}
		else
			Error = ERROR_NO_FREE_STORE;

		FreeVecPooled(Handle);
	}
	else
		Error = ERROR_NO_FREE_STORE;

	if(Error)
		SetIoErr(Error);

	return(NULL);
}

STATIC struct GreyImage * __regargs
CreateImage(LONG Width,LONG Height)
{
	struct GreyImage *Image;

	if(Image = (struct GreyImage *)AllocVecPooled(sizeof(struct GreyImage) + sizeof(UBYTE *) * Height,MEMF_ANY | MEMF_CLEAR))
	{
		LONG Size = (Width < 16384 ? 16384 : Width);

		Image -> Width	= Width;
		Image -> Height	= Height;
		Image -> Lines	= (UBYTE **)(Image + 1);

		if(!(Image -> Pool = LibCreatePool(MEMF_ANY,Size,Size)))
		{
			FreeVecPooled(Image);

			return(NULL);
		}
	}

	return(Image);
}

STATIC UBYTE * __regargs
AddLine(struct GreyImage *Image)
{
	if(Image -> Index < Image -> Height)
	{
		UBYTE *Line;

		if(Line = (UBYTE *)LibAllocPooled(Image -> Pool,(Image -> Width + 3) & ~3))
			Image -> Lines[Image -> Index++] = Line;

		return(Line);
	}
	else
		return(NULL);
}

STATIC struct BufferHandle * __regargs
ReadImageHeader(STRPTR Name,BOOL PatchColours,struct PictureInfo *PictureInfo)
{
	struct BufferHandle *Handle;

	*PictureInfo -> Error = 0;

	if(Handle = BufferOpen(Name))
	{
		STATIC LONG Stops[] =
		{
			ID_ILBM,ID_BMHD,
			ID_ILBM,ID_CMAP,
			ID_ILBM,ID_CAMG,
			ID_ILBM,ID_CLUT,
			ID_ILBM,ID_BODY
		};

		LONG Error;

		if(!(Error = StopChunks(Handle -> Handle,Stops,5)))
		{
			struct ContextNode	*Chunk;
			BOOL			 GotBody	= FALSE,
						 Terminated	= FALSE;
			struct CLUT		 LocalCLUT;

			while(!Terminated && !(Error = ParseIFF(Handle -> Handle,IFFPARSE_SCAN)))
			{
				Chunk = CurrentChunk(Handle -> Handle);

				switch(Chunk -> cn_ID)
				{
					case ID_BMHD:

						if(ReadChunkBytes(Handle -> Handle,&PictureInfo -> BitMapHeader,sizeof(struct BitMapHeader)) != sizeof(struct BitMapHeader))
						{
							*PictureInfo -> Error = ERR_READ_ERROR;

							Terminated = TRUE;
						}
						else
						{
							if(PictureInfo -> BitMapHeader . bmh_Pad & BMHDF_CMAPOK)
								PatchColours = FALSE;

							if(PictureInfo -> BitMapHeader . bmh_Depth == 6)
								PictureInfo -> ViewModes = HAM;
							else
								PictureInfo -> ViewModes = NULL;

							if(PictureInfo -> BitMapHeader . bmh_Width > 320)
								PictureInfo -> ViewModes |= HIRES;

							switch(GfxBase -> DisplayFlags & (PAL | NTSC))
							{
								case PAL:

									if(PictureInfo -> BitMapHeader . bmh_Height > 256)
										PictureInfo -> ViewModes |= LACE;

									break;

								case NTSC:

									if(PictureInfo -> BitMapHeader . bmh_Height > 200)
										PictureInfo -> ViewModes |= LACE;

									break;
							}

							if(PictureInfo -> BitMapHeader . bmh_Compression > 1)
							{
								*PictureInfo -> Error = ERR_WEIRD_COMPRESSION;

								Terminated = TRUE;
							}
						}

						break;

					case ID_CMAP:

						if(PictureInfo -> NumColours = Chunk -> cn_Size / 3)
						{
							if(PictureInfo -> ColourMap = (ULONG *)AllocVecPooled(sizeof(ULONG) * PictureInfo -> NumColours,MEMF_ANY))
							{
								ULONG	RGB;
								WORD	i;

								for(i = 0 ; i < PictureInfo -> NumColours ; i++)
								{
									if(ReadChunkBytes(Handle -> Handle,&RGB,3) != 3)
									{
										*PictureInfo -> Error = ERR_READ_ERROR;

										Terminated = TRUE;

										break;
									}
									else
										PictureInfo -> ColourMap[i] = RGB >> 8;
								}

								if(PatchColours)
								{
									BOOL Patch = TRUE;

									for(i = 0 ; i < PictureInfo -> NumColours ; i++)
									{
										if(PictureInfo -> ColourMap[i] & 0x0F0F0F)
										{
											Patch = FALSE;

											break;
										}
									}

									if(Patch)
									{
										for(i = 0 ; i < PictureInfo -> NumColours ; i++)
											PictureInfo -> ColourMap[i] |= (PictureInfo -> ColourMap[i] >> 4);
									}
								}
							}
							else
							{
								*PictureInfo -> Error = ERR_NO_MEM;

								Terminated = TRUE;
							}
						}

						break;

					case ID_CAMG:

						if(ReadChunkBytes(Handle -> Handle,&PictureInfo -> ViewModes,sizeof(ULONG)) != sizeof(ULONG))
						{
							*PictureInfo -> Error = ERR_READ_ERROR;

							Terminated = TRUE;
						}
						else
						{
							if(!(PictureInfo -> ViewModes & MONITOR_ID_MASK) || ((PictureInfo -> ViewModes & EXTENDED_MODE) && !(PictureInfo -> ViewModes & 0xFFFF0000)))
								PictureInfo -> ViewModes &= ~(EXTENDED_MODE | SPRITES | VP_HIDE | GENLOCK_AUDIO | GENLOCK_VIDEO);
						}

						break;

					case ID_CLUT:

						if(ReadChunkBytes(Handle -> Handle,&LocalCLUT,sizeof(struct CLUT)) != sizeof(struct CLUT))
							Terminated = TRUE;
						else
						{
							if(LocalCLUT . Type < CLUT_HUE)
							{
								BOOL Blank = TRUE;
								WORD i;

								for(i = 0 ; i < 256 ; i++)
								{
									if(LocalCLUT . LUT[i] != i)
									{
										Blank = FALSE;

										break;
									}
								}

								if(!Blank)
								{
									if(!PictureInfo -> CLUT[LocalCLUT . Type])
									{
										if(!(PictureInfo -> CLUT[LocalCLUT . Type] = (struct CLUT *)AllocVecPooled(sizeof(struct CLUT),MEMF_ANY)))
											Terminated = TRUE;
									}

									if(PictureInfo -> CLUT[LocalCLUT . Type])
									{
										CopyMem(&LocalCLUT,PictureInfo -> CLUT[LocalCLUT . Type],sizeof(struct CLUT));

										PictureInfo -> GotCLUT = TRUE;
									}
								}
							}
						}

						break;

					case ID_BODY:

						GotBody = Terminated = TRUE;
						break;
				}
			}

			if(!PictureInfo -> CLUT[CLUT_MONO] && PictureInfo -> ColourMap)
			{
				if(!(PictureInfo -> CLUT[CLUT_MONO] = (struct CLUT *)AllocVecPooled(sizeof(struct CLUT),MEMF_ANY)))
					*PictureInfo -> Error = ERR_NO_MEM;
				else
				{
					ULONG	RGB;
					WORD	i;

					PictureInfo -> CLUT[CLUT_MONO] -> Type		= CLUT_MONO;
					PictureInfo -> CLUT[CLUT_MONO] -> Reserved	= 0;

					for(i = 0 ; i < PictureInfo -> NumColours ; i++)
					{
						RGB = PictureInfo -> ColourMap[i];

						PictureInfo -> CLUT[CLUT_MONO] -> LUT[i] = Luminance((RGB >> 16) & 0xFF,(RGB >> 8) & 0xFF,RGB & 0xFF);
					}

					if(PictureInfo -> ViewModes & EXTRA_HALFBRITE)
					{
						for(i = 0 ; i < PictureInfo -> NumColours ; i++)
							PictureInfo -> CLUT[CLUT_MONO] -> LUT[i + 32] = PictureInfo -> CLUT[CLUT_MONO] -> LUT[i] / 2;
					}
				}
			}

			if(Error && !(*PictureInfo -> Error))
				*PictureInfo -> Error = Error;

			if(!GotBody)
				*PictureInfo -> Error = ERR_FILE_FORMAT_ERROR;
			else
			{
				if(!(*PictureInfo -> Error))
					return(Handle);
			}
		}

		if(Error && !(*PictureInfo -> Error))
			*PictureInfo -> Error = Error;

		BufferClose(Handle);
	}
	else
		*PictureInfo -> Error = IoErr();

	return(NULL);
}

STATIC VOID __regargs
ReadHAM6Data(struct BufferHandle *Handle,struct PictureInfo *PictureInfo)
{
	WORD	 Scale[16];
	UBYTE	*Line;
	LONG	 i,k;
	BOOL	 Error = FALSE;

	for(i = 0 ; i < 16 ; i++)
		Scale[i] = 17 * i;

	SetMaxProgress(PictureInfo -> BitMapHeader . bmh_Height - 1);

	for(i = 0 ; i < PictureInfo -> BitMapHeader . bmh_Height ; i++)
	{
		if(Line = AddLine(PictureInfo -> GreyImage))
		{
			for(k = 0 ; k < PictureInfo -> BitMapHeader . bmh_Depth ; k++)
			{
				if(PictureInfo -> BitMapHeader . bmh_Compression == 1)
				{
					register BYTE Char;
					register UBYTE *Destination = (UBYTE *)PictureInfo -> BitMap[0] -> Planes[k];
					register WORD Count,LineBytes = PictureInfo -> BitMap[0] -> BytesPerRow;

					do
					{
						if((Count = (BYTE)BufferGet(Handle)) >= 0)
						{
							Count++;

							if((LineBytes -= Count) < 0)
							{
								Error = TRUE;

								break;
							}
							else
							{
								if(BufferRead(Handle,(UBYTE *)Destination,Count) == Count)
									Destination += Count;
								else
								{
									Error = TRUE;

									break;
								}
							}
						}
						else
						{
							if(Count != (WORD)(-128))
							{
								Count = -Count + 1;

								if((LineBytes -= Count) < 0)
								{
									Error = TRUE;

									break;
								}
								else
								{
									Char = BufferGet(Handle);

									do
										*Destination++ = Char;
									while(--Count);
								}
							}
						}
					}
					while(LineBytes > 0);

					if(Error)
						break;
				}
				else
				{
					if(BufferRead(Handle,PictureInfo -> BitMap[0] -> Planes[k],PictureInfo -> BitMap[0] -> BytesPerRow) != PictureInfo -> BitMap[0] -> BytesPerRow)
					{
						Error = TRUE;

						break;
					}
				}
			}

			if(PictureInfo -> BitMapHeader . bmh_Masking == 1 && !Error)
			{
				if(PictureInfo -> BitMapHeader . bmh_Compression == 1)
				{
					register WORD Count,LineBytes = PictureInfo -> BitMap[0] -> BytesPerRow;

					do
					{
						if((Count = (BYTE)BufferGet(Handle)) >= 0)
						{
							Count++;

							if((LineBytes -= Count) < 0)
							{
								Error = TRUE;

								break;
							}
							else
							{
								if(BufferSeek(Handle,Count) != Count)
								{
									Error = TRUE;

									break;
								}
							}
						}
						else
						{
							if(Count != (WORD)(-128))
							{
								Count = -Count + 1;

								if((LineBytes -= Count) < 0)
								{
									Error = TRUE;

									break;
								}
								else
									BufferGet(Handle);
							}
						}
					}
					while(LineBytes > 0);
				}
				else
				{
					if(BufferSeek(Handle,PictureInfo -> BitMap[0] -> BytesPerRow) != PictureInfo -> BitMap[0] -> BytesPerRow)
					{
						*PictureInfo -> Error = ERR_READ_ERROR;

						break;
					}
				}
			}

			if(Error)
			{
				*PictureInfo -> Error = ERR_READ_ERROR;

				break;
			}
			else
			{
				register UBYTE	Value;
				register WORD	R = 0,G = 0,B = 0;
				register ULONG	Colour;

				(* ReadLine)(&PictureInfo -> DummyRPort,0,PictureInfo -> GreyImage -> Width,PictureInfo -> TempLine[0],PictureInfo -> TempRPort);

				for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
				{
					switch((Value = PictureInfo -> TempLine[0][k]) & 0x30)
					{
						case 0x00:

							Colour = PictureInfo -> ColourMap[Value];

							R = (Colour >> 16) & 0xFF;
							G = (Colour >> 8) & 0xFF;
							B = Colour & 0xFF;

							break;

						case 0x10:

							B = Scale[Value & 0x0F];
							break;

						case 0x20:

							R = Scale[Value & 0x0F];
							break;

						case 0x30:

							G = Scale[Value & 0x0F];
							break;
					}

					*Line++ = Luminance(R,G,B);
				}

				ShowProgress(i);
			}
		}
		else
		{
			*PictureInfo -> Error = ERR_NO_MEM;

			break;
		}
	}
}

STATIC VOID __regargs
ConvertHAM6Data(struct PictureInfo *PictureInfo)
{
	WORD	 Scale[16];
	UBYTE	*Line;
	LONG	 i,k;

	for(i = 0 ; i < 16 ; i++)
		Scale[i] = 17 * i;

	SetMaxProgress(PictureInfo -> BitMapHeader . bmh_Height - 1);

	for(i = 0 ; i < PictureInfo -> BitMapHeader . bmh_Height ; i++)
	{
		if(Line = AddLine(PictureInfo -> GreyImage))
		{
			register UBYTE	Value;
			register WORD	R = 0,G = 0,B = 0;
			register ULONG	Colour;

			(* ReadLine)(&PictureInfo -> DummyRPort,i,PictureInfo -> GreyImage -> Width,PictureInfo -> TempLine[0],PictureInfo -> TempRPort);

			for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
			{
				switch((Value = PictureInfo -> TempLine[0][k]) & 0x30)
				{
					case 0x00:

						Colour = PictureInfo -> ColourMap[Value];

						R = (Colour >> 16) & 0xFF;
						G = (Colour >> 8) & 0xFF;
						B = Colour & 0xFF;

						break;

					case 0x10:

						B = Scale[Value & 0x0F];
						break;

					case 0x20:

						R = Scale[Value & 0x0F];
						break;

					case 0x30:

						G = Scale[Value & 0x0F];
						break;
				}

				*Line++ = Luminance(R,G,B);
			}

			ShowProgress(i);
		}
		else
		{
			*PictureInfo -> Error = ERR_NO_MEM;

			break;
		}
	}
}

STATIC VOID __regargs
ReadHAM8Data(struct BufferHandle *Handle,struct PictureInfo *PictureInfo)
{
	WORD	 Scale[64];
	UBYTE	*Line;
	LONG	 i,k;
	BOOL	 Error = FALSE;

	for(i = 0 ; i < 64 ; i++)
		Scale[i] = (255 * i) / 63;

	SetMaxProgress(PictureInfo -> BitMapHeader . bmh_Height - 1);

	for(i = 0 ; i < PictureInfo -> BitMapHeader . bmh_Height ; i++)
	{
		if(Line = AddLine(PictureInfo -> GreyImage))
		{
			for(k = 0 ; k < PictureInfo -> BitMapHeader . bmh_Depth ; k++)
			{
				if(PictureInfo -> BitMapHeader . bmh_Compression == 1)
				{
					register BYTE Char;
					register UBYTE *Destination = (UBYTE *)PictureInfo -> BitMap[0] -> Planes[k];
					register WORD Count,LineBytes = PictureInfo -> BitMap[0] -> BytesPerRow;

					do
					{
						if((Count = (BYTE)BufferGet(Handle)) >= 0)
						{
							Count++;

							if((LineBytes -= Count) < 0)
							{
								Error = TRUE;

								break;
							}
							else
							{
								if(BufferRead(Handle,(UBYTE *)Destination,Count) == Count)
									Destination += Count;
								else
								{
									Error = TRUE;

									break;
								}
							}
						}
						else
						{
							if(Count != (WORD)(-128))
							{
								Count = -Count + 1;

								if((LineBytes -= Count) < 0)
								{
									Error = TRUE;

									break;
								}
								else
								{
									Char = BufferGet(Handle);

									do
										*Destination++ = Char;
									while(--Count);
								}
							}
						}
					}
					while(LineBytes > 0);

					if(Error)
						break;
				}
				else
				{
					if(BufferRead(Handle,PictureInfo -> BitMap[0] -> Planes[k],PictureInfo -> BitMap[0] -> BytesPerRow) != PictureInfo -> BitMap[0] -> BytesPerRow)
					{
						Error = TRUE;

						break;
					}
				}
			}

			if(PictureInfo -> BitMapHeader . bmh_Masking == 1 && !Error)
			{
				if(PictureInfo -> BitMapHeader . bmh_Compression == 1)
				{
					register WORD Count,LineBytes = PictureInfo -> BitMap[0] -> BytesPerRow;

					do
					{
						if((Count = (BYTE)BufferGet(Handle)) >= 0)
						{
							Count++;

							if((LineBytes -= Count) < 0)
							{
								Error = TRUE;

								break;
							}
							else
							{
								if(BufferSeek(Handle,Count) != Count)
								{
									Error = TRUE;

									break;
								}
							}
						}
						else
						{
							if(Count != (WORD)(-128))
							{
								Count = -Count + 1;

								if((LineBytes -= Count) < 0)
								{
									Error = TRUE;

									break;
								}
								else
									BufferGet(Handle);
							}
						}
					}
					while(LineBytes > 0);
				}
				else
				{
					if(BufferSeek(Handle,PictureInfo -> BitMap[0] -> BytesPerRow) != PictureInfo -> BitMap[0] -> BytesPerRow)
					{
						*PictureInfo -> Error = ERR_READ_ERROR;

						break;
					}
				}
			}

			if(Error)
			{
				*PictureInfo -> Error = ERR_READ_ERROR;

				break;
			}
			else
			{
				register UBYTE	Value;
				register WORD	R = 0,G = 0,B = 0;
				register ULONG	Colour;

				(* ReadLine)(&PictureInfo -> DummyRPort,0,PictureInfo -> GreyImage -> Width,PictureInfo -> TempLine[0],PictureInfo -> TempRPort);

				for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
				{
					switch((Value = PictureInfo -> TempLine[0][k]) & 0xC0)
					{
						case 0x00:

							Colour = PictureInfo -> ColourMap[Value];

							R = (Colour >> 16) & 0xFF;
							G = (Colour >> 8) & 0xFF;
							B = Colour & 0xFF;

							break;

						case 0x40:

							B = Scale[Value & 0x3F];
							break;

						case 0x80:

							R = Scale[Value & 0x3F];
							break;

						case 0xC0:

							G = Scale[Value & 0x3F];
							break;
					}

					*Line++ = Luminance(R,G,B);
				}

				ShowProgress(i);
			}
		}
		else
		{
			*PictureInfo -> Error = ERR_NO_MEM;

			break;
		}
	}
}

STATIC VOID __regargs
ConvertHAM8Data(struct PictureInfo *PictureInfo)
{
	WORD	 Scale[64];
	UBYTE	*Line;
	LONG	 i,k;

	for(i = 0 ; i < 64 ; i++)
		Scale[i] = (255 * i) / 63;

	SetMaxProgress(PictureInfo -> BitMapHeader . bmh_Height - 1);

	for(i = 0 ; i < PictureInfo -> BitMapHeader . bmh_Height ; i++)
	{
		if(Line = AddLine(PictureInfo -> GreyImage))
		{
			register UBYTE	Value;
			register WORD	R = 0,G = 0,B = 0;
			register ULONG	Colour;

			(* ReadLine)(&PictureInfo -> DummyRPort,i,PictureInfo -> GreyImage -> Width,PictureInfo -> TempLine[0],PictureInfo -> TempRPort);

			for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
			{
				switch((Value = PictureInfo -> TempLine[0][k]) & 0xC0)
				{
					case 0x00:

						Colour = PictureInfo -> ColourMap[Value];

						R = (Colour >> 16) & 0xFF;
						G = (Colour >> 8) & 0xFF;
						B = Colour & 0xFF;

						break;

					case 0x40:

						B = Scale[Value & 0x3F];
						break;

					case 0x80:

						R = Scale[Value & 0x3F];
						break;

					case 0xC0:

						G = Scale[Value & 0x3F];
						break;
				}

				*Line++ = Luminance(R,G,B);
			}

			ShowProgress(i);
		}
		else
		{
			*PictureInfo -> Error = ERR_NO_MEM;

			break;
		}
	}
}

STATIC VOID __regargs
ReadVanillaData(struct BufferHandle *Handle,struct PictureInfo *PictureInfo)
{
	UBYTE	*Line,
		*LUT;
	LONG	 i,k;
	WORD	 LUTType;
	BOOL	 Error = FALSE;

	for(i = CLUT_MONO ; i <= CLUT_BLUE ; i++)
	{
		if(PictureInfo -> CLUT[i])
		{
			LUT	= PictureInfo -> CLUT[i] -> LUT;
			LUTType	= i;

			break;
		}
	}

	SetMaxProgress(PictureInfo -> BitMapHeader . bmh_Height - 1);

	for(i = 0 ; i < PictureInfo -> BitMapHeader . bmh_Height ; i++)
	{
		if(Line = AddLine(PictureInfo -> GreyImage))
		{
			for(k = 0 ; k < PictureInfo -> BitMapHeader . bmh_Depth ; k++)
			{
				if(PictureInfo -> BitMapHeader . bmh_Compression == 1)
				{
					register BYTE Char;
					register UBYTE *Destination = (UBYTE *)PictureInfo -> BitMap[0] -> Planes[k];
					register WORD Count,LineBytes = PictureInfo -> BitMap[0] -> BytesPerRow;

					do
					{
						if((Count = (BYTE)BufferGet(Handle)) >= 0)
						{
							Count++;

							if((LineBytes -= Count) < 0)
							{
								Error = TRUE;

								break;
							}
							else
							{
								if(BufferRead(Handle,(UBYTE *)Destination,Count) == Count)
									Destination += Count;
								else
								{
									Error = TRUE;

									break;
								}
							}
						}
						else
						{
							if(Count != (WORD)(-128))
							{
								Count = -Count + 1;

								if((LineBytes -= Count) < 0)
								{
									Error = TRUE;

									break;
								}
								else
								{
									Char = BufferGet(Handle);

									do
										*Destination++ = Char;
									while(--Count);
								}
							}
						}
					}
					while(LineBytes > 0);

					if(Error)
						break;
				}
				else
				{
					if(BufferRead(Handle,PictureInfo -> BitMap[0] -> Planes[k],PictureInfo -> BitMap[0] -> BytesPerRow) != PictureInfo -> BitMap[0] -> BytesPerRow)
					{
						Error = TRUE;

						break;
					}
				}
			}

			if(PictureInfo -> BitMapHeader . bmh_Masking == 1 && !Error)
			{
				if(PictureInfo -> BitMapHeader . bmh_Compression == 1)
				{
					register WORD Count,LineBytes = PictureInfo -> BitMap[0] -> BytesPerRow;

					do
					{
						if((Count = (BYTE)BufferGet(Handle)) >= 0)
						{
							Count++;

							if((LineBytes -= Count) < 0)
							{
								Error = TRUE;

								break;
							}
							else
							{
								if(BufferSeek(Handle,Count) != Count)
								{
									Error = TRUE;

									break;
								}
							}
						}
						else
						{
							if(Count != (WORD)(-128))
							{
								Count = -Count + 1;

								if((LineBytes -= Count) < 0)
								{
									Error = TRUE;

									break;
								}
								else
									BufferGet(Handle);
							}
						}
					}
					while(LineBytes > 0);
				}
				else
				{
					if(BufferSeek(Handle,PictureInfo -> BitMap[0] -> BytesPerRow) != PictureInfo -> BitMap[0] -> BytesPerRow)
					{
						*PictureInfo -> Error = ERR_READ_ERROR;

						break;
					}
				}
			}

			if(Error)
			{
				*PictureInfo -> Error = ERR_READ_ERROR;

				break;
			}
			else
			{
				(* ReadLine)(&PictureInfo -> DummyRPort,0,PictureInfo -> GreyImage -> Width,PictureInfo -> TempLine[0],PictureInfo -> TempRPort);

				if(LUT)
				{
					switch(LUTType)
					{
						case CLUT_MONO:

							for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
								*Line++ = LUT[PictureInfo -> TempLine[0][k]];

							break;

						case CLUT_RED:

							for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
								*Line++ = Luminance(LUT[PictureInfo -> TempLine[0][k]],0,0);

							break;

						case CLUT_GREEN:

							for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
								*Line++ = Luminance(0,LUT[PictureInfo -> TempLine[0][k]],0);

							break;

						case CLUT_BLUE:

							for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
								*Line++ = Luminance(0,0,LUT[PictureInfo -> TempLine[0][k]]);

							break;
					}
				}
				else
					CopyMem(PictureInfo -> TempLine[0],Line,PictureInfo -> GreyImage -> Width);

				ShowProgress(i);
			}
		}
		else
		{
			*PictureInfo -> Error = ERR_NO_MEM;

			break;
		}
	}
}

STATIC VOID __regargs
ConvertVanillaData(struct PictureInfo *PictureInfo)
{
	UBYTE	*Line,
		*LUT;
	LONG	 i,k;
	WORD	 LUTType;

	for(i = CLUT_MONO ; i <= CLUT_BLUE ; i++)
	{
		if(PictureInfo -> CLUT[i])
		{
			LUT	= PictureInfo -> CLUT[i] -> LUT;
			LUTType	= i;

			break;
		}
	}

	SetMaxProgress(PictureInfo -> BitMapHeader . bmh_Height - 1);

	for(i = 0 ; i < PictureInfo -> BitMapHeader . bmh_Height ; i++)
	{
		if(Line = AddLine(PictureInfo -> GreyImage))
		{
			(* ReadLine)(&PictureInfo -> DummyRPort,i,PictureInfo -> GreyImage -> Width,PictureInfo -> TempLine[0],PictureInfo -> TempRPort);

			if(LUT)
			{
				switch(LUTType)
				{
					case CLUT_MONO:

						for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
							*Line++ = LUT[PictureInfo -> TempLine[0][k]];

						break;

					case CLUT_RED:

						for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
							*Line++ = Luminance(LUT[PictureInfo -> TempLine[0][k]],0,0);

						break;

					case CLUT_GREEN:

						for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
							*Line++ = Luminance(0,LUT[PictureInfo -> TempLine[0][k]],0);

						break;

					case CLUT_BLUE:

						for(k = 0 ; k < PictureInfo -> GreyImage -> Width ; k++)
							*Line++ = Luminance(0,0,LUT[PictureInfo -> TempLine[0][k]]);

						break;
				}
			}
			else
				CopyMem(PictureInfo -> TempLine[0],Line,PictureInfo -> GreyImage -> Width);

			ShowProgress(i);
		}
		else
		{
			*PictureInfo -> Error = ERR_NO_MEM;

			break;
		}
	}
}

STATIC VOID __regargs
ReadDeepData(struct BufferHandle *Handle,struct PictureInfo *PictureInfo)
{
	WORD	Scale[256];
	UBYTE	*Line;
	LONG	i,j,k;
	WORD	NumPlanes = PictureInfo -> BitMapHeader . bmh_Depth / 3;
	BOOL	Error = FALSE;

	for(i = 0 ; i < (1 << NumPlanes) ; i++)
		Scale[i] = (255 * i) / ((1 << NumPlanes) - 1);

	SetMaxProgress(PictureInfo -> BitMapHeader . bmh_Height - 1);

	for(i = 0 ; i < PictureInfo -> BitMapHeader . bmh_Height ; i++)
	{
		if(Line = AddLine(PictureInfo -> GreyImage))
		{
			for(j = 0 ; j < 3 ; j++)
			{
				for(k = 0 ; k < NumPlanes ; k++)
				{
					if(PictureInfo -> BitMapHeader . bmh_Compression == 1)
					{
						register BYTE Char;
						register UBYTE *Destination = (UBYTE *)PictureInfo -> BitMap[j] -> Planes[k];
						register WORD Count,LineBytes = PictureInfo -> BitMap[j] -> BytesPerRow;

						do
						{
							if((Count = (BYTE)BufferGet(Handle)) >= 0)
							{
								Count++;

								if((LineBytes -= Count) < 0)
								{
									Error = TRUE;

									break;
								}
								else
								{
									if(BufferRead(Handle,(UBYTE *)Destination,Count) == Count)
										Destination += Count;
									else
									{
										Error = TRUE;

										break;
									}
								}
							}
							else
							{
								if(Count != (WORD)(-128))
								{
									Count = -Count + 1;

									if((LineBytes -= Count) < 0)
									{
										Error = TRUE;

										break;
									}
									else
									{
										Char = BufferGet(Handle);

										do
											*Destination++ = Char;
										while(--Count);
									}
								}
							}
						}
						while(LineBytes > 0);

						if(Error)
							break;
					}
					else
					{
						if(BufferRead(Handle,PictureInfo -> BitMap[j] -> Planes[k],PictureInfo -> BitMap[j] -> BytesPerRow) != PictureInfo -> BitMap[j] -> BytesPerRow)
						{
							Error = TRUE;

							break;
						}
					}
				}

				PictureInfo -> DummyRPort . BitMap = PictureInfo -> BitMap[j];

				(* ReadLine)(&PictureInfo -> DummyRPort,0,PictureInfo -> GreyImage -> Width,PictureInfo -> TempLine[j],PictureInfo -> TempRPort);

				if(Error)
					break;
			}

			if(Error)
			{
				*PictureInfo -> Error = ERR_READ_ERROR;

				break;
			}
			else
			{
				if(PictureInfo -> GotCLUT)
				{
					register WORD R,G,B;

					for(j = 0 ; j < PictureInfo -> GreyImage -> Width ; j++)
					{
						if(PictureInfo -> CLUT[CLUT_RED])
							R = Scale[PictureInfo -> CLUT[CLUT_RED] -> LUT[PictureInfo -> TempLine[0][j]]];
						else
							R = Scale[PictureInfo -> TempLine[0][j]];

						if(PictureInfo -> CLUT[CLUT_GREEN])
							G = Scale[PictureInfo -> CLUT[CLUT_GREEN] -> LUT[PictureInfo -> TempLine[1][j]]];
						else
							G = Scale[PictureInfo -> TempLine[1][j]];

						if(PictureInfo -> CLUT[CLUT_BLUE])
							B = Scale[PictureInfo -> CLUT[CLUT_BLUE] -> LUT[PictureInfo -> TempLine[2][j]]];
						else
							B = Scale[PictureInfo -> TempLine[2][j]];

						*Line++ = Luminance(R,G,B);
					}
				}
				else
				{
					for(j = 0 ; j < PictureInfo -> GreyImage -> Width ; j++)
						*Line++ = Luminance(Scale[PictureInfo -> TempLine[0][j]],Scale[PictureInfo -> TempLine[1][j]],Scale[PictureInfo -> TempLine[2][j]]);
				}

				ShowProgress(i);
			}
		}
		else
		{
			*PictureInfo -> Error = ERR_NO_MEM;

			break;
		}
	}
}

STATIC struct GreyImage * __regargs
ReadPicture(STRPTR Name,BOOL PatchColours,LONG *Error)
{
	struct GreyImage	*Image = NULL;
	Object			*Picture;

	*Error = 0;

	if(Picture = NewDTObject(Name,
		DTA_SourceType,	DTST_FILE,
		DTA_GroupID,	GID_PICTURE,

		PDTA_Remap,	FALSE,
	TAG_DONE))
	{
		if(DoMethod(Picture,DTM_PROCLAYOUT,NULL,TRUE))
		{
			ULONG			 ViewModes = INVALID_ID;
			LONG			 NumColours = 0;
			struct BitMapHeader	*BitMapHeader = NULL;
			struct BitMap		*BitMap = NULL;
			ULONG			*CRegs = NULL;

			if(GetDTAttrs(Picture,
				PDTA_ModeID,		&ViewModes,
				PDTA_NumColors,		&NumColours,
				PDTA_BitMapHeader,	&BitMapHeader,
				PDTA_BitMap,		&BitMap,
				PDTA_CRegs,		&CRegs,
			TAG_DONE))
			{
				if(ViewModes != INVALID_ID && CRegs && NumColours && BitMapHeader && BitMap)
				{
					struct PictureInfo *PictureInfo;

					if(BitMapHeader -> bmh_Pad & BMHDF_CMAPOK)
						PatchColours = FALSE;

					if(PictureInfo = (struct PictureInfo *)AllocVecPooled(sizeof(struct PictureInfo),MEMF_ANY | MEMF_CLEAR))
					{
						PictureInfo -> Error		= Error;
						PictureInfo -> NumColours	= NumColours;
						PictureInfo -> ViewModes	= ViewModes;
						PictureInfo -> BitMap[0]	= BitMap;

						CopyMem(BitMapHeader,&PictureInfo -> BitMapHeader,sizeof(struct BitMapHeader));

						if(PictureInfo -> ColourMap = (ULONG *)AllocVecPooled(sizeof(ULONG) * NumColours,MEMF_ANY))
						{
							LONG i;

							for(i = 0 ; i < NumColours ; i++)
								PictureInfo -> ColourMap[i] = ((CRegs[i * 3] >> 8) & 0xFF0000) | ((CRegs[i * 3 + 1] >> 16) & 0xFF00) | ((CRegs[i * 3 + 2] >> 24) & 0xFF);

							if(PatchColours)
							{
								BOOL Patch = TRUE;

								for(i = 0 ; i < PictureInfo -> NumColours ; i++)
								{
									if(PictureInfo -> ColourMap[i] & 0x0F0F0F)
									{
										Patch = FALSE;

										break;
									}
								}

								if(Patch)
								{
									for(i = 0 ; i < PictureInfo -> NumColours ; i++)
										PictureInfo -> ColourMap[i] |= (PictureInfo -> ColourMap[i] >> 4);
								}
							}

							if(PictureInfo -> CLUT[CLUT_MONO] = (struct CLUT *)AllocVecPooled(sizeof(struct CLUT),MEMF_ANY))
							{
								ULONG RGB;

								PictureInfo -> CLUT[CLUT_MONO] -> Type		= CLUT_MONO;
								PictureInfo -> CLUT[CLUT_MONO] -> Reserved	= 0;

								for(i = 0 ; i < PictureInfo -> NumColours ; i++)
								{
									RGB = PictureInfo -> ColourMap[i];

									PictureInfo -> CLUT[CLUT_MONO] -> LUT[i] = Luminance((RGB >> 16) & 0xFF,(RGB >> 8) & 0xFF,RGB & 0xFF);
								}

								if(PictureInfo -> ViewModes & EXTRA_HALFBRITE)
								{
									for(i = 0 ; i < PictureInfo -> NumColours ; i++)
										PictureInfo -> CLUT[CLUT_MONO] -> LUT[i + 32] = PictureInfo -> CLUT[CLUT_MONO] -> LUT[i] / 2;
								}

								if(PictureInfo -> GreyImage = CreateImage(PictureInfo -> BitMapHeader . bmh_Width,PictureInfo -> BitMapHeader . bmh_Height))
								{
									if(PictureInfo -> BitMapHeader . bmh_Depth <= 8)
									{
										if(PictureInfo -> TempLine[0] = CreateTempLine(PictureInfo -> BitMapHeader . bmh_Width,1))
										{
											InitRastPort(&PictureInfo -> DummyRPort);

											PictureInfo -> DummyRPort . BitMap = PictureInfo -> BitMap[0];

											if(PictureInfo -> TempRPort = CreateTempRPort(&PictureInfo -> DummyRPort))
											{
												if(PictureInfo -> ViewModes & EXTRA_HALFBRITE)
												{
													if(PictureInfo -> BitMapHeader . bmh_Depth == 6)
														ConvertVanillaData(PictureInfo);
													else
														*Error = ERR_FILE_FORMAT_ERROR;
												}
												else
												{
													if(PictureInfo -> ViewModes & HAM)
													{
														if(PictureInfo -> BitMapHeader . bmh_Depth == 6)
															ConvertHAM6Data(PictureInfo);
														else
														{
															if(PictureInfo -> BitMapHeader . bmh_Depth == 8)
																ConvertHAM8Data(PictureInfo);
															else
																*Error = ERR_FILE_FORMAT_ERROR;
														}
													}
													else
														ConvertVanillaData(PictureInfo);
												}

												DeleteTempRPort(PictureInfo -> TempRPort);
											}
											else
												*Error = ERR_NO_MEM;

											DeleteTempLine(PictureInfo -> TempLine[0]);
										}
										else
											*Error = ERR_NO_MEM;
									}
									else
										*Error = ERR_NO_MEM;

									if(*Error)
										DeleteImage(PictureInfo -> GreyImage);
									else
										Image = PictureInfo -> GreyImage;
								}
								else
									*Error = ERR_NO_MEM;

								FreeVecPooled(PictureInfo -> CLUT[CLUT_MONO]);
							}
							else
								*Error = ERR_NO_MEM;

							FreeVecPooled(PictureInfo -> ColourMap);
						}
						else
							*Error = ERR_NO_MEM;

						FreeVecPooled(PictureInfo);
					}
					else
						*Error = ERR_NO_MEM;
				}
				else
					*Error = ERR_NO_MEM;
			}
			else
				*Error = ERR_NO_MEM;
		}
		else
			*Error = ERR_NO_MEM;

		DisposeDTObject(Picture);
	}
	else
		*Error = IoErr();

	return(Image);
}

STATIC struct GreyImage * __regargs
ReadILBM(STRPTR Name,BOOL PatchColours,LONG *Error)
{
	struct PictureInfo	*PictureInfo;
	struct GreyImage	*Image = NULL;

	*Error = 0;

	if(PictureInfo = (struct PictureInfo *)AllocVecPooled(sizeof(struct PictureInfo),MEMF_ANY | MEMF_CLEAR))
	{
		struct BufferHandle	*Handle;
		LONG			 i;

		PictureInfo -> Error = Error;

		if(Handle = ReadImageHeader(Name,PatchColours,PictureInfo))
		{
			if(PictureInfo -> GreyImage = CreateImage(PictureInfo -> BitMapHeader . bmh_Width,PictureInfo -> BitMapHeader . bmh_Height))
			{
				BOOL GotIt = TRUE,Deep;

				if(PictureInfo -> BitMapHeader . bmh_Depth <= 8)
				{
					if(!(PictureInfo -> BitMap[0] = CreateBitMap(PictureInfo -> BitMapHeader . bmh_Width,1,PictureInfo -> BitMapHeader . bmh_Depth,NULL,NULL)))
						GotIt = FALSE;
					else
					{
						if(!(PictureInfo -> TempLine[0] = CreateTempLine(PictureInfo -> BitMapHeader . bmh_Width,1)))
							GotIt = FALSE;
						else
							Deep = FALSE;
					}
				}
				else
				{
					if(PictureInfo -> BitMapHeader . bmh_Depth >= 12 && !(PictureInfo -> BitMapHeader . bmh_Depth % 3))
					{
						Deep = TRUE;

						for(i = 0 ; i < 3 ; i++)
						{
							if(!(PictureInfo -> BitMap[i] = CreateBitMap(PictureInfo -> BitMapHeader . bmh_Width,1,PictureInfo -> BitMapHeader . bmh_Depth / 3,NULL,NULL)))
							{
								GotIt = FALSE;

								break;
							}
							else
							{
								if(!(PictureInfo -> TempLine[i] = CreateTempLine(PictureInfo -> BitMapHeader . bmh_Width,1)))
								{
									GotIt = FALSE;

									break;
								}
							}
						}
					}
					else
						GotIt = FALSE;
				}

				if(GotIt)
				{
					InitRastPort(&PictureInfo -> DummyRPort);

					PictureInfo -> DummyRPort . BitMap = PictureInfo -> BitMap[0];

					if(PictureInfo -> TempRPort = CreateTempRPort(&PictureInfo -> DummyRPort))
					{
						if(Deep)
						{
							if(!PictureInfo -> BitMapHeader . bmh_Masking)
								ReadDeepData(Handle,PictureInfo);
							else
								*Error = ERR_FILE_FORMAT_ERROR;
						}
						else
						{
							if(PictureInfo -> ViewModes & EXTRA_HALFBRITE)
							{
								if(PictureInfo -> BitMapHeader . bmh_Depth == 6)
									ReadVanillaData(Handle,PictureInfo);
								else
									*Error = ERR_FILE_FORMAT_ERROR;
							}
							else
							{
								if(PictureInfo -> ViewModes & HAM)
								{
									if(PictureInfo -> BitMapHeader . bmh_Depth == 6)
										ReadHAM6Data(Handle,PictureInfo);
									else
									{
										if(PictureInfo -> BitMapHeader . bmh_Depth == 8)
											ReadHAM8Data(Handle,PictureInfo);
										else
											*Error = ERR_FILE_FORMAT_ERROR;
									}
								}
								else
									ReadVanillaData(Handle,PictureInfo);
							}
						}

						DeleteTempRPort(PictureInfo -> TempRPort);
					}
					else
						*Error = ERR_NO_MEM;
				}
				else
					*Error = ERR_NO_MEM;

				for(i = 0 ; i < 3 ; i++)
				{
					if(PictureInfo -> BitMap[i])
						DeleteBitMap(PictureInfo -> BitMap[i]);

					if(PictureInfo -> TempLine[i])
						DeleteTempLine(PictureInfo -> TempLine[i]);
				}

				if(*Error)
					DeleteImage(PictureInfo -> GreyImage);
				else
					Image = PictureInfo -> GreyImage;
			}
			else
				*Error = ERR_NO_MEM;

			BufferClose(Handle);
		}

		for(i = 0 ; i < CLUT_HUE ; i++)
		{
			if(PictureInfo -> CLUT[i])
				FreeVecPooled(PictureInfo -> CLUT[i]);
		}

		if(PictureInfo -> ColourMap)
			FreeVecPooled(PictureInfo -> ColourMap);

		FreeVecPooled(PictureInfo);
	}
	else
		*Error = ERR_NO_MEM;

	return(Image);
}

STATIC BOOL __regargs
GetPictureSize(STRPTR Name,LONG *Width,LONG *Height,LONG *Error)
{
	Object	*Picture;
	BOOL	 Result = FALSE;

	*Error = 0;

	if(Picture = NewDTObject(Name,
		DTA_SourceType,	DTST_FILE,
		DTA_GroupID,	GID_PICTURE,

		PDTA_Remap,	FALSE,
	TAG_DONE))
	{
		struct FrameInfo FrameInfo;

		memset(&FrameInfo,0,sizeof(struct FrameInfo));

		if(DoMethod(Picture,DTM_FRAMEBOX,NULL,&FrameInfo,&FrameInfo,sizeof(struct FrameInfo),NULL))
		{
			if(FrameInfo . fri_Dimensions . Depth)
			{
				*Width	= FrameInfo . fri_Dimensions . Width;
				*Height	= FrameInfo . fri_Dimensions . Height;

				Result	= TRUE;
			}
			else
				*Error = ERR_WRONG_IMAGE_FORMAT;
		}
		else
			*Error = ERR_FILE_FORMAT_ERROR;

		DisposeDTObject(Picture);
	}
	else
		*Error = IoErr();

	return(Result);
}

STATIC BOOL __regargs
GetILBMSize(STRPTR Name,LONG *Width,LONG *Height,LONG *Error)
{
	struct IFFHandle	*Handle;
	BOOL			 Result = FALSE;
	LONG			 ClipID;

	*Error = 0;

	if(Handle = AllocIFF())
	{
		if(GetClipID(Name,&ClipID))
		{
			if(Handle -> iff_Stream = (ULONG)OpenClipboard(ClipID))
			{
				InitIFFasClip(Handle);

				if(!(*Error = OpenIFF(Handle,IFFF_READ)))
				{
					if(!(*Error = StopChunk(Handle,ID_ILBM,ID_BMHD)))
					{
						if(!(*Error = ParseIFF(Handle,IFFPARSE_SCAN)))
						{
							struct BitMapHeader BitMapHeader;

							if(ReadChunkBytes(Handle,&BitMapHeader,sizeof(struct BitMapHeader)) == sizeof(struct BitMapHeader))
							{
								*Width	= BitMapHeader . bmh_Width;
								*Height	= BitMapHeader . bmh_Height;

								Result	= TRUE;
							}
							else
								*Error = ERROR_SEEK_ERROR;
						}

						if(*Error == IFFERR_EOF || *Error == IFFERR_EOC)
							*Error = 0;
					}

					CloseIFF(Handle);
				}

				CloseClipboard((struct ClipboardHandle *)Handle -> iff_Stream);
			}
			else
				*Error = ERR_NO_MEM;
		}
		else
		{
			if(Handle -> iff_Stream = (ULONG)Open(Name,MODE_OLDFILE))
			{
				InitIFFasDOS(Handle);

				if(!(*Error = OpenIFF(Handle,IFFF_READ)))
				{
					if(!(*Error = StopChunk(Handle,ID_ILBM,ID_BMHD)))
					{
						if(!(*Error = ParseIFF(Handle,IFFPARSE_SCAN)))
						{
							struct BitMapHeader BitMapHeader;

							if(ReadChunkBytes(Handle,&BitMapHeader,sizeof(struct BitMapHeader)) == sizeof(struct BitMapHeader))
							{
								*Width	= BitMapHeader . bmh_Width;
								*Height	= BitMapHeader . bmh_Height;

								Result	= TRUE;
							}
							else
								*Error = IoErr();
						}

						if(*Error == IFFERR_EOF || *Error == IFFERR_EOC)
							*Error = 0;
					}

					CloseIFF(Handle);
				}

				Close((BPTR)Handle -> iff_Stream);
			}
			else
				*Error = IoErr();
		}

		FreeIFF(Handle);
	}
	else
		*Error = ERR_NO_MEM;

	return(Result);
}

STATIC BOOL __regargs
GetPicture_DPI(STRPTR Name,LONG *DPI_X,LONG *DPI_Y,LONG *Error)
{
	Object	*Picture;
	BOOL	 Result = FALSE;

	*Error = 0;

	if(Picture = NewDTObject(Name,
		DTA_SourceType,	DTST_FILE,
		DTA_GroupID,	GID_PICTURE,

		PDTA_Remap,	FALSE,
	TAG_DONE))
	{
		if(DoMethod(Picture,DTM_PROCLAYOUT,NULL,TRUE))
		{
			struct BitMapHeader *BitMapHeader = NULL;

			if(GetDTAttrs(Picture,PDTA_BitMapHeader,&BitMapHeader,TAG_DONE))
			{
				if(BitMapHeader)
				{
					if(BitMapHeader -> bmh_XAspect && BitMapHeader -> bmh_YAspect)
						*DPI_Y = (*DPI_X * BitMapHeader -> bmh_XAspect) / BitMapHeader -> bmh_YAspect;
				}
			}
		}
		else
			*Error = ERR_FILE_FORMAT_ERROR;

		DisposeDTObject(Picture);
	}
	else
		*Error = IoErr();

	return(Result);
}

STATIC BOOL __regargs
GetILBM_DPI(STRPTR Name,LONG *DPI_X,LONG *DPI_Y,LONG *Error)
{
	STATIC LONG Stops[] =
	{
		ID_ILBM,ID_BMHD,
		ID_ILBM,ID_DPI
	};

	struct IFFHandle	*Handle;
	BOOL			 Result = FALSE;
	LONG			 ClipID;

	*Error = 0;

	if(Handle = AllocIFF())
	{
		if(GetClipID(Name,&ClipID))
		{
			if(Handle -> iff_Stream = (ULONG)OpenClipboard(ClipID))
			{
				InitIFFasClip(Handle);

				if(!(*Error = OpenIFF(Handle,IFFF_READ)))
				{
					if(!(*Error = StopChunks(Handle,Stops,2)))
					{
						struct ContextNode	*Chunk;
						struct BitMapHeader	 BitMapHeader;
						struct DPIHeader	 DPIHeader;

						while(!(*Error = ParseIFF(Handle,IFFPARSE_SCAN)))
						{
							Chunk = CurrentChunk(Handle);

							switch(Chunk -> cn_ID)
							{
								case ID_BMHD:

									if(ReadChunkBytes(Handle,&BitMapHeader,sizeof(struct BitMapHeader)) == sizeof(struct BitMapHeader))
									{
										if(BitMapHeader . bmh_XAspect && BitMapHeader . bmh_YAspect)
											*DPI_Y = (*DPI_X * BitMapHeader . bmh_XAspect) / BitMapHeader . bmh_YAspect;
									}
									else
										*Error = ERROR_SEEK_ERROR;

									break;

								case ID_DPI:

									if(ReadChunkBytes(Handle,&DPIHeader,sizeof(struct DPIHeader)) == sizeof(struct DPIHeader))
									{
										*DPI_X = DPIHeader . DPI_X;
										*DPI_Y = DPIHeader . DPI_Y;
									}
									else
										*Error = ERROR_SEEK_ERROR;

									break;
							}
						}

						if(*Error == IFFERR_EOF || *Error == IFFERR_EOC)
							*Error = 0;
					}

					CloseIFF(Handle);
				}

				CloseClipboard((struct ClipboardHandle *)Handle -> iff_Stream);
			}
			else
				*Error = ERR_NO_MEM;
		}
		else
		{
			if(Handle -> iff_Stream = (ULONG)Open(Name,MODE_OLDFILE))
			{
				InitIFFasDOS(Handle);

				if(!(*Error = OpenIFF(Handle,IFFF_READ)))
				{
					if(!(*Error = StopChunks(Handle,Stops,2)))
					{
						struct ContextNode	*Chunk;
						struct BitMapHeader	 BitMapHeader;
						struct DPIHeader	 DPIHeader;

						while(!(*Error = ParseIFF(Handle,IFFPARSE_SCAN)))
						{
							Chunk = CurrentChunk(Handle);

							switch(Chunk -> cn_ID)
							{
								case ID_BMHD:

									if(ReadChunkBytes(Handle,&BitMapHeader,sizeof(struct BitMapHeader)) == sizeof(struct BitMapHeader))
									{
										if(BitMapHeader . bmh_XAspect && BitMapHeader . bmh_YAspect)
											*DPI_Y = (*DPI_X * BitMapHeader . bmh_XAspect) / BitMapHeader . bmh_YAspect;
									}
									else
										*Error = ERROR_SEEK_ERROR;

									break;

								case ID_DPI:

									if(ReadChunkBytes(Handle,&DPIHeader,sizeof(struct DPIHeader)) == sizeof(struct DPIHeader))
									{
										*DPI_X = DPIHeader . DPI_X;
										*DPI_Y = DPIHeader . DPI_Y;
									}
									else
										*Error = ERROR_SEEK_ERROR;

									break;
							}
						}

						if(*Error == IFFERR_EOF || *Error == IFFERR_EOC)
							*Error = 0;
					}

					CloseIFF(Handle);
				}

				Close((BPTR)Handle -> iff_Stream);
			}
			else
				*Error = IoErr();
		}

		FreeIFF(Handle);
	}
	else
		*Error = ERR_NO_MEM;

	return(Result);
}

VOID __regargs
DeleteImage(struct GreyImage *Image)
{
	if(Image)
	{
		LibDeletePool(Image -> Pool);

		FreeVecPooled(Image);
	}
}

struct GreyImage * __regargs
ReadImage(STRPTR Name,BOOL PatchColours,LONG *Error)
{
	struct GreyImage *Image;

	*Error = 0;

	if(AP_Application)
		set(AP_Application,MUIA_Application_Sleep,TRUE);

	if(!(Image = ReadILBM(Name,PatchColours,Error)))
	{
		if(DataTypesBase && !GetClipID(Name,NULL))
			Image = ReadPicture(Name,PatchColours,Error);
	}

	if(AP_Application)
		set(AP_Application,MUIA_Application_Sleep,FALSE);

	return(Image);
}

BOOL __regargs
GetImageSize(STRPTR Name,LONG *Width,LONG *Height,LONG *Error)
{
	BOOL Result;

	if(!(Result = GetILBMSize(Name,Width,Height,Error)))
	{
		if(DataTypesBase && !GetClipID(Name,NULL))
			Result = GetPictureSize(Name,Width,Height,Error);
	}

	return(Result);
}

BOOL __regargs
GetImageDPI(STRPTR Name,LONG *DPI_X,LONG *DPI_Y,LONG *Error)
{
	BOOL Result;
	LONG LocalError = 0;

	if(!Error)
		Error = &LocalError;

	if(!(Result = GetILBM_DPI(Name,DPI_X,DPI_Y,Error)))
	{
		if(DataTypesBase && !GetClipID(Name,NULL))
			Result = GetPicture_DPI(Name,DPI_X,DPI_Y,Error);
	}

	return(Result);
}
