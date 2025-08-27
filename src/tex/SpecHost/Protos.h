/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heﬂmann
*/

	/* debug.lib */

VOID __stdargs	kprintf(STRPTR,...);

	/* Host.c */

BPTR __regargs				OpenConfigFile(STRPTR Name,LONG Mode);
BOOL __regargs				ReadConfig(struct config_struct *ConfigPtr);
VOID __regargs				SaveConfig(struct config_struct *ConfigPtr);
VOID					ChangeDrawMode(LONG Transfer,LONG Render,BOOL GetThem);
BOOL					OpenGUI(VOID);
VOID					CloseAll(VOID);
BOOL					OpenAll(VOID);
VOID __stdargs				PrintLine(STRPTR Format,...);
VOID __regargs				GetNewSize(struct special_map *SpecialMap,struct parse_result *Result,LONG *NewWidth,LONG *NewHeight,LONG DPI_X,LONG DPI_Y);
struct BitMap * __regargs		ProcessImage(struct special_msg *Message);
VOID					HandleInput(VOID);
VOID __regargs				SmallWriteLine(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp);
VOID __regargs				SmallReadLine(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp);
VOID __regargs				LargeWriteLine(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp);
VOID __regargs				LargeReadLine(struct RastPort *RPort,LONG Line,LONG Width,UBYTE *Buffer,struct RastPort *Temp);

	/* Tools.c */

VOID __regargs				FreeVecPooled(APTR Mem);
APTR __regargs				AllocVecPooled(LONG Size,ULONG Flags);
VOID __regargs				ClearList(struct List *List);
struct ListEntry * __regargs		NewEntry(STRPTR Title);
WORD __stdargs				ShowRequest(struct Window *Window,STRPTR Text,STRPTR Gadgets,...);
STRPTR __regargs			ShowError(LONG Primary,LONG Secondary,BOOL GetPrimary);
VOID __regargs				DeleteBitMap(struct BitMap *BitMap);
struct BitMap * __regargs		CreateBitMap(LONG Width,LONG Height,LONG Depth,ULONG Flags,struct BitMap *Friend);
VOID __regargs				DeleteTempLine(UBYTE *Line);
UBYTE * __regargs			CreateTempLine(LONG Width,LONG Height);
VOID __regargs				DeleteTempRPort(struct RastPort *Temp);
struct RastPort * __regargs		CreateTempRPort(struct RastPort *Source);
LONG __regargs				FileDateCheck(STRPTR File1,STRPTR File2,LONG *Error);
VOID __regargs				AddProtection(STRPTR Name,ULONG Mask);
VOID __regargs				SetMaxProgress(LONG Count);
VOID __regargs				ShowProgress(LONG Count);
LONG __regargs				GetMapCode(struct MapTable *Table,STRPTR Key);
VOID __regargs				InvertBitMap(struct BitMap *BitMap, LONG Width, LONG Height);

	/* ReadImage.c */

VOID __regargs				DeleteImage(struct GreyImage *Image);
struct GreyImage * __regargs		ReadImage(STRPTR Name,BOOL PatchColours,LONG *Error);
BOOL __regargs				GetImageSize(STRPTR Name,LONG *Width,LONG *Height,LONG *Error);
BOOL __regargs				GetImageDPI(STRPTR Name,LONG *DPI_X,LONG *DPI_Y,LONG *Error);

	/* Luminance.asm */

LONG __asm				Luminance(register __d0 WORD R,register __d1 WORD G,register __d2 WORD B);

	/* Dither.c */

LONG __regargs				ScaleImage(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight);
LONG __regargs				DitherImage_Matrix(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight,UBYTE *Matrix);
LONG __regargs				DitherImage_FS(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight);
LONG __regargs				DitherImage_Burkes(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight);
LONG __regargs				DitherImage_Sierra(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight);
LONG __regargs				DitherImage_JJN(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight);
LONG __regargs				DitherImage_Stevenson_Arce(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight);
LONG __regargs				DitherImage_Stucki(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight);
LONG __regargs				DitherImage_BlueNoise(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight,WORD NoiseLevel);

	/* Filter.c */

VOID __regargs				BrightnessFilter(UBYTE *Table,LONG Scale);
VOID __regargs				ContrastFilter(UBYTE *Table,LONG Scale);
VOID __regargs				GammaFilter(UBYTE *Table,LONG Scale);

	/* Post.c */

struct BitMap * __regargs		ProcessPostscript(struct special_msg *Message,struct special_map *SpecialMap,struct parse_result *Result,LONG *Error);
VOID __regargs 				PSGetSizeDot(struct parse_result *Result, LONG *width, LONG *height, LONG *hoff, LONG *voff, LONG *hoff_cp, LONG *voff_cp);
VOID __regargs				Add_PSHeaderName(STRPTR Name);
VOID __regargs				Init_PSHeaders(VOID);
VOID __regargs				CTM_Transf(struct ctm *ctm, float x, float y, int transformation);
VOID __regargs				Init_CTM(struct ctm *ctm);
VOID __regargs				Copy_CTM(struct ctm *dest, struct ctm *source);
VOID __regargs				Init_FontMapName(VOID);
VOID __regargs				Free_FontMapName(VOID);

	/* CRC.asm */

LONG __asm				DoCRC(register __a0 APTR Data,register __d0 LONG Size);

	/* Parse.c */

STRPTR * __regargs 			StrToArray(STRPTR String, STRPTR BrkStr, UBYTE *Buffer, LONG Size);
