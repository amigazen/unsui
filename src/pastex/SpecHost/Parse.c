/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heßmann
*/

/*
** Parse.c
**
** latest revision: 7 Oct 1994, by Giuseppe Ghibò
*/

#include "Global.h"

STATIC VOID __regargs Parse_PSLit(STRPTR SpecialString, struct parse_result *Result);
BOOL __regargs Add_Extra_Transf(int transformation, float arg_x, float arg_y);
BOOL __regargs Del_Extra_Transf(int transformation);
VOID __regargs Init_Extra_Transf(VOID);

LONG	psfig_status = PSFIG_OFF;
STATIC	BYTE psfig_name[MAX_FILENAME_LEN] = { '\0' };
struct	psfig_data psfig_data;

enum	{	ARG_IFFFILE,ARG_PSFILE,ARG_HSIZE,ARG_VSIZE,ARG_HOFFSET,
		ARG_VOFFSET,ARG_SCALE,ARG_HSCALE,ARG_VSCALE,ARG_ANGLE,ARG_CLIP,ARG_MODE,
		ARG_BRIGHT,ARG_CONTRAST,ARG_GAMMA,ARG_RED,ARG_GREEN,ARG_BLUE,
		ARG_TRANSFER,ARG_RENDERING,ARG_INVERT,ARG_BASEDPI,ARG_THRESHOLD,
		ARG_PATCHCOLOURS,ARG_LLX,ARG_LLY,ARG_URX,ARG_URY,ARG_RWI,ARG_RHI,
		ARG_DITHEROPT,ARG_PSINITSTRING,ARG_PSINITFILE,ARG_HEADER,ARG_PSLIT,
//              ARG_PSLITQUOTE,
		ARGCOUNT
	};

#define TEMPLATE "IMAGE=IFFFILE/K,PSFILE/K,HSIZE/K,VSIZE/K,HOFFSET/K,VOFFSET/K,SCALE=/K,HSCALE/K,VSCALE/K,ANGLE/K,CLIP/S,MODE/K,BRIGHT/K,CONTRAST/K,GAMMA/K,RED/K,GREEN/K,BLUE/K,TRANSFER/K,RENDER/K,INVERT/K,BASEDPI/K,THRESHOLD/K,PATCH=PATCHCOLOURS/S,LLX/K,LLY/K,URX/K,URY/K,RWI/K,RHI/K,DITHEROPT/K/N,PSINITSTRING/K,PSINITFILE/K,HEADER=PROLOG/K,PS:=PS::/F"

STATIC BOOL __regargs
GetDimension(STRPTR Argument, float *Storage, float Unit, LONG DVI_mag)
{
	UBYTE LocalBuffer[40];
	LONG Len;
	float mag = (float)DVI_mag / 1000.0;

	memcpy(LocalBuffer,Argument,39);

	LocalBuffer[39] = 0;

	Len = strlen(LocalBuffer);

	while(Len > 0 && LocalBuffer[Len - 1] == ' ')
		Len--; /* remove trailing spaces */

	LocalBuffer[Len] = 0;

	if(Len > 2) /* if(Len > 2 && isalpha(LocalBuffer[Len-1])) */
	{
		STATIC struct { STRPTR Name; float Unit; } UnitTable[] =
		{
			"pt",	72.27,
			"pc",	6.0225,     /* 72.27/12.0 */
			"in",	1.0,
			"bp",	72.0,
			"cm",	2.54,
			"mm",	25.4,
			"dd",	67.5415105, /* 72.27*1157.0/1238.0 */
			"cc",	5.6284592,  /* 72.27*1157.0/1238.0/12.0 */
                        "sp",   4736286.72  /* 72.27 * 65536.0 */
		};

		LONG i;

		for(i = 0 ; i < 9 ; i++)
		{
			if(!Stricmp(&LocalBuffer[Len - 2],UnitTable[i] . Name))
			{
				Unit = UnitTable[i] . Unit;

				if (Len > 6)
				{
					if (!strncmp(&LocalBuffer[Len - 6], "true", 4))
					{
						Unit *= mag;
						LocalBuffer[Len - 6] = 0;
					}
				}
				else
					LocalBuffer[Len - 2] = 0;

				break;
			}
		}
	}

	*Storage = atof(LocalBuffer);

	switch(_FPERR)
	{
		case 0:

			*Storage /= Unit;

			return(TRUE);

		case _FPEUND:

			PrintLine("\33bFloating point underflow \"%s\".\33n",Argument);
			return(FALSE);

		case _FPEOVF:

			PrintLine("\33bFloating point overflow \"%s\".\33n",Argument);
			return(FALSE);

		case _FPEZDV:

			PrintLine("\33bDivision by zero \"%s\".\33n",Argument);
			return(FALSE);

		case _FPENAN:

			PrintLine("\33bInvalid operation \"%s\".\33n",Argument);
			return(FALSE);

		case _FPECOM:

			PrintLine("\33bFloating point value not comparable \"%s\".\33n",Argument);
			return(FALSE);
	}
}

STATIC BOOL __regargs
GetScale(STRPTR Argument,float *Storage, float Unit)
{
	*Storage = atof(Argument);

	switch(_FPERR)
	{
		case 0:

			if(*Storage <= 0.0)
			{
				PrintLine("\33bIllegal scale value \"%s\".\33n",Argument);

				return(FALSE);
			}
			else
			{
				*Storage /= Unit;

				return(TRUE);
			}

		case _FPEUND:

			PrintLine("\33bFloating point underflow \"%s\".\33n",Argument);
			return(FALSE);

		case _FPEOVF:

			PrintLine("\33bFloating point overflow \"%s\".\33n",Argument);
			return(FALSE);

		case _FPEZDV:

			PrintLine("\33bDivision by zero \"%s\".\33n",Argument);
			return(FALSE);

		case _FPENAN:

			PrintLine("\33bInvalid operation \"%s\".\33n",Argument);
			return(FALSE);

		case _FPECOM:

			PrintLine("\33bFloating point value not comparable \"%s\".\33n",Argument);
			return(FALSE);
	}
}

STATIC BOOL __regargs
GetInteger(STRPTR Argument,LONG *Storage,LONG Min,LONG Max)
{
	if(StrToLong(Argument,Storage) < 1)
	{
		PrintLine("\33bIllegal value \"%s\".\33n",Argument);

		return(FALSE);
	}
	else
	{
		if(Min != Max)
		{
			if(*Storage < Min)
				*Storage = Min;
			else
			{
				if(*Storage > Max)
					*Storage = Max;
			}
		}

		return(TRUE);
	}
}

BOOL
ParseSpecial(STRPTR OldString,struct parse_result *Result)
{
	STRPTR NewString;
	LONG Len = strlen(OldString);
	BOOL Success = FALSE;

	if(NewString = (STRPTR)AllocVecPooled(Len + 2,MEMF_ANY))
	{
		STRPTR Args[ARGCOUNT];
		struct RDArgs *ArgsPtr;

		memset(Args,0,sizeof(Args));

		while(*OldString == ' ')
			OldString++;

		strcpy(NewString,OldString);

		Len = strlen(NewString);

		while(Len > 0 && NewString[Len - 1] == ' ')
			Len--;

		NewString[Len] = '\n';
		NewString[Len + 1] = 0;

		if(ArgsPtr = (struct RDArgs *)AllocDosObjectTags(DOS_RDARGS,TAG_DONE))
		{
				/* Don't prompt for input! */
	
			ArgsPtr -> RDA_Flags |= RDAF_NOPROMPT;
	
				/* Set up for local parsing. */
	
			ArgsPtr -> RDA_Source . CS_Buffer	= NewString;
			ArgsPtr -> RDA_Source . CS_Length	= Len + 1;
			ArgsPtr -> RDA_Source . CS_CurChr	= 0;
	
				/* Read the arguments. */
	
			if(ReadArgs(TEMPLATE,(LONG *)Args,ArgsPtr))
			{
				BOOL	PostScript;
				float	Unit;
				float	ScaleUnit;

				if(Args[ARG_PATCHCOLOURS])
					Result -> patch_colours = TRUE;

				if(Args[ARG_DITHEROPT])
					Result -> dither_opt = *(LONG *)Args[ARG_DITHEROPT];

				if(Args[ARG_PSINITSTRING])
					strcpy(Result -> psinit_string,Args[ARG_PSINITSTRING]);

				if(Args[ARG_PSINITFILE])
					strcpy(Result -> psinit_file,Args[ARG_PSINITFILE]);

				if(Args[ARG_IFFFILE] && Args[ARG_PSFILE])
					PrintLine("\33bYou can either use \"IFFFILE=...\" or \"PSFILE=...\" but not both.\33n");
				else
				{
					Success = TRUE;

					if(Args[ARG_IFFFILE])
					{
						strcpy(Result -> iffile,Args[ARG_IFFFILE]);

						Unit = 1.0;	// `small points'

						ScaleUnit = 1.0;

						PostScript = FALSE;
					}

					if(Args[ARG_PSFILE])
					{
						strcpy(Result -> psfile,Args[ARG_PSFILE]);

						Unit = 72.0;	// `big points'

						ScaleUnit = 100.0; // percentage unit

						PostScript = TRUE;
					}
				}

				if(Args[ARG_HEADER])
					Add_PSHeaderName(Args[ARG_HEADER]);

				if(Args[ARG_PSLIT])
				{
					Parse_PSLit(Args[ARG_PSLIT], Result);
				}

				if(Args[ARG_HSIZE])
				{
					if(Success = GetDimension(Args[ARG_HSIZE], &Result -> hsize, Unit, Result -> DVI_mag))
						Result -> gotcontrol |= GOT_HSIZE;
				}

				if(Args[ARG_VSIZE] && Success)
				{
					if(Success = GetDimension(Args[ARG_VSIZE], &Result -> vsize, Unit, Result -> DVI_mag))
						Result -> gotcontrol |= GOT_VSIZE;
				}

				if(Args[ARG_HOFFSET] && Success)
				{
					if(Success = GetDimension(Args[ARG_HOFFSET], &Result -> hoffset, Unit, Result -> DVI_mag))
						Result -> gotcontrol |= GOT_HOFFSET;
				}

				if(Args[ARG_VOFFSET] && Success)
				{
					if(Success = GetDimension(Args[ARG_VOFFSET], &Result -> voffset, Unit, Result -> DVI_mag))
						Result -> gotcontrol |= GOT_VOFFSET;
				}

				if(Args[ARG_SCALE] && Success)
				{
					if(Success = GetScale(Args[ARG_SCALE],&Result -> scale, 1.0f))
						Result -> gotcontrol |= GOT_SCALE;
				}

				if(Args[ARG_HSCALE] && Success)
				{
					if (Success = GetScale(Args[ARG_HSCALE],&Result -> hscale,ScaleUnit))
						Result -> gotcontrol |= GOT_HSCALE;
				}

				if(Args[ARG_VSCALE] && Success)
				{
					if(Success = GetScale(Args[ARG_VSCALE],&Result -> vscale,ScaleUnit))
						Result -> gotcontrol |= GOT_VSCALE;
				}

				if(Args[ARG_MODE] && Success)
				{
					STATIC struct { STRPTR Name; LONG Mode; } ModeTable[] =
					{
						"bw",		BandW,
						"gray",		FS,
						"fs",		FS,
						"burkes",	Burkes,
						"sierra",	Sierra,
						"jarvis",	JJN,
						"jjn",		JJN,
						"whitenoise",	RandomNoise,
						"bluenoise",	BlueNoise,
						"stucki",	Stucki,
						"ordered",	Ordered,
						"halftone",	Halftone,
						"random",	RandomNoise,
						"bckbrick",	BckBrick,
						"fwdbrick",	FwdBrick,
						"hexagon",	Hexagon,
						"spiraldot",	SpiralDot,
						"horizontal",	Horizontal,
						"stevenson",	StevensonArce,
						"sa",		StevensonArce
					};

					LONG i;

					Success = FALSE;

					for(i = 0 ; i < 20 ; i++)
					{
						if(!Stricmp(Args[ARG_MODE],ModeTable[i] . Name))
						{
							Result -> mode = ModeTable[i] . Mode;

							Success = TRUE;

							break;
						}
					}

					if(!Success)
						PrintLine("\33bUnknown shading mode \"%s\".\33n",Args[ARG_MODE]);
				}

				if(Args[ARG_LLX] && Success)
				{
					if(Success = GetDimension(Args[ARG_LLX], &Result -> llx, Unit, Result -> DVI_mag))
							Result -> gotcontrol |= GOT_LLX;
				}

				if(Args[ARG_LLY] && Success)
				{
					if(Success = GetDimension(Args[ARG_LLY], &Result -> lly, Unit, Result -> DVI_mag))
						Result -> gotcontrol |= GOT_LLY;
				}

				if(Args[ARG_URX] && Success)
				{
					if(Success = GetDimension(Args[ARG_URX], &Result -> urx, Unit, Result -> DVI_mag))
						Result -> gotcontrol |= GOT_URX;
				}

				if(Args[ARG_URY] && Success)
				{
					if(Success = GetDimension(Args[ARG_URY], &Result -> ury, Unit, Result -> DVI_mag))
						Result -> gotcontrol |= GOT_URY;
				}

				if(Args[ARG_RWI] && Success)
				{
					if(Success = GetDimension(Args[ARG_RWI], &Result -> rwi, Unit, Result -> DVI_mag))
						Result -> gotcontrol |= GOT_RWI;
				}

				if(Args[ARG_RHI] && Success)
				{
					if(Success = GetDimension(Args[ARG_RHI], &Result -> rhi, Unit, Result -> DVI_mag))
						Result -> gotcontrol |= GOT_RHI;
				}

				if (PostScript && Args[ARG_ANGLE])
				{
					if(Success = GetDimension(Args[ARG_ANGLE], &Result -> angle, 1.0f, 1000L))
						Result -> gotcontrol |= GOT_ANGLE;
				}

				if (PostScript && Args[ARG_CLIP])
						Result -> gotcontrol |= GOT_CLIP;

				if(Args[ARG_BRIGHT] && Success)
					Success = GetInteger(Args[ARG_BRIGHT],&Result -> bright,-100,100);

				if(Args[ARG_CONTRAST] && Success)
					Success = GetInteger(Args[ARG_CONTRAST],&Result -> contrast,-100,100);

				if(Args[ARG_GAMMA] && Success)
					Success = GetInteger(Args[ARG_GAMMA],&Result -> gamma,-100,100);

				if(Args[ARG_RED] && Success)
					Success = GetInteger(Args[ARG_RED],&Result -> red,0,100);

				if(Args[ARG_GREEN] && Success)
					Success = GetInteger(Args[ARG_GREEN],&Result -> green,0,100);

				if(Args[ARG_BLUE] && Success)
					Success = GetInteger(Args[ARG_BLUE],&Result -> blue,0,100);

				if(Args[ARG_BASEDPI] && Success)
					Success = GetInteger(Args[ARG_BASEDPI],&Result -> base_dpi,0,1000000);

				if(Args[ARG_THRESHOLD] && Success)
					Success = GetInteger(Args[ARG_THRESHOLD],&Result -> threshold,0,100);

				if(Args[ARG_TRANSFER] && Success)
				{
					STATIC struct { STRPTR Name; LONG Mode; } TransferTable[] =
					{
						"memory",	CYID_Transfer_Memory,
						"file",		CYID_Transfer_Disk,
						"none",		CYID_Transfer_None
					};

					LONG i;

					Success = FALSE;

					for(i = 0 ; i < 3 ; i++)
					{
						if(!Stricmp(Args[ARG_TRANSFER],TransferTable[i] . Name))
						{
							Result -> transfer = TransferTable[i] . Mode;

							Success = TRUE;

							break;
						}
					}

					if(!Success)
						PrintLine("\33bUnknown transfer mode \"%s\".\33n",Args[ARG_TRANSFER]);
				}

				if(Args[ARG_RENDERING] && Success)
				{
					STATIC struct { STRPTR Name; LONG Mode; } RenderTable[] =
					{
						"plain",	CYID_Render_None,
						"frame",	CYID_Render_Frame,
						"clear",	CYID_Render_Clear
					};

					LONG i;

					Success = FALSE;

					for(i = 0 ; i < 3 ; i++)
					{
						if(!Stricmp(Args[ARG_RENDERING],RenderTable[i] . Name))
						{
							Result -> rendering = RenderTable[i] . Mode;

							Success = TRUE;

							break;
						}
					}

					if(!Success)
						PrintLine("\33bUnknown render mode \"%s\".\33n",Args[ARG_RENDERING]);
				}

				if(Args[ARG_INVERT] && Success)
				{
					STATIC struct { STRPTR Name; LONG Mode; } InvertTable[] =
					{
						"on",		TRUE,
						"yes",		TRUE,
						"true",		TRUE,
						"1",		TRUE,
						"off",		FALSE,
						"no",		FALSE,
						"false",	FALSE,
						"0",		FALSE
					};

					LONG i;

					Success = FALSE;

					for(i = 0 ; i < 8 ; i++)
					{
						if(!Stricmp(Args[ARG_INVERT],InvertTable[i] . Name))
						{
							Result -> invert = InvertTable[i] . Mode;

							Success = TRUE;

							break;
						}
					}

					if(!Success)
						PrintLine("\33bUnknown invert mode \"%s\".\33n",Args[ARG_INVERT]);
				}

				FreeArgs(ArgsPtr);
			}
			else
			{
				UBYTE LocalBuffer[256];

				Fault(IoErr(),"",LocalBuffer,256);

				PrintLine("\33bParsing error: %s\33n",LocalBuffer + 2);
			}

			FreeDosObject(DOS_RDARGS,ArgsPtr);
		}

		FreeVecPooled(NewString);
	}

	return(Success);
}

/* (ghi) Parse_PSLit(): parse psfig specials and a small set of literal
   PostScript specials. */
STATIC VOID __regargs
Parse_PSLit(STRPTR SpecialString, struct parse_result *Result)
{
	BOOL Success = TRUE;
	STRPTR String, Buffer, *array;
	STATIC float new_xpos = 0.0, new_ypos = 0.0;

	if (!(String = AllocVecPooled(strlen(SpecialString)+1L, MEMF_ANY)))
		return;

	if (!(Buffer = AllocVecPooled(8192L, MEMF_ANY)))
	{
		FreeVecPooled(String);
		return;
	}

	if (!Extra_Transf)
	{
		ET_CurrentPoint_x = (float) Result -> current_x / Result -> hres;
		ET_CurrentPoint_y = (float) Result -> current_y / Result -> vres;
		new_xpos = new_ypos = 0.0;
	}
	else
	{
		new_xpos = (float) Result -> current_x / Result -> hres - ET_CurrentPoint_x;
		new_ypos = - (float) Result -> current_y / Result -> vres + ET_CurrentPoint_y;
	}

	strcpy(String, SpecialString);
	array = StrToArray(String, " ", Buffer, 8192L);
	FreeVecPooled(String);

	if (!strcmp(array[0],"gsave"))
	{
		float angle = 0.0;
		int i;
		STATIC STRPTR RotTable[] =
		{
			"gsave",
			"currentpoint",
			"currentpoint",
			"translate",
			"*",
			"neg",
			"rotate",
			"neg",
			"exch",
			"neg",
			"exch",
			"translate"
		};

		for (i = 1; i < 12; i++)
		{
			if (array[i])
			{
				if (!strcmp(RotTable[i],"*"))
				{
					angle = atof(array[i]);
					Success = TRUE;
				}
				else
				{
					if (strcmp(array[i],RotTable[i]) != 0)
					{
						PrintLine("\33bps: gsave ... Syntax Error, unknown keyword \"%s\"!\33n",array[i]);
						Success = FALSE;
						break;
					}
				}
			}
			else
			{
				PrintLine("\33bps: gsave ... not enough arguments, needed 12 found %ld!\33n",i);
				Success = FALSE;
				break;
			}
		}
		if (Success)
		{
			if (!Add_Extra_Transf(TR_TRANSLATION, new_xpos, new_ypos) ||
			    !Add_Extra_Transf(TR_ROTATION, RAD(angle), 0.0f) ||
			    !Add_Extra_Transf(TR_TRANSLATION, (float)-new_xpos, (float)-new_ypos))
			{
				PrintLine("\33bps: gsave ... can't execute extra rotation!\33n");
				Success = FALSE;
			}
			else
			{
				PrintLine("Added extra rotation.");
			}
		}
	}
	else if (!strcmp(array[0],"currentpoint"))
	{
		int count = 0;

		while (array[count])
			count++; /* count arguments */

		if (count > 15)
		{
			PrintLine("\33bps: currentpoint... Too many arguments!\33n");
			Success = FALSE;
		}
		else if (count == 3)
		{
			if (!strcmp(array[1],"grestore") && !strcmp(array[2],"moveto"))
			{
				if (!Del_Extra_Transf(TR_TRANSLATION) ||
				    !Del_Extra_Transf(TR_ROTATION) ||
				    !Del_Extra_Transf(TR_TRANSLATION))
				{
					PrintLine("\33bps: currentpoint grestore moveto, doesn't match last transformation!\33n");
					PrintLine("\33bExtra Transformation Matrix re-initialized.\33n");
					Init_Extra_Transf();
					Success = FALSE;
				}
				else
				{
					PrintLine("Removed extra rotation.");
					Success = TRUE;
				}
			}
			else
			{
				PrintLine("\33bps: currentpoint %s %s, Unknown keywords.\33n",array[1],array[2]);
				Success = FALSE;
			}

		}
		else if (count == 11)
		{
			float xscale = 0.0, yscale = 0.0;
			int i;
			STATIC STRPTR ScaleTable[] =
			{
				"currentpoint",
				"currentpoint",
				"translate",
				"*x",
				"*y",
				"scale",
				"neg",
				"exch",
				"neg",
				"exch",
				"translate"
			};

			for (i = 0; i < count; i++)
			{
				if (!strcmp(ScaleTable[i],"*x"))
				{
					xscale = atof(array[i]);
					Success = TRUE;
				}
				else if (!strcmp(ScaleTable[i],"*y"))
				{
					yscale = atof(array[i]);
					Success = TRUE;
				}
				else
				{
					if (strcmp(array[i],ScaleTable[i]) != 0)
					{
						PrintLine("\33bps: %s ... Syntax Error, unknown keyword \"%s\"!\33n",array[0],array[i]);
						Success = FALSE;
						break;
					}
				}
			}
			if (Success)
			{
				if (!Add_Extra_Transf(TR_TRANSLATION, new_xpos, new_ypos) ||
				    !Add_Extra_Transf(TR_SCALING, xscale, yscale) ||
				    !Add_Extra_Transf(TR_TRANSLATION, (float)-new_xpos, (float)-new_ypos))
				{
					PrintLine("\33bps: %s ... can't execute extra scaling!\33n",array[0]);
					Success = FALSE;
				}
				else
				{
					PrintLine("Added extra scaling.");
				}
			}
		}
		else if (count == 15)
		{
			float xunscale = 0.0, yunscale = 0.0;
			int i;
			STATIC STRPTR UnScaleTable[] =
			{
				"currentpoint",
				"currentpoint",
				"translate",
				"1",
				"*x",
				"div",
				"1",
				"*y",
				"div",
				"scale",
				"neg",
				"exch",
				"neg",
				"exch",
				"translate"
			};

			for (i = 0; i < count; i++)
			{
				if (!strcmp(UnScaleTable[i],"*x"))
				{
					xunscale = atof(array[i]);
					Success = TRUE;
				}
				else if (!strcmp(UnScaleTable[i],"*y"))
				{
					yunscale = atof(array[i]);
					Success = TRUE;
				}
				else
				{
					if (strcmp(array[i],UnScaleTable[i]) != 0)
					{
						PrintLine("\33bps: %s ... Syntax Error, unknown keyword \"%s\"!\33n",array[0],array[i]);
						Success = FALSE;
						break;
					}
				}
			}
			if (Success)
			{
				if (!Del_Extra_Transf(TR_TRANSLATION) ||
				    !Del_Extra_Transf(TR_SCALING) ||
				    !Del_Extra_Transf(TR_TRANSLATION))
				{
					PrintLine("\33bps: currentpoint ..., doesn't match last transformation!\33n");
					PrintLine("\33bExtra Transformation Matrix re-initialized.\33n");
					Init_Extra_Transf();
					Success = FALSE;
				}
				else
				{
					PrintLine("Removed extra scaling.");
				}
			}
		}
		else
		{
			PrintLine("\33bps: %s, Literal PostScript Special: Syntax Unknown!\33n",array[0]);
			Success = FALSE;
		}
	}
	else if (!Stricmp(array[0],"ps::[begin]"))
	{
		int i;
		LONG value[6];

		for (i = 1 ; i < 7; i++) {
			if (array[i])
			{
				if (StrToLong(array[i],&value[i-1]) < 1)
				{
					PrintLine("\33bps::[begin] Syntax Unknown: Illegal value \"%s\".\33n",array[i]);
					Success = FALSE;
					break;
				}
			}
			else
			{
				PrintLine("\33bps::[begin] Syntax Unknown: needed 6 argument, found %ld. \33n",i+1);
				Success = FALSE;
				break;
			}
		}

		if (Success && array[7])
		{
			if (!strcmp(array[7],"startTexFig"))
			{
				psfig_status = PSFIG_BEGIN;
				psfig_data . width  = value[0];
				psfig_data . height = value[1];
				psfig_data . llx    = value[2];
				psfig_data . lly    = value[3];
				psfig_data . urx    = value[4];
				psfig_data . ury    = value[5];
				psfig_data . angle  = 0.0;
				psfig_data . clip   = FALSE;

				FreeVecPooled(Buffer);
				return;
			}
			else
			{
				psfig_status = PSFIG_OFF;			
				PrintLine("\33bps::[begin] Needed \"startTeXFig\" keyword; psfig special aborted!\33n");
				Success = FALSE;

				FreeVecPooled(Buffer);
				return;
			}
		}
		else
		{
			psfig_status = PSFIG_OFF;
			PrintLine("\33bps::[begin] Syntax Unknown: psfig special aborted!\33n");
			Success = FALSE;
		}
	}
	else if (psfig_status == PSFIG_BEGIN)
	{
		if (array[0])
		{
			if (!Stricmp(array[0],"plotfile"))
			{
				if (!array[1])
				{
					PrintLine("\33bps: plotfile, you must specify a filename!\33n");
				}
				else
				{
					strcpy(psfig_name,array[1]);
					psfig_status = PSFIG_WAIT; /* now wait for ps::[end] */
				}
			}
			else if (!strcmp(array[0],"doclip"))
			{
				psfig_data . clip = TRUE;
			}
			else if (!strcmp(array[1],"rotate"))
			{
				psfig_data . angle += atof(array[0]);
			}
			else
				PrintLine("\33bps: Literal PostScript ignored.\33n");
		}
	}
	else if (psfig_status == PSFIG_WAIT)
	{
		if (!Stricmp(array[0],"ps::[end]"))
		{
			if (!array[1])
			{
				PrintLine("\33bps::[end] Argument expected, psfig special aborted!\33n");
				psfig_status = PSFIG_OFF;
			}
			else
			{
				if (!strcmp(array[1],"endTexFig"))
				{
					psfig_status = PSFIG_END;
					strcpy(Result -> psfile, psfig_name);
				}
				else
				{
					PrintLine("\33bps::[end] \"endTexFig\" expected, psfig special aborted!\33n");
					psfig_status = PSFIG_OFF;
				}
			}
		}
	}
	else
	{
		PrintLine("\33b%s: Literal PostScript ignored.\33n",array[0]);
	}

	FreeVecPooled(Buffer);
}

/* (ghi) StrToArray() converts a string into an array of strings. strings are
   drawed in the Buffer. */
STRPTR __regargs *StrToArray(STRPTR String, STRPTR BrkStr, UBYTE *Buffer, LONG Size)
{
	STRPTR *p = (STRPTR *)Buffer, q = Buffer + Size, s;
	LONG i = 0L;

	s = strtok(String, BrkStr);

	do
	{
		q -= (strlen(s) + 1L);

		if (&p[i] > (STRPTR *)(q - 2 * sizeof(STRPTR)))
		{
			p[i] = NULL;
			break;
		}
		else
		{
			p[i] = q;
			strcpy(p[i++], s);
		}
		
	}
	while (s = strtok(NULL, BrkStr));

	p[i] = NULL;
	return p;
}

/* (ghi) Add the transformation to the global list Extra_Transf. */
BOOL __regargs
Add_Extra_Transf(int transformation, float arg_x, float arg_y)
{
	if (Extra_Transf == NULL)
	{
		if (Extra_Transf = AllocVecPooled(sizeof(struct extra_transf), MEMF_ANY))
		{
			Extra_Transf -> prev = Extra_Transf -> next = NULL;
			Init_CTM(&Extra_Transf -> CTM);
		}
		else
			return (FALSE);
	}
	else
	{
		while (Extra_Transf -> next)
			Extra_Transf = Extra_Transf -> next;

		if (Extra_Transf -> next = AllocVecPooled(sizeof(struct extra_transf), MEMF_ANY))
		{
			Extra_Transf -> next -> prev = Extra_Transf;
			Extra_Transf = Extra_Transf -> next;
			Extra_Transf -> next = NULL;
			Copy_CTM(&Extra_Transf -> CTM, &Extra_Transf -> prev -> CTM);
		}
		else
			return (FALSE);
	}

	Extra_Transf -> type_of_transf = transformation;
	CTM_Transf(&Extra_Transf -> CTM, arg_x, arg_y, transformation);

	return (TRUE);
}

/* (ghi) we try to match transformation, otherwise return FALSE */
BOOL __regargs
Del_Extra_Transf(int transformation)
{
	if (!Extra_Transf)
		return(FALSE);

	while (Extra_Transf -> next)
		Extra_Transf = Extra_Transf -> next;
 
	if (Extra_Transf -> type_of_transf != transformation)
		return (FALSE);
	else
	{
		struct extra_transf *p = Extra_Transf;

		if (p -> prev)
		{
			p -> prev -> next = NULL;
			Extra_Transf = p -> prev;
			FreeVecPooled(p);
		}
		else
		{
			FreeVecPooled(p);
			Extra_Transf = NULL;
		}

		return(TRUE);
	}

}

VOID __regargs Init_Extra_Transf(VOID)
{
	struct extra_transf *p;

	ET_CurrentPoint_x = ET_CurrentPoint_y = 0.0f;

	if (!Extra_Transf)
		return;

	while (Extra_Transf -> next)
		Extra_Transf = Extra_Transf -> next;

	while ((p = Extra_Transf) && p -> prev)
	{
		Extra_Transf = p -> prev;
		FreeVecPooled(p);
	}

	if (Extra_Transf)
	{
		FreeVecPooled(Extra_Transf);
		Extra_Transf = NULL;
	}
}
