/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heßmann
**
*/

/*
** Post.c
**
** Improvements to the PostScript support written by Giuseppe Ghibò.
**
** latest revision: 13 Jun 1995 (ghi)
*/

#include "Global.h"
#include "postlib.h"

/*#define DEBUG*/

STATIC VOID __regargs PSGetSize_S(struct parse_result *Result, float *width, float *height, float *hoff, float *voff, float *hoff_cp, float *voff_cp);
STATIC VOID __regargs PSGetSize_P(float *width, float *height, float *hoff, float *voff, float *hoff_cp, float *voff_cp, LONG DVI_mag);
STATIC BOOL __regargs FindBBox(STRPTR, struct bbox *);
STATIC VOID __regargs Point_Transf(struct ctm *, float, float, float *, float *);
STATIC VOID __regargs bb_new(struct ctm *, struct bbox *, struct bbox *);
STATIC BOOL __regargs Is_MPfile(STRPTR);
STATIC STRPTR __regargs FontMapAlias(STRPTR font_name);

#define MAXPSHEADERS 256
#define TEMPSTRLEN MAX_FILENAME_LEN
#define MAXMAPENTRIES 8192L

STATIC STRPTR PSHeaders[MAXPSHEADERS] = { NULL };
STATIC UBYTE TempStr[MAX_FILENAME_LEN];

struct extra_transf *Extra_Transf;
float	ET_CurrentPoint_x = 0.0f,
	ET_CurrentPoint_y = 0.0f; /* point on page where extra transformations begin. */

struct Library *PSbase;
STATIC struct PSparm	 PSparm;
STATIC struct BitMap	*BitMap;
STATIC LONG		 ActivationRecord;
STATIC BOOL		 Transferred;

#ifdef DEBUG
STATIC struct Screen	*LocalScreen;
STATIC struct Window	*LocalWindow;
#endif	/* DEBUG */

extern void insertftrap(void);
extern void deleteftrap(void);

STATIC VOID __saveds
CopyPage(VOID)
{
	Transferred = TRUE;

#ifdef DEBUG
	if(LocalScreen)
	{
		struct IntuiMessage *IntuiMessage;

		CopyMem(BitMap -> Plane[0], LocalScreen -> RastPort . BitMap -> Planes[0], LocalScreen -> RastPort . BitMap -> BytesPerRow * LocalScreen -> RastPort . BitMap -> Rows);

		ScreenToFront(LocalScreen);

		WaitPort(LocalWindow -> UserPort);

		while(IntuiMessage = (struct IntuiMessage *)GetMsg(LocalWindow -> UserPort))
			ReplyMsg(IntuiMessage);
	}
#endif	/* DEBUG */
}

void __saveds sigfpe(void)
{
	PSsignalfpe(ActivationRecord);
}

void __saveds sigint(void)
{
	PSsignalint(ActivationRecord,1);
}

/* (ghi) the function ProcessPostscript() is slightly changed from the one
   provided by Olaf. Now we pass to the PostScript interpreter part of the
   same code used by dvips. Of course, to place the PostScript picture in
   the same (or similar) condition as if it was inside a PostScript file
   created by dvips, we need to pass a PostScript ``glue code''. Then to
   calculate the correct size of the picture according to the special used,
   we have to determine the PostScript current transformation matrix. These
   operations are performed by the function PSGetSizeDot() */

struct BitMap * __regargs
ProcessPostscript(struct special_msg *Message,struct special_map *SpecialMap,struct parse_result *Result,LONG *Error)
{
	*Error = 0;
	
	BitMap = NULL;

	if(PSbase = OpenLibrary("post.library",15))
	{
		BPTR NIL;

		if(NIL = Open("NIL:",MODE_READWRITE))
		{
			LONG	NewWidth,
				NewHeight,
				H_OffSet,
				V_OffSet,
				X_DPI = Message -> hresolution,
				Y_DPI = Message -> vresolution;

			float	CurrentPoint_x = 0.0f, CurrentPoint_y = 0.0f;
			STRPTR special_string = NULL, sp;

			memset(&PSparm,0,sizeof(struct PSparm));

			PSGetSizeDot(Result, &NewWidth, &NewHeight, &H_OffSet, &V_OffSet, &SpecialMap -> hoffset, &SpecialMap -> voffset);
			SpecialMap -> width	= NewWidth;
			SpecialMap -> height	= NewHeight;
			
			if ((special_string = AllocVecPooled(1024, MEMF_ANY)) == NULL)
			{
				*Error = ERR_NO_MEM;
				Close(NIL);
				CloseLibrary(PSbase);
				PSbase = NULL;

				if (psfig_status == PSFIG_END)
					psfig_status = PSFIG_OFF;

				return(NULL);
			}

			sp = special_string;

			sprintf(sp,"TeXDict begin %ld %ld %ld %ld %ld (noname) @start\n", \
				(LONG) (INTOSP((float) NewWidth/X_DPI) + 0.5), \
				(LONG) (INTOSP((float) NewHeight/Y_DPI) + 0.5), \
				Result -> DVI_mag, X_DPI, Y_DPI);
			sp += strlen(sp);
			sprintf(sp,"1 0 bop\nResolution neg VResolution neg vsize round -72 div 1 add mul a ");
			sp += strlen(sp);

			if (psfig_status == PSFIG_END)
			{
				sprintf(sp,"%ld %ld %ld %ld %ld %ld startTexFig\n", psfig_data . width, psfig_data . height, \
					 psfig_data .llx, psfig_data . lly, psfig_data . urx, psfig_data . ury);

				if (psfig_data . clip)
				{
					sp += strlen(sp);
					sprintf(sp,"doclip\n");
				}

				if (psfig_data . angle != 0.0)
				{
					sp += strlen(sp);
					sprintf(sp,"%g rotate\n", psfig_data . angle);
				}
			}
			else
				sprintf(sp,"@beginspecial\n");

			sp += strlen(sp);

			if (Extra_Transf)
			{
				CurrentPoint_x = (float) Result -> current_x / Result -> hres - ET_CurrentPoint_x;
				CurrentPoint_y = - (float) Result -> current_y / Result -> vres + ET_CurrentPoint_y;

				sprintf(sp,"[%g %g %g %g %g %g] concat %g %g TR\n",
					Extra_Transf -> CTM . a,
					Extra_Transf -> CTM . b,
					Extra_Transf -> CTM . c,
					Extra_Transf -> CTM . d,
					INTOBP(Extra_Transf -> CTM . tx),
					INTOBP(Extra_Transf -> CTM . ty),
					INTOBP(CurrentPoint_x),
					INTOBP(CurrentPoint_y));
				sp += strlen(sp);
			}

			if(Result -> gotcontrol & GOT_HSIZE)
			{
				sprintf(sp,"%g @hsize ", INTOBP(Result -> hsize));
				sp += strlen(sp);
			}

			if(Result -> gotcontrol & GOT_VSIZE)
			{
				sprintf(sp,"%g @vsize ", INTOBP(Result -> vsize));
				sp += strlen(sp);
			}

			if(Result -> gotcontrol & GOT_HSCALE)
			{
				sprintf(sp,"%g @hscale ", Result -> hscale * 100.0);
				sp += strlen(sp);
			}

			if(Result -> gotcontrol & GOT_VSCALE)
			{
				sprintf(sp,"%g @vscale ", Result -> vscale * 100.0);
				sp += strlen(sp);
			}

			if(Result -> gotcontrol & GOT_SCALE)
			{
				sprintf(sp,"%g @hscale %g @vscale ", Result -> scale * 100.0, Result -> scale * 100.0);
				sp += strlen(sp);
			}

			if(Result -> gotcontrol & GOT_HOFFSET)
			{
				sprintf(sp,"%g @hoffset ", INTOBP(Result -> hoffset));
				sp += strlen(sp);
			}

			if(Result -> gotcontrol & GOT_VOFFSET)
			{
				sprintf(sp,"%g @voffset ", INTOBP(Result -> voffset));
				sp += strlen(sp);
			}

			if (Result -> gotcontrol & GOT_ANGLE)
			{
				sprintf(sp,"%g @angle ", Result -> angle);
				sp += strlen(sp);
			}

			if (Result -> gotcontrol & GOT_CLIP)
			{
				sprintf(sp,"@clip ");
				sp += strlen(sp);
			}

			if((Result -> gotcontrol & (GOT_LLX | GOT_LLY | GOT_URX | GOT_URY)) == (GOT_LLX | GOT_LLY | GOT_URX | GOT_URY))
			{
				sprintf(sp,"%g @llx %g @lly %g @urx %g @ury ",
					INTOBP(Result -> llx), INTOBP(Result -> lly),
					INTOBP(Result -> urx), INTOBP(Result -> ury));
				sp += strlen(sp);

				if (Result -> gotcontrol & GOT_RWI)
				{
					sprintf(sp,"%g @rwi ", INTOBP(Result -> rwi));
					sp += strlen(sp);
				}
				if (Result -> gotcontrol & GOT_RHI)
				{
					sprintf(sp,"%g @rhi ", INTOBP(Result -> rhi));
					sp += strlen(sp);
				}
			}

			if (psfig_status != PSFIG_END)
			{
				sprintf(sp,"@setspecial\n");
				sp += strlen(sp);
			}
	
			if(NewWidth > 30000 || NewHeight > 30000)
				*Error = ERR_TOO_LARGE;
			else
			{
				if(NewWidth < 1 || NewHeight < 1)
					*Error = ERR_TOO_SMALL;
				else
				{
					PrintLine("Page size %ld × %ld, %ld × %ld DPI",NewWidth,NewHeight,Message -> hresolution,Message -> vresolution);

					if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMAP_MEMF_ANY,NULL))
					{
						LONG ResultCode;

						insertftrap();

						PSparm . page . depth	= 1;
						PSparm . page . ydir	= -1;
						PSparm . page . xden	= X_DPI;
						PSparm . page . yden	= Y_DPI;
						PSparm . page . xsize	= NewWidth;
						PSparm . page . ysize	= NewHeight;
						PSparm . page . yheight	= NewHeight;
						PSparm . page . xbytes	= BitMap -> BytesPerRow;
						PSparm . page . xoff	= H_OffSet;
						PSparm . page . yoff	= V_OffSet;
						PSparm . page . len	= PSparm . page . xbytes * PSparm . page . ysize;
						PSparm . page . buf[0]	= BitMap -> Planes[0]; /* (ghi) to store the PS picture in the same memory area passed to ShowDVI */

						PSparm . copyfunc	= (APTR)CopyPage;
						PSparm . infh		= NIL;
						PSparm . outfh		= NIL;
						PSparm . errfh		= NIL;

						ActivationRecord = PScreateact(&PSparm);

						if(ActivationRecord <= errmax)
						{
							if(ActivationRecord)
								*Error = 20000 + ActivationRecord;
							else
								*Error = ERR_NO_MEM;
						}
						else
						{
							ULONG Flags = PSFLAGCLEAR | PSFLAGERASE;
#ifdef DEBUG
							if(LocalScreen = OpenScreenTags(NULL,
								SA_Width,	NewWidth,
								SA_Height,	NewHeight,
								SA_Depth,	1,
								SA_DisplayID,	HIRESLACE_KEY,
								SA_Quiet,	TRUE,
								SA_AutoScroll,	TRUE,
								SA_ShowTitle,	FALSE,
								SA_Overscan,	OSCAN_STANDARD,
							TAG_DONE))
							{
								if(!(LocalWindow = OpenWindowTags(NULL,
									WA_Left,	0,
									WA_Top,		0,
									WA_Width,	LocalScreen -> Width,
									WA_Height,	LocalScreen -> Height,
									WA_RMBTrap,	TRUE,
									WA_Backdrop,	TRUE,
									WA_Borderless,	TRUE,
									WA_CustomScreen,LocalScreen,
									WA_Activate,	TRUE,
									WA_IDCMP,	IDCMP_VANILLAKEY,
								TAG_DONE)))
								{
									CloseScreen(LocalScreen);

									LocalScreen = NULL;
								}
							}
#endif	/* DEBUG */
							Transferred = FALSE;

							if(Result -> psinit_file[0] && !(*Error))
							{
								STRPTR s;

								if (s = EVP_FileSearch(Result -> psinit_file, psheaders_var, TempStr, TEMPSTRLEN))
								{
									PrintLine("Executing initialization file \"%s\"...", s);

									if(ResultCode = PSintstring(ActivationRecord,s,-1,PSFLAGFILE | Flags))
										*Error = 20000 + ResultCode;
									else
										Flags = NULL;
								}
								else
								{
									PrintLine("\33bInitialization file \"%s\" not found.\33n", Result -> psinit_file);
									*Error = 19999;
								}
							}

							if(Result -> psinit_string[0] && !(*Error))
							{
								PrintLine("Executing initialization string...");

								if(ResultCode = PSintstring(ActivationRecord,Result -> psinit_string,strlen(Result -> psinit_string),PSFLAGSTRING | Flags))
									*Error = 20000 + ResultCode;
								else
									Flags = NULL;
							}

							if(!(*Error))
							{
								STRPTR s;

								if (s = EVP_FileSearch("tex.pro", psheaders_var, TempStr, TEMPSTRLEN))
								{
									PrintLine("Executing file \"%s\"...",s);

									if(ResultCode = PSintstring(ActivationRecord,s,-1,PSFLAGFILE | Flags))
										*Error = 20000 + ResultCode;
									else
										Flags = NULL;
								}
								else
								{
									PrintLine("\33bFile \"tex.pro\" not found.\33n");
									*Error = 19999;
								}
							}

							if(!(*Error) && PSHeaders[0]) /* (ghi) we pass the headers (if there is at least one) to the PostScript interpreter */
							{
								STRPTR s;
								int i = 0;

								while (PSHeaders[i] && PSHeaders[i][0])
								{
									if (s = EVP_FileSearch(PSHeaders[i], psheaders_var, TempStr, TEMPSTRLEN))
									{
										PrintLine("Executing prologue file \"%s\"...", s);

										if(ResultCode = PSintstring(ActivationRecord,s,-1,PSFLAGFILE | Flags))
										{
											*Error = 20000 + ResultCode;
											break;
										}
										else
											Flags = NULL;
									}
									else
									{
										PrintLine("\33bPrologue file \"%s\" not found.\33n", PSHeaders[i]);
										*Error = 19999;
										break;
									}
									i++;
								}
							}

							if(!(*Error))
							{
								STRPTR s;

								if (s = EVP_FileSearch("special.pro", psheaders_var, TempStr, TEMPSTRLEN))
								{
									PrintLine("Executing file \"%s\"...", s);

									if(ResultCode = PSintstring(ActivationRecord,s,-1,PSFLAGFILE | Flags))
										*Error = 20000 + ResultCode;
									else
										Flags = NULL;
								}
								else
								{
									PrintLine("\33bFile \"special.pro\" not found.\33n");
									*Error = 19999;
								}
							}

							if (!(*Error))
							{	/* (ghi) pass to the interpreter some PostScript glue code (see comment above) */
								if (psfig_status == PSFIG_END)
									PrintLine("Executing glue string for psfig special (startTexFig)...");
								else
									PrintLine("Executing glue string (@beginspecial)...");

								if(ResultCode = PSintstring(ActivationRecord,special_string,strlen(special_string),PSFLAGSTRING | Flags))
									*Error = 20000 + ResultCode;
								else
									Flags = NULL;
							}

							if(!(*Error))
							{
								if (Is_MPfile(Result -> psfile))
								{
									BPTR fh;
									UBYTE *buf, *mpbuf, *mpbufend;
									STRPTR p, q, s;
									LONG nfont = 0L;

									Init_FontMapName();
									if (fh = Open(Result -> psfile, MODE_OLDFILE))
									{
										if (mpbuf = AllocVecPooled(8192L, MEMF_ANY))
										{
											s = mpbuf;
											mpbufend = mpbuf + 8192L;

											if (buf = AllocVecPooled(256, MEMF_ANY))
											{
												while (FGets(fh, buf, 256) && !strstr(buf, "%%EndProlog"))
												{
													if (p = strstr(buf, "%*Font:"))
													{
														p += 7;
														q = strtok(p, " \t");
														if (q && *q && s < mpbufend)
														{
															s += sprintf(s,"/%s /%s def\n", q, FontMapAlias(q));
															nfont++;
														}
													}
												}
												if (nfont > 0L)
												{
													sprintf(s, "/fshow {exch findfont exch scalefont setfont show}bind def\n");

													PrintLine("Executing glue code for a METAPOST file...");

													if (ResultCode = PSintstring(ActivationRecord, mpbuf, strlen(mpbuf), PSFLAGSTRING | Flags))
														*Error = 20000 + ResultCode;
													else
														Flags = NULL;
												}
												FreeVecPooled(buf);
											}
											FreeVecPooled(mpbuf);
										}
										Close(fh);
									}
								}

								PrintLine("Executing file \"%s\"...",Result -> psfile);

								if(ResultCode = PSintstring(ActivationRecord,Result -> psfile,-1,PSFLAGFILE | Flags))
									*Error = 20000 + ResultCode;
/*
								else
								{
									if(!Transferred)
									{
										if(ResultCode = PSintstring(ActivationRecord,"showpage",8,PSFLAGSTRING))
											*Error = 20000 + ResultCode;
									}										
								}
*/
							}

							if (!(*Error))
							{	/* (ghi) pass to the interpreter some PostScript glue code (see comment above) */
								if (psfig_status == PSFIG_END)
								{
									PrintLine("Executing glue string for psfig special (endTexFig)...");
									sprintf(special_string,"endTexFig SI restore userdict /eop-hook known{eop-hook}if end userdict /end-hook known{end-hook}if\n");
								}
								else
								{
									PrintLine("Executing glue string (@endspecial)...");
									sprintf(special_string,"@endspecial SI restore userdict /eop-hook known{eop-hook}if end userdict /end-hook known{end-hook}if\n");
								}

								if(ResultCode = PSintstring(ActivationRecord,special_string,strlen(special_string),PSFLAGSTRING | Flags))
									*Error = 20000 + ResultCode;
								else
									Flags = NULL;

								if(psfig_status == PSFIG_END)
									psfig_status = PSFIG_OFF; /* re-init psfig_status */
							}

							PSdeleteact(ActivationRecord);
#ifdef DEBUG
							if(LocalWindow)
							{
								CloseWindow(LocalWindow);

								LocalWindow = NULL;
							}

							if(LocalScreen)
							{
								CloseScreen(LocalScreen);

								LocalScreen = NULL;
							}
#endif	/* DEBUG */
						}

						deleteftrap();

						if(*Error)
						{
							DeleteBitMap(BitMap);

							BitMap = NULL;
						}
					}
					else
						*Error = ERR_NO_MEM;
				}
			}

			Close(NIL);
			if (special_string)
				FreeVecPooled(special_string);
		}
		else
			*Error = ERR_NO_NIL;

		CloseLibrary(PSbase);
		PSbase = NULL;
	}
	else
		*Error = ERR_NO_POST;

	Free_FontMapName();

	if (psfig_status == PSFIG_END)
		psfig_status = PSFIG_OFF; /* re-init psfig_status */

	return(BitMap);
}

/* (ghi) Here follow some functions to get the size of the PostScript
   picture */

/* (ghi) concatenates a transformation with the current transformation matrix. */
VOID __regargs
CTM_Transf(struct ctm *ctm, float x, float y, int transformation)
{
	float anew, bnew, cnew, dnew, costheta, sintheta;

	switch (transformation) {

		case TR_ROTATION:

			costheta = cos(x);
			sintheta = sin(x);
			anew =   ctm -> a * costheta + ctm -> c * sintheta;
			bnew =   ctm -> b * costheta + ctm -> d * sintheta;
			cnew = - ctm -> a * sintheta + ctm -> c * costheta;
			dnew = - ctm -> b * sintheta + ctm -> d * costheta;
			ctm -> a = anew;
			ctm -> b = bnew;
			ctm -> c = cnew;
			ctm -> d = dnew;
			break;

		case TR_SCALING:

			ctm -> a *= x;
			ctm -> b *= x;
			ctm -> c *= y;
			ctm -> d *= y;
			break;
	
		case TR_TRANSLATION:

			ctm -> tx += ctm -> a * x + ctm -> c * y;
			ctm -> ty += ctm -> b * x + ctm -> d * y;
			break;

		default:

			break;
	}
}


/* (ghi) given a point (x,y) and the current transformation matrix ctm,
   returns the transformed point (x1,y1). */
STATIC VOID __regargs
Point_Transf(struct ctm *ctm, float x, float y, float *x1, float *y1)
{
	*x1 = ctm -> a * x + ctm -> c * y + ctm -> tx;
	*y1 = ctm -> b * x + ctm -> d * y + ctm -> ty;
}


/* (ghi) given the current transformation matrix and a bounding box,
   returns the transformed bounding box. */
STATIC VOID __regargs
bb_new(struct ctm *ctm, struct bbox *bb, struct bbox *bbnew) {
	
	float x0, y0, x1, y1, x2, y2, x3, y3, xmin, xmax, ymin, ymax;

	xmin = ymin =  1.0e38;
	xmax = ymax = -1.0e38;

	Point_Transf(ctm, bb -> llx, bb -> lly, &x0, &y0);
	Point_Transf(ctm, bb -> urx, bb -> lly, &x1, &y1);
	Point_Transf(ctm, bb -> urx, bb -> ury, &x2, &y2);
	Point_Transf(ctm, bb -> llx, bb -> ury, &x3, &y3);
	
	xmin = min(xmin, x0);	ymin = min(ymin, y0);
	xmin = min(xmin, x1);	ymin = min(ymin, y1);
	xmin = min(xmin, x2);	ymin = min(ymin, y2);
	xmin = min(xmin, x3);	ymin = min(ymin, y3);
	xmax = max(xmax, x0);	ymax = max(ymax, y0);
	xmax = max(xmax, x1);	ymax = max(ymax, y1);
	xmax = max(xmax, x2);	ymax = max(ymax, y2);
	xmax = max(xmax, x3);	ymax = max(ymax, y3);

	bbnew -> llx = xmin;
	bbnew -> lly = ymin;
	bbnew -> urx = xmax;
	bbnew -> ury = ymax;
}


/* (ghi) PSGetSize_S, returns width, height and offset of picture to
   process; y-axis is orientated from bottom to top for hoff and voff
   and from top to bottom for hoff_cp and voff_cp.
   hoff and voff are the offsets needed for the PostScript interpreter;
   hoff_cp and voff_cp are the offsets relative to the current position.
   Units are in inch. */
STATIC VOID __regargs
PSGetSize_S(struct parse_result *Result, float *width, float *height, float *hoff, float *voff, float *hoff_cp, float *voff_cp)
{
	float	hscale = 1.0,		vscale = 1.0,
		hscale_rxi = 1.0,	vscale_rxi = 1.0,
		hoffset = 0.0,		voffset=0.0,
		angle = 0.0,
		mag = (float)Result -> DVI_mag / 1000.0,
		CurrentPoint_x=0.0f,	CurrentPoint_y=0.0f;

	struct bbox bb, bbnew;
	struct ctm ctm;

	bb . llx = 0.0;
	bb . lly = 0.0;
	bb . urx = 8.0;		/* default width  = 8in */
	bb . ury = 11.0;	/* default height = 11in */

	Init_CTM(&ctm); /* ctm = identity matrix */

	if (Result -> gotcontrol & GOT_HSCALE)
		hscale = Result -> hscale;
	
	if (Result -> gotcontrol & GOT_VSCALE)
		vscale = Result -> vscale;

	if (Result -> gotcontrol & GOT_SCALE)
		hscale = vscale = Result -> scale;

	if (Result -> gotcontrol & GOT_HOFFSET)
		hoffset = Result -> hoffset;

	if (Result -> gotcontrol & GOT_VOFFSET)
		voffset = Result -> voffset;

	if (Result -> gotcontrol & GOT_ANGLE)
		angle = Result -> angle;

	if ( (Result -> gotcontrol & (GOT_HOFFSET | GOT_VOFFSET)) == (GOT_HOFFSET | GOT_VOFFSET) && !(Result -> gotcontrol & (GOT_HSIZE | GOT_VSIZE | GOT_LLX | GOT_LLY | GOT_URX | GOT_URY | GOT_RWI | GOT_RHI)) )
	{
		BOOL success;

		success = FindBBox(Result -> psfile, &bb);
		if (success)
		{
			if ((bb . urx - bb .llx) < 2.0/72.0)
			{
				bb . urx += 9.0/72.0; /* at least 20bp large */
				bb . llx -= 9.0/72.0;
			}
			if ((bb . ury - bb . lly) < 2.0/72.0)
			{
				bb . ury += 9.0/72.0; /* at least 20bp high */
				bb . lly -= 9.0/72.0;
			}

			bb . urx += 1.0/72.0;
			bb . llx -= 1.0/72.0;
			bb . ury += 1.0/72.0;
			bb . lly -= 1.0/72.0;

			Result -> llx = bb . llx;
			Result -> lly = bb . lly;
			Result -> urx = bb . urx;
			Result -> ury = bb . ury;
			Result -> hoffset += Result -> llx;
			Result -> voffset += Result -> lly;
			hoffset = Result -> hoffset;
			voffset = Result -> voffset;
			Result -> rwi = 10.0 * (Result -> urx - Result -> llx);
			Result -> gotcontrol |= SUFFICIENT_PS_ARGS;
		}
	}

	if ((Result -> gotcontrol & (GOT_LLX | GOT_LLY | GOT_URX | GOT_URY)) == (GOT_LLX | GOT_LLY | GOT_URX | GOT_URY))
	{
		bb . llx = Result -> llx;
		bb . lly = Result -> lly;
		bb . urx = Result -> urx;
		bb . ury = Result -> ury;
	}

	if ((Result -> gotcontrol & (GOT_RWI | GOT_RHI)) == (GOT_RWI | GOT_RHI))
	{
		hscale_rxi = Result -> rwi / (10.0 * (bb . urx - bb . llx));
		vscale_rxi = Result -> rhi / (10.0 * (bb . ury - bb . lly));
	}
	else if ((Result -> gotcontrol & GOT_RWI) == GOT_RWI)
	{
		hscale_rxi = Result -> rwi / (10.0 * (bb . urx - bb . llx));
		vscale_rxi = hscale_rxi;
	}
	else
	{		
		if ((Result -> gotcontrol & GOT_RHI) == GOT_RHI)
		{
			vscale_rxi = Result -> rhi / (10.0 * (bb . ury - bb . lly));
			hscale_rxi = vscale_rxi;
		}
	}

/* Performs PostScript transformations */

	if (Extra_Transf)
	{
		Copy_CTM(&ctm, &Extra_Transf -> CTM);

		CurrentPoint_x = (float) Result -> current_x / Result -> hres - ET_CurrentPoint_x;
		CurrentPoint_y = - (float) Result -> current_y / Result -> vres + ET_CurrentPoint_y;

		CTM_Transf(&ctm, CurrentPoint_x, CurrentPoint_y, TR_TRANSLATION);
	}

	CTM_Transf(&ctm, mag, mag, TR_SCALING);
	CTM_Transf(&ctm, hoffset, voffset, TR_TRANSLATION);
	CTM_Transf(&ctm, hscale, vscale, TR_SCALING);
	CTM_Transf(&ctm, RAD(angle), 0.0f, TR_ROTATION);
	CTM_Transf(&ctm, hscale_rxi, vscale_rxi, TR_SCALING);
	CTM_Transf(&ctm, (float)-bb.llx, (float)-bb.lly, TR_TRANSLATION);

	bb_new(&ctm, &bb, &bbnew);
	*width	= bbnew . urx - bbnew . llx;
	*height	= bbnew . ury - bbnew . lly;
	*hoff = bbnew . llx;
	*voff = bbnew . lly;

/* hsize and vsize define the clipping area; the clipping area isn't
   affected by scaling, translation or rotation. */

	if (Result -> gotcontrol & GOT_HSIZE)
	{
		*width = Result -> hsize ;
		*hoff  = 0.0;
	}

	if (Result -> gotcontrol & GOT_VSIZE)
	{
		*height = Result -> vsize ;
		*voff = 0.0;
	}

	if (Extra_Transf)
	{
		*hoff_cp = *hoff - CurrentPoint_x;
		*voff_cp = *voff - CurrentPoint_y;
	}
	else
	{
		*hoff_cp = *hoff;
		*voff_cp = *voff;
	}
}

/* (ghi) PSGetSizeDot() returns width, height and offsets
   in 'dot' or 'pixels' of the PostScript picture to process.

   hoff, voff = offset to pass to the PostScript interpreter.
   hoff_cp, voff_cp = offsets to pass to SpecialMap -> (h|v)offset.
*/
VOID __regargs
PSGetSizeDot(struct parse_result *Result, LONG *width, LONG *height, LONG *hoff, LONG *voff, LONG *hoff_cp, LONG *voff_cp)
{
	float f_width, f_height, f_hoff, f_voff, f_hoff_cp, f_voff_cp;

	if (psfig_status == PSFIG_END)
		PSGetSize_P(&f_width, &f_height, &f_hoff, &f_voff, &f_hoff_cp, &f_voff_cp, Result -> DVI_mag);
	else
		PSGetSize_S(Result, &f_width, &f_height, &f_hoff, &f_voff, &f_hoff_cp, &f_voff_cp);

	*width	 =   (LONG)(f_width   * Result -> hres + 0.5);
	*height	 =   (LONG)(f_height  * Result -> vres + 0.5);
	*hoff	 =   (LONG)(f_hoff    * Result -> hres + 0.5);
	*voff	 =   (LONG)(f_voff    * Result -> vres + 0.5);
	*hoff_cp =   (LONG)(f_hoff_cp * Result -> hres + 0.5);
	*voff_cp = - (LONG)(f_voff_cp * Result -> vres + 0.5);
}

/* (ghi) Scans a PS file to find a BoundingBox. */
STATIC BOOL __regargs
FindBBox(STRPTR filename, struct bbox *bb)
{
	BYTE *buf,*s,*p;
	BPTR fh;
	BOOL success = FALSE;

	if ((fh = Open(filename,MODE_OLDFILE)) == DOSFALSE) {
		return(FALSE);
	}

	if ((buf = AllocVecPooled(256,MEMF_ANY)) == NULL) {
		Close(fh);
		return(FALSE);
	}

	while (FGets(fh,buf,256) != NULL) {
		if ((s=strstr(buf,"%%BoundingBox:")) != NULL)
		{
			s += 14; /* skip `%%BoundingBox:' */
			if (strstr(s,"atend"))
				continue;
			else
			{
				p = strtok(s," \t");
				bb -> llx = atof(p)/72.0;
				p = strtok(NULL," \t");
				bb -> lly = atof(p)/72.0;
				p = strtok(NULL," \t");
				bb -> urx = atof(p)/72.0;
				p = strtok(NULL," \t");
				bb -> ury = atof(p)/72.0;
				success = TRUE;
				break;
			}
		}
	}

	FreeVecPooled(buf);
	Close(fh);

	return(success);
}

/* (ghi) set the name of a PostScript header (prologue) */
VOID __regargs
Add_PSHeaderName(STRPTR Name)
{
	int i = 0;

	while (PSHeaders[i] && i < MAXPSHEADERS - 1)
	{
		if (!Stricmp(PSHeaders[i], Name))
			return;
		else
			i++;
        }

	if (PSHeaders[i] = AllocVecPooled(strlen(Name) + 1, MEMF_ANY))
		strcpy(PSHeaders[i], Name);
}

VOID __regargs
Init_PSHeaders(VOID)
{
	int i = 0;

	while (PSHeaders[i] && i < MAXPSHEADERS)
	{
		FreeVecPooled(PSHeaders[i]);
		PSHeaders[i++] = NULL;
	}
}

/* (ghi) Given a pointer to the struct psfig_data (GLOBAL), PSGetSize_P()
   returns width, height, offsets of the picture, in inch
   (y-axis orientated from bottom to top).
   This function returns sizes from psfig specials. Psfig specials are those
   created by the TeX/LaTeX macro package `psfig.sty' or `epsfig.sty'. These
   kind of specials spread the data across two or more specials. The first
   one special begins with \special{ps::[begin] ... startTexFig}, while the
   last one with \special{ps::[end] endTexFig}.*/
STATIC VOID __regargs
PSGetSize_P(float *width, float *height, float *hoff, float *voff, float *hoff_cp, float *voff_cp, LONG DVI_mag)
{
	float	hscale = 1.0,	vscale = 1.0,
		angle = 0.0,
		wd = 0.0,	ht = 0.0,
		mag = (float)DVI_mag/1000.0;

	struct bbox bb, bbnew;
	struct ctm ctm;

	bb . llx = SPTOIN(psfig_data . llx);
	bb . lly = SPTOIN(psfig_data . lly);
	bb . urx = SPTOIN(psfig_data . urx);
	bb . ury = SPTOIN(psfig_data . ury);
	wd       = SPTOIN(psfig_data . width);
	ht       = SPTOIN(psfig_data . height);
	angle    = psfig_data . angle;

	/* initialize the current transformation matrix with the identity matrix */
	ctm.a = ctm.d = 1.0;
	ctm.b = ctm.c = ctm.tx = ctm.ty = 0.0;

	/* Performs PostScript transformations */

	hscale = wd / (bb . urx - bb . llx);
	vscale = ht / (bb . ury - bb . lly);

	CTM_Transf(&ctm, mag, mag, TR_SCALING);
	CTM_Transf(&ctm, hscale, vscale, TR_SCALING);
	CTM_Transf(&ctm, (float)-bb.llx, (float)-bb.ury,TR_TRANSLATION);
	CTM_Transf(&ctm, RAD(angle), 0.0f, TR_ROTATION);

	bb_new(&ctm, &bb, &bbnew);
	*width	= bbnew . urx - bbnew . llx;
	*height	= bbnew . ury - bbnew . lly;
	*hoff = bbnew . llx;
	*voff = bbnew . lly;

	if (psfig_data . angle != 0.0)
	{
		/* 0.15in are added to the width and the height of the picture.
		   This value compensate BoundingBox round errors. Same for
		   hoff and voff, which instead are added by -0.1in. */

		*width  = (wd * mag) + 0.15;
		*height = (ht * mag) + 0.15;
		*hoff   = - 0.1;
		*voff   = - ht - 0.1;
	}

	*hoff_cp = *hoff;
	*voff_cp = *voff;
}

VOID __regargs
Init_CTM(struct ctm *ctm)
{
	ctm -> a = ctm -> d = 1.0;
	ctm -> b = ctm -> c = ctm -> tx = ctm -> ty = 0.0;
}

VOID __regargs
Copy_CTM(struct ctm *dest, struct ctm *source)
{
	memcpy(dest, source, sizeof(struct ctm));
}

/* This function returns TRUE if the file is a MP file, else it
   returns FALSE. */
STATIC BOOL __regargs
Is_MPfile(STRPTR name)
{
	BPTR fh;
	BOOL status = FALSE;
	UBYTE	*buf;
	STRPTR s;
	LONG linecnt = 0;

	if (fh = Open(name, MODE_OLDFILE))
	{
		if ((buf = AllocVecPooled(256, MEMF_ANY)) == NULL)
		{
			Close(fh);
			return(FALSE);
		}

		while (FGets(fh, buf, 256) != NULL && linecnt < 20)
		{
			if ((s = strstr(buf, "%%Creator: MetaPost")) != NULL)
			{
				status = TRUE;	/* a metapost file found */
				break;
			}
			else if (strstr(buf, "%%EndProlog") != NULL)
				break;
			else
				linecnt++;
		}

		Close(fh);
		FreeVecPooled(buf);
	}

	return(status);
}

struct FontMapName
{
	STRPTR name_ori, name_alias;
};

STATIC struct FontMapName *FontMapName = NULL;

/* Init_FontMapName() initializes the array `FontMapName' with
   entries from the file `psfonts.map'. */
VOID __regargs Init_FontMapName(VOID)
{
	BPTR FileMap;
	UBYTE *buf;
	STRPTR name_ori, name_alias;
	LONG namecnt = 0L;

	if (FontMapName != NULL)
		return;

	if (buf = AllocVecPooled(256, MEMF_ANY))
	{
		if (FileMap = EVP_Open("psfonts.map", psheaders_var, NULL, NULL, MODE_OLDFILE))
		{
			if (FontMapName = AllocVecPooled(sizeof(struct FontMapName) * MAXMAPENTRIES, MEMF_ANY))
			{
				while (FGets(FileMap, buf, 256) != NULL && namecnt < MAXMAPENTRIES-1)
				{
					name_ori = strtok(buf, " \t");
					name_alias = strtok(NULL, " \t\n");

					if (name_ori != NULL && name_alias != NULL)
					{
						if (*name_ori != '\0' && *name_alias != '\0')
						{
							if (FontMapName[namecnt].name_ori = AllocVecPooled(strlen(name_ori) + 1, MEMF_ANY))
							{
								if (FontMapName[namecnt].name_alias = AllocVecPooled(strlen(name_alias) + 1, MEMF_ANY))
								{
									strcpy(FontMapName[namecnt].name_ori, name_ori);
									strcpy(FontMapName[namecnt].name_alias, name_alias);
									namecnt++;
								}
								else
								{
									FreeVecPooled(FontMapName[namecnt].name_ori);
									break; /* Not enough mem. exit from the while cycle. */
								}
							}
							
						}
					}
				}
				FontMapName[namecnt].name_ori = NULL;
			}
			Close(FileMap);
		}
		FreeVecPooled(buf);
	}
}

/* Free_FontMapName() frees all the memory allocated for the
   `FontMapName' structure. */
VOID __regargs Free_FontMapName(VOID)
{
	LONG i;

	if (FontMapName)
	{
		for (i = 0; FontMapName[i].name_ori != NULL; i++)
		{
			FreeVecPooled(FontMapName[i].name_ori);
			FreeVecPooled(FontMapName[i].name_alias);
		}
		FreeVecPooled(FontMapName);
		FontMapName = NULL;
	}
}

/* FontMapAlias(), finds an alias for the font `font_name' using the
   files `PSFonts.map'. If not found it returns a pointer to `font_name'. */
STATIC STRPTR __regargs FontMapAlias(STRPTR font_name)
{
	LONG i;
	STRPTR name_alias = font_name;

	if (FontMapName)
	{
		for (i = 0L; FontMapName[i].name_ori != NULL; i++)
		{
			if (!strcmp(font_name, FontMapName[i].name_ori))
			{
				name_alias = FontMapName[i].name_alias;
				break;
			}
		}
	}

	return(name_alias);
}
