/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heßmann
*/

/*
** Parse.h
**
** latest revision: 4 Oct 1994, by Giuseppe Ghibò
**
*/

#define STRSIZE 256

/* Modes */

enum	{	BandW,FS,Burkes,Sierra,JJN,Stucki,BlueNoise,Ordered,Halftone,
		RandomNoise,BckBrick,FwdBrick,Hexagon,SpiralDot,Horizontal,
		StevensonArce
	};

#define GOT_LLX		(1L << 0)
#define GOT_LLY		(1L << 1)
#define GOT_URX		(1L << 2)
#define GOT_URY		(1L << 3)
#define GOT_RWI		(1L << 4)
#define GOT_RHI		(1L << 5)
#define GOT_ANGLE	(1L << 6)
#define GOT_HSIZE	(1L << 7)
#define GOT_VSIZE	(1L << 8)
#define GOT_HSCALE	(1L << 9)
#define GOT_VSCALE	(1L << 10)
#define GOT_SCALE	(1L << 11)
#define GOT_HOFFSET	(1L << 12)
#define GOT_VOFFSET	(1L << 13)
#define GOT_CLIP	(1L << 14)

#define SUFFICIENT_PS_ARGS (GOT_LLX | GOT_LLY | GOT_URX | GOT_URY | GOT_RWI)

struct parse_result
{
	char	iffile[STRSIZE];
	char	psfile[STRSIZE];
	float	hsize;
	float	vsize;
	float	hoffset;
	float	voffset;
	float	scale;
	float	hscale;
	float	vscale;
	float	angle;
	long	hres;
	long	vres;
	long	mode;
	long	bright;
	long	contrast;
	long	gamma;
	long	red;
	long	green;
	long	blue;

	float	llx,lly,urx,ury,rwi,rhi;

	long	gotcontrol;

	long	transfer,
		rendering,
		invert,
		base_dpi,
		threshold,
		patch_colours,
		dither_opt;

	char	psinit_file[STRSIZE],
		psinit_string[STRSIZE];

	long	current_x;	/* (ghi) current point */
	long	current_y;

	long	page_width;	/* (ghi) page width */
	long	page_height;

	long	DVI_mag;	/* DVI magnification */
};

BOOL ParseSpecial(STRPTR OldString,struct parse_result *Result);

struct psfig_data {
	LONG	width,
		height,
		llx,
		lly,
		urx,
		ury;
	float	angle;
	BOOL	clip;
};

#define PSFIG_OFF	0 /* there are no psfig special */
#define PSFIG_BEGIN	1 /* ps::[begin] ... hitted and successful */
#define PSFIG_WAIT	2 /* next command has to be `ps::[end] endTeXFig */
#define PSFIG_END	3 /* psfig special is complete and may be processed */

GLOBAL LONG psfig_status; /* (ghi) status of a psfig special. */
GLOBAL struct psfig_data psfig_data; /* (ghi) a structure containing psfig special information */
GLOBAL VOID __regargs Init_Extra_Transf(VOID);
