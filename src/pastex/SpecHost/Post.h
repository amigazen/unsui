/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heﬂmann
*/

#define INTOBP(x) ((float)(x*72.0))		/* (ghi) converts 'inch' to 'big point' */
#define INTOSP(x) ((float)(x*72.0*65781.76))	/* (ghi) converts 'inch' to 'scaled point' */
#define SPTOIN(x) (x/(72.0*65781.76))		/* (ghi) converts `scaled points' to `inch' */

#define	TR_ROTATION	1
#define	TR_SCALING	2
#define	TR_TRANSLATION	3

#define RAD(a) ((float)(a*3.1415926535358979324/180.0))

struct ctm {
	float a, b, c, d, tx, ty;
	};

struct extra_transf {
	struct extra_transf *prev, *next;
	struct ctm CTM;
	int type_of_transf;
};

struct bbox {
	float llx, lly, urx, ury;
	};

GLOBAL struct extra_transf *Extra_Transf;
GLOBAL struct Library *PSbase;
GLOBAL float ET_CurrentPoint_x, ET_CurrentPoint_y;
