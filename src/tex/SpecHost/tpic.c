/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heﬂmann
*/

#include "Global.h"

/** tpic.c **/


static void flushPath    (int invis);
static void flushDashed	 (int dotted);
static void flushSpline	 (void);
static void arc		 (int invis);

static void dot_at       (int x, int y);
static void line_btw     (int x0, int y0, int x1, int y1);
static void draw_ellipse (int xc, int yc, int xr, int yr);


struct tpic_msg   *tp;
struct driver_map *dm;

long hres, vres;
long path_len;

#define xRESOLUTION	hres
#define yRESOLUTION	vres

struct bitmap     map;
long		  upper_limit;
long		  lower_limit;




/*
 * APPROXIMATE integer distance between two points
 */
#define	dist(x0, y0, x1, y1)	(abs(x0-x1)+abs(y0-y1))

#define	TWOPI		(3.14159265359*2.0)

#define xCONV_PT(x) \
  ((x) < 0 ? -((-(x) * hres + 500) / 1000) \
	   : (((x) * hres + 500) / 1000) )
#define yCONV_PT(y) \
  ((y) < 0 ? -((-(y) * vres + 500) / 1000) \
	   : (((y) * vres + 500) / 1000) )






void work_with_tpic(struct tpic_msg *tpic, struct driver_map *dmap,
			long hresolution, long vresolution)
{
  tp = tpic;
  dm = dmap;

  hres = hresolution;
  vres = vresolution;
  path_len = tp->path_len;
  
  /* Initialisierung fuer 'graphics.c' */
  map.width  = dm->width;
  map.height = dm->height;
  map.pixptr = dm->pixptr;
  upper_limit = dm->upper_limit;
  lower_limit = dm->lower_limit;

  device_SetPenSize(xCONV_PT(tpic->pen_size), yCONV_PT(tpic->pen_size));

  switch (tpic->tpic_com) {
    case TPIC_FP:
	flushPath(FALSE);
    case TPIC_IP:
	flushPath(TRUE);
	break;
    case TPIC_DA:
	flushDashed(FALSE);
	break;
    case TPIC_DT:
	flushDashed(TRUE);
	break;
    case TPIC_SP:
    case TPIC_SPB:
	flushSpline();
	break;
    case TPIC_AR:
	arc(FALSE);
	break;
    case TPIC_IA:
	arc(TRUE);
	break;
    default:
	break;
  }
}



/*----------------------------------------------------------------*/


static void flushPath(int invis)
{
  int i, x, y;

  if (tp->path_len == 1) {
    PrintLine("FP/IP path with less than 2 points ignored");
  }
  else {
    if (invis) {
      /* not jet implemented */
      ;
    }
    else {
      x = dm->x;
      y = dm->y;
      for (i=1; i<tp->path_len; i++) {
        device_DrawLine(
  		x + xCONV_PT(tp->xx[i]), y + yCONV_PT(tp->yy[i]),
  		x + xCONV_PT(tp->xx[i+1]), y + yCONV_PT(tp->yy[i+1]));
      }
    }
  }
}


static void flushDashed(int dotted)
{
  long *xx = &(tp->xx[0]);
  long *yy = &(tp->yy[0]);
  int i, numdots, x0, y0, x1, y1;
  int cx0, cy0, cx1, cy1;
  float d, spacesize, a, b, dx, dy, milliperdash;
  float inchesperdash;

  inchesperdash = atof(tp->opt_float[0]);

  if (tp->path_len <= 1 || inchesperdash <= 0.0) {
    PrintLine("\33bIllegal conditions for dotted/dashed line (%ld,%ld).\33n",tp->path_len, inchesperdash);
    return;
  }

    milliperdash = inchesperdash * 1000.0;
    x0 = xx[1];	y0 = yy[1];
    x1 = xx[2];	y1 = yy[2];
    dx = x1 - x0;
    dy = y1 - y0;
    if (dotted) {
	numdots = sqrt(dx*dx + dy*dy) / milliperdash + 0.5;
	if (numdots == 0) numdots = 1;
	for (i=0; i <= numdots; i++) {
	    a = (float) i / (float) numdots;
	    cx0 = x0 + a*dx + 0.5;
	    cy0 = y0 + a*dy + 0.5;
	    dot_at(cx0, cy0);
	}
    } else {
	d = sqrt(dx*dx + dy*dy);
	if (d <= 2.0*milliperdash)
	    line_btw(x0, y0, x1, y1);
	else {
	    numdots = d / (2.0*milliperdash) + 1.0;
	    spacesize = (d - numdots * milliperdash) / (numdots - 1);
	    for (i=0; i<numdots-1; i++) {
		a = i * (milliperdash + spacesize) / d;
		b = a + milliperdash / d;
		cx0 = x0 + a*dx + 0.5;
		cy0 = y0 + a*dy + 0.5;
		cx1 = x0 + b*dx + 0.5;
		cy1 = y0 + b*dy + 0.5;
		line_btw(cx0, cy0, cx1, cy1);
		b += spacesize / d;
	    }
	    cx0 = x0 + b*dx + 0.5;
	    cy0 = y0 + b*dy + 0.5;
	    line_btw(cx0, cy0, x1, y1);
	}
    }
}

static void flushSpline(void)
{
  long *xx = &(tp->xx[0]);
  long *yy = &(tp->yy[0]);
  int xp, yp, N, lastx, lasty;
  int t1, t2, t3, steps, j;
  int i, w;

#if 0
  int ipd;

  if( *cp ) {
    float inchesPerDash;

    inchesPerDash = atof(cp);

    ipd = (int)(1000.0 * inchesPerDash);	/* to get milli-inches */
  }

  if( ipd < 0 )	{	/* dotted */
  } else {		/* dashed */
  }
#endif

  N = path_len + 1;
  xx[0] = xx[1];	yy[0] = yy[1];
  xx[N] = xx[N-1];	yy[N] = yy[N-1];
  for (i=0; i<N-1; i++) {	/* interval */
	/* original was:
	 * steps = (dist(xx[i], yy[i], xx[i+1], yy[i+1]) +
	 *	 dist(xx[i+1], yy[i+1], xx[i+2], yy[i+2])) / 80;
	 */
	steps = (dist(xx[i], yy[i], xx[i+1], yy[i+1]) +
		 dist(xx[i+1], yy[i+1], xx[i+2], yy[i+2])) * xRESOLUTION / 4000;

 	w  = 500 / steps;
	t1 = w * w / 20;
	w -= 500;
	t2 = (750000 - w * w) / 10;
	w -= 500;
	t3 = w * w / 20;
	lastx = (t1*xx[i+2] + t2*xx[i+1] + t3*xx[i] + 50000) / 100000;
	lasty = (t1*yy[i+2] + t2*yy[i+1] + t3*yy[i] + 50000) / 100000;

	for (j=1; j<steps; j++) {	/* points within */
	    w  = (j*1000 + 500) / steps;
	    t1 = w * w / 20;
	    w -= 500;
	    t2 = (750000 - w * w) / 10;
	    w -= 500;
	    t3 = w * w / 20;
	    xp = (t1*xx[i+2] + t2*xx[i+1] + t3*xx[i] + 50000) / 100000;
	    yp = (t1*yy[i+2] + t2*yy[i+1] + t3*yy[i] + 50000) / 100000;
	    line_btw(lastx, lasty, xp, yp);
	    lastx = xp;
	    lasty = yp;
	}
  }
}

static void arc(int invis)
{
  int xc, yc, xrad, yrad, n;
  int lastx, lasty;
  float start_angle, end_angle, angle, theta, r;
  float xradius, yradius;

  xc   = tp->opt_long[0];
  yc   = tp->opt_long[1];
  xrad = tp->opt_long[2];
  yrad = tp->opt_long[3];

  start_angle = atof(tp->opt_float[0]);
  end_angle   = atof(tp->opt_float[1]);

  /* fuer `invis' == true sollte die Ellipse nicht gezeichnet, sondern
   * nur ausgefuellt werden.... wir machen nichts...
   */
  if( invis )
    return;

  /* ... und Warnung fuer folgende Faelle ?? */
  if( start_angle > end_angle ) {
    angle = start_angle;  start_angle = end_angle;  end_angle = angle;
  }
  if( xrad < 0 ) xrad = -xrad;
  if( yrad < 0 ) yrad = -yrad;

  /* We have a specialized fast way to draw closed circles/ellipses */
  if (start_angle <= 0.0 && end_angle >= 6.283) {
    draw_ellipse(xc, yc, xrad, yrad);
    return;
  }

  r = ((float) (xrad + yrad)) / 2.0;
  theta = sqrt(1.0 / r);
  n = 0.3 * TWOPI / theta + 0.5;
  if (n < 12)
    n = 12;
  else if (n > 80)
    n = 80;
  /* n /= 2; */
  theta = TWOPI / n;

  xradius = xrad;
  yradius = yrad;
  lastx = xc + (int)(xradius * cos(start_angle) + 0.5);
  lasty = yc + (int)(yradius * sin(start_angle) + 0.5);

#ifdef __GNUC__
  /* GCC lib fuer den ST hat in _addsf3() anscheinend einen Fehler :-( */
  angle = (float)start_angle + theta;
#else
  angle = start_angle + theta;
#endif

  n += 10;
  while (--n > 0 && angle < end_angle) {
    xrad = xc + (int) (xradius*cos(angle) + 0.5);
    yrad = yc + (int) (yradius*sin(angle) + 0.5);
    line_btw(lastx, lasty, xrad, yrad);
    lastx = xrad;
    lasty = yrad;
    angle += theta;
  }

  if( n <= 0 ) {
    PrintLine("\33bError in ellipse, start angle %f, end angle %f, theta %f.\33n",start_angle, end_angle, theta);
  }

  xrad = xc + (int)(xradius*cos(end_angle) + 0.5);
  yrad = yc + (int)(yradius*sin(end_angle) + 0.5);
  line_btw(lastx, lasty, xrad, yrad);
}



/*-------------------------------------------------------------------*/

  static void
dot_at(int x, int y)
{
  x = dm->x + xCONV_PT(x);
  y = dm->y + yCONV_PT(y);

  device_DrawPoint(x, y);
}


  static void
line_btw(int x0, int y0, int x1, int y1)
{
  x0 = dm->x + xCONV_PT(x0);
  y0 = dm->y + yCONV_PT(y0);
  x1 = dm->x + xCONV_PT(x1);
  y1 = dm->y + yCONV_PT(y1);

  device_DrawLine(x0, y0, x1, y1);
}




/*
 * Draw an ellipse with the indicated center and radices.
 */
  static void
draw_ellipse(int xc, int yc, int xr, int yr)
{ float angle, theta;
  int n, px0, py0, px1, py1;

  angle = (xr + yr) / 2.0;
  theta = sqrt(1.0 / angle);
  n = TWOPI / theta + 0.5;
  if (n < 12)
    n = 12;
  else if (n > 80)
    n = 80;
  n /= 2;
  theta = TWOPI / n;

  px0 = xc + xr;	/* cos(0) = 1 */
  py0 = yc;		/* Sin(0) = 0 */
  angle = theta;

  while (angle <= TWOPI) {
    px1 = xc + xr*cos(angle) + 0.5;
    py1 = yc + yr*sin(angle) + 0.5;
    line_btw(px0, py0, px1, py1);
    px0 = px1;
    py0 = py1;
    angle += theta;
  }
  line_btw(px0, py0, xc + xr, yc);
}

