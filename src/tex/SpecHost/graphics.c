/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heßmann
*/

#include "Global.h"

/* graphics.c
 *
 * Routinen zum Zeichnen in die Seiten-Bitmap:
 *	device_SetPenSize(x,y)		- setze Pinselstaerke auf x/y
 *	device_DrawPoint(x,y)		- Punkt bei (x,y)
 *	device_DrawLine(x,y,x1,y1)	- Linie von (x,y) nach (x1,y1)
 *
 * Mit v_pline, etc. kann nur auf den Bildschirm geschrieben werden,
 * Linien, Punkte, etc. sollten jedoch in die Bitmap.
 *
 * Die hier verwendeten Koordinaten sind schon Bildschirmpunkte !!!
 */


/*
 *	externe Variablen
 */
extern struct bitmap map;
extern long upper_limit;
extern long lower_limit;

static void CopyBitArray(int x, int y, int w, int h, unsigned short *p);

/*----------------------------------------------------------------------*/

static void CopyBitArray(int x, int y, int w, int h, unsigned short *p)
{
  register unsigned long 	reg;
  unsigned short		*char_adr;
  unsigned short		*start_adr, *adr;
  long				nr_sh, length_row;
  long				c, l;

  long			words;


  length_row = map.width >> 4;		/* Laenge einer Zeile in Woerter */

  start_adr = ((unsigned short *)map.pixptr) 
				+ ((y - upper_limit) * length_row)
				+ (x >> 4);
  /* Adresse des ersten Wortes */

  nr_sh = x & 15;			/* Anzahl benoetigter Shifte */
  words = (w + 15) / 16;		/* Anzahl Words des Chars pro Zeile */

  char_adr = p;


   if (words == 1) {				/* width char <= width word */
     for (l=0; l<h; l++) {
        reg = ((long)(*char_adr++)) << 16;
        reg >>= nr_sh;
        *(unsigned long *)start_adr |= reg;
        start_adr += length_row;
    }						/* end for l		*/
   }
   else {
    adr = start_adr;
    for (l=0; l<h; l++) {
      for (c=0; c<words; c++) {
        reg = ((long)(*char_adr++)) << 16;
        reg >>= nr_sh;
        *(unsigned long *)(adr++) |= reg;
      }						/* end for c		*/
      start_adr += length_row;
      adr = start_adr;
    }						/* end for l		*/
  }						/* end else w<16	*/
}

/*----------------------------------------------------------------------*/



/*************************************************************
 ********************  device_SetPenSize  ********************
 *************************************************************/
/*
 * Hier werden jetzt die Formen der Pinselpunkte definiert.
 * Bei unsymmetrischen Punkten sollte man die Bitmaps (=Ellipsen) besser
 * in device_SetPenSize() direkt berechnen. Allgemein sollte man besser
 * die Bitmap und deren Eigenschaften (Breite, Hoehe, HotSpot) in
 * device_SetPenSize() berechnen. (Evtl. kann man auch fuer grosse Pens
 * die Schrittweite in draw_line > 1 setzen.)
 */

#define	MAX_PEN_SIZE	4 	/* Max pixels of pen width */

static unsigned short bitmap1[] = { 0x8000, 0x0000 };
static unsigned short bitmap2[] = { 0xc000, 0xc000, 0x0000 };
static unsigned short bitmap3[] = { 0x4000, 0xe000, 0x4000, 0x0000 };
static unsigned short bitmap4[] = { 0x6000, 0xf000, 0xf000, 0x6000, 0x0000 };

static struct pens {
  unsigned short *map;
  char height, width;
  char x_offset, y_offset;	/* Hot Spot Location von links oben */
} pens[MAX_PEN_SIZE] = {
  bitmap1, 1, 1, 0, 0,
  bitmap2, 2, 2, 1, 1,
  bitmap3, 3, 3, 1, 1,
  bitmap4, 4, 4, 2, 2
};

static struct pens *actual_pen = &pens[0];


/* Set actual drawing pen to xpen/ypen (both in device points) */
  void
device_SetPenSize(long xpen, long ypen)
{ long pen;

  /* for screen: xpen == ypen (normally) */
  pen = (xpen + ypen + 1) / 2;
  if( pen < 1 )
    pen = 1;
  else if( pen > MAX_PEN_SIZE )
    pen = MAX_PEN_SIZE;

  --pen;	/* Index in pens[] */
  actual_pen = &pens[pen];
}


/**************************************************************
 *********************  device_DrawPoint  *********************
 **************************************************************/

  void			/* Koordinaten des Pen-Mittelpunkts in device points */
device_DrawPoint(long x, long y)
{ long h, w;

  /* Der aktuelle Pen ist in bitmap_pen eingetragen
   * und ist pen_height hoch und pen_width breit.
   * Der Mittelpunkt liegt bei pen_xoffset/pen_yoffset.
   */

  x -= actual_pen->x_offset;
  y -= actual_pen->y_offset;
	/* Falls man Hot Spot von links unten angibt, so muesste man
	 * hier  y -= pen_height - pen_yoffset;  schreiben.
	 */

  /*
   *  Jetzt noch alle Punkte, die nicht auf der Seite liegen, aussieben.
   */
  w = actual_pen->width;
  if( x < 0L || x + w >= map.width )
	return;

  h = actual_pen->height;
  if( y < upper_limit || y + h >= lower_limit )
	return;

#ifdef ATARI
  AtCopyBitArray(x, y-upper_limit, w, h, actual_pen->map);
#else
  CopyBitArray(x, y, w, h, actual_pen->map);
#endif
}


/*************************************************************
 *********************  device_DrawLine  *********************
 *************************************************************/

#if 1
/*
 * Zeichne Linie nach der "Quadrantal DDA" Methode.
 *
 * ergibt dickere Linien, da Diagonalen so gezeichnet werden:
 *
 *            *****
 *         ****
 *     *****
 */

  void			/* Anfangs- und Endpunktkoordinaten in device points */
device_DrawLine(long x, long y, long x1, long y1)
{ short dx, dy;
  short xstep=0, ystep=0;
  short diff;
#define STEP	1	/* oder in Abhaengigkeit der pensize auch groesser */

  /* als erstes: dx sollte >= 0 sein, d.h. x1 >= x */
  dx = x1 - x;  dy = y1 - y;
  if( dx < 0 ) {
    dx = -dx;  dy = -dy;
    x = x1;    y = y1;
  }

  /* jetzt: dx >= 0, also nur 1. oder 4.Quadrant moeglich */
  device_DrawPoint(x, y);

  if( dy < 0 ) {	/* 4.Quadrant */
    diff = ( dx > -dy ) ? -dx/2 : -dy/2;
    do {
	if( diff > 0 ) {
	  ystep -= STEP;	diff -= dx;
	} else {
	  xstep += STEP;	diff -= dy;
	}
	device_DrawPoint(x + xstep, y + ystep);
    } while( xstep < dx || ystep > dy );
  } else {		/* 1.Quadrant */
    diff = ( dx > dy ) ? -dx/2 : dy/2;
    do {
	if( diff > 0 ) {
	  ystep += STEP;	diff -= dx;
	} else {
	  xstep += STEP;	diff += dy;
	}
	device_DrawPoint(x + xstep, y + ystep);
    } while( xstep < dx || ystep < dy );
  }
}

#else

/*
 * Zeichne Linie nach der "Octantal DDA" Methode.
 *
 * ergibt duennere Linien, da Diagonalen so gezeichnet werden:
 *
 *             ****
 *         ****
 *     ****
 */

  void			/* Anfangs- und Endpunktkoordinaten in device points */
device_DrawLine(long x, long y, long x1, long y1)
{ short dx, dy;
  short xstep=0, ystep=0;
  short diff;
#define STEP	1

  /* als erstes: dx sollte >= 0 sein, d.h. x1 >= x */
  dx = x1 - x;  dy = y1 - y;
  if( dx < 0 ) {
    dx = -dx;  dy = -dy;
    x = x1;    y = y1;
  }

  /* jetzt: dx >= 0, also nur 1. oder 4.Quadrant moeglich */
  device_DrawPoint(x, y);

  if( dy < 0 ) {
    if( dx > -dy ) {	/* 8. Octant */
      diff = -dx/2;
      do {
	xstep += STEP;
	if( diff > 0 ) {
	  ystep -= STEP;
	  diff = diff - dy - dx;
	} else {
	  diff = diff - dy;
	}
	device_DrawPoint(x + xstep, y + ystep);
      } while(xstep < dx);
    } else {		/* 7. Octant */
      diff = (dx == -dy) ? 0 : -dy/2;
      do {
	ystep -= STEP;
	if( diff > 0 ) {
	  diff = diff - dx;
	} else {
	  xstep += STEP;
	  diff = diff - dy - dx;
	}
	device_DrawPoint(x + xstep, y + ystep);
      } while(ystep > dy);
    }
  } else {
    if( dx > dy ) {	/* 1. Octant */
      diff = -dx/2;
      do {
	xstep += STEP;
	if( diff > 0 ) {
	  ystep += STEP;
	  diff = diff + dy - dx;
	} else {
	  diff = diff + dy;
	}
	device_DrawPoint(x + xstep, y + ystep);
      } while(xstep < dx);
    } else {		/* 2. Octant */
      diff = (dx == dy) ? 0 : dy/2;
      do {
	ystep += STEP;
	if( diff > 0 ) {
	  diff = diff - dx;
	} else {
	  xstep += STEP;
	  diff = diff + dy - dx;
	}
	device_DrawPoint(x + xstep, y + ystep);
      } while(ystep < dy);
    }
  }
}
#endif

/********************  end of graphics.c  ********************/


