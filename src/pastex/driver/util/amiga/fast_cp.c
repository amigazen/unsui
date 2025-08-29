#ifndef SLOW

#include "defines.h"

#include <stdio.h>
#include <string.h>

#ifdef AMIGA
# ifdef DISPLAY
#  include <intuition/intuition.h>
   extern struct RastPort 	 myRastPort;
# endif
#endif

#ifdef AZTEC_C
#  include "functions.h"
#endif


#include "globals.h"
#include "bitmap.h"
#include "commands.h"
#include "flmt.h"
#include "new_font.h"

#include "globals.i"
#include "fast_cp.i"
#include "dvihand.i"
#include "unpack.i"
#include "new_font.i"		/* fuer Load_really() */
#include "search.i"

#ifdef DISPLAY
#  include <clib/graphics_protos.h>
#endif

#ifdef MYDEBUG
extern struct Library * DOSBase;
#endif


#define PixRound(x,conv)	(long)(((x) + ((conv) >> 1)) / (conv))

#define BITSPERLONG	32
#define BITSPERWORD	16
#define WORD_WIDTH	16

extern long  hconv, vconv;            /* converts DVI units to pixels       */
extern long  h;                       /* current horizontal position        */
extern long  hh;                      /* current h on device                */
extern long  v;                       /* current vertical position          */
extern long  vv;                      /* current v on device                */



extern int		DeBug;
extern struct bitmap	map;
extern long		upper_limit;
extern long		lower_limit;

extern int   hmaxdrift;                 /* max pixels away from true rounded position */
extern int   vmaxdrift;                 /* max pixels away from true rounded position */


/* extern struct font_entry *cfontptr; */

extern struct Font *cfontptr;


static void __inline setmotion(void);


static void __inline setmotion(void)
{
  long roundpos;

  roundpos = PixRound(h, hconv);
  if (roundpos - hh > hmaxdrift) {
    hh = roundpos - hmaxdrift;
  }
  else if (hh - roundpos > hmaxdrift) { 
    hh = roundpos + hmaxdrift;
  }
}



/*-->SetChar*/
/**********************************************************************/
/*****************************  SetChar  ******************************/
/**********************************************************************/

#if !defined(USE_ASM_SETCHAR) || !defined(DISPLAY)

void SetChar(long pc, int command)
{
  struct Char			*ptr;

  register unsigned long 	reg;
  unsigned short		*char_adr;
  unsigned short		*start_adr, *adr;
  long				nr_sh, length_row;
  long				c, l;

  long			x, y;        /* upper left corner (pixels)           */
  long			cp_h, w;     /* height / width of character (pixels) */
  /* long		*p; */          /* pointer to bitmap */
  unsigned short	*p;          /* pointer to bitmap */
  struct Chars		*cd;

  long			words, lmin, lmax;

 
  cd = &(cfontptr->common->ch[pc]);

  if (cd->packed_data == -1) {
    /* character not in font:
     * add tfm width to h and set hh = PixRound(h)
     */
    if (command <= SET4) {    	/* SET command (or PUT command) ? */
	if (!cfontptr->ctfmw_valid ) {
	   h += scalewidth(cfontptr->common->fnt_group->tfmw[(int)pc&255],
			 	  cfontptr->space_faktor);
        }
	else {
	  h += cfontptr->ctfmw[(int)pc&255];
	}
	hh = PixRound(h, hconv);
    }
    return ;  /* kein Shift */
  }

  if ((ptr = cd->unpacked) == NULL) {
    if (cfontptr->common->fnt_status == FNT_DEFINED ||
        cfontptr->common->fnt_status == FNT_FOUND) {
      /* Font noch nicht geladen?? */
      /* Die Abfrage kommt nur, wenn der Buchstabe nicht eh schon gefunden	*/
      /* wird, also wenn der Char sicher noch nie ausgepackt wurde!		*/

      Load_really(cfontptr);		/* nun wird er geladen... */
    }
    unpack_char(cfontptr, pc);
    if ((ptr = cd->unpacked) == NULL) {
      /* auspacken hat nicht geklappt, aber die Breite des Chars ist bekannt */

      D(bug("Font %s, Num: %ld, Status: %ld, Zeichen: %ld (Zeichen auspacken)\n", 
        cfontptr->common->fnt_group->fnt_name, cfontptr->fnt_number, cfontptr->common->fnt_status, pc));

      goto sh_return;
    }
  }

#if 0
  x = (long)(PixRound(h, hconv)-ptr->xOffset) + hoffset;
  y = (long)(PixRound(v, vconv)-ptr->yOffset) + voffset; 
#endif


#ifdef OLD_NO_OFFSET_VARS

#ifdef DISPLAY
  x = hh - ptr->xOffset + hoffset;
  y = vv - ptr->yOffset + voffset; 
#else
  if (landscape) {
    // wird spaeter noch gedreht, also jetzt etwas seltsame Offsets
    x = hh - ptr->xOffset + hconvresolution;	// plus 1 inch
    y = vv - ptr->yOffset + hoffset; 
  }
  else {
    x = hh - ptr->xOffset + hoffset;
    y = vv - ptr->yOffset + voffset; 
  }

  // die rechte Seite wird direkt neben die linke gesetzt.
  // Dazu wird der hoffset der rechten abgezogen, da dort kein Offset mehr benoetigt wird.
  // hoffset gibt es wirklich nur am linken Rand. Ansonsten hat man ja den moffset.
  if (twopage && !leftpage) x += paper_width + moffset - hoffset;
#endif

#else

  x = hh - ptr->xOffset + OffsetBitmap_X;
  y = vv - ptr->yOffset + OffsetBitmap_Y; 

#endif


  w = (long)ptr->width;					/* char width */
  cp_h = (long)ptr->height;				/* char height */


#ifdef DISPLAY
  // Such-Modus?
  if (InSearchMode) {
    BMSearch(pc, x, y, w, cp_h);
    goto sh_return;
  }
#endif


  p = (unsigned short *)(ptr+1);

  /* Buchstaben am Rand werden vollstaendig weggelassen */
  if (x < 0L || x+w >= map.width) {
      goto sh_return;
  }
#ifdef BERND
  if (y >= lower_limit || y-cp_h < upper_limit) {
  			  ^^^^^^ falsche Richtung!
      goto sh_return;
  }
#else
  /* Buchstaben die ganz unten oder oben heraus sind werden weggelassen */
  if (y >= lower_limit || y+cp_h < upper_limit) {
      goto sh_return;
  }
#endif

  lmin = 0; lmax = cp_h;

  /* Buchstaben die nach unten herausragt wird abgeschnitten */
  if (y+cp_h >= lower_limit) {
    lmax = lower_limit - y /*- 1*/;	/* -1 wird nicht benoetigt, da <lmax! */
  }
  /* Buchstaben die nach oben herausragen werden ebenfalls abgeschnitten */
  if (y < upper_limit) {
    lmin = upper_limit - y;
  }

  length_row = map.width >> 4;		/* Laenge einer Zeile in Woerter */

  start_adr = ((unsigned short *)map.pixptr) 
				+ ((y - upper_limit + lmin) * length_row)
				+ (x >> 4);
  /* Adresse des ersten Wortes */

  nr_sh = x & 15;			/* Anzahl benoetigter Shifte */
  words = (w + 15) / 16;		/* Anzahl Words des Chars pro Zeile */

  char_adr = (unsigned short *) ((unsigned short *)p + (lmin * words));

   //reg = 0L;

#if 0
  // Zeichen 98....'b' hatte Probleme gemacht
  if (!strcmp(cfontptr->common->fnt_group->fnt_name, "cmsl12")) {
    D(bug("Font %s, Num: %ld, Status: %ld, Zeichen: %ld\n", 
      cfontptr->common->fnt_group->fnt_name, cfontptr->fnt_number, cfontptr->common->fnt_status, pc));
  }
#endif


   if (words == 1) {				/* width char <= width word */
     for (l=lmax-lmin; l>0; l--) {
        reg = ((long)(*char_adr++)) << 16;
        reg >>= nr_sh;
        *(unsigned long *)start_adr |= reg;
        start_adr += length_row;
    }						/* end for l		*/
   }
   else {
    adr = start_adr;
    for (l=lmax-lmin; l>0; l--) {
      for (c=words; c>0; c--) {
        reg = ((long)(*char_adr++)) << 16;
        reg >>= nr_sh;
        *(unsigned long *)(adr++) |= reg;
      }						/* end for c		*/
      start_adr += length_row;
      adr = start_adr;
    }						/* end for l		*/
  }						/* end else w<16	*/


sh_return:

    if (command <= SET4) {	/* SET command, not a PUT command */
       /* ... but {\tt DVItype} will allow character codes greater 255,
        * assuming that they all have the same width as the character
        * whose code is  c mod 256.
        */
      if (!cfontptr->ctfmw_valid ) {
        /* Diese Abfrage ist eigentlich ueberfluessig!!		 */
        /* Wenn das Programm korrekt funktioniert, sollte dieser */
        /* Fall nie auftreten!					 */
        WarningStr("Internal error: 'ctfmw_valid' (%s, #%d, St:%d)",
		cfontptr->common->fnt_group->fnt_name, cfontptr->fnt_number, cfontptr->common->fnt_status);
	setup_ctfmw(cfontptr);
        h  += cfontptr->ctfmw[(int)pc&255];
	/* h += scalewidth(cfontptr->common->fnt_group->tfmw[(int)pc&255], cfontptr->space_faktor); */
      }
      else {
        h  += cfontptr->ctfmw[(int)pc&255];
      }
      hh += cd->pixelwidth;
      setmotion(); 	/* Immer wenn was geaendert wird, wird auch setmotion() gemacht! */
    }
#if 0
    setmotion(); 	/* seltsam, warum wird das immer gemacht? */
#endif

  /* Antwort: Wird nicht immer gemacht, sondern nur nach einer Bewegung,
   * deshalb sollte der Aufruf von setmotion eigentlich in's if.
   * Wird nach jeder Aenderung von h gemacht, damit hh nicht mehr als
   * maxdrift von PixRound(h) abweicht. (br)
   */

}

#endif // !USE_ASM_SETCHAR || !DISPLAY



/*-->SetRule*/
/**********************************************************************/
/*****************************  SetRule  ******************************/
/**********************************************************************/

void SetRule(long y, long x, BOOLEAN Set)	/* this routine will draw a rule */
{
#ifndef WEGDISPLAY
  unsigned short	*start_adr, *adr;
  unsigned short	start_mask, end_mask;
  long			l, c;
  long			length_row;
  long			words;
#endif
  long			lmin, lmax;

  long			cp_x, cp_y;        /* upper left corner (pixels)           */
  long			cp_h, cp_w;        /* height / width of character (pixels) */

  long rxx, ryy;


    /* Hoehe und Breite immer aufrunden */
    rxx = (x + (hconv - 1)) / hconv;
    ryy = (y + (vconv - 1)) / vconv;


#ifdef DISPLAY
    if (InSearchMode) goto shift;
#endif


    if (x > 0 && y > 0) {


#ifdef OLD_NO_OFFSET_VARS

#ifdef DISPLAY
       cp_x = hh + hoffset;
       cp_y = vv + voffset;
#else
       if (landscape) {
         // wird spaeter noch gedreht, also jetzt etwas seltsame Offsets
         cp_x = hh + hconvresolution;	// plus 1 inch
         cp_y = vv + hoffset; 
       }
       else {
         cp_x = hh + hoffset;
         cp_y = vv + voffset;
       }

       if (twopage && !leftpage) cp_x += paper_width + moffset - hoffset;
#endif

#else

       cp_x = hh + OffsetBitmap_X;
       cp_y = vv + OffsetBitmap_Y;

#endif


       cp_w = rxx;
       cp_h = ryy;

       /* mache linken unteren zum linken oberen ... */
       cp_y -= (cp_h - 1);

      /*---- Schneide Rule links und rechts an den Seitengrenzen ab ----*/
      if( cp_x < 0L ) {
	cp_w += cp_x;	cp_x = 0L;
      }
      if( cp_x + cp_w >= map.width )	/* oder ist -1 notwendig ? */
         cp_w = map.width - cp_x;

      if( cp_w <= 0L )	/* Rule liegt nicht mehr auf der Seite */
	goto shift;

      /*---- Schneide Rule oben und unten an den Seitengrenzen ab ----*/
      if( cp_y < upper_limit ) {
	cp_h = cp_h - (upper_limit - cp_y);
	cp_y = upper_limit;
      }
      if( cp_y + cp_h >= lower_limit )	/* oder ist -1 notwendig ? */
	cp_h = lower_limit - cp_y;

      if( cp_h <= 0L )	/* Rule liegt nicht mehr auf der Seite */
	goto shift;


       lmin = 0; lmax = cp_h;

#if 0
       if (cp_x+cp_w < 0L || cp_x >= map.width-1) {
         goto shift;
       }
       if (cp_x+cp_w >= map.width) {	/* Abfrage, dass die Linie nicht rechts raus geht */
         cp_w = map.width - cp_x - 1;
       }

       if (cp_y >= lower_limit || cp_y+cp_h < upper_limit) {
         goto shift;
       }
       if (cp_y+cp_h >= lower_limit) {
         lmax = lower_limit - cp_y - 1;
       }
       if (cp_y - upper_limit < 0) {
         lmin = upper_limit - cp_y;
       }
#endif

#ifdef WEGDISPLAY
       RectFill(&myRastPort, cp_x, cp_y+lmin, cp_x+cp_w-1, cp_y+lmax-1);
#else

  length_row = map.width >> 4;		/* Laenge einer Zeile in Woerter */
  start_adr = ((unsigned short *)map.pixptr) + ((cp_y - upper_limit + lmin) * length_row)
				    + (cp_x >> 4);
  start_mask = (1 << (16-(cp_x & 15)))-1;
  end_mask   = ~((1 << (16-((cp_x+cp_w) & 15)))-1);

  adr = start_adr;
  c = (cp_x & 15) + cp_w;	/* Hilfe fuer unten */

  if (c < 16 ) {
    start_mask &= end_mask;
    for (l=lmin; l<lmax; l++) {
      *adr |= start_mask;
      adr += length_row;
    }
  }
  else {
    if (c < 32) {
      length_row--;
      for (l=lmin; l<lmax; l++) {
        *adr |= start_mask;
	adr++;
	*adr |= end_mask;
        adr += length_row;
      }
    }
    else {
      words = (cp_w + 15 - (16-(cp_x&15)) - ((cp_x+cp_w)&15) ) >> 4;
	/* Anzahl der Woerter */
      for (l=lmin; l<lmax; l++) {
        *adr |= start_mask;
	adr++;
	for (c=0; c<words; c++) {
	  *adr = 0xFFFF;
	  adr++;
	}
	*adr |= end_mask;
        start_adr += length_row;
        adr = start_adr;
      }
    }
  }
#endif		/* WEGDISPLAY */
     }

shift:
    if (Set) {
      h  += x;
      hh += rxx ;
      setmotion();	/* muss nach jeder Aenderung gemacht werden! */
    }
}


#endif		/* SLOW */
