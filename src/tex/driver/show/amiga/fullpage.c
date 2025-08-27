/* fullpage.c */

#include "defines.h"


/* #define DOS_DOSASL_H */

#include <sprof.h>

#include <stdio.h>
#include <signal.h>
#include <dos.h>


#include <exec/types.h>
#include <intuition/intuition.h>

#ifdef AZTEC_C
#  include <functions.h>
#endif

#include "globals.h"

#ifdef ANSI
#  include <string.h>
#  include <stdlib.h>
#endif

#include <clib/intuition_protos.h>
#include <clib/graphics_protos.h>

#include <pragmas/intuition_pragmas.h>
#include <pragmas/diskfont_pragmas.h>

#include "version.h"
#include "amscreen.h"

extern struct BitMap	BitMapWin;	// amscreen.c


#include "globals.i"

/*
 * Fuer die locale-Library:
 *
 * Hier duerfen *nur* die MSG_#? Nummern eingebunden werden!
 * Achtung:
 * Es muss/sollte 'multiple-include' erlaubt sein!
 */
#include "local.i"

#undef  CATCOMP_ARRAY
#undef  CATCOMP_BLOCK
#undef  CATCOMP_STRINGS
#define CATCOMP_NUMBERS
#include "localstr.h"


#define FPAGE_ABS HeightWinTitle

void make_full_page(struct RastPort *rp, int xk, int yk,
			int p_width, int p_height, unsigned short *fpage)
{
  register unsigned long mask, erg;
  long j, bitpos, mask_anf, mask_width;
  long k, i, adr;
  int weite_x, rest_weite_x, weite_y, rest_weite_y, merk_rest_weite_x;
  int vert_rest_x, vert_rest_y, hilf_vert_x, hilf_vert_y;
  long zeilenlaenge;
  unsigned short *zeile, *anf_zeile, copy_mask, *write_pointer, *start_showmap;
  int start_row, akt_row, length_row;
  int yskip, xskip, add_y /*, add_x*/;
  /* int zeilen = 0; */	/* fuer DEBUG Zwecke... */

  if (p_width == 0) Fatal(20,MSG_INTERNAL_ERROR);

  weite_x = wx / p_width;
  merk_rest_weite_x = rest_weite_x = (wx % p_width);

  if (rest_weite_x > p_width/2) {
    merk_rest_weite_x = rest_weite_x = p_width * (weite_x+1) - wx;
    xskip = 1;		/* Rest entspricht der wegzulassenden Spalten */
    weite_x++;
  }
  else {
    xskip = 0;		/* Rest entspricht der hinzuzufuegenden Spalten */
  }
  if (rest_weite_x == 0) {
    vert_rest_x = (p_width << 7);		/* KEIN -128 da eigentlich p_width+1 */
  }
  else {
    vert_rest_x = (p_width << 7) / rest_weite_x - 128;
  }

  if (p_height == 0) Fatal(20,MSG_INTERNAL_ERROR);

  weite_y = wy / p_height;
  rest_weite_y = (wy % p_height);

  if (rest_weite_y > p_height/2) {
    rest_weite_y = p_height * (weite_y+1) - wy;
    yskip = 1;		/* Rest entspricht der wegzulassenden Zeilen */
  }
  else {
    yskip = 0;		/* Rest entspricht der hinzuzufuegenden Zeilen */
  }
  if (rest_weite_y == 0) {
    vert_rest_y = (p_height << 7);		/* KEIN -128 da eigentlich p_height+1 */
  }
  else {
    vert_rest_y = (p_height << 7) / rest_weite_y - 128;		/* immer mit Faktor 100 rechnen */
  }
  hilf_vert_y = 64;	/* Heuristik, bin mir nicht sicher ob's besser als 0 ist...?? */

/**
  add_x = (x_win_i_width - p_width) / 2;
  add_y = FPAGE_ABS + (x_win_i_height - p_height) / 2;
**/

#if 0
  start_row = ((x_win_i_width - p_width) / 2) % 16;	/* Um wieviele Pixel muss der Anfang verschoben werden */
  length_row = rp->BitMap->BytesPerRow / 2;

  add_y = (yk+win2->BorderTop) * length_row + xk / 16;
  start_showmap = ((unsigned short *)(rp->BitMap->Planes[0]));
  write_pointer = start_showmap + add_y;
#else
  start_row = 0;
  length_row = (p_width + 15) / 16;

  add_y = 0;
  start_showmap = fpage;
  write_pointer = start_showmap + add_y;
#endif


  anf_zeile = (unsigned short *)(BitMapWin.Planes[0]);
  zeilenlaenge = wx >> 4;
  zeile = (unsigned short *)xmalloc((unsigned)(zeilenlaenge * sizeof(unsigned short)));
  
#ifdef DEBUG
  if (DeBug) { 
	printf("make_full_page: weite x,y: %d,%d ;rest x,y: %d,%d\n",
			weite_x, weite_y, rest_weite_x, rest_weite_y);
	printf("\tvert_rest_x,y: %d,%d\n",vert_rest_x,vert_rest_y);
	printf("\tadd_y: %d, p_width,height: %d,%d\n", add_y,p_width,p_height);
	printf("\tzeilenlaenge: %d yskip: %d, xskip: %d\n", zeilenlaenge,yskip, xskip);
	printf("\ttotal page size: %d,%d\n",wx,wy);
  }
#endif


  /**************** Schleife ueber die y-Werte ****************/
  for(i=0; i<p_height; i++) {		/* Anzahl der Zeilen des Bildes */

    for (j=0; j<zeilenlaenge; j++) {
      zeile[j] = anf_zeile[j];		/* erste zeile in Zwischensp. kopieren */
    }
     /* zeilen++; */
    
    anf_zeile += zeilenlaenge;
    for (k=1; k<weite_y; k++) {		/* Rest bis weite_y einodern */
      for (j=0; j<zeilenlaenge; j++) {
        zeile[j] |= anf_zeile[j];
      }
      anf_zeile += zeilenlaenge;
      /* zeilen++; */
    }
    if (hilf_vert_y < 128) {	/* selten */
      /* existiert ein Rest, davon auch einen nehmen */
      if (!yskip && rest_weite_y > 0) {
        for (j=0; j<zeilenlaenge; j++) {
          zeile[j] |= anf_zeile[j];
        }
        anf_zeile += zeilenlaenge;
        /* zeilen++; */
      }
      else {
        if (yskip && (rest_weite_y <= 0)) {
          for (j=0; j<zeilenlaenge; j++) {
            zeile[j] |= anf_zeile[j];
          }
          anf_zeile += zeilenlaenge;
          /* zeilen++; */
        }
      }
      rest_weite_y--;
      hilf_vert_y += vert_rest_y;	/* Addition, damit ich den rest immer mitbekome */
    }
    else {			/* oefters */
      if (yskip) {
        for (j=0; j<zeilenlaenge; j++) {
          zeile[j] |= anf_zeile[j];
        }
        anf_zeile += zeilenlaenge;
        /* zeilen++; */
      }
      hilf_vert_y -= 128;		/* Immer Faktor 128 abziehen */
    }

    // Rand vor dem folgenden Test loeschen
    
    zeile[0] &= 0x7FFF;			// erstes (Rand) Bit loeschen
    zeile[zeilenlaenge-1] &= 0xFFFE;	// letztes (Rand) Bit loeschen (ist immer am Word Ende)


    // testen ob die Zeile leer ist
    
    j = 0;		/* suche erstes Wort != 0 */
    while (zeile[j] == 0 && j < zeilenlaenge) {
      j++;
    }

    // wieder den Rand setzen

    *write_pointer |= 0x8000;					  // linkes Rand Bit setzen
    *(write_pointer+length_row-1) |= 1<<(length_row*16)-p_width;  // rechtes Rand Bit setzen


    if (j < zeilenlaenge) {		/* ab j nicht alles gleich 0 */
    
      bitpos = 0;
      rest_weite_x = merk_rest_weite_x;
      hilf_vert_x = 64;		/* fang ma mal mit der Haelfte an (Heuristik...?? */

      akt_row = 16-start_row;		/* Anfang der Zeile */
      copy_mask = 0;
      
      /**************** Schleife ueber die x-Werte ****************/
      for (k=0; k<p_width; k++) {		/* Schleife ueber die x Werte */
        adr = bitpos>>4;			/* Wortweise vorgehen */
        mask_anf = 32 - (bitpos & 15);	/* Die Maske ist aber 32 Bit gross */
        mask_width = weite_x;		/* Dadurch darf die Maske 16Bit gross sein */
        bitpos += weite_x;
        if (hilf_vert_x < 128 && rest_weite_x > 0) {
          hilf_vert_x += vert_rest_x;	/* um soviel soll jetzt nichts gemacht werden */
          if (xskip) {
            rest_weite_x--;
            bitpos--;
            mask_width--;
          }
          else {
	    /* Rest gleichmaesig verteilen */
            rest_weite_x--;
	    bitpos++;
	    mask_width++;
	  }
        }
        else {
          hilf_vert_x -= 128;
        }
        mask = ((1L<<mask_width)-1) << (mask_anf-mask_width);
        erg = (*((long *)((short *)zeile+adr))) & mask;
        akt_row--;
        if (erg != 0) {
          copy_mask |= (unsigned short)(1 << akt_row);
        }
        if (akt_row == 0) {
          akt_row = 16;
	  *write_pointer |= copy_mask;
	  write_pointer++;
	  copy_mask = 0;
        }
      }		/* end for k; Schleife ueber die x Werte */
      /**********************/
      if (copy_mask != 0) {
	*write_pointer |= copy_mask;
      }
    }		/* end if ==zeilenlaenge */

    add_y += length_row;
    write_pointer = start_showmap + add_y;
    
  }		/* end for i; Schleife ueber die y Werte */
  
  /* printf("zeilen: %d, wy: %d\n", zeilen, wy); */

  xfree((char *)zeile);
}

