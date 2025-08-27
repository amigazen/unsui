/* *** landscap.c *** */


#include "defines.h"

#ifndef AMIGA
#  define __stdargs
#endif

#include <stdio.h>
#include <exec/types.h>
#include <stdlib.h>

#ifdef ATARI
#  include <tos.h>
#  define Chk_Abort()		/* hope this works */
#endif

#ifdef AMIGA
#  include <dos.h>
#endif

#include "globals.h"
#include "bitmap.h"
#include "prhelp.h"

#include "globals.i"

#define BITS   7                      /* höchste vorkommende Bit-Position */

static UBYTE Mask[8] ={               /* zum ausmaskieren der einzelnen   */
               0x80,0x40,0x20,0x10,   /* Bits, müßte bei größerem BITS    */
               0x08,0x04,0x02,0x01};  /* erweitert werden, bringt nichts  */

/*
  Querdruck dreht die übergebene Bitmap um 90 Grad gegen den Uhrzeigersinn
  Aus Speicherplatzgründen wird nur Streifenweise gearbeitet. Dieser Streifen
  wird als Bitmap der Druckfunktion weitergereicht.

  *bmap    Zeiger auf die zu drehende Bitmap-Struktur
  lineno   Breite des Streifens (in Bildpunkte) Minimum 8, 24, 48 je nach
           Drucker und Auflösung, damit immer eine Druckzeile voll wird
  draft    wird an Druckfunktion weitergereicht (hat nur für HP_DeskJet
           Bedeutung)

  *Druck_Bitimage   Zeiger auf die zu verwendende Hardcopy-Funktion
                    (aus kompatibilitäts Gründen alle mit drei Argumenten)

  __stdargs  Die Funktion wird nur einmal aufgerufen!
*/

void __stdargs Querdruck(struct bitmap *bmap, long lineno, int draft,
                 void (*Druck_Bitimage) (struct bitmap *bmap, long lineno,
                 int draft))
{

  /*
    Nach Einsicht in den Code vom Global Optimizer würde ich sagen:
    'besser so'
    Ansonsten einfach mal drauf ansetzen und messen was passiert.
  */
  register int i, j, k;
  int lbit, rbit;
  register UBYTE dNull, dEins;
  long Bwidth;                   /* wenn UBYTE Bwidth => 8*255/(360/in) = 5.7in maximale Breite ?? */
  UBYTE *ptrbuf, *block, *spalte;
  register UBYTE *ptr, *ende;

  // bm_width_rounded gibt die gerundede Breite der Bitmap in *Pixel* an
  const int bm_width_rounded = ((bmap->height + (WP_GROESSE-1)) / WP_GROESSE) * WP_GROESSE;
  // round_value gibt an, um wieviel Bytes die Map groesser wird, als wenn WP_GROESSE 8 waere
  const int round_value      = (bm_width_rounded - bmap->height) / 8;	// (abrunden)

  struct bitmap Vmap;            /* Bitmap-Struktur für Gedrehtes */
  long l_count = 0;              /* Streifenzähler                */
  

  lineno >>=3;                   /* in Bytes */
  if (lineno < 1) {lineno = 1;}  /* nur für den Fall */

                                 /* Speicherplatz für gedrehte Bitmap */
  Vmap.pixptr = (long *) xmalloc(lineno * bm_width_rounded);

  Vmap.width  = bm_width_rounded;    	    /* umdrehen und auf ganze */
  Vmap.height = bmap->width;                /* Bytes runden           */

  Bwidth = (long)(bmap->width>>3);          /* Breite in Bytes */
  ptrbuf = (UBYTE *)Vmap.pixptr;            /* Zeiger Ablage Anfang */

  /*
    Das Bitimage wird Spaltenweise von hinten abgearbeitet, nur
    so ist es nach Drehung um 90 Grad gegen den Uhrzeigersinn
    auch seitenrichtig.
    Eine Spalte ist dabei 8 Bit breit, eine Bildzeile muß also
    Byte-Bündig sein!
    Ein Block sind 8 Byte im Bitimage der Seite untereinander,
    haben also im Speicher einen Abstand von Bwidth.
    Es wird im Block zuerst das letzte Bit bearbeitet, dann der
    nächste Block in der Spalte angesprungen, das letzte Bit ... ,
    bis alle Blöcke durch sind. Dann geht es im ersten Block der
    letzten Spalte mit dem vorletzten Bit weiter.
    So hangelt man sich bit- und spaltenweise nach vorne
  */

  for (spalte = (UBYTE *)bmap->pixptr+(Bwidth-1),   /* Spalte im Bitimage */
       ende   = (UBYTE *)bmap->pixptr+(Bwidth*bmap->height)-1;
       spalte >= (UBYTE *)bmap->pixptr;
       spalte--, ende--) {

    /*
      ende muß gemerkt werden, da die Höhe der Ausgangs-Bitmap
      nicht immer in Byte-Grenzen passen wird
    */

    Chk_Abort();

    for (i=BITS; i>=0; i--) {        /* Bit in der Spalte */
      for (k=0,block=spalte; k<bmap->height; k+=8,block+=Bwidth<<3) {
                                     /* Block in der Spalte */

        lbit = i;                    /* Ausgangswert Links-Schieber */
        rbit = BITS-i;               /* Ausgangswert Rechts-Schieber */
        dEins = 0;
        ptr  = block;                /* Init */

        /*
          holen, ausmaskieren für alles was links geschoben
          werden muß
        */
        for (j=lbit; (j>0 && ptr <= ende); j--, ptr+=Bwidth){
          dNull = *ptr & Mask[i];
          dNull <<= j;
          dEins |= dNull;
        }

        /*
          s.o. ohne schieben
        */
        if (ptr <= ende) {
          dNull = *ptr & Mask[i];
          dEins |= dNull;
          ptr+=Bwidth;
        }

        /*
          s.o. rechts schieben
        */
        for (j=1; (j<=rbit && ptr <= ende); j++, ptr+=Bwidth){
          dNull = *ptr & Mask[i];
          dNull >>= j;
          dEins |= dNull;
        }

        *ptrbuf = dEins;                   /* hinein in die Ablage */
        ptrbuf++;
      }  /* Block in der Spalte */

      // da mittels WP_TYP die Bitmap Breite gerundet wurde und so groesser geworden
      // sein kann (als bei Rundung durch char), muss nun der Rest noch dazu addiert werden.
      ptrbuf += round_value;	// ptrbuf muss char * sein!

    }  /* Bit in der Spalte */
    /* jetzt ist die gesamte Spalte durch, ab zum drucken */

  
    l_count++;                                /* Spalte ein Byte breiter */
    if (l_count == lineno) {                  /* Spaltenbreite zusammen? */
      Druck_Bitimage(&Vmap,lineno*8, draft);
      l_count = 0;
      ptrbuf = (UBYTE *)Vmap.pixptr;          /* Zeiger Ablage Anfang    */
    }

  }  /* Spalte im Bitimage */

  if (l_count > 0) {                        /* noch eine unvollständige */
    Druck_Bitimage(&Vmap,l_count*8, draft); /* Spalte übrig ? Muß auch  */
  }                                         /* gedruckt werden          */

  xfree(Vmap.pixptr);                       /* weg damit */
}
