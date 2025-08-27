/* Boyer-Moor Suchalgorithmus nach Algorithmen in C von Robert Sedgewick */


#include "defines.h"

#include <stdio.h>
#include <ctype.h>

#include <graphics/gfx.h>

#include "commands.h"
#include "globals.h"
#include "globvars.h"

#ifdef ANSI
#  include <string.h>
#  include <stdlib.h>
#endif

#include "globals.i"
#include "search.i"


#ifndef min
# define min(a,b)	((a)<(b) ? (a) : (b))
#endif
#ifndef max
# define max(a,b)	((a)>(b) ? (a) : (b))
#endif


static __inline int doBMSearch(void);



/* Boyer-Moore */

static unsigned char StrStep[256];		// Array über alle vorkommbaren Zeichen
static char StrBuf[256];			// Puffer, in dem gesucht wird
static int  StrBufPtr;				// Zeiger in dem Suchpuffer

       char  SearchPattern[102] = "";		// Such Pattern (Pattern muss kleiner StrBuf sein!)
       						// wird direkt auch schon verwendet/gesetzt
static int   PatternLen;			// Laenge des Patterns

static struct Rectangle SRects[256];		// Positionen der Buchstaben (2kB)

static struct Rectangle LocalRect;		// da wird das Ergebniss drin gespeichert



/**
 ** Suche in einem fortlaufenden Strom von Buchstaben:
 **
 ** Vor Beginn der eigentlichen Suche muss InitBMSearch() mit dem
 ** zu suchenden Muster aufgerufen werden. Dieses Muster muss 
 ** kuerzer sein, als der StrBuf.
 **
 ** Bei der eigentlichen Suche wird fuer jeden Buchstaben BMSearch
 ** aufgerufen. Dabei wird der eigentliche Suchvorgang nur bei
 ** vollem Buffer, oder aber bei dem '\0' Zeichen aufgerufen.
 ** Am Ende einer Seite muss also die Suche immer noch expliziet mit
 ** einem BMSearch('\0') aufgerufen und damit der Buffer entlehrt
 ** werden.
 **
 ** Wenn BMSearch() etwas gefunden hat, dann wird die globale
 ** Variable SearchRect auf eine Rechteck Struktur gesetzt.
 ** Ansonsten ist SearchRect immer gleich NULL.
 **
 **/


void InitBMSearch(char * p)
{
  int i;
  int M = strlen(p);

  for (i=0; i<256; i++) StrStep[i]    = M;
  for (i=0; i<M; i++)   StrStep[p[i]] = M-i-1;
  
  StrBufPtr = 0;
  StrBuf[sizeof(StrBuf)-1] = '\0';
  
  if (M >= sizeof(SearchPattern)-2) {
    FatalStr(20, "zu langes Suchmuster");
  }
  
  strcpy(SearchPattern, p);
  PatternLen = M;
  
  SearchRect = NULL;
}


// static FILE * debout = NULL;

void BMSearch(char a, int x, int y, int w, int h)
{
  static int count = 0;

  int ret = -1;
  int M = PatternLen;
  
  // if (!debout) debout = fopen("ram:debout", "w");
  // if (debout) fprintf(debout, "%d (%d, %d, %d, %d)\n", a, x, y, w, h);
  
  if (StrBufPtr >= sizeof(StrBuf)-1 || a == '\0') {
    ret = doBMSearch();

    // wurde was gefunden?
    if (ret == sizeof(StrBuf)-1) ret = -1;
    else {
      LocalRect.MinX = min(SRects[ret+1].MinX, SRects[ret+M].MinX);
      LocalRect.MinY = min(SRects[ret+1].MinY, SRects[ret+M].MinY);
      LocalRect.MaxX = max(SRects[ret+1].MaxX, SRects[ret+M].MaxX);
      LocalRect.MaxY = max(SRects[ret+1].MaxY, SRects[ret+M].MaxY);
      ret += count;
    }
    
    if (a == '\0') {
      // erzwungenes Such-Ende. Puffer komplett leeren.
      count = 0;
      StrBufPtr = 0;
    }
    else {
      // Anzahl gelesener Buchstaben erhoehen (minus rechten Rand)
      count += sizeof(StrBuf)-1 - M;

      // rechten Rand nach links kopieren
      for (StrBufPtr=0; StrBufPtr < M-1; StrBufPtr++) {			// kopiere die rechten M Zeichen nach links um neu anzufangen
        StrBuf[StrBufPtr] = StrBuf[sizeof(StrBuf)-1-M+1+StrBufPtr];
        SRects[StrBufPtr] = SRects[sizeof(StrBuf)-1-M+1+StrBufPtr];	// Kopie einer Struktur
      }
    }
  }

  if (a) {
    StrBuf[StrBufPtr] = a;
    SRects[StrBufPtr].MinX = x;
    SRects[StrBufPtr].MinY = y;
    SRects[StrBufPtr].MaxX = x+w;
    SRects[StrBufPtr].MaxY = y+h;
    StrBufPtr++;
  }
  
  if (ret != -1) {
    // es wurde was gefunden
    SearchRect = &LocalRect;
  }
  else SearchRect = NULL;
}




static __inline int doBMSearch(void)
{
  int i, j, t;

  char * a = StrBuf;
  char * p = SearchPattern;
  int    M = PatternLen;
  int    N = strlen(a);
  
  for (i=M-1, j=M-1; j>=0; i--, j--) {
    while (a[i] != p[j]) {
      t = StrStep[a[i]];
      i += (M-j > t) ? M-j : t;
      if (i >= N) return N;
      j = M-1;
    }
  }
  
  return i;
}

