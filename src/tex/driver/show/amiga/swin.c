/*
 *  Modul swin.c  (This module is PD)
 *
 *  Aufgabe: Fenster einschlaefern, so dass der Benutzer nichts
 *           mehr mit ihnen anfangen kann.
 *           Dazu bekommt das Fenster einen ZZ-Pointer und einen
 *           unsichtbaren Requester.
 *
 *  Schnittstelle:
 *
 *    void SleepWin(struct Window * win);
 *
 *      Schlaefert Window 'win' ein. Kann auch mit einem NULL Pointer
 *      aufgerufen werden. Auch mehrmaliges Einschlaefern eines
 *      Windows bringt keine Prbleme.
 *      Achtung: Ein Window muss aufgeweckt werden, bevor es geschlossen
 *               wird. Am besten einfach WakeUpWin(win) vor jedem
 *               Window Schliessen aufrufen.
 *
 *
 *    void WakeUpWin(struct Window * win);
 *
 *      Weckt ein Window wieder auf.
 *      Es macht nichts, die Funktion mit NULL, oder aber mit einem
 *      Window aufzurufen, das nicht im eingeschlaeferten Zustand ist.
 *
 *
 *    void WakeUpAll(void);
 *
 *      Weckt alle Windows auf.
 *
 *
 *  Voraussetzungen: Laeuft mit allen Betriebssystemversionen > 1.1 :)
 *
 *
 *  5.Aug.92 Georg Hessmann
 *
 */




#include <intuition/intuition.h>
#include <clib/intuition_protos.h>
#include <pragmas/intuition_pragmas.h>

#include <exec/memory.h>
#include <clib/exec_protos.h>
#include <pragmas/exec_pragmas.h>




/*------  P r o t o t y p e n  --------------*/
void SleepWin(struct Window * win);
void WakeUpWin(struct Window * win);
void WakeUpAll(void);
/*-------------------------------------------*/






extern struct ExecBase		* SysBase;
extern struct IntuitionBase	* IntuitionBase;


struct SWinLst {
  struct Window		* win;
  struct SWinLst	* prev;
  struct SWinLst	* next;
  struct Requester	  req;
};

static struct SWinLst * SWLst = NULL;

#define SIZE	sizeof(struct SWinLst)


static UWORD __chip SleepPointerData[] = {
    0x0000, 0x0000,	/* vert. and horiz. start posn. */
	0x0400,	0x07C0,
	0x0000,	0x07C0,
	0x0100,	0x0380,
	0x0000,	0x07E0,
	0x07C0,	0x1FF8,
	0x1FF0,	0x3FEC,
	0x3FF8,	0x7FDE,
	0x3FF8,	0x7FBE,
	0x7FFC,	0xFF7F,
	0x7EFC,	0xFFFF,
	0x7FFC,	0xFFFF,
	0x3FF8,	0x7FFE,
	0x3FF8,	0x7FFE,
	0x1FF0,	0x3FFC,
	0x07C0,	0x1FF8,
	0x0000,	0x07E0,
    0x0000, 0x0000,	/* reserved, must be NULL */
};


// Prototyp der internen Funktion
static struct SWinLst * FindSWin(struct Window * win);


static struct SWinLst * FindSWin(struct Window * win)
{
  struct SWinLst * hLst;
  
  for (hLst=SWLst; hLst && hLst->win != win; hLst=hLst->next);
  
  return hLst;
}



/*
 *  Funktion zum Einschlaefern von Windows.
 *
 */

void SleepWin(struct Window * win)
{
  struct SWinLst * wLst = FindSWin(win);
  
  if (win && !wLst) {
    // ist nicht schon eingeschlaefert
    wLst = (struct SWinLst *) AllocMem(SIZE, MEMF_PUBLIC|MEMF_CLEAR);
    if (wLst) {
      wLst->win = win;
      if (Request(&wLst->req, win)) {
        SetPointer(win, SleepPointerData, 16, 15, 0, 0);
        // nun wLst in die Liste aufnehmen (wird erstes Element)
        if (SWLst) {
          // wLst vor alle anderen Elemente haengen
          SWLst->prev = wLst;
          wLst->next = SWLst;
          SWLst = wLst;
        }
        else {
          // wLst wird erstes Element der Liste
          SWLst = wLst;
          // wLst->next = NULL; ist ohnehin durch CLEAR auf NULL gesetzt
        }
        // wLst->prev = NULL; ist ueberfluessig, da mit CLEAR allociert
      }
      else {
        // Requester ist nicht aufgegangen, gib speicher frei und aergere Dich
        FreeMem(wLst, SIZE);
      }
    }
  }
}



/*
 *  Funktion zum Aufwecken von Windows.
 *
 */

void WakeUpWin(struct Window * win)
{
  struct SWinLst * wLst = FindSWin(win);
  
  if (wLst) {
    // schlaeft tatsaechlich
    ClearPointer(wLst->win);
    EndRequest(&wLst->req, wLst->win);
    if (wLst->prev) {
      // wLst aushaengen. prev mit next verbinden
      struct SWinLst * prev, * next;
      prev = wLst->prev;
      next = wLst->next;
      prev->next = next;
      if (next) next->prev = prev;
    }
    else {
      SWLst = wLst->next;
      if (SWLst) SWLst->prev = NULL;
    }
    FreeMem(wLst, SIZE);
  }
}


/*
 *  Diese Funktion weckt auf ein Mal alle eingeschlaeferten
 *  Windows auf.
 *
 */

void WakeUpAll(void)
{
  while (SWLst) {
    WakeUpWin(SWLst->win);
  }
}


