/**********************************************************************/
/*                                                                    */
/* liste.c  : Modul zum Verwalten doppeltverketteter Listen           */
/*            mit Hilfe eines Listenkopfes und eines Cursors.         */
/*            15.07.88  Georg Hessmann                                */
/*            20.08.88  Robert Stabl (revised)                        */
/**********************************************************************/


#include "defines.h"
#include <stdio.h>
#include "globals.h"
#include "liste.h"

#ifdef ANSI
#  include <stdlib.h>
#endif


#include "globals.i"
#include "liste.i"


extern short use_phy_number;

static struct head *head = NULL;		/* Global variable */


static long take_pptr_log    (long pnr, long pptr);
#if 0
static int  take_next_log    (long *pnr, long *pptr);
static int  take_prev_log    (long *pnr, long *pptr);
#endif
static void take_first_log   (long *pnr, long *pptr);
static void take_last_log    (long *pnr, long *pptr);
static void take_less_log    (long apnr, long apptr, long *pnr, long *pptr);
static void take_greater_log (long apnr, long apptr, long *pnr, long *pptr);

static long take_pptr_phy    (long phynr);
#if 0
static int  take_next_phy    (long *pnr, long *pptr);
static int  take_prev_phy    (long *pnr, long *pptr);
#endif
static void take_first_phy   (long *pnr, long *pptr);
static void take_last_phy    (long *pnr, long *pptr);
static void take_less_phy    (long apptr, long *pnr, long *pptr);
static void take_greater_phy (long apptr, long *pnr, long *pptr);


/**********************************/
/* initialisiert einen Listenkopf */

int init_liste(void)	/* ret: TRUE || FALSE */
{
  head = (struct head *)xmalloc(sizeof(struct head));
  if (head == NULL) {
    return FALSE;
  }
  else {
    head->cursor    = NULL;
    head->first     = NULL;
    head->last      = NULL;
    head->first_phy = NULL;
    head->last_phy  = NULL;
    return TRUE;
  }
}


/************************/
/* gibt eine Liste frei */

void free_liste(void)
{
  register struct liste *lis, *hlis;

  if (head == NULL) {
    FatalStr(20, "no list-head?!");
  }

  lis = head->last;
  while (lis != NULL)
   {
    hlis = lis->prev;
    xfree((char *)lis);
    lis = hlis;
   }
  xfree((char *)head);
  head = NULL;
}


/**********************************************************************/
/* fuegt ein Element in die Liste ein und setzt den Cursor auf dieses */

void in_list(long pnr, long pptr, long phy_nr)
{
  struct liste *lis, *list;
  long count;

  if (head == NULL) {
    FatalStr(20, "no list-head?!");
  }

  lis = (struct liste *)xmalloc(sizeof(struct liste)); 

  lis->pagenr = pnr;
  lis->pageptr = pptr;
  lis->phy_nr = phy_nr;
  lis->next_phy = NULL;
  lis->secundary_nr = 0;		/* 0 => only one page with pagenr */

 /* wenn phy_nr == 0, dann ist das DVI-File defekt und phy_nr muss berechnet werden */

  if (head->first == NULL) {       /* leere Liste */
   lis->prev = (struct liste *)NULL;
   lis->next = (struct liste *)NULL;
   head->first = lis;
   head->last = lis;
 }
 else {
   if (head->first->pagenr >= pnr) {      /* an den Anfang der Liste */
     lis->next = head->first;
     lis->prev = (struct liste *)NULL;
     head->first->prev = lis;
     head->first = lis;
   }
   else {
     if (head->last->pagenr <= pnr) {     /* an das Ende der Liste  */
       lis->next = (struct liste *)NULL;
       lis->prev = head->last;
       head->last->next = lis;
       head->last = lis;
     }
     else {
       list = head->first;
       while ((list->pagenr < pnr) && (list->next != NULL))
        {
         list = list->next;        /* weitergehen */
        }

       if (list->pagenr >= pnr)    /* nicht ans Ende der Liste */
        {
         lis->prev = list->prev;
         lis->next = list;
         list->prev = lis;
         lis->prev->next = lis;
        }
       else
        {
         FatalStr(20,"Error in \"in_list\"!");
        }
      }
    }
  }
 head->cursor = lis;

 if (phy_nr == 0L) {
   count = 1;			/* berechne die physical-page-number		*/
   list = head->first;		/* es wird vorausgesetzt, dass alle Seiten vor	*/
   while (list != NULL) {	/* der aktuellen schon in der liste sind	*/
     if (list->pageptr < pptr) {
       count++;
     }
     list = list->next;
   }
   lis->phy_nr = count;
   if (count == 1) {
     head->first_phy = lis;
   }
   head->last_phy = lis;

   list = head->first;		/* correct the list of phy numbers	*/
   if (count > 1) {
     while (list != NULL && list->phy_nr == count-1) {
       list = list->next;
     }
     if (list != NULL) {
       list->next_phy = lis;
     }
     else {
       FatalStr(20,"Error in \"in_list\"!");
     }
   }
   list = head->first;		/* calculate secundary number		*/
   count = 0;
   while (list != NULL) {
     if (list->pagenr == pnr) count++;
     list = list->next;
   }
   if (count > 1) {
     lis->secundary_nr = count;
   }
 }

 return;
}


/* get's the logical number from the physical or logical */

long get_page_number(long nr)
{
  struct liste *lis;

  if (use_phy_number == 0)
    return nr;

  lis = head->first;
#if 0
  if (lis != NULL) {
      while (lis->phy_nr != nr && lis->next != NULL) {
        lis = lis->next;
      }
  }	/* lis kann NULL sein !! (Wozu sonst die Abfrage vor while ? ;-) */
  if (lis->phy_nr == nr) {
      ret = lis->pagenr;
  }
#else
  while( lis != NULL && lis->phy_nr != nr )
    lis = lis->next;
  if( lis != NULL )	/* dann muss (lis->phy_nr == nr) sein */
    return lis->pagenr;
#endif
  return 0L;
}

/* get's the logical number from the physical */

long get_log_page_number(long phy)
{
  struct liste *lis;

  if (head == NULL) {
    FatalStr(20, "no list-head?!");
  }

  lis = head->first;
  if (lis != NULL) {
    while (lis->phy_nr != phy && lis->next != NULL) {
      lis = lis->next;
    }
    if (lis->phy_nr == phy) {
      return lis->pagenr;
    }
  }

  return 0L;
}


/* get the pysical number from the file pointer */

long get_phy_number(long cpagep)
{
  struct liste *lis;

  if (head == NULL) {
    FatalStr(20, "no list-head?!");
  }

  lis = head->first;
  if (lis != NULL) {
    while (cpagep != lis->pageptr && lis->next != NULL) {
      lis = lis->next;
    }
    if (cpagep == lis->pageptr) {
      return lis->phy_nr;
    }
  }
  return 0L;
}


long get_secundary(long phy)
{
  struct liste *lis;

  if (head == NULL) {
    FatalStr(20, "no list-head?!");
  }

  lis = head->first;
  if (lis != NULL) {
    while (lis->phy_nr != phy && lis->next != NULL) {
      lis = lis->next;
    }
    if (lis->phy_nr == phy) {
      return lis->secundary_nr;
    }
  }
  
  return 0L;
}


long take_pptr(long pnr_log, long pptr, long phy_nr, short is_phy)
{
  if (is_phy) {
    return take_pptr_phy(phy_nr);
  }
  else {
    return take_pptr_log(pnr_log, pptr);
  }
}

#if 0
int  take_next(long *pnr,long *pptr)
{
  if (use_phy_number) {
    return take_next_phy(pnr, pptr);
  }
  else {
    return take_next_log(pnr, pptr);
  }
}

int  take_prev(long *pnr,long *pptr)
{
  if (use_phy_number) {
    return take_prev_phy(pnr, pptr);
  }
  else {
    return take_prev_log(pnr, pptr);
  }
}
#endif

void take_first(long *pnr,long *pptr)
{
  if (use_phy_number) {
    take_first_phy(pnr, pptr);
  }
  else {
    take_first_log(pnr, pptr);
  }
}

void take_last(long *pnr,long *pptr)
{
  if (use_phy_number) {
    take_last_phy(pnr, pptr);
  }
  else {
    take_last_log(pnr, pptr);
  }
}

void take_less(long apnr,long apptr,long *pnr,long *pptr)
{
  if (use_phy_number) {
    take_less_phy(apptr, pnr, pptr);
  }
  else {
    take_less_log(apnr, apptr, pnr, pptr);
  }
}

void take_greater(long apnr,long apptr,long *pnr,long *pptr)
{
  if (use_phy_number) {
    take_greater_phy(apptr, pnr, pptr);
  }
  else {
    take_greater_log(apnr, apptr, pnr, pptr);
  }
}




/****************************************************************************/
/* holt den pageptr der Seite mit der Nr pnr und setzt den Cursor auf diese */

static long take_pptr_log( long pnr, long pptr)
{
 register struct liste *lis;
 long ret = 0L;

 lis = head->first;
 if (lis == NULL)
  {
   return 0L;
  }
 while((lis->pagenr < pnr) && (lis->next != NULL))
  {
   lis = lis->next;
  }
 while (lis->pagenr == pnr)
  {
   if ((pptr==lis->pageptr) || (pptr == 0L))
    {
     head->cursor = lis;
     ret = lis->pageptr;
     break;
    }
   if (lis->next == NULL)
    {
     break;
    }
   lis = lis->next;
  }
 return (ret);
}


/***************************************************************/
/* holt pagenr und pageptr der Seite nach dem aktuellen Cursor */
/* return 0 => error, Ende der Liste ueberschritten            */

#if 0
static int take_next_log(long *pnr,long *pptr)
{
 register struct liste *lis;

 lis = head->cursor;
 if (lis == NULL || lis->next == NULL)
  {
   return (0);           /* error */
  }
 else
  {
   *pnr = lis->next->pagenr;
   *pptr = lis->next->pageptr;
   head->cursor = lis->next;
   return (1);
  }
}
#endif

/**************************************************************/
/* holt pagenr und pageptr der Seite vor dem aktuellen Cursor */
/* return 0 => error, Anfang der Liste ueberschritten         */

#if 0
static int take_prev_log(long *pnr,long *pptr)
{
 register struct liste *lis;

 lis = head->cursor;
 if (lis == NULL || lis->prev == NULL)
  {
   return (0);
  }
 else
  {
   head->cursor = lis->prev;
   *pnr = lis->prev->pagenr;
   *pptr = lis->prev->pageptr;
   return (1);
  }
}
#endif


/**********************************************************************/
/* holt die Werte des ersten Elements und setzt den Cursor auf dieses */

static void take_first_log(long *pnr,long *pptr)
{
 head->cursor = head->first;
 if (head->cursor != NULL)
  {
   *pnr = head->cursor->pagenr;
   *pptr = head->cursor->pageptr;
  }
 else
  {
   *pnr = 0L;
   *pptr = 0L;
  }
}


/***********************************************************************/
/* holt die Werte des letzten Elements und setzt den Cursor auf dieses */

static void take_last_log(long *pnr,long *pptr)
{
 head->cursor = head->last;
 if (head->cursor != NULL)
  {
   *pnr = head->cursor->pagenr;
   *pptr = head->cursor->pageptr;
  }
 else
  {
   *pnr = 0L;
   *pptr = 0L;
  }
}

/*************************************************************/
/* sucht die Nummer und den Pointer der nachstkleinere Seite */

static void take_less_log(long apnr,long apptr,long *pnr,long *pptr)
{
 struct liste *lis;

 lis = head->last;
 if (lis == NULL)
  {
   *pnr = 0L;
   *pptr = 0L;
   return;
  }
 while ((lis->pagenr > apnr) && (lis->prev != NULL))
  {
   lis = lis->prev;
  }
 if (lis->prev == NULL)
  {
   *pnr = 0L;
   *pptr = 0L;
   return;
  }
 while (lis->pagenr == apnr)
  {
   if ((apptr==lis->pageptr) || (apptr == 0L))
    {
     if (lis->prev != NULL)
      {
       *pnr = (lis->prev)->pagenr;
       *pptr = (lis->prev)->pageptr;
       return;
      }
     else
      {
       *pptr = 0L;
       *pnr  = 0L;
       return;
      }
    }
   if (lis->prev == NULL)
    {
     *pptr = 0L;
     *pnr = 0L;
     return;
    }
   lis = lis->prev;
  }
 *pnr = 0L;
 *pptr = 0L;
}


/***************************************************************/
/* sucht die Nummer und den Pointer der nachstgroesseren Seite */

static void take_greater_log(long apnr,long apptr,long *pnr,long *pptr)
{
 struct liste *lis;

 lis = head->first;
 if (lis == NULL)
  {
   *pnr = 0L;
   *pptr = 0L;
   return;
  }
 while ((lis->pagenr < apnr) && (lis->next != NULL))
  {
   lis = lis->next;
  }
 if (lis->next == NULL)
  {
   *pnr = 0L;
   *pptr = 0L;
   return;
  }
 while (lis->pagenr == apnr)
  {
   if ((apptr==lis->pageptr) || (apptr == 0L))
    {
     if (lis->next != NULL)
      {
       *pnr = (lis->next)->pagenr;
       *pptr = (lis->next)->pageptr;
       return;
      }
     else
      {
       *pptr = 0L;
       *pnr  = 0L;
       return;
      }
    }
   if (lis->next == NULL)
    {
     *pptr = 0L;
     *pnr = 0L;
     return;
    }
   lis = lis->next;
  }
 *pnr = 0L;
 *pptr = 0L;
}


/****************************************************************************/
static long take_pptr_phy(long phynr)
{
  struct liste *lis;

  lis = head->first;
  while (lis != NULL && lis->phy_nr != phynr) {
    lis = lis->next;
  }
  if (lis != NULL)
    return lis->pageptr;

  return 0L;
}


/****************************************************************************/
static void take_less_phy(long apptr,long *pnr,long *pptr)
{
  struct liste *lis, *slis = NULL;
  long curptr = 0L;

  lis = head->first;
  while (lis != NULL) {
    if (lis->pageptr < apptr && lis->pageptr > curptr) {
      slis = lis;
      curptr = lis->pageptr;
    }
    lis = lis->next;
  }
  if (slis == NULL) {
    *pnr = 0L;
    *pptr = 0L;
  }
  else {
    *pnr = slis->pagenr;
    *pptr = slis->pageptr;
    head->cursor = slis;
  }
}



static void take_greater_phy(long apptr,long *pnr,long *pptr)
{
  struct liste *lis;

  lis = head->first_phy;
  while (lis != NULL && lis->pageptr <= apptr) {
    lis = lis->next_phy;
  }
  if (lis == NULL) {
    *pnr  = 0L;
    *pptr = 0L;
  }
  else {
    *pnr  = lis->pagenr;
    *pptr = lis->pageptr;
    head->cursor = lis;
  }
}

#if 0
/****************************************************************************/
static void take_greater_phy(long apptr,long *pnr,long *pptr)
{
  struct liste *lis, *slis = NULL;
  long curptr = 0x0FFFFFFF;

  lis = head->first;
  while (lis != NULL) {
    if (lis->pageptr > apptr && lis->pageptr < curptr) {
      slis = lis;
      curptr = lis->pageptr;
    }
    lis = lis->next;
  }
  if (slis == NULL) {
    *pnr = 0L;
    *pptr = 0L;
  }
  else {
    *pnr = slis->pagenr;
    *pptr = slis->pageptr;
    head->cursor = slis;
  }
}
#endif



#if 0
static int  take_next_phy    (long *pnr,long *pptr)
{
  FatalStr(10,"Not jet implemented!");
  return 0;
}
#endif

#if 0
static int  take_prev_phy    (long *pnr,long *pptr)
{
  FatalStr(10,"Not jet implemented!");
  return 0;
}
#endif


static void take_first_phy   (long *pnr,long *pptr)
{
  register struct liste *lis = head->first_phy;

  if (lis == NULL) {
    *pnr  = 0L;
    *pptr = 0L;
  }
  else {
    *pnr  = lis->phy_nr;
    *pptr = lis->pageptr;
    head->cursor = lis;
  }
}

#if 0
static void take_first_phy   (long *pnr,long *pptr)
{
  struct liste *lis, *slis = NULL;
  long curptr = 0x0FFFFFFF;

  lis = head->first;
  while (lis != NULL) {
    if (lis->pageptr < curptr) {
      slis = lis;
      curptr = lis->pageptr;
    }
    lis = lis->next;
  }
  if (slis == NULL) {
    *pnr  = 0L;
    *pptr = NULL;
  }
  else {
    *pnr = slis->phy_nr;
    *pptr = slis->pageptr;
    head->cursor = slis;
  }
}
#endif


static void take_last_phy    (long *pnr,long *pptr)
{
  register struct liste *lis = head->last_phy;

  if (lis == NULL) {
    *pnr  = 0L;
    *pptr = 0L;
  }
  else {
    *pnr  = lis->phy_nr;
    *pptr = lis->pageptr;
    head->cursor = lis;
  }
}

#if 0
static void take_last_phy    (long *pnr,long *pptr)
{
  struct liste *lis, *slis = NULL;
  long curptr = 0L;

  lis = head->first;
  while (lis != NULL) {
    if (lis->pageptr > curptr) {
      slis = lis;
      curptr = lis->pageptr;
    }
    lis = lis->next;
  }
  if (slis == NULL) {
    *pnr  = 0L;
    *pptr = NULL;
  }
  else {
    *pnr = slis->phy_nr;
    *pptr = slis->pageptr;
    head->cursor = slis;
  }
}
#endif




/****************************************************************************/
/* reverse the phy numbers and sort the phy numbers*/
void calc_phy_nr(long max_pages)
{
  struct liste *lis, *lis2;
  long i;

  max_pages++;
  lis = head->first;
  while (lis != NULL) {
    lis->phy_nr = max_pages - lis->phy_nr;
    lis = lis->next;
  }
  max_pages--;
  if (max_pages > 1) {			/* set up the list of phy numbers */
    lis = head->first;
    while (lis != NULL && lis->phy_nr != 1) {
      lis = lis->next;
    }
    head->first_phy = lis;
    for (i=2; i<=max_pages; i++) {
      lis2 = head->first;
      while (lis2 != NULL && lis2->phy_nr != i) {
        lis2 = lis2->next;
      }
      lis->next_phy = lis2;
      lis = lis2;
    }
    head->last_phy = lis;

    /* calculate the secundary numbers */    
    for (lis = head->first; lis != NULL; lis = lis->next) {
      if (lis->next != NULL) {
        if (lis->next->pagenr == lis->pagenr) {
          lis->secundary_nr++;
          if (lis->next->next == NULL ||
	      (lis->next->next != NULL &&
	       lis->next->next->pagenr != lis->pagenr)) {
            lis->next->secundary_nr = lis->secundary_nr+1;
          }
          else {
            lis->next->secundary_nr = lis->secundary_nr;
          }
        }
      }
    }
  }
  else {	/* only one page */
    head->last_phy = head->first_phy = head->first;
  }
}


/****************************************************************************/
/* debug Funktion */
void print_list(void)
{
  struct liste *lis;

  lis = head->first;
  while (lis != NULL) {
    LoggingStr("# logical number: %2ld, physical number: %2ld, sec number: %ld, file ptr: %5ld",
		lis->pagenr, lis->phy_nr, lis->secundary_nr, lis->pageptr);
    lis = lis->next_phy;
  }
}
