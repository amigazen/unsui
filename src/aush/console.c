/*
* This file is part of AUSH.
* Copyright (C) 1994 Denis Gounelle
* 
* AUSH is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* AUSH is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with AUSH.  If not, see <http://www.gnu.org/licenses/>.
*
*/
/*
 * RawConsole.c
 *
 * Shell 2.07M	17-Jun-87
 * console handling, command line editing support for Shell
 * using new console packets from 1.2.
 * Written by Steve Drew. (c) 14-Oct-86.
 * 16-Dec-86 Slight mods to rawgets() for Disktrashing.
 * Version 4.01A by Carlo Borreo & Cesare Dieni 17-Feb-90
 *
 * Heavy mods by Denis GOUNELLE to add filename/varname completion, file request,
 * and other features for AUSH
 */

#define SRC_CONSOLE_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

#ifdef unix
#include <curses.h>
#include <term.h>
#include <termio.h>
#endif

/* positions in EditKeys[] */

#define BEGINL	1  /* begin line */
#define PREVC	2  /* previous char */
#define LISTC	3  /* list completion */
#define ENDL	4  /* end line */
#define NEXTC	5  /* next char */
#define KENDL	6  /* kill end line */
#define RFSHL	7  /* refresh line */
#define NEXTL	8  /* next line */
#define PREVL	9  /* previous line */
#define SWAPC	10 /* swap char */
#define BEGINH	11 /* begin history */
#define REPLV	12 /* replace var */
#define KENDW	13 /* kill end word */
#define KILLL	14 /* kill line */
#define ENDH	15 /* end history */
#define NBKEYS	ENDH

static char EditKeys[NBKEYS+1] ;

/* names of control sequences */

#define CS_CON	 0  /* cursor on  */
#define CS_COFF  1  /* cursor off */
#define CS_KEOD  2  /* erase to end of display */
#define CS_PREVL 3  /* previous line */
#define CS_NEXTC 4  /* next character */
#define CS_PREVC 5  /* previous character */
#define CS_DELC  6  /* delete character */
#define CS_INSC  7  /* insert space */
#define CS_NBR	 (CS_INSC+1)

static char *CS_TermNames[CS_NBR] =	/* capnames to get control sequences */
{
  "ve", "vi", "cd", "up", "nd" , "le", "dc", "ic"
} ;

static char *ControlSeq[CS_NBR] =	/* pointers to control sequences */
{
  "\x9b\x20p" , "\x9b\x30\x20p", "\233J", "\233F", "\233C", "\233D", "\233P", "\233@"
} ;

/* special keys (arrows, f1, etc...) */

#define SK_UP	  0  /* up arrow */
#define SK_DOWN   1  /* down arrow */
#define SK_LEFT   2  /* left arrow */
#define SK_RIGHT  3  /* right arrow */
#define SK_SLEFT  4  /* shifted left arrow */
#define SK_SRIGHT 5  /* shifted right arrow */
#define SK_HELP   6  /* help key */
#define SK_F1	  7  /* functions keys */
#define SK_F2	  8
#define SK_F3	  9
#define SK_F4	  10
#define SK_F5	  11
#define SK_F6	  12
#define SK_F7	  13
#define SK_F8	  14
#define SK_F9	  15
#define SK_F10	  16
#define SK_NOTS   SK_F10
#define SK_SF1	  17 /* shifted functions keys */
#define SK_SF2	  18
#define SK_SF3	  19
#define SK_SF4	  20
#define SK_SF5	  21
#define SK_SF6	  22
#define SK_SF7	  23
#define SK_SF8	  24
#define SK_SF9	  25
#define SK_SF10   26
#define SK_SUP	  27  /* shifted up arrow */
#define SK_CLOSE  28  /* window close gadget */
#define SK_NBKEYS (SK_CLOSE+1)

static char *SK_TermNames[SK_NOTS+1] =	/* capnames to get special keys values */
{
  "ku", "kd", "kl", "kr", "#4", "%i", "%1", "k1", "k2", "k3", "k4", "k5",
  "k6", "k7", "k8", "k9", "k;"
} ;

static char NewKey[MAXNAME+1] ;
static char *SpecialKeys[SK_NBKEYS] =	/* pointers to special keys values */
{
  "\233A", "\233B", "\233D", "\233C", "\233 A", "\233 @", "\233?~",
  "\233\60~", "\233\61~", "\233\62~", "\233\63~", "\233\64~", "\233\65~",
  "\233\66~", "\233\67~", "\233\70~", "\233\71~",
  "\233\61\60~", "\233\61\61~", "\233\61\62~", "\233\61\63~", "\233\61\64~",
  "\233\61\65~", "\233\61\66~", "\233\61\67~", "\233\61\70~", "\233\61\71~",
  "\233T", "\233\61\61;0;"
} ;

/* other variables */

#ifdef unix
static char *term, tinfo[1024] ;
static struct termio etat1, etat2 ;
#endif

static int max, pos, plen, width = 80 ;
static char *tyahdptr, typeahd[MAXLINE], tmp[MAXLINE], aux[MAXCMD] = { '\0' } ;

#ifdef _AMIGA
extern struct Process *Moi ;
extern struct Window *ConWin ;
#endif

extern struct stat sbuf ;
extern struct List TeteHist ;
extern int HistNumber, HistCount ;

#define CURSOR_ON  SendCommand( CS_CON  , 1 ) ;
#define CURSOR_OFF SendCommand( CS_COFF , 1 ) ;

/****************************************************************************/

static void SendCommand( int cs , int nb )

/*
 * Send a control sequence to the terminal
 * cs = sequence number (like CS_KEOD)
 * nb = number of times we have to send the command
 */

{
  char *p ;

  p = ControlSeq[cs] ;
  if ( ! p ) return ;

  while ( nb > 0 )
  {
    printf( p ) ;
    nb-- ;
  }
}

/****************************************************************************/

#ifdef unix
static int GetControlSeq( void )

/*
 * Get control sequences and special keys values
 * Usefull only if terminal is not "amiga"
 */

{
  int n ;
  char *p ;

  /* recupere le type de terminal */

  term = getenv( "TERM" ) ;
  if ( ! term ) return( FALSE ) ;

  /* prepare le decodage */

  if ( tgetent( tinfo , term ) == ERR ) return( FALSE ) ;
  if (! strcmp( term , "amiga" )) return( TRUE ) ;
  p = tinfo ;

  /* lecture des sequences de controle */

  for ( n = CS_CON ; n <= CS_INSC ; n++ )
    ControlSeq[n] = tgetstr( CS_TermNames[n] , &p ) ;

  /* lecture des touches speciales */

  for ( n = SK_UP ; n <= SK_NOTS ; n++ )
    SpecialKeys[n] = tgetstr( SK_TermNames[n] , &p ) ;

  return( TRUE ) ;
}
#endif

/****************************************************************************/

static int myget( struct MyFile *in )  /* returns next input character */
{
  char c ;

  if ( c = *tyahdptr )
  {
    tyahdptr++ ;
    return( (int)c ) ;
  }
  return( FGetchar( in ) ) ;
}

/****************************************************************************/

static int GetWidth( void )     /* returns number of columns of display */
{
  int width ;

#ifdef _AMIGA
  width  = ConWin->Width - ConWin->BorderLeft - ConWin->BorderRight ;
  if ( ConWin->RPort->TxWidth ) width /= ConWin->RPort->TxWidth ;
#endif

#ifdef unix
  width = tgetnum( "cols" ) ;
#endif

  return( width ) ;
}

/****************************************************************************/

static void SetRawMode( int flg )       /* set terminal raw mode on/off */
{
#ifdef _AMIGA
  MySendPacket( ACTION_SCREEN_MODE , (flg) ? -1 : 0 ) ;
#endif

#ifdef unix
  ioctl( 0 , TCSETA , (flg) ? &etat2 : &etat1 ) ;
#endif
}

/****************************************************************************/

static int GetCurWord( char *line , int *curpos )

/*
 * Get the word under the cursor and return it's length
 * line   = buffer to receive the word
 * curpos = pointer to a place to put cursor position in the word (may be NULL)
 */

{
  int j, l ;

  l = 0 ;
  if ( curpos ) *curpos = pos ;
  for ( j = pos - 1 ; (line[j] != ' ') && (j >= 0) ; j-- ) ;
  j++ ;
  for ( l = 0 ; ((tmp[l] = line[j]) != ' ') && (tmp[l]) ; l++ , j++ )
    if ( (j == pos) && curpos ) *curpos = l ;

  tmp[l] = '\0' ;
  return( l ) ;
}

/****************************************************************************/

static int DoCompletion( char *name , int addthem , int curpos )

/*
 * Expand "name" as far as possible, and add the new characters in
 * typeahd[] if "addthem" is not zero.
 * If necessary, the remaining choices are left in "name" in order
 * to be displayed.
 */

{
  int flg ;	     /* 1 if file completion, 0 if var completion */
  char *p, a, b ;
  int nb, k, l, lg ;
  static char *choices[MAXARG] ;

  lg = strlen( name ) ;
  if ( curpos > lg ) curpos = lg ;
  strcpy( aux , name ) ;

  /* expand */

  if ( flg = (*name != '$') )
  {
    if (! (p = myGetVar( "filepat" ))) p = "*" ;
    strcpy( &name[curpos] , p ) ;
    strcat( name , &aux[curpos] ) ;
    nb = ExpandPat( name , choices ) ;
  }
  else
  {
    nb = ExpandVar( name , choices ) ;
    curpos = lg - 1 ;
    lg -= 2 ;
  }

  if ( ! nb )
  {
    putchar( '\007' ) ;
    fflush( stdout ) ;
    return( 0 ) ;
  }

  p = choices[0] ;
  p = &p[curpos] ;

  /* add the new characters to typeahd */

  if ( addthem )
  {
    /* only one choice ? */

    if ( nb == 1 )
    {
      k = strlen( p ) - lg + curpos ;
      if ( k < 0 ) k = 0 ;
      p[k] = '\0' ;

      strcpy( typeahd , p ) ;
      strcat( aux , p ) ;

      if ( (flg) && (curpos == lg) && (! stat( aux , &sbuf )) )
	strcat( typeahd , (sbuf.st_mode & S_IFDIR) ? "/" : " " ) ;

      *name = '\0' ;
      return( 1 ) ;
    }

    /* several choices: expand as far as possible */

    l = 0 ;
    lg = curpos ;
    while ( *p )
    {
      a = ( flg ) ? tolower( *p ) : *p ;

      for ( k = 1 ; k < nb ; k++ )
      {
	b = choices[k][lg] ;
	if ( flg ) b = tolower( b ) ;
	if ( a != b ) break ;
      }

      if ( k < nb ) break ;
      typeahd[l] = *p ;
      lg++ ;
      l++ ;
      p++ ;
    }
  }
  else l = 0 ;

  typeahd[l] = '\0' ;

  /* copy remaining choices to "name" */

  p = name ;
  for ( k = 0 ; k < nb ; k++ )
    if ( p == name ) p = strpcpy( p , choices[k] ) ;
		else p = strxcat( p , " " , choices[k] , NULL ) ;

  return( nb ) ;
}

/****************************************************************************/

#ifdef _AMIGA
void DoRequest( char *line )   /* file requester */
{
  char *p ;
  int l, j ;

  /* copy previous word (up to '/' or ':') in fr_Dir */

  l = GetCurWord( line , NULL ) ;
  p = strrchr( tmp , '/' ) ;
  if ( ! p ) p = strrchr( tmp , ':' ) ;
  if ( p ) p[1] = '\0' ;

  /* do the request and copy the selected file/dir in typeahd */

  if ( DoFileRequest( tmp ) )
  {
    for ( j = 0 ; j < l ; j++ ) typeahd[j] = '\010' ;
    typeahd[l] = '\0' ;
    strcat( typeahd , tmp ) ;
    tyahdptr = typeahd ;
  }
}
#endif

/****************************************************************************/

void DoReplace( char *line )   /* replace variable by it's value */
{
  char *p ;
  int l, j ;

  /* get current word */

  l = GetCurWord( line , NULL ) ;
  if ( ! l ) return ;
  p = tmp ;
  if ( *p == '$' ) p++ ;

  /* expand var and copy it in typehead */

  if ( p = myGetVar( p ) )
  {
    for ( j = 0 ; j < l ; j++ ) typeahd[j] = '\010' ;
    typeahd[l] = '\0' ;
    strcat( typeahd , p ) ;
    tyahdptr = typeahd ;
    return ;
  }

  putchar( '\007' ) ;
  fflush( stdout ) ;
}

/****************************************************************************/

char *rawgets( struct MyFile *in , char *line , char *prompt )
{
#ifdef AMIGA
  BPTR cle ;
#endif

  int recall, l ;
  char fkeys[5], *s, *delim ;
  int n, savn, insert, c1, c2 ;

#ifdef unix

  /* terminal-dependant initialization */

  if (! GetControlSeq()) return( (char *)-1 ) ;

  /* prepare switching to raw mode (cf Rifflet page 233) */

  ioctl( 0 , TCGETA , &etat1 ) ;
  memcpy( &etat2 , &etat1 , sizeof(struct termio) ) ;
  etat2.c_lflag &= ~(ICANON|IXON|ISIG|ECHO) ;
  etat2.c_cc[4]  = 1 ; /* = MIN dans doc termio */

#endif /* unix */

  width = GetWidth() ;
  if ( ! width ) return( (char *)-1 ) ;
  SetRawMode( TRUE ) ;

  /* compute prompt length */

#ifdef _AMIGA

  /* get all chars already waiting */

  cle = FILEtoBPTR( in->f_desc , FALSE ) ;
  while ( WaitForChar( cle , 10 ) )
  {
    n = Read( cle , tmp , 16 ) ;
    if ( n > 0 )
    {
      tmp[n] = '\0' ;
      strcat( aux , tmp ) ;
    }
  }

  /* get cursor position */

  plen	    = 0 ;
  tyahdptr  = typeahd ;
  *tyahdptr = '\0' ;

  printf( "\x9b\x36\x6e" ) ; /* CSI6n */
  fflush( stdout ) ;
  for ( s = line ; c1 = myget( in ) ; )
  {
    *s++ = c1 ;
    if ( c1 == 'R' ) break ;
  }
  *s = '\0' ;

  for ( s = line ; *s && (*s != ';') ; s++ ) ;
  if ( *s == ';' ) plen = atol( &s[1] ) - 1 ;
  if ( plen < 0 ) plen = 0 ;

#endif

#ifdef unix
  plen = strlen( prompt ) ;
#endif

  /* other initializations */

  tyahdptr = typeahd ;
  *NewKey  = '\0' ;
  *line    = '\0' ;
  strcpy( typeahd , aux ) ;
  max = pos = savn = n = 0 ;

  recall = HistNumber + 1 ;
  insert = IsSet( "insert" ) ;
  if (! (delim = myGetVar( "delim" ))) delim = " " ;
  if ( s = myGetVar( "keys" ) ) /* get control keys assignement */
  {
    strncpy( EditKeys , s , NBKEYS ) ;
    EditKeys[NBKEYS] = '\0' ;
  }

  for (;;)
  {
    /* wait for a key */

_wait:

    fflush( stdout ) ;
    CURSOR_ON ;
    c1 = myget( in ) ;
    CURSOR_OFF ;
    if ( c1 == EOF ) break ;

    /* add the new key to input buffer */

    for ( n = 0 ; NewKey[n] ; n++ ) ;
    NewKey[n++] = c1 ;
    NewKey[n] = '\0' ;

    while ( *NewKey )
    {
      /* test if input buffer matches a special key value */

      l = n ;
      for ( n = 0 ; n < SK_NBKEYS ; n++ )
	if ( (SpecialKeys[n] != NULL) &&
	     (! strncmp( NewKey , SpecialKeys[n] , l )) ) break ;

      if ( (n < SK_NBKEYS) && strcmp( NewKey , SpecialKeys[n] ) ) goto _wait ;

      /* execute the corresponding action */

      if ( (n < SK_NBKEYS) && (! strcmp( NewKey , SpecialKeys[n] )) )
      {
	*fkeys	= 'f' ;
	*NewKey = '\0' ;
_direct:
	switch ( n )
	{
	  case SK_UP	 : if ( recall <= (HistNumber - HistCount + 1) )
			   break ;
			   n = recall - 1 ;
	  case SK_DOWN	 : *line = '\0' ;
			   if ( n == SK_DOWN )
			   {
			     n = -1 ;
			     if ( recall <= HistNumber ) n = recall + 1 ;
			   }
			   if ( n > 0 )
			   {
			     recall = n ;
			     sprintf( tmp , "%ld" , recall ) ;
			     ExpandHist( tmp ) ;
			     strcpy( line , tmp ) ;
			   }
			   if ( pos ) SendCommand( CS_PREVC , pos ) ;
			   putchar( '\015' ) ;
			   SendCommand( CS_KEOD , 1 ) ;
			   printf( "%s%s" , prompt , line ) ;
			   pos = max = strlen( line ) ;
			   break ;
	  case SK_RIGHT  : if ( pos < max )
			   {
			     pos++ ;
			     SendCommand( CS_NEXTC , 1 ) ;
			   }
			   break ;
	  case SK_LEFT	 : if ( pos > 0 )
			   {
			     pos-- ;
			     SendCommand( CS_PREVC , 1 ) ;
			   }
			   break ;
	  case SK_SUP	 : strcpy( tmp , line ) ;
			   n = ExpandHist( tmp ) ;
			   if ( n < 0 )
			   {
			     *line = '\0' ;
			     recall = HistNumber + 1 ;
			   }
			   else
			   {
			     strcpy( line , tmp ) ;
			     recall = n ;
			   }
			   if ( pos ) SendCommand( CS_PREVC , pos ) ;
			   putchar( '\015' ) ;
			   SendCommand( CS_KEOD , 1 ) ;
			   printf( "%s%s" , prompt , line ) ;
			   pos = max = strlen( line ) ;
			   break ;
	  case SK_SRIGHT : while ((! strchr( delim , line[pos] )) && (pos < max))
			   {
			     pos++ ;
			     SendCommand( CS_NEXTC , 1 ) ;
			   }
			   while (strchr( delim , line[pos] ) && (pos < max))
			   {
			     pos++ ;
			     SendCommand( CS_NEXTC , 1 ) ;
			   }
			   break ;
	  case SK_SLEFT  : while ((pos > 0) && strchr( delim , line[pos-1] ))
			   {
			     pos-- ;
			     SendCommand( CS_PREVC , 1 ) ;
			   }
			   while ((pos > 0) && (! strchr( delim , line[pos-1] )))
			   {
			     pos-- ;
			     SendCommand( CS_PREVC , 1 ) ;
			   }
			   break ;
#ifdef _AMIGA
	  case SK_HELP	 : DoRequest( line ) ;
			   break ;
#endif
	  case SK_SF1	 :
	  case SK_SF2	 :
	  case SK_SF3	 :
	  case SK_SF4	 :
	  case SK_SF5	 :
	  case SK_SF6	 :
	  case SK_SF7	 :
	  case SK_SF8	 :
	  case SK_SF9	 :
	  case SK_SF10	 : *fkeys = 'F' ;
			   n -= SK_SF1 - SK_F1 ;
	  case SK_F1	 :
	  case SK_F2	 :
	  case SK_F3	 :
	  case SK_F4	 :
	  case SK_F5	 :
	  case SK_F6	 :
	  case SK_F7	 :
	  case SK_F8	 :
	  case SK_F9	 :
	  case SK_F10	 : sprintf( &fkeys[1] , "%d" , n - SK_F1 + 1 ) ;
			   if ( s = myGetVar( fkeys ) )
			   {
			     strcpy( typeahd , s ) ;
			     tyahdptr = typeahd ;
			   }
			   break ;
#ifdef _AMIGA
	  case SK_CLOSE  : goto _exit ;
#endif
	}
	continue ;
      }

      c1 = *NewKey ;
      if ( l > 1 ) strcpy( NewKey , &NewKey[1] ) ;
	      else *NewKey = '\0' ;

      /* test if match to a control key */

      if ( (c1 >= 1) && (c1 <= 26) )
      {
	c2 = c1 + 'a' - 1 ;
	for ( n = 0 ; EditKeys[n] ; n++ ) if ( EditKeys[n] == c2 ) break ;
	if ( ! EditKeys[n] ) goto _notmapped ;
	n++ ;

	switch ( n )
	{
	  case BEGINL :
	  case KILLL  : if ( pos > 0 ) SendCommand( CS_PREVC , pos ) ;
			pos = 0 ;
			if ( n == BEGINL ) break ;
	  case KENDL  : SendCommand( CS_KEOD , 1 ) ;
			max = pos ;
			line[pos] = '\0' ;
			break ;
	  case PREVC  : n = SK_LEFT ; goto _direct ;
	  case ENDL   : if ( pos != max ) SendCommand( CS_NEXTC , (max - pos) ) ;
			pos = max ;
			break ;
	  case NEXTC  : n = SK_RIGHT ; goto _direct ;
	  case NEXTL  : n = SK_DOWN  ; goto _direct ;
	  case PREVL  : n = SK_UP    ; goto _direct ;
	  case SWAPC  : if ( pos > 1 )
			{
			  c1 = line[pos-1] ;
			  line[pos-1] = line[pos-2] ;
			  line[pos-2] = c1 ;
			  SendCommand( CS_PREVC , 2 ) ;
			  printf( "%c%c" , c1 , line[pos-1] ) ;
			}
			break ;
	  case BEGINH : recall = HistNumber - HistCount + 1 ;
	  case ENDH   : if ( n == ENDH ) recall = HistNumber ;
			sprintf( line , "%ld" , recall ) ;
			ExpandHist( line ) ;
	  case RFSHL  : n =  pos / width ;
			if ( n ) SendCommand( CS_PREVL , n ) ;
			putchar( '\015' ) ;
			SendCommand( CS_KEOD , 1 ) ;
			printf( "%s%s" , prompt , line ) ;
			pos = max = strlen( line ) ;
			break ;
	  case KENDW  : for ( n = pos+1 ; (n < max) && (! strchr( delim , line[n] )) ; n++ ) ;
			if ( n < max )
			{
			  memmove( &line[pos] , &line[n] , (max - n + 1) ) ;
			  max -= n - pos ;
			  putchar( '\015' ) ;
			  SendCommand( CS_KEOD , 1 ) ;
			  printf( "%s%s" , prompt , line ) ;
			  SendCommand( CS_PREVC , (max - pos) ) ;
			}
			else
			{
			  SendCommand( CS_KEOD , 1 ) ;
			  max = pos ;
			  line[pos] = '\0' ;
			}
			break ;
	  case REPLV  : if ( pos > 0 ) DoReplace( line ) ;
			break ;
	  case LISTC  : c1 = 4 ; goto _notmapped ;
	  default     : goto _notmapped ;
	}

	continue ;
      }

      /* all other keys are handled here */

_notmapped:

      switch( c1 )
      {
	case 8	 : if ( pos > 0 )
		   {
		     pos-- ;
		     printf( "\010" ) ;
		   }
		   else break ;
	case 127 : if ( pos < max )
		   {
		     int dist, npos ;

		     memmove( &line[pos] , &line[pos+1] , (max - pos) ) ;
		     SendCommand( CS_DELC , 1 ) ;
		     max-- ;

		     n = 0 ;
		     dist = width - plen ;
		     for ( npos = 0 ; npos <= pos ; dist = width ) npos += dist ;
		     dist = npos - pos - 1 ;

		     while ( max >= npos )
		     {
		       if ( dist < 1 ) break ;

		       n += dist ;
		       SendCommand( CS_NEXTC , dist ) ;
		       putchar( line[npos-1] ) ;
		       SendCommand( CS_DELC , 1 ) ;
		       dist = width ;
		       npos += dist ;
		       n++ ; /* à cause du caractère line[npos] */
		     }

		     if ( n > 0 ) SendCommand( CS_PREVC , n ) ;
		   }
		   break ;
	case 4	 :
	case 9	 : GetCurWord( line , &n ) ;
		   if ( DoCompletion( tmp , (c1 == 9) , n ) )
		   {
		     tyahdptr = typeahd ;
		     if ( *tmp ) printf( "\n%s\n" , tmp ) ;
		     printf( "\015%s%s" , prompt , line ) ;
		     if ( pos < max ) SendCommand( CS_PREVC , max-pos ) ;
		   }
		   break ;
	case 28  : goto _exit ;
	case 27  : AddHist( line , max ) ;
		   max = pos = 0 ;
	case 10  :
	case 13  : line[max] = '\0' ;
		   if ( max > 0 ) SendCommand( CS_NEXTC , (max - pos) ) ;
		   putchar( '\n' ) ;
		   SetRawMode( FALSE ) ;
		   CURSOR_ON ;
		   fflush( stdout ) ;
		   strcpy( aux , tyahdptr ) ;
		   return( line ) ;
	default  : c1 &= 0x00ff ;			/* "normal" key */
		   if ( c1 == 9 ) c1 = 32 ;
		   if ((c1 > 31) & (pos < MAXLINE))
		   {
		     if ((pos < max) && (insert))
		     {
		       int dist, npos ;

		       memmove( &line[pos+1] , &line[pos] , (max - pos) ) ;
		       SendCommand( CS_INSC , 1 ) ;
		       putchar( c1 ) ;
		       max++ ;

		       n = 0 ;
		       dist = width - plen ;
		       for ( npos = 0 ; npos <= pos ; dist = width ) npos += dist ;
		       dist = npos - pos - 1 ;

		       while ( max > npos )
		       {
			 if ( dist > 0 )
			 {
			   n += dist ;
			   SendCommand( CS_NEXTC , dist ) ;
			 }
			 if ( dist >= 0 ) SendCommand( CS_INSC , 1 ) ;
			 putchar( line[npos] ) ;
			 dist = width ;
			 npos += dist ;
			 n++ ; /* à cause du caractère line[npos] */
		       }

		       if ( n > 0 ) SendCommand( CS_PREVC , n ) ;
		     }
		     else
		     {
		       if ((! pos) && (max == pos)) printf( "\015%s%s" , prompt , line ) ;
		       putchar( c1 ) ;
		     }
		     line[pos++] = c1 ;
		     if ( max < pos ) max = pos ;
		     line[max] = '\0' ;
		   }
		   break ;
      }
    }
  }

_exit:

  SetRawMode( FALSE ) ;
  CURSOR_ON ;
  fflush( stdout ) ;
  *aux = '\0' ;
  return( NULL ) ;
}
