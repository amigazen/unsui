/* arexx.c */

#include "defines.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <intuition/intuition.h>
#include <libraries/dos.h>
#include <libraries/reqbase.h>
#include <clib/dos_protos.h>
#include <pragmas/dos_pragmas.h>

#include "minrexx.h"
#include "globals.h"
#include "amscreen.h"
#include "gad_def.h"
#include "arexx.h"

#include "globals.i"
#include "amscreen.i"
#include "gadget.i"
#include "showdvi.i"
#include "config.i"
#include "new_font.i"
#include "am_requ.i"
#include "am_menu.i"

#include <clib/intuition_protos.h>
#include <clib/graphics_protos.h>

#include <pragmas/intuition_pragmas.h>
#include <pragmas/graphics_pragmas.h>


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




/* local functions */
static int  getnm		(char **where, int *err);
static int  parseargs		(char *p, int n, int args[]);
static void cutAnf		(char **p);
static void gettwostr		(char *p, char **s, char **t);

static long rxfirst		(struct RexxMsg *msg, char *p);
static long rxlast		(struct RexxMsg *msg, char *p);
static long rxprev		(struct RexxMsg *msg, char *p);
static long rxnext		(struct RexxMsg *msg, char *p);
static long rxgoto		(struct RexxMsg *msg, char *p);
static long rxtofront		(struct RexxMsg *msg, char *p);
static long rxtoback		(struct RexxMsg *msg, char *p);
static long rxwbtofront		(struct RexxMsg *msg, char *p);
static long rxfullpage		(struct RexxMsg *msg, char *p);
static long rxprintpage		(struct RexxMsg *msg, char *p);
static long rxsetdir		(struct RexxMsg *msg, char *p);
static long rxloadnew		(struct RexxMsg *msg, char *p);
static long rxloadagain		(struct RexxMsg *msg, char *p);
static long rxgetdir		(struct RexxMsg *msg, char *p);
static long rxgetfile		(struct RexxMsg *msg, char *p);
static long rxgetpage		(struct RexxMsg *msg, char *p);
static long rxgetnumofpages	(struct RexxMsg *msg, char *p);
static long rxtogglelace	(struct RexxMsg *msg, char *p);
static long rxtogglescrollbar	(struct RexxMsg *msg, char *p);
static long rxsetcolor		(struct RexxMsg *msg, char *p);
static long rxsetresolution	(struct RexxMsg *msg, char *p);
static long rxbeep		(struct RexxMsg *msg, char *p);
static long rxmessage		(struct RexxMsg *msg, char *p);
static long rxrefresh		(struct RexxMsg *msg, char *p);
static long rxscreen		(struct RexxMsg *msg, char *p);
static long rxspawn		(struct RexxMsg *msg, char *p);
static long rxversion		(struct RexxMsg *msg, char *p);
static long rxsaveconfig	(struct RexxMsg *msg, char *p);
static long rxexit		(struct RexxMsg *msg, char *p);

static long rxgetstring		(struct RexxMsg *msg, char *p);
static long rxgetnumber		(struct RexxMsg *msg, char *p);
static long rxokay1		(struct RexxMsg *msg, char *p);
static long rxokay2		(struct RexxMsg *msg, char *p);

static long rxmenu		(struct RexxMsg *msg, char *p);

static long rxactivate		(struct RexxMsg *msg, char *p);

static long rxgetpubscreenname	(struct RexxMsg *msg, char *p);


struct rexxCommandList rcl[] = {
   { "first", (APTR)&rxfirst },			/* go to first page		*/
   { "last", (APTR)&rxlast },			/* go to last page		*/
   { "prev", (APTR)&rxprev },			/* go to prev (arg1) page	*/
   { "next", (APTR)&rxnext },			/* go to next (arg1) page	*/
   { "goto", (APTR)&rxgoto },			/* go to arg1 page		*/

   { "tofront", (APTR)&rxtofront },		/* screen to front		*/
   { "toback", (APTR)&rxtoback },		/* screen to back		*/
   { "wbtofront", (APTR)&rxwbtofront },		/* workbench-screen to front	*/

   { "fullpage", (APTR)&rxfullpage },		/* show fullpage		*/
   { "printpage", (APTR)&rxprintpage },		/* print current page	     !! */
   { "setdir", (APTR)&rxsetdir },		/* set current directory	*/
   { "cd", (APTR)&rxsetdir },			/* set current directory	*/
   { "loadnew", (APTR)&rxloadnew },		/* load arg1 as new dvi-file	*/
   { "loadagain", (APTR)&rxloadagain },		/* load file again		*/
   { "getdir", (APTR)&rxgetdir },		/* get curreny directory	*/
   { "getcwd", (APTR)&rxgetdir },		/* get curreny directory	*/
   { "getfile", (APTR)&rxgetfile },		/* get current dvi-file		*/
   { "filename", (APTR)&rxgetfile },		/* get current dvi-file		*/
   { "getpage", (APTR)&rxgetpage },		/* get current page number	*/
   { "getnumofpages", (APTR)&rxgetnumofpages },	/* get total number of pages	*/
   { "pagenum", (APTR)&rxgetpage },		/* get current page number	*/
   { "togglelace", (APTR)&rxtogglelace },	/* toggle lace modus		*/
   { "togglescrollbar", (APTR)&rxtogglescrollbar },	/* toggle scroll bar	*/
   { "setcolor", (APTR)&rxsetcolor },		/* set color arg1 to arg2-4	*/
   { "setresolution", (APTR)&rxsetresolution },	/* set resolution to arg1	*/
   { "beep", (APTR)&rxbeep },			/* beep				*/
   { "message", (APTR)&rxmessage },		/* write a message		*/
   { "refresh", (APTR)&rxrefresh },		/* refresh screen		*/
   { "screen", (APTR)&rxscreen },		/* get screen address		*/
   { "spawn", (APTR)&rxspawn },			/* */
   { "version", (APTR)&rxversion },		/* print version string		*/
   { "saveconfig", (APTR)&rxsaveconfig },	/* saves configuration		*/
   { "exit", (APTR)&rxexit },			/* quit showdvi			*/
   
   { "getstring", (APTR)&rxgetstring },		/* REQ: get a string		*/
   { "getnumber", (APTR)&rxgetnumber },		/* REQ: get a number		*/
   { "okay1", (APTR)&rxokay1 },			/* REQ: one gadget requester	*/
   { "okay2", (APTR)&rxokay2 },			/* REQ: two gadget requester	*/
   
   { "menu", (APTR)&rxmenu },			/* address the ShowDVI menu	*/

   { "activate", (APTR)&rxactivate },		/* activate ShowDVI window	*/
   
   { "getpubscreenname", (APTR)&rxgetpubscreenname }, /* get the public screen name */

   { NULL, NULL } } ;




/*
 *   This function takes a pointer to a pointer to a string, grabs the
 *   next number, returns it, and advances the pointer to the string to
 *   point after the number.
 */
static int getnm(char **where, int *err)
{
   register char *p = *where ;
   register int val = 0 ;

   *err = 1;
   while (*p <= ' ' && *p)
      p++ ;

   while ('0' <= *p && *p <= '9') {
      val = 10 * val + *p++ - '0' ;
      *err = 0;
   }
   *where = p ;
   return(val) ;
}

/*
 *   This function trys to find `n' numeric arguments in the command
 *   string, and stuffs them into the args array.
 */
static int parseargs(char *p, int n, int args[])
{
   register int i ;
   int err;

   while (*p > ' ' && *p)
      p++ ;
   for (i=0; i<n; i++)
      args[i] = getnm(&p, &err) ;

   return err;
}


/* weg mit umschliessenden Anfuehrungszeichen... */
static void cutAnf(char **p)
{
  int len;

  if (*p != NULL) {
    len = strlen(*p);
    if (**p == '\"' && (*p)[len-1] == '\"' || **p == '\'' && (*p)[len-1] == '\'') {
      (*p)[len-1] = '\0';
      (*p)++;
    }
  }
}


static void gettwostr(char *p, char **s, char **t)
{
  int len;

#ifdef LATTICE
  while (*p <= ' ' && *p) p++ ;
  if (*p != '\0') {
    len = stcarg(p, " 	");
    if (len > 0) {
      *s = p;
      p += len;
      if (*p != '\0') {
        *p = '\0';
        p++;
      }
    }
    if (*p != '\0') {
      len = stcarg(p, " 	");
      if (len > 0) {
        *t = p;
        p += len;
        *p = '\0';
      }
    }
  }
#else
  while (*p <= ' ' && *p) p++ ;
  
  if (*p != '\0') {
    char *h, *k, c;

	/*     vvvvvv Fehler, aber ich hab grad keine Lust... */ 
    h = strcpy(buffer, p);	/* arbeiten wir auf einer Kopie... */
    *s = h;
    
    while (!isspace(*h)) {
      if (*h == '\"' || *h == '\'') {	/* skip Anf.zeichen */
        c = *h;
        k = h++;
        while (*h != c && *h != '\0') {
          *k++ = *h++;			/* verschiebe alles um 1 nach links */
        }
        if (*h == '\0') *k = '\0';
      }
    }
    if (*h != '\0') *h++ = '\0';	/* defstr beenden */
    
    while (*h <= ' ' && *h) h++;
      
    if (*h != '\0') {
      *t = h;
    
      while (!isspace(*h)) {
        if (*h == '\"' || *h == '\'') {	/* skip Anf.zeichen */
          c = *h;
          k = h++;
          while (*h != c && *h != '\0') {
            *k++ = *h++;			/* verschiebe alles um 1 nach links */
          }
          if (*h == '\0') *k = '\0';
        }
      }
    }
  }
#endif

  /* weg mit den Anfuehrungszeichen aussen herum... */
  cutAnf(s);
  cutAnf(t);

}




/*
 * OK
 */
static long rxfirst(struct RexxMsg *msg, char *p)
{
  if (is_dvif) {
    replyRexxCmd(msg, 0L, 0L, NULL);
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  return KOMM-2;
}

/*
 * OK
 */
static long rxlast(struct RexxMsg *msg, char *p)
{
  if (is_dvif) {
    replyRexxCmd(msg, 0L, 0L, NULL);
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  return KOMM+2;
}

/*
 * OK
 */
static long rxprev(struct RexxMsg *msg, char *p)
{
  long help, ret=0;
  int err, args[1];

  if (is_dvif) {
    err = parseargs(p, 1, args);
    if (err == 0) {
      help = args[0];
    }
    if (help == 0) {
      ret = KOMM-1;
    }
    else {
      ret = current_page - help;
    }
    static_y_Koo = wy;		/* ganz nach unten */

    replyRexxCmd(msg, 0L, 0L, NULL);
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }

  return ret;
}

/*
 * OK
 */
static long rxnext(struct RexxMsg *msg, char *p)
{
  long help, ret=0;
  int err, args[1];

  if (is_dvif) {
    err = parseargs(p, 1, args);
    if (err == 0) {
      help = args[0];
    }
    if (help == 0) {
      ret = KOMM+1;
    }
    else {
      ret = current_page + help;
    }
    static_y_Koo = 0;		/* ganz nach oben */

    replyRexxCmd(msg, 0L, 0L, NULL);
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }

  return ret;
}

/*
 * OK
 */
static long rxgoto(struct RexxMsg *msg, char *p)
{
  long help=0;
  int err, args[1];

  if (is_dvif) {
    err = parseargs(p, 1, args);
    if (err == 0) {
      help = args[0];
    }
    if (help == 0) help = 1;

    replyRexxCmd(msg, 0L, 0L, NULL);
    }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  return help;
}

/*
 * OK
 */
static long rxtofront(struct RexxMsg *msg, char *p)
{
  WindowToFront(win2);
  ScreenToFront(screen);		/* Screen her!! */
  /** make_show_active(); **/

  replyRexxCmd(msg, 0L, 0L, NULL);
  return 0;
}


/*
 * OK
 */
static long rxwbtofront(struct RexxMsg *msg, char *p)
{
  (void)WBenchToFront();		/* WB-Screen her!! */
  make_old_active();

  replyRexxCmd(msg, 0L, 0L, NULL);
  return 0;
}

/*
 * OK
 */
static long rxtoback(struct RexxMsg *msg, char *p)
{
  ScreenToBack(screen);			/* Screen weg!! */
  make_old_active();

  replyRexxCmd(msg, 0L, 0L, NULL);
  return 0;
}

/*
 * OK
 */
static long rxfullpage(struct RexxMsg *msg, char *p)
{
  if (is_dvif) {
    show_full_page(FALSE);
    replyRexxCmd(msg, 0L, 0L, NULL);
  }
  else {			/* noch kein DVI-File geladen */
    replyRexxCmd(msg, 5L, 0L, NULL);
  }

  return 0;
}

/*
 * OK
 */
static long rxprintpage(struct RexxMsg *msg, char *p)
{
  if (is_dvif) {
    printing();
    replyRexxCmd(msg, 0L, 0L, NULL);
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  return 0;
}

/*
 *
 */
static long rxsetdir(struct RexxMsg *msg, char *p)
{
  while (*p <= ' ' && *p) p++ ;

  if (!is_dir(p)) {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  else {
    strcpy(dirname, p);
    mk_correct_path(dirname);
    if (!is_dvif) strcpy(filename, dirname);
    replyRexxCmd(msg, 0L, 0L, NULL);
  }
  return 0;
}

/*
 *
 */
static long rxloadnew(struct RexxMsg *msg, char *p)
{
  long ret = 0;
  char *ptr;
  char answerarray[FCHARS+DSIZE+1];

  while (*p <= ' ' && *p) p++ ;

  answerarray[sizeof(answerarray)-1] = '\0';
  strncpy(answerarray, p, sizeof(answerarray)-2);

  ptr = strrchr(answerarray, '.');
  
  if (ptr && !stricmp(ptr, ".tex")) {
    strcpy(ptr, ".dvi");
  }

  if (access(answerarray, 4) != 0) {
    if (ptr == NULL || stricmp(ptr, ".dvi") != 0) {
      strncat(answerarray, ".dvi", sizeof(answerarray)-2);
    }
  }

  if (access(answerarray, 4) != 0) {
    Message(MSG_CANT_FOUND_FILE, answerarray);
    beep();
  }
  else {
    if (stricmp(filename, &(answerarray[0])) == 0) {
      OpenNewDVI(&(answerarray[0]), FALSE);
      ret = 3;		    /* neues File, Name *nicht* geaendert. */
    }
    else {
      strcpy(filename, &(answerarray[0]));
      OpenNewDVI(&(answerarray[0]), FALSE);
      ret = 4;		    /* neues File, Name geaendert. */
    }
  }

  if (ret == 3 || ret == 4) {
    replyRexxCmd(msg, 0L, 0L, NULL);
    ret += KOMM;
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }

  return ret;
}

/*
 * OK
 */
static long rxloadagain(struct RexxMsg *msg, char *p)
{
  long ret;

  if (is_print) {
    replyRexxCmd(msg, 1L, 0L, NULL);
    Message(MSG_NOT_WHILE_PRINTING);
    beep();
    ret = 0;
  }
  else {
    if (access(filename,4) == 0) {
      char *p = strrchr(filename,'.');
      if (p != NULL && strcmp(p,".dvi") == 0) {
        if (!is_notify && is_newer_dvi_file()) {	/* zum Updaten des Datums und damit nicht ein File */
          replyRexxCmd(msg, 0L, 0L, NULL);		/* 'again' geladen wird, das schon ueber notify neu geladen wurde */
          OpenNewDVI(filename, FALSE);
          ret = KOMM + 3;		/* neues File, Name nicht geaendert. */
        }
      }
      else {
        replyRexxCmd(msg, 5L, 0L, NULL);
        Message(MSG_NO_DVI_FILE);
        beep();
      }
    }
    else {			/* noch kein DVI-File geladen */
      replyRexxCmd(msg, 5L, 0L, NULL);
      Message(MSG_CANT_ACCESS_FILE, filename);
      beep();
    }
  }

  return ret;
}

/*
 *
 */
static long rxgetdir(struct RexxMsg *msg, char *p)
{
  char * str;

  strcpy(m_string, filename);

  if (!is_dir(filename)) {
    if (!strchr(m_string, '/') && !strchr(m_string, ':')) {
      // kein Pfad Teil
      getdir("", m_string);
    }
    else {
      str = PathPart(m_string);
      if (str != m_string) *str = '\0';
    }
  }

  mk_correct_path(m_string);
  
  replyRexxCmd(msg, 0L, 0L, m_string);

  return 0;
}

/*
 *
 */
static long rxgetfile(struct RexxMsg *msg, char *p)
{
  char *str;

  m_string[0] = 0;	// vorinitialisieren

  if (is_dvif) {
    str = FilePart(filename);
    if (str) {
      char * str2;

      strcpy(m_string, str);
      str2 = strrchr(m_string, '.');	// .dvi soll weg!
      if (str2 && !stricmp(str2, ".dvi")) *str2 = '\0';
    }
    replyRexxCmd(msg, 0L, 0L, m_string);
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }


  return 0;
}

/*
 *
 */
static long rxgetpage(struct RexxMsg *msg, char *p)
{
  if (is_dvif) {
    sprintf(m_string, "%ld", current_page);
    replyRexxCmd(msg, 0L, 0L, m_string);
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  return 0;
}


/*
 *
 */
static long rxgetnumofpages(struct RexxMsg *msg, char *p)
{
  if (is_dvif) {
    sprintf(m_string, "%ld", max_page_number);
    replyRexxCmd(msg, 0L, 0L, m_string);
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  return 0;
}


/*
 * OK
 */
static long rxtogglelace(struct RexxMsg *msg, char *p)
{
  change_resolution();
  replyRexxCmd(msg, 0L, 0L, NULL);
  return 0;
}

/*
 * OK
 */
static long rxtogglescrollbar(struct RexxMsg *msg, char *p)
{
  if (is_show) {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  else {
    toggle_scrollbar(FALSE);
    replyRexxCmd(msg, 0L, 0L, NULL);
  }
  return 0;
}

/*
 * OK
 */
static long rxsetcolor(struct RexxMsg *msg, char *p)
{
  int err, args[4];

  err = parseargs(p, 4, args);
  if (err == 0) {
    if (args[1] > 15 || args[2] > 15 || args[3] > 15) {
      replyRexxCmd(msg, 5L, 0L, NULL);
    }
    else {
      if (args[0] >= 0 && args[0] <= 3) {
        switch (args[0]) {
            case 0:
		current_col.red_0   = args[1];
		current_col.green_0 = args[2];
		current_col.blue_0  = args[3];
		break;
            case 1:
		current_col.red_1   = args[1];
		current_col.green_1 = args[2];
		current_col.blue_1  = args[3];
		break;
            case 2:
		current_col.red_2   = args[1];
		current_col.green_2 = args[2];
		current_col.blue_2  = args[3];
		break;
            case 3:
		current_col.red_3   = args[1];
		current_col.green_3 = args[2];
		current_col.blue_3  = args[3];
		break;
        }
        replyRexxCmd(msg, 0L, 0L, NULL);
      }
      else {
        replyRexxCmd(msg, 5L, 0L, NULL);
      }
    }
  }
  return 0;
}

/*
 *
 */
static long rxsetresolution(struct RexxMsg *msg, char *p)
{
  int err, args[1];
  long ret = 0;

  err = parseargs(p, 1, args);
  if (err == 0) {
    resolution = args[0];
    hconvresolution = resolution;
    vconvresolution = resolution;
    OpenNewDVI(filename, TRUE);
    set_chres;
    ret = KOMM+3L;	    /* neues File, Name nicht geaendert. */
    replyRexxCmd(msg, 0L, 0L, NULL);
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  return ret;
}

/*
 * OK
 */
static long rxbeep(struct RexxMsg *msg, char *p)
{
  beep();
  replyRexxCmd(msg, 0L, 0L, NULL);
  return 0;
}

/*
 * OK
 */
static long rxmessage(struct RexxMsg *msg, char *p)
{
  strcpy(m_string, p+1);
  replyRexxCmd(msg, 0L, 0L, NULL);
  MessageStr(m_string);
  return 0;
}

/*
 *
 */
static long rxrefresh(struct RexxMsg *msg, char *p)
{
  replyRexxCmd(msg, 0L, 0L, NULL);
  refresh_screen();
  window_show();
  return 0;
}

/*
 * OK
 */
static long rxspawn(struct RexxMsg *msg, char *p)
{
  while (*p <= ' ' && *p)
      p++ ;
  asyncRexxCmd(p);
  replyRexxCmd(msg, 0L, 0L, NULL);
  return 0;
}

/*
 * OK
 */
static long rxscreen(struct RexxMsg *msg, char *p)
{
  sprintf(m_string,"0x%08x",screen);
  replyRexxCmd(msg, 0L, 0L, m_string);
  return 0;
}

/*
 * OK
 */
static long rxversion(struct RexxMsg *msg, char *p)
{
  replyRexxCmd(msg, 0L, 0L, GetCopy());
  return 0;
}

/*
 * OK
 */
static long rxsaveconfig(struct RexxMsg *msg, char *p)
{
  write_config_file();
  replyRexxCmd(msg, 0L, 0L, NULL);
  return 0;
}

/*
 * OK
 */
static long rxexit(struct RexxMsg *msg, char *p)
{
  if (can_i_exit()) {
    Enable_Abort = 0;		/* am Ende nicht noch ein ^C */
    replyRexxCmd(msg, 0L, 0L, NULL);
    return KOMM+10;		/* QUIT */
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
    return 0;
  }
}




/*
 *
static long rx(struct RexxMsg *msg, char *p)
{
}
 */

static long rxgetstring(struct RexxMsg *msg, char *p)
{
  char *deftitle = GetTeXString(MSG_ENTER_A_STRING);
  char *s, *t;
  
  s = t = NULL;
  gettwostr(p, &s, &t);
  
  if (s != NULL) {
    strcpy(m_string, s);
  }
  else {
    m_string[0] = '\0';
  }

  if (t != NULL) {
    deftitle = t;
  }

  if (MyGetString(deftitle, m_string)) {
    replyRexxCmd(msg, 0L, 0L, m_string);
    return 0;
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  
  return 0;
}

static long rxgetnumber(struct RexxMsg *msg, char *p)
{
  long defnumber;
  long res = 0;
  char *deftitle = GetTeXString(MSG_ENTER_A_NUMBER);
  char *s, *t;
  

  s = t = NULL;
  gettwostr(p, &s, &t);
  
  if (s != NULL) {
    if (sscanf(s, "%ld", &defnumber) != 1) {
      defnumber = -12321;	/* eigentlich sollte das nun 'kein default' sein.. */
    }
  }
  else {
    defnumber = -12321;		/* eigentlich sollte das nun 'kein default' sein.. */
  }

  if (t != NULL) {
    deftitle = t;
  }

  if (MyGetLong(deftitle, -100000, 100000, defnumber, &res)) {
    sprintf(m_string, "%ld", res);
    replyRexxCmd(msg, 0L, 0L, m_string);
    return 0;
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  
  return 0;
}


static long rxokay1(struct RexxMsg *msg, char *p)
{
  cutAnf(&p);
  Okay1(p);
  replyRexxCmd(msg, 0L, 0L, NULL);
  return 0;
}


static long rxokay2(struct RexxMsg *msg, char *p)
{
  cutAnf(&p);
  if (Okay2(p)) {
    replyRexxCmd(msg, 0L, 0L, "1");
  }
  else {
    replyRexxCmd(msg, 0L, 0L, "0");
  }
  return 0;
}

static long rxmenu(struct RexxMsg *msg, char *p)
{
  long menu, item, subitem;
  long ret = 0;
  
  menu = NOMENU;
  item = NOITEM;
  subitem = NOSUB;

  if (sscanf(p,"%ld %ld %ld", &menu, &item, &subitem) > 0) {
    replyRexxCmd(msg, 0L, 0L, NULL);	/* das ist etwas frueh, sonst kann es aber passieren, */
					/* das eine Message *nie* beantwortet wird!! */
    ret = work_with_os_menu((UBYTE)menu, (UBYTE)item, (UBYTE)subitem);
    if (ret == KOMM + 5) {
      ret = 0;	/* special hack wg. work_with_os-Menu... :( */
    }
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  
  return ret;
}


static long rxactivate(struct RexxMsg *msg, char *p)
{
  make_show_active();
  replyRexxCmd(msg, 0L, 0L, NULL);
  return 0L;
}

static long rxgetpubscreenname(struct RexxMsg *msg, char *p)
{
  if (is_pubscr) {
    replyRexxCmd(msg, 0L, 0L, GetCurrentPubScr());
  }
  else {
    replyRexxCmd(msg, 5L, 0L, NULL);
  }
  
  return 0;
}
