/*
 *   This software is Copyright 1988 by Radical Eye Software.
 */
#include "dvips.h"
#ifdef AMIGA

#include <stdlib.h>
#include "makefont_protos.h"
#include "dvips_protos.h"
#include "search_protos.h"
#include "rexx_protos.h"

extern char *pkpath;
extern int actualdpi;
extern int vactualdpi;

enum {PKEXPAND_FULL, PKEXPAND_ONE, PKEXPAND_TWO, PKEXPAND_PKPART};

struct amipk_info {
	char *path;	/* font path */
	char *name;	/* font name */
	char *mode;	/* METAFONT mode */
	int dpi;
	int xdpi;	/* horizontal base resolution */
	int ydpi;	/* vertical base resolution */
};

static char *last_pkpath = NULL;
static char *expand_pkpath(struct amipk_info *, int);
static int magstep(register int, register int);

#endif /* AMIGA */
extern int quiet ;
extern int filter ;
extern int dontmakefont ;
#ifndef AMIGA
extern int system() ;
extern char *getenv(), *newstring() ;
#endif
extern Boolean secure ;
extern char *mfmode ;
#ifdef OS2
#include <stdlib.h>
#endif
#if defined MSDOS || defined OS2
extern char *mfjobname ;
extern FILE *mfjobfile ;
extern char *pkpath ;
extern int actualdpi ;
extern int vactualdpi ;
/*
 *  Write mfjob file
 */
void
mfjobout(font,mag)
char *font;
double mag;
{
   if (mfjobfile == (FILE *)NULL) {
      char pkout[128];
      char *p;
      int i;
      for (p=pkpath, i=0; *p && *p!=PATHSEP && i<127; p++) {
         if (*p=='%') {
            p++;
            switch(*p) { /* convert %x codes to mfjob @y codes */
               case 'b':
                  sprintf(pkout+i,"%d",actualdpi);
                  break;
               case 'd':
                  strcpy(pkout+i,"@Rr");
                  break;
               case 'f':
                  strcpy(pkout+i,"@f");
                  break;
               case 'p':
                  strcpy(pkout+i,"pk");
                  break;
               case 'm':
                  strcpy(pkout+i, mfmode ? mfmode : "default");
                  break;
               case '%':
                  strcpy(pkout+i,"%");
                  break;
               default:
                  sprintf(pkout+i, "%%%c", *p) ;
                  fprintf(stderr,"Unknown option %%%c in pk path\n",*p);
            }
            i += strlen(pkout+i);
         }
         else
           pkout[i++] = *p;
      }
      /* *p='\0'; Could some DOS person explain to me what this does? */
      pkout[i] = 0 ;
      mfjobfile =  fopen(mfjobname,"w");
      if (mfjobfile == (FILE *)NULL)
         return;
      fprintf(mfjobfile,"input[dvidrv];\n{\ndriver=dvips;\n");
      if (actualdpi == vactualdpi)
         fprintf(mfjobfile,"mode=%s[%d];\n",mfmode,actualdpi);
      else
         fprintf(mfjobfile,"mode=%s[%d %d];\n",mfmode,actualdpi,vactualdpi);
      fprintf(mfjobfile,"output=pk[%s];\n",pkout);
   }
   fprintf(mfjobfile,"{font=%s; mag=%f;}\n",font,mag);
   (void)fprintf(stderr,
        "Appending {font=%s; mag=%f;} to %s\n",font,mag,mfjobname) ;
}
#endif
/*
 *   Calculate magstep values.
 */
static int
magstep(n, bdpi)
register int n, bdpi ;
{
   register float t ;
   int neg = 0 ;

   if (n < 0) {
      neg = 1 ;
      n = -n ;
   }
   if (n & 1) {
      n &= ~1 ;
      t = 1.095445115 ;
   } else
      t = 1.0 ;
   while (n > 8) {
      n -= 8 ;
      t = t * 2.0736 ;
   }
   while (n > 0) {
      n -= 2 ;
      t = t * 1.2 ;
   }
   if (neg)
      return((int)(0.5 + bdpi / t)) ;
   else
      return((int)(0.5 + bdpi * t)) ;
}
#ifdef MAKEPKCMD
static char *defcommand = MAKEPKCMD " %n %d %b %m" ;
#else
#ifdef OS2
static char *doscommand = "command /c MakeTeXP %n %d %b %m" ;
static char *os2command = "MakeTeXP %n %d %b %m" ;
#define defcommand ( _osmode==OS2_MODE ? os2command : doscommand )
#else
#ifdef MSDOS
static char *defcommand = "command /c MakeTeXP %n %d %b %m" ;
#else
#ifdef VMCMS
static char *defcommand = "EXEC MakeTeXPK %n %d %b %m" ;
#else
#ifdef ATARIST
static char *defcommand = "maketexp %n %d %b %m" ;
#else
static char *defcommand = "MakeTeXPK %n %d %b %m" ;
#endif
#endif
#endif
#endif
#endif
char *command = 0 ;
/*
 *   This routine tries to create a font by executing a command, and
 *   then opening the font again if possible.
 */
#ifdef AMIGA
static char buf[256], amibuf[256] ;
#else
static char buf[125] ;
#endif
void
makefont(name, dpi, bdpi)
   char *name ;
   int dpi, bdpi ;
{
   register char *p, *q ;
   register int m, n ;
   int modegiven = 0 ;
#if defined MSDOS || defined OS2 || defined(ATARIST)
   double t;
#endif

#ifdef AMIGA
#define CALLMFNAME "CALLMF"
   Boolean amigamaketexpk = 0; /* use of Amiga MakeTeXPK features ? */
   Boolean arexx_callmf = 0;   /* call the arexx script specified by the env var CALLMF ? */
   struct amipk_info amipk;

   amipk.path = pkpath;
   amipk.name = name;
   amipk.mode = mfmode;
   amipk.dpi  = dpi;
   amipk.xdpi = bdpi;
   amipk.ydpi = vactualdpi;

   command = 0;
   if (secure == 0 && (command=getenv("MAKETEXPK")))
      command = newstring(command);
   else
   {
      if (command = getenv(CALLMFNAME)) /* try to get the env var CALLMF */
         arexx_callmf = 1;
      else
         command = defcommand; /* default command */
   }

   if (!stricmp(command,CALLMFNAME))
   {
      if (command = getenv(CALLMFNAME))
         arexx_callmf = 1;
      else
         command = defcommand;
   }         

#else
   if (command == 0)
      if (secure == 0 && (command=getenv("MAKETEXPK")))
         command = newstring(command) ;
      else 
         command = defcommand ;
#endif
#ifdef AMIGA
   if (!arexx_callmf) {
#endif /* AMIGA */
   for (p=command, q=buf; *p; p++)
      if (*p != '%')
         *q++ = *p ;
      else {
         switch (*++p) {
case 'n' : case 'N' :
            (void)strcpy(q, name) ;
            break ;
case 'd' : case 'D' :
            (void)sprintf(q, "%d", dpi) ;
            break ;
case 'b' : case 'B' :
            (void)sprintf(q, "%d", bdpi) ;
            break ;
case 'o' : case 'O' :
            (void)sprintf(q, "%s", mfmode ? mfmode : "default") ;
            modegiven = 1 ;
            break ;
case 'm' : case 'M' :
/*
 *   Here we want to return a string.  If we can find some integer
 *   m such that floor(0.5 + bdpi * 1.2 ^ (m/2)) = dpi, we write out
 *      magstep(m/2)
 *   where m/2 is a decimal number; else we write out
 *      dpi/bdpi
 *   We do this for the very slight improvement in accuracy that
 *   magstep() gives us over the rounded dpi/bdpi.
 */
            m = 0 ;
            if (dpi < bdpi) {
               while (1) {
                  m-- ;
                  n = magstep(m, bdpi) ;
                  if (n == dpi)
                     break ;
                  if (n < dpi || m < -40) {
                     m = 9999 ;
                     break ;
                  }
               }
            } else if (dpi > bdpi) {
               while (1) {
                  m++ ;
                  n = magstep(m, bdpi) ;
                  if (n == dpi)
                     break ;
                  if (n > dpi || m > 40) {
                     m = 9999 ;
                     break ;
                  }
               }
            }
#if defined MSDOS || defined OS2
/* write out magnification as decimal number */
            if (m == 9999) {
               t = (double)dpi/bdpi;
            } else {
               if (m < 0)
                    n = -m;
               else
                    n = m;
               if (n & 1) {
                    n &= ~1 ;
                    t = 1.095445115 ;
               } else
                    t = 1.0 ;
               while (n > 0) {
                    n -= 2 ;
                    t = t * 1.2 ;
               }
               if (m < 0)
                    t = 1 / t ;
            }
            (void)sprintf(q, "%12.9f", t) ;
#else
#ifndef ATARIST
            if (m == 9999) {
#else
            {
#endif
               (void)sprintf(q, "%d+%d/%d", dpi/bdpi, dpi%bdpi, bdpi) ;
            } else if (m >= 0) {
#ifdef AMIGA
               (void)sprintf(q, "magstep(%d.%d)", m/2, (m&1)*5) ;
            } else {
               (void)sprintf(q, "magstep(-%d.%d)", (-m)/2, (m&1)*5) ;
#else
               (void)sprintf(q, "magstep\\(%d.%d\\)", m/2, (m&1)*5) ;
            } else {
               (void)sprintf(q, "magstep\\(-%d.%d\\)", (-m)/2, (m&1)*5) ;
#endif /* AMIGA */
            }
#endif
            break ;
#ifdef AMIGA
case 'p':
            amigamaketexpk = 1;
            (void) expand_pkpath(&amipk, PKEXPAND_ONE);
            (void) strcpy(q, amibuf);
            break;

case 'P':
            amigamaketexpk = 1;
            (void) expand_pkpath(&amipk, PKEXPAND_TWO);
            (void) strcpy(q, amibuf);
            break;

case 'x' : case 'X' :
	    amigamaketexpk = 1;
            (void)sprintf(q, "%d", actualdpi) ;
            break ;

case 'y' : case 'Y' :
	    amigamaketexpk = 1;
            (void)sprintf(q, "%d", vactualdpi) ;
            break ;

#endif /* AMIGA */
case 0 :    *q = 0 ;
            break ;
default:    *q++ = *p ;
            *q = 0 ;
            break ;
         }
         q += strlen(q) ;
      }
#ifdef AMIGA
      *q = 0;
      if (mfmode && !modegiven && !amigamaketexpk)
      {
	      strcpy(q, " ");
	      strcat(q, mfmode);
      }
   }
   else
   {
      long result = 20;

      sprintf(buf, "'%s %s %d %d %d dvips %d/%s.%dpk %s'", command,
         name, dpi, bdpi, vactualdpi, dpi, name, dpi,
         expand_pkpath(&amipk, PKEXPAND_PKPART));

      if (!quiet)
         (void)fprintf(stderr, "- %s\n", buf);

      (void)call_rexx(buf, &result);
      return;
   }

#else
   *q = 0 ;
   if (mfmode && !modegiven) {
      strcpy(q, " ") ;
      strcat(q, mfmode) ;
   }
#endif /* AMIGA */
#ifdef OS2
   if ((_osmode == OS2_MODE) && filter)
      (void)strcat(buf, quiet ? " >nul" : " 1>&2") ;
#else
#ifndef VMCMS   /* no filters and no need to print to stderr */
#ifndef MVSXA
#ifndef MSDOS
#ifndef ATARIST
   if (filter)
#ifdef AMIGA
      (void)strcat(buf, quiet ? " >NIL:" : "") ;
#else
      (void)strcat(buf, quiet ? " >/dev/null" : " 1>&2") ;
#endif /* AMIGA */
#endif
#endif
#endif
#endif
#endif

#if defined MSDOS || defined OS2
   if (! quiet && mfjobname == (char *)NULL)
      (void)fprintf(stderr, "- %s\n", buf) ;
   if (dontmakefont == 0) {
      if (mfjobname != (char *)NULL)
         mfjobout(name,t);
      else
         (void)system(buf) ;
   }
#else
   if (! quiet)
      (void)fprintf(stderr, "- %s\n", buf) ;
   if (dontmakefont == 0)
      (void)system(buf) ;
#endif
   else {
      static FILE *fontlog = 0 ;

      if (fontlog == 0) {
         fontlog = fopen("missfont.log", "a") ;
         if (fontlog != 0) {
            (void)fprintf(stderr,
#ifndef VMCMS
                  "Appending font creation commands to missfont.log\n") ;
#else
  "\nMissing font data will be passed to DVIPS EXEC via MISSFONT LOG\n");
#endif
         }
      }
      if (fontlog != 0) {
         (void)fprintf(fontlog, "%s\n", buf) ;
         (void)fflush(fontlog) ;
      }
   }
}

#ifdef AMIGA
static char *expand_pkpath(struct amipk_info *amipk, int action)
{
   char *p = amipk->path; /* index into path */
   char *s = amibuf;      /* index into amibuf */
   char *t = NULL;        /* last path position before font name */
   char *k = s + 256;     /* bottom of buffer */
   Boolean subs = 0;      /* true if a substitution was performed */

   while (*p != PATHSEP && *p && s < k)
   {
      if (*p == '%')
      {
         subs = 1;
         switch (*++p)
         {
            case 'b': case 'x':
               (void)sprintf(s, "%d", amipk->xdpi);
               break;

            case 'y':
               (void)sprintf(s, "%d", amipk->ydpi);
               break;

            case 'd':
               (void)sprintf(s, "%d", amipk->dpi);
               break;

            case 'f': case 's':
               (void)strcpy(s, amipk->name);
               t = s; /* save name position */
               break;

            case 'm':
               if (amipk->mode == 0)
               {
                  if (actualdpi == 180)
                     amipk->mode = "nec";
                  else if (actualdpi == 300)
                     amipk->mode = "cx";
                  else if (actualdpi == 360)
                     amipk->mode = "nechi";
                  else if (actualdpi == 400)
                     amipk->mode = "nexthi";
                  else if (actualdpi == 600)
                     amipk->mode = "ljfour";
                  else if (actualdpi == 635)
                     amipk->mode = "linoone";
                  else if (actualdpi == 1270)
                     amipk->mode = "linohi";
                  else if (actualdpi == 2540)
                     amipk->mode = "linotzzh";
               }
               (void)strcpy(s, amipk->mode);
               break ;

            case 'p':
               (void)strcpy(s, "pk");
               break ;

            case '%':
               (void)strcpy(s, "%");
               break;

            default:
               error("! bad format character in pk path");
         }
         s = amibuf + strlen(amibuf);
         if (*p) p++;
      }
      else *s++ = *p++; /* copy the path string */
   }
   *s = '\0'; /* terminate the string */

   if (subs)
   {
      int l;

      if (action == PKEXPAND_PKPART)
      {
         static char filepart[80];
         l = strlen(amibuf);

         sprintf(filepart,"%d/%.32s.%dpk", amipk->dpi, amipk->name, amipk->dpi);
         if ((s = strstr(amibuf,filepart)) != NULL)
            *s = '\0';
      }
      else
      {
         if (action == PKEXPAND_ONE || action == PKEXPAND_TWO)
         {
            *t = '\0'; /* remove font name from pk path */
            l = strlen(amibuf);

            if (l > 0 && amibuf[l-1] == DIRSEP)
               amibuf[l-1] = '\0'; /* remove trailing DIRSEP */

            if (action == PKEXPAND_TWO)
            {
               if ((s = strrchr(amibuf, DIRSEP)) != NULL)
                  *s = '\0';
            }
         }
      }
   }

   return(amibuf);
}
#endif
