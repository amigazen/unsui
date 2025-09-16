/*--------------------------------------------------------------------------*
              __    __
             / /   /_/                      $RCSfile: ti.c,v $
            / /_  __                        $Release$
           / __/ / /                        $Revision: 1.5 $
          / /__ / /_                        $Date: 93/07/25 04:22:26 $
         /____//___/                        $Author: tf $
                                            $State: Exp $

           (c) Copyright 1992 Tobias Ferber, All Rights Reserved.

 *--------------------------------------------------------------------------*/

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef AMIGA
#include <exec/types.h>
#endif

static char rcs_id[]= "$VER: $Id: ti.c,v 1.5 93/07/25 04:22:26 tf Exp $";
#define BANNER &rcs_id[6]

#ifdef AMIGA
#define crsr_on  "\233\40\160\r"
#define crsr_off "\233\60\40\160\r"
#else
#define crsr_on  ""
#define crsr_off ""
#endif /* AMIGA */

/* word seperators */

#define isws(c) ((c)==' ' || (c)=='\t' || (c)=='\n')

FILE *fp; /* global (for _abort) */

#ifdef _DCC
void _abort(void)
{ if(fp && fp!=stdin) fclose(fp);
  printf("^C\n**BREAK\n\n%s",crsr_on);
  exit(1);
}
#endif

#ifdef GNUDOS
#define stricmp(s,t) strcmp(strupr(s),strupr(t))
#endif

#include <stdarg.h>

char *whoami; /* global copy of argv[0] */

void fatal(char *fmt, ...)
/* gibt 'whoami: <Fehlermeldung>' "uber 'stderr' aus */
{
  va_list argp;
  va_start(argp,fmt);
  fprintf(stderr,"%s: ",whoami);
  vfprintf(stderr,fmt,argp);
  fprintf(stderr,"\n");
  fflush(stderr);
  va_end(argp);
}

char *howtouse[] = {

 "CHECKWIDTH", "-w", "<width>", "inform about lines longer than <width> characters",
 "TABSIZE",    "-t", "<size> ", "set tabsize to <size> (default: 8)",
 "BRIEF",      "-b", "       ", "stop after the first line of output",
 "FIND",       "-c", "<value>", "find characters with an ASCII value of <value>",
 "PRINT",      "-p", "<n> [s]", "type out every <n> th line starting with line [s]",
 0L,0L,0L,0L
};

extern void ti();

main(int argc, char *argv[])
{
  int badopt= 0,   /* bad option ? */
      verbose= 1;  /* display statistics */

  char *fname= (char *)0L; /* filename */

  long ts=8,    /* tab size */
       chw=0,   /* width to check */
       asc=-1,  /* char to find */
       p=0,     /* first line to print, print line counter */
       n=0;     /* print every n th line */

#ifdef _DCC
  onbreak(_abort);
#endif

  whoami= argv[0];

  if(argc>1)
  {

#if !defined(unix) && !defined(__MSDOS__)
    expand_args(argc, argv, &argc, &argv);
#endif /* unix || __MSDOS__ */

    --argc;
    ++argv;
    while(argc>0 && !badopt)
    { char *arg=argv[0];
      if(isalpha(*arg)) /* convert options: AmigaDOS -> UNIX */
      { char **aopt= &howtouse[0];
        while(*aopt && stricmp(arg,*aopt))
          aopt= &aopt[4];
        if(*aopt) arg= aopt[1];
      }
      if(*arg=='-')
      { arg++;
        switch(*arg)
        {
/* -w */  case 'w': case 'W': /* CHECKWIDTH */
            if(arg[1]) ++arg;
            else arg= (--argc > 0) ? *(++argv) : (char *)0L;
            if(arg && *arg)
            { if((chw= atol(arg)) < 1)
              { fatal("Illegal width: %d (option '-w%s').",chw,arg);
                badopt++;
              }
            }
            else
            { fatal("Missing width value after '-w' option");
              badopt++;
            }
            break;
/* -t */  case 't': case 'T': /* TABSIZE */
            if(arg[1]) ++arg;
            else arg= (--argc > 0) ? *(++argv) : (char *)0L;
            if(arg && *arg)
            { if((ts= atol(arg)) < 1)
              { fatal("Illegal TAB size: %d (option '-t%s').",ts,arg);
                badopt++;
              }
            }
            else
            { fatal("Missing TAB size after '-t' option");
              badopt++;
            }
            break;
/* -p */  case 'p': case 'P': /* PRINT */
            if(arg[1]) ++arg;
            else arg= (--argc > 0) ? *(++argv) : (char *)0L;
            if(arg && *arg)
            { if((n= atol(arg)) < 1)
              { fatal("Error: n=%d makes no sense in '-p <n> [s]'",n);
                badopt++;
              }
              else
              { arg= *(++argv);
                if(arg && *arg && --argc > 0)
                { if((p= atol(arg)) < 1)
                  { fatal("Error: s=%d makes no sense in '-p <n> [s]'",p);
                    badopt++;
                  }
                }
                else p=1; /* default: start with line 1 */
              }
            }
            else
            { fatal("Missing <n> (and [s]) after '-p' option");
              badopt++;
            }
            break;
/* -c */  case 'c': case 'C': /* FIND */
            if(arg[1]) ++arg;
            else arg= (--argc > 0) ? *(++argv) : (char *)0L;
            if(arg && *arg)
            { asc= atol(arg);
              if(asc < 0 || 255 < asc)
              { fatal("ASCII value out of range in -c%d",asc);
                badopt++;
              }
            }
            else
            { fatal("Missing ASCII value after '-c' option");
              badopt++;
            }
            break;
/* -h */  case 'h': case 'H': /* HELP */
             { char **s= &howtouse[0];

                printf("%s\n"
                  "(c)Copyright 1992 by Tobias Ferber, All Rights Reserved\n"
                  "TI <textfile> [-w] [-t] [-q] [-b] [-c] [-p]\n",BANNER);

                while(*s)
                { printf("%-10s or %-2s %s  %s\n",s[0],s[1],s[2],s[3]);
                  s= &s[4];
                }
                exit(0);
              }
              break;
/* -b */  case 'b': case 'B': /* BRIEF */
            verbose= 0;
            break;
/* -  */  case '\0':
             if(fname) ti(fname, verbose, ts, chw, asc, p, n);
             fname= *argv;
             break;
/* ?? */  default:
            fatal("Bad option: -%c.",*arg);
            badopt++;
            break;
        }
      }
      else
      { if(fname) ti(fname, verbose, ts, chw, asc, p, n);
        fname= arg;
      }
      --argc;
      ++argv;
    }
  }
  if(!badopt) ti(fname, verbose, ts, chw, asc, p, n);
  exit(0);
}

/*** / TI / ***/

void ti(char *fname,
        int verbose, /* display statistics? */
        long ts,     /* tab size */
        long chw,    /* width to check */
        long asc,    /* char to find */
        long p,      /* first line to print, print line counter */
        long n)      /* print every n th line */
{
  long b=0,  /* byte counter */
       l=0,  /* lines counter */
       w=0,  /* words counter */
       cl=0, /* chars per line counter */
       cm=0; /* max #of chars per line */

  int goon= 1; /* don't stop now */

  char c,d='\0';

  if(!fname || (fname && *fname=='-'))
  { fp=stdin;
    fname= "stdin";
  }
  else if(!(fp= fopen(fname,"r")))
  { printf("%s: No information for \"%s\": object not found\n",whoami,fname);
    exit(5);
  }
  printf(crsr_off);

  for(;(c=fgetc(fp))!=EOF && !feof(fp) && goon;++b)
  {
    if(p==1) putchar(c);
    if(asc>=0 && c==(char)asc)
    { printf("%s: Line %ld \"%s\" contains character 0x%02x\n",whoami,l,fname,c);
      goon= verbose;
    }
    if(c=='\n'||c=='\f')
    { ++l;
      ++w;
      if(cl>cm) cm=cl;
      if(chw>0 && cl>chw)
      { printf("%s: Line %ld \"%s\" width %d > %ld\n",whoami,l,fname,cl,chw);
        goon= verbose;
      }
      cl=0L;

      --p;
      if(p==0) p=n;
    }
    else
    { if(isws(c) && !isws(d)) ++w;
      if(c=='\t' && ts>1) cl= cl+ts-(cl%ts);
      else ++cl;
    }
    d=c;
  }
  if(verbose)
  { printf("file \"%s\" size: %ld bytes, %ld lines <= %ld chars, %ld words\n",
      fname,b,l,cm,w);
  }
  printf(crsr_on);
  if(fp && fp!=stdin) fclose(fp);
}
