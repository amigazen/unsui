/*--------------------------------------------------------------------------*
            __                                                    ts=2
           / /                         $RCSfile: lp.c,v $
          / /  ______                  $Release$
         / /_ / __  /                  $Revision: 2.0 $
        /___// ____/                   $Date: 93/07/29 23:20:36 $
            / /                        $Author: tf $
           /_/                         $State: Exp $

         (c) Copyright 1991,92 Tobias Ferber, All Rights Reserved.

 *--------------------------------------------------------------------------*/

#include "lp.h"

#ifdef _DCC /* DICE */
#include <sys/param.h>
void _abort(void)
{ printf("^C\n\n***BREAK\n");
  exit(1);
}
#endif /* _DCC */

#ifndef MACHINE
#define MACHINE "but where?"
#endif /* !MACHINE */

static char rcs_id[] = "$VER: $Id: lp.c,v 2.0 93/07/29 23:20:36 tf Exp $\0"
                       "(compiled " __DATE__ ", " __TIME__ " " MACHINE ")";

#define BANNER &rcs_id[6]

char *whoami;  /* who am I? (global copy of argv[0]) */

#ifdef NOT_USED_YET
static char *jobreport[]=
{ "Print job terminated without errors.",                     /* 0 */
  "Fatal-Error: AllocMem() failed, ran out of memory.",       /* 1 */
  "Nothing to print.",                                        /* 2 */
  "Unable to open input file.",                               /* 3 */
  "Error: Illegal or unknown directive.",                     /* 4 */
  "Unable to open print destination.",                        /* 5 */
  "Fatal-Error: origin of error unknown.",                    /* 6 */
};
#endif /* NOT_USED_YET */

void lpecho(const char *fmt, ...)
{
  va_list argp;
  va_start(argp,fmt); /* name the last known argument */
  fprintf(stdout,"\r%s: ",whoami);
  vfprintf(stdout,fmt,argp);
  fprintf(stdout,"\n");
  fflush(stdout); /* just to be sure... */
  va_end(argp);
}

/* write the statistics into our logfile */

void statistics(jobparm *jp,
                jobstate *js )
{ FILE *sf;
  if( sf=fopen(LOGFILENAME,"a") )
  { long flags= jp->flags;

    fprintf(sf,"%s (%d+%d lines, %d cleaned, %d wraped, %d broken)\n",
                                                   jp->infile,
                                                   js->inln,
                                                   js->fillups,
                                                   js->cleanups,
                                                   js->wraps,
                                                   js->cracks );

    fprintf(sf,"jobparm -i%d -w%d -h%d -t%d -c%d ",jp->indent,
                                                   jp->charct,
                                                   jp->linect,
                                                   jp->tabsize,
                                                   jp->columns );

    if(JP_PLAIN(flags))       fprintf(sf,"-p ");  else fprintf(sf,"+p ");
    if(JP_AUTOLF(flags))      fprintf(sf,"-e ");  else fprintf(sf,"+e ");
    if(JP_MINI(flags))        fprintf(sf,"-m ");  else fprintf(sf,"+m ");
    if(JP_USEFF(flags))       fprintf(sf,"-f ");  else fprintf(sf,"+f ");
    if(JP_CLEANUP(flags))     fprintf(sf,"+d ");  else fprintf(sf,"-d ");
    if(jp->leader)            fprintf(sf,"-l \"%s\" ",jp->leader);
    if(jp->lineskip > 0)      fprintf(sf,"-s %d ",jp->lineskip);
    else if(jp->lineskip < 0) fprintf(sf,"-n %d ",-(jp->lineskip));
    if(JP_REPLACE(flags))     fprintf(sf,"-r ");
    if(JP_APPEND(flags))      fprintf(sf,"-a ");  else fprintf(sf,"+a ");
    if(jp->outfile)           fprintf(sf,"-o \"%s\" ",jp->outfile);
    if(JP_HEADING(flags))     fprintf(sf,"-n ");  else fprintf(sf,"+n ");
    if(JP_FEEDOUT(flags))     fprintf(sf,"-x ");  else fprintf(sf,"+x ");
    if(JP_DOUBLESIDED(flags)) fprintf(sf,"-2 %d ",jp->firstpage);
    if(JP_SINGLESIDED(flags)) fprintf(sf,"-1 %d ",jp->firstpage);
    if(!JP_FOOTING(flags))    fprintf(sf,"+0 ");
    fprintf(sf,"\n\n");
    fclose(sf);
  }
}

/*
 * submit all print jobs in given jobparm list
 */

int submit_printjob(jobparm *jp)
{
  int state= STATE_OKAY;
  jobstate js;

  if(jp)
  { if(jp->infile && *(jp->infile) &&
       jp->outfile && *(jp->outfile) )
    { state= print_file(jp, &js);
      if(state==STATE_OKAY)
      {
        if(JP_REPLACE(jp->flags))
        { remove(jp->infile);
          rename(jp->outfile, jp->infile);
        }
        printf("   %s => %s [printed]\n",jp->infile, jp->outfile);
        statistics(jp, &js);
      }
    }
    if(state==STATE_OKAY && jp->next)
      state= submit_printjob(jp->next);
  }
  return(state);
}

/*
 * here we build the whole jobparm-list out of main()'s arguments
 */

jobparm *build_joblist(int argc, char **argv)
{ jobparm *def;  /* head of the jobparm-list */
  int state= STATE_OKAY;
  def= alloc_jobparm(0L);  /* default settings + env:lpopts */
  if(def)
  { jobparm *cli= jptail(def);
    cli->next= alloc_jobparm(cli);
    cli= cli->next;
    if(cli)
    { arglist2jobparm(cli, argc, argv);
      check_jobparm(cli,0,"command-line");
      if(cli->tocfile)
        state= file2jobparm(cli,cli->tocfile);
    }
    else
    { lpecho("Not enough memory to parse command line!");
      state= STATE_NOMEM;
    }
  }
  else
  { lpecho("Not enough memory to allocate print job!");
    state= STATE_NOMEM;
  }

  if((state!=STATE_OKAY) && def)
  { free_jobparm(def);
    def= (jobparm *)0L;
  }
  return(def);
}


void main(int argc, char *argv[])
{
  int state= STATE_OKAY; /* job state => return code */
  jobparm *jobparm;

#ifdef _DCC /* DICE */
  onbreak(_abort);
#endif /* _DCC */

  puts(BANNER); /* let me introduce myself... */
  if(argc==2 && (argv[1][0]=='?' || argv[1][0]=='.'))
  { char **s= (char **)howtouse;
    puts(USAGE);
    puts("\nLegal options are:\n");
    if(argv[1][0]=='?') while(*s)
    { printf("%-11s %s %s\n",s[0],s[2],s[3]);
      s= &s[4];
    }
    else while(*s)
    { printf("%-2s %s %s\n",s[1],s[2],s[3]);
      s= &s[4];
    }
    puts("\nALL THIS IS HIGHLY EXPERIMENTAL !!! TAKE GREAT CARE !!!");
  }
  else if(argc>1)
  {
    whoami= argv[0]; /* who am I ? */

    if( jobparm= build_joblist(--argc, ++argv) )
    {
      remove(LOGFILENAME);

#ifdef DEBUG
      debug_joblist(jobparm);
#else
      state= submit_printjob(jobparm);
#endif /* DEBUG */

      free_jobparm(jobparm);
    }
  }
  exit((state==STATE_OKAY) ? 0 : 1);
}

