/*
 *  JOBPARM.C
 */

#include "lp.h"

/*
 * jptail() returns a pointer to the last jobparm structure in the job-list
 * using a while loop instead of recursion to increase efficiency.
 */

jobparm *jptail(jobparm *node)
{ while(node->next)
    node= node->next;
  return(node);
}

/*
 */

void free_jobparm(jobparm *jp)
{ if(jp)
  { if(jp->next)
      free_jobparm(jp->next);
    if(jp->argmem && jp->membytes)
      free(jp->argmem);
    free(jp);
  }
}

/*
 * allocate jobparm structure and init default values.
 */

jobparm *alloc_jobparm(jobparm *jp_default)
{
  jobparm *jp= (jobparm *)malloc(sizeof(jobparm));

  if(jp)
  {
    /* Just to be sure that this is unset: */

    jp->firstpage   = 0;             /* => continue with current # */
    jp->infile      = (char *)0L;    /* which file to print? */
    jp->tocfile     = (char *)0L;    /* list of files for this job */
    jp->argmem      = (char *)0L;    /* not allocated yet */
    jp->membytes    = 0;             /* => not set yet */
    jp->next        = (jobparm *)0L;

    if(jp_default)
    { jp->indent    = jp_default->indent;
      jp->charct    = jp_default->charct;
      jp->linect    = jp_default->linect;
      jp->tabsize   = jp_default->tabsize;
      jp->columns   = jp_default->columns;
      jp->flags     = jp_default->flags;
      jp->leader    = jp_default->leader;
      jp->lineskip  = jp_default->lineskip;
      jp->outfile   = jp_default->outfile;
    }
    else
    { jp->indent    = DEFINDENT;    /* left margin */
      jp->charct    = DEFCHARCT;    /* characters per line = bufsize */
      jp->linect    = DEFLINECT;    /* = don't fill pages */
      jp->tabsize   = 0;            /* = don't convert tabs -> spaces */
      jp->columns   = 1;            /* #of columns of equal width */
      jp->flags     = DEFJPFLAGS;   /* default flags */
      jp->leader    = (char *)0L;
      jp->lineskip  = 0;
      jp->outfile   = PRINTERNAME;  /* should be sth. like "PRT:" */

      { char *lpopts= (char *)getenv(ENV_LPOPTS);
        if(lpopts && *lpopts)
        { int ac;
          char *av[MAXARGS];
          long ln= 0;
#ifdef DEBUG
          printf("LPOPTS= \"%s\"\n",lpopts);
#endif
          lpopts= make_arglist(lpopts, &ac, av, &ln);
          if(ac<0)
          { lpecho(
#ifdef AMIGA
                   "Warning line %d \"ENV:%s\"  Argument scan failed.",ln,
#else
                   "Argument scan failed for environment variable `%s'.",
#endif
                   ENV_LPOPTS);
          }
          else arglist2jobparm(jp,ac,av);
          /* our caller will check jp later... */
        }
      }
    }
  }
  return(jp);
}

/*
 * check the settings in a jobparm structure
 */

int check_jobparm(jobparm *jp, int line, char *from)
{ int state= STATE_OKAY;
  char loc[256];  /* error location */

  if(line>0)
    sprintf(loc,"Error line %ld \"%s\"",line,from);
  else
    sprintf(loc,"Error in %s",from);

  if(jp->charct<1)
  { lpecho("%s Illegal page width %ld  (option -w).",
      loc,jp->charct );
    state= STATE_ERROPT;
  }
  if(jp->linect<1)
  { lpecho("%s Illegal page height %d  (option -h).",
      loc,jp->linect );
    state= STATE_ERROPT;
  }
  if(jp->indent<0)
  { lpecho("%s Negative indention (%ld) not supported.",
      loc,jp->indent );
    state= STATE_ERROPT;
  }
  if(jp->charct <= jp->indent)
  { lpecho("%s Page width must be > indention!",loc);
    state= STATE_ERROPT;
  }
  if(!(jp->outfile && *(jp->outfile)))
  { lpecho("%s No output (device) specified.",loc);
    state= STATE_ERRPRT;
  }
  return state;
}

/*
 */

#ifdef DEBUG
void debug_joblist(jobparm *jp)
{
  while(jp)
  { long flags= jp->flags;
    printf("LP -u \"%s\" -i%d -w%d -h%d -t%d -c%d ",jp->infile,
                                                    jp->indent,
                                                    jp->charct,
                                                    jp->linect,
                                                    jp->tabsize,
                                                    jp->columns );

    if(JP_PLAIN(flags))       printf("-p ");  else printf("+p ");
    if(JP_AUTOLF(flags))      printf("-e ");  else printf("+e ");
    if(JP_MINI(flags))        printf("-m ");  else printf("+m ");
    if(JP_USEFF(flags))       printf("-f ");  else printf("+f ");
    if(JP_CLEANUP(flags))     printf("+d ");  else printf("-d ");
    if(jp->leader)            printf("-l \"%s\" ",jp->leader);
    if(jp->lineskip > 0)      printf("-s %d ",jp->lineskip);
    else if(jp->lineskip < 0) printf("-n %d ",-(jp->lineskip));
    if(JP_REPLACE(flags))     printf("-r ");
    if(JP_APPEND(flags))      printf("-a ");  else printf("+a ");
    if(jp->outfile)           printf("-o \"%s\" ",jp->outfile);
    if(JP_HEADING(flags))     printf("-n ");  else printf("+n ");
    if(JP_FEEDOUT(flags))     printf("-x ");  else printf("+x ");
    if(JP_DOUBLESIDED(flags)) printf("-2 %d ",jp->firstpage);
    if(JP_SINGLESIDED(flags)) printf("-1 %d ",jp->firstpage);
    if(!JP_FOOTING(flags))    printf("+0 ");
    if(jp->tocfile) printf("@ \"%s\"",jp->tocfile);
    printf("\n\n");
    jp= jp->next;
  }
}
#endif
