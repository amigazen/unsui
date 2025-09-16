/*
 *  PRINT.C
 */

#include "lp.h"

void newline(FILE *fp, long flags)
{ static BOOL ss= FALSE;  /* first newline changes to subscript */
  if(JP_MINI(flags))
  { if(ss)
      fwrite(SUPERSCRIPT,sizeof(char),sizeof(SUPERSCRIPT),fp);
    else
      fwrite(SUBSCRIPT,sizeof(char),sizeof(SUBSCRIPT),fp);
    ss=!ss;
  }
  else if(JP_AUTOLF(flags))
    fputc(CR,fp);
  else
    fputc(LF,fp);
}

/*
 * seek given file pointer `numlines' lines towards the end of file
 */

void skip_lines(fp, numlines)
FILE *fp;
long numlines;
{ if(numlines > 0)
  { while((fgetc(fp)!=LF) && !feof(fp))
      ;
    if(!feof(fp))
      skip_lines(fp, numlines-1);
  }
}

/*
 * make the headerlines and return a pointer to it's static string
 */

#include <time.h>

char *make_heading(jobparm *jp)
{
  static char header[512];
  int len, maxlen;

  /*len = stcgfn(header,jp->infile);*/
                              /* seperate filename's node.ext */

  len= strlen(jp->infile);
  strcpy(header,jp->infile);

  maxlen = jp->charct;  /* page width */

  if(maxlen<3) header[0]='\n';
  else
  {
    if(len>maxlen)
      strcpy(&header[maxlen-3],"...\n");

    /* add another 24 visible characters from ctime() string */

    else if(len+24< maxlen)
    { unsigned long t;
      char *s;
      time(&t);        /* get #of sec. since 00:00:00 GMT Jan 1, 1970 */
      s= ctime(&t);    /* 24+2 characters ascii string */
      for(t=len; t<maxlen-24; t++)
        header[t]=' ';
      strcpy(&header[t],s);
    }
  }
  strcat(header,"\n\n");
  return(header);
}



/* return the number of digits in a long */
static int numdigits(unsigned long x)
{ int d=0;
  do { x/=10; d++; } while(x>0);
  return(d);
}

/*
 * make page footings, return a pointer to a static string
 */

char *make_footing(jobparm *jp, unsigned long pageno)
{
  static char footer[512];
  char fmt[80];

  if(JP_SINGLESIDED(jp->flags))
  { sprintf(fmt,"\n\n%%%ldld\n",
      (jp->charct + numdigits(pageno) -1) /2 );
    sprintf(footer,fmt,pageno);
  }
  else if(pageno%2)
  { sprintf(fmt,"\n\n%%%ldld\n",jp->charct);
    sprintf(footer,fmt,pageno);
  }
  else sprintf(footer,"\n\n%ld\n",pageno);
  return(footer);
}

/*
 * Here we do the post-processing (i.e. column merging)
 */

int join_columns(jobparm *jp)
{
  FILE *fp1,    /* file pointer reading first column */
       *fp2,    /* file pointer reading second column */
       *fpo;    /* file pointer writing data */
  char *buf;    /* line buffer */

  /* open files */

  if(!(fp1=fopen(TEMPNAME,"r")))
  { lpecho("File-Error: Unable to open `%s'.",TEMPNAME);
    return(STATE_ERRFILE);
  }
  if(!(fp2=fopen(TEMPNAME,"r")))
  { lpecho("Error: Unable to open `%s'.",TEMPNAME);
    if(fp1) fclose(fp1);
    return(STATE_ERRFILE);
  }
  { char *mode= (JP_APPEND(jp->flags)) ? ("a") : ("w");
    if(!(fpo= fopen(jp->outfile,mode)))
    { lpecho("File-Error: Unable to open %s.",jp->outfile);
      if(fp1) fclose(fp1);
      if(fp2) fclose(fp2);
      return(STATE_ERRPRT);
    }
  }

  /* allocate line buffer */

  if(!(buf=(char *)malloc(jp->charct)))
  { lpecho("Fatal-Error: Unable to allocate %ld bytes of memory.",
      jp->charct);
    if(fp1) fclose(fp1);
    if(fp2) fclose(fp2);
    if(fpo) fclose(fpo);
    return(STATE_NOMEM);
  }

  skip_lines(fp2, jp->linect);
  term_clear;

  if(!feof(fp2))
  { char fmt[20];
    char *s,
         *header= 0L,
         *footer= 0L;
    long l=0,
         p=jp->firstpage;

    int  linect= jp->linect;

    if(JP_HEADING(jp->flags))
    { linect-= HEADERLINES;
      header= make_heading(jp);
      fwrite(header, sizeof(char), strlen(header), fpo);
    }

    if(JP_FOOTING(jp->flags))
    { linect-= FOOTERLINES;
      footer= make_footing(jp,p);
    }
    sprintf(fmt,"%%-%lds | ",jp->charct/jp->columns);

    while(!(feof(fp1) || feof(fp2)))
    { for(s=buf;((*s=fgetc(fp1))!=LF) && !feof(fp1); s++)
        ;
      *s='\0';
      fprintf(fpo,fmt,buf);
      for(s=buf;((*s=fgetc(fp2))!=LF) && !feof(fp2); s++)
        ;
      *++s='\0';
      if(!feof(fp2))
      { fputs(buf,fpo);
        l++;
        if((l%linect)==0)
        { skip_lines(fp1, jp->linect);
          skip_lines(fp2, jp->linect);
          term_write(l);
          if(footer)
          { fwrite(footer,sizeof(char),strlen(footer),fpo);
            footer= make_footing(jp, ++p);
          }
          if(header) fwrite(header,sizeof(char),strlen(header),fpo);
        }
      }
    }
  }
  free(buf);
  if(fp2) fclose(fp2);
  if(fp1) fclose(fp1);
  if(fpo) fclose(fpo);
  return(STATE_OKAY);
}

/*
 * print a file
 */

int print_file(jobparm *jp, jobstate *jobstate)
{
  static unsigned long pageno=1;

  FILE *fpi,  /* input stream */
       *fpo;  /* output stream */

  char *buf;       /* line buffer */
  long b=0,        /* general counter */
       inln=0,     /* lines of input */
       outln=0,    /* lines of output */
       wraps=0,    /* #of word wraps */
       cracks=0,   /* #of line breaks */
       cleanups=0, /* #of invisible chars */
       fillups=0;  /* #of empty lines added */

  int  indent = jp->indent,
       charct = jp->charct,
       linect = jp->linect,
       tabsize= jp->tabsize,
       flags  = jp->flags;

  char *header= (char *)0L;      /* page header */

  /* open input- and output stream */

  if(jp->infile && *(jp->infile))
  { if(!strcmp(jp->infile, "-")) fpi= stdin; /* use stdin */
    else if(!(fpi=fopen(jp->infile,"r")))
    { lpecho("Error: Unable to open `%s'.\n",jp->infile);
      return(STATE_ERRFILE);
    }
    if(jp->columns > 1)
    { if(!(fpo= fopen(TEMPNAME, "w")))
      { lpecho("Error: Unable to write temorary data to %s.\n",TEMPNAME);
        if(fpi && fpi!=stdin) fclose(fpi);
        return(STATE_ERRPRT);
      }
      charct /= jp->columns;
      if(JP_HEADING(flags))
      { linect-= (HEADERLINES * (jp->columns -1) );
        flags&= ~JPFLAG_HEADING;
      }
      if(JP_FOOTING(flags))
      { linect-= (FOOTERLINES * (jp->columns -1) );
        flags&= ~JPFLAG_FOOTING;
      }
    }
    else
    { char *mode= (JP_APPEND(flags)) ? ("a") : ("w");
      if(!(fpo= fopen(jp->outfile,mode)))
      { lpecho("Error: Unable to open %s.\n",jp->outfile);
        if(fpi && fpi!=stdin) fclose(fpi);
        return(STATE_ERRPRT);
      }
    }
  }
  else return(STATE_ERRFILE); /* nothing to print */

  /* init printer */

  if(JP_MINI(flags)) /* begin with superscript */
    fwrite(INITMINI,sizeof(char),sizeof(INITMINI),fpo);
  else if(!JP_PLAIN(flags))
  { char *fname= (char *)getenv(ENV_PRTINIT);
    if(fname && *fname)
    { FILE *fpx= fopen(fname,"r");
      if(fpx)
      { char c;
        fprintf(stdout,"%s found, printer installed.\n",fname);
        while((c=fgetc(fpx))!=EOF && !feof(fpx))
        { fputc(c,fpo);
          if(c=='\n') outln++;
        }
        fclose(fpx);
      }
    }
  }

  if(!(buf=(char *)malloc(charct)))
  { lpecho("Fatal-Error: Unable to allocate %ld bytes of memory.",
      charct);
    if(fpi && fpi!=stdin) fclose(fpi);
    if(fpo) fclose(fpo);
    return(STATE_NOMEM);
  }
  for(b=0;b<indent;b++) /* init indention */
    buf[b]=' ';
  if(jp->leader)
  { strcpy(&buf[indent],jp->leader);
    indent+= strlen(jp->leader);
  }
  b= indent;

  printf("        %s => %s\r",jp->infile,jp->outfile);

  /* skip lines */

  if(jp->lineskip>0)
  { int skip= jp->lineskip;
    if(skip<0)
      skip= -skip*linect; /* skip pages */
    if(skip>0)
      skip_lines(fpi,skip);
  }

  if(jp->firstpage != 0)
    pageno= jp->firstpage;
  else jp->firstpage= pageno;

  if(JP_HEADING(flags))
  { header= make_heading(jp);
    linect-= HEADERLINES;
  }
  if(JP_FOOTING(flags))
    linect-= FOOTERLINES;

  /* main loop */

  { BOOL eol= FALSE;  /* line- or page break read */
    char c;
    int refresh=(linect>1)?(linect):(DEFREFRESH);
                      /* when to print out the current line number */

    while(!feof(fpi))
    { /* b is == indent */
      do
      { c= fgetc(fpi);
        if(c==TAB && tabsize>0)
          do buf[b++]=' '; while((b%tabsize) && b<charct);
        eol= ((c==LF) || (c==FF) || feof(fpi));
        if(!eol) /* LF, FF or EOF  are _not_ part of buf[] */
          buf[b++]= c;
      } while(b<charct && !eol);

      /* b= #of chars in buf[],  buf[b-1] is the last valid char */

      if(eol)
      { inln++;  /* we've read a complete line */
        if(inln%refresh==0)
          term_write(inln);   /* we're alive ! */
      }

      if(JP_CLEANUP(flags)) /* => remove invisible chars */
       { while((buf[b-1]==' ' || buf[b-1]=='\t') && b>0)
        { --b;
          ++cleanups;
        }
      }

      if(header && (outln%linect)==0)
        fwrite(header,sizeof(char),strlen(header),fpo);

      if(eol) /* normal line */
      { if((feof(fpi) && b>indent) || (!feof(fpi) && b>0))
          fwrite(buf,sizeof(char),b,fpo);
        newline(fpo,flags);
        outln++;
        /*
         * We increase outln in any case, since the first line on a new
         * page terminated without '\n' would imply (outln%linect) to
         * be ==0 and this page would NOT be filled up if an EOF follows.
         * note that a file ending without a '\n' character will get it from
         * lp in any case.
         */
        b=indent;
      }
      else /* line too long */
      { int bp=b-1; /* break/wrap position */

        /* search for a word wrap position */
        while(buf[bp]!=' ' && buf[bp]!='\t' && --bp>indent);

        if(bp>indent) /* => confirm word wrap */
        { int i;
          fwrite(buf,sizeof(char),bp,fpo);
          newline(fpo,flags);
          outln++;
          bp++;  /* skip word terminating character */
          for(i=0;i<b-bp;i++) buf[indent+i]=buf[bp+i];
          b=indent+i;
          wraps++;
        }

        else /* => break line... we've got no choice */
        { fwrite(buf,sizeof(char),b,fpo);
          newline(fpo,flags);
          outln++;
          b=indent;
          cracks++;
        }
      }

      /* handle form feed character */
      if(c==FF && (outln%linect) > 0)
      { int al= linect-(outln%linect);  /* #of additional lines */
        /* Warning:
         * If (outln%linect)==0 we would add another lincet lines to
         * the output without increasing/printing the page number and a
         * new header! So: If a header and/or footer were also toggled,
         * linect would not be enough to fill up a full page since we
         * decreased it by headerlines and/or footerlines!!!
         */
        if(JP_USEFF(flags))
          fputc(FF, fpo);
        else
        { int i;
          for(i=0;i<al;i++) newline(fpo,flags);
          fillups+=al;
        }
        outln+=al;
      }

      if((outln%linect)==0)
      { if(JP_FOOTING(flags))
        { char *s= make_footing(jp, pageno);
          fwrite(s,sizeof(char),strlen(s),fpo);
        }
        pageno++;
      }
    }
    if(JP_FEEDOUT(flags))
    { linect*= jp->columns;
      for(b=0;(outln%linect)!=0;outln++,b++)
        newline(fpo,flags); /* feed out last page */
      fillups+=b;

      /* take care: (here was a bug v1.16)
       * if(b==0) we've already added the footer !
       */
      if(b>0)
      { if(JP_FOOTING(flags))
        { char *s= make_footing(jp, pageno);
          fwrite(s,sizeof(char),strlen(s),fpo);
        }
        pageno++;
      }
    }
  }
  if(JP_MINI(flags)) /* turn off all special modi at the EOF */
    fwrite(INITNOMINI,sizeof(char),sizeof(INITNOMINI),fpo);

  if(buf) free(buf);

  if(fpi && fpi!=stdin) fclose(fpi);
  if(fpo) fclose(fpo);

  jobstate->inln     = inln;
  jobstate->outln    = outln;
  jobstate->wraps    = wraps;
  jobstate->cracks   = cracks;
  jobstate->cleanups = cleanups;
  jobstate->fillups  = fillups;

  if(jp->columns>1)
    join_columns(jp);

  return(STATE_OKAY);
}

