/*
 *  ARGS.C
 */

#include <ctype.h>
#include "lp.h"

char *alloc_argmem(jobparm *jp, long bytesize)
{ char *argmem= (char *)malloc(bytesize * sizeof(char));
  if(argmem)
  { jp->argmem= argmem;
    jp->membytes= bytesize;
  }
  return argmem;
}

char *load_argfile(jobparm *jp, char *filename)
{ FILE *fp;
  char *argmem= (char *)0L;
#ifdef DEBUG
  printf("> load_argfile(\"%s\");\n",filename);
#endif /* DEBUG */
  if(filename)
  { long fsize=0;
    fp=fopen(filename,"r");
    if(fp)
    { fseek(fp, 0L, 2L);  /* OFFSET_END = 2L */
      fsize=ftell(fp);
      if(fsize)
      { fseek(fp, 0L, 0L);
        argmem= alloc_argmem(jp, 1+fsize); /* + EOF */
        if(argmem)
        { fread(argmem, sizeof(char), fsize, fp);
          argmem[fsize]= EOF;
#ifdef DEBUG
          printf("file \"%s\" (%ld+1 bytes) loaded to $%lx",
            filename, fsize, argmem);
#endif /* DEBUG */
        }
      }
      fclose(fp);
    }
  }
  return(argmem);
}


/*
 * arglist2jobparm() parses argc arguments of given arglist argv[]
 * and fills the jobparm structure accordingly. (no checking!)
 */

void arglist2jobparm(jobparm *jp, int argc, char **argv)
{ char *arg;
  if(jp)
  { while(argc>0)
    { arg=argv[0];
      if(isalpha(*arg))
      { char **aopt= (char **)howtouse;
        while(*aopt && stricmp(arg,*aopt))
          aopt= &aopt[4];
        if(*aopt)
        { arg= aopt[1];
#ifdef DEBUG
          printf("AmigaDOS option %s changed to '%s'\n",aopt[0],aopt[1]);
#endif /* DEBUG */
        }
      }

      if(*arg=='-')
      { arg++;
        switch(*arg)
        {
/* -  */  case '\0':
            jp->infile= argv[0];
            break;
/* -1 */  case '1':
            if(arg[1]) jp->firstpage= atol(arg+1);
            else if(argc > 1 && isdigit(argv[1][0]))
            { jp->firstpage= atol(*(++argv));
              argc--;
            }
            jp->flags |= JPFLAG_FOOTING;
            jp->flags &= ~JPFLAG_DOUBLESIDED;
            break;
/* -2 */  case '2':
            if(arg[1]) jp->firstpage= atol(arg+1);
            else if(argc > 1 && isdigit(argv[1][0]))
            { jp->firstpage= atol(*(++argv));
              argc--;
            }
            jp->flags |= JPFLAG_FOOTING
                      |  JPFLAG_DOUBLESIDED;
            break;
/* -a */  case 'a': case 'A':
            jp->flags |= JPFLAG_APPEND;
            break;
/* -b */  case 'b': case 'B':
            if(arg[1]) jp->lineskip= -atol(arg+1);
            else if(argc > 1)
            { jp->lineskip= -atol(*(++argv));
              argc--;
            }
            else jp->lineskip= 0;
            break;  /* negative lineskip will be converted to pageskip later */
/* -c */  case 'c': case 'C':
            if(arg[1]) jp->columns= atol(arg+1);
            else if(argc > 1)
            { jp->columns= atol(*(++argv));
              argc--;
            }
            else jp->columns= 1;
            break;
/* -d */  case 'd': case 'D':
            jp->flags &= ~JPFLAG_CLEANUP;
            break;
/* -e */  case 'e': case 'E':
            jp->flags |= JPFLAG_AUTOLF;
            break;
/* -f */  case 'f': case 'F':
            jp->flags |= JPFLAG_USEFF;
            break;
/* -h */  case 'h': case 'H':
            if(arg[1]) jp->linect= atol(arg+1);
            else if(argc > 1)
            { jp->linect= atol(*(++argv));
              argc--;
            }
            else jp->linect= DEFLINECT;
            break;
/* -i */  case 'i': case 'I':
            if(arg[1]) jp->indent= atol(arg+1);
            else if(argc > 1)
            { jp->indent= atol(*(++argv));
              argc--;
            }
            else jp->indent= 0;
            break;

/* -l */  case 'l': case 'L':
            if(arg[1]) jp->leader= &arg[1];
            else if (argc > 1)
            { jp->leader= *(++argv);
              argc--;
            }
            else jp->leader= (char *)0L;
            break;
/* -m */  case 'm': case 'M':
            jp->flags |= JPFLAG_MINI;
            break;
/* -n */  case 'n': case 'N':
            jp->flags |= JPFLAG_HEADING;
            break;
/* -o */  case 'o': case 'O':
            if(arg[1]) jp->outfile= &arg[1];
            else if(argc > 1)
            { jp->outfile= *(++argv);
              argc--;
            }
            else jp->outfile= PRINTERNAME;
            break;
/* -p */  case 'p': case 'P':
            jp->flags |= JPFLAG_PLAIN;
            break;
/* -r */  case 'r': case 'R':
            jp->outfile= DEFOUT;
            jp->flags &= ~JPFLAG_APPEND;
            jp->flags |= JPFLAG_REPLACE;
            break;
/* -s */  case 's': case 'S':
            if(arg[1]) jp->lineskip= atol(arg+1);
            else if(argc > 1)
            { jp->lineskip= atol(*(++argv));
              argc--;
            }
            else jp->lineskip= 0L;
            break;
/* -t */  case 't': case 'T':
            if(arg[1]) jp->tabsize= atol(arg+1);
            else if(argc > 1 && isdigit(argv[1][0]))
            { jp->tabsize= atol(*(++argv));
              argc--;
            }
            else jp->tabsize= DEFTABSIZE;
            break;
/* -u */  case 'u': case 'U':
            if(arg[1]) jp->infile= &arg[1];
            else if(argc > 1)
            { jp->infile= *(++argv);
              argc--;
            }
            break;
/* -w */  case 'w': case 'W':
            if(arg[1]) jp->charct= atol(arg+1);
            else if(argc > 1)
            { jp->charct= atol(*(++argv));
              argc--;
            }
            else jp->charct= DEFCHARCT;
            break;
/* -x */  case 'x': case 'X':
            jp->flags |= JPFLAG_FEEDOUT;
            break;
          default:
            lpecho("Warning: Unknown option `-%c' ignored.",*arg);
            break;
        }
      }
      else if(*arg=='+')
      { arg++;
        switch(*arg)
        {
/* +0 */  case '0':
            jp->flags &= ~JPFLAG_FOOTING;
            break;
/* +a */  case 'a': case 'A':
            jp->flags &= ~JPFLAG_APPEND;
/* +d */  case 'd': case 'D':
            jp->flags |= JPFLAG_CLEANUP;
            break;
/* +e */  case 'e': case 'E':
            jp->flags &= ~JPFLAG_AUTOLF;
            break;
/* +f */  case 'f': case 'F':
            jp->flags &= ~JPFLAG_USEFF;
            break;
/* +l */  case 'l': case 'L':
            jp->leader = (char *)0L;
            break;
/* +m */  case 'm': case 'M':
            jp->flags &= ~JPFLAG_MINI;
            break;
/* +n */  case 'n': case 'N':
            jp->flags &= ~JPFLAG_HEADING;
            break;
/* +p */  case 'p': case 'P':
            jp->flags &= ~JPFLAG_PLAIN;
            break;
/* +t */  case 't': case 'T':
            jp->tabsize= 0;
            break;
/* +x */  case 'x': case 'X':
            jp->flags &= ~JPFLAG_FEEDOUT;
            break;
          default:
            lpecho("Warning: Unknown option '+%c' ignored.",*arg);
            break;
        }
      }
      else if(*arg=='@')
      { if(arg[1])
          jp->tocfile= &arg[1];
        else
        { argv++;
          argc--;
          jp->tocfile= argv[0];
        }
        jp->flags |= JPFLAG_APPEND;
      }
      else if(*arg > ' ')
        jp->infile= arg;
      argc--;
      argv++;
    }
  }
}

/*
 * make_arglist() converts given argument string `args' into a list of argv[]
 * and a #of args `argc'.
 */

char *make_arglist(args, argc, argv, line)
char *args;
int *argc;
char *argv[];
long *line;
{ int ac=0,         /* argument counter */
      smode=0;      /* scanner mode:  0= outer arg mode
                     *                1= inner argument mode
                     *                2= inner quote mode
                     *                3= EOL found
                     *                4= EOF reached
                     */

  char *t=args;     /* pointer to the beginning of the current arg string */

  while(smode<3)
  {
    switch(*args)
    {  case ';':
        if(smode<2)
        { *args='\0';
          while(*args!='\n' && *args!=EOF)   /* ';' == EOL comment leader */
            args++;
        }
        else ++args;
        break;
      case '\r': case '\n': case EOF: case '\0':
        (*line)++; /* since our caller begins counting with line=0 */
        if(smode==2)
        { lpecho("Warning: Unmatched quote found.");
          ac= -1;     /* error! */
          *args= EOF; /* stop scanner (return 0L) */
        }
        else if(smode==1)
          argv[ac++]=t;
        if(*args==EOF || *args=='\0') smode=4;
        else if(ac>0) smode=3;
        *args++='\0';
        t=args;
#ifdef GNUDOS
        if(*args=='\r' && args[1]=='\n')
          ++t;
#endif /* GNUDOS */
        break;
      case '\"':
        if(smode>0)
        { argv[ac++]=t;
          *args='\0';
          if(smode==1) smode=2;
          else smode=0;
        }
        else smode=2;
        t=++args;
        break;
      case ' ': case '\t':
        if(smode<2)
        { if(smode==1)
          { argv[ac++]=t;
            *args='\0';
          }
          t=&args[1];
          smode=0;
        }
        ++args;
        break;
      default:
        if(smode==0) smode=1;
        ++args;
        break;
    }
  }

  *argc=ac;
  return (smode==4) ? (char *)0L : args;
}

/*
 * file2jobparm() converts each line of given file(name) into a jobparm
 * structure and chains the jobparm structures to a list.
 * The head of this chained list (i.e. first line's jobparm structure)
 * will then be chained to the given jobparm structure 'jp_base'.
 *
 * NOTE:
 *   Given jobparm structure contains the default settings for the whole
 * file.  Each new allocated jobparm structure will be initilalized with
 * it's values.
 */

int file2jobparm(jobparm *jp_base, char *filename)
{
  static char *tgraph[256];  /* recursion depth limited to 256 !!! */
  static int tnodect= 0;
  int state= STATE_FATAL;

#ifdef DEBUG
  printf("> file2jobparm(\"%s\");\n",filename);
#endif /* DEBUG */

  if(jp_base && filename && *filename)
  { char *filemem;

    /* Check the tgraph for non-deterministic recursions. */

    int n;
    for(n=0; n<tnodect; n++)
    { if(!stricmp(filename, tgraph[n]))
      { lpecho("Fatal-Error with \"%s\"  Contents is non-deterministic!",
          filename);
        return(STATE_FATAL);
      }
    }
    if(tnodect>=225)
    { lpecho("Ran out of memory!  Recursion depth > 256");
      return(STATE_FATAL);
    }
    tgraph[tnodect++]= filename; /* add the tgraph node */

    filemem= load_argfile(jp_base, filename);
    if(filemem)
    { jobparm *jp= jp_base; /* working pointer */
      char *args= filemem;  /* pointer into filemem */
      int argc;             /* argument counter */
      char *argv[MAXARGS];  /* argument values */
      long line=0;          /* line number for error messages */
      state= STATE_OKAY;    /* everything seems okay now */

      while(args && (state==STATE_OKAY))
      { jp->next= alloc_jobparm(jp_base);
        if(jp->next)
        { jp= jp->next;
          args= make_arglist(args, &argc, argv, &line);
          if(argc<0)
          { lpecho("Error line %d \"%s\"  Argument scan failed.",
              line,filename);
            return(STATE_FATAL);
          }
          arglist2jobparm(jp,argc,argv);
          state= check_jobparm(jp, line, filename);
          if(state==STATE_OKAY)
          { if(jp->tocfile && *(jp->tocfile))
            { 
              /* prevent lp from performing non-deterministric recursions */
              for(n=0; n<tnodect; n++)
              { if(!stricmp(jp->tocfile, tgraph[n]))
                { lpecho("Fatal-Error line %ld \"%s\"  Non-deterministic recursion!",
                    line,filename);
                  return(STATE_FATAL);
                }
              }
              state= file2jobparm(jp, jp->tocfile);
            }
            jp= jptail(jp);
          }
        }
        else
        { lpecho("Error line %ld \"%s\"  Out of memory!",
            line,filename);
          state=STATE_NOMEM;
        }
      }
    }
    else
    { lpecho("File-Error: Unable to load \"%s\".",filename);
      state=STATE_NOMEM;
    }
    --tnodect; /* delete the tgraph node */
  }
  return state;
}
