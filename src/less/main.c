/*
 * Entry point, initialization, miscellaneous routines.
 */

#include "less.h"

#include "position.h"
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

public int      ispipe;
public jmp_buf  main_loop;
public char *   first_cmd;
public char *   every_first_cmd;
public int      new_file;
public int      is_tty;
public char     current_file[FILENAME];
public char     previous_file[FILENAME];
public POSITION prev_pos;
public int      any_display;
public int      ac;
public char **  av;
public int      curr_ac;
#if LOGFILE
public int      logfile = -1;
public int      force_logfile = 0;
public char *   namelogfile = NULL;
#endif
#if EDITOR
public char *   editor;
#endif

extern int file;
extern int nbufs;
extern int sigs;
extern int quit_at_eof;
extern int p_nbufs, f_nbufs;
extern int back_scroll;
extern int top_scroll;
extern int sc_height;
extern int errmsgs;

/* Prototypes for functions defined in main.c */

static void SeekRoot __PROTO((ULONG lock));

/* Command line argument parsing */
static void usage(void);
static void parse_arguments(int argc, char **argv);

#ifdef AMIGA
/********** amiga **************/
#include <dos.h>
#include <intuition/intuition.h>
#include <exec/memory.h>
#include <ios1.h>

extern struct ExecBase *SysBase;

extern struct UFB *__ufbs;
extern int __nufbs;

/* Used by SeekRoot */
#define MAXPATHSTRING 256
static char    absDir[MAXPATHSTRING];

struct IntuitionBase *IntuitionBase;
struct Remember *RememberKey;
int called_from_WB = 0;
int IsV2;

/* max items and chars that *.c can expand to */
#define MAXTEMPLATES 100
public char     *local_argv[MAXTEMPLATES];
public int      local_argc;
#endif

/*
 * Display usage information and exit.
 */
static void usage(void)
{
    printf("Usage: Less [OPTIONS] [FILE...]\n");
    printf("View file contents with pagination.\n\n");
    printf("OPTIONS:\n");
    printf("  -b N          Set buffer size to N (default: 5 for files, 12 for pipes)\n");
    printf("  -c            Repaint by clearing each line\n");
    printf("  -d            Don't complain about dumb terminals\n");
    printf("  -e            Quit at end-of-file\n");
    printf("  -f            Assume data is clean (no nulls, etc.)\n");
    printf("  -h N          Set backwards scroll limit to N lines\n");
    printf("  -m            Use medium prompt\n");
    printf("  -M            Use long prompt\n");
    printf("  -n            Don't use line numbers\n");
    printf("  -p PATTERN    Start at first line matching PATTERN\n");
    printf("  -P PROMPT     Set prompt string\n");
    printf("  -q            Suppress error messages\n");
    printf("  -r            Display raw control characters\n");
    printf("  -s            Squeeze multiple blank lines\n");
    printf("  -t TAG        Find tag TAG\n");
    printf("  -T TAGS       Use TAGS file\n");
    printf("  -u            Underline special characters\n");
    printf("  -w            Highlight first unread line\n");
    printf("  -x N          Set tab stops every N characters\n");
    printf("  -z N          Set window size to N lines\n");
    printf("  -[WINDOW]     Set window position/size (Amiga specific)\n");
    printf("  +COMMAND      Execute COMMAND before displaying\n");
    printf("  ++COMMAND     Execute COMMAND before displaying each file\n");
    printf("  -?            Show this help message\n");
    printf("  --help        Show this help message\n");
    printf("  --version     Show version information\n\n");
    printf("FILE:\n");
    printf("  File(s) to view. Use '-' for standard input.\n\n");
    printf("ENVIRONMENT:\n");
    printf("  LESS         Default options\n");
    printf("  EDITOR       Editor to use for editing files\n\n");
    printf("EXAMPLES:\n");
    printf("  Less file.txt           View file.txt\n");
    printf("  Less -z 20 file.txt     View with 20-line window\n");
    printf("  Less -p \"search\" file   Start at first line containing \"search\"\n");
    printf("  cat file.txt | Less     View from pipe\n");
    exit(0);
}

/*
 * Parse command line arguments using getopts-style parsing.
 */
static void parse_arguments(int argc, char **argv)
{
    int i, sc_window;
    char *arg;
    char *optarg;
    
    for (i = 1; i < argc; i++)
    {
        arg = argv[i];
        
        /* Handle help options */
        if (strcmp(arg, "-?") == 0 || strcmp(arg, "--help") == 0)
        {
            usage();
        }
        
        /* Handle version option */
        if (strcmp(arg, "--version") == 0)
        {
            printf("Less version 1.7Z (Amiga port)\n");
            printf("Copyright (c) 1984,1985 Mark Nudelman\n");
            printf("Amiga port by Bob Leivian and others\n");
            exit(0);
        }
        
        /* Handle options starting with - */
        if (arg[0] == '-' && arg[1] != '\0')
        {
            /* Handle -number for window size */
            if (isdigit(arg[1]))
            {
                sc_window = atoi(&arg[1]);
                continue;
            }
            
            /* Handle other single-letter options */
            if (arg[1] != '-' && arg[1] != '\0')
            {
                scan_option(arg);
                continue;
            }
        }
        
        /* Handle + options */
        if (arg[0] == '+')
        {
            if (arg[1] == '+')
            {
                every_first_cmd = &arg[2];
            }
            else
            {
                first_cmd = &arg[1];
            }
            continue;
        }
        
        /* Handle window specification for Amiga */
#ifdef AMIGA
        if (arg[0] == '[' && arg[strlen(arg)-1] == ']')
        {
            scan_option(arg);
            continue;
        }
#endif
        
        /* If we get here, this is a filename */
        break;
    }
    
    /* Update argc and argv to point to remaining arguments */
    if (i < argc)
    {
        ac = argc - i;
        av = &argv[i];
    }
    else
    {
        ac = 0;
        av = NULL;
    }
}

/*
 * Edit a new file.
 * Filename "-" means standard input.
 * No filename means the "current" file, from the command line.
 */
#ifdef __STDC__
void edit (register char *filename)
#else
        public void
edit(filename)
        register char *filename;
#endif
{
        register int f;
        register char *m;
        POSITION initial_pos;
        char message[100];
        char tempfile[FILENAME];
        static int didpipe;

        initial_pos = NULL_POSITION;
        if (filename == NULL || *filename == '\0')
        {
                if (curr_ac >= ac)
                {
                        error("No current file");
                        return;
                }
                filename = av[curr_ac];
        }
        if (strcmp(filename, "#") == 0)
        {
                if (*previous_file == '\0')
                {
                        error("no previous file");
                        return;
                }
                strtcpy(tempfile, previous_file, sizeof(tempfile));
                filename = tempfile;
                initial_pos = prev_pos;
        }
        if (strcmp(filename, "-") == 0)
        {
                /*
                 * Use standard input.
                 */
                if (didpipe)
                {
                        error("Can view standard input only once");
                        return;
                }
                f = 0;
        } else if ((m = bad_file(filename, message, sizeof(message))) != NULL)
        {
                error(m);
                return;
        } else if ((f = open(filename, 0)) < 0)
        {
                error(errno_message(filename, message, sizeof(message)));
                return;
        }

#ifdef AMIGA
/* SAS 5.10a isatty is broken; reports that pipe:x is a tty */
        if (isatty(f) && f==0)
#else
        if (isatty(f))
#endif
        {
                /*
                 * Not really necessary to call this an error,
                 * but if the control terminal (for commands)
                 * and the input file (for data) are the same,
                 * we get weird results at best.
                 */
                error("Can't take input from a terminal");
                if (f > 0)
                        close(f);
                return;
        }

#if LOGFILE
        /*
         * If he asked for a log file and we have opened standard input,
         * create the log file.
         * We take care not to blindly overwrite an existing file.
         */
        end_logfile();
        if (f == 0 && namelogfile != NULL && is_tty)
        {
                int exists;
                int answer;

                /*
                 * {{ We could use access() here. }}
                 */
                exists = open(namelogfile, 0);
                close(exists);
                exists = (exists >= 0);

                if (exists && !force_logfile)
                {
                        static char w[] = "WARNING: log file exists: ";
                        strcpy(message, w);
                        strtcpy(message+sizeof(w)-1, namelogfile,
                                sizeof(message)-sizeof(w));
                        error(message);
                        answer = 'X';   /* Ask the user what to do */
                } else
                        answer = 'O';   /* Create the log file */

        loop:
                switch (answer)
                {
                case 'O': case 'o':
                        logfile = creat(namelogfile, 0644);
                        break;
                case 'A': case 'a':
                        logfile = open(namelogfile, 1);
                        if (lseek(logfile, (offset_t)0, 2) < 0)
                        {
                                close(logfile);
                                logfile = -1;
                        }
                        break;
                case 'D': case 'd':
                        answer = 0;     /* Don't print an error message */
                        break;
                case 'q':
                        quit();
                default:
                        putstr("\n  Overwrite, Append, or Don't log? ");
                        answer = getchr();
                        putstr("\n");
                        flush();
                        goto loop;
                }

                if (logfile < 0 && answer != 0)
                {
                        sprintf(message, "Cannot write to \"%s\"",
                                namelogfile);
                        error(message);
                }
        }
#endif

        /*
         * We are now committed to using the new file.
         * Close the current input file and set up to use the new one.
         */
        if (file > 0)
                close(file);
        new_file = 1;
        strtcpy(previous_file, current_file, sizeof(previous_file));
        strtcpy(current_file, filename, sizeof(current_file));
        prev_pos = position(TOP);
#ifdef AMIGA
        /* Some heuristics to determine if we've been given a pipe.
           (there must be a better way...)
        */
        ispipe = ((f == 0) && !called_from_WB)
            || (strnicmp(filename, "pipe:", 5) == 0); /* kludge! */
#else
        ispipe = (f == 0);
#endif
        if (ispipe)
                didpipe = 1;
        file = f;
        ch_init( (ispipe) ? p_nbufs : f_nbufs );
        init_mark();

        if (every_first_cmd != NULL)
                first_cmd = every_first_cmd;

        if (is_tty)
        {
                int no_display = !any_display;
                any_display = 1;
                if (no_display && errmsgs > 0)
                {
                        /*
                         * We displayed some messages on error output
                         * (file descriptor 2; see error() function).
                         * Before erasing the screen contents,
                         * display the file name and wait for a keystroke.
                         */
                        error(filename);
                }
                /*
                 * Indicate there is nothing displayed yet.
                 */
                pos_clear();
                if (initial_pos != NULL_POSITION)
                        jump_loc(initial_pos);
        }
}

/*
 * Edit the next file in the command line list.
 */
#ifdef __STDC__
void next_file (int n)
#else
        public void
next_file(n)
        int n;
#endif
{
        if (curr_ac + n >= ac)
        {
                if (quit_at_eof)
                        quit();
                error("No (N-th) next file");
        } else
                edit(av[curr_ac += n]);
}

/*
 * Edit the previous file in the command line list.
 */
#ifdef __STDC__
void prev_file (int n)
#else
        public void
prev_file(n)
        int n;
#endif
{
        if (curr_ac - n < 0)
                error("No (N-th) previous file");
        else
                edit(av[curr_ac -= n]);
}

#ifndef AMIGA
/*
 * Copy a file directly to standard output.
 * Used if standard output is not a tty.
 */
        static void
cat_file()
{
        register int c;

        while ((c = ch_forw_get()) != EOF)
                putchr(c);
        flush();
}
#endif


#ifdef AMIGA
/**************** amiga *****************************/
/* Bob Leivian  4/28/87 fudge up things so it will work
   when called from Work Bench */

char argvbuf[80];

#include "workbench/startup.h"

#ifdef MANX
/* ignore AZTECs wb stuff */
_wb_parse(ignore, ignore2)
char *ignore;
char *ignore2;
{
    return;
}
#endif
#endif


#ifdef NO_GETENV
/* this requires the workbench disk --
  ignore all environment variables for now */
char * getenv(ignore)
{
        return NULL;
}
#endif

/*
 * Entry point.
 */
#ifdef __STDC__
int main (int argc, char **argv)
#else
main(argc, argv)
        int argc;
        char *argv[];
#endif
{
        char *getenv();

#ifdef AMIGA
/***************** amiga ********************/
		/* Version 2 or greater of AmigaDos? */
        IsV2 = ((struct Library *)SysBase)->lib_Version >= 36;

        IntuitionBase = (struct IntuitionBase *)OpenLibrary ( "intuition.library", 0 );
        RememberKey = NULL;

        /* get standard input */
        if ( !chkufb(0) ) /* if linked with tinymain, we get no stdin */
        {
            struct UFB *up;

            up = (struct UFB *)malloc(sizeof(struct UFB));
            up->ufbnxt = __ufbs;
            __ufbs = up;
            up->ufbfh = Input();
            up->ufbflg = UFB_RA | O_RAW;
            up->ufbfn = "StdInput";
            __nufbs++;
            if ( !called_from_WB ) up->ufbflg |= UFB_NC;
        }

        /* if we were called from the workbench we will have no args
           but a pointer to WBstruct, get the filename from this structure */
        if(argc == 0) {
                struct WBStartup *WBmsg;
                struct WBArg *p;
                char *cp, c;
                BPTR newlock;

                /* the argv is really the work bench structure */
                WBmsg = (struct WBStartup *) argv;
                p = WBmsg->sm_ArgList;

                /* fake up the args now */
                /* argv[0] = p->wa_Name; */
                p++; /* ignore first parm (name), since Less don't use it */
                for ( local_argc = 1; local_argc < WBmsg->sm_NumArgs
                    && local_argc < MAXTEMPLATES; local_argc++ )
                {
                    *absDir = '\0';
                    /* SeekRoot UnLocks its argument */
                    newlock = DupLock ( p->wa_Lock );
                    SeekRoot ( newlock );
                    /* The first part is really a device name */
                    for ( cp=absDir; (c = *cp) && c != '/'; cp++ )
                        /* nothing */;
                    *cp = ':';
                    if ( c == '\0' ) /* root dir of a device */
                        *++cp = '\0';
                    else
                        strcat(cp, "/");
                    if ( !(cp = AllocRemember(&RememberKey,
                        strlen(absDir)+strlen(p->wa_Name)+1, 0L)) )
                        quit();
                    strcpy ( cp, absDir );
                    local_argv[local_argc] = strcat(cp, p->wa_Name);
                    p++;
                }
                local_argv[local_argc] = NULL;

                called_from_WB = 1;
        }


#endif

        /*
         * Process command line arguments and LESS environment arguments.
         * Command line arguments override environment arguments.
         */
        init_option();
        scan_option(getenv("LESS"));
        parse_arguments(argc, argv);

#if EDITOR
        editor = getenv("EDITOR");
        if (editor == NULL || *editor == '\0')
                editor = "ed";
#endif

        /*
         * Set up list of files to be examined.
         */
#ifdef AMIGA
        if ( called_from_WB )
        {
            ac = local_argc-1;
            av = local_argv+1; /* CLI case did an argv++ */
        }
        else
        {
#endif
        ac = argc;
        av = argv;
#ifdef AMIGA
        }
#endif


        curr_ac = 0;

        /*
         * Set up terminal, etc.
         */
#ifdef AMIGA
        is_tty = 1;
#else
        is_tty = isatty(1);
        if (!is_tty)
        {
                /*
                 * Output is not a tty.
                 * Just copy the input file(s) to output.
                 */
                if (ac < 1)
                {
                        edit("-");
                        cat_file();
                } else
                {
                        do
                        {
                                edit((char *)NULL);
                                if (file >= 0)
                                        cat_file();
                        } while (++curr_ac < ac);
                }
                quit();
        }
#endif

        raw_mode(1);
        get_term();
        open_getchr();
        init();

        if (setjmp(main_loop))
                quit();
        init_signals();

        /*
         * Select the first file to examine.
         */
        if (ac < 1)
                edit("-");      /* Standard input */
        else
        {
                /*
                 * Try all the files named as command arguments.
                 * We are simply looking for one which can be
                 * opened without error.
                 */
                do
                {
                        edit((char *)NULL);
                } while (file < 0 && ++curr_ac < ac);
        }

        if (file >= 0)
                commands();
        quit();
        /*NOTREACHED*/
}

/*
 * Copy a string, truncating to the specified length if necessary.
 * Unlike strncpy(), the resulting string is guaranteed to be null-terminated.
 */
#ifdef __STDC__
void strtcpy (char *to, char *from, int len)
#else
strtcpy(to, from, len)
        char *to;
        char *from;
        int len;
#endif
{
        strncpy(to, from, len);
        to[len-1] = '\0';
}

/*
 * Exit the program.
 */
#ifdef __STDC__
void quit (void)
#else
        public void
quit()
#endif
{
        /*
         * Put cursor at bottom left corner, clear the line,
         * reset the terminal modes, and exit.
         */
#if LOGFILE
        end_logfile();
#endif
#ifdef AMIGA
        ttclose();

        if (IntuitionBase)
        {
            FreeRemember( &RememberKey, 1 );
            CloseLibrary((struct Library *)IntuitionBase);
        }
#else
        lower_left();
        clear_eol();
        deinit();

        flush();
        raw_mode(0);

#endif

        exit(0);
}


#ifdef AMIGA

/* Thanks to Bruce Rogers for a UseNet posting including the following
   useful function...
*/

/*!*******************************************************************
 * FindRoot by Bruce Rogers 1/20/90
 *********************************************************************/
/*
------------------------------------------------------          Quantum _\/_
2727 Eel                   Bruce (6502 RULES!) Rogers        |\  Duck  ( 0 0)
Davis, Ca 95616            Quantum Duck Software,           |\ \______/ / \\\
916-756-2684               rogers@iris.ucdavis.edu         |\ <  <     |   \/
"My brain is on fire!"                                       \________/ Quark!

*/


/*!*******************************************************************
 * Recursively go up parent chain, looking for oldest parent.
 * Create the absolute path string as we go.
 *********************************************************************/
static void SeekRoot(lock)
ULONG   lock;
{
struct  FileInfoBlock   *fileInfo;
ULONG newlock;
char NameEnd[MAXPATHSTRING], sep;

    fileInfo=AllocMem(sizeof(struct FileInfoBlock),0);

    Examine(lock,fileInfo);
    strcpy ( NameEnd, absDir );
    strcpy ( absDir, fileInfo->fib_FileName );
    sep = 0; if ( *absDir ) sep = absDir[strlen(absDir) - 1];
    if ( sep && *NameEnd && sep != '/' && sep != ':' ) strcat ( absDir, "/" );
    strcat ( absDir, NameEnd );
    newlock = ParentDir(lock);
    UnLock(lock);
    if (newlock!=NULL) SeekRoot(newlock);

    FreeMem(fileInfo,sizeof(struct FileInfoBlock));
}
#endif
