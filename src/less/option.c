/*
 * Process command line options.
 * Each option is a single letter which controls a program variable.
 * The options have defaults which may be changed via
 * the command line option, or toggled via the "-" command.
 */

#include "less.h"


#ifdef AMIGA
/* use the old-style toupper() that doesn't check its arguement */
#undef toupper
#define toupper(c)      ((c)-'a'+'A')
#endif

#define END_OPTION_STRING       ('$')

/*
 * Types of options.
 */
#define BOOL            01      /* Boolean option: 0 or 1 */
#define TRIPLE          02      /* Triple-valued option: 0, 1 or 2 */
#define NUMBER          04      /* Numeric option */
#define REPAINT         040     /* Repaint screen after toggling option */
#define NO_TOGGLE       0100    /* Option cannot be toggled with "-" cmd */

/* Prototypes for functions defined in option.c */

static char *optstring __PROTO((char *s));
static int getnum __PROTO((char **sp, int c));


/*
 * Variables controlled by command line options.
 */
public int p_nbufs, f_nbufs;    /* Number of buffers.  There are two values,
                                   one used for input from a pipe and
                                   the other for input from a file. */
#ifndef AMIGA
public int clean_data;          /* Can we assume the data is "clean"?
                                   (That is, free of nulls, etc) */
#else
public int clean_data = 1;      /* Can we assume the data is "clean"?
                                   (That is, free of nulls, etc) */
public int ShowCR;				/* Suppress ^M characters? */
#endif
public int quiet;               /* Should we suppress the audible bell? */
public int top_search;          /* Should forward searches start at the top
                                   of the screen? (alternative is bottom) */
public int top_scroll;          /* Repaint screen from top?
                                   (alternative is scroll from bottom) */
public int pr_type;             /* Type of prompt (short, medium, long) */
public int bs_mode;             /* How to process backspaces */
#ifdef DUMBTERM
public int know_dumb;           /* Don't complain about dumb terminals */
#endif
public int quit_at_eof;         /* Quit after hitting end of file twice */
public int squeeze;             /* Squeeze multiple blank lines into one */
public int tabstop;             /* Tab settings */
public int back_scroll;         /* Repaint screen on backwards movement */
public int twiddle;             /* Display "~" for lines after EOF */

extern char *prproto[];
extern int nbufs;
extern int sc_window;
#ifdef AMIGA
public int scroll;
public int sc_window_spec;      /* user's requested -z */
extern int Wind_Spec[4];        /* User-specified window size/position */
#endif
extern char *first_cmd;
extern char *every_first_cmd;
#if LOGFILE
extern char *namelogfile;
extern int force_logfile;
#endif

#define DEF_F_NBUFS     5       /* Default for f_nbufs */
#define DEF_P_NBUFS     12      /* Default for p_nbufs */

static struct option
{
        char oletter;           /* The controlling letter (a-z) */
        char otype;             /* Type of the option */
        int odefault;           /* Default value */
        int *ovar;              /* Pointer to the associated variable */
        char *odesc[3];         /* Description of each value */
} option[] =
{
        { 'c', TRIPLE, 2, &top_scroll,
                { "Repaint by scrolling from bottom of screen",
                  "Repaint by clearing each line",
                  "Repaint by painting from top of screen"
                }
        },
#ifdef DUMBTERM
        { 'd', BOOL|NO_TOGGLE, 0, &know_dumb,
                { NULL, NULL, NULL}
        },
#endif
        { 'e', TRIPLE, 1, &quit_at_eof,
                { "Don't quit at end-of-file",
#ifdef AMIGA
                  "Space bar quits at end-of-file",
                  "Quit at end-of-file"
#else
                  "Quit at end-of-file",
                  "Quit immediately at end-of-file"
#endif
                }
        },
#ifndef AMIGA
        { 'f', BOOL, 0, &clean_data,
                { "Don't assume data is clean",
                  "Assume data is clean",
                  NULL
                }
        },
#else
        { 'f', BOOL|REPAINT, 0, &ShowCR,
                { "Don't show ^M characters",
                  "Show any ^M characters",
                  NULL
                }
        },
#endif
        { 'h', NUMBER, -1, &back_scroll, /* default set at window open */
                { "Backwards scroll limit is %ld lines",
                  NULL, NULL
                }
        },
        { 'm', TRIPLE, 1, &pr_type,
                { "Short prompt",
                  "Medium prompt",
                  "Long prompt"
                }
        },
        { 'q', TRIPLE, 0, &quiet,
                { "Ring the bell for errors AND at eof/bof",
                  "Ring the bell for errors but not at eof/bof",
                  "Never ring the bell"
                }
        },
        { 's', BOOL|REPAINT, 0, &squeeze,
                { "Don't squeeze multiple blank lines",
                  "Squeeze multiple blank lines",
                  NULL
                }
        },
        { 't', BOOL, 1, &top_search,
                { "Forward search starts from bottom of screen",
                  "Forward search starts from top of screen",
                  NULL
                }
        },
        { 'u', TRIPLE|REPAINT, 0, &bs_mode,

                { "Underlined text displayed in underline mode",
                  "Backspaces cause overstrike",
                  "Backspaces print as ^H"
                }
        },
        { 'w', BOOL|REPAINT, 1, &twiddle,
                { "Display nothing for lines after end-of-file",
                  "Display ~ for lines after end-of-file",
                  NULL
                }
        },
        { 'x', NUMBER|REPAINT, 8, &tabstop,
                { "Tab stops every %ld spaces",
                  NULL, NULL
                }
        },
        { 'z', NUMBER|REPAINT, -1, &sc_window,
                { "Scroll window size is %ld lines",
                  NULL, NULL
                }
        },
        { '\0' }
};

public char all_options[64];    /* List of all valid options */

/*
 * Initialize each option to its default value.
 */
#ifdef __STDC__
void init_option (void)
#else
        public void
init_option()
#endif
{
        register struct option *o;
        register char *p;

        /*
         * First do special cases, not in option table.
         */
        first_cmd = every_first_cmd = NULL;
        f_nbufs = DEF_F_NBUFS;          /* -bf */
        p_nbufs = DEF_P_NBUFS;          /* -bp */

        p = all_options;
        *p++ = 'b';

        for (o = option;  o->oletter != '\0';  o++)
        {
                /*
                 * Set each variable to its default.
                 * Also make a list of all options, in "all_options".
                 */
                *(o->ovar) = o->odefault;
                *p++ = o->oletter;
                if (o->otype & TRIPLE)
                        *p++ = toupper(o->oletter);
        }
        *p = '\0';
}

/*
 * Toggle command line flags from within the program.
 * Used by the "-" command.
 */
#ifdef __STDC__
void toggle_option (char *s)
#else
        public void
toggle_option(s)
        char *s;
#endif
{
        int c;
        register struct option *o;
        char *msg;
        int n;
        int dorepaint;
        char message[100];
        char buf[5];

        c = *s++;

        /*
         * First check for special cases not handled by the option table.
         */
        switch (c)
        {
        case 'b':
                sprintf(message, "%ld buffers", nbufs);
                error(message);
                return;
        }

        msg = NULL;
        for (o = option;  o->oletter != '\0';  o++)
        {
                if (o->otype & NO_TOGGLE)
                        continue;
                dorepaint = (o->otype & REPAINT);
                if ((o->otype & BOOL) && (o->oletter == c))
                {
                        /*
                         * Boolean option:
                         * just toggle it.
                         */
                        *(o->ovar) = ! *(o->ovar);
                } else if ((o->otype & TRIPLE) && (o->oletter == c))
                {
                        /*
                         * Triple-valued option with lower case letter:
                         * make it 1 unless already 1, then make it 0.
                         */
                        *(o->ovar) = (*(o->ovar) == 1) ? 0 : 1;
                } else if ((o->otype & TRIPLE) && (toupper(o->oletter) == c))
                {
                        /*
                         * Triple-valued option with upper case letter:
                         * make it 2 unless already 2, then make it 0.
                         */
                        *(o->ovar) = (*(o->ovar) == 2) ? 0 : 2;
                } else if ((o->otype & NUMBER) && (o->oletter == c))
                {
                        n = getnum(&s, '\0');
                        if (n < 0)
                        {
                                /*
                                 * No number; just a query.
                                 * No need to repaint screen.
                                 */
                                dorepaint = 0;
                        } else
                        {
                                /*
                                 * Number follows the option letter.
                                 * Set the variable to that number.
                                 */
                                *(o->ovar) = n;
#ifdef AMIGA
                                if ( c == 'z' )
                                {
                                        sc_window_spec = sc_window;
                                        set_scroll();
                                }
#endif
                        }
                        sprintf(message, o->odesc[0],
                                (o->ovar == &back_scroll) ?
                                get_back_scroll() : *(o->ovar));
                        msg = message;
                } else
                        continue;


                if (dorepaint)
                        repaint();
                if (msg == NULL)
                        msg = o->odesc[*(o->ovar)];
                error(msg);
                return;
        }

        if (control_char(c))
                sprintf(buf, "^%lc", carat_char(c));
        else
                sprintf(buf, "%lc", c);
        sprintf(message, "\"-%s\": no such flag.  Use one of \"%s\"",
                buf, all_options);
        error(message);
}

/*
 * Scan to end of string or to an END_OPTION_STRING character.
 * In the latter case, replace the char with a null char.
 * Return a pointer to the remainder of the string, if any.
 */
#ifdef __STDC__
static char *optstring (char *s)
#else
        static char *
optstring(s)
        char *s;
#endif
{
        register char *p;

        for (p = s;  *p != '\0';  p++)
                if (*p == END_OPTION_STRING)
                {
                        *p = '\0';
                        return (p+1);
                }
        return (p);
}

/*
 * Scan an argument (either from command line or from LESS environment
 * variable) and process it.
 */
#ifdef __STDC__
void scan_option (char *s)
#else
        public void
scan_option(s)
        char *s;
#endif
{
        register struct option *o;
        register int c;
        char message[80];

        if (s == NULL)
                return;

    next:
        if (*s == '\0')
        {
                sc_window_spec = sc_window;
                return;
        }
        switch (c = *s++)
        {
        case '-':
        case ' ':
        case '\t':
        case END_OPTION_STRING:
                goto next;
        case '+':
                if (*s == '+')
                        every_first_cmd = ++s;
                first_cmd = s;
                s = optstring(s);
                goto next;
        case 'P':
                switch (*s)
                {
                case 'm':  prproto[PR_MEDIUM] = ++s;  break;
                case 'M':  prproto[PR_LONG] = ++s;    break;
                default:   prproto[PR_SHORT] = s;     break;
                }
                s = optstring(s);
                goto next;
#if LOGFILE
        case 'L':
                force_logfile = 1;
                /* fall thru */
        case 'l':
                namelogfile = s;
                s = optstring(s);
                goto next;
#endif
        case 'b':
                switch (*s)
                {
                case 'f':
                        s++;
                        f_nbufs = getnum(&s, 'b');
                        break;
                case 'p':
                        s++;
                        p_nbufs = getnum(&s, 'b');
                        break;
                default:
                        f_nbufs = p_nbufs = getnum(&s, 'b');
                        break;
                }
                goto next;
        case '0':  case '1':  case '2':  case '3':  case '4':
        case '5':  case '6':  case '7':  case '8':  case '9':
                {
                        /*
                         * Handle special "more" compatibility form "-number"
                         * to set the scrolling window size.
                         */
                        s--;
                        sc_window = getnum(&s, '-');
                        goto next;
                }
#ifdef AMIGA
        case '[':   /* window specifier */
                {
#include <intuition/intuitionbase.h>
                        int i, rel, wRelative;
                        ULONG ilock;	/* for locking IntutitionBase */
                        struct Screen *WBScr;
                        struct Window *OrigWind;
                        int OrigWS[4] = {0, 0, 640, 400};

						WBScr = MyFindWB();
                        if ( WBScr )
                        {
                        	OrigWS[0] = WBScr->LeftEdge;
                        	OrigWS[1] = WBScr->TopEdge;
                        	OrigWS[2] = WBScr->Width;
                        	OrigWS[3] = WBScr->Height;
                        }
                        MyFreeWB(WBScr);
                        if ( !called_from_WB )
                        {
                        	ilock = LockIBase(0);
                        	if ( (OrigWind = IntuitionBase->ActiveWindow) )
                        	{
                        		OrigWS[0] = OrigWind->LeftEdge;
                        		OrigWS[1] = OrigWind->TopEdge;
                        		OrigWS[2] = OrigWind->Width;
                        		OrigWS[3] = OrigWind->Height;
                        	}
                        	UnlockIBase ( ilock );
                        }
                        wRelative = 0;

						UseCLI = FALSE;
                        for (i = 0; i < 4; i++)
                        	Wind_Spec[i] = 0;
                        for (i = 0; i < 4; i++)
                        {
                        	rel = 1;
                        	while ( *s && *s != ',' && *s != ']' )
                        	{
                        		switch ( *s )
                        		{
                        		case ' ':	break;
                        		case '-':	rel = -rel; break;
                        		case '+':	break;
                        		case 'W':
                        		case 'w':	Wind_Spec[i] += rel * OrigWS[i];
                        					rel = 1;
                        					break;
                        		case '0':	case '1':	case '2':	case '3':	case '4':
                        		case '5':	case '6':	case '7':	case '8':	case '9':
                        					Wind_Spec[i] += rel * getnum ( &s, s[-1] );
                        					rel = 1;
                        					continue; /* s is already incremented */
                        		case 'C':
                        		case 'c':	if ( !strnicmp(s, "cli", 3) )
                        					{
                        						UseCLI = TRUE;
                        						s += 3;
                        						continue;
                        					}
                        					/* v v v Fall through v v v */
                            	default:	error ("Invalid window specification");
                            				quit();
                        		}
                        		s++;
                        	}
                        	/* If a window-relative calculation has generated a negative
                        	   result, constrain it to zero for the top and left or one
                        	   for right or bottom.  Otherwise the negative entry would
                        	   be taken later as relative to the lower right corner.
                        	   Zero will keep the upper left in the upper left of the
                        	   screen, while one will be increased to a minimum dimension.
                        	*/
                        	if ( wRelative && Wind_Spec[i] < 0 )
                        		Wind_Spec[i] = ( i > 1? 1: 0 );
                        	if ( *s == ']' || *s == '\0' ) break;
                        	s++;
                        }
                        if ( *s++ != ']' )
                        {
                            error ("Invalid window specification");
                            quit();
                        }
                        goto next;
                }
#endif
        }

        for (o = option;  o->oletter != '\0';  o++)
        {
                if ((o->otype & BOOL) && (o->oletter == c))
                {
                        *(o->ovar) = ! o->odefault;
                        goto next;
                } else if ((o->otype & TRIPLE) && (o->oletter == c))
                {
                        *(o->ovar) = (o->odefault == 1) ? 0 : 1;
                        goto next;
                } else if ((o->otype & TRIPLE) && (toupper(o->oletter) == c))
                {
                        *(o->ovar) = (o->odefault == 2) ? 0 : 2;
                        goto next;
                } else if ((o->otype & NUMBER) && (o->oletter == c))
                {
                        *(o->ovar) = getnum(&s, c);
                        goto next;
                }
        }

        sprintf(message, "\"-%lc\": invalid flag", c);
        error(message);
        quit();
}

/*
 * Translate a string into a number.
 * Like atoi(), but takes a pointer to a char *, and updates
 * the char * to point after the translated number.
 */
#ifdef __STDC__
static int getnum (char **sp, int c)
#else
        static int
getnum(sp, c)
        char **sp;
        int c;
#endif
{
        register char *s;
        register int n;
        char message[80];

        s = *sp;
        if (*s < '0' || *s > '9')
        {
                if (c == '\0')
                        return (-1);
#ifdef AMIGA
                sprintf(message,
                    "number is required in option after '%lc'", c);
#else
                sprintf(message, "number is required after -%c", c);
#endif
                error(message);
                quit();
        }

        n = 0;
        while (*s >= '0' && *s <= '9')
                n = 10 * n + *s++ - '0';
        *sp = s;
        return (n);
}
