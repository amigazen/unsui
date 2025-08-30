#include "less.h"

extern char editor[];

/* Prototypes for functions defined in help.c */

static void help0 __PROTO((void));
static void help1 __PROTO((void));


/*
 * Display some help.
 * Help is in two pages.
 */
#ifdef __STDC__
static void help0 (void)
#else
        static void
help0()
#endif
{
        ttputs("f,SPACE,^V  *Forward one screen.\n");
        ttputs("b,B         *Backward one screen.\n");
        ttputs("e,j,^N,CR   *Forward N lines, default 1.\n");
        ttputs("y,k,^P,BS   *Backward N lines, default 1.\n");
        ttputs("d           *Forward N lines,  \\ default half screen or\n");
        ttputs("u           *Backward N lines, / last N to d or u command.\n");
        ttputs("r            Repaint screen.\n");
        ttputs("g, <        *Go to line N, default 1.\n");
        ttputs("G, >        *Like g, but default is last line in file.\n");
        ttputs("=            Show current file name\n");
        ttputs("/pattern    *Search forward for N-th occurence of pattern.\n");
        ttputs("?pattern    *Search backward for N-th occurence of pattern.\n");
        ttputs("n           *Repeat previous search (for N-th occurence).\n");
        ttputs("q,Q          Exit.\n");
}

#ifdef __STDC__
static void help1 (void)
#else
        static void
help1()
#endif
{
        char message[100];
        extern char all_options[];

        ttputs("R            Repaint screen, discarding buffered input.\n");
        ttputs("p, %        *Position to N percent into the file.\n");
        ttputs("m<letter>    Mark the current position with <letter>.\n");
        ttputs("'<letter>    Return to a previously marked position.\n");
        ttputs("''           Return to previous position.\n");
        sprintf(message,
           "-X           Toggle a flag (one of \"%s\").\n",  all_options);
        ttputs(message);
        ttputs("E [file]     Examine a new file.\n");
        ttputs("N           *Examine the next file (from the command line).\n");
        ttputs("P           *Examine the previous file (from command line).\n");
        ttputs("V            Print version number.\n");
#if SHELL_ESCAPE
        ttputs("!command     Passes the command to a CLI to be executed.\n");
#endif
#if EDITOR
        sprintf(message,
             "v            Edit the current file (default editor:%s).\n",
                                editor);
        ttputs(message);
#endif
        ttputs("Cursor Keys *Up/Down: by pages; Left/Right: by lines.\n");
        ttputs("Shft Crsr   *Up/Down: by pages; Left/Right: by half-pages.\n");
        ttputs("Help Key    *This screen.\n");
        error("");
}

#ifdef __STDC__
void help (void)
#else
        public void
help()
#endif
{
        extern int sc_height, sc_width;
        register int i;

        for (i = 0;  i < 2;  i++)
        {
                clear();
                so_enter();
                ttputs(Ver); ttputs ( "\n" );
                ttputs("R. Zarling rayz@csustan.EDU");
                so_exit();

                ttputs("\n\nCommands marked with * may be preceeded by a number, N.\n\n");

                switch (i)
                {
                case 0:         help0();
                                if ( sc_height < 35 || sc_width < 60 )
                                {
                                    error("More help...");
                                    break;
                                }
                                /* else vvv fall through vvv */
                case 1:         help1(); return;
                }
        }
}
