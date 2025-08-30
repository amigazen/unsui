/*
 * Routines dealing with signals.
 *
 * A signal usually merely causes a bit to be set in the "signals" word.
 * At some convenient time, the mainline code checks to see if any
 * signals need processing by calling psignal().
 * An exception is made if we are reading from the keyboard when the
 * signal is received.  Some operating systems will simply call the
 * signal handler and NOT return from the read (with EINTR).
 * To handle this case, we service the interrupt directly from
 * the handler if we are reading from the keyboard.
 */

#ifdef AMIGA
/*
 * This all works on the Amiga under SAS-C.  The SIGINT stuff only
 * responds to ^C from the window that started Less, not the Less window
 * itself.  ^C from the Less window is polled by chk_sigs(), and
 * eventually does the same thing.
 */

#define SIGWINCH
#define S_WINCH 04
#endif

#include "less.h"

#include <signal.h>
#include <setjmp.h>

/*
 * The type of signal handler functions.
 * Usually int, although it should be void.
 */
#ifdef AMIGA
typedef void            HANDLER;
#else
typedef int             HANDLER;
#endif

/* Prototypes for functions defined in signal.c */

static HANDLER interrupt __PROTO((void));


/*
 * "sigs" contains bits indicating signals which need to be processed.
 */
public int sigs;
#ifndef AMIGA /* This is in Less.h for Amiga, since shared with io.c */
#define S_INTERRUPT     01
#endif
#ifdef SIGTSTP
#define S_STOP          02
#endif


extern int reading;
extern int sc_width, sc_height;
extern char *first_cmd;
extern jmp_buf main_loop;

/*
 * Interrupt signal handler.
 */
#ifdef __STDC__
static HANDLER interrupt (void)
#else
        static HANDLER
interrupt()
#endif
{
#ifdef AMIGA
        /* This gets entered upon ^C from the CLI window.  ^C in the
         * Less window is handled in io.c.  A ^C in the CLI window is
         * taken as a real sign of distress, we quit.
         * (Sending less a break from another process gets here, too.
         */
        quit();
#else
        SIGNAL(SIGINT, interrupt);
        sigs |= S_INTERRUPT;
        if (reading)
                psignals();
#endif
}

#ifdef SIGTSTP
/*
 * "Stop" (^Z) signal handler.
 */
        static HANDLER
stop()
{
        SIGNAL(SIGTSTP, stop);
        sigs |= S_STOP;
        if (reading)
                psignals();
}
#endif

#ifdef SIGWINCH
/*
 * "Window" change handler
 */
#ifdef __STDC__
void winch (void)
#else
winch()
#endif
{
#ifndef AMIGA
        SIGNAL(SIGWINCH, winch);
#endif
        sigs |= S_WINCH;
        if (reading)
                psignals();
#ifdef AMIGA
        else
                sigs |= S_INTERRUPT;
#endif
}
#else
#ifdef SIGWIND
/*
 * "Window" change handler
 */
winch()
{
        SIGNAL(SIGWIND, winch);
        sigs |= S_WINCH;
        if (reading)
                psignals();
}
#endif
#endif

/*
 * Set up the signal handlers.
 */
#ifdef __STDC__
void init_signals (void)
#else
        public void
init_signals()
#endif
{
        (void) SIGNAL(SIGINT, interrupt);
#ifdef SIGTSTP
        (void) SIGNAL(SIGTSTP, stop);
#endif
#ifdef SIGWINCH
#ifndef AMIGA
        (void) SIGNAL(SIGWINCH, winch);
#endif
#else
#ifdef SIGWIND
        (void) SIGNAL(SIGWIND, winch);
#endif
#endif
}

/*
 * Process any signals we have recieved.
 * A received signal cause a bit to be set in "sigs".
 */
#ifdef __STDC__
void psignals (void)
#else
        public void
psignals()
#endif
{
        register int tsignals;

        tsignals = sigs;
        sigs = 0;
        if (tsignals == 0)
                return;

        dropout();              /* Discard any buffered output */

#ifdef S_WINCH
        if (tsignals & S_WINCH)
        {
                int old_width, old_height;
                /*
                 * Re-execute get_term() to read the new window size.
                 */
                old_width = sc_width;
                old_height = sc_height;
                get_term();
                if (sc_width != old_width || sc_height != old_height)
                        first_cmd = "r";
#ifndef AMIGA
                longjmp(main_loop, 1);
#endif
        }
#endif
#ifdef SIGTSTP
        if (tsignals & S_STOP)
        {
                /*
                 * Clean up the terminal.
                 */
#ifdef SIGTTOU
                SIGNAL(SIGTTOU, SIG_IGN);
#endif
                lower_left();
                clear_eol();
                flush();
                raw_mode(0);
#ifdef SIGTTOU
                SIGNAL(SIGTTOU, SIG_DFL);
#endif
                SIGNAL(SIGTSTP, SIG_DFL);
#if SIGSETMASK
                /*
                 * This system will not allow us to send a
                 * stop signal (SIGTSTP) to ourself
                 * while we are in the signal handler, like maybe now.
                 * (This can be the case if we are reading; see comment above.)
                 * So we ask the silly system for permission to do so.
                 */
                sigsetmask(0);
#endif
                kill(getpid(), SIGTSTP);
                /*
                 * ... Bye bye. ...
                 * Hopefully we'll be back later and resume here...
                 * Reset the terminal and arrange to repaint the
                 * screen when we get back to the main command loop.
                 */
                SIGNAL(SIGTSTP, stop);
                raw_mode(1);
                first_cmd = "r";
                longjmp(main_loop, 1);
        }
#endif
        if (tsignals & S_INTERRUPT)
        {
#ifdef AMIGA
                error("Interrupt");
#else
                bell();
                /*
                 * {{ You may wish to replace the bell() with
                 *    error("Interrupt"); }}
                 */
#endif
        }

        longjmp(main_loop, 1);
}

