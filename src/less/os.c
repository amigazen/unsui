/*
 * Operating system dependent routines.
 *
 * Most of the stuff in here is based on Unix, but an attempt
 * has been made to make things work on other operating systems.
 * This will sometimes result in a loss of functionality, unless
 * someone rewrites code specifically for the new operating system.
 *
 * The makefile provides defines to decide whether various
 * Unix features are present.
 */

#include "less.h"

#include <signal.h>

#ifdef AMIGA
#ifndef STAT
#define STAT 1
#endif
#endif

#if EDITOR || SHELL_ESCAPE
char *getenv();

/*
 * Pass the specified command to a shell to be executed.
 * Like plain "system()", but handles resetting terminal modes, etc.
 */
        public void
lsystem(cmd)
        char *cmd;
{
        int inp;
        char cmdbuf[256];
        char *shell;

        /*
         * Print the command which is to be executed,
         * unless the command starts with a "-".
         */
        if (cmd[0] == '-')
                cmd++;
        else
        {
                lower_left();
                clear_eol();
                putstr("!");
                putstr(cmd);
                putstr("\n");
        }

        /*
         * De-initialize the terminal and take out of raw mode.
         */
        deinit();
        flush();
        raw_mode(0);

#ifdef AMIGA
        {
        extern long tty;

        if (called_from_WB)
                error("can't execute a CLI command from Workbench, sorry!");
        else
                if (cmd && *cmd)
                        /* run a one shot */
                        Execute(cmd, 0L, tty);
        }
#else
        /*
         * Restore signals to their defaults.
         */
        SIGNAL(SIGINT, SIG_DFL);
#ifdef SIGTSTP
        SIGNAL(SIGTSTP, SIG_DFL);
#endif
        /*
         * Force standard input to be the terminal, "/dev/tty",
         * even if less's standard input is coming from a pipe.
         */
        inp = dup(0);
        close(0);
        if (open("/dev/tty", 0) < 0)
                dup(inp);

        /*
         * Pass the command to the system to be executed.
         */
        if ((shell = getenv("SHELL")) != NULL)
        {
                sprintf(cmdbuf, "%s -c \"%s\"", shell, cmd);
                cmd = cmdbuf;
        }
        system(cmd);

        /*
         * Restore standard input, reset signals, raw mode, etc.
         */
        close(0);
        dup(inp);
        close(inp);

        init_signals();
        raw_mode(1);
        init();
#endif
}
#endif


/*
 * Expand a filename, substituting any environment variables, etc.
 * The implementation of this is necessarily very operating system
 * dependent.  This implementation is unabashedly only for Unix systems.
 */
#if GLOB

#include <stdio.h>
FILE *popen();

        public char *
glob(filename)
        char *filename;
{
        FILE *f;
        char *p;
        int ch;
        static char ECHO[] = "echo ";
        static char filebuf[FILENAME+sizeof(ECHO)+1];

        if (filename[0] == '#')
                return (filename);
        strcpy(filebuf, ECHO);
        strtcpy(filebuf+sizeof(ECHO)-1, filename, sizeof(filebuf)-sizeof(ECHO));
        if ((f = popen(filebuf, "r")) == NULL)
                return (filename);
        for (p = filebuf;  p < &filebuf[sizeof(filebuf)-1];  p++)
        {
                if ((ch = getc(f)) == '\n' || ch == EOF)
                        break;
                *p = ch;
        }
        *p = '\0';
        pclose(f);
        return (filebuf);
}

#else

#ifdef __STDC__
char *glob (char *filename)
#else
        public char *
glob(filename)
        char *filename;
#endif
{
        return (filename);
}

#endif


/*
 * Returns NULL if the file can be opened and
 * is an ordinary file, otherwise an error message
 * (if it cannot be opened or is a directory, etc.)
 */

#if STAT

#include <sys/types.h>
#include <sys/stat.h>

static char *stat_erm
    __PROTO((char *filename, char *message, int len, char *erm));


#ifdef __STDC__
char *bad_file (char *filename, char *message, int len)
#else
        public char *
bad_file(filename, message, len)
        char *filename;
        char *message;
        int len;
#endif
{
        struct stat statbuf;

#ifdef AMIGA
        /* On the Amiga, you can't stat a pipe: file, for some reason */
        if (strnicmp(filename, "pipe:", 5) == 0)  /* kludge! */
                return NULL;
#endif
        if (stat(filename, &statbuf) < 0)
                return (errno_message(filename, message, len));
        if ((statbuf.st_mode & S_IFMT) == S_IFDIR)
        {
#ifdef AMIGA
                return stat_erm(filename, message, len, " is a directory");
#else
                static char is_dir[] = " is a directory";
                strtcpy(message, filename, len-sizeof(is_dir)-1);
                strcat(message, is_dir);
                return (message);
#endif
        }
        if ((statbuf.st_mode & S_IFMT) != S_IFREG)
        {
#ifdef AMIGA
                return stat_erm
                    (filename, message, len, " is not a regular file");
#else
                static char not_reg[] = " is not a regular file";
                strtcpy(message, filename, len-sizeof(not_reg)-1);
                strcat(message, not_reg);
                return (message);
#endif
        }
#ifdef AMIGA
        if (!(statbuf.st_mode & S_IREAD))
                return
                    stat_erm(filename, message, len, " is read-protected");
#endif
        return (NULL);
}

#ifdef AMIGA
static char *stat_erm(char *filename, char *message, int len, char *erm)
{
        strtcpy(message, filename, len - strlen(erm));
        strcat(message, erm);
        return (message);
}
#endif

#else

#ifdef __STDC__
char *bad_file (char *filename, char *message, int len)
#else
        public char *
bad_file(filename, message, len)
        char *filename;
        char *message;
        int len;
#endif
{
        return (NULL);
}

#endif

/*
 * Return an error message based on the value of "errno".
 */

#if PERROR

#ifdef __STDC__
char *errno_message (char *filename, char *message, int len)
#else
        public char *
errno_message(filename, message, len)
        char *filename;
        char *message;
        int len;
#endif
{
        char *p;
        static char msg[16];

        extern char *__sys_errlist[];
        extern int __sys_nerr;
        extern int errno;

        if (errno < __sys_nerr)
                p = __sys_errlist[errno];
        else
        {
                sprintf(msg, "Error %ld", errno);
                p = msg;
        }
        strtcpy(message, filename, len-strlen(p)-3);
        strcat(message, ": ");
        strcat(message, p);
        return (message);
}

#else

        public char *
errno_message(filename, message, len)
        char *filename;
        char *message;
        int len;
{
        static char msg[] = ": cannot open";

        strtcpy(message, filename, len-sizeof(msg)-1);
        strcat(message, msg);
        return (message);
}

#endif
