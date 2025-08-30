/*
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */
#ifdef __STDC__
#ifndef __PROTO
#define __PROTO(a) a
#endif
#endif

#ifndef __NOPROTO
#ifndef __PROTO
#define __PROTO(a) a
#endif
#else
#ifndef __PROTO
#define __PROTO(a) ()
#endif
#endif

#define NSUBEXP  10
typedef struct regexp {
        char *startp[NSUBEXP];
        char *endp[NSUBEXP];
        char regstart;          /* Internal use only. */
        char reganch;           /* Internal use only. */
        char *regmust;          /* Internal use only. */
        int regmlen;            /* Internal use only. */
        char program[1];        /* Unwarranted chumminess with compiler. */
} regexp;

/* Prototypes for functions defined in regexp.c */

regexp *regcomp __PROTO((char *exp));
int regexec __PROTO((register regexp *prog,
                     register char *string));

extern void regsub();
extern void regerror();
