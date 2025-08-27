/* common.h: Definitions and declarations common both to the change
   files and to web2c itself.  */

#ifndef COMMON_H
#define COMMON_H

#if defined(AMIGA) && defined(LATTICE)
# define VOLATILE
# define CONST const
#else
# ifdef __STDC__
#  define VOLATILE volatile
#  define CONST const
# else
#  define VOLATILE
#  define CONST
# endif
#endif


/* pltotf et al. use the symbol `index' themselves; we don't want to
   redefine it in those cases (and those programs don't use the other
   string functions, fortunately).  */ 
#ifndef index
#ifndef	BSD
#include <string.h>
#if !defined(AMIGA) || !defined(__SASC_60)
#define index strchr
#define rindex strrchr
#endif
#else /* BSD */
#include <strings.h>
#endif /* BSD */
#endif /* not index */

#if !defined(AMIGA) || (!defined(_DCC) && !defined(__SASC_60))
extern char *getenv (), *rindex ();
#endif

#if !defined(ANSI) && !defined(_POSIX_SOURCE)
extern SPRINTF_RETURN_TYPE sprintf ();
#endif


/* Global constants.  */
#define true 1
#define false 0

#define TRUE 1
#define FALSE 0


#endif /* not COMMON_H */
