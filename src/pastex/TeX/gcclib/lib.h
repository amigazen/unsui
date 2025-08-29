/*
 * library-specific stuff
 */
#ifndef _LIB_H
#define _LIB_H

#include <time.h>	/* for time_t */

/* filename mapping function type */
#ifndef __FNMAP
#define __FNMAP
#ifdef __STDC__
typedef void (*fnmapfunc_t)(const char *, char *);
#else
typedef void (*fnmapfunc_t)();
#endif
#endif

#if defined(__STDC__ ) && (!(defined(__NO_PROTO__)))
# define P_(x) x
#else
# define P_(x)
#endif

int	_unx2dos P_((const char *, char *));
int	_dos2unx P_((const char *, char *));
int	spawnve P_((int, char *, char **, char **));
int	console_input_status P_((int));
unsigned int	console_read_byte P_((int));
void		console_write_byte P_((int, int));
int		dos2unx P_((const char *, char *));
time_t		dostime P_((time_t));
char *		findfile P_((char *, char *, char **));
char *		_itoa P_((int, char *, int));
char *		_ltoa P_((long, char *, int));
char *		_ultoa P_((unsigned long, char *, int));
time_t		unixtime P_((unsigned int, unsigned int));
int		unx2dos P_((const char *, char *));
void		fnmapfunc P_((fnmapfunc_t u2dos, fnmapfunc_t dos2u));
long		get_sysvar P_((void *var));
void		set_sysvar_to_long P_((void *var, long val));

#undef P_

#endif /* _LIB_H */
