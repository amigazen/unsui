/*	standard definitions for ansic.library	*/
/*	(c)Copyright 1992 Davide Pasetto 		*/

#ifndef	_STDDEF_H_
#define	_STDDEF_H_

#include	<sys/types.h>

typedef	long int	ptrdiff_t;
typedef	struct div_t { int quot, rem;}		div_t;
typedef	struct ldiv_t { long quot, rem;}	ldiv_t;

#define	offsetof(TYPE, MEMBER)	((size_t) &((TYPE *)0)->MEMBER)

#ifndef MAX
#define MAX(a,b)	((a)>(b)?(a):(b))
#endif

#ifndef	MIN
#define MIN(a,b)	((a)<=(b)?(a):(b))
#endif

#endif		/* _STDDEF_H_ */

