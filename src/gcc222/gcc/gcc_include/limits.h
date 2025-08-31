/*	type limits for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef	_LINITS_H
#define	_LIMITS_H

#include	<stddef.h>

#define CHAR_BIT    8			/* Number of bits in a `char'.  */
#define CHAR_MAX    SCHAR_MAX
#define CHAR_MIN    SCHAR_MIN
#define MB_LEN_MAX  1			/* No multibyte characters supported yet.  */
#define INT_MAX     0x7fffffff
#define INT_MIN     0x80000000
#define LONG_MAX    INT_MAX
#define LONG_MIN    INT_MIN
#define SCHAR_MAX   0x7f
#define SCHAR_MIN   0x80
#define SHRT_MAX    0x7fff
#define SHRT_MIN    0x8000
#define UCHAR_MAX   0xff
#define UINT_MAX    0xffffffff
#define ULONG_MAX   UINT_MAX
#define USHRT_MAX   0xffff

#endif	/* _LINITS_H */
