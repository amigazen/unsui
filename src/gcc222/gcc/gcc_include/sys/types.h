/*	system types for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef	_SYS_TYPES_H
#define _SYS_TYPES_H

/* define some new types */

typedef char *		caddr_t;
typedef unsigned long	u_long;
typedef unsigned int	u_int;
typedef unsigned short	u_short;
typedef unsigned char	u_char;
typedef unsigned short	ushort;
typedef unsigned long	ulong;
typedef unsigned char	uchar;
typedef long		dev_t;
typedef long		ino_t;
typedef long		off_t;
typedef unsigned long	time_t;
typedef unsigned long	clock_t;
typedef unsigned long	size_t;
typedef unsigned long	SIZE_T;
typedef unsigned long	fpos_t;

/* macros to change to and from BPTRs */

#define btod(p, t) ((t)(((long)p)<<2))
#define dtob(p) ((BPTR)((long)(p)>>2))

/* a null pointer constant */

#ifndef NULL
#define NULL ((void *)0)
#endif

#endif	/* _SYS_TYPES_H */

