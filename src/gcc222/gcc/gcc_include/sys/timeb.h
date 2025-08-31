/*	system time b for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef	_SYS_TIMEB_H
#define _SYS_TIMEB_H

#include	<sys/types.h>

struct	timeb
{
    time_t	    time;
    unsigned short  millitm;
    short	    timezone;
    short	    dstflag;
};

#ifdef	__cplusplus
extern "C" {
#endif

extern	void	ftime(struct timeb *__tim);
extern	void	utime(char *__nome, time_t __tim[2]);

#ifdef	__cplusplus
}
#endif

#endif	/* _SYS_TIMEB_H */
