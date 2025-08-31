/*	time functions for ansic.library	*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef	_TIME_H
#define _TIME_H

#include	<sys/types.h>

struct tm {
	int tm_sec,		/* secondi 0-59 */
		tm_min,		/* minuti  0-59 */
		tm_hour,	/* ora     0-23 */
		tm_mday,	/* giorno del mese 0-31 */
		tm_mon,		/* mese    0-11 */
		tm_year,	/* anni dopo 1900 */
		tm_wday,	/* giorno da domenica 0-6 */
		tm_yday,	/* giorni dal 1 gennaio 0-366 */
		tm_isdst;	/* flag per l'ora legale  >0 se legale, <0 se non disponibile, =0 ora solare */
};

#ifdef	__cplusplus
extern "C" {
#endif

extern	clock_t		clock(void);
/*extern	time_t		time(time_t *);*/
extern	double		difftime(time_t, time_t);
extern	time_t		mktime(struct tm *);
extern	char		*asctime(const struct tm *);
extern	char		ctyme(const struct tm *);
extern	struct tm	*gmtime(time_t *);
extern	struct tm	*localtime(time_t *);

#ifdef	__cplusplus
}
#endif

#define CLOCKS_PER_SEC	1000

#endif /* _TIME_H */
