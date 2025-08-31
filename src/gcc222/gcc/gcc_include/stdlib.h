/*	standard library for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 		*/

#ifndef	_STDLIB_H
#define _STDLIB_H

#include	<limits.h>
#include	<stddef.h>

#ifdef	__cplusplus
extern "C" {
#endif

extern	double		atof(const char *);
extern	int		atoi(const char *);
extern	long		atol(const char *);
extern	int		rand(void);
extern	void		srand(unsigned int);
extern	void		*calloc(size_t, size_t);
extern	int		system(const char *);
extern	char		*getenv(const char *);
extern	void		qsort(void *, size_t, size_t, int (*)(const void *, const void *));
extern	long const	labs(long);
extern	div_t const	div(int, int);
extern	ldiv_t const	ldiv(long, long);
extern	int		atexit(void (*)(void));
extern	void volatile	abort(void);
extern	void volatile	exit(int);
extern	void volatile	_exit(int);
extern	void		*malloc(size_t);
extern	void		*realloc(void *, size_t);
extern	void		free(void*);

#ifdef	__cplusplus
}
#endif

#define RAND_MAX		INT_MAX

#ifdef  TARGET_CHECK_STACK
#define alloca(x)      ({ extern unsigned long __Stack_Limit; \
                         register unsigned long z asm ("d0"),y=x;  \
                         asm volatile ( "move.l a7,%0\n\tsub.l ___Stack_Limit,%0\n\tsub.l %2,%0\n\tbgt 0f\n\tmove.l d0,-(a7)\n\tmove.l %2,-(a7)\n\tjsr ___Stack_Overflow\n\tadda.w #4,a7\n0:" :  \
                           "=g" (z) : "0" (z) , "g" (y) );         \
                         __builtin_alloca(y);                      \
                       })
#else
#define	alloca(x)		__builtin_alloca(x)
#endif

#endif	/* _STDLIB_H */
