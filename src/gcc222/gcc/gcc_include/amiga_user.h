/*	amiga user hooks for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 		*/

/*	These are two hook in the startup module; they are called before	*/
/*	(and after) starting the task. All the standard library are opened	*/
/*	and file, memory & directory functions are initialized.				*/

#ifndef	_AMIGA_USER_H
#define	_AMIGA_USER_H

#ifdef	__cplusplus
extern "C" {
#endif

extern int	___user_init(void);
extern void	___user_cleanup(void);
extern void	_stub(void);

#ifdef	__cplusplus
}
#endif

#endif		/* _AMIGA_USER_H */

