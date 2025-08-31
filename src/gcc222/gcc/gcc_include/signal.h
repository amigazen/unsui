/*	signal handling for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 		*/

#ifndef	_SIGNAL_H
#define _SIGNAL_H

#include	<dos/dos.h>

/* This header file contains definitions needed by the signal function	*/
/* NSIG supposedly defines the number of signals actually recognized.	*/

#define	NSIG	25

/* The following symbols are the supported signals */

#define	SIGHUP	1	/* hangup */
#define	SIGINT	2	/* interrupt */
#define	SIGQUIT	3	/* quit */
#define SIGILL	4	/* illegal istruction (not reset when cought) */
#define	SIGTRAP	5	/* trace trap (not reset when cought) */
#define SIGIOT	6	/* IOT istruction */
#define SIGEMT	7	/* EMT istruction */
#define SIGFPE	8	/* floating point exception */
#define SIGKILL	9	/* kill cannot be caught or ignored */
#define SIGBUS	10	/* bus error */
#define SIGSEGV	11	/* segment violation */
#define SIGSYS	12	/* bad argument to system call */
#define SIGPIPE	13	/* write to a pipe with no one reading */
#define	SIGALRM 14	/* alarm signal */
#define SIGTERM	15	/* software termination signal */
					/* this is unassigned */
#define SIGSTOP	17	/* stop (cannot be caught or ignored) */
#define	SIGTSTP	18	/* stop from keyboard */
#define SIGCONT	19	/* continue after stop */
#define SIGCHLD	20	/* child status changed */
#define SIGTTIN	21	/* background read attempted from control terminal */
#define SIGTTOU	22	/* background write attempted to control terminal */
#define SIGTINT	23	/* input record avaiable at control terminal */
#define SIGXCPU	24	/* cpu time limit exceded */
#define SIGXFSZ	25	/* file size limit exceded */

/* The following symbols are the standard actions supported for each signal */

#define SIG_IGN	(void (*)(int))0	/* ignore the signal */
#define SIG_DFL	(void (*)(int))1	/* default action for that signal */

/* Function declarations */

#ifdef	__cplusplus
extern "C" {
#endif

void	*signal(int, void *);

#ifdef	__cplusplus
}
#endif

/* These are needed for correct signal handling: a call to the operating system MUST be	*/
/* an atomic operation in the sense that no break signal must occour during that call.	*/
/* So we must use the below macro to start and end signal exception processing.		*/
/* If you want to use Amiga exception capability you must do an ATOMIC_ON to stop	*/
/* library internal exception processing; before exiting you must set everything back	*/
/* as at start.										*/
/* NOTE: Calling the c-library is safe becouse all routine know of this.		*/
/* NOTE: forbid woudn't help: the task will be breaked anycase.				*/
/* IMPORTANT NOTE: if you want to use exec exception processing you CANNOT call any	*/
/*					library routine					*/

#define SIGNALSET	SIGBREAKF_CTRL_C|SIGBREAKF_CTRL_D|SIGBREAKF_CTRL_E|SIGBREAKF_CTRL_F

#ifdef	__cplusplus
extern "C" {
#endif

extern	long	_atomic_counter;

#ifdef	__cplusplus
}
#endif

#define ATOMIC_ON	{ if(!_atomic_counter) SetExcept(0L,-1L); _atomic_counter++; }
#define ATOMIC_OFF	{ _atomic_counter--; if(_atomic_counter<=0) { SetExcept(SIGNALSET,-1L); _atomic_counter=0; } }

#endif	/* _SIGNAL_H */
