/*	set jump for ansic.library			*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

/* This structure is used by the setjmp & longjmp functions to save */
/* and restore the current environment of the program.				*/

#ifndef	_SETJMP_H
#define _SETJMP_H

struct JMP_BUF
	{
	long jmpret,		/* return address */
	     reg_d1,		/* data registers (exept d0) */
	     reg_d2,
	     reg_d3,
	     reg_d4,
	     reg_d5,
	     reg_d6,
	     reg_d7,
	     reg_a1,		/* address registers (exept a0) */
	     reg_a2,
	     reg_a3,
	     reg_a4,
	     reg_a5,
	     reg_a6,
	     reg_a7;
	};

typedef struct JMP_BUF jmp_buf[1];

#ifdef	__cplusplus
extern "C" {
#endif

extern	int	setjmp(jmp_buf);
extern	void	longjmp(jmp_buf, int);

#ifdef	__cplusplus
}
#endif

#endif	/* _SETIMP_H */
