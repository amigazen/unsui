#ifndef DEFINES_H
#define DEFINES_H

/*
#define ATARI 
#define DISPLAY
*/

#define ANSI


#ifndef AMIGA
#  define AMIGA
#endif


#ifdef ATARI
#  define SLOW
#endif



/**************************************************************************/
/***			A	N	S	I			***/
/**************************************************************************/


#ifdef MCH_AMIGA		/* Aztec C defines this */
# ifndef AMIGA
#  define AMIGA
# endif
#endif

#ifdef AMIGA
#  ifdef LATTICE
#    define ANSI
#  endif
#  ifdef AZTEC_C
#    define ANSI
#  endif
#endif

#ifdef ANSI
#  define Args(x)	x
#else
#  define Args(x)	()
#endif


#ifdef AZTEC_C
#  define __stdargs
#  define chip
#endif


/***		O	T	H	E	R		***/


// #define DEBUG
#undef DEBUG



#undef BETACOPYRIGHT	/* zusaetzlicher Copyright-String */


#if defined(AMIGA)
# if defined(M68020)
#  include <m68881.h>
# endif
#endif



#endif  /* DEFINES_H */
