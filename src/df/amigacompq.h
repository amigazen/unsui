/*****************************************************************************\
 * amigacompq.h                                 DICE/LATTICE C/SAS C/AZTEC C *
 *                 _                                                         *
 *            _   // (c)1992 by "Quarky" Dieter Temme                        *
 *            \\ // Version 1.1 of 25.8.92                                   *
 *             \X/ --- Freeware --- ONLY AMIGA MAKES IT POSSIBLE             *
 *                                                                           *
 * this header file define a few constants for easier feature detection in   *
 * C programs for the Amiga                                                  *
\*****************************************************************************/

#ifdef LATTICE
 #define PRAGMAS_
 #define GETA4TYPE_
 #define GETA4FUNC_ geta4()
#endif

#ifdef _DCC
 #define GETA4TYPE_ __geta4
 #define GETA4FUNC_
 #define min(x,y) (((x) < (y))? (x) : (y))
 #define max(x,y) (((x) > (y))? (x) : (y))
#endif

#ifdef AZTEC_C
 #define PRAGMAS_
 #define GETA4TYPE_
 #define GETA4FUNC_ geta4()
 #define min(x,y) (((x) < (y))? (x) : (y))
 #define max(x,y) (((x) > (y))? (x) : (y))
#endif
