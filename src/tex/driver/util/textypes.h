/* textypes.h */

/*
 *   Type declarations.  integer must be a 32-bit signed; shalfword must
 *   be a sixteen-bit signed; halfword must be a sixteen-bit unsigned;
 *   quarterword must be an eight-bit unsigned.
 */
typedef long integer;
typedef char boolean;
typedef double real;
typedef short shalfword ;
typedef unsigned short halfword ;
typedef unsigned char quarterword ;
typedef short Boolean ;
/*
 *   If the machine has a default integer size of 16 bits, and 32-bit
 *   integers must be manipulated with %ld, set the macro SHORTINT.
 */
#ifdef XENIX
#define SHORTINT
#else
#undef SHORTINT
#endif
#ifdef MSDOS
#define SHORTINT
#endif

