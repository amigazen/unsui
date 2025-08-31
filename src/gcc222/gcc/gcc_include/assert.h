/* Assert macro for GNU CC															*/
/* Allow this file to be included multiple times with different settings of NDEBUG.	*/

#undef	assert
#undef	__assert

#ifndef	DEBUG
#define assert(ignore)  ((void)0)
#else

#ifdef	__cplusplus
extern "C" {
#endif

extern void	__eprintf();
extern void	abort(void);

#ifdef	__cplusplus
}
#endif


#ifdef __STDC__

#define assert(expression)	\
  ((expression) ? 0 : (__assert (#expression, __FILE__, __LINE__), 0))

#define __assert(expression, file, line)						\
  (__eprintf ("Failed assertion `%s' at line %d of `%s'.\n",	\
	      expression, line, file),								\
   abort ())

#else /* no __STDC__; i.e. -traditional.  */

#define assert(expression)	\
  ((expression) ? 0 : __assert (expression, __FILE__, __LINE__))

#define __assert(expression, file, lineno)						\
  (__eprintf ("Failed assertion `%s' at line %d of `%s'.\n",	\
	      "expression", lineno, file),							\
   abort ())

#endif /* no __STDC__; i.e. -traditional.  */

#endif

