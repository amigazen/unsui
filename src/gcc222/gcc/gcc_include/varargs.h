/* These macros implement traditional (non-ANSI) varargs for GNU C.  */

#ifndef _GVARARGS_H
#define _GVARARGS_H

#ifdef _STDARG_H
#undef __va_rounded_size
#undef va_start
#undef va_end
#undef va_arg
#endif

#define va_alist	__builtin_va_alist
#define va_dcl		int __builtin_va_alist;
#define va_list		char *

#define va_start(AP)	AP=(char *) &__builtin_va_alist
#define va_end(AP)

#define __va_rounded_size(TYPE)	\
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#define va_arg(AP, TYPE)		\
 (AP += __va_rounded_size (TYPE),*((TYPE *) (AP - __va_rounded_size (TYPE))))

#endif
