#define EXTERN extern
#include "texd.h"

/* overflow.c */

static char *overflow_messages[] = {
	/* 0 */ "pool size",
	/* 1 */ "buffer size",
	/* 2 */ "number of strings",
	/* 3 */ "main memory size",
	/* 4 */ "semantic nest size",
	/* 5 */ "hash size",
	/* 6 */ "save size",
	/* 7 */ "grouping levels",
	/* 8 */ "input stack size",
	/* 9 */ "text input levels",
	/* 10 */ "parameter stack size",
	/* 11 */ "font memory",
	/* 12 */ "exception dictionary",
	/* 13 */ "pattern memory ops",
	/* 14 */ "pattern memory ops per language",
	/* 15 */ "pattern memory",
	0L
};


VOLATILE void STDARGS overflow ( strnumber s, integer n )
{ overflow_regmem

  normalizeselector ();
  print_err("TeX capacity exceeded, sorry ["); /* 286 */
  c_print( overflow_messages[(int)s] );
  printchar( 61 );  printint( n );  printchar( 93 );
  zhelp1( STR_H_IF_NEED_CAPACITY );
  succumb();
}

/* -- end -- */
