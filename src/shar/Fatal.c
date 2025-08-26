/* Fatal.c

   Copyright (c) 1987 by F. G. Dufoe, III
   All rights reserved.

   Permission is granted to redistribute this program provided the source
   code is included in the distribution and this copyright notice is
   unchanged.

*/


#include <stdio.h>
#ifdef AMIGA
#include <error.h>
#include <exec/types.h>
#else
#include <errno.h>
#include <ctype.h>
extern int sys_nerr;
extern char *sys_errlist[];
#endif


void
fatal(string)
   /* This function prints an error message passed by the calling program
      and exits with an error code. */
char *string;
{
   extern int errno;
      /* This global variable is used to communicate error codes. */

   fprintf(stderr, "%s\n", string);
      /* Print the error message passed by the calling program on standard
         error. */
   if (errno > 0 && errno < sys_nerr)
      fprintf(stderr, "\t%d: %s\n", errno, sys_errlist[errno]);
         /* If the error number is in the system error list print it and its
            explanation on standard error. */
   exit(errno);
      /* Terminate the program and pass an error code back to the parent
         program. */
}
