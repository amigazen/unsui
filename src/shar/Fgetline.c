/* Fgetline.c

   Copyright (c) by Fabbian G. Dufoe, III
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


#define AVGLINE 512
   /* This is the amount of space which will be allocated initially for
      the buffer into which lines read from the file will be placed. */
#define MARGIN 10
   /* When we get within MARGIN characters of the end of the buffer we
      will try to get more memory before proceeding. */


char *
fgetline(ifile)
   /* This function gets the next line of text from a file.  The calling
      program passes it a FILE pointer to identify the file.  Fgetline()
      returns a pointer to a null-terminated character string containing the
      line from that file if it successfully read any characters.  It
      returns a NULL pointer if an error occurs or if it encountered end of
      file.  The calling program must examine the external variable errno
      to determine which is the case.  If fgetline() encountered end of file
      errno will be zero. */
FILE *ifile;
   /* This is a file pointer for the file to be read. */
{
   int c;
      /* This is the character read from the file. */
   extern int errno;
      /* This global variable is used to communicate error codes. */
   int i;
      /* This is a counter that is incremented for each character placed
         in the buffer. */
   static int length = AVGLINE;
      /* This is the length of the character buffer that has been
         allocated. */
   static char *line = NULL;
      /* This is the address of the character string containing the line
         read from the file. */
   char *newline;
      /* This is a temporary pointer used when more memory has to be
         allocated.  Realloc() returns a NULL pointer if it fails and we
         don't want to lose track of the characters we have already
         collected. */


   if (line == NULL)
      /* If the line buffer hasn't been allocated yet do it now. */
      if ((line = (char *)malloc(length)) == NULL)
         /* If malloc() returns a NULL pointer the memory couldn't be
            allocated.  Return a NULL pointer.  The calling program
            can look at errno for the reason. */
         return(NULL);

   for (i = 0; (c = fgetc(ifile)) != EOF && c != '\n'; i++)
      /* As long as we don't hit EOF or a newline keep collecting
         characters. */
   {
      *(line + i) = c;
      if (i > length - MARGIN)
         /* If we are within MARGIN characters of the end of the
            buffer try to allocate more memory. */
      {
         newline = (char *)realloc(line, length + AVGLINE);
         if (newline == NULL)
            /* If realloc() failed we try to salvage what we have
               already collected. */
         {
            *(line + ++i) = '\0';
               /* Tack on a null-terminator. */
            return(line);
               /* Return the pointer to the string we have
                  collected so far. */
         }
         length += AVGLINE;
            /* The buffer is longer now, so we'll update our record
               of its length. */
         line = newline;
            /* We got the additional space we asked for so we change
               the pointer to refer to the new buffer. */
      }
   }

   if (c == '\n')
      /* If we came to a newline character we are at the end of the
         line.  Add the newline to the end of the buffer. */
      *(line + i++) = c;

   if (c == EOF && i == 0)
      /* If we are at end of file and there are no characters in the
         buffer we can free the buffer's memory, set the pointer to
         NULL, and return. */
   {
      free(line);
      line = NULL;
      errno = 0;
      return(NULL);
   }

   *(line + i) = '\0';
      /* Terminate the string with a null character. */

   return(line);
      /* Return the pointer to the calling program. */
}
