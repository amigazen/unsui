/* Gettoken.c

   Copyright (c) 1987 by F. G. Dufoe, III
   All rights reserved.

   Permission is granted to redistribute this program provided the source
   code is included in the distribution and this copyright notice is
   unchanged.

*/

#include <string.h>
#include "Token.h"


TOKEN
gettoken(line, word)
   /* This function gets the next token from a line of text.  The calling
      program passes it two character pointers.  The first points to the
      buffer containing the line of text to be parsed.  The second points to
      a buffer in which the function is to place the text of the token.  The
      function returns the token type (as defined in Token.h). */
char *line;
   /* This points to the line of text to be parsed. */
char *word;
   /* This is where the function can store the text of the token. */
{
   int c;
      /* This is the current character read from the line. */

   static int il = 0;
      /* This is the index for line[]. */

   int iw = 0;
      /* This is the index for word[]. */

   enum
   {
      LT,      /* One "<" found, look for another. */
      NEUTRAL, /* Look for first character of token. */
      INQUOTE, /* Open quote found, accumulate until close quote found. */
      INWORD   /* Unrecognized character, accumulate until word ends. */
   } state = NEUTRAL;

   while ((c = line[il++]) != '\0')
      /* We are prepared to read the entire line.  If we identify a token
         before we reach the end of the line we'll return early. */
   {
      switch (state)
      {
         case NEUTRAL:
            switch (c)
            {
               case '>':
                  word[iw++] = c;
                  word[iw] = '\0';
                  return(T_GT);
                     /* We recognize the greater than sign immediately, so
                        we put it in the buffer and return. */
               case '<':
                  state = LT;
                  word[iw++] = c;
                  continue;
                     /* We can't identify this token until we know whether
                        the next character is another "<", so we just save
                        it and look at the next character.  The state change
                        "remembers" that we saw a "<". */
               case ' ':
               case '\n':
               case '\t':
                  continue;
                     /* We just ignore any whitespace characters. */
               case '"':
                  state = INQUOTE;
                  continue;
                     /* Inside a quoted string we will accumulate all
                        characters, including whitespace characters.  But we
                        don't consider the quotation marks as part of the
                        string.  We change state to accumulate characters
                        differently. */
               default:
                  state = INWORD;
                  word[iw++] = c;
                  continue;
                     /* Any character not listed above is the beginning of a
                        word.  We will want to accumulate it and those that
                        follow until the word ends. */
            }
         case LT:
            if (c == '<')
            {
               word[iw++] = c;
               word[iw] = '\0';
               return(T_LTLT);
                  /* We found the second "<", so we put it in the buffer and
                     return. */
            }
            word[iw] = '\0';
            il--;
            return(T_WORD);
               /* The second character wasn't "<", so we will return the
                  single "<" as a word.  We will want to look at the second
                  character again, so we decrement il. */
         case INQUOTE:
            switch (c)
            {
               case '\\':
                  word[iw++] = line[il++];
                  continue;
                     /* If we find a "\" we want to include the next
                        character in our string instead of acting on it. */
               case '"':
                  word[iw] = '\0';
                  return(T_WORD);
                     /* We are at the end of the quoted string.  We
                        terminate the string with a null and return.  We
                        don't include the quotation mark in the string. */
               default:
                  word[iw++] = c;
                  continue;
                     /* This isn't one of the special characters dealt with
                        above, so we just accumulate it into our string. */
            }
         case INWORD:
            switch (c)
            {
               case ' ':
               case '\t':
               case '\n':
               case '<':
               case '>':
               case '"':
                  il--;
                     /* Decrement the pointer so we will look at this
                        character again. */
                  word[iw] = '\0';
                     /* Add a null terminator to the token's text string. */
                  if (strcmp(word, "cat") == 0)
                     return(T_CAT);
                        /* There are two special cases of word tokens, the
                           "cat" and "echo" commands. */
                  if (strcmp(word, "echo") == 0)
                     return(T_ECHO);
                  return(T_WORD);
               default:
                  word[iw++] = c;
                     /* Add the character to the string. */
                  continue;
                     /* Get the next character and repeat the cycle. */
            }
      }
   }
   il = 0;
      /* Since we have finished with the current line we reset the index so
         we will start examining the next line at the beginning. */
   return(T_NL);
      /* This is how we tell the calling program we have reached the end of
         the line. */
}
