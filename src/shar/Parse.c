/* Parse.c

   Copyright (c) 1987 by F. G. Dufoe, III
   All rights reserved.

   Permission is granted to redistribute this program provided the source
   code is included in the distribution and this copyright notice is
   unchanged.

*/

#include <stdlib.h>
#include <string.h>
#include "Token.h"


struct Token *
parse(line, text)
   /* This function parses a line of text.  The calling program passes it
      two character pointers.  The first points to the null-terminated
      string containing the line of text to be parsed.  The second points to
      a buffer the calling program has defined for storing the text of the
      tokens parse() gets.  Parse() returns a pointer to a linked list of
      Token structures (Token structures are defined in Token.h). */
char *line;
   /* This points to the line of text to be parsed.  If it is NULL the
      calling program is through with the list of Token structures and their
      memory can be freed. */
char *text;
   /* This points to the area where token text will be stored.  Text from
      all the tokens will be stored here, so it must be as long as the line
      to be parsed. */
{
   struct Token *current;
      /* This points to the current Token structure. */

   static struct Token *first = NULL;
      /* This points to the beginning of the list of Token structures. */

   void freetokens(struct Token *);
      /* This function walks the list of Token structures and frees their
         memory. */

   TOKEN gettoken(char *, char *);
      /* This function gets the next token from a character string. */

   TOKEN type;
      /* This is the token type returned by gettoken(). */

   char word[100];
      /* This array will be used by gettoken() to store the text of the
         tokens it gets.  Only one token at a time will be stored here. */


   if (line == NULL)
      /* If the calling program passes a NULL pointer we'll free the memory
         allocated for Token structures and return a NULL pointer. */
   {
      freetokens(first);
      return(NULL);
   }

   if (first == NULL)
      /* No memory for Token structures has been allocated yet.  Allocate
         the first one. */
   {
      if ((first = (struct Token *)malloc(sizeof(struct Token))) == NULL)
         fatal("Parse: Could not allocate Token structure.");
         /* If we can't allocate memory for a Token structure we can't
            continue.  Notify the user and terminate. */
      first->text = text;
         /* We'll use the text buffer supplied by the calling program to
            store the token text. */
      first->next = NULL;
         /* Until we have allocated another Token structure this one is the
            end of the list. */
   }

   current = first;
      /* We'll start by using the first Token structure we allocated. */

   while ((type = gettoken(line, word)) != T_NL)
      /* Until we get to the end of the line, keep asking for the next
         token. */
   {
      current->type = type;
         /* Save the type which gettoken() returned. */
      strcpy (current->text, word);
         /* Save the text which gettoken() returned. */
      if (current->next == NULL)
         /* If we don't already have another Token structure allocated let's
            allocate one now to use for the next token. */
      {
         if ((current->next = (struct Token *)
                              malloc(sizeof(struct Token))) == NULL)
            fatal("Parse: Could not allocate Token structure.");
               /* If we couldn't allocate the next Token structure we can't
                  continue.  Notify the user and terminate. */
         current->next->next = NULL;
            /* Make the new structure the one at the end of the list by
               setting its next pointer to NULL. */
      }
      current->next->text = current->text + strlen(word) + 1;
         /* Advance the text pointer to the next available address in the
            calling program's text buffer. */
      current = current->next;
         /* Make the next structure the current one. */
   }
   current->type = type;
   *(current->text) = 0;
   if (current->next != NULL)
      /* If we have some unused Token structures in our list let's free them
         now.  Since we use the same Token structure list over for
         subsequent lines this can happen if a short line follows a long
         one. */
   {
      freetokens(current->next);
      current->next = NULL;
   }
   return(first);
}


void
freetokens(head)
     /* This function walks the list of Token structures and frees the
        memory allocated for the structures. */
struct Token *head;
{
     if (head->next != NULL)
          freetokens(head->next);
     free((char *)head);
}
