/* Sh.c

   Copyright (c) 1987 by F. G. Dufoe, III
   All rights reserved.

   Permission is granted to redistribute this program provided the source
   code is included in the distribution and this copyright notice is
   unchanged.

*/


#include <stdio.h>
#include <stdlib.h>
#include "Token.h"


main(argc, argv)
   /* This program unpacks a shell archive created by Shar.  If the user
      typed more than one file name or typed a question mark on the command
      line the program prints an error message and terminates.  Otherwise it
      tries to open the specified file for input.  If it cannot open the
      file it prints an error message and terminates. */
int argc;
   /* This is the number of arguments on the command line. */
char **argv;
   /* This points to a character array containing the argument values. */
{
   char ehd[512];
      /* When we find a token which identifies the end of the "here
         document" we'll store it here. */
   int ehdl;
      /* This is the length of the "end here document" string. */
   char errmsg[256];
      /* We'll use this character array to put together error messages which
         contain variables. */
   char *fgetline(FILE *);
      /* This function gets the next line of text from a file. */
   FILE *ifile;
      /* This FILE pointer identifies the input file we want to read with
         fgetline(). */
   char *line;
      /* This is the pointer to the line of text returned by fgetline(). */
   int linelen;
      /* This is the length of the line returned by fgetline(). */
   enum
   {
      NEUTRAL,
      COPY
   } mode = NEUTRAL;
      /* In NEUTRAL mode the program reads through the file looking for
         "echo" and "cat" commands and their arguments.  In COPY mode it
         writes each line to an output file until it encounters the text
         string marking the end of the "here document". */
   FILE *ofile;
      /* This file pointer identifies the current output file. */
   struct Token *parse(char *, char *);
      /* This function parses a line of text. */
   TOKEN state;
      /* State is the token type of the previous token. */
   char *text = NULL;
      /* This points to the text buffer where the token text will be
         stored. */
   int textlen = 0;
      /* This is the length of the buffer allocated for token text. */
   struct Token *token;
      /* This points to the list of Token structures returned by parse(). */

   if ((argc != 2) || (argv[1][0] == '?'))
      fatal("Usage: Sh file.");
      /* If the user requested one or provided the wrong number of arguments
         print a usage message and terminate. */

   if ((ifile = fopen(*(argv + 1), "r")) == NULL)
   {
      sprintf(errmsg, "Sh: Can't open %s for input.", *(argv + 1));
      fatal(errmsg);
   }
      /* If we can't open the input file print an error message and
         terminate. */

   while ((line = fgetline(ifile)) != NULL)
      /* Read the file, line by line, until we get to the end. */
   {
      switch (mode)
      {
      case NEUTRAL:
         if (textlen < (linelen = strlen(line)))
            /* If the line is longer than the text buffer we already have
               let's get a new one. */
         {
            if (text != NULL)
               free(text);
                  /* If we had a text buffer allocated we must free it
                     before we allocate another one. */
            if ((text = malloc(linelen)) == NULL)
               /* If we couldn't allocate memory for the token text print an
                  error message and terminate. */
               fatal("Sh: Couldn't allocate buffer for token text.");
            textlen = linelen;
               /* Remember the new text buffer length. */
         }
         token = parse(line, text);
            /* Break the line down into tokens. */
         switch (token->type)
         {
         case T_CAT:
            while (token->next != NULL)
            {
               token = token->next;
               switch (token->type)
               {
               case T_GT:
                  state = T_GT;
                  continue;
               case T_LTLT:
                  state = T_LTLT;
                  continue;
               case T_WORD:
                  switch (state)
                  {
                  case T_GT:
                     if ((ofile = fopen(token->text, "w")) == NULL)
                     {
                        sprintf(errmsg, "Sh: Can't open %s.", token->text);
                        fatal(errmsg);
                     }
                     state = T_WORD;
                     continue;
                  case T_LTLT:
                     strcpy(ehd, token->text);
                     ehdl = strlen(ehd);
                     mode = COPY;
                     state = T_WORD;
                     continue;
                  }
               }
            }
            continue;
         case T_ECHO:
            while (token->next != NULL)
            {
               token = token->next;
               printf("%s ", token->text);
            }
            printf("\n");
            continue;
         }
         continue;
      case COPY:
         if (strncmp(line, ehd, ehdl) == 0)
            /* This line marks the "here document" end. */
         {
            mode = NEUTRAL;
            fclose(ofile);
            continue;
         }
         fprintf(ofile, "%s", line);
         continue;
      }
   }
   parse(NULL, NULL);
   return(0);
}
