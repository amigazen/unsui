/*   Shar.c

     Copyright (c) 1987 by Fabbian G. Dufoe, III
     All rights reserved.

     Permission is granted to redistribute this program provided the source
     code is included in the distribution and this copyright notice is
     unchanged.

     This program creates a Unix-compatible shell archive in the first file
     named on the command line.  It packs all the command-line files after
     the first into that archive.

     For each file to be included in the archive the program writes
          echo "Creating filename"
          cat > filename <<"***EOF filename***"
     Then it writes a copy of the file and terminates it with
          ***EOF filename***

*/

#include <stdio.h>
#include <time.h>
#ifdef AMIGA
#include <error.h>
#include <exec/types.h>
#else
#include <errno.h>
#include <ctype.h>
extern int sys_nerr;
extern char *sys_errlist[];
#endif

main(argc, argv)
int argc;
char **argv;
{
     int c;
          /* The character or code returned by getc will be stored here. */

     int i;
          /* This will be used as a loop counter to point to the command
             line argument the program is processing. */

     FILE *in;
          /* This is the file pointer which will be used to refer to the
             current input file in the fgetc() and fclose() functions. */

     FILE *out;
          /* This is the file pointer which will be used to refer to the
             output file in the fputc() and fclose() functions. */

     int r;
          /* The code returned by putc will be stored here. */

     long t;
          /* The current time in seconds returned by the time() function
             will be stored here.  The ctime() function will convert it to
             an ASCII string for inclusion in the output file. */

     if (argc < 3)
          /* There must be at least two files named for the program to work.
             The first argument is always the program name.  The second is
             the output file name.  The third is the first input file name.
             Without at least one input file there is no point continuing.
             The program will write an error message to standard error and
             terminate. */
     {
          fprintf(stderr, "Usage: shar outputfile file[s]\n");
          return(-1);
     }

     if ((out = fopen(argv[1], "r")) == NULL)
          /* We try to open the file for reading to see if it is there.  If
             the open fails we check the error number to see why. */
     {
          if (errno == ENOENT)
               /* An error number of ENOENT means the file doesn't exist.
                  That's what we want, so we'll just open it. */

          {
               if ((out = fopen(argv[1], "w")) == NULL)
                    /* If we couldn't open the file for writing print an
                       error message and terminate. */
               {
                    fprintf(stderr, "Shar: Couldn't open %s for output.\n",
                            argv[1]);
                    if (errno > 0 && errno < sys_nerr)
                         /* If there is an entry in the system error list
                            for this error, print the reason for the
                            error. */
                         fprintf(stderr, "\t%d: %s\n", errno,
                                 sys_errlist[errno]);
                    return(-1);
                         /* Terminate with a code to indicate the program
                            did not complete successfully. */
               }
          }
          else
               /* If the file open failed for any other reason we know it
                  exists so we want to display the error message and
                  terminate. */
          {
               fprintf(stderr, "Shar: %s already exists.\n", argv[1]);
               return(-1);
          }
     }
     else
          /* If we were able to open the file we want to close it before we
             print our error message and terminate. */
     {
          (void)fclose(out);
               /* We don't care if fclose fails--we're going to terminate
                  the program anyway--so we ignore the value it returns by
                  casting it to a void. */
          fprintf(stderr, "Shar: %s already exists.\n", argv[1]);
          return(-1);
     }

     /* If we got this far we succeeded in opening the output file for
        writing. */

     time(&t);
          /* We want to include the current time in the opening comments.
             This function gets the number of seconds since the system's
             base date. */

     /* Write some identifying comments to the beginning of the output
        file. */
     fprintf(out,
        "# This is a shell archive.  Remove anything before this line,\n");
     fprintf(out,
        "# then unpack it by saving it in a file and typing \"sh file\"\n");
     fprintf(out, "# Created %s#\n", ctime(&t));
     fprintf(out, "# This archive contains:\n");

     for (i = 2; i < argc; i++)
          /* Now we are going to list each of the remaining file names in a
             comment line. */
          fprintf(out, "#\t\t%s\n", argv[i]);

     for (i = 2; i < argc; i++)
          /* Now we are going to copy each of the remaining file names
             from the command line. */
     {
          if ((in = fopen(argv[i], "r")) == NULL)
               /* Try to open the file for reading.  If the open fails write
                  an error message. */
          {
               fprintf(stderr, "Shar: couldn't open %s for input.\n",
                       argv[i]);
               if (errno > 0 && errno < sys_nerr)
                    /* If there is an entry in the system error list
                       for this error, print the reason for the
                       error. */
                    fprintf(stderr, "\t%d: %s\n", errno,
                            sys_errlist[errno]);
          }
          else
               /* If the file was opened successfully add it to the output
                  file. */
          {
               fprintf(out, "echo \"Creating %s\"\n", argv[i]);
               fprintf(out, "cat > %s <<\"***EOF %s***\"\n", argv[i],
                       argv[i]);
               while ((c = getc(in)) != EOF)
                    /* Read the entire input file and copy it to the output
                       file. */
                    if ((r = putc(c, out)) != c)
                         /* If we couldn't write the character successfully
                            print an error message and terminate. */
                    {
                         fprintf(stderr,
                                 "Shar: couldn't write from %s to %s.\n",
                                 argv[i], argv[1]);
                         if (errno > 0 && errno < sys_nerr)
                              /* If there is an entry in the system error
                                 list for this error, print the reason for
                                 the error. */
                              fprintf(stderr, "\t%d: %s\n", errno,
                                      sys_errlist[errno]);
                         return(-1);
                    }
               fprintf(out, "***EOF %s***\n", argv[i]);
               fclose(in);
          }
     }
     return(0);
}
