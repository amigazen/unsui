/*
 * NAME: pr.c
 *
 * FUNCTION: Prints a list of files to the printer.
 *
 * COPYRIGHT: 1987 by Samuel Paolucci
 *
 * NOTE: This code may be freely distributed provided this notice is retained.
 */

#include <stdio.h>
#include <ctype.h>
#include <exec/types.h>
#include <time.h>

#define NOT !
#define to_decimal(x) (x - '0')

BOOL io_error = FALSE;

FILE *in;
FILE *out;
FILE *fopen ();

static int page;
static int linetot;
static int linenum = 0;
static int linesize = 80;
static char output[32] = "";  /* empty = stdout by default */
static char linebuffer[256];
static char *copyright = "Copyright 1987 by Samuel Paolucci";

int confirm = 0;
int control = 0;
int copies  = 1;
int lines   = 58;
int headers = 1;
int numbers = 0;
int wrap    = 0;

/* Tab expansion variables */
int nstops = 0;
int tabstops[100];
int tabchar = '\t';  /* input tab character */
int tabgap = 8;      /* tab gap size */
int outtabchar = '\t'; /* output tab character */
int outtabgap = 8;   /* output tab gap size */

/* POSIX pr options */
int expand_tabs = 0;     /* -e option */
int compress_spaces = 0; /* -i option */
int no_headers = 0;      /* -t option */
int offset = 0;          /* -o option */
int columns = 1;         /* -c option */
int round_robin = 0;     /* -a option */
int merge_files = 0;     /* -m option */
int suppress_errors = 0; /* -r option */
int separator = '\t';    /* -s option */

struct tm *localtime();
char *scdir ();
char *fgets ();
char *gets ();
char *index ();
char *ctime ();
time_t time ();
int  fputs ();

/* Tab expansion functions */
void getstops(char *cp);
void expand_tabs(char *line, char *expanded);
void compress_spaces(char *line, char *compressed);

void help ()                           /* print help page */
{
   fprintf (stderr, "\npr [options] file1 [file2 ...]\n");
   fprintf (stderr, "   formats files for printing (POSIX compatible).\n");
   fprintf (stderr, "   By default, output goes to stdout.  Use -p for printer.\n\n");
   fprintf (stderr, "The program will accept Un*x style wildcards:\n");
   fprintf (stderr, "   *  matches any substring\n");
   fprintf (stderr, "   ?  matches any single character\n\n");
   fprintf (stderr, "The valid options are:\n");
   fprintf (stderr, "  -?  - displays this page\n");
   fprintf (stderr, "  -c  - confirms wild cards\n");
   fprintf (stderr, "        <CR>, y, or Y - print this file\n");
   fprintf (stderr, "        n or N - don't print this file\n");
   fprintf (stderr, "        ! - print this and all remaining files\n");
   fprintf (stderr, "  -f  - specifies wide carriage\n");
   fprintf (stderr, "  -h  - removes headers\n");
   fprintf (stderr, "  -l# - specifies the # of lines per page\n");
   fprintf (stderr, "  -m# - specifies the # of copies\n");
   fprintf (stderr, "  -n  - displays line numbers\n");
   fprintf (stderr, "  -s  - diverts output to the standard output file\n");
   fprintf (stderr, "  -p  - prints directly to printer (PRT:)\n");
   fprintf (stderr, "  -w  - wraps lines\n");
   fprintf (stderr, "  -e  - expand tabs to spaces\n");
   fprintf (stderr, "  -i  - compress spaces to tabs\n");
   fprintf (stderr, "  -t  - suppress headers and trailers\n");
   fprintf (stderr, "  -o  - offset each line by N spaces\n");
   fprintf (stderr, "  -c  - multi-column output\n");
   fprintf (stderr, "  -a  - round-robin column filling\n");
   fprintf (stderr, "  -m  - merge multiple files\n");
   fprintf (stderr, "  -r  - suppress error messages\n");
   fprintf (stderr, "  -s  - separate columns with character\n");
}

static short hour;
static short minute;
static short year;
static short day;
static short month;

void getdate ()

{
   long clock;
   struct tm *tm;

   clock = time (0L);
   tm = localtime (&clock);

   hour = tm->tm_hour;
   minute = tm->tm_min;
   year = 1900 + tm->tm_year;
   day = tm->tm_mday;
   month = 1 + tm->tm_mon;
}

void putheader (name)

char *name;

{
   int i = 0;
   int j;
   int ablanks = (linesize+numbers*5)/2 - 4;
   int bblanks = (linesize+numbers*5)/2 - 21;
   char outfb[133];
   static char headbuffer[174];

   if (out != stdout && page == 1) {      /* reset printer */
      headbuffer[i++] = 0x1b;
      headbuffer[i++] = 0x63;
   }
   for (j = 0; name[j] != NULL; j++, i++)         /* add filename */
      headbuffer[i] = name[j];
   for (j = 0; j < ablanks - strlen (name); j++, i++)   /* add blanks */
      headbuffer[i] = ' ';
   sprintf (outfb, "Page %-3d", page++);  /* add page number */
   for (j = 0; outfb[j] != NULL; j++, i++)
      headbuffer[i] = outfb[j];
   for (j = 0; j < bblanks; j++, i++)     /* add blanks*/
      headbuffer[i] = ' ';
   sprintf (outfb, "%02.2d/%02.2d/%4d, %02d:%02.2d\n\n\n",
            month, day, year, hour, minute);      /* add timestamp */
   for (j = 0; outfb[j] != NULL; j++, i++)
      headbuffer[i] = outfb[j];
   headbuffer[i++]= '\0';
   
   (void) fputs (headbuffer, out);       /* write it out */
}

int get_a_line ()

{
   char *i;

   i = fgets (linebuffer, sizeof (linebuffer), in);

   return (i != NULL);
}

void newpage (name)

char *name;

{
   if (out == stdout)
      ;
   else {
      (void) fputs ("\f", out);        
      if (headers) 
         putheader (name);
      linenum = 1;             /* reset line number */
   }
}

int remove_substring (string, first, num_char)

char string[];
int first;     /* location of the first character to remove */
int num_char;  /* number of characters to remove */

{
   int length;		/* length of the string */

   int index1;          /* points to the first character to remove */
   int index2;          /* points to the first character remaining after
			   removal of the other characters */

   length = strlen (string);

   if (first >= length || first < 0)
      return (-1);	/* invalid starting location */

   if (first + num_char <= length)
      index2 = first + num_char;
   else
      index2 = length;	/* only delete to NULL */

   /* remove the characters */

   for (index1 = first; (string[index1] = string[index2]) != NULL; index1++)
      index2++;
      
   return (first);
}

int insert_string (string, substring, location)

char *string;
char *substring;
int location;

{
   int index1;		/* index into the string */
   int index2;		/* index into the substring */
   int index_temp;	/* index into the temporary string */
   
   char temp[256];
   
   /* see if the location is valid */
   
   if (location >= strlen (string))
      return (-1);
      
   /* copy the characters in the string prior to the
      starting location to the temporary string temp */
      
   for (index_temp = 0, index1 = 0; index1 < location; index1++, index_temp++)
      temp[index_temp] = string[index1];
   
   /* append the substring to the current contents of the temporary string */
   
   for (index2 = 0; substring[index2] != NULL; ++index2, index_temp++)
      temp[index_temp] = substring[index2];
   
   /* append the remainder of the string to the temporary string */
   
   while (temp[index_temp++] = string[index1++])
      ;
   
   /* put contents of the temporary string back into the string */
   
   strcpy (string, temp);
      
   return (location);
}

void put_a_line (name)

char *name;

{
   int len;
   char string[144];
   char expanded[256];
   char compressed[256];

   
   if ((len = strlen (linebuffer)) != 0) {
      if (linenum == lines)
         newpage (name);
      if (numbers)                /* print a line number */
         fprintf (out, "%4d ", linetot++);

      /* Apply tab expansion/compression if requested */
      if (expand_tabs) {
         expand_tabs(linebuffer, expanded);
         strcpy(linebuffer, expanded);
      }
      if (compress_spaces) {
         compress_spaces(linebuffer, compressed);
         strcpy(linebuffer, compressed);
      }

      /* Apply line offset if requested */
      if (offset > 0) {
         int i;
         for (i = 0; i < offset; i++) {
            fputc(' ', out);
         }
      }

      strncpy (string, linebuffer, linesize);
      if (string[linesize-1] != NULL) {
         if (string[linesize-1] != '\n') {
	    string[linesize] = '\n';
            string[linesize+1] = '\0';
	 } else
	    string[linesize] = '\0';
      }
      (void) fputs (string, out);
      linenum++;
      len -= linesize;

      while (len > 1 && wrap) {
         (void) remove_substring (linebuffer, 0, linesize);
         (void) insert_string (linebuffer, "     ", 0);
         strncpy (string, linebuffer, linesize);
         if (string[linesize-1] != NULL) {
            string[linesize] = '\n';
            string[linesize+1] = '\0';
         }
         if (linenum == lines)
            newpage (name);
         (void) fputs (string, out);
         linenum++;
         len -= linesize;
      }
   }
}

void printfile (name)                  /* do the actual printing of the file */

char *name;

{
   int nc;
   
   nc = copies;

   do {
      if ((in = fopen (name, "r")) != NULL) {
         if (out != stdout)
	    fprintf (stderr, "Printing: %s\n", name);
         linetot = 1;             /* reset line number */
         page = 1;                /* reset page number */

         if (headers && out != stdout) {      /* need a header */
            getdate ();           /* get timestamp */
            putheader (name);
         }

         (void) get_a_line ();    /* get a line */

         do {
            put_a_line (name);
         } while (NOT io_error && (BOOL) get_a_line ());
   
         if (io_error) {
            io_error = FALSE;
            fprintf (stderr, "pr: I/O error while printing %s\n", name);
         }
   
         if (out != stdout)
            (void) fputs ("\f", out);
         fclose (in);
                
      } else
         fprintf (stderr, "pr: unable to open '%s'\n", name);
   } while (--nc);

}

int main (argc, argv)

int argc;
char *argv[];

{

   int i;
   int accepted;
   char *p;
   char name[32];                 /* name of file to print */
   char answer[32];
   char c;

   if (argc < 2) {
      fprintf (stderr, "Usage: pr [-?cfhl#m#npsw] [-e[char][gap]] [-i[char][gap]] [-t] [-o offset] [-c columns] [-a] [-m] [-r] [-s char] file1 [file2 ...]\n");
      fprintf (stderr, "       use pr -? for more help\n");
      exit (1);
   }

   /* Set default output to stdout if no output specified */
   if (output[0] == '\0') {
      out = stdout;
   } else {
      if ((out = fopen (output, "w")) == NULL) {
         fprintf(stderr, "pr: can't open output '%s'\n", output);
         exit (1);
      }
   }

   while (argv[1] != NULL) {

      while (*argv[1] == '-') {   /* process options if any */
         p = (char *) &*argv[1];

         p++;                     /* point to the option chars */
         do {
            switch (*p) {
               case 's':          /* list to stdout */
                         if (out != stdout) {
                            fclose (out);
                            out = stdout;
                         }
                         if (numbers)
			    linesize = 72;
			 else
			    linesize = 77;
                         break;

               case 'p':          /* print to printer */
                         if (out != stdout) {
                            fclose (out);
                         }
                         strcpy(output, "PRT:");
                         if ((out = fopen (output, "w")) == NULL) {
                            fprintf(stderr, "pr: can't open printer '%s'\n", output);
                            exit (1);
                         }
                         break;

               case 'm':          /* specify # of copies */
                         copies = 0;
                         p++;
                         while (isdigit (*p))
                            copies = copies*10 + to_decimal (*p++);
                         --p;
                         break;

               case 'h':          /* do not print a header */
                         headers--;
                         lines = 60;
                         break;

               case 'w':          /* wrap lines */
                         wrap++;
                         break;

               case 'n':          /* print line numbers */
                         numbers++;
                         linesize -= 5;
                         break;

               case 'f':          /* specify wide carriage printer */
			 linesize = 132;
			 if (numbers) linesize -= 5;
                         break;

               case 'l':          /* specify # of lines per page */
                         lines = 0;
                         p++;
                         while (isdigit (*p))
                            lines = lines*10 + to_decimal (*p++);
                         --p;
                         if (lines == 0) {
                            fprintf (stderr, "pr: the number of lines per page must be greater than zero\n");
                            exit (1);
                         }
                         break;

               case 'c':          /* confirm any templates */
                         confirm++;
                         break;

               case 'e':          /* expand tabs */
                         expand_tabs = 1;
                         if(arg[1]) {
                            tabchar = arg[1];
                            if(arg[2] && isdigit(arg[2])) {
                               tabgap = atoi(&arg[2]);
                            }
                         }
                         break;

               case 'i':          /* compress spaces to tabs */
                         compress_spaces = 1;
                         if(arg[1]) {
                            outtabchar = arg[1];
                            if(arg[2] && isdigit(arg[2])) {
                               outtabgap = atoi(&arg[2]);
                            }
                         }
                         break;

               case 't':          /* suppress headers and trailers */
                         no_headers = 1;
                         headers = 0;
                         break;

               case 'o':          /* offset lines */
                         if(arg[1]) {
                            offset = atoi(&arg[1]);
                         } else {
                            arg = (--argc > 0) ? *(++argv) : (char *)0L;
                            if(arg) offset = atoi(arg);
                         }
                         break;

               case 'c':          /* multi-column output */
                         if(arg[1]) {
                            columns = atoi(&arg[1]);
                         } else {
                            arg = (--argc > 0) ? *(++argv) : (char *)0L;
                            if(arg) columns = atoi(arg);
                         }
                         if(columns < 1) columns = 1;
                         break;

               case 'a':          /* round-robin column filling */
                         round_robin = 1;
                         break;

               case 'm':          /* merge multiple files */
                         merge_files = 1;
                         break;

               case 'r':          /* suppress error messages */
                         suppress_errors = 1;
                         break;

               case 's':          /* column separator */
                         if(arg[1]) {
                            separator = arg[1];
                         } else {
                            arg = (--argc > 0) ? *(++argv) : (char *)0L;
                            if(arg) separator = arg[0];
                         }
                         break;

               case '?':          /* display help page */
                         help ();
                         exit (0);

               default:           /* invalid option */
                         fprintf(stderr, "pr: '%c' is an invalid option\n", *p);
                         exit (1);
            }
            p++;
         } while (*p);
         
         argc--;
         argv++;
         
      }

      strcpy (name, argv[1]);     /* must be a file name */

      if (index (name, '*') || index (name, '?')) {

         i = 0;                   /* a template was given, expand it */

         while (((p = scdir (name)) != NULL) && ++i < 100) {

            if (confirm) {        /* if confirm ask him for each file */

again:
               fprintf (stderr, "confirm '%s'? ", p);
               (void) gets (answer);
               c = answer[0];
               switch (c) {
                  case '!':        /* if he answers ! then accept all remaining */
                            confirm = 0;
                            accepted = 1;
                            break;

                  case '\0':      /* if yes then include this name */
                  case 'y':
                  case 'Y':
                            accepted = 1;
                            break;

                  case 'n':       /* if no skip this name */
                  case 'N':
                            accepted = 0;
                            break;

                  default:
                            fprintf (stderr, "valid answers are: <CR>, y, or Y - yes do print\n");
                            fprintf (stderr, "                   n or N - no don't print this one\n");
                            fprintf (stderr, "                   ! - yes print this one and all remaining\n");
                                goto again;
                    
               }
            } else                /* if not confirming then match all items */
               accepted = 1;

            if (accepted)         /* go ahead and print this one */
               printfile (p);     /* now print the file */
        
         }
        
      } else 
         printfile (name);        /* now print the file */
        
      argv++;
     
   }
   
   return (0);
   
}

void getstops(char *cp)
{
   int i;
   
   nstops = 0;
   cp++;
   for(;;) {
      i = 0;
      while(*cp >= '0' && *cp <= '9')
         i = i * 10 + *cp++ - '0';
      if (i <= 0 || i > 256) {
         fprintf(stderr, "Bad tab stop spec\n");
         exit(1);
      }
      if(nstops > 0 && i <= tabstops[nstops-1]) {
         fprintf(stderr, "Bad tab stop spec\n");
         exit(1);
      }
      tabstops[nstops++] = i;
      if(*cp == 0)
         break;
      if(*cp++ != ',') {
         fprintf(stderr, "Bad tab stop spec\n");
         exit(1);
      }
   }
}

void expand_tabs(char *line, char *expanded)
{
   int column = 0;
   int n;
   char *src = line;
   char *dst = expanded;
   
   while(*src) {
      if(*src == tabchar) {
         if(nstops == 0) {
            do {
               *dst++ = ' ';
               column++;
            } while(column & 07);
         }
         else if(nstops == 1) {
            do {
               *dst++ = ' ';
               column++;
            } while(((column - 1) % tabstops[0]) != (tabstops[0] - 1));
         }
         else {
            for(n = 0; n < nstops; n++)
               if(tabstops[n] > column)
                  break;
            if(n == nstops) {
               *dst++ = ' ';
               column++;
            }
            else {
               while(column < tabstops[n]) {
                  *dst++ = ' ';
                  column++;
               }
            }
         }
      }
      else if(*src == '\b') {
         if(column)
            column--;
         *dst++ = '\b';
      }
      else {
         *dst++ = *src;
         if(*src == '\n')
            column = 0;
         else
            column++;
      }
      src++;
   }
   *dst = '\0';
}

void compress_spaces(char *line, char *compressed)
{
   int column = 0;
   int n;
   char *src = line;
   char *dst = compressed;
   
   while(*src) {
      if(*src == ' ') {
         column++;
      }
      else if(*src == '\t') {
         column += outtabgap;
         column &= ~(outtabgap - 1);
      }
      else {
         while(((column + outtabgap) & ~(outtabgap - 1)) <= column) {
            if(column + 1 == column)
               break;
            *dst++ = outtabchar;
            column += outtabgap;
            column &= ~(outtabgap - 1);
         }
         while(column < column) {
            *dst++ = ' ';
            column++;
         }
         *dst++ = *src;
         if(*src == '\n')
            column = 0;
         else
            column++;
      }
      src++;
   }
   *dst = '\0';
}   
