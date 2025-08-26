/*
   print.h: output functions

   bf 11-22-96
*/

BPTR StdErr (void);
void init_stderr (int argc, char *argv[]);
void fhprintf (BPTR fh, const char *fmt, ...);
void print_error (const char *str);
