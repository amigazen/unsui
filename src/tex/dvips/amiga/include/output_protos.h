/* Prototypes for functions defined in
output.c
 */

void copyfile(char * s);

void figcopyfile(char * s,
                 int systemtype);

void specialout(int c);

void stringend(void);

void scout(int c);

void cmdout(char * s);

void floatout(double n);

void numout(integer n);

void mhexout(register unsigned char * p,
             register long len);

void fontout(int n);

void hvpos(void);

void newline(void);

void nlcmdout(char * s);

int mlower(int c);

int ncstrcmp(char * a,
             char * b);

void findpapersize(void);

void paperspec(char * s,
               int hed);

char * epsftest(void);

void initprinter(int n);

void setup(void);

void cleanprinter(void);

void psflush(void);

void pageinit(void);

void pageend(void);

void drawrule(integer rw,
              integer rh);

void drawchar(chardesctype * c,
              int cc);

void tell_needed_fonts(void);

