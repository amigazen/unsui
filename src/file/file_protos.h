/* Prototypes for functions defined in
file.c
 */

extern BPATTERN bmagic[];

extern PATTERN amagic[];

extern PATTERN asearch[];

extern PATTERN IFFforms[];

extern PATTERN compress;

extern PATTERN zoo;

int main(int , char **);

void type(char *, char *);

int memncmp(char *, char *, int );

char * strrpbrk(char *, char *);

char * basename(char *);

void filetype(char *, char *);

void dofile(char *, char *, FILEINFOBLOCK *);

BOOLEAN istextfile(UBYTE *, int );

void matchtype(char *, char *, char *, FILEINFOBLOCK *);

BOOLEAN search(char *, char *, int );

void usage(char *);

