/* Prototypes for functions defined in
dospecial.c
 */

void specerror(char * s);

void outbangspecials(void);

char Tolower(register int c);

int IsSame(char * a,
           char * b);

char * GetKeyVal(char * str,
                 int * tno);

void predospecial(integer numbytes,
                  Boolean scanning);

int maccess(char * s);

void dospecial(integer numbytes);

void fil2ps(char * task,
            char * iname);

