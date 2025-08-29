/* Prototypes for functions defined in
dvips.c
 */

void help(void);

void error(char * s);

char * mymalloc(integer n);

void morestrings(void);

void checkstrings(void);

void initialize(void);

char * newstring(char * s);

void newoutname(void);

void * revlist(void * p);

void queryargs(void);

void main(int argc,
          char ** argv);

