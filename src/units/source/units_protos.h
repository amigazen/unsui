/* Prototypes for functions defined in
units.c
 */

extern char * powerstring;

extern struct { char * uname; char * uval;  }  unittable[1000];

extern struct { char * prefixname; char * prefixval;  }  prefixtable[50];

extern char * NULLUNIT;

extern int unitcount;

extern int prefixcount;

char * dupstr(char * );

void readerror(int );

void readunits(char * );

void initializeunit(struct unittype * );

int addsubunit(char ** , char * );

void showunit(struct unittype * );

void zeroerror(void);

int addunit(struct unittype * , char * , int );

int compare(void const * , void const * );

void sortunit(struct unittype * );

void cancelunit(struct unittype * );

char * lookupunit(char * );

int reduceproduct(struct unittype * , int );

int reduceunit(struct unittype * );

int compareproducts(char ** , char ** );

int compareunits(struct unittype * , struct unittype * );

int completereduce(struct unittype * );

void showanswer(struct unittype * , struct unittype * );

void usage(void);

void main(int , char ** );

