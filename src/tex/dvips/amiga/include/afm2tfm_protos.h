/* Prototypes for functions defined in
afm2tfm.c
 */

void error(register char * s);

int transform(register int x,
              register int y);

int getline(void);

int interest(char * s);

char * mymalloc(unsigned long len);

char * newstring(char * s);

char * paramnewstring(void);

char * paramstring(void);

int paramnum(void);

float paramfloat(void);

struct adobeinfo * newchar(void);

struct kern * newkern(void);

struct pcc * newpcc(void);

struct lig * newlig(void);

void expect(char * s);

void handlechar(void);

struct adobeinfo * findadobe(char * p);

void handlekern(void);

void handleconstruct(void);

void makeaccentligs(void);

void readadobe(void);

void handlereencoding(void);

struct adobeinfo * revlist(struct adobeinfo * p);

void assignchars(void);

void upmap(void);

void write16(register int what);

void writearr(register long * p,
              register int n);

void makebcpl(register long * p,
              register char * s,
              register int n);

int mincover(long * what,
             register long d);

void remap(long * what,
           int oldn,
           int newn);

long checksum(void);

long scale(long what);

void buildtfm(void);

void writesarr(long * what,
               int len);

void writetfm(void);

int texheight(register struct adobeinfo * ai);

void vlevout(void);

void vlevnlout(void);

void vleft(void);

void vright(void);

char * vchar(int c);

void writevpl(void);

void usage(FILE * f);

void openfiles(int argc,
               char ** argv);

struct kern * rmkernmatch(struct kern * k,
                          char * s);

void rmkern(char * s1,
            char * s2,
            struct adobeinfo * ai);

void checkligkern(char * s);

char * gettoken(void);

void getligkerndefaults(void);

struct encoding * readencoding(char * enc);

void conspsfonts(void);

void main(int argc,
          char ** argv);

