/* Prototypes for functions defined in
repack.c
 */

void was_putlong(register char * a,
                 long i);

long getlong(register unsigned char * a);

void dochar(unsigned char * from,
            int width,
            int height);

char * makecopy(register unsigned char * what,
                register long len,
                register unsigned char * p);

void repack(register chardesctype * cp);

