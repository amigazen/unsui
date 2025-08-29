/* Prototypes for functions defined in
fontdef.c
 */

fontdesctype * newfontdesc(integer cksum,
                           integer scsize,
                           integer dssize,
                           char * name,
                           char * area);

fontdesctype * matchfont(char * name,
                         char * area,
                         integer scsize,
                         char * scname);

void fontdef(int siz);

int skipnop(void);

