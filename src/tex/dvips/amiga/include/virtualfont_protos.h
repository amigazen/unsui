/* Prototypes for functions defined in
virtualfont.c
 */

void badvf(char * s);

shalfword vfbyte(void);

integer vfquad(void);

integer vftrio(void);

Boolean vfopen(register fontdesctype * fd);

fontmaptype * vfontdef(integer s,
                       int siz);

Boolean virtualfont(register fontdesctype * curfnt);

