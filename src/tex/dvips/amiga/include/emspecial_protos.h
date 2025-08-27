/* Prototypes for functions defined in
emspecial.c
 */

void emclear(void);

struct empt * emptput(shalfword point,
                      integer x,
                      integer y);

struct empt * emptget(shalfword point);

double emunits(double width,
               char * unit);

void emspecial(char * p);

integer readinteger(FILE * f);

halfword readhalfword(FILE * f);

typedef struct 
{
	quarterword man;
	quarterword ver;
	quarterword enc;
	quarterword bitperpix;
	halfword xmin;
	halfword ymin;
	halfword xmax;
	halfword ymax;
	halfword hres;
	halfword vres;
	quarterword pal[48];
	quarterword reserved;
	quarterword colorplanes;
	halfword byteperline;
	halfword paltype;
	quarterword fill[58];
} PCXHEAD; 

int PCXreadhead(FILE * pcxf,
                PCXHEAD * pcxh);

int PCXreadline(FILE * pcxf,
                unsigned char * pcxbuf,
                unsigned int byteperline);

void PCXgetpalette(FILE * pcxf,
                   PCXHEAD * pcxh,
                   unsigned char * r,
                   unsigned char * g,
                   unsigned char * b);

void PCXshowpicture(FILE * pcxf,
                    int wide,
                    int high,
                    int bytes,
                    int cp,
                    int bp,
                    unsigned char * r,
                    unsigned char *g,
                    unsigned char * b);

void imagehead(char * filename,
               int wide,
               int high,
               double emwidth,
               double emheight);

void imagetail(void);

void pcxgraph(FILE * pcxf,
              char * filename,
              double emwidth,
              double emheight);

void MSP_2_ps(FILE * f,
              int wide,
              int high);

void MSP_1_ps(FILE * f,
              int wide,
              int high);

void mspgraph(FILE * f,
              char * filename,
              double emwidth,
              double emheight);

void rgbread(FILE * f,
             int w,
             int b,
             char * s);

void rle4read(FILE * f,
              int w,
              int b,
              char * s);

void rle8read(FILE * f,
              int w,
              int b,
              char * s);

void bmpgraph(FILE * f,
              char * filename,
              double emwidth,
              double emheight);

void emgraph(char * filename,
             double emwidth,
             double emheight);

