/* Prototypes for functions defined in amprhelp.c */

extern int InitPrinterDevice		Args((void));
extern void setup_printer_preferences	Args((UBYTE set_density));
extern void get_printer_resolution	Args((long *hres, long *vres, short *pwidth));

extern void ClosePrinterDevice		Args((void));
extern void prnzero			Args((void));
extern void PrinterError		Args((BYTE err));

#ifndef DISPLAY
extern void FormFeedGeneric	Args((void));
extern void HardcopyGeneric	Args((struct bitmap *bmap, long lineno, int draft, int last_pass));
#endif

#ifdef DISPLAY
extern void PrintRastPort	Args((struct RastPort *rp,
	             		      unsigned long modes,
             			      unsigned short sizex,
             			      unsigned short sizey,
        	     		      unsigned short in_x,
	             		      unsigned short in_y));
extern void EndPrintRastPort	Args((void));
#endif
