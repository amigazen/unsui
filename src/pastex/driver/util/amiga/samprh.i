/* Prototypes for functions defined in amprhelp.c */
extern int InitPrinterDevice	Args((void));
extern void ClosePrinterDevice	Args((void));
extern void prnzero		Args((void));
extern void PrintRastPort	Args((struct RastPort *rp,
                		      struct ColorMap *colmap,
             			      unsigned long modes,
        	     		      unsigned short offx,
	             		      unsigned short offy,
             			      unsigned short sizex,
             			      unsigned short sizey,
       		      		      unsigned short in_x,
	             		      unsigned short in_y));
extern void EndPrintRastPort	Args((void));
