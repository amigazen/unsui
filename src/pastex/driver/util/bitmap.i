/* Prototypes for functions defined in bitmap.c */


extern void ClearBitmap		Args((void));
extern int InitBitmap		Args((int pix_length,
	             	       	      int pix_width));
extern int PrintPage		Args((void));

#ifdef ATARI
extern void PaintImage		Args((char *pfilename, 
						  long xakt,
						  long yakt,
						  int maxwidth,
						  int maxheight));

#endif

#ifdef SLOW
extern void CopyBitArray	Args((long x,
						  long y,
						  int w,
						  int h,
						  unsigned short *p));

#endif

