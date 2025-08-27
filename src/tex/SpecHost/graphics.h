/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heﬂmann
*/

/* graphics.h */

void device_SetPenSize	(long xpen, long ypen);
void device_DrawPoint	(long x, long y);
void device_DrawLine	(long x, long y, long x1, long y1);


struct bitmap {
  long width, height;
  long *pixptr;
 };

extern struct bitmap map;
extern long upper_limit;
extern long lower_limit;

