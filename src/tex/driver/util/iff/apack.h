#ifndef APACK_H
#define APACK_H

/* This macro computes the worst case packed size of a "row" of bytes. */
#define MaxPackedSize(rowSize)  ( (rowSize) + ( ((rowSize)+127) >> 7 ) )

/* Given POINTERS to POINTER variables, packs one row, updating the source
 * and destination pointers. Returns the size in bytes of the packed row.
 * ASSUMES destination buffer is large enough for the packed row.
 * See MaxPackedSize. */
extern __stdargs LONG packrow(BYTE **, BYTE **, LONG);
		/*  pSource, pDest,   rowSize */

#endif

