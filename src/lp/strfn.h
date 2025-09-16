/*
 *  STRFN.H
 *
 *  (c)Copyright 1991-92 by Tobias Ferber,  All Rights Reserved.
 */

#ifdef AMIGA
#include <exec/types.h>
#endif
#include <stdio.h>

#define _SLASH    '/'
#define _COLON    ':'
#define _PERIOD   '.'

extern char *strmfn();  /* make file name */
extern void strsfn();   /* split file name */
extern int stcgfd();    /* get file drive */
extern int stcgfp();    /* get file path w/ drive name */
extern int stcgfn();    /* get file name w/ extension(s) */
extern int stcgfe();    /* get last file extension */
extern void strmfe();   /* make file name with new extension */
extern void strmfp();   /* make filename: path (+ slash) + node */
