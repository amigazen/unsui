/*
 *  IO.H
 *
 *  (c)Copyright 1991-92 by Tobias Ferber,  All Rights Reserved.
 */

#include <stdio.h>

#ifdef AMIGA
#include <exec/types.h>
#include <libraries/dosextens.h>

extern struct DosLibrary *DOSBase;

#ifndef DLT_UNKNOWN
#define DLT_UNKNOWN -1L /* device not found (not mounted) */
#endif

#define isdev(x) (dltype(x)==DLT_DEVICE)
#define isvol(x) (dltype(x)==DLT_VOLUME)
#define isdir(x) (dltype(x)==DLT_DIRECTORY)

extern long dltype();   /* get the device list type */
#endif /* AMIGA */

extern BOOL fexist();   /* check if a file exists */
