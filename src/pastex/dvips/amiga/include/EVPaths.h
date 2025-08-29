#ifndef EVPATHS_H
#define EVPATHS_H
/*
** $VER:  EVPaths.h 1.36 (14.12.94)
**
** Header for EVPaths Routines.
**
** Written by Giuseppe Ghibò
**
** Copyright © 1994 by Giuseppe Ghibò
**
*/
#include <exec/types.h>
#include <dos/dos.h>

struct EnvVarPath
{
	STRPTR name;

	union {
		STRPTR *strings;
		UBYTE *buffer;
	} storage;

        STRPTR defpath;
	LONG size;
	LONG status;
	LONG pos;
};

/* Prototypes */
GLOBAL LONG	__stdargs SNPrintf(STRPTR String, LONG Size, STRPTR FmtString, ...);
GLOBAL LONG	__regargs GetVarLength(STRPTR envname);

GLOBAL struct EnvVarPath __regargs *Alloc_EnvVarPath(STRPTR varname, LONG size);
GLOBAL VOID		__regargs Free_EnvVarPath(struct EnvVarPath *p);
GLOBAL VOID		__regargs Init_EnvVarPath(struct EnvVarPath *p, APTR deflt_str, LONG mode);
GLOBAL STRPTR		__regargs EVP_FileSearch(STRPTR filename, struct EnvVarPath *evp, UBYTE *buffer, LONG size);
BPTR			__regargs EVP_Open(STRPTR filename, struct EnvVarPath *evp, UBYTE *buffer, LONG size, LONG mode);

#define ENVPATH_DEFSTR 		(1L << 0)	/* default path provided as string */
#define ENVPATH_DEFARR 		(1L << 1)	/* default path provided as array of strings */
#define ENVPATH_PREPEND_PATH	(1L << 31)	/* Prepend the default path */
#define ENVPATH_APPEND_PATH	(1L << 30)	/* Append the default path */

#define ENVPATH_BUFFER_EMPTY		0L /* buffer is empty */
#define ENVPATH_BUFFER_COMPLETE		1L /* buffer is complete, i.e. no more entries */
#define ENVPATH_BUFFER_FULL		2L /* buffer is full, i.e. not enough room for other entries */
#define ENVPATH_BUFFER_TRUNC		3L /* buffer is truncated because a not enough memory was encountered during processing */
#define ENVPATH_BUFFER_INCOMPLETE	4L /* there could be other entries, but actually aren't into the buffer */

#endif	/* EVPATHS_H */
