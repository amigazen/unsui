/*
 * EVPaths.c - Environment Variables Path Routines for TeX related programs.
 *
 * Written by Giuseppe Ghibò
 *
 * © 1994 by Giuseppe Ghibò <ghibo@galileo.polito.it>
 *
 * Version 1.2 (09-Nov-94)
 *
*/

#include "evpaths.h"
#include <exec/memory.h>
#include <dos/dosextens.h>
#include <proto/exec.h>
#include <proto/dos.h>
#define USE_BUILTIN_MATH
#include <string.h>

extern struct DosLibrary *DOSBase;

#define ENVPATH_OK		0L	/* Success */
#define ENVPATH_NOT_ENOUGH_ROOM	1L	/* Array was truncated */
#define ENVPATH_VAR_NOT_FOUND	2L	/* There is no env var */
#define ENVPATH_NOT_ENOUGH_MEM	3L	/* Not enough mem to alloc */

#define ENVPATH_SCAN_NONE	0L	/* No dirs scan */
#define ENVPATH_SCAN_ONE	1L	/* Scan one level subdirs */
#define ENVPATH_SCAN_ALL	2L	/* Scan recursively all subdirs */
#define ENVPATH_SCAN_VAR	3L	/* Scan recursively another var */
#define ENVPATH_SCAN_DEFPATH	4L	/* Put default path */

#define ENVPATH_GETVAR		0L	/* Get var */
#define ENVPATH_DONTGETVAR	1L	/* Don't get var */

/* Prototypes */
STATIC LONG	__regargs GetVar13(STRPTR name, UBYTE *buf, LONG size);
STATIC VOID 	__regargs GetEnvPath(STRPTR envname, UBYTE *buffer, LONG size, LONG *Error, LONG mode);
STATIC STRPTR 	__regargs Super_Strtok(STRPTR s, STRPTR p1, STRPTR p2, UBYTE open_delim, UBYTE close_delim);
STATIC LONG	__regargs Add_Path_String(STRPTR s, STRPTR *p, LONG size);
STATIC STRPTR	__regargs Parse_Path_String(STRPTR s, LONG *scan_status);
STATIC LONG	__regargs Scan_Path_String(BPTR dirlock, STRPTR name, BOOL recursion, STRPTR *buf, LONG size);
STATIC VOID	__regargs Insert_Path(struct EnvVarPath *p, APTR path, LONG mode);

#define PATH_TMPLEN 256
STATIC UBYTE Path_TmpStr[PATH_TMPLEN]; /* temporary storage */

/* Super_Strtok() is similar to strtok(). It breaks the string s if
 * one or more char from the string p1 is matched. The string s is also
 * breaked if one or more char from the string p2 is matched, but only
 * if such char is found outside the string delimited by
 * open_delim, close_delim. For instance if
 *
 * s = "one,two,three,four,<fi,ve>\nsix,seven" ;
 * p1 = "\n" ;
 * p2 = "," ;
 * open_delim = '<' ;
 * close_delim = '>' ;
 *
 * Then after each call to Super_Strtok, we obtain respectively the
 * following strings:
 *
 *	one	two	three	four	<fi,ve>
 *	six	seven
 *
 */
STATIC STRPTR last;
STATIC STRPTR __regargs Super_Strtok(STRPTR s, STRPTR p1, STRPTR p2, UBYTE open_delim, UBYTE close_delim)
{
	STRPTR p, s1, s2, token;
	BOOL opened = FALSE;

	if (s == NULL && (s = last) == NULL)
		return (NULL);

	s += strspn(s, p1);
	s += strspn(s, p2);

	if (*s == '\0')
	{
		last = NULL;
		return (NULL);
	}

	token = s;
	s1 = s + strcspn(s, p1);
	s2 = s + strcspn(s, p2);

	for (p = s; *p && p < s2; p++)
	{
		if (*p == open_delim)
		{
			if (open_delim == close_delim && opened)
				opened = FALSE;
			else
				opened = TRUE;
		}
		else
		{
			if (*p == close_delim)
				opened = FALSE;
		}
	}

	if (opened)
	{
		for (p = s2; *p && p < s1 && *p++ != close_delim; ) ;
		s = p;
	}
	else
		s = (s1 < s2 ? s1 : s2);

	if (*s == '\0') {
		last = NULL;
	}
	else
	{
		*s = '\0';
		last = s + 1;
	}

	return(token);
}

/*
 * GetVarLength() returns the length of an environment variable. If the
 * given env var is not found or it doesn't exists, then the returned
 * value is 0L.
 */
LONG __regargs GetVarLength(STRPTR envname)
{
	REGISTER LONG len;

	if (DOSBase->dl_lib.lib_Version >= 37)
	{
		UBYTE buffer[1];

		if ((len = GetVar(envname, buffer, 1L, GVF_GLOBAL_ONLY | GVF_BINARY_VAR)) != -1L)
			len = IoErr();
		else
			len = 0L;
	}
	else
	{
		struct FileInfoBlock *fib;
		BPTR lock;

		SNPrintf(Path_TmpStr, PATH_TMPLEN, "ENV:%s", envname);

		if (lock = Lock(Path_TmpStr, ACCESS_READ))
		{
			if ((fib = AllocMem(sizeof(struct FileInfoBlock), MEMF_ANY)))
			{
				if (Examine(lock, fib))
				{
					if (fib->fib_DirEntryType < 0L)
						len = fib->fib_Size;
					else
						len = 0L;
				}
				else
					len = 0L;

				FreeMem(fib, sizeof(struct FileInfoBlock));
			}
			else
				len = 0L;

			UnLock(lock);
		}
		else
			len = 0L;
	}

	return (len);
}

/*
 * Given an env var name, GetVar13() copies the contents of the env var into
 * the buffer buf. If the size of the env var is greater than the size of
 * the buffer, then the copied length will be size-1.
 * The buffer is terminated with '\0'.
 * The returned value is the number of the bytes copied into the buffer,
 * without consider the trailing '\0'.
 * If the environment variable doesn't exists, no action is performed and
 * the returned value is 0L.
 * Note that this function is similar to the GetVar() function of OS2.1,
 * but has not the same behaviour. In fact GetVar() returns -1L if the var
 * doesn't exists, while GetVar13() returns 0L in such case.
 */
STATIC LONG __regargs GetVar13(STRPTR name, UBYTE *buf, LONG size)
{
	LONG len;

	SNPrintf(Path_TmpStr, PATH_TMPLEN, "ENV:%s", name);

	len = min(GetVarLength(name), size);

	if (len > 0L)
	{
		BPTR fh;

		if ((fh = Open(Path_TmpStr, MODE_OLDFILE)) == DOSFALSE)
			return(0L);

		if ((len = Read(fh, buf, len)) > 0L)
		{
			Close(fh);

			if (size <= len)
				len = size - 1L;

			buf[len] = '\0';
			return(len);
		}
		else
		{
			Close(fh);
			return(0L);
		}
	}
	else
		return(0L);
}

#define ABSTOKEN "\012\014" 	/* breaks only on line feed or form feed */
#define RELTOKEN "\t\f\040,;"
#define LQUOTE '\042'
#define RQUOTE '\042'
#define QUOTE  '\042'

/*
 * GetEnvPath() processes the path of a given environment variable. The
 * passed buffer is then fitted with path elements. Such buffer may be
 * accessed as an array of string pointers.
 * The 'mode' is either one of EVPATH_DONTGETVAR or EVPATH_GETVAR. If
 * EVPATH_DONTGETVAR is specified then the 'envname' is considered a
 * pointer to a path string, and paths element are treated as they were read
 * from an environment variable.
 *
 * The *Error contains (on exit) one of these values:
 * 
 *  -  ENVPATH_OK: all was successful.
 *
 *  -  ENVPATH_NOT_ENOUGH_ROOM: the buffer is full. In this case the
 *     buffer contains entries which fit into the buffer itself, i.e.
 *     no truncated entries.
 *
 *  -  ENVPATH_VAR_NOT_FOUND: the given environment variable isn't
 *     found or has zero length.
 *
 *  -  ENVPATH_NOT_ENOUGH_MEM: there was a not enough mem during processing.
 *
 */

#define MAX_RECURS_VAR 5
STATIC VOID __regargs GetEnvPath(STRPTR envname, UBYTE *buffer, LONG size, LONG *Error, LONG mode)
{
	STRPTR	envbuf, *p = (STRPTR *)buffer, s;
	LONG	envbuflen, scan_status = ENVPATH_SCAN_NONE, len;
	STATIC LONG recurs_level = 0L;
	STATIC STRPTR env_names[MAX_RECURS_VAR + 1];

	recurs_level++;

	if (recurs_level == 1L)
	{
		*Error = ENVPATH_OK;
	}

	if (mode == ENVPATH_DONTGETVAR && recurs_level == 1)
	{
		if (envname)
			envbuf = envname;
		else
		{
			*Error = ENVPATH_VAR_NOT_FOUND;
			--recurs_level;
			return;
		}
	}
	else
	{
		envbuflen = GetVarLength(envname) + 1L;

		if (envbuflen <= 1L)
		{
			*Error = ENVPATH_VAR_NOT_FOUND;
			--recurs_level;
			return;
		}

		if (envbuf = AllocMem(envbuflen, MEMF_ANY | MEMF_CLEAR))
		{
			if (DOSBase->dl_lib.lib_Version >= 37)
				GetVar(envname, envbuf, envbuflen, GVF_GLOBAL_ONLY | GVF_BINARY_VAR);
			else
				GetVar13(envname, envbuf, envbuflen);
		}
		else
		{
			*Error = ENVPATH_NOT_ENOUGH_MEM;
			--recurs_level;
			return;
		}

		len = strlen(envname) + 1L;

		if (env_names[recurs_level - 1] = AllocMem(len, MEMF_ANY))
		{
			strcpy(env_names[recurs_level - 1], envname); /* store env name to avoid recursion loops */
			env_names[recurs_level] = NULL;
		}
		else
		{
			*Error = ENVPATH_NOT_ENOUGH_MEM;
			--recurs_level;
			return;
		}
	}

	s = Super_Strtok(envbuf, ABSTOKEN, RELTOKEN, LQUOTE, RQUOTE); /* get first token */

	do
	{
		s = Parse_Path_String(s, &scan_status);

		if (scan_status == ENVPATH_SCAN_VAR)
		{
			if (recurs_level < MAX_RECURS_VAR)
			{
				BOOL recursion = TRUE;
				LONG i = 0L;

				while (env_names[i])
				{
					if (!strcmp(env_names[i++], s))
					{
						recursion = FALSE;
						break;
					}
				}

				if (recursion)
				{
					GetEnvPath(s, buffer, size, Error, ENVPATH_GETVAR);

					if (*Error == ENVPATH_VAR_NOT_FOUND)
						*Error = ENVPATH_OK; /* ignore recurs vars not found */
				}
			}
		}
		else if (scan_status == ENVPATH_SCAN_ONE || scan_status == ENVPATH_SCAN_ALL)
		{
			BPTR dirlock;

			if (dirlock = Lock(s, ACCESS_READ))
			{
				*Error = Add_Path_String(s, p, size);

				if (*Error == ENVPATH_OK)
					*Error = Scan_Path_String(dirlock, s, (scan_status == ENVPATH_SCAN_ALL ? TRUE : FALSE), p, size);

				UnLock(dirlock);
			}
		}
		else
			*Error = Add_Path_String(s, p, size);
	}
	while (*Error == ENVPATH_OK && (s = Super_Strtok(NULL, ABSTOKEN, RELTOKEN, LQUOTE, RQUOTE)));

	if (mode != ENVPATH_DONTGETVAR)
		FreeMem(env_names[recurs_level - 1], len);

	if (envbuf && mode != ENVPATH_DONTGETVAR)
		FreeMem(envbuf, envbuflen);

	--recurs_level;
}

/*
 * Add_Path_String(). Adds the string s to the array of strings p.
 * The array must be initialized before to be passed to this function: the
 * first element must be NULL! (p[0] = NULL).
 *
 * Returned values: ENVPATH_OK or ENVPATH_NOT_ENOUGH_ROOM.
 *
 */
STATIC LONG __regargs Add_Path_String(STRPTR s, STRPTR *p, LONG size)
{
	LONG i = 0L, len;
	STRPTR q;

	if (size < (2 * sizeof(STRPTR) + 1L))
		return (ENVPATH_NOT_ENOUGH_ROOM);

	len = strlen(s);

	if (p[0] == NULL)
	{
		q = (STRPTR)p + size - (len + 1L);
	}
	else
	{
		for (i = 0L; p[i]; i++);
		q = p[i-1] - (len + 1L);
	}

	if (&p[i] > (STRPTR *)(q - 2*sizeof(STRPTR)))
	{
		p[i] = NULL;
		return (ENVPATH_NOT_ENOUGH_ROOM);
	}
	else
	{
		LONG j;
		BOOL is_valid_entry = TRUE;

		for (j = 0L; j < i; j++)
		{
			if (!stricmp(p[j], s))
			{
				is_valid_entry = FALSE;
			}
		}

		if (is_valid_entry)
		{
			p[i] = q;
			strcpy(p[i], s);
			p[i+1] = NULL;
		}
	}

	return (ENVPATH_OK);
}

#define MAX_RECURS_DIR 10L	/* maximum recursion dir level */
STATIC LONG __regargs Scan_Path_String(BPTR dirlock, STRPTR name, BOOL recursion, STRPTR *buf, LONG size)
{
	struct FileInfoBlock *fib;
	LONG result = ENVPATH_OK;
	STATIC LONG recurs_level = 0L;

	recurs_level++;

	if ((fib = AllocMem(sizeof(struct FileInfoBlock), MEMF_ANY)))
	{
		if (Examine(dirlock, fib))
		{
			if (fib->fib_DirEntryType > 0L)
			{
				while (ExNext(dirlock, fib) && (result == ENVPATH_OK))
				{
					if (fib->fib_DirEntryType > 0L)
					{
						LONG len = strlen(name);

						if (len > 0L)
							SNPrintf(Path_TmpStr, PATH_TMPLEN, "%s%s%s", name,
								 ((name[len-1] != '/' && name[len-1] != ':') ? "/" : ""), fib->fib_FileName);
						else
							SNPrintf(Path_TmpStr, PATH_TMPLEN, "%s", fib->fib_FileName);

						result = Add_Path_String(Path_TmpStr, buf, size);

						if (recursion && (result == ENVPATH_OK) && (recurs_level < MAX_RECURS_DIR))
						{
							BPTR OldDirLock, NewDirLock;

							OldDirLock = CurrentDir(dirlock);

							if (NewDirLock = Lock(Path_TmpStr, ACCESS_READ))
							{
								STRPTR dirname;
								LONG len = strlen(Path_TmpStr) + 1L;

								if (dirname = AllocMem(len, MEMF_ANY))
								{
									strcpy(dirname, Path_TmpStr);
									result = Scan_Path_String(NewDirLock, dirname, recursion, buf, size);
									FreeMem(dirname, len);
								}
								UnLock(NewDirLock);
							}
							CurrentDir(OldDirLock);
						}
					}
				}
			}
		}
		FreeMem(fib, sizeof(struct FileInfoBlock));
	}
	else
		result = ENVPATH_NOT_ENOUGH_MEM;

	--recurs_level;
	return (result);
}


STATIC STRPTR __regargs Parse_Path_String(STRPTR s, LONG *scan_status)
{
	UBYTE *c;
	LONG len;

	*scan_status = ENVPATH_SCAN_NONE;

	if (!s || !*s)
		return("");

	if (s[0] == LQUOTE)
		s++;		/* remove leading quote */

	for (c = s; *c; c++);	/* seek to the end of the string */

	if (*--c == RQUOTE)
		*c = '\0';	/* remove trailing quote */

	if (s[0] == '.')	/* '.' or './' represents current dir */
	{
		switch (s[1])
		{
			case '\0':
				s[0] = '\0';	/* null string is current dir */
				break;

			case '/':
				if (s[2])
					s = &s[2];
				else
					s[0] = '\0';

				break;

			case '.':	/* '../' is also parent dir */
				if (s[2] == '/')
					s = &s[2];
				
				break;
		}
	}

	len = strlen(s);

	if (len > 0)
	{
		if (s[0] == '$')
		{
			s++;
			*scan_status = ENVPATH_SCAN_VAR;
			if (s[0] == QUOTE)
				s++;
		}
		else if (s[0] == '?' && s[1] == '\0')
		{
			s++;
			*scan_status = ENVPATH_SCAN_DEFPATH;
		}
		else
		{
			switch (s[len - 1])
			{
				case '*':
					if (len > 1)
					{
						switch (s[len - 2])
						{
							case '*':
								*scan_status = ENVPATH_SCAN_ALL;
								s[len - 2] = '\0';
								break;

							default:
								*scan_status = ENVPATH_SCAN_ONE;
								s[len - 1] = '\0';
								break;
						}
					}
					else /* len == 1 */
					{
						*scan_status = ENVPATH_SCAN_ONE;
						s[len - 1] = '\0';
					}

					break;

				case '?':
					if (len > 1)
					{
						switch (s[len - 2])
						{
							case '#':
								*scan_status = ENVPATH_SCAN_ONE;
								s[len - 2] = '\0';
								break;

							default:
								*scan_status = ENVPATH_SCAN_NONE;
								break;
						}
					}
					else
						*scan_status = ENVPATH_SCAN_NONE;

					break;

				case '>':
					if (len > 1)
					{
						switch (s[len - 2])
						{
							case '*':
								*scan_status = ENVPATH_SCAN_ALL;
								s[len - 2] = '\0';
								break;

							case '?':
								if (len > 2)
								{
									switch (s[len - 3])
									{
										case '#':
											*scan_status = ENVPATH_SCAN_ALL;
											s[len -3] = '\0';
											break;

										default:
											*scan_status = ENVPATH_SCAN_NONE;
											break;
									}
								}
								else
									*scan_status = ENVPATH_SCAN_NONE;

								break;

							default:
								*scan_status = ENVPATH_SCAN_NONE;
								break;
						}
					}
					else
						*scan_status = ENVPATH_SCAN_NONE;

					break;

				default:
					*scan_status = ENVPATH_SCAN_NONE;
					break;
			}
		}
	}

	return(s);
}

/*
 * Alloc_EnvVarPath(). Alloc an EnvVarPath structure.
 *
 */
struct EnvVarPath __regargs *Alloc_EnvVarPath(STRPTR varname, LONG size)
{
	struct EnvVarPath *p;

	if (p = AllocMem(sizeof(struct EnvVarPath), MEMF_ANY))
	{
		if ((size >= 2*sizeof(STRPTR) + 1L) && (p->storage.buffer = AllocMem(size, MEMF_ANY)))
		{
			if (p->name = AllocMem(strlen(varname) + 1, MEMF_ANY))
			{
				strcpy(p->name, varname);
				p->storage.strings[0] = NULL; /* init first entry */
				p->size = size;
				p->status = ENVPATH_BUFFER_EMPTY;
				p->defpath = NULL;
				p->pos = 0L;
			}
			else
			{
				FreeMem(p->storage.buffer, size);
				FreeMem(p, sizeof(struct EnvVarPath));
				p = NULL;
			}
		}
		else
		{
			FreeMem(p, sizeof(struct EnvVarPath));
			p = NULL;
		}
	}

	return(p);
}

/*
 * Free_EnvVarPath. Frees an allocated EnvVarPath structure.
 *
 */
VOID __regargs Free_EnvVarPath(struct EnvVarPath *p)
{
	if (p)
	{
		if (p->name) FreeMem(p->name, strlen(p->name) + 1);
		if (p->storage.buffer) FreeMem(p->storage.buffer, p->size);
		if (p->defpath)	FreeMem(p->defpath, strlen(p->defpath) + 1);
		FreeMem(p, sizeof(struct EnvVarPath));
		
	}
}

/*
 * Init_EnvVarPath() inits an EnvVarPath structure provided by
 * Alloc_EnvVarPath().
 *
 * deflt_path is an array of string pointers or a string pointer to be used
 * if the specified environment variable doesn't exists or has zero length.
 * 'mode' is one of ENVPATH_DEFSTR or ENPATH_DEFARR. If it is ENVPATH_DEFSTR
 * then deflt_path is considered a string pointer, otherwise ENVPATH_DEFARR
 * specifies that deflt_path is an array of string pointers.
 *
 */
VOID __regargs Init_EnvVarPath(struct EnvVarPath *p, APTR deflt_path, LONG mode)
{
	if (p)
	{
		LONG Error;

		if ((mode & ENVPATH_PREPEND_PATH) == ENVPATH_PREPEND_PATH)
			Insert_Path(p, deflt_path, mode);

		GetEnvPath(p->name, p->storage.buffer, p->size, &Error, ENVPATH_GETVAR);

		if ((mode & ENVPATH_APPEND_PATH) == ENVPATH_APPEND_PATH && (mode & ENVPATH_PREPEND_PATH) != ENVPATH_PREPEND_PATH && Error == ENVPATH_OK)
			Insert_Path(p, deflt_path, mode);

		switch (Error)
		{
			case ENVPATH_OK:
				p->status = ENVPATH_BUFFER_COMPLETE;
				break;

			case ENVPATH_NOT_ENOUGH_ROOM:
				p->status = ENVPATH_BUFFER_FULL;
				break;

			case ENVPATH_NOT_ENOUGH_MEM:
				p->status = ENVPATH_BUFFER_TRUNC;
				break;

			case ENVPATH_VAR_NOT_FOUND:
				if (!(mode & (ENVPATH_PREPEND_PATH | ENVPATH_APPEND_PATH)))
					Insert_Path(p, deflt_path, mode);
				break;

			default:
				p->status = ENVPATH_BUFFER_EMPTY;
				break;
		}
	}
}

/*
   Insert_Path() adds a path to the p->buffer; mode is one of
   ENVPATH_DEFARR or ENVPATH_DEFSTR, as in the function Init_EnvVarPath().
*/
STATIC VOID __regargs Insert_Path(struct EnvVarPath *p, APTR path, LONG mode)
{
	LONG Error = ENVPATH_OK;

	if (path)
	{
		if ((mode & ENVPATH_DEFARR) == ENVPATH_DEFARR)
		{
			LONG i = 0L, l = 0L;
			STRPTR *array = (STRPTR *) path, BufTmp, s;

			while (array[i])
				l += strlen(array[i++]) + 1L;

			if (BufTmp = AllocMem(++l, MEMF_ANY))
			{
				s = BufTmp;
				*s = '\0';
				i = 0L;

				while (array[i])
				{
					strcat(s, array[i++]);
					strcat(s, " ");
				}

				GetEnvPath(BufTmp, p->storage.buffer, p->size, &Error, ENVPATH_DONTGETVAR);
				FreeMem(BufTmp, l);
			}
			else
				Error = ENVPATH_NOT_ENOUGH_MEM;
		}
		else
			GetEnvPath(path, p->storage.buffer, p->size, &Error, ENVPATH_DONTGETVAR);

		switch(Error)
		{
			case ENVPATH_OK:
				p->status = ENVPATH_BUFFER_COMPLETE;
				break;

			case ENVPATH_NOT_ENOUGH_ROOM:
				p->status = ENVPATH_BUFFER_FULL;
				break;

			case ENVPATH_NOT_ENOUGH_MEM:
				p->status = ENVPATH_BUFFER_TRUNC;
				break;

			case ENVPATH_VAR_NOT_FOUND:
				p->status = ENVPATH_BUFFER_EMPTY;
				break;

			default:
				p->status = ENVPATH_BUFFER_EMPTY;
				break;
		}
	}
	else
		p->status = ENVPATH_BUFFER_EMPTY;
}

/*
 * Given a pointer to an initialized structure EnvVarPath, a filename
 * and a buffer, EVP_FileSearch() returns the string pointer to the
 * complete filename found. The returned string pointer is same of buffer.
 * If the file isn't found then NULL is returned and buffer contains the
 * empty string ("").
 *
 */
STRPTR __regargs
EVP_FileSearch(STRPTR filename, struct EnvVarPath *evp, UBYTE *buffer, LONG size)
{
	LONG i = 0L, len ;

	if (evp && buffer && size > 0L && filename && *filename)
	{
		*buffer = '\0';

		while (evp->storage.strings[i])
		{
			BPTR fh;

			if (DOSBase->dl_lib.lib_Version >= 37)
			{
				SNPrintf(Path_TmpStr, PATH_TMPLEN, "%s", evp->storage.strings[i]);
				AddPart(Path_TmpStr, filename, PATH_TMPLEN);
				len = strlen(Path_TmpStr);
			}
			else
			{
				LONG l = strlen(evp->storage.strings[i]);

				if (l > 0L && !strchr(filename, ':'))
				{
					UBYTE c = evp->storage.strings[i][l - 1];

					len = SNPrintf(Path_TmpStr, PATH_TMPLEN, "%s%s%s", evp->storage.strings[i],
						((c != '/' && c != ':') ? "/" : ""), filename);
				}
				else
					len = SNPrintf(Path_TmpStr, PATH_TMPLEN, "%s", filename);
			}

			if (Path_TmpStr[0] && (fh = Open(Path_TmpStr, MODE_OLDFILE)) != DOSFALSE)
			{
				/* note that the Lock() function would be faster than Open() to see if
                                   a file exists or not, but in such case we should have to call
                                   Examine() to discriminate files from directories. */

				Close(fh);
				memcpy(buffer, Path_TmpStr, min(len, size));
				buffer[min(len, size - 1)] = '\0';
			}

			if (*buffer)
				break;
			else
				i++;
		}
	}
	else
	{
		if (size > 0L)
			buffer[0] = '\0';
	}

	if (*buffer)
		return(buffer);
	else
		return(NULL);
}
