/* dir.c	15 May 1990 David G. Grubbs (-dgg-)

   It is intended to provide the 4BSD directory functions for AmigaDOS.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <libraries/dosextens.h>
#include <exec/memory.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include "dir.h"
#include "common.h"

DIR *opendir(char *n)
{
    BPTR f, p;
    DIR *d;
    char name[255];

    if (!n || !(d = (DIR *)AllocMem(sizeof(DIR),MEMF_PUBLIC|MEMF_CLEAR)))
	return(NULLDIR);

    strcpy(name,n);
    amigaizepath(name);
    /* Attempt to allow Users to specify Unix-style "." and ".." strings to
	open the current directory and the Parent directory.

	It won't hurt to leave it here, though "amigizepath" does it for us.
    */
    if (*name == '.' && (!*(name+1) || (*(name+1) == '.' && !*(name+2)))) {
	if (f = Lock("", ACCESS_READ)) {
	    if (*(name+1)) {		/* Must be ".." */
		p = (BPTR)ParentDir((struct FileLock *)f);
		UnLock(f);
		f = p;
	    }
	}
    } else
	f = Lock(name, ACCESS_READ);

    /* If it doesn't exist or it's not a directory, return NULLDIR */
    if (!f || !Examine(f,(BPTR)&(d->d_fib))||(d->d_fib.fib_DirEntryType < 0)) {
	if (f) UnLock(f);
	FreeMem(d, sizeof(DIR));
	return(NULLDIR);
    }
    d->d_lock = (struct FileLock *)f;
    d->d_max = -1;
    d->d_tell = 0;
    return(d);
}

/* The extra call to ExNext after a test for a null fib_FileName is due to an
   apparent RAM: disk bug that causes a bogus null directory entry.
*/
struct direct *readdir(DIR *d)
{
    static struct direct rtn;

    if (!d || !d->d_lock || !ExNext((BPTR)(d->d_lock),(BPTR)&(d->d_fib)))
	return(NULL);

    if (!d->d_fib.fib_FileName[0]) {
	if (!ExNext((BPTR)(d->d_lock),(BPTR)&(d->d_fib))
		|| !d->d_fib.fib_FileName[0])
	    return(NULL);
    }

    rtn.d_reclen = rtn.d_ino = 1;			/* Zero is special */
    strcpy(rtn.d_name, d->d_fib.fib_FileName);
    rtn.d_namlen = strlen(rtn.d_name);
    return(&rtn);
}

int closedir(DIR *d)
{
    int i;

    if (!d) {
	errno = EINVAL;
	return (-1);
    }
    for (i = 0; i <= d->d_max; i++)
	free(d->d_sfib[i]);		/* Never freed until close */

    if (d->d_lock)
	UnLock((BPTR)(d->d_lock));
    FreeMem(d, sizeof(DIR));
    return (0);
}

/* 
 * In Unix, the "telldir" return value used by "seekdir" may or may not be
 * useful across a "closedir".  Here they are really useless after a
 * "closedir" because they are allocated at opendir and freed on a closedir.
 */
long telldir(DIR *d)
{
    long rtn;
    int i;

    if (!d) return (-1);

    i = (rtn = d->d_tell) & TELLMASK;

    if (!(d->d_sfib[i]))
	d->d_sfib[i] = (struct FileInfoBlock *)malloc(sizeof(d->d_fib));
	
    *(d->d_sfib[i]) = d->d_fib;		/* structure copy */
    d->d_tell++; /* Ignore wrap -- assume 2 Billion telldir's Never happen */
    if (i > d->d_max) d->d_max = i;
    return (rtn);
}

void seekdir(DIR *d, long tell)
{
    int i = tell & TELLMASK;

    if (!d) return;
    if (tell < 0 ||			/* If really invalid "tell" */
	tell >= d->d_tell ||		/* If it has not yet been allocated */
	(tell & TELLMASK) > d->d_max ||	/* Early upper bound */
	tell < (d->d_tell - TELLSLOTS)) { /* Late lower bound */
	UnLock((BPTR)(d->d_lock));
	d->d_lock = 0;	/* Will fail on next readdir as in the BSD manual */
    } else
	d->d_fib = *(d->d_sfib[tell & TELLMASK]);  /* structure copy */
}

/* Rewind is just another kind of Seek, failure mode is similar */
void rewinddir(DIR *d)
{
    if (!d) return;
    if (!Examine((BPTR)(d->d_lock), (BPTR)&(d->d_fib))) {
	UnLock((BPTR)(d->d_lock));
	d->d_lock = 0;	/* Will fail on next readdir as in the BSD manual */
    }
}
