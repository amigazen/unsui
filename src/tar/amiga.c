#include <exec/types.h>
#include <libraries/dos.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#define NARGS		/* get it to shut up about my mkdir */
#include <stdio.h>
#include "tar.h"

umask(int mask)
{
    return (0);
}

stat(char *filename, struct stat * statbuf)
{
    struct FileLock *alock, *parent;
    struct FileInfoBlock *fib;
    int modes;
    extern long timezone;

    if ((!statbuf) || (!filename))
    {
	errno = EFAULT;
	return (-1);
    }
    if (alock = Lock(filename, ACCESS_READ))
    {
	if ((fib = (struct FileInfoBlock *) malloc(sizeof(*fib))) == NULL)
	{
	    UnLock(alock);
	    errno = ENOMEM;
	    return (-1);	/* malloc failed */
	}
	Examine(alock, fib);
	if (!(parent = ParentDir(alock)))
	{
	    statbuf->st_mode = 0700;
	    statbuf->st_prot = ~0xf;
	}
	else
	{
	    UnLock(parent);
	    modes = (~fib->fib_Protection >> 1) & 0x7;
	    statbuf->st_mode = (modes << 6);
	    statbuf->st_prot = fib->fib_Protection;
	}
	if (fib->fib_EntryType > 0)
	    statbuf->st_mode |= S_IFDIR;
	else
	    statbuf->st_mode |= S_IFREG;
	statbuf->st_dev = 0;
	statbuf->st_ino = 0;
	statbuf->st_size = fib->fib_Size;
	statbuf->st_rdev = fib->fib_DiskKey;
	statbuf->st_gid = 0;
	statbuf->st_uid = 0;

    /*
     * getft() doesn't compensate for time zones.
     */
	statbuf->st_mtime = getft(filename) + timezone;
	statbuf->st_nlink = 1;

    /*
     * These next fields really only exist for Amiga tar's benefit
     */
	strcpy(statbuf->st_comment, fib->fib_Comment);
	memcpy((char *) &(statbuf->st_date), (char *) &(fib->fib_Date),
	       sizeof(statbuf->st_date));
	free(fib);
	UnLock(alock);
	return (0);
    } else
    {
	errno = ENOENT;
	return (-1);		/* couldn't access file */
    }
}


/*
 * Convert Amiga style path to UNIX style. Does the following conversions:
 * ull string => ./ leading /   => ../
 */
char *
cvtAmi2UNIX(char *src, char *dst)
{
    char *dstp,
    *srcp;

    if (!*src)
    {
	strcpy(dst, "./");
	return (dst);
    }
    dstp = dst;
    srcp = src;
/*
 * Each leading / converts to ../
 */
    while (*srcp == '/')
    {
	*dstp++ = '.';
	*dstp++ = '.';
	*dstp++ = '/';
	srcp++;
    }
    while (*dstp++ = *srcp++)	/* copy rest of chars */
	;
    return (dst);
}

/*
 * Convert UNIX style path to Amiga style. Does the following conversions:
 * /  => null string ../ => /
 */
cvtUNIX2Ami(char *dst)
{
    char *dstp,
    *srcp,
     src[NAMSIZ + 2];

    strcpy(src, dst);
    dstp = dst;
    srcp = src;

    while (*srcp == '.')
    {
	if (*(srcp + 1) == '/')
	    srcp += 2;		/* ./ gets skipped */
	else if ((*(srcp + 1) == '.') && (*(srcp + 2) == '/'))
	{
	    srcp += 3;		/* ../ turns into / */
	    *dstp++ = '/';
	} else
	    break;
    }
    while (*dstp++ = *srcp++)	/* copy rest of chars */
	;
    return (dst);
}


/*
 * Wrapper for SetComment.  "Smart" because it knows how to handle "", and
 * handle case if current directory is root.  But not so smart it can handle
 * case when we're sitting in a directory it wants to SetComment on other than
 * "".
 */
SmartSetComment(char *filename, char *comment)
{
    extern int SetComment();

    if (!*comment)
	return(0);
    else
	return(SmartDoSomething(filename, (int) comment, SetComment,
				"SetComment"));
}


/*
 * Wrapper for SetProtection.  "Smart" because it knows how to handle "", and
 * handle case if current directory is root.  But not so smart it can handle
 * case when we're sitting in a directory it wants to SetProtection on other
 * than "".
 */
SmartSetProtection(char *filename, int protection)
{
    extern int SetProtection();

    return(SmartDoSomething(filename, protection, SetProtection,
			    "SetProtection"));
}


SmartDoSomething(char *filename, int arg, int (*doSomething)(), char *funcstr)
{
    struct FileLock *lock = 0, *parentLock;
    struct FileInfoBlock *fib = 0;
    int retval;

    if (*filename)
    {
	if (!(*doSomething)(filename, arg))
 	{
	    errno = ENOENT;
	    return(-1);
	}
	else
	    return(0);
    }
    if (filename[strlen(filename) - 1] == ':')
    {
	errno =  EACCES;		/* root dir */
	return(-1);
    }
 
   /*
     * "" (current directory) case
     *
     * There are a couple things to worry about here.  First of all, you have
     * to find the parent directory, and the name of the current directory.
     * But if there is no parent, you can't set the comment.  Once you get
     * past this, you have to change directories and unlock the old current
     * directory because you can't set the protection on a busy directory.
     */
    if (!(lock = Lock("", ACCESS_READ)))
    {
	errno = ENOENT;
	return (-1);
    }
    if (!(parentLock = ParentDir(lock)))
    {
	UnLock(lock);
	errno = EACCES;		/* root dir */
	return (-1);
    }
    if (!(fib = (struct FileInfoBlock *) malloc(sizeof(*fib))))
    {
	UnLock(lock);
	errno = ENOMEM;
	return (-1);
    }
    Examine(lock, fib);
    UnLock(lock);		/* free Lock obtained by Lock("",) */

    /*
     * At this point we have a FileInfoBlock with the name of this directory,
     * and a lock on our parent directory.  CD to the parent directory, unlock
     * the old directory, and set the comment.
     */
    lock = CurrentDir(parentLock);
    UnLock(lock);		/* free Lock held by shell??? */
    if (!(*doSomething)(fib->fib_FileName, arg))
    {
	errno = ENOENT;
	retval = -1;
    }
    else
	retval = 0;

    /*
     * Whew, now cleanup.  Remember, we are now in the parent directory.
     */
    lock = Lock(fib->fib_FileName);
    parentLock = CurrentDir(lock);
    UnLock(parentLock);
    free(fib);
    return (retval);
}

/*
 * Lattice C perror() puts out a leading \n - this is the wrong behavior
 */
perror(char *s)
{
    if ((errno >= 0) && (errno <= sys_nerr))
    {
	fputs(s, stderr);
	fputs(": ", stderr);
	fputs(sys_errlist[errno], stderr);
	putc('\n', stderr);
	fflush(stderr);		/* why do I have to do this???  It shouldn't
				   be buffered. */
    }
    return(errno);
}

/*
 * It's probably cleaner to write my own mkdir() which takes two arguments,
 * rather than paste in the code to set the mode everywhere mkdir() is
 * called
 */
mkdir(char *dirname, int mode)
{
    struct FileLock *lock;

    if (lock = CreateDir(dirname))
    {
	UnLock(lock);
	return(SmartSetProtection(dirname, (~(((mode & 0700) >> 5) | 1)) & 0xf));
    }
    else
    {
	if (IoErr() == ERROR_DIRECTORY_NOT_EMPTY)
	    return(0);
 	errno = EACCES;		/* FIXME */
	return(-1);
    }
}
