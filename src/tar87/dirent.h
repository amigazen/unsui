/*
 * dirent.h for AmigaDOS - derived from an old document written by Doug Gwyn
 *
 * $Header$
 */
#include <sys/types.h>
#include <libraries/dosextens.h>
#include <proto/dos.h>

#define MAXNAMELEN	108		/* space in FIB for name */

#ifndef NAME_MAX
#define NAME_MAX	(MAXNAMELEN - 1)
#endif

struct dirent  {
	long		d_ino;
	off_t		d_off;
	unsigned short	d_reclen;
	char		d_name[1];
};

typedef struct  {
	struct dirent	*dd_dirent;	/* ok to reuse this over and over */
	struct FileLock	*dd_lock;
	struct FileInfoBlock	*dd_fib;
	int		dd_loc;
} DIR;

#define DIRENTBASESIZ	(&((struct dirent *) 0)->d_name \
			- (char *) &((struct dirent *) 0)->d_ino)
#define	DIRENTSIZ(namelen)	((DIRENTBASESIZ + sizeof(long) + (namelen)) \
				/ sizeof(long) * 4)

DIR	*opendir(char *dirname);
struct dirent	*readdir(DIR *dirp);
off_t	telldir(DIR *dirp);
void	seekdir(DIR *dirp, off_t loc);
void	rewinddir(DIR *dirp);
