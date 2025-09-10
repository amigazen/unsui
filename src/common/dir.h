/* dir.h	1 May 1990 David G. Grubbs (-dgg-)

   This file originally came from "Ndir" from Mike Meyer on Fish Disk 92.
   It was created and handed to Rich Salz (moderator of comp.unix.sources)
   in response to his request for "readdir" functions for all machines.

   It is intended to provide the 4BSD directory functions for AmigaDOS.

   I have modified it considerably.
*/
#ifndef __DIR_H__
#define __DIR_H__

#include <libraries/dosextens.h>
/*
 * The direct struct is from 4BSD in the name of source code compatibility.
 * 
 */
#define	MAXNAMLEN	31		/* AmigaDOS file max length */

struct	direct {
    unsigned long	d_ino;		/* Unused in AmigaDOS */
    unsigned short	d_reclen;	/* Unused in AmigaDOS */
    unsigned short	d_namlen;	/* length of d_name */
    char d_name[MAXNAMLEN + 1];		/* directory entry. */
};
/*
 * DIRSIZ is the sum of the length of the filename (plus the terminating null)
 * and the "struct direct" overhead, rounded to a longword boundary.
 */

#undef DIRSIZ
#define DIRSIZ(dp) (((((dp)->d_namlen) + 4) & ~3) + 8)
/* TELLSLOTS must be a power of 2 */
#define TELLSLOTS	16
#define TELLMASK	(TELLSLOTS - 1)

/*
 * A DIR structure useful to AmigaDOS.  It is entirely different from the
 * Unix version, but it serves its function as a directory "handle".
 */

typedef struct {
    struct FileInfoBlock d_fib,		/* Main info struct */
			*d_sfib[TELLSLOTS];  /* seek save pointers */
    struct FileLock	*d_lock;	/* Directory Lock */ 
    long		 d_tell;	/* next "telldir" handle */
    long		 d_max;		/* Idx of highest fib ever */
} DIR;
	
#define NULLDIR ((DIR *)0)

extern	DIR *opendir(char *);
extern	struct direct *readdir(DIR *);
extern	long telldir(DIR *);
extern	void seekdir(DIR *, long);
extern	void rewinddir(DIR *);
extern	int closedir(DIR *);
#endif
