/*	file statistics for ansic.library	*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef	_SYS_STAT_H
#define _SYS_STAT_H

#include    <sys/types.h>

/* structure returned by stat and fstat calls */

struct	stat
{
	dev_t			st_dev;
	ino_t			st_ino;
	unsigned short	st_mode;
	short			st_nlink;
	short			st_uid;
	short			st_gid;
	dev_t			st_rdev;
	off_t			st_size;
	time_t			st_atime;
	time_t			st_mtime;
	time_t			st_ctime;
};

/* flags for st_mode */

#define	S_IFMT		0170000		/* type of file */
#define	S_IFDIR		0040000 	/* directory */
#define S_IFCHR		0020000 	/* character special */
#define S_IFBLK		0060000 	/* block special */
#define S_IFREG		0100000 	/* regular */
#define S_IFMPC		0030000 	/* multiplexed character special */
#define S_IFMPB		0070000 	/* multiplexed block special */

#define S_ISUID     0004000	    /* set used id on execution */
#define S_ISGID     0002000	    /* set group id on execution */
#define S_ISVTX     0001000	    /* save swapped text even after use */
#define S_IREAD     0000400	    /* read permission, owner */
#define S_IWRITE    0000200	    /* write permission, owner */
#define S_IEXEC     0000100	    /* execute/search permission, owner */

#define S_IFSOCK    0140000	    /* socket */

/* function prototypes */

#ifdef	__cplusplus
extern "C" {
#endif

extern int stat(char *name,struct stat *buf);
extern int fstat(int fd,struct stat *buf);

#ifdef	__cplusplus
}
#endif

#endif	/* _SYS_STAT_H */
