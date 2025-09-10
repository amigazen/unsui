/* stat.h	1 May 1990 David G. Grubbs (-dgg-)

   Amiga support for Unix "stat" function.
*/
#ifndef __STAT_H__
#define __STAT_H__

struct	stat
{
    unsigned short st_mode;
    long	st_size;
    long	st_mtime;
    long	st_blocks;
    long	st_blksize;
    short	st_uid,st_gid;	/* Special - returned as '0' (== root?) */
    short	st_nlink;	/* Special - == 1 since no links in AmigaDOS */
    short	st_ino;		/* The Amiga DiskKey is a "file number" */
    short	st_dev;		/* Later? */
    short	st_rdev;	/* No such thing as device files */
    long	st_atime,st_ctime; /* No Access or creation time available*/
};

/* Unix compatible names supported by the local "stat" function.
   Those marked with '**' do not exist in AmigaDos.
*/
#define	S_IFMT	0160000		/*    Mask for 3 bits holding file type */
#define   S_IFDIR	0040000	/*    directory */
#define   S_IFCHR	0020000	/* ** character device */
#define   S_IFBLK	0060000	/* ** block device */
#define   S_IFREG	0100000	/*    normal file */
#define   S_IFLNK	0120000	/* ** sym link */
#define   S_IFSOCK	0140000	/* ** socket */
#define   S_IFIFO	0160000	/* ** fifo */

/* Since the sticky bit and the suid bits don't mean anything in AmigaDOS,
   we'll reserve a bit that is all three together that is always set
   to zero.  That leaves a few bits for AmigaDOS-specific bits.

   Also, to keep Unix compatibility, we are wasting 6 bits for "group" and
   "other" protections.
*/
#define	S_ISUID		0020000
#define	S_ISGID		0020000
#define	S_ISVTX		0020000
#define	S_IREAD		0000400
#define	S_IWRITE	0000200
#define	S_IEXEC		0000100

/* AmigaDOS-specific bits */
#define	S_NODELETE	0010000
#define	S_ARCHIVE	0004000
#define	S_SCRIPT	0002000
#define	S_PURE		0001000
#endif
