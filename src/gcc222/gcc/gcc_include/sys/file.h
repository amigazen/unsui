/*	system files for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef	_SYS_FILE_H
#define _SYS_FILE_H

/* flags for access */

#define	X_OK	1
#define	W_OK	2
#define	R_OK	4

/* flags for open call */

#define O_RDONLY	0x01	/* open existing file for reading only */
#define O_WRONLY	0x02	/* open for writing (also create) */
#define O_RDWR		0x03	/* open for read-write */
#define O_APPEND	0x40	/* append to file */
#define O_CREAT		0x20	/* create new file if needed */
#define O_TRUNC		0x40	/* make file 0 length */
#define O_EXCL		0x80	/* error if file exists */

/* flags for lseek call */

#define L_XTND		2		/* lseek from end */

/* standard file calls prototypes */

#ifdef	__cplusplus
extern "C" {
#endif

extern	int		access(char *name,int mode);
extern	int		chdir(char *name);
extern	int		chmod(char *name,int mode);
extern	int		chqwn(char *name,int owner, int group);
extern	int		close(int fd);
extern	int		creat(char *name, int mode);
extern	int		execv(char *name,char *argv[]);
extern	int		execve(char *name,char *argv[],char *envp[]);
extern	int		link(char *name1,char *name2);
extern	long	lseek(int fd,long offset,int whence);
/* extern	int		open(char *name,int mode); */
extern	int		read(int fd,char *buf,int nbytes);
extern	int		unlink(char *name);
extern	int		write(int fd,char *buf,int nbytes);
extern	int		isatty(int fd);

#ifdef	__cplusplus
}
#endif

/* some general un*x calls defined as macros for more speed */

/* #define tell(file)			lseek(file,0L,1)	*/
#define umask(mode)			(0)				/* masks not supported */
#define getdtablesize()		(50)			/* MAX_FILE_NUMBER */
#define getpid()			(FindTask(0L))	/* get process ID */
#define getuid()			(0)				/* get user id */
#define geteuid()			(0)				/* get effective user id */
#define getgid()			(0)				/* get group id */
#define getegid()			(0)				/* get effective group id */

#endif	/* _SYS_FILE_H */
