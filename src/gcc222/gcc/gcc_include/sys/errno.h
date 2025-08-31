/*	error codes for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

/* The following symbols are the error codes returned by the UNIX system	*/
/* functions.  Typically, a UNIX function returns -1 when an error occurs,	*/
/* and the global integer named errno contains one of these values.			*/

#ifndef	_SYS_ERRNO_H
#define _SYS_ERRNO_H

#define UNUSED	0
#define EPERM	1 	/* User is not owner */
#define ENOENT	2	/* No such file or directory */
#define ESRCH	3 	/* No such process */
#define EINTR	4 	/* Interrupted system call */
#define EIO		5	/* I/O error */
#define ENXIO	6 	/* No such device or address */
#define E2BIG	7 	/* Arg list is too long */
#define ENOEXEC	8	/* Exec format error */
#define EBADF	9 	/* Bad file number */
#define ECHILD	10	/* No child process */
#define EAGAIN	11	/* No more processes allowed */
#define ENOMEM	12	/* No memory available */
#define EACCES	13	/* Access denied */
#define EFAULT	14	/* Bad address */
#define ENOTBLK	15	/* Bulk device required */
#define EBUSY	16	/* Resource is busy */
#define EEXIST	17	/* File already exists */
#define EXDEV	18	/* Cross-device link */
#define ENODEV	19	/* No such device */
#define ENOTDIR	20	/* Not a directory */
#define EISDIR	21	/* Is a directory */
#define EINVAL	22	/* Invalid argument */
#define ENFILE	23	/* No more files (units) allowed */
#define EMFILE	24	/* No more files (units) allowed for this process */
#define ENOTTY	25	/* Not a terminal */
#define ETXTBSY	26	/* Text file is busy */
#define EFBIG	27	/* File is too large */
#define ENOSPC	28	/* No space left */
#define ESPIPE	29	/* Seek issued to pipe */
#define EROFS	30	/* Read-only file system */
#define EMLINK	31	/* Too many links */
#define EPIPE	32	/* Broken pipe */
#define EDOM	33 	/* Math function argument error */
#define ERANGE	34	/* Math function result is out of range */

#define EWOULDBLOCK		35
#define EINPROGRESS		36
#define EALREADY		37
#define ENOTSOCK		38
#define EDESTADDRREQ	39
#define EMSGSIZE		40
#define EPROTOTYPE		41
#define ENOPROTOOPT		42
#define EPROTONOSUPPORT 43
#define ESOCKTNOSUPPORT 44
#define EOPNOTSUPP		45
#define EPFNOSUPPORT	46
#define EAFNOSUPPORT	47
#define EADDRINUSE		48
#define EADDRNOTAVAIL	49
#define ENETDOWN		50
#define ENETUNREACH		51
#define ENETRESET		52
#define ECONNABORTED	53
#define ECONNRESET		54
#define ENOBUFS 		55
#define EISCONN 		56
#define ENOTCONN		57
#define ESHUTDOWN		58
#define ETOOMANYREFS	59
#define ETIMEDOUT		60
#define EREFUSED		61
#define ELOOP			62
#define ENAMETOOLONG	63
#define EHOSTDOWN		64
#define EHOSTUNREACH	65
#define ENOTEMPTY		66
#define EPROCLIM		67
#define EUSERS			68
#define EDQUOT			69

#define MAXERRORS		70		/* error number limit */

#ifdef	__cplusplus
extern "C" {
#endif

extern	int		errno;			/* last error code */
extern	int		sys_nerr;		/* max number of errors (equiv to MAXERRORS) */
extern	char	*sys_errlist[];	/* error texts vector */

#ifdef	__cplusplus
}
#endif

#endif	/* _SYS_ERRNO_H */
