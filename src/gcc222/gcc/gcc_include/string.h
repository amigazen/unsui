/*	string functions for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 		*/

#ifndef	_STRING_H
#define _STRING_H

#include    <sys/types.h>
#include    <stddef.h>

/* string functions */

#ifdef	__cplusplus
extern "C" {
#endif

extern int		strcmp(const char *, const char *);
extern int		strncmp(const char *, const char *, size_t);
extern char		*strcat(char *, const char *);
extern char		*strncat(char *, const char *, size_t);
extern char		*strcpy(char *, const char *);
extern char		*strncpy(char *, const char *, size_t);
extern char		*strchr(const char *, int);
extern char		*strrchr(const char *, int);
extern char		*strpbrk(const char *, char *);
extern char		*strstr(const char *, const char *);
extern char		*strtok(char *, const char *);
extern size_t	strspn(const char *, const char *);
extern size_t	strcspn(const char *, const char *);
extern size_t   strlen(const char *);
extern char		*strcatn(char *, char *, size_t);
extern int		strcmpn(char *,char *, int);
extern char		*strcpyn(char *,char *, int);
extern char		*mktemp(char *);
extern char		*strerror(int);

/* memory functions (both BSD and SYS V versions) */

extern int	memcmp(const void *, const void *, size_t);
extern void	*memcpy(void *, const void *, size_t);
extern void	*memmove(void *, const void *, size_t);
extern void	*memset(void *, int, size_t);
extern void	*memchr(const void *, int, size_t);
extern void	bcopy(const void *,void *,int);
extern void	bzero(void *,int);
extern int	bcmp(const void *,const void *,int);

#ifdef	__cplusplus
}
#endif

/* some alias */

#ifdef __cplusplus

extern inline char *strrchar(char *a,int b)
{
  return strrchr(a,b);
}

extern inline int stclen(char *a)
{
  return strlen(a);
}

#else

#define	strrchar	strrchr
#define stclen(a)	strlen(a)

#endif /* __cplusplus */

#endif /* _STRING_H */
