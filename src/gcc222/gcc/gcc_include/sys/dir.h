/*	directory scanning for ansic.library	*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef	_SYS_DIR_H
#define _SYS_DIR_H

#include	<sys/types.h>
#include	<dos/dos.h>

typedef	long int	DIR;		/* this is private */

struct direct {
  ino_t	d_ino;
  char  d_name[108];
  int   d_namlen;
};

#ifdef	__cplusplus
extern "C" {
#endif

DIR           *opendir(const char *);
struct direct *readdir(DIR *);
void          rewinddir(DIR *);
void          closedir(DIR *);

#ifdef	__cplusplus
}
#endif

#endif	/* _SYS_DIR_H */


