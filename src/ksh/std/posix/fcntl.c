/* fcntl emulation */
/* $Id: fcntl.c,v 1.3 93/05/05 21:17:45 sjg Exp $ */

#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

#if _V7

#include <sgtty.h>

int
fcntl(fd, cmd, arg)
	int fd, cmd, arg;
{
	switch (cmd) {
	  case F_SETFD:		/* set fd flags */
		ioctl(fd, (arg&FD_CLEXEC) ? FIOCLEX : FIONCLEX, (char *)NULL);
		break;
	  case F_DUPFD:		/* dup fd */
		/* this one is fun. find an unused fd >= arg and dup2 */
		break;
	}
	return 0;
}

#endif

