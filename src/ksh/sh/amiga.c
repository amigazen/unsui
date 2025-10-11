#include <errno.h>
#include <fcntl.h>

int
fork()
{
  /* we don't have fork(). If I missed some fork-call (or if there's one
     in a later version of the shell), just return with out-of-processes. */
  errno = EPROCLIM;

  return -1;
}

int
tty_read (int fd, char *buf, int len)
{
  /* On AmigaOS, just do a direct read for now */
  /* TODO: Implement proper select() handling if needed */
  return read (fd, buf, len);
}

/* should update the library.... the header prototypes are *pgid(), and
   POSIX *pgrp(), but the library doesn't contain the Posix versions yet,
   and uses oldstyle BSD *pgrp() functions... */
int setpgid (int p, int q) { return setpgrp (p, q); }
int getpgid (int p) { return getpgrp (p); }
