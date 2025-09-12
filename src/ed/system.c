#define SHELL	"/bin/sh"

system(c)
char *c; {
	int pid, status;
	
	switch (pid = fork()) {
	case -1:
		return -1;
	case 0:
		execl(SHELL, "sh", "-c", c, (char *) 0);
		exit(-1);
	default:
		while (wait(&status) != pid)
			;
	}
	return status;
}
