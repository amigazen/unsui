struct InternalNode
{
	char *name;
	int (*code)(int,char **);
};

void FreeCmdCache(void);

int DOcd(int argc,char *argv[]);
int DOchgrp(int argc, char *argv[]);
int DOchmod(int argc,char *argv[]);
int DOchown(int argc, char *argv[]);
int DOecho(int argc,char *argv[]);
int DOfilenote(int argc,char *argv[]);
int DOif(int argc,char *argv[]);
int DOinternal(int argc,char *argv[]);
int DOrehash(int argc,char *argv[]);
int DOwhich(int argc,char *argv[]);

