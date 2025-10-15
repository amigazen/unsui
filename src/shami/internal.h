struct InternalNode
{
	char *name;
	int (*code)(int,char **);
};

void FreeCmdCache(void);

int DOdot(int argc,char *argv[]);
int DOcolon(int argc,char *argv[]);
int DOalias(int argc,char *argv[]);
int DOcd(int argc,char *argv[]);
int DOchgrp(int argc, char *argv[]);
int DOchmod(int argc,char *argv[]);
int DOchown(int argc, char *argv[]);
int DOecho(int argc,char *argv[]);
int DOeval(int argc,char *argv[]);
int DOexport(int argc,char *argv[]);
int DOfilenote(int argc,char *argv[]);
int DOif(int argc,char *argv[]);
int DOinternal(int argc,char *argv[]);
int DOpwd(int argc,char *argv[]);
int DOread(int argc,char *argv[]);
int DOrehash(int argc,char *argv[]);
int DOset(int argc,char *argv[]);
int DOtest(int argc,char *argv[]);
int DOunalias(int argc,char *argv[]);
int DOunset(int argc,char *argv[]);
int DOwhich(int argc,char *argv[]);
int DOhistory(int argc,char *argv[]);

