#include <stdio.h>
#include <fcntl.h>

main(int argc, char **argv)
{
    char **cpp, **cppo, **wildexpand();
    int rtn = 0;		/* Exit code is number of errors */

    if (argc < 2) {
	argc = 1;
	*argv = "-";
    } else {
	argc--;
	argv++;
    }
    for (; argc-- > 0; argv++) {
	amigaizepath(*argv);
	if (!(cppo = wildexpand(*argv)) || !strcmp(*argv, *cppo)) {
	    if (strcmp (*argv,"-")  && access(*argv, 0)) {
		fprintf(stderr, "cat: File \'%s\' doesn't exist.\n", *argv);
		continue;
	    }
	    rtn += catfile(*argv);
	} else {
	    for (cpp = cppo; *cpp; cpp++)
		rtn += catfile(*cpp);

	}
	if (cppo)
	    wildfree(cppo);
    }
}

catfile(char *f)
{
    char buf[1024];
    FILE *fp;

    if (!strcmp(f, "-")) {
	while (fgets(buf, sizeof(buf), stdin))
	    fputs(buf, stdout);

	return(0);
    }
    if (fp = fopen(f,"r")) {
	while (fgets(buf, sizeof(buf), fp))
	    fputs(buf, stdout);
        fclose(fp);
	return(0);
    }
    perror(f);
    return(1);
}
