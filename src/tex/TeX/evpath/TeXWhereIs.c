/*
 * TeXWhereIs.c - a simple file to show how evpaths.lib routines work.
 *
 * Version 1.0 - © 1994 by Giuseppe Ghibò
 *
 * Compile with:
 *
 *	 SC OPT LINK NOSTKCHK LIB evpaths.lib TeXWhereIs
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include "evpaths.h"

#define BUFLEN 256
#define EVPBUF 8192L

//char *default_path[] = { ".",
//			  "TeX:inputs",
//			  "TeX:texinputs",
//			  NULL };

char *default_path = ".,TeX:inputs,TeX:texinputs";

void main(int argc, char **argv)
{
	struct EnvVarPath *var;
	char buf[BUFLEN], *s;

	if (argc < 2)
	{
		fprintf(stderr,"TeXWhereIs v1.0 - © 1994 by Giuseppe Ghibò\n");
		fprintf(stderr,"Usage:\n");
		fprintf(stderr,"\tTeXWhereIs ENVVAR filename [-p]\n");
		fprintf(stderr,"Example:\n");
		fprintf(stderr,"\tTeXWhereIs MFINPUTS cmr10.mf\n");
		fprintf(stderr,"Search the file `cmr10.mf' in the path specified by the environment\n");
		fprintf(stderr,"variable `MFINPUTS' and then show where it is located. If the flag `-p'\n");
		fprintf(stderr,"also specified then TeXWhereIs shows also the expanded entries of the\n");
		fprintf(stderr,"specified environment variable.\n");
		exit(1);
	}

/* argv[1] = ENV VAR NAME, argv[2] = FILE NAME */

	var = Alloc_EnvVarPath(argv[1], EVPBUF);

/* note that Init_EnvVarPath() must be executed only once for each env var, unless we
   call Free_EnvVarPath() */

//	Init_EnvVarPath(var, NULL, NULL);
//	Init_EnvVarPath(var, default_path, ENVPATH_DEFARR);
//	Init_EnvVarPath(var, default_path, ENVPATH_DEFSTR);
	Init_EnvVarPath(var, default_path, ENVPATH_DEFSTR | ENVPATH_APPEND_PATH);

	if (argv[3][0] == '-' && argv[3][1] == 'p')
	{
		int i = 0;
		printf("The environment variable: %s has length %ld\n",argv[1],GetVarLength(argv[1]));
		
		while (var->storage.strings[i] != NULL)
			printf("%d -> `%s'\n", i, var->storage.strings[i++]);

	}

 	if (s = EVP_FileSearch(argv[2], var, buf, 256))
		printf("File found: `%s'\n",s);
	else
		printf("No file found.\n");

	Free_EnvVarPath(var); /* don't forget this!!! */
}
