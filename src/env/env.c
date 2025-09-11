/* Print the Amiga environment. */

#include <stdio.h>
#include "dir.h"

main(int argc, char **argv)
{
    DIR	*dirp;
    struct direct *dp;
    char name[128];
    char buff[128];
    FILE *f;
    int len;

    if (!(dirp = opendir("ENV:"))) {
	fprintf(stderr, "Can't open ENV:\n");
	exit(1);
    }

    for (dp = readdir(dirp); dp; dp = readdir(dirp)) {
	sprintf(name, "ENV:%s", dp->d_name);
	if (!(f = fopen(name, "r"))) {
	    fprintf(stderr, "Bad file, ENV:%s\n", dp->d_name);
	    continue;
	}
	len = fread(buff, 1, sizeof(buff) - 1, f);
	fclose(f);
	if (len > 0 && buff[len - 1] == '\n') len--;
	buff[len] = '\0';
	printf ("%s=%s\n", dp->d_name, buff);
    }

    closedir(dirp);
    exit(0);
}
