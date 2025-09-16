/*
 * Copyright (c) 2005-2006, Shane Lahey
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, 
 * are permitted provided that the following conditions are met:
 *
 *    * Redistributions of source code must retain the above copyright notice, 
 *      this list of conditions and the following disclaimer.
 *
 *    * Redistributions in binary form must reproduce the above copyright notice, 
 *      this list of conditions and the following disclaimer in the documentation 
 *      and/or other materials provided with the distribution.
 *
 *    * Neither the name of the OpenSFV nor the names of its contributors 
 *      may be used to endorse or promote products derived from this software 
 *      without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR 
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// $Id: sfv.c,v 1.3 2005/09/18 20:51:50 craz1 Exp $

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <libgen.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>

#include "opensfv.h"
#include "md5file.h"
#include "crc32.h"
#include "functions.h"
#include "dir.h"
#include "sha1.h"

void opensfv_version(void)
{
#ifdef CYGWIN
    printf("OpenSFV %s-cygwin -- Copyright (c) 2005-2006, Shane Lahey <s.lahey@gmail.com>%s", OPENSFV_VERSION, EOLSTR);
#else
    printf("OpenSFV %s -- Copyright (c) 2005-2006, Shane Lahey <s.lahey@gmail.com>%s", OPENSFV_VERSION, EOLSTR);
#endif
}

void usage(char *appname)
{
    opensfv_version();
    printf("Usage: %s <file1> [file2] [file3]...%s", appname, EOLSTR);
    printf("%sGenerates SFV(crc32) checksums for the given file(s)%s", EOLSTR, EOLSTR);
    printf("and outputs the results to stdout, or optinally validates%s", EOLSTR);
    printf("the SFV file <sfvfile> (see below for details)%s%s", EOLSTR, EOLSTR);
    printf("[SFV validation options]%s", EOLSTR);
    printf("   -c <sfvfile>     - validate <sfvfile>%s", EOLSTR);
    printf("   -d <path>        - files in <sfvfile> are located in folder <path>%s", EOLSTR);
    printf("%s[SFV generation options]%s", EOLSTR, EOLSTR);
    printf("   -i               - include file info in the header of sfv files%s", EOLSTR);
    printf("   -d <path>        - create SFV's for all files in <path>%s", EOLSTR);
    printf("   -r               - create SFV's for all subdirs in <path> (used with -d only)%s", EOLSTR);
    printf("%s[Miscellaneous options]%s", EOLSTR, EOLSTR);
    printf("   -v               - display version info and exit.%s", EOLSTR);
    printf("   -h               - display this help and exit.%s", EOLSTR);
    printf("%s[Hash options]%s", EOLSTR, EOLSTR);
    printf("   -m               - generate/validate MD5 instead of SFV%s", EOLSTR);
    printf("   -s               - generate/validate SHA1 instead of SFV%s", EOLSTR);
    printf("%s", EOLSTR);
    exit(0x1);
}

char *rsplit(char *str, char delimiter)
{
    char *p;
    
    if ((p = strrchr(str, delimiter)) == NULL)
	return NULL; // not found
    
    *p = 0x0;
    return p + 1;
}

/* rmspace() function borrowed from the eggdrop source
 * Copyright (C) 1997 Robey Pointer
 * Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Eggheads Development Team
 */ 
void rmspace(char *s)
{
    register char *p = NULL, *q = NULL;
    
    if (!s || !*s)
	return;
    
    /* Remove trailing whitespaces. */
    for (q = s + strlen(s) - 1; q >= s && isspace(*q); q--);
    *(q + 1) = 0;
    
    /* Remove leading whitespaces. */
    for (p = s; isspace(*p); p++);

    if (p != s)
	memmove(s, p, q - p + 2);
}

void checksfvfile(char *sfvfile, char *rootdir)
{
    FILE *fd;
    int fdin;
    char buf[4096];
    char *p;
    char checksum[9];
    char filename[4096];
    unsigned long crc32;
    unsigned long oldcrc32;
    
    if ((fd = fopen(sfvfile, "r")) == NULL)
    {
	printf("Error: unable to read SFV file: %s (%s)%s", sfvfile, strerror(errno), EOLSTR);
	exit(0x1);
    }
    
    gen_table_crc32(); // *MUST* do this first
    
    // chdir AFTER sfvfile is open, to avoid fopen() error
    chdir(rootdir);
    
    while (fgets(buf, sizeof(buf)-1, fd) != NULL)
    {
	if (buf[0] == ';')
	    continue; // comment

	if ((p = strrchr(buf, 0x20)) == NULL)
	    continue; // error on line? bahf!

	*p = 0x0;
	strncpy(filename, buf, sizeof(filename));
	strncpy(checksum, p + 1, 8);
	checksum[8] = 0x0;
	
	oldcrc32 = strtoll(checksum, NULL, 16);
	
	if ((fdin = open(filename, O_RDONLY)) == -1)
	{
	    printf("FAILED: %s (%s)%s", filename, strerror(errno), EOLSTR);
	    continue;
	}
	
	crc32 = get_crc32(fdin);
	close(fdin);
	
	if (crc32 == oldcrc32)
	    printf("PASSED: %s [%lX]%s", filename, crc32, EOLSTR);
	else
	    printf("FAILED: %s [was:%lX] [now:%lX]%s", filename, oldcrc32, crc32, EOLSTR);
    }
    fclose(fd);
    exit(0x0);
}

void checkmd5file(char *file, char *rootdir)
{
    FILE *fd;
    int fdin;
    char buf[4096];
    char *p;
    char checksum[32];
    char filename[4096];
    char *md5;
    long failed = 0;
    long checked = 0;
    
    if ((fd = fopen(file, "r")) == NULL)
    {
	printf("Error: unable to read MD5 file: %s (%s)%s", file, strerror(errno), EOLSTR);
	exit(0x1);
    }
    
    // chdir AFTER checksums file is open, to avoid fopen() errors
    chdir(rootdir);
    
    while (fgets(buf, sizeof(buf)-1, fd) != NULL)
    {
	if (buf[0] == '#')
	    continue; // comment

	// 3 types of MD5 files can be
	//   1. checksum* [path]
	//   2. checksum  [path]
	//   3. MD5(file)= checksum
	
	// type 3
	if (strncmp(buf, "MD5(", 4) == 0) {
	    if ((p = strrchr(buf, 0x20)) == NULL)
		continue; // error on line...
	    *p = 0x0;
	    strncpy(checksum, p + 1, 32);
	    checksum[32] = 0x0;
	    
	    if ((p = strrchr(buf, ')')) == NULL)
		continue; // error on line...
	    *p = 0x0;
	    
	    if ((p = strchr(buf, '(')) == NULL)
		continue; // error on line...
	    
	    strncpy(filename, p + 1, sizeof(filename));
	    
	}
	else {
	    // type 1 & 2 can be done by simply by copying the first 32 bytes of each line
	    if ((p = strchr(buf, 0x20)) == NULL)
		continue; // error on line...
	    *p = 0x0;
	    strncpy(checksum, buf, 32);
	    checksum[32] = 0x0;
	    strncpy(filename, p + 1, sizeof(filename));
	    rmspace(filename);
	}

	checked++; // ok to check this file, increment counter
	
	if ((fdin = open(filename, O_RDONLY)) == -1)
	{
	    printf("%s: FAILED%s", filename, EOLSTR);
	    continue;
	}
	
	md5 = md5file(fdin);
	close(fdin);

	if (strcasecmp(md5, checksum) == 0)
	    printf("%s: OK%s", filename, EOLSTR);
	else {
	    printf("%s: FAILED%s", filename, EOLSTR);
	    failed++;
	}
    }
    fclose(fd);
    

    if (failed > 0) {
	printf("OpenSFV: WARNING: %lu of %lu computed checksums did NOT match%s", failed, checked, EOLSTR);
	exit(0x1);
    }
    
    exit(0x0);
}

void checksha1file(char *file, char *rootdir)
{
    FILE *fd;
    int fdin;
    char buf[4096];
    char *p;
    char checksum[40];
    char filename[4096];
    char *sha1;
    long failed = 0;
    long checked = 0;
    
    if ((fd = fopen(file, "r")) == NULL)
    {
	printf("Error: unable to read SHA1 file: %s (%s)%s", file, strerror(errno), EOLSTR);
	exit(0x1);
    }
    
    // chdir AFTER checksums file is open, to avoid fopen() errors
    chdir(rootdir);
    
    while (fgets(buf, sizeof(buf)-1, fd) != NULL)
    {
	if (buf[0] == '#')
	    continue; // comment

	// 2 types of SHA1 files can be
	//   1. checksum  [path]
	//   2. SHA1(file)= checksum
	
	// type 2
	if (strncmp(buf, "SHA1(", 4) == 0) {
	    if ((p = strrchr(buf, 0x20)) == NULL)
		continue; // error on line...
	    *p = 0x0;
	    strncpy(checksum, p + 1, 40);
	    checksum[40] = 0x0;
	    
	    if ((p = strrchr(buf, ')')) == NULL)
		continue; // error on line...
	    *p = 0x0;
	    
	    if ((p = strchr(buf, '(')) == NULL)
		continue; // error on line...
	    
	    strncpy(filename, p + 1, sizeof(filename));
	    
	}
	else {
	    // type 1
	    if ((p = strchr(buf, 0x20)) == NULL)
		continue; // error on line...
	    *p = 0x0;
	    strncpy(checksum, buf, 40);
	    checksum[40] = 0x0;
	    strncpy(filename, p + 1, sizeof(filename));
	    rmspace(filename);
	}

	checked++; // ok to check this file, increment counter
	
	if ((fdin = open(filename, O_RDONLY)) == -1)
	{
	    printf("%s: FAILED%s", filename, EOLSTR);
	    continue;
	}
	
	sha1 = sha1file(fdin);
	close(fdin);

	if (strcasecmp(sha1, checksum) == 0)
	    printf("%s: OK%s", filename, EOLSTR);
	else {
	    printf("%s: FAILED%s", filename, EOLSTR);
	    failed++;
	}
    }
    fclose(fd);
    

    if (failed > 0) {
	printf("OpenSFV: WARNING: %lu of %lu computed checksums did NOT match%s", failed, checked, EOLSTR);
	exit(0x1);
    }
    
    exit(0x0);
}

void printheader(long sfvmode)
{
    struct tm *tm;
    time_t now;
    char comment = ';';
    
    now = time(NULL);
    tm = localtime( &now );
    
    switch (sfvmode)
    {
    case MD5MODE:
    case SHA1MODE:
	comment = '#';
	break;
    case SFVMODE:
    default:
	comment = ';';
	break;
    }
    
#ifdef CYGWIN
    printf("%c Generated by OpenSFV v%s-cygwin on %d-%.2d-%.2d at %02d:%02d:%02d%s",
	   comment,
	   OPENSFV_VERSION,
	   tm->tm_year+1900, tm->tm_mon, tm->tm_mday,
	   tm->tm_hour, tm->tm_min, tm->tm_sec, EOLSTR);
#else
    printf("%c Generated by OpenSFV v%s on %d-%.2d-%.2d at %02d:%02d:%02d%s",
	   comment,
	   OPENSFV_VERSION,
	   tm->tm_year+1900, tm->tm_mon, tm->tm_mday,
	   tm->tm_hour, tm->tm_min, tm->tm_sec, EOLSTR);
#endif
    printf("%c Copyright (c) 2005-2006, Shane Lahey (s.lahey@gmail.com)%s", comment, EOLSTR);
    printf("%c http://opensfv.sourceforge.net%s", comment, EOLSTR);
    printf("%c%s", comment, EOLSTR);
}

void printfileinfo(char *path, struct fileinfo *p, long sfvmode)
{
    struct stat st;
    struct tm *tm;
    char comment = ';';
    
    if (p != NULL)
	st = p->st;
    else {
	if (stat(path, &st) != 0)
	    return; // file not found?
    }
    
    if (!S_ISREG(st.st_mode))
	return; // not a regular file, don't print anything
    
    tm = localtime( &st.st_mtime );
    
    switch (sfvmode)
    {
    case MD5MODE:
    case SHA1MODE:
	comment = '#';
	break;
    case SFVMODE:
    default:
	comment = ';';
	break;
    }
    
#ifdef CYGWIN
    printf("%c %16lld  %.2d:%.2d.%.2d %d-%.2d-%.2d %s%s",
	   comment,
	   st.st_size,
	   tm->tm_hour, tm->tm_min, tm->tm_sec,
	   tm->tm_year+1900, tm->tm_mon, tm->tm_mday,
	   path, EOLSTR
	   );
    
#else	   
    printf("%c %16lu  %.2d:%.2d.%.2d %d-%.2d-%.2d %s%s",
	   comment,
	   st.st_size,
	   tm->tm_hour, tm->tm_min, tm->tm_sec,
	   tm->tm_year+1900, tm->tm_mon, tm->tm_mday,
	   path, EOLSTR
	   );
#endif

}

int main(int argc, char *argv[])
{
    int opt, i;
    int showfileinfo = 0;
    int dofolder = 0;
    char *foldername = ".";
    int dofolderrecursive = 0;
    int fd;
    unsigned long sfvmode = SFVMODE;
    int dovalidation = 0;
    char *validationfile = NULL;
    struct fileinfo *p;
    
    if (argc < 2)
	usage( basename(argv[0]) );
    
    while ((opt = getopt(argc, argv, "d:c:hirvms")) != -1)
    {
	switch(opt)
	{
	case 'v':
	    opensfv_version();
	    return 0x0;
	    break;
	case 'd':
	    dofolder = 1;
	    foldername = optarg;
	    break;
	case 'r':
	    dofolderrecursive = 1;
	    break;
	case 'c':
	    dovalidation = 1;
	    validationfile = optarg;
	    break;
	case 'i':
	    showfileinfo = 1;
	    break;
	case 'm':
	    sfvmode = MD5MODE;
	    break;
	case 's':
	    sfvmode = SHA1MODE;
	    break;
	case 'h':
	default:
	    usage( basename(argv[0]) );
	    break;
	}
    }

    // are we in validation mode?
    if (dovalidation)
    {
	switch (sfvmode)
	{
	case SFVMODE:
	    checksfvfile(validationfile, foldername);
	    break;
	case MD5MODE:
	    checkmd5file(validationfile, foldername);
	    break;
	case SHA1MODE:
	    checksha1file(validationfile, foldername);
	    break;
	default:
	    printf("Unsupported hash type specified%s", EOLSTR);
	    return 0x1;
	}
    }
    
    printheader(sfvmode);
        
    gen_table_crc32(); // *MUST* do this first

    if (dofolder)
    {
	chdir(foldername);
	foldername = ".";
	getdircontents(foldername, dofolderrecursive);
	
	if (showfileinfo)
	{
	    p = filetree;
	    // show file info first required
	    while (p) {
		printfileinfo(striproot(p->filename, foldername), p, sfvmode);
		p = p->next;
	    }
	}
	
	// now gen SFV's
	p = filetree;
	while (p) {
            if (!isfile(p->filename)) {
		p = p->next;
		continue;
	    }
	    
	    if ((fd = open(p->filename, O_RDONLY)) == -1) {
		fprintf(stderr, "failed to read file: %s (%s)%s", p->filename, strerror(errno), EOLSTR);
		p = p->next;
		continue;
	    }
	    
	    if (sfvmode == MD5MODE)
		printf("%s  %s%s", md5file(fd), striproot(p->filename, foldername), EOLSTR);
	    else if (sfvmode == SHA1MODE)
		printf("%s  %s%s", sha1file(fd), striproot(p->filename, foldername), EOLSTR);
	    else
		printf("%s %lX%s", striproot(p->filename, foldername), get_crc32(fd), EOLSTR); // SFVMODE
	    
	    close(fd);
	    p = p->next;
	}
    }
    else {
	// show file info for each file first
        if (showfileinfo) {
            for (i = 1; i < argc; i++)
                printfileinfo(argv[i], NULL, sfvmode);
        }
	
	for (i = 1; i < argc; i++)
	{
	    if (!isfile(argv[i]))
		continue;
	    if ((fd = open(argv[i], O_RDONLY)) == -1) {
		fprintf(stderr, "failed to read file: %s (%s)%s", argv[i], strerror(errno), EOLSTR);
		continue;
	    }
	    
	    if (sfvmode == MD5MODE)
		printf("%s  %s%s", md5file(fd), basename(argv[i]), EOLSTR);
	    else if (sfvmode == SHA1MODE)
		printf("%s  %s%s", sha1file(fd), basename(argv[i]), EOLSTR);
	    else
		printf("%s %lX%s", basename(argv[i]), get_crc32(fd), EOLSTR); // SFVMODE
	    
	    close(fd);
	}
    }
    
    return 0x0;
}
