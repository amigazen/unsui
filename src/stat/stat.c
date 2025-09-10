/*
 * stat - display file or file system status
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.

 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <time.h>
#include <errno.h>
#include <exec/types.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <workbench/workbench.h>
#include <exec/memory.h>
#include <dos/dosextens.h>

#include <proto/dos.h>
#include <proto/exec.h>
#include <proto/icon.h>
#include <proto/utility.h>

#include "common.h"
#include "getopt.h"
#include "stat.h"

extern struct DosLibrary *DOSBase;

/* Forward declarations */
extern time_t Stamp_to_mtime(struct DateStamp *d);

/* POSIX stat system call implementation */
int stathelper(BPTR lk, struct stat *s, int special)
{
    struct FileInfoBlock *fib;

    if (!s) {
	errno = EINVAL;		/* Unix does an EFAULT */
	return (-1);
    }
    memset (s, 0, sizeof(struct stat));
    if (!(fib = (struct FileInfoBlock *)malloc(sizeof *fib))) {
	errno = ENOMEM;
	return (-1);
    }
    if (!Examine(lk, fib)) {
	free(fib);
	errno = ENOENT;
	return(-1);
    }
    /* Set owner UID/GID from FileInfoBlock if available */
    s->st_uid = fib->fib_OwnerUID;
    s->st_gid = fib->fib_OwnerGID;
    s->st_blksize = 512;
    s->st_nlink = 1;			/* No links, so fixed to 1 */
    s->st_blocks = fib->fib_NumBlocks;
    s->st_size = fib->fib_Size;
    s->st_ino = fib->fib_DiskKey;
    s->st_dev = 1;              /* Default device for AmigaDOS */
    s->st_rdev = 0;             /* Not a device file by default */
    s->st_mtime = Stamp_to_mtime(&(fib->fib_Date));
    s->st_atime = s->st_mtime;  /* AmigaDOS doesn't track separate access time */
    s->st_ctime = s->st_mtime;  /* AmigaDOS doesn't track separate change time */

    if (special) {
	s->st_mode = S_IFCHR;
	s->st_rdev = 1;  /* Special device file */
    } else if (fib->fib_DirEntryType > 0)
	s->st_mode = S_IFDIR;
    else
	s->st_mode = S_IFREG;

    if (fib->fib_Protection & FIBF_PURE) s->st_mode |= S_PURE;
    if (fib->fib_Protection & FIBF_SCRIPT) s->st_mode |= S_SCRIPT;
    if (fib->fib_Protection & FIBF_ARCHIVE) s->st_mode |= S_ARCHIVE;
    if (fib->fib_Protection & FIBF_DELETE) s->st_mode |= S_NODELETE;

    /* Map AmigaDOS permissions to POSIX permissions */
    /* Note: AmigaDOS supports multiuser permissions via FIBF_GRP_* and FIBF_OTR_* bits */
    /* Owner permissions (low-active: 0 = allowed) */
    if (!(fib->fib_Protection & FIBF_READ))
	s->st_mode |= S_IREAD;
    if (!(fib->fib_Protection & FIBF_WRITE))
	s->st_mode |= S_IWRITE;
    if (!(fib->fib_Protection & FIBF_EXECUTE))
	s->st_mode |= S_IEXEC;
    
    /* Group permissions (high-active: 1 = allowed) - for multiuser systems */
    if (fib->fib_Protection & FIBF_GRP_READ)
	s->st_mode |= (S_IREAD >> 3);
    if (fib->fib_Protection & FIBF_GRP_WRITE)
	s->st_mode |= (S_IWRITE >> 3);
    if (fib->fib_Protection & FIBF_GRP_EXECUTE)
	s->st_mode |= (S_IEXEC >> 3);
    
    /* Other permissions (high-active: 1 = allowed) - for multiuser systems */
    if (fib->fib_Protection & FIBF_OTR_READ)
	s->st_mode |= (S_IREAD >> 6);
    if (fib->fib_Protection & FIBF_OTR_WRITE)
	s->st_mode |= (S_IWRITE >> 6);
    if (fib->fib_Protection & FIBF_OTR_EXECUTE)
	s->st_mode |= (S_IEXEC >> 6);

    free(fib);
    return(0);
}

/* fstat should deal with fd's opened by "mypopen", terminals and files.

   I've been told that getting a lock from a file handle will not be
   supported until 2.0.  Until then, "fstat" is broken.
*/
int fstat (int fd, struct stat *s)
{
#ifdef AMIGADOS20
    struct _dev *dvp;
    struct FileHandle *fh;

    if (fd < 0 || fd >= _numdev || !((dvp = (_devtab + fd))->fd)) {
	errno = EBADF;
	return(-1);
    }
    fh = (struct FileHandle *)((long)(dvp->fd) << 2);
    if (fh->fh_Args) {
	return (stathelper((BPTR)(fh->fh_Args), s, 0));
    } else {
	return (-1);
    }
#else
    return (-1);		/* Can't do it now */
#endif
}

static char *devfiles[] = { "NIL:", "NULL:", "CON:", NULL };

/* Unix-like "stat" call.  See stat.h for bit definitions and see the
   "stathelper" function above for the meat of the code.
*/
int stat(char *name, struct stat *s)
{
    BPTR lk;
    int rtn, special = 0;
    char *cp, **cpp;

    if (!name) {
	errno = EINVAL;
	return (-1);
    }
    if (!(cp = (char *)malloc(strlen(name) + 1))) {
	errno = ENOMEM;
	return (-1);
    }
    strcpy(cp, name);
    amigaizepath(cp);
    lk = Lock(cp, ACCESS_READ);
    if (!lk) {
	free(cp);
	errno = ENOENT;
	return (-1);		/* No such file or directory */
    }
    for (cpp = devfiles; *cpp; cpp++) {
	if (!strcmp(*cpp, cp)) {
	    special = 1;
	    break;
	}
    }
    free(cp);
    
    rtn = stathelper(lk, s, special);
    UnLock(lk);
    return (rtn);
}

/* Define snprintf if not available */
#ifndef snprintf
int snprintf(char *str, size_t size, const char *format, ...) {
    va_list args;
    int result;
    va_start(args, format);
    result = VSNPrintf(str, size, format, args);
    va_end(args);
    return result;
}
#endif

/* External function declarations from common library */
extern char *my_basename(char *path);
extern int is_getopt_style(int argc, char **argv);
extern char *build_command_string(int argc, char **argv, const char *exclude);
extern int tokenize_string(char *str, char **argv, int max_args);
extern void reset_getopt(void);
extern int getopt(int argc, char * const argv[], const char *optstring);
extern char *optarg;
extern int optind;

/* Wildcard expansion functions */
extern char **wildexpand(char *w);
extern void wildfree(char **freelist);
extern int amigaizepath(char *to);

/* Stamp conversion functions */
extern time_t Stamp_to_mtime(struct DateStamp *d);

/* Version tag for Amiga */
static const char *verstag = "$VER: stat 1.0 (10/09/25)";
static const char *stack_cookie = "$STACK: 4096";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
static const char *template = "FILE/A";

/* Global variables for options */
static int format_flag = 0;
static int terse_flag = 0;
static char *format_string = NULL;

/* Function prototypes */
static void usage(void);
static void version(void);
static int process_file(const char *filename);
static void print_file_info(const char *filename, struct stat *st);
static char *format_mode(unsigned short mode);
static char *format_time(time_t time);

/* Usage information */
static void usage(void)
{
    printf("Usage: stat [OPTION]... FILE...\n");
    printf("Display file or file system status.\n\n");
    printf("  -f, --format=FORMAT   use the specified FORMAT instead of the default\n");
    printf("  -L, --dereference     follow links\n");
    printf("  -t, --terse           print the information in terse form\n");
    printf("  -h, --help            display this help and exit\n");
    printf("  -V, --version         output version information and exit\n\n");
    printf("The valid format sequences for files:\n");
    printf("  %%n   file name\n");
    printf("  %%s   total size, in bytes\n");
    printf("  %%b   number of blocks allocated\n");
    printf("  %%f   raw mode in octal\n");
    printf("  %%a   access rights in octal\n");
    printf("  %%A   access rights in human readable form\n");
    printf("  %%u   user ID of owner\n");
    printf("  %%g   group ID of owner\n");
    printf("  %%N   quoted file name with dereference if symbolic link\n");
    printf("  %%Y   quoted file name with dereference if symbolic link\n");
    printf("  %%x   time of last access, human readable\n");
    printf("  %%y   time of last data modification, human readable\n");
    printf("  %%z   time of last status change, human readable\n");
    printf("  %%w   time of file birth, human readable\n");
    printf("  %%W   time of file birth, seconds since Epoch\n");
    printf("  %%X   time of last access, seconds since Epoch\n");
    printf("  %%Y   time of last data modification, seconds since Epoch\n");
    printf("  %%Z   time of last status change, seconds since Epoch\n");
}

/* Version information */
static void version(void)
{
    printf("stat 1.0\n");
}

/* Process a single file */
static int process_file(const char *filename)
{
    struct stat st;
    char *expanded_name;
    char **file_list;
    int i, result = SUCCESS;
    
    
    /* Expand wildcards if present */
    expanded_name = (char *)malloc(strlen(filename) + 1);
    if (!expanded_name) {
        fprintf(stderr, "stat: memory allocation failed\n");
        return FAILURE;
    }
    strcpy(expanded_name, filename);
    
    /* Convert Amiga path to Unix-style if needed */
    amigaizepath(expanded_name);
    
    /* Check if filename contains wildcards */
    if (strpbrk(expanded_name, "*?[]")) {
        file_list = wildexpand(expanded_name);
        if (file_list) {
            for (i = 0; file_list[i]; i++) {
                if (stat(file_list[i], &st) < 0) {
                    perror(file_list[i]);
                    result = FAILURE;
                } else {
                    print_file_info(file_list[i], &st);
                }
            }
            wildfree(file_list);
        } else {
            fprintf(stderr, "stat: %s: No match\n", filename);
            result = FAILURE;
        }
    } else {
        if (stat(expanded_name, &st) < 0) {
            perror(expanded_name);
            result = FAILURE;
        } else {
            print_file_info(expanded_name, &st);
        }
    }
    
    free(expanded_name);
    return result;
}

/* Print file information */
static void print_file_info(const char *filename, struct stat *st)
{
    char *mode_str, *access_time_str, *modify_time_str, *change_time_str;
    
    if (format_string) {
        /* Custom format string processing */
        char *p = format_string;
        while (*p) {
            if (*p == '%' && *(p+1)) {
                p++; /* Skip the % */
                switch (*p) {
                    case 'n': printf("%s", filename); break;
                    case 's': printf("%ld", st->st_size); break;
                    case 'b': printf("%ld", st->st_blocks); break;
                    case 'f': printf("%o", st->st_mode); break;
                    case 'a': printf("%o", st->st_mode & 07777); break;
                    case 'A': printf("%s", format_mode(st->st_mode)); break;
                    case 'u': printf("%d", st->st_uid); break;
                    case 'g': printf("%d", st->st_gid); break;
                    case 'N': printf("\"%s\"", filename); break;
                    case 'x': printf("%s", format_time(st->st_atime)); break;
                    case 'y': printf("%s", format_time(st->st_mtime)); break;
                    case 'z': printf("%s", format_time(st->st_ctime)); break;
                    case 'X': printf("%ld", st->st_atime); break;
                    case 'Y': printf("%ld", st->st_mtime); break;
                    case 'Z': printf("%ld", st->st_ctime); break;
                    case 'd': printf("%ld", st->st_dev); break;
                    case 'D': printf("%ld", st->st_rdev); break;
                    case 'i': printf("%ld", st->st_ino); break;
                    case 'h': printf("%ld", st->st_nlink); break;
                    case 'B': printf("%ld", st->st_blksize); break;
                    case 'k': printf("%ld", st->st_blocks); break;
                    case 'w': printf("-"); break; /* Birth time not available */
                    case 'W': printf("0"); break; /* Birth time not available */
                    default: printf("%%%c", *p); break; /* Unknown format, print literally */
                }
                p++;
            } else {
                putchar(*p);
                p++;
            }
        }
        printf("\n");
        return;
    }
    
    mode_str = format_mode(st->st_mode);
    access_time_str = format_time(st->st_atime);
    modify_time_str = format_time(st->st_mtime);
    change_time_str = format_time(st->st_ctime);
    
    if (terse_flag) {
        /* Terse format: single line with all information */
        printf("%ld %ld %s %ld %d %d %ld %ld %ld %ld %ld %ld %ld %ld %s\n",
               st->st_dev, st->st_ino, mode_str, st->st_nlink, st->st_uid, st->st_gid,
               st->st_rdev, st->st_size, st->st_atime, st->st_mtime, st->st_ctime,
               st->st_blksize, st->st_blocks, 0, filename);
    } else {
        /* Normal format */
        printf("  File: %s\n", filename);
        printf("  Size: %ld\t\tBlocks: %ld\t\tIO Block: %ld\n", 
               st->st_size, st->st_blocks, st->st_blksize);
        printf("Device: %ld/%ld\tInode: %ld\t\tLinks: %ld\n",
               st->st_dev, st->st_rdev, st->st_ino, st->st_nlink);
        printf("Access: (%04o/%s)  Uid: (%d)   Gid: (%d)\n",
               st->st_mode & 07777, mode_str, st->st_uid, st->st_gid);
        printf("Access: %s\n", access_time_str);
        printf("Modify: %s\n", modify_time_str);
        printf("Change: %s\n", change_time_str);
        printf(" Birth: -\n");
    }
}

/* Format file mode as string */
static char *format_mode(unsigned short mode)
{
    static char mode_str[11];
    char *p = mode_str;
    
    /* File type */
    if (S_ISDIR(mode)) *p++ = 'd';
    else if (S_ISCHR(mode)) *p++ = 'c';
    else if (S_ISBLK(mode)) *p++ = 'b';
    else if (S_ISREG(mode)) *p++ = '-';
    else if (S_ISLNK(mode)) *p++ = 'l';
    else if (S_ISSOCK(mode)) *p++ = 's';
    else if (S_ISFIFO(mode)) *p++ = 'p';
    else *p++ = '?';
    
    /* Owner permissions */
    *p++ = (mode & S_IREAD) ? 'r' : '-';
    *p++ = (mode & S_IWRITE) ? 'w' : '-';
    *p++ = (mode & S_IEXEC) ? 'x' : '-';
    
    /* Group permissions */
    *p++ = (mode & (S_IREAD >> 3)) ? 'r' : '-';
    *p++ = (mode & (S_IWRITE >> 3)) ? 'w' : '-';
    *p++ = (mode & (S_IEXEC >> 3)) ? 'x' : '-';
    
    /* Other permissions */
    *p++ = (mode & (S_IREAD >> 6)) ? 'r' : '-';
    *p++ = (mode & (S_IWRITE >> 6)) ? 'w' : '-';
    *p++ = (mode & (S_IEXEC >> 6)) ? 'x' : '-';
    
    *p = '\0';
    return mode_str;
}

/* Format time as string */
static char *format_time(time_t time)
{
    static char time_str[32];
    struct tm *tm_info;
    
    tm_info = localtime(&time);
    if (tm_info) {
        strftime(time_str, sizeof(time_str), "%Y-%m-%d %H:%M:%S", tm_info);
    } else {
        strcpy(time_str, "Unknown");
    }
    
    return time_str;
}

/* Main function */
int main(int argc, char **argv)
{
    int c, result = SUCCESS;
    int i;
    char *cmd_string = NULL;
    struct RDArgs *rdargs;
    
    /* Initialize DOSBase */
    if (!(DOSBase = (struct DosLibrary *)OpenLibrary("dos.library", 37))) {
        fprintf(stderr, "stat: Cannot open dos.library\n");
        return FAILURE;
    }
    
    
    /* Check if we should use getopt style parsing */
    if (is_getopt_style(argc, argv)) {
        /* POSIX style argument parsing */
        while ((c = getopt(argc, argv, "f:LtVh")) != -1) {
            switch (c) {
                case 'f':
                    format_string = optarg;
                    format_flag = 1;
                    break;
                case 'L':
                    /* Dereference links - not implemented yet */
                    break;
                case 't':
                    terse_flag = 1;
                    break;
                case 'V':
                    version();
                    CloseLibrary((struct Library *)DOSBase);
                    return SUCCESS;
                case 'h':
                    usage();
                    CloseLibrary((struct Library *)DOSBase);
                    return SUCCESS;
                default:
                    usage();
                    CloseLibrary((struct Library *)DOSBase);
                    return FAILURE;
            }
        }
        
        /* Process remaining arguments as filenames */
        for (i = optind; i < argc; i++) {
            if (process_file(argv[i]) != SUCCESS) {
                result = FAILURE;
            }
        }
    } else {
        /* Amiga style argument parsing with ReadArgs */
        cmd_string = build_command_string(argc, argv, NULL);
        if (!cmd_string) {
            fprintf(stderr, "stat: Failed to build command string\n");
            CloseLibrary((struct Library *)DOSBase);
            return FAILURE;
        }
        
        rdargs = setup_readargs(template, cmd_string);
        if (!rdargs) {
            fprintf(stderr, "stat: Failed to setup ReadArgs\n");
            free(cmd_string);
            CloseLibrary((struct Library *)DOSBase);
            return FAILURE;
        }
        
        
        /* Process files from ReadArgs */
        if (rdargs->RDA_Source.CS_Buffer) {
            char **file_list;
            int file_count;
            
            /* Allocate memory for file list */
            file_list = (char **)malloc(100 * sizeof(char *));
            if (!file_list) {
                perror("malloc");
                return FAILURE;
            }
            
            file_count = tokenize_string(rdargs->RDA_Source.CS_Buffer, 
                                       file_list, 100);
            for (i = 0; i < file_count; i++) {
                if (process_file(file_list[i]) != SUCCESS) {
                    result = FAILURE;
                }
            }
            
            /* Free the file list memory */
            free(file_list);
        } else {
            /* Fallback: process argv directly */
            for (i = 1; i < argc; i++) {
                if (process_file(argv[i]) != SUCCESS) {
                    result = FAILURE;
                }
            }
        }
        
        cleanup_readargs(rdargs, cmd_string);
    }
    
    CloseLibrary((struct Library *)DOSBase);
    return result;
}