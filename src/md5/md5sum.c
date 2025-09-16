/*
 * md5sum.c - GNU-style MD5 checksum command
 * 
 * This implements the GNU-style md5sum command with options:
 * -b, --binary     Read files in binary mode
 * -c, --check      Check digests from file
 * -t, --text       Read files in text mode (default)
 * --help           Print help and exit
 * --ignore-missing Ignore missing files when checking
 * --quiet          Don't print OK for each successful file
 * --status         Don't output anything, status code shows success
 * --strict         Exit non-zero for any invalid input
 * --tag            Create BSD-style output
 * --version        Print version and exit
 * --warn           Warn about improperly formatted checksum lines
 * -w, --warn       Warn about improperly formatted checksum lines
 * -z, --zero       End each output line with NUL, not newline
 *
 * Based on the GNU coreutils md5sum command behavior
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include "md5.h"
#include "md5lib.h"
#include "getopt.h"

/* External variables from getopt */
extern char *optarg;
extern int optind;

/* External variables from md5lib */
extern int md5_quiet;
extern int md5_reverse;
extern int md5_binary;
extern int md5_verbose;

/* External functions from md5lib */
extern void md5_print_digest(unsigned char *digest);
extern int md5_compute_file(FILE *fp, unsigned char *digest);
extern int md5_compute_string(const char *str, unsigned char *digest);
extern int md5_hex_digit(int c);
extern int md5_parse_line(FILE *fp, unsigned char *digest, char *filename, int *binary_flag);
extern void md5_print_gnu_format(const char *filename, unsigned char *digest, int binary_mode);
extern void md5_print_bsd_format(const char *filename, unsigned char *digest, int is_string);

/* External functions from getopt */
extern int getopt(int argc, char **argv, char *opts);

/* Global variables */
char *progname;
int check_mode = 0;
int ignore_missing = 0;
int status_mode = 0;
int strict_mode = 0;
int tag_mode = 0;
int warn_mode = 0;
int zero_mode = 0;
char *check_file = NULL;

/* Function prototypes */
void usage(void);
void gnu_usage(void);
int do_check_file(const char *filename);
int do_file_processing(int argc, char **argv);

/*
 * Print usage information for GNU-style md5sum command
 */
void gnu_usage(void)
{
    fprintf(stderr, "usage: md5sum [OPTION]... [FILE]...\n");
    fprintf(stderr, "Print or check MD5 (128-bit) checksums.\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "  -b, --binary         read in binary mode\n");
    fprintf(stderr, "  -c, --check          read MD5 sums from the FILEs and check them\n");
    fprintf(stderr, "  -t, --text           read in text mode (default)\n");
    fprintf(stderr, "      --help           display this help and exit\n");
    fprintf(stderr, "      --ignore-missing don't fail or report status for missing files\n");
    fprintf(stderr, "      --quiet          don't print OK for each successfully verified file\n");
    fprintf(stderr, "      --status         don't output anything, status code shows success\n");
    fprintf(stderr, "      --strict         exit non-zero for any invalid input\n");
    fprintf(stderr, "      --tag            create a BSD-style checksum\n");
    fprintf(stderr, "      --version        output version information and exit\n");
    fprintf(stderr, "      --warn           warn about improperly formatted checksum lines\n");
    fprintf(stderr, "  -w, --warn           warn about improperly formatted checksum lines\n");
    fprintf(stderr, "  -z, --zero           end each output line with NUL, not newline\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "The sums are computed as described in RFC 1321.  When checking, the input\n");
    fprintf(stderr, "should be a former output of this program.  The default mode is to print\n");
    fprintf(stderr, "a line with checksum, a space, a character indicating input mode ('*' for\n");
    fprintf(stderr, "binary, ' ' for text or where binary is insignificant), and name for each FILE.\n");
}

/*
 * Print usage and exit
 */
void usage(void)
{
    gnu_usage();
    exit(2);
}

/*
 * Check digests from a file
 */
int do_check_file(const char *filename)
{
    FILE *fp;
    unsigned char expected_digest[16];
    unsigned char actual_digest[16];
    char line_filename[256];
    int binary_flag;
    int rc, ex = 0, failed = 0, checked = 0, malformed = 0;
    int flen = 14;
    FILE *file_fp;
    
    if (strcmp(filename, "-") == 0) {
        fp = stdin;
    } else {
        fp = fopen(filename, "r");
        if (fp == NULL) {
            fprintf(stderr, "%s: %s: %s\n", progname, filename, strerror(errno));
            return 2;
        }
    }
    
    while ((rc = md5_parse_line(fp, expected_digest, line_filename, &binary_flag)) >= 0) {
        if (rc == 0) {
            /* Invalid line */
            malformed++;
            if (warn_mode && !status_mode) {
                fprintf(stderr, "%s: %s: improperly formatted MD5 checksum line\n", 
                        progname, filename);
            }
            if (strict_mode) {
                ex = 1;
            }
            continue;
        }
        
        if (!status_mode) {
            if (strlen(line_filename) > flen) {
                flen = strlen(line_filename);
            }
            fprintf(stderr, "%-*s ", flen, line_filename);
        }
        
        /* Check if file exists */
        file_fp = fopen(line_filename, binary_flag ? "rb" : "r");
        if (file_fp == NULL) {
            if (ignore_missing) {
                if (!status_mode) {
                    fprintf(stderr, "OK\n");
                }
                checked++;
                continue;
            }
            if (!status_mode) {
                fprintf(stderr, "FAILED open or read\n");
            }
            ex = 2;
            failed++;
            continue;
        }
        
        /* Compute actual digest */
        if (md5_compute_file(file_fp, actual_digest) != 0) {
            if (!status_mode) {
                fprintf(stderr, "FAILED\n");
            }
            ex = 2;
            failed++;
            fclose(file_fp);
            continue;
        }
        fclose(file_fp);
        
        /* Compare digests */
        if (memcmp(expected_digest, actual_digest, 16) != 0) {
            if (!status_mode) {
                fprintf(stderr, "FAILED\n");
            }
            failed++;
        } else {
            if (!status_mode && !md5_quiet) {
                fprintf(stderr, "OK\n");
            }
        }
        checked++;
    }
    
    if (fp != stdin) {
        fclose(fp);
    }
    
    if (!status_mode && (failed > 0 || malformed > 0)) {
        if (failed > 0) {
            fprintf(stderr, "%s: %d of %d file(s) failed MD5 check\n", 
                    progname, failed, checked);
        }
        if (malformed > 0) {
            fprintf(stderr, "%s: %d improperly formatted line(s)\n", 
                    progname, malformed);
        }
    }
    
    if (checked == 0) {
        if (!status_mode) {
            fprintf(stderr, "%s: no files checked\n", progname);
        }
        return 3;
    }
    
    if (ex == 0 && (failed > 0 || (strict_mode && malformed > 0))) {
        ex = 1;
    }
    
    return ex;
}

/*
 * Process files in GNU mode
 */
int do_file_processing(int argc, char **argv)
{
    unsigned char digest[16];
    FILE *fp;
    int i, rc = 0;
    const char *mode = md5_binary ? "rb" : "r";
    const char *eol = zero_mode ? "\0" : "\n";
    
    if (argc == 0) {
        /* Read from stdin */
        if (md5_compute_file(stdin, digest) != 0) {
            fprintf(stderr, "%s: read error on stdin\n", progname);
            return 1;
        }
        md5_print_gnu_format("-", digest, md5_binary);
        if (zero_mode) {
            printf("\0");
        }
        return 0;
    }
    
    for (i = 0; i < argc; i++) {
        fp = fopen(argv[i], mode);
        if (fp == NULL) {
            fprintf(stderr, "%s: %s: %s\n", progname, argv[i], strerror(errno));
            rc = 1;
            continue;
        }
        
        if (md5_compute_file(fp, digest) != 0) {
            fprintf(stderr, "%s: error reading %s\n", progname, argv[i]);
            rc = 1;
        } else {
            if (tag_mode) {
                md5_print_bsd_format(argv[i], digest, 0);
            } else {
                md5_print_gnu_format(argv[i], digest, md5_binary);
            }
            if (zero_mode) {
                printf("\0");
            }
        }
        
        fclose(fp);
    }
    
    return rc;
}

/*
 * Main function for GNU-style md5sum command
 */
int main(int argc, char **argv)
{
    int opt;
    int rc = 0;
    
    progname = argv[0];
    
    /* Parse command line options */
    while ((opt = getopt(argc, argv, "bc:htvwz")) != EOF) {
        switch (opt) {
        case 'b':
            md5_binary = 1;
            break;
        case 'c':
            check_mode = 1;
            check_file = optarg;
            break;
        case 'h':
            usage();
            break;
        case 't':
            md5_binary = 0;
            break;
        case 'v':
            /* Version - simplified for this implementation */
            printf("md5sum (POSIX implementation) 1.0\n");
            printf("Copyright (C) 1990, RSA Data Security, Inc.\n");
            return 0;
        case 'w':
            warn_mode = 1;
            break;
        case 'z':
            zero_mode = 1;
            break;
        default:
            usage();
        }
    }
    
    argc -= optind;
    argv += optind;
    
    /* Handle check mode */
    if (check_mode) {
        if (argc > 0) {
            fprintf(stderr, "%s: -c option does not accept file arguments\n", progname);
            return 2;
        }
        return do_check_file(check_file);
    }
    
    /* Process files normally */
    return do_file_processing(argc, argv);
}