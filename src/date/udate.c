/*
 * date - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * This version integrates AmigaDOS ReadArgs and standard POSIX getopt
 * for command-line parsing, providing both POSIX compatibility and
 * Amiga native functionality using dos.library clock functions.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <stdarg.h>
#include <exec/types.h>
#include <dos/dos.h>
#include <dos/dosasl.h>
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

/* Version tag for Amiga */
static const char *verstag = "$VER: date 2.0 (12/09/25)\n";
static const char *stack_cookie = "$STACK: 4096";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* External function declarations from common library */
extern char *my_basename(char *path);
extern int is_getopt_style(int argc, char **argv);
extern char *build_command_string(int argc, char **argv, const char *exclude);
extern int tokenize_string(char *str, char **argv, int max_args);
extern void reset_getopt(void);
extern int getopt(int argc, char * const argv[], const char *optstring);
extern char *optarg;
extern int optind;
extern char **wildexpand(char *w);
extern void wildfree(char **freelist);
extern int amigaizepath(char *to);

extern struct DosLibrary *DOSBase;

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

/* Function prototypes */
int ampm(int hour);
int dateset(char name[], char tempy[], int length, int daymax[13], 
            char month[13][4], char monthext[13][7], char node[], 
            unsigned char clock[8]);
void defaultdate(int tznflag, char *timezone, char day[7][4], 
                 char dayext[7][4], char month[13][4], 
                 unsigned char clock[8], int year_fix);
char *ext(int day);
void mistake(char description[], char node[]);
void quitcheck(char xxx[]);
void setdate(unsigned char clock[8], int daymax[13], char month[13][4], 
             char monthext[13][7], char node[]);
int twelve(int hour);
int isdst(unsigned char clock[8]);

/* For ReadArgs template */
enum {
    ARG_FORMAT,
    ARG_SET,
    ARG_UTC,
    ARG_REFERENCE,
    ARG_DEBUG,
    ARG_POSIX,
    ARG_HELP_FORMAT,
    ARG_DST,
    ARG_COUNT
};

/* Global options structure */
typedef struct {
    BOOL set_flag;              /* -s: set date/time */
    BOOL utc_flag;              /* -u: use UTC instead of local time */
    BOOL debug_flag;            /* -d: debug mode */
    BOOL help_format_flag;      /* --help-format: show format help */
    BOOL dst_flag;              /* --dst: adjust for DST */
    char *reference_file;       /* -r: reference file for -d */
    char *date_string;          /* -d: date string to display */
    char *format_string;        /* format string for output */
    int exit_code;              /* Exit code */
} DateOptions;

/* Function declarations for forward references */
void usage(const char *program);
void print_version(const char *program);
int run_date_logic(DateOptions *options, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, DateOptions *options, int *file_start, const char *program);
void init_options(DateOptions *options);
void cleanup_options(DateOptions *options);
int parse_date_string(const char *date_str, unsigned char *clock, const char *program);
void show_format_help(void);
void adjust_dst(void);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    DateOptions options;
    int file_start = 1;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FORMAT/K,SET/S,UTC/S,REFERENCE/K,DEBUG/S,POSIX/K/F,HELPFORMAT/S,DST/S";
    LONG arg_array[ARG_COUNT] = {0};
    struct RDArgs *rdargs = NULL;
    char *cmd_string = NULL;
    int ret_code = SUCCESS;
    BOOL interactive_help = FALSE;
    
    /* POSIX/F Path Variables */
    char *posix_str;
    int new_argc;
    char *new_argv[MAX_TEMPLATE_ITEMS];
    int i;
    int file_count;
    char initial_args_str[256];
    char user_input_buf[256];

    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);

    if (argc == 1) {
        /* No arguments, show default date */
        init_options(&options);
        return run_date_logic(&options, 0, NULL, program);
    }

    /* Check for special commands first */
    if (argc > 1) {
        if (strcmp(argv[1], "--help-format") == 0) {
            show_format_help();
            return SUCCESS;
        }
        if (strcmp(argv[1], "--dst") == 0) {
            adjust_dst();
            return SUCCESS;
        }
    }

    /* Initialize options */
    init_options(&options);

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &options, &file_start, program);
        files = &argv[file_start];
        file_count = argc - file_start;
        
        return run_date_logic(&options, file_count, files, program);
        
    } else {
        /* --- READARGS PATH --- */
        for (i = 1; i < argc; i++) {
            if (strcmp(argv[i], "?") == 0) {
                interactive_help = TRUE;
                break;
            }
        }

        rdargs = AllocDosObject(DOS_RDARGS, NULL);
        if (!rdargs) {
            fprintf(stderr, "%s: out of memory for RDArgs\n", program);
            cleanup_options(&options);
            return FAILURE;
        }

        if (interactive_help) {
            /* Interactive help mode - output ReadArgs template */
            printf("%s: ", template);
            
            if (fgets(user_input_buf, sizeof(user_input_buf), stdin)) {
                /* Remove newline */
                user_input_buf[strcspn(user_input_buf, "\n")] = '\0';
                
                if (strlen(user_input_buf) > 0) {
                    /* Process POSIX arguments */
                    posix_str = user_input_buf;
                    
                    /* Build initial arguments string */
                    snprintf(initial_args_str, sizeof(initial_args_str), "%s %s", program, posix_str);
                    
                    /* Tokenize the string */
                    new_argc = tokenize_string(initial_args_str, new_argv, MAX_TEMPLATE_ITEMS);
                    
                    if (new_argc > 1) {
                        /* Parse as POSIX arguments */
                        parse_getopt_args(new_argc, new_argv, &options, &file_start, program);
                        files = &new_argv[file_start];
                        file_count = new_argc - file_start;
                        
                        ret_code = run_date_logic(&options, file_count, files, program);
                    } else {
                        usage(program);
                        ret_code = FAILURE;
                    }
                } else {
                    /* Use Amiga ReadArgs style */
                    cmd_string = build_command_string(argc, argv, "?");
                    if (!cmd_string) {
                        fprintf(stderr, "%s: out of memory\n", program);
                        cleanup_options(&options);
                        FreeDosObject(DOS_RDARGS, rdargs);
                        return FAILURE;
                    }
                    
                    rdargs->RDA_Source.CS_Buffer = cmd_string;
                    rdargs->RDA_Source.CS_Length = strlen(cmd_string);
                    
                    if (ReadArgs(template, arg_array, rdargs)) {
                        /* Process ReadArgs results */
                        if (arg_array[ARG_SET]) options.set_flag = TRUE;
                        if (arg_array[ARG_UTC]) options.utc_flag = TRUE;
                        if (arg_array[ARG_DEBUG]) options.debug_flag = TRUE;
                        if (arg_array[ARG_REFERENCE]) options.reference_file = (char *)arg_array[ARG_REFERENCE];
                        if (arg_array[ARG_FORMAT]) options.format_string = (char *)arg_array[ARG_FORMAT];
                        
                        files = NULL;
                        file_count = 0;
                        
                        ret_code = run_date_logic(&options, file_count, files, program);
                    } else {
                        fprintf(stderr, "%s: invalid arguments\n", program);
                        usage(program);
                        ret_code = FAILURE;
                    }
                }
            } else {
                usage(program);
                ret_code = FAILURE;
            }
        } else {
            /* Standard ReadArgs processing */
            cmd_string = build_command_string(argc, argv, NULL);
            if (!cmd_string) {
                fprintf(stderr, "%s: out of memory\n", program);
                cleanup_options(&options);
                FreeDosObject(DOS_RDARGS, rdargs);
                return FAILURE;
            }
            
            rdargs->RDA_Source.CS_Buffer = cmd_string;
            rdargs->RDA_Source.CS_Length = strlen(cmd_string);
            
            if (ReadArgs(template, arg_array, rdargs)) {
                /* Process ReadArgs results */
                if (arg_array[ARG_SET]) options.set_flag = TRUE;
                if (arg_array[ARG_UTC]) options.utc_flag = TRUE;
                if (arg_array[ARG_DEBUG]) options.debug_flag = TRUE;
                if (arg_array[ARG_REFERENCE]) options.reference_file = (char *)arg_array[ARG_REFERENCE];
                if (arg_array[ARG_FORMAT]) options.format_string = (char *)arg_array[ARG_FORMAT];
                
                files = NULL;
                file_count = 0;
                
                ret_code = run_date_logic(&options, file_count, files, program);
            } else {
                fprintf(stderr, "%s: invalid arguments\n", program);
                usage(program);
                ret_code = FAILURE;
            }
        }

        /* Clean up */
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
    }

    cleanup_options(&options);
    return ret_code;
}

/*-------------------------------------------------------------------------*/

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param options Options structure to populate
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, DateOptions *options, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "d:r:suVh")) != -1) {
        switch (c) {
            case 'd':
                options->date_string = optarg;
                    break;  
            case 'r':
                options->reference_file = optarg;
                break;
            case 's':
                options->set_flag = TRUE;
                break;
            case 'u':
                options->utc_flag = TRUE;
                break;
            case 'V':
                print_version(program);
                exit(SUCCESS);
                break;
            case 'h':
                usage(program);
                exit(SUCCESS);
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    *file_start = optind;
}

/**
 * @brief Core date logic separated from argument parsing
 * @param options Options structure
 * @param file_count Number of files to process
 * @param files Array of file names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_date_logic(DateOptions *options, int file_count, char **files, const char *program) {
    /* Variable declarations - all at the beginning of the block */
    int i, j, nflag, dflag, counter;
    char leadspacer[80], *odata, *timezone, node[33];
char format[80];
    int escape_char, tznflag, y_fix;
unsigned char clock[8];
    int daymax[13];
    char day[7][4];
    char dayext[7][4];
    char month[13][4];
    char monthext[13][7];
    
    /* Initialize variables */
    nflag = 0;
    dflag = 0;
    counter = -1;
    escape_char = '%';  /* POSIX compliant escape character */
    tznflag = 0;
    y_fix = 80;
leadspacer[0] = '\0';

    /* Initialize arrays */
    daymax[0] = 0; daymax[1] = 31; daymax[2] = 28; daymax[3] = 31;
    daymax[4] = 30; daymax[5] = 31; daymax[6] = 30; daymax[7] = 31;
    daymax[8] = 31; daymax[9] = 30; daymax[10] = 31; daymax[11] = 30; daymax[12] = 31;
    
    strcpy(day[0], "Sun"); strcpy(day[1], "Mon"); strcpy(day[2], "Tue");
    strcpy(day[3], "Wed"); strcpy(day[4], "Thu"); strcpy(day[5], "Fri"); strcpy(day[6], "Sat");
    
    strcpy(dayext[0], ""); strcpy(dayext[1], ""); strcpy(dayext[2], "s");
    strcpy(dayext[3], "nes"); strcpy(dayext[4], "rs"); strcpy(dayext[5], ""); strcpy(dayext[6], "ur");
    
    strcpy(month[0], ""); strcpy(month[1], "Jan"); strcpy(month[2], "Feb");
    strcpy(month[3], "Mar"); strcpy(month[4], "Apr"); strcpy(month[5], "May");
    strcpy(month[6], "Jun"); strcpy(month[7], "Jul"); strcpy(month[8], "Aug");
    strcpy(month[9], "Sep"); strcpy(month[10], "Oct"); strcpy(month[11], "Nov"); strcpy(month[12], "Dec");
    
    strcpy(monthext[0], ""); strcpy(monthext[1], "uary"); strcpy(monthext[2], "ruary");
    strcpy(monthext[3], "ch"); strcpy(monthext[4], "il"); strcpy(monthext[5], "");
    strcpy(monthext[6], "e"); strcpy(monthext[7], "y"); strcpy(monthext[8], "ust");
    strcpy(monthext[9], "tember"); strcpy(monthext[10], "ober"); strcpy(monthext[11], "ember"); strcpy(monthext[12], "ember");

    /* Get program name */
    stcgfn(node, (char *)program);

    /* Handle setting date/time if requested */
    if (options->set_flag) {
        if (file_count > 0 && files[0]) {
            /* Parse date string and set clock */
            if (!parse_date_string(files[0], clock, program)) {
                return FAILURE;
            }
            /* Set the system clock */
            if (chgclk(clock)) {
                fprintf(stderr, "%s: cannot set system clock\n", program);
                return FAILURE;
            }
            return SUCCESS;
        } else {
            /* Interactive date setting */
            setdate(clock, daymax, month, monthext, node);
            return SUCCESS;
        }
    }

    /* Handle DST adjustment if requested */
    if (options->debug_flag) {
        if(isdst(clock)) {
                    clock[4]++;
            if(chgclk(clock)) mistake("Can't Correct for DST", node);
                    }
                 getclk(clock);
                 dflag++;
    }


    /* Handle date string if requested */
    if (options->date_string) {
        if (!parse_date_string(options->date_string, clock, program)) {
            return FAILURE;
        }
    } else if (options->reference_file) {
        struct FileInfoBlock *fib;
        struct DateStamp ds;
        BPTR lock;
        
        fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL);
        if (fib) {
            lock = Lock((char *)options->reference_file, ACCESS_READ);
            if (lock) {
                if (Examine(lock, fib)) {
                ds = fib->fib_Date;
                /* Convert Amiga DateStamp to our clock format */
                clock[0] = ds.ds_Days % 7;  /* day of week */
                clock[1] = ds.ds_Days / 365;  /* year (simplified) */
                clock[2] = (ds.ds_Days % 365) / 30;  /* month (simplified) */
                clock[3] = (ds.ds_Days % 365) % 30;  /* day (simplified) */
                clock[4] = ds.ds_Minute / 60;  /* hour */
                clock[5] = ds.ds_Minute % 60;  /* minute */
                clock[6] = ds.ds_Tick / 50;  /* second (50 ticks per second) */
                } else {
                    fprintf(stderr, "%s: cannot examine '%s'\n", program, options->reference_file);
                    UnLock(lock);
                    FreeDosObject(DOS_FIB, fib);
                    return FAILURE;
                }
                UnLock(lock);
            } else {
                fprintf(stderr, "%s: cannot access '%s'\n", program, options->reference_file);
                FreeDosObject(DOS_FIB, fib);
                return FAILURE;
            }
            FreeDosObject(DOS_FIB, fib);
        } else {
            fprintf(stderr, "%s: out of memory for FileInfoBlock\n", program);
            return FAILURE;
        }
    } else {
        getclk(clock);   /*  gets current system clock settings  */
    }
    if(clock[1] > 19) y_fix = -20;

/*---- SET TIMEZONE if set ------------------------------------------------*/
timezone = getenv("TIMEZONE");
    if (timezone) {
i = strlen(timezone);
if(i == 3 || i == 7) {
   if(isdst(clock) && i == 7) {
      timezone[0] = timezone[4]; 
      timezone[1] = timezone[5]; 
      timezone[2] = timezone[6]; 
      }
   timezone[3] = '\0'; tznflag++;
        } else {
   timezone[0] = '\0';
        }
    } else {
        timezone = "";
   }
/*------------------------------------------------------------------------*/

    printf("%s", leadspacer);

/*-----------------------------*/
    if (options->format_string) {
        strcpy(format, options->format_string);
    } else {
      odata = getenv("DEFAULT");
        if(odata && odata[0] != '\0') {
            strcpy(format, odata);
        } else {
            defaultdate(tznflag, timezone, day, dayext, month, clock, y_fix);
         if(!nflag) printf("\n");
            return SUCCESS;
         }
   }
/*-----------------------------*/

    /* Format and output the date */
    while(format[++counter] != '\0') {
        if(format[counter] == escape_char) {
            if(format[++counter] == escape_char) {
                printf("%c", escape_char);
        continue;
        }
      switch(format[counter]) {
                /* POSIX compliant format specifiers */
                case 'a':  printf("%s", day[clock[0]]); break;  /* abbreviated weekday name */
                case 'A':  printf("%s%sday", day[clock[0]], dayext[clock[0]]); break;  /* full weekday name */
                case 'b':  printf("%s", month[clock[2]]); break;  /* abbreviated month name */
                case 'B':  printf("%s%s", month[clock[2]], monthext[clock[2]]); break;  /* full month name */
                case 'c':  printf("%s %02d-%s-%02d %02d:%02d", day[clock[0]], clock[3], month[clock[2]], clock[1] + y_fix, clock[4], clock[5]); break;  /* date and time */
                case 'd':  printf("%02d", clock[3]); break;  /* day of month 01 to 31 */
                case 'D':  printf("%02d/%02d/%02d", clock[2], clock[3], clock[1] + y_fix); break;  /* date as mm/dd/yy */
                case 'e':  printf("%2d", clock[3]); break;  /* day of month 1 to 31 */
                case 'F':  printf("%04d-%02d-%02d", clock[1] + 1980, clock[2], clock[3]); break;  /* full date */
                case 'H':  printf("%02d", clock[4]); break;  /* hour 00 to 23 */
                case 'I':  printf("%02d", twelve(clock[4])); break;  /* hour 01 to 12 */
                case 'j':  /* julian date */
                    if((clock[1] + 1980) % 4 == 0) daymax[2] = 29;
                    for(i = 1, j = 0; i != clock[2]; j += daymax[i++]);
                    j += clock[3];
                    printf("%d", j);
                    break;
                case 'm':  printf("%02d", clock[2]); break;  /* month of year 01-12 */
                case 'M':  printf("%02d", clock[5]); break;  /* minute 00-59 */
                case 'n':  printf("\n"); break;  /* newline */
                case 'p':  printf("%cM", ampm(clock[4])); break;  /* AM/PM */
                case 'r':  printf("%02d:%02d:%02d %cM", twelve(clock[4]), clock[5], clock[6], ampm(clock[4])); break;  /* 12-hour time */
                case 'R':  printf("%02d:%02d", clock[4], clock[5]); break;  /* 24-hour time */
                case 'S':  printf("%02d", clock[6]); break;  /* seconds 00-59 */
                case 't':  printf("\t"); break;  /* tab */
                case 'T':  printf("%02d:%02d:%02d", clock[4], clock[5], clock[6]); break;  /* time */
                case 'u':  printf("%d", (clock[0] == 0) ? 7 : clock[0]); break;  /* day of week 1-7, Monday=1 */
                case 'w':  printf("%d", clock[0]); break;  /* day of week 0-6, Sunday=0 */
                case 'y':  printf("%d", clock[1] + y_fix); break;  /* two digit year */
                case 'Y':  printf("%4d", clock[1] + 1980); break;  /* four digit year */
                case 'Z':  if(tznflag) printf("%s", timezone); break;  /* timezone name */
                
                /* Additional POSIX format specifiers */
                case 'g':  /* last two digits of year of ISO week number */
                    printf("%02d", (clock[1] + 1980) % 100);
                    break;
                case 'G':  /* year of ISO week number */
                    printf("%4d", clock[1] + 1980);
                    break;
                case 'h':  /* same as %b */
                    printf("%s", month[clock[2]]);
                    break;
                case 'k':  /* hour, space padded (0..23) */
                    printf("%2d", clock[4]);
                    break;
                case 'l':  /* hour, space padded (1..12) */
                    printf("%2d", twelve(clock[4]));
                    break;
                case 'N':  /* nanoseconds (not available on Amiga, show 0) */
                    printf("000000000");
                    break;
                case 'P':  /* like %p, but lower case */
                    printf("%cm", ampm(clock[4]));
                    break;
                case 's':  /* seconds since 1970-01-01 00:00:00 UTC (simplified) */
                    {
                        long days_since_1970 = (clock[1] + 1980 - 1970) * 365 + 
                                              ((clock[1] + 1980 - 1970) / 4);
                        long seconds_since_1970 = days_since_1970 * 86400 + 
                                                 clock[4] * 3600 + clock[5] * 60 + clock[6];
                        printf("%ld", seconds_since_1970);
                    }
                    break;
                case 'U':  /* week number of year, with Sunday as first day of week */
                    {
                        int day_of_year = 0;
                        if((clock[1] + 1980) % 4 == 0) daymax[2] = 29;
                        for(i = 1, day_of_year = 0; i != clock[2]; day_of_year += daymax[i++]);
                        day_of_year += clock[3];
                        printf("%02d", (day_of_year + 6 - clock[0]) / 7);
                    }
                    break;
                case 'V':  /* ISO week number, with Monday as first day of week */
                    {
                        int day_of_year = 0;
                        int wday;
                        int week;
                        
                        if((clock[1] + 1980) % 4 == 0) daymax[2] = 29;
                        for(i = 1, day_of_year = 0; i != clock[2]; day_of_year += daymax[i++]);
                        day_of_year += clock[3];
                        wday = (clock[0] == 0) ? 7 : clock[0];  /* Monday = 1 */
                        week = (day_of_year - wday + 10) / 7;
                        if (week < 1) week = 52;
                        if (week > 52) week = 1;
                        printf("%02d", week);
                    }
                    break;
                case 'W':  /* week number of year, with Monday as first day of week */
                    {
                        int day_of_year = 0;
                        int wday;
                        
                        if((clock[1] + 1980) % 4 == 0) daymax[2] = 29;
                        for(i = 1, day_of_year = 0; i != clock[2]; day_of_year += daymax[i++]);
                        day_of_year += clock[3];
                        wday = (clock[0] == 0) ? 7 : clock[0];  /* Monday = 1 */
                        printf("%02d", (day_of_year - wday + 6) / 7);
                    }
                    break;
                case 'X':  /* locale's time representation */
                    printf("%02d:%02d:%02d", clock[4], clock[5], clock[6]);
                    break;
                case 'z':  /* +hhmm numeric timezone (simplified) */
                    printf("+0000");  /* Amiga doesn't have timezone info */
                    break;
                case ':':  /* timezone with colon - check next char */
                    if (format[counter + 1] == 'z') {
                        counter++;
                        printf("+00:00");
                    } else if (format[counter + 1] == ':' && format[counter + 2] == 'z') {
                        counter += 2;
                        printf("+00:00:00");
                    } else if (format[counter + 1] == ':' && format[counter + 2] == ':' && format[counter + 3] == 'z') {
                        counter += 3;
                        printf("+00:00:00");
                    } else {
                        printf(":");
                    }
                    break;

                /* Amiga-specific extensions (using % prefix for compatibility) */
                case 'Q':  /* greeting and date and time (Amiga extension) */
                    printf("Good "); 
                    if(clock[4] < 12) {
                       printf("Morning");
                    } else if(clock[4] < 17) {
                       printf("Afternoon");
                    } else {
                       printf("Evening");
                       }
                    printf("!    ");
                   printf("Today is %s%sday, %s%s %d%s, %02d at %d:%02d %cM",
                        day[clock[0]], dayext[clock[0]], month[clock[2]], monthext[clock[2]],
                        clock[3], ext(clock[3]), clock[1] + 1980, twelve(clock[4]), clock[5], ampm(clock[4]));
                    if(tznflag) printf(" (%s)", timezone);
                    break;
                case 'K':  /* date/time string (Amiga extension) */
                    printf("Today is %s%sday, %s%s %d%s, %02d at %d:%02d %cM",
                        day[clock[0]], dayext[clock[0]], month[clock[2]], monthext[clock[2]],
                        clock[3], ext(clock[3]), clock[1] + 1980, twelve(clock[4]), clock[5], ampm(clock[4]));
                    if(tznflag) printf(" (%s)", timezone);
                    break;  
                case 'x':  printf("%s", ext(clock[3])); break;  /* date extension (st, nd, rd, th) */
                case 'q':  printf("\""); break;  /* quote character */
                
                /* Amiga color codes (using % prefix) */
                case '0':  printf("[30m"); break;  /* Blue pen color */
                case '1':  printf("[31m"); break;  /* White pen color */
                case '2':  printf("[32m"); break;  /* Black pen color */
                case '3':  printf("[33m"); break;  /* Orange pen color */
                case '4':  printf("[0m"); break;   /* Default colors */
                case '5':  printf("[1m"); break;   /* Boldface */
                case '6':  printf("[4m"); break;   /* Underline */
                case '7':  printf("[3m"); break;   /* Italics */
                case ')':  printf("[40m"); break;  /* color 0 bkgnd */
                case '!':  printf("[41m"); break;  /* color 1 bkgnd */
                case '@':  printf("[42m"); break;  /* color 2 bkgnd */
                case '#':  printf("[43m"); break;  /* color 3 bkgnd */

                default:  printf("\n\07  ERROR: %s  [33mBad Format Character.[0m \\(%c%c\\)\n\n",
                            node, format[--counter], format[++counter]);
                         return FAILURE;
            }
        } else {
      putchar(format[counter]);
      }
   }
    
    printf("[0m");  /* Reset colors */
if(!nflag) printf("\n");
    return SUCCESS;
}

/**
 * @brief Initialize options structure
 * @param options Options structure to initialize
 */
void init_options(DateOptions *options) {
    options->set_flag = FALSE;
    options->utc_flag = FALSE;
    options->debug_flag = FALSE;
    options->help_format_flag = FALSE;
    options->dst_flag = FALSE;
    options->reference_file = NULL;
    options->date_string = NULL;
    options->format_string = NULL;
    options->exit_code = SUCCESS;
}

/**
 * @brief Clean up options structure
 * @param options Options structure to clean up
 */
void cleanup_options(DateOptions *options) {
    /* Nothing to clean up for this structure */
    (void)options;
}

/**
 * @brief Print usage information
 * @param program Program name
 */
void usage(const char *program) {
    printf("Usage (POSIX): %s [OPTION]... [+FORMAT] or %s [-u|--utc|--universal] [MMDDhhmm[[CC]YY][.ss]]\n", program, program);
    printf("Usage (Amiga): %s [FORMAT/K] [SET/S] [UTC/S] [REFERENCE/K] [DEBUG/S] [POSIX/K/F]\n", program);
    printf("Display the current time in the given FORMAT, or set the system date.\n\n");
    printf("  -d, --date=STRING          display time described by STRING, not 'now'\n");
    printf("  -r, --reference=FILE       display the last modification time of FILE\n");
    printf("  -s, --set=STRING           set time described by STRING\n");
    printf("  -u, --utc, --universal     print or set Coordinated Universal Time\n");
    printf("  -h, --help                 display this help and exit\n");
    printf("  -V, --version              output version information and exit\n");
    printf("      --help-format          show format specifier help\n");
    printf("      --dst                  adjust for Daylight Saving Time\n\n");
    printf("FORMAT controls the output.  Interpreted sequences are:\n");
    printf("  %%%%a     locale's abbreviated weekday name (e.g., Sun)\n");
    printf("  %%%%A     locale's full weekday name (e.g., Sunday)\n");
    printf("  %%%%b     locale's abbreviated month name (e.g., Jan)\n");
    printf("  %%%%B     locale's full month name (e.g., January)\n");
    printf("  %%%%c     locale's date and time (e.g., Thu Mar  3 23:05:25 2005)\n");
    printf("  %%%%d     day of month (01..31)\n");
    printf("  %%%%e     day of month, space padded ( 1..31)\n");
    printf("  %%%%F     full date; same as %%%%Y-%%%%m-%%%%d\n");
    printf("  %%%%g     last two digits of year of ISO week number (see %%%%G)\n");
    printf("  %%%%G     year of ISO week number (see %%%%V); normally useful only with %%%%V\n");
    printf("  %%%%h     same as %%%%b\n");
    printf("  %%%%H     hour (00..23)\n");
    printf("  %%%%I     hour (01..12)\n");
    printf("  %%%%j     day of year (001..366)\n");
    printf("  %%%%k     hour, space padded ( 0..23); same as %%%%H\n");
    printf("  %%%%l     hour, space padded ( 1..12); same as %%%%I\n");
    printf("  %%%%m     month (01..12)\n");
    printf("  %%%%M     minute (00..59)\n");
    printf("  %%%%n     a newline\n");
    printf("  %%%%N     nanoseconds (000000000..999999999)\n");
    printf("  %%%%p     locale's equivalent of either AM or PM; blank if not known\n");
    printf("  %%%%P     like %%%%p, but lower case\n");
    printf("  %%%%r     locale's 12-hour clock time (e.g., 11:11:04 PM)\n");
    printf("  %%%%R     24-hour hour and minute; same as %%%%H:%%%%M\n");
    printf("  %%%%s     seconds since 1970-01-01 00:00:00 UTC\n");
    printf("  %%%%S     second (00..60)\n");
    printf("  %%%%t     a tab\n");
    printf("  %%%%T     time; same as %%%%H:%%%%M:%%%%S\n");
    printf("  %%%%u     day of week (1..7); 1 is Monday\n");
    printf("  %%%%U     week number of year, with Sunday as first day of week (00..53)\n");
    printf("  %%%%V     ISO week number, with Monday as first day of week (01..53)\n");
    printf("  %%%%w     day of week (0..6); 0 is Sunday\n");
    printf("  %%%%W     week number of year, with Monday as first day of week (00..53)\n");
    printf("  %%%%x     locale's date representation (e.g., 12/31/99)\n");
    printf("  %%%%X     locale's time representation (e.g., 23:13:48)\n");
    printf("  %%%%y     last two digits of year (00..99)\n");
    printf("  %%%%Y     year\n");
    printf("  %%%%z     +hhmm numeric timezone (e.g., -0400)\n");
    printf("  %%%%:z    +hh:mm numeric timezone (e.g., -04:00)\n");
    printf("  %%%%::z   +hh:mm:ss numeric timezone (e.g., -04:00:00)\n");
    printf("  %%%%:::z  numeric timezone with : to necessary precision (e.g., -04, +05:30)\n");
    printf("  %%%%Z     alphabetic timezone abbreviation (e.g., EDT)\n\n");
    printf("By default, date pads numeric fields with zeroes.\n");
    printf("The following optional flags can follow '%%%%':\n");
    printf("  -      (hyphen) do not pad the field\n");
    printf("  _      (underscore) pad with spaces\n");
    printf("  0      (zero) pad with zeros\n");
    printf("  ^      use upper case if possible\n");
    printf("  #      use opposite case if possible\n\n");
    printf("After any flags comes an optional field width, as a decimal number;\n");
    printf("then an optional modifier, which is either E to use the locale's\n");
    printf("alternate representation if available, or O to use the locale's\n");
    printf("alternate numeric symbols if available.\n");
}

/**
 * @brief Print version information
 * @param program Program name
 */
void print_version(const char *program) {
    printf("%s", verstag);
}

/*-------------------------------------------------------------------------*/

/*  AMPM FUNCTION  */

int ampm(int hour)
{
    if(hour < 12) {
        return('A');
    } else {
        return('P');
    }
}

/*-------------------------------------------------------------------------*/

/*  DATESET FUNCTION  !!!! */

int dateset(char name[], char tempy[], int length, int daymax[13], 
            char month[13][4], char monthext[13][7], char node[], 
            unsigned char clock[8])
{
    int h, i, j, k, slash, colon;
char date[80];
char *s1, *s2, temp[3][6];
static char delim[] = "/-:";
    
    /* Initialize variables */
    h = 0;
    k = 0;
    slash = 0;
    colon = 0;

strcpy(date,tempy);                  /* this is necessary for some reason */
for( i = 0 ; i != length ; i++ ) {   /* or else argv[1] gets changed      */
   if(date[i] == '\\') return(1);    /* I have no idea why ????           */  
   else if(date[i] == ':') colon++;
   else if(date[i] == '/' || date[i] == '-') slash++;
   else if(!isdigit(date[i])) return(1);
   }
if((colon && slash) || (!colon && !slash)) return(1);
i = strlen(date);
if(colon && i != 4 && i != 5) return(1);

s1 = date;
while((s2 = strtok(s1,delim)) != NULL) {      /* break out seperate month */
   strcpy(temp[h],s2);                        /* day, year, hour & minute */
   if(h++ > 3) break;
   s1 = NULL;
   }
if(colon > 1 || slash > 2) return(1); 

i =  atoi(temp[0]); j = atoi(temp[1]); 

if(slash) {     /*  if a 'date' was entered  */
   if(h == 2) {
      k = clock[1];  
      }
      else {
      k = atoi(temp[2]);
      if(k < 0 || k > 99) mistake("Invalid Year",node);
      if(k >= 80) clock[1] = k - 80; else clock[1] = k + 20;
      }
   if((clock[1] + 1980) % 4 == 0) daymax[2] = 29; else daymax[2] = 28;
   if(i < 0 || i > 12 || j < 1 || j > daymax[i]) mistake("Invalid Date",node); 
   clock[2] = i; clock[3] = j; 
   chgclk(clock);
   }

else if(colon) {    /*  if a 'time' was entered  */
   if(i < 0 || i > 23 || j < 0 || j > 59) mistake("Invalid Time",node);
   clock[4] = i;
   clock[5] = j;
   clock[6] = clock[7] = 0;
   chgclk(clock);
   }
/*  confirm the changes  */
   printf("\n%sDate: %s%s %d%s, %d     %sTime: %02d:%02d:%02d\n\n",
           slash ? "New ":"",
           month[clock[2]],
           monthext[clock[2]],
           clock[3],
           ext(clock[3]),
           clock[1] + 1980,
           colon ? "New ":"",
           clock[4],
           clock[5],
           clock[6]);

   return(0);   /*  return '0' for success  */
}

/*-------------------------------------------------------------------------*/

/*  DEFAULTDATE FUNCTION  */

void defaultdate(int tznflag, char *timezone, char day[7][4], 
                 char dayext[7][4], char month[13][4], 
                 unsigned char clock[8], int year_fix)
{
printf("%s%sday %02d-%s-%02d %02d:%02d:%02d",
            day[clock[0]],
            dayext[clock[0]],
            clock[3],
            month[clock[2]],
            clock[1] + year_fix,
            clock[4],
            clock[5],
            clock[6]);
if(tznflag) printf(" %s",timezone);
      
}

/*-------------------------------------------------------------------------*/

/*  EXT FUNCTION  */

char *ext(int day)
{
switch(day) {
   case  1:
   case 21:
   case 31: return("st"); 
   case  2:
   case 22: return("nd");
   case  3:
   case 23: return("rd");
   default: return("th");
   }
}

/*-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*/

/*  ISDST FUNCTION  */

int isdst(unsigned char clock[8])
{
    int year, startyear, sday, eday, leapyear, dstflag;

    /* Initialize variables */
    startyear = 1980;
    sday = 6;
    leapyear = 0;
    dstflag = 0;
year = clock[1];

for(  ; year > 0 ; year--) {
     leapyear = 0;
     startyear++ ;
     if(!(startyear % 4)) leapyear = 1 ;
     sday-- ;
     if(!sday) sday = 7;
     sday -= leapyear;
     if(!sday && leapyear) sday = 7; 
     }                                              /*  Ending for brace */

/* Calculate Daylight Savings Time end date using start date              */

if((sday + 20) < 25)  eday = sday + 27; else eday = sday + 20;

/* Determine if it's daylight savings time NOW                            */

if((clock[2] > 4 && clock[2] < 10) ||
    (clock[2] == 4 && clock[3] >= sday && clock[4] >= 2) ||
    (clock[2] == 10 && clock[3] <= eday && clock[4] < 2) ||
    (clock[2] == 4 && clock[3] > sday) ||
    (clock[2] == 10 && clock[3] < eday)) {
    dstflag = 1; 
    }
return(dstflag);
}

/*-------------------------------------------------------------------------*/

/*  MISTAKE FUNCTION  */

void mistake(char description[], char node[])
{
printf("\n\07[0m  ERROR: %s  --> [33m%s.[0m\n\n",node,description);
exit(5);
}

/*-------------------------------------------------------------------------*/

/*  QUITCHECK FUNCTION  */

void quitcheck(char xxx[])  
{
if(xxx[0] == 'q' || xxx[0] == 'Q') {
   puts("[0m\n");
   exit(0) ;
   }
}

/*-------------------------------------------------------------------------*/

/*  SETDATE FUNCTION  */

void setdate(unsigned char clock[8], int daymax[13], char month[13][4], 
             char monthext[13][7], char node[])
{
char input[30];
int temp;
    char current[13];
    char accept[30];
    char new[12];
    
    /* Initialize strings */
    strcpy(current, "The current ");
    strcpy(accept, " or <return> to accept [q]: [33m");
    strcpy(new, " enter new ");

while(TRUE) {
      printf("\x0c\n\n\n  Set [33mDATE/TIME[0m Utility.                 by George Kerber\n\n");
      while(TRUE) {
         printf("\n  [0m%syear is [33m[1m%2d[0m,  %syear%s",
            current,
            clock[1] + 1980,
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if(temp > 2040 || temp < 1980) {
               printf("\n\07            [0mERROR:  [33mInvalid Year (1980 - 2040).[0m\n\n");
               continue ;
               }
               else {
               clock[1] = temp - 1980;
               }
            }
         break ;
         }

      while(TRUE) {
         printf("\n   [0m%smonth is [33m[1m%02d[0m, %smonth%s",
            current,
            clock[2],
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if(temp < 1 || temp > 12 ) {
               printf("\n\07            [0mERROR:  [33mInvalid Month (1 - 12).[0m\07\n\n");
               continue ;
               }
               else {
               clock[2] = temp;
               }
            }
         break ;
         }

      if((clock[1] + 1980) % 4 == 0) daymax[2] = 29 ;
      if(clock[3] > daymax[clock[2]] ) { 
         printf("\n            [0mNOTE:  [33m%sday is invalid for %d.\n\n",
                current,
                clock[1] + 1980);
         }
   
      while(TRUE) {
         printf("\n     [0m%sday is [33m[1m%02d[0m,   %sday%s",
            current,
            clock[3],
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if(temp < 1 || temp > daymax[clock[2]]) {
               printf("\n\07            [0mERROR:  [33mInvalid Day (1 - %d).[0m\n\n",daymax[clock[2]]);
               continue ;
               }
               else {          
               clock[3] = temp;
               }
            }
         if(clock[3] > daymax[clock[2]] ) {   
         printf("\n            [0mERROR:  [33m%sday is invalid for %d.\n\n",
                current,
                clock[1] + 1980);
            continue ;
            }
         break ;
         }

      while(TRUE) {
         printf("\n    [0m%shour is [33m[1m%02d[0m,  %shour%s",
            current,
            clock[4],
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if((temp > 0 && temp < 24) ||
               !strcmp(input,"0") || !strcmp(input,"00")) {
               clock[4] = temp;
               }
               else {
               printf("\n\07            [0mERROR:  [33mInvalid Hour (00 - 23).[0m\n\n");
               continue ;
               }
            }
         break ;
         }

      while(TRUE) {
         printf("\n  [0m%sminute is [33m[1m%02d[0m,%sminute%s",
            current,
            clock[5],
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if((temp > 0 && temp < 60) ||
               !strcmp(input,"0") || !strcmp(input,"00")) {
               clock[5] = temp;
               }
               else {
               printf("\n\07            [0mERROR:  [33mInvalid Minute (00 - 59).[0m\n\n");
               continue;
               }
            } 
         break ;
         }

      printf("\n\n      [0mThe system clock will be set to: [33m%02d-%s%s-%d %02d:%02d:00[0m\n",
              clock[3],
              month[clock[2]],
              monthext[clock[2]],
              clock[1] + 1980,
              clock[4],
              clock[5]);
      printf("\n\n      Do you want to q[33m\(uit\)[0m, s[33m\(et\)[0m or e[33m\(dit\)[0m the date/time. [q,S,e]: ");
      gets(input);
      quitcheck(input);
   if(input[0] == 'e' || input[0] == 'E') {
        putchar('\n');
        continue ;
        }
      clock[6] = 0;
     if(chgclk(clock)) mistake("Can't set SYSTEM CLOCK",node);
      putchar('\n');
      exit(0);
   }
putchar('\n');
exit(0);
}

/*-------------------------------------------------------------------------*/

/*  TWELVE FUNCTION  */

int twelve(int hour)
{
if(hour > 13) {
   return(hour - 12);
    } else {
   if(!hour) {
      return(12);
        } else {
      return(hour);
      }
   }
} 

/**
 * @brief Parse a date string and set clock values
 * @param date_str Date string to parse
 * @param clock Clock array to populate
 * @param program Program name for error messages
 * @return 1 on success, 0 on failure
 */
int parse_date_string(const char *date_str, unsigned char *clock, const char *program) {
    int year, month, day, hour, minute, second;
    int parsed = 0;
    
    /* Try to parse various date formats */
    /* Format: YYYY-MM-DD HH:MM:SS */
    if (sscanf(date_str, "%d-%d-%d %d:%d:%d", &year, &month, &day, &hour, &minute, &second) == 6) {
        parsed = 1;
    }
    /* Format: MM/DD/YYYY HH:MM:SS */
    else if (sscanf(date_str, "%d/%d/%d %d:%d:%d", &month, &day, &year, &hour, &minute, &second) == 6) {
        parsed = 1;
    }
    /* Format: YYYY-MM-DD */
    else if (sscanf(date_str, "%d-%d-%d", &year, &month, &day) == 3) {
        hour = minute = second = 0;
        parsed = 1;
    }
    /* Format: MM/DD/YYYY */
    else if (sscanf(date_str, "%d/%d/%d", &month, &day, &year) == 3) {
        hour = minute = second = 0;
        parsed = 1;
    }
    /* Format: HH:MM:SS */
    else if (sscanf(date_str, "%d:%d:%d", &hour, &minute, &second) == 3) {
        /* Use current date */
        getclk(clock);
        year = clock[1] + 1980;
        month = clock[2];
        day = clock[3];
        parsed = 1;
    }
    
    if (!parsed) {
        fprintf(stderr, "%s: invalid date format '%s'\n", program, date_str);
        return 0;
    }
    
    /* Validate ranges */
    if (year < 1980 || year > 2099) {
        fprintf(stderr, "%s: year %d out of range (1980-2099)\n", program, year);
        return 0;
    }
    if (month < 1 || month > 12) {
        fprintf(stderr, "%s: month %d out of range (1-12)\n", program, month);
        return 0;
    }
    if (day < 1 || day > 31) {
        fprintf(stderr, "%s: day %d out of range (1-31)\n", program, day);
        return 0;
    }
    if (hour < 0 || hour > 23) {
        fprintf(stderr, "%s: hour %d out of range (0-23)\n", program, hour);
        return 0;
    }
    if (minute < 0 || minute > 59) {
        fprintf(stderr, "%s: minute %d out of range (0-59)\n", program, minute);
        return 0;
    }
    if (second < 0 || second > 59) {
        fprintf(stderr, "%s: second %d out of range (0-59)\n", program, second);
        return 0;
    }
    
    /* Convert to Amiga clock format */
    clock[0] = 0;  /* day of week (will be calculated later) */
    clock[1] = year - 1980;  /* year offset from 1980 */
    clock[2] = month;  /* month */
    clock[3] = day;  /* day */
    clock[4] = hour;  /* hour */
    clock[5] = minute;  /* minute */
    clock[6] = second;  /* second */
    
    return 1;
}

/**
 * @brief Show format help (integrated from datehelp.c)
 */
void show_format_help(void) {
    printf(" [33ma[0m abbreviated weekday name        [33mA[0m full weekday name    [33mR[0m time - hh:mm\n");
    printf(" [33mb[0m abbreviated month name          [33mB[0m full month name      [33mH[0m hour - 00 to 23\n");
    printf(" [33md[0m day of month - 01 to 31         [33mD[0m date - mm/dd/yy      [33mZ[0m timezone name*\n");
    printf(" [33mS[0m second - 00 to 59               [33mt[0m tab character        [33mi[0m hour - 1 to 12\n");
    printf(" [33mx[0m day of month ext \\(st,nd,rd,th\\)  [33mn[0m newline character    [33mI[0m hour - 01 to 12\n");
    printf(" [33me[0m day of month -  1 to 31         [33mY[0m four digit year      [33my[0m two digit year\n");
    printf(" [33mm[0m month of year - 01 to 12        [33mM[0m minute - 00 to 59    [33mT[0m time - hh:mm:ss\n");
    printf(" [33mq[0m print a literal quote           [33mj[0m julian day of year   [33mJ[0m days remaining\n");
    printf(" [33mg[0m greeting + date/time (o + k)    [33mo[0m greeting (Good...)   [33mk[0m date/time string\n");
    printf(" [33mu[0m ddd mmm dd hh:mm:ss tzn* yyyy   [33mr[0m time - hh:mm:ss pp   [33mp[0m string, AM or PM\n");
    printf(" [33mw[0m day of week - Sunday = 0        [33mW[0m same as w, Sun = 1   [33m$[0m default date\n\n");
    printf("\n  NOTE: Use --help-format to show this help from the date program.\n\n");
}

/**
 * @brief Adjust for Daylight Saving Time (integrated from dst2.c)
 */
void adjust_dst(void) {
    int i, startyear, sday, eday, leapyear;
    unsigned char clock[8];
    
    /* Initialize variables */
    startyear = 1980;
    sday = 6;
    leapyear = 0;

    /* Get battery clock and calculate Daylight Savings Time start date */
    getclk(clock); 

    for( i = clock[1] ; i > 0 ; i--) {
        leapyear = 0;
        startyear++ ;
        if((startyear % 4) == 0) leapyear = 1 ;
        sday-- ;
        if(!sday) sday = 7;
        sday -= leapyear;
        if(!sday && leapyear) sday = 7; 
    }

    /* Calculate Daylight Savings Time end date using start date */
    if((sday + 20) < 25)  eday = sday + 27; else eday = sday + 20;

    /* Determine if it's daylight savings time NOW */
    if((clock[2] > 4 && clock[2] < 10) ||
       (clock[2] == 4 && clock[3] >= sday && clock[4] >= 2) ||
       (clock[2] == 10 && clock[3] <= eday && clock[4] < 2) ||
       (clock[2] == 4 && clock[3] > sday) ||
       (clock[2] == 10 && clock[3] < eday)) {
        clock[4]++;
        chgclk(clock);
        printf("Daylight Saving Time adjustment applied.\n");
    } else {
        printf("No Daylight Saving Time adjustment needed.\n");
    }
}

/*-------------------------------------------------------------------------*/


