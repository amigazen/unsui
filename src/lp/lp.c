/*
 * lp - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 */

#include "lp.h"

/* Exit codes */
#define SUCCESS 0
#define FAILURE 1

static const char *verstag = "$VER: lp 2.0 (16/09/25)\n";

static const char *stack_cookie = "$STACK: 4096";

/* For ReadArgs template */
enum {
    ARG_FILES,
    ARG_ENCRYPT,
    ARG_USERNAME,
    ARG_COPIES,
    ARG_DESTINATION,
    ARG_HOSTNAME,
    ARG_EMAIL,
    ARG_OPTIONS,
    ARG_PRIORITY,
    ARG_SILENT,
    ARG_TITLE,
    ARG_HANDLING,
    ARG_PAGE_LIST,
    ARG_JOB_ID,
    ARG_VERBOSE,
    ARG_POSIX,
    ARG_COUNT
};

/* Global options structure */
typedef struct {
    BOOL encrypt_flag;          /* -E: force encryption */
    char *username;             /* -U: username */
    int copies;                 /* -n: number of copies */
    char *destination;          /* -d: destination printer */
    char *hostname;             /* -h: hostname:port */
    BOOL email_flag;            /* -m: email notification */
    char *options;              /* -o: options */
    int priority;               /* -q: priority (1-100) */
    BOOL silent_flag;           /* -s: silent mode */
    char *title;                /* -t: title */
    char *handling;             /* -H: job handling */
    char *page_list;            /* -P: page list */
    char *job_id;               /* -i: job ID to modify */
    BOOL verbose_flag;          /* -v: verbose output */
    int exit_code;              /* Exit code */
} LpOptions;

/* Function declarations */
void usage(const char *program);
void print_version(const char *program);
int run_lp_logic(LpOptions *options, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, LpOptions *options, int *file_start, const char *program);
void init_options(LpOptions *options);
void cleanup_options(LpOptions *options);
int submit_to_spool(LpOptions *options, char **files, int file_count, const char *program);

/* Static function declarations */
static int check_spool_system(const char *program);
static int send_spool_message(const char *filename, LpOptions *options, const char *program);
static void print_error(const char *program, const char *filename, LONG error_code);

/* Global variables */
char *whoami;  /* Program name for error messages */

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    LpOptions options;
    int file_start = 1;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FILES/M,ENCRYPT/S,USERNAME/K,COPIES/K/N,DESTINATION/K,HOSTNAME/K,EMAIL/S,OPTIONS/K,PRIORITY/K/N,SILENT/S,TITLE/K,HANDLING/K,PAGE_LIST/K,JOB_ID/K,VERBOSE/S,POSIX/K/F";
    LONG arg_array[ARG_COUNT] = {0};
    struct RDArgs *rdargs = NULL;
    char *cmd_string = NULL;
    int ret_code = SUCCESS;
    BOOL interactive_help = FALSE;
    
    /* POSIX/F Path Variables */
    int i;
    int file_count;
    char user_input_buf[256];

    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);
    whoami = program;

    if (argc == 1) {
        /* No arguments, show usage */
        usage(program);
        return FAILURE;
    }

    /* Initialize options */
    init_options(&options);

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &options, &file_start, program);
        files = &argv[file_start];
        file_count = argc - file_start;
        
        if (file_count == 0) {
            fprintf(stderr, "%s: missing operand\n", program);
            usage(program);
            cleanup_options(&options);
            return FAILURE;
        }
        
        return run_lp_logic(&options, file_count, files, program);
        
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
            /* Interactive help using ReadArgs */
            printf("LP - Line Printer utility\n\n");
            printf("Usage: %s [options] file...\n\n", program);
            printf("Options:\n");
            printf("  -E               Force encryption when connecting to server\n");
            printf("  -U username      Username for server authentication\n");
            printf("  -c copies        Number of copies to print (legacy, use -n)\n");
            printf("  -d destination   Destination printer\n");
            printf("  -h hostname:port Alternate server\n");
            printf("  -m               Send email when job is completed\n");
            printf("  -n copies        Number of copies to print (default: 1)\n");
            printf("  -o options       Print options\n");
            printf("  -q priority      Job priority (1-100, default: 50)\n");
            printf("  -s               Silent mode (don't report job IDs)\n");
            printf("  -t title         Job title\n");
            printf("  -H handling      Job handling (hold, immediate, restart, resume)\n");
            printf("  -P page-list     Pages to print (e.g., 1,3-5,16)\n");
            printf("  -i job-id        Modify existing job\n");
            printf("  -v               Verbose output\n");
            printf("  -V               Version information\n");
            printf("  --               End of options\n");
            printf("\nEnter command line arguments: ");
            
            if (fgets(user_input_buf, sizeof(user_input_buf), stdin)) {
                /* Remove newline */
                char *newline = strchr(user_input_buf, '\n');
                if (newline) *newline = '\0';
                
                /* Parse the user input */
                if (strlen(user_input_buf) > 0) {
                    cmd_string = build_command_string(0, NULL, user_input_buf);
                    if (cmd_string) {
                        rdargs->RDA_Source.CS_Buffer = cmd_string;
                        rdargs->RDA_Source.CS_Length = strlen(cmd_string);
                        
                        if (ReadArgs(template, arg_array, rdargs)) {
                            /* Process ReadArgs results */
                            if (arg_array[ARG_ENCRYPT]) options.encrypt_flag = TRUE;
                            if (arg_array[ARG_USERNAME]) options.username = (char *)arg_array[ARG_USERNAME];
                            if (arg_array[ARG_COPIES]) options.copies = *(LONG *)arg_array[ARG_COPIES];
                            if (arg_array[ARG_DESTINATION]) options.destination = (char *)arg_array[ARG_DESTINATION];
                            if (arg_array[ARG_HOSTNAME]) options.hostname = (char *)arg_array[ARG_HOSTNAME];
                            if (arg_array[ARG_EMAIL]) options.email_flag = TRUE;
                            if (arg_array[ARG_OPTIONS]) options.options = (char *)arg_array[ARG_OPTIONS];
                            if (arg_array[ARG_PRIORITY]) options.priority = *(LONG *)arg_array[ARG_PRIORITY];
                            if (arg_array[ARG_SILENT]) options.silent_flag = TRUE;
                            if (arg_array[ARG_TITLE]) options.title = (char *)arg_array[ARG_TITLE];
                            if (arg_array[ARG_HANDLING]) options.handling = (char *)arg_array[ARG_HANDLING];
                            if (arg_array[ARG_PAGE_LIST]) options.page_list = (char *)arg_array[ARG_PAGE_LIST];
                            if (arg_array[ARG_JOB_ID]) options.job_id = (char *)arg_array[ARG_JOB_ID];
                            if (arg_array[ARG_VERBOSE]) options.verbose_flag = TRUE;
                            
                            if (arg_array[ARG_FILES]) {
                                files = (char **)arg_array[ARG_FILES];
                                file_count = 1; /* ReadArgs gives us one file at a time */
                                
                                ret_code = run_lp_logic(&options, file_count, files, program);
                            } else {
                                fprintf(stderr, "%s: missing operand\n", program);
                                usage(program);
                                ret_code = FAILURE;
                            }
                        } else {
                            fprintf(stderr, "%s: invalid arguments\n", program);
                            usage(program);
                            ret_code = FAILURE;
                        }
                        free(cmd_string);
                    }
                }
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
                if (arg_array[ARG_ENCRYPT]) options.encrypt_flag = TRUE;
                if (arg_array[ARG_USERNAME]) options.username = (char *)arg_array[ARG_USERNAME];
                if (arg_array[ARG_COPIES]) options.copies = *(LONG *)arg_array[ARG_COPIES];
                if (arg_array[ARG_DESTINATION]) options.destination = (char *)arg_array[ARG_DESTINATION];
                if (arg_array[ARG_HOSTNAME]) options.hostname = (char *)arg_array[ARG_HOSTNAME];
                if (arg_array[ARG_EMAIL]) options.email_flag = TRUE;
                if (arg_array[ARG_OPTIONS]) options.options = (char *)arg_array[ARG_OPTIONS];
                if (arg_array[ARG_PRIORITY]) options.priority = *(LONG *)arg_array[ARG_PRIORITY];
                if (arg_array[ARG_SILENT]) options.silent_flag = TRUE;
                if (arg_array[ARG_TITLE]) options.title = (char *)arg_array[ARG_TITLE];
                if (arg_array[ARG_HANDLING]) options.handling = (char *)arg_array[ARG_HANDLING];
                if (arg_array[ARG_PAGE_LIST]) options.page_list = (char *)arg_array[ARG_PAGE_LIST];
                if (arg_array[ARG_JOB_ID]) options.job_id = (char *)arg_array[ARG_JOB_ID];
                if (arg_array[ARG_VERBOSE]) options.verbose_flag = TRUE;
                
                if (arg_array[ARG_FILES]) {
                    files = (char **)arg_array[ARG_FILES];
                    file_count = 1; /* ReadArgs gives us one file at a time */
                    
                    ret_code = run_lp_logic(&options, file_count, files, program);
                } else {
                    fprintf(stderr, "%s: missing operand\n", program);
                    usage(program);
                    ret_code = FAILURE;
                }
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

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param options Options structure to populate
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, LpOptions *options, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "EU:c:d:h:m:n:o:q:st:H:P:ivV--")) != -1) {
        switch (c) {
            case 'E':
                options->encrypt_flag = TRUE;
                break;
            case 'U':
                options->username = strdup(optarg);
                break;
            case 'c':
                /* Legacy -c option for copies (for compatibility) */
                options->copies = atoi(optarg);
                if (options->copies <= 0) {
                    fprintf(stderr, "%s: invalid number of copies: %s\n", program, optarg);
                    exit(FAILURE);
                }
                break;
            case 'd':
                options->destination = strdup(optarg);
                break;
            case 'h':
                options->hostname = strdup(optarg);
                break;
            case 'm':
                options->email_flag = TRUE;
                break;
            case 'n':
                options->copies = atoi(optarg);
                if (options->copies <= 0) {
                    fprintf(stderr, "%s: invalid number of copies: %s\n", program, optarg);
                    exit(FAILURE);
                }
                break;
            case 'o':
                options->options = strdup(optarg);
                break;
            case 'q':
                options->priority = atoi(optarg);
                if (options->priority < 1 || options->priority > 100) {
                    fprintf(stderr, "%s: priority must be between 1 and 100: %s\n", program, optarg);
                    exit(FAILURE);
                }
                break;
            case 's':
                options->silent_flag = TRUE;
                break;
            case 't':
                options->title = strdup(optarg);
                break;
            case 'H':
                options->handling = strdup(optarg);
                break;
            case 'P':
                options->page_list = strdup(optarg);
                break;
            case 'i':
                options->job_id = strdup(optarg);
                break;
            case 'v':
                options->verbose_flag = TRUE;
                break;
            case 'V':
                print_version(program);
                exit(SUCCESS);
                break;
            case '?':
                usage(program);
                exit(FAILURE);
                break;
        }
    }
    
    *file_start = optind;
}

/**
 * @brief Core lp logic separated from argument parsing
 * @param options Options structure
 * @param file_count Number of files to process
 * @param files Array of file names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_lp_logic(LpOptions *options, int file_count, char **files, const char *program) {
    int i;
    int ret_code = SUCCESS;
    
    /* Check if spool system is available */
    if (check_spool_system(program) != SUCCESS) {
        return FAILURE;
    }
    
    /* Submit files to spool */
    for (i = 0; i < file_count; i++) {
        if (options->verbose_flag) {
            printf("Submitting %s to print queue\n", files[i]);
        }
        
        if (send_spool_message(files[i], options, program) != SUCCESS) {
            ret_code = FAILURE;
        }
    }
    
    return ret_code;
}

/**
 * @brief Initialize options structure
 * @param options Options structure to initialize
 */
void init_options(LpOptions *options) {
    options->encrypt_flag = FALSE;
    options->username = NULL;
    options->copies = 1;
    options->destination = NULL;
    options->hostname = NULL;
    options->email_flag = FALSE;
    options->options = NULL;
    options->priority = 50;  /* Default priority 50 (middle of 1-100 range) */
    options->silent_flag = FALSE;
    options->title = NULL;
    options->handling = NULL;
    options->page_list = NULL;
    options->job_id = NULL;
    options->verbose_flag = FALSE;
    options->exit_code = SUCCESS;
}

/**
 * @brief Clean up options structure
 * @param options Options structure to clean up
 */
void cleanup_options(LpOptions *options) {
    if (options->username) free(options->username);
    if (options->destination) free(options->destination);
    if (options->hostname) free(options->hostname);
    if (options->options) free(options->options);
    if (options->title) free(options->title);
    if (options->handling) free(options->handling);
    if (options->page_list) free(options->page_list);
    if (options->job_id) free(options->job_id);
}

/**
 * @brief Check if spool system is available
 * @param program Program name for error messages
 * @return SUCCESS if available, FAILURE otherwise
 */
static int check_spool_system(const char *program) {
    struct MsgPort *spool_port;
    
    spool_port = FindPort(SPOOL_ME);
    if (!spool_port) {
        fprintf(stderr, "%s: spool system not available\n", program);
        return FAILURE;
    }
    
    return SUCCESS;
}

/**
 * @brief Send file to spool system
 * @param filename File to print
 * @param options Print options
 * @param program Program name for error messages
 * @return SUCCESS if sent, FAILURE otherwise
 */
static int send_spool_message(const char *filename, LpOptions *options, const char *program) {
    struct MsgPort *spool_port;
    SPOOLmsg *packet;
    int i;
    
    spool_port = FindPort(SPOOL_ME);
    if (!spool_port) {
        fprintf(stderr, "%s: spool system not available\n", program);
        return FAILURE;
    }
    
    /* Send multiple copies if requested */
    for (i = 0; i < options->copies; i++) {
        packet = (SPOOLmsg *)AllocMem(sizeof(SPOOLmsg), MEMF_PUBLIC | MEMF_CLEAR);
        if (!packet) {
            fprintf(stderr, "%s: out of memory\n", program);
            return FAILURE;
        }
        
        /* Initialize message */
        packet->minfo.mn_Node.ln_Type = NT_MESSAGE;
        packet->minfo.mn_Length = sizeof(SPOOLmsg);
        packet->log_status = LOG_IN;
        strncpy(packet->filename, filename, sizeof(packet->filename) - 1);
        packet->filename[sizeof(packet->filename) - 1] = '\0';
        
        /* Send message to spool system */
        PutMsg(spool_port, (struct Message *)packet);
        
        if (options->verbose_flag) {
            printf("Submitted %s (copy %d of %d)\n", filename, i + 1, options->copies);
        }
    }
    
    return SUCCESS;
}

/**
 * @brief Print error message
 * @param program Program name
 * @param filename File name
 * @param error_code Error code
 */
static void print_error(const char *program, const char *filename, LONG error_code) {
    fprintf(stderr, "%s: %s: ", program, filename);
    PrintFault(error_code, NULL);
    fprintf(stderr, "\n");
}

/**
 * @brief Print usage information
 * @param program Program name
 */
void usage(const char *program) {
    printf("Usage: %s [options] file...\n", program);
    printf("Submit files for printing\n\n");
    printf("Options:\n");
    printf("  -E               Force encryption when connecting to server\n");
    printf("  -U username      Username for server authentication\n");
    printf("  -c copies        Number of copies to print (legacy, use -n)\n");
    printf("  -d destination   Destination printer\n");
    printf("  -h hostname:port Alternate server\n");
    printf("  -m               Send email when job is completed\n");
    printf("  -n copies        Number of copies to print (default: 1)\n");
    printf("  -o options       Print options\n");
    printf("  -q priority      Job priority (1-100, default: 50)\n");
    printf("  -s               Silent mode (don't report job IDs)\n");
    printf("  -t title         Job title\n");
    printf("  -H handling      Job handling (hold, immediate, restart, resume)\n");
    printf("  -P page-list     Pages to print (e.g., 1,3-5,16)\n");
    printf("  -i job-id        Modify existing job\n");
    printf("  -v               Verbose output\n");
    printf("  -V               Version information\n");
    printf("  --               End of options\n");
    printf("\n");
}

/**
 * @brief Print version information
 * @param program Program name
 */
void print_version(const char *program) {
    printf("%s - POSIX-compliant line printer utility\n", program);
}