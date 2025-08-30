/*
 * ps.c - POSIX ps command for Amiga
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 * Based on original stat.c by James M Synge and Henning Schmiedehausen
 * Rewritten as POSIX-compliant ps command
 */


#include "ps.h"

#define BUF_SIZE 256
#define MAX_PROCESSES 100

/* Global variables for getopt */
extern struct DosLibrary *DOSBase;

/* ReadArgs template and argument array */
#define ARG_COUNT 7
enum {
    ARG_ALL = 0,
    ARG_LONG,
    ARG_FULL,
    ARG_VERBOSE,
    ARG_FORMAT,
    ARG_POSIX,
    ARG_HELP
};

/* Version string */
static const char vertag[] = "$VER: ps 1.0 (28.08.2025) amigazen project$\n";

int main(int argc, char **argv) {
    struct ps_options options;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "ALL/S,LONG/S,FULL/S,VERBOSE/S,FORMAT/K,POSIX/K/F,HELP/S";
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
    char initial_args_str[256];
    char user_input_buf[256];
    char *temp_str;
    size_t combined_len;

    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);

    if (argc == 1) {
        /* No arguments, show basic process list */
        init_options(&options);
        return run_ps_logic(&options, program);
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        init_options(&options);
        parse_getopt_args(argc, argv, &options, program);
        return run_ps_logic(&options, program);
        
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
            return FAILURE;
        }

        if (interactive_help) {
            /* Initialize buffers */
            initial_args_str[0] = '\0';
            user_input_buf[0] = '\0';

            /* Build a string from any args that are NOT '?' */
            temp_str = build_command_string(argc, argv, "?");
            if (temp_str) {
                strncpy(initial_args_str, temp_str, 255);
                free(temp_str);
            }

            /* Print template and prompt for more input */
            printf("%s: ", template);
            fflush(stdout);
            if (fgets(user_input_buf, sizeof(user_input_buf), stdin)) {
                /* Combine initial args with the new user input */
                combined_len = strlen(initial_args_str) + strlen(user_input_buf) + 2;
                cmd_string = malloc(combined_len);
                if (cmd_string) {
                    strcpy(cmd_string, initial_args_str);
                    if (initial_args_str[0] != '\0' && user_input_buf[0] != '\n') {
                        strcat(cmd_string, " ");
                    }
                    strcat(cmd_string, user_input_buf);
                }
            } else {
                cmd_string = strdup(initial_args_str);
                if (cmd_string) strcat(cmd_string, "\n");
            }
        } else {
            /* Standard case: build command string from all args */
            cmd_string = build_command_string(argc, argv, NULL);
        }

        if (!cmd_string) {
            fprintf(stderr, "%s: Failed to build command string\n", program);
            FreeDosObject(DOS_RDARGS, rdargs);
            return FAILURE;
        }

        /* Set up ReadArgs */
        rdargs->RDA_Source.CS_Buffer = cmd_string;
        rdargs->RDA_Source.CS_Length = strlen(cmd_string);
        rdargs->RDA_Flags |= RDAF_NOPROMPT;

        /* Parse arguments */
        if (ReadArgs(template, arg_array, rdargs)) {
            /* Process ReadArgs results */
            init_options(&options);
            
            if (arg_array[ARG_ALL]) {
                options.all_flag = TRUE;
            }
            if (arg_array[ARG_LONG]) {
                options.long_flag = TRUE;
            }
            if (arg_array[ARG_FULL]) {
                options.full_flag = TRUE;
            }

            if (arg_array[ARG_VERBOSE]) {
                options.verbose_flag = TRUE;
            }
            if (arg_array[ARG_FORMAT]) {
                options.format_string = (char *)arg_array[ARG_FORMAT];
            }
            if (arg_array[ARG_HELP]) {
                usage(program);
                ret_code = SUCCESS;
            } else {
                /* Check for POSIX escape hatch */
                if (arg_array[ARG_POSIX]) {
                    posix_str = (char *)arg_array[ARG_POSIX];
                    
                    /* Build new argv for getopt parsing */
                    new_argv[0] = program;
                    new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;
                    
                    /* Parse with getopt */
                    parse_getopt_args(new_argc, new_argv, &options, program);
                }
                
                ret_code = run_ps_logic(&options, program);
            }
        } else {
            /* ReadArgs failed, check for interactive help */
            if (strstr(cmd_string, "?")) {
                interactive_help = TRUE;
            } else {
                fprintf(stderr, "%s: Invalid arguments\n", program);
                usage(program);
                ret_code = FAILURE;
            }
        }

        /* Clean up ReadArgs */
        FreeArgs(rdargs);
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
        
        if (interactive_help) {
            printf("\nps - Report process status\n\n");
            printf("ALL/S - Show all processes\n");
            printf("LONG/S - Long format output\n");
            printf("FULL/S - Full format output\n");
            printf("VERBOSE/S - Verbose output (enhanced AmigaOS format)\n");
            printf("FORMAT/K - Custom format string\n");
            printf("POSIX/K/F - Use POSIX-style arguments\n");
            printf("HELP/S - Show this help\n\n");
            printf("Example: ps ALL/S LONG/S\n");
            ret_code = SUCCESS;
        }
        
        return ret_code;
    }
}

/**
 * @brief Initialize options structure with defaults
 * @param opts Pointer to options structure
 */
void init_options(struct ps_options *opts) {
    opts->all_flag = FALSE;
    opts->long_flag = FALSE;
    opts->full_flag = FALSE;
    opts->verbose_flag = FALSE;
    opts->help_flag = FALSE;
    opts->version_flag = FALSE;
    opts->format_string = NULL;
}

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param opts Pointer to options structure
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, struct ps_options *opts, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "alfvhV")) != -1) {
        switch (c) {
            case 'a':
                opts->all_flag = TRUE;
                break;
            case 'l':
                opts->long_flag = TRUE;
                break;
            case 'f':
                opts->full_flag = TRUE;
                break;
            case 'v':
                opts->verbose_flag = TRUE;
                break;
            case 'h':
                opts->help_flag = TRUE;
                break;
            case 'V':
                opts->version_flag = TRUE;
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    if (opts->help_flag) {
        usage(program);
        exit(SUCCESS);
    }
    
    if (opts->version_flag) {
        version();
        exit(SUCCESS);
    }
}

/**
 * @brief Core ps logic separated from argument parsing
 * @param opts Pointer to options structure
 * @param program Program name for error messages
 * @return Exit code
 */
int run_ps_logic(struct ps_options *opts, const char *program) {
    struct process_info procs[MAX_PROCESSES];
    int process_count;
    int i;
    
    /* Check AmigaOS version requirement */
    struct Library *execbase = OpenLibrary("exec.library", 37L);
    if (!execbase) {
        fprintf(stderr, "%s: You need AmigaOS V37 or beyond\n", program);
        return FAILURE;
    }
    CloseLibrary(execbase);
    
    /* Get process list */
    process_count = get_process_list(procs, MAX_PROCESSES);
    if (process_count < 0) {
        fprintf(stderr, "%s: Failed to get process list\n", program);
        return FAILURE;
    }
    
    /* Print header */
    print_process_header(opts);
    
            /* Print process information */
        for (i = 0; i < process_count; i++) {
            print_process_line(&procs[i], opts);
        }
    
    return SUCCESS;
}

/**
 * @brief Get list of processes from AmigaOS
 * @param procs Array to store process information
 * @param max_procs Maximum number of processes to store
 * @return Number of processes found, or -1 on error
 */
int get_process_list(struct process_info *procs, int max_procs) {
    struct RootNode *rn;
    struct CliProcList *cl;
    long *ta;
    long process, processes;
    char *msg;
    struct Process *pr;
    struct CommandLineInterface *cli;
    int count = 0;
    
    rn = (struct RootNode *)(DOSBase->dl_Root);
    
    for (cl = (struct CliProcList *)rn->rn_CliList.mlh_Head; 
         cl->cpl_Node.mln_Succ && count < max_procs; 
         cl = (struct CliProcList *)cl->cpl_Node.mln_Succ) {
        
        ta = (long *)cl->cpl_Array;
        processes = *ta++;
        
        for (process = 0; process < processes && count < max_procs; process++) {
            msg = (char *)(*ta++);
            if (!msg) continue;
            
            pr = (struct Process *)(msg - sizeof(struct Task));
            
            procs[count].tasknum = (int)(pr->pr_TaskNum);
            procs[count].priority = (int)(pr->pr_Task.tc_Node.ln_Pri);
            procs[count].address = (void *)pr;
            procs[count].pid = (int)(pr->pr_TaskNum); /* Use task number as PID */
            
            /* Enhanced process information */
            procs[count].stack_size = pr->pr_StackSize;
            procs[count].stack_base = pr->pr_StackBase;
            procs[count].seg_list = pr->pr_SegList;
            procs[count].flags = pr->pr_Flags;
            
            /* Determine process state - enhanced for AmigaOS */
            if (pr->pr_Task.tc_State == TS_READY) {
                strcpy(procs[count].state, "R");
            } else if (pr->pr_Task.tc_State == TS_WAITING) {
                strcpy(procs[count].state, "S");
            } else if (pr->pr_Task.tc_State == TS_SLEEP) {
                strcpy(procs[count].state, "S");
            } else {
                strcpy(procs[count].state, "?");
            }
            
            /* Get CLI information */
            cli = (struct CommandLineInterface *)BADDR(pr->pr_CLI);
            if (cli) {
                moveBSTR(cli->cli_CommandName, procs[count].command, BUF_SIZE);
                if (!procs[count].command[0]) {
                    strcpy(procs[count].command, "Waiting for a command");
                }
                moveBSTR(cli->cli_SetName, procs[count].directory, BUF_SIZE);
                
                /* Enhanced CLI information */
                procs[count].background = cli->cli_Background;
                procs[count].interactive = cli->cli_Interactive;
                procs[count].return_code = cli->cli_ReturnCode;
                procs[count].fail_level = cli->cli_FailLevel;
                
                /* Get command arguments and file */
                if (pr->pr_Arguments) {
                    strncpy(procs[count].arguments, (char *)BADDR(pr->pr_Arguments), 255);
                    procs[count].arguments[255] = '\0';
                } else {
                    procs[count].arguments[0] = '\0';
                }
                
                if (cli->cli_CommandFile) {
                    moveBSTR(cli->cli_CommandFile, procs[count].command_file, 255);
                } else {
                    procs[count].command_file[0] = '\0';
                }
                
                /* Get prompt */
                if (cli->cli_Prompt) {
                    moveBSTR(cli->cli_Prompt, procs[count].prompt, 63);
                } else {
                    procs[count].prompt[0] = '\0';
                }
                
                /* Calculate start time (approximate) - use system boot time as baseline */
                /* Note: AmigaOS doesn't provide individual process creation times */
                procs[count].start_time = get_system_boot_time();
            } else {
                strcpy(procs[count].command, "Not a CLI");
                procs[count].directory[0] = '\0';
                procs[count].background = 0;
                procs[count].interactive = 0;
                procs[count].return_code = 0;
                procs[count].fail_level = 0;
                procs[count].arguments[0] = '\0';
                procs[count].command_file[0] = '\0';
                procs[count].prompt[0] = '\0';
                procs[count].start_time = 0;
            }
            
            count++;
        }
    }
    
    return count;
}

/**
 * @brief Print process list header
 * @param opts Pointer to options structure
 */
void print_process_header(struct ps_options *opts) {
    if (opts->long_flag) {
        printf("F S   UID   PID  PPID  C PRI  ADDR SZ WCHAN  TTY          TIME CMD\n");
    } else if (opts->full_flag) {
        printf("UID        PID  PPID  C UPTIME TTY          TIME CMD\n");
    } else if (opts->verbose_flag) {
        printf("PID PRI STATE STACK  BG INT TTY      UPTIME CMD\n");
    } else {
        printf("  PID TTY          TIME CMD\n");
    }
}

/**
 * @brief Print a single process line
 * @param proc Pointer to process information
 * @param opts Pointer to options structure
 */
void print_process_line(struct process_info *proc, struct ps_options *opts) {
    if (opts->long_flag) {
        print_long_format(proc);
    } else if (opts->full_flag) {
        print_full_format(proc);
    } else if (opts->verbose_flag) {
        print_enhanced_format(proc);
    } else {
        /* Default format */
        printf("%5d %-12s %s %s\n", 
               proc->pid, 
               "CON:", 
               "00:00:00", 
               proc->command);
    }
}

/**
 * @brief Print long format output
 * @param proc Pointer to process information
 */
void print_long_format(struct process_info *proc) {
    printf("%c %s %5d %5d %5d %5d %5d %8lx %5lu %8lx %-12s %s %s\n",
           proc->state[0],           /* F - flags */
           proc->state,              /* S - state */
           0,                        /* UID - user ID (not available on Amiga) */
           proc->pid,                /* PID */
           0,                        /* PPID - parent PID (not available on Amiga) */
           0,                        /* C - CPU usage (not available on Amiga) */
           proc->priority,           /* PRI - priority */
           (unsigned long)proc->address, /* ADDR - address */
           proc->stack_size,         /* SZ - stack size in bytes */
           0,                        /* WCHAN - wait channel (not available on Amiga) */
           "CON:",                   /* TTY */
           "00:00:00",              /* TIME */
           proc->command);           /* CMD */
}

/**
 * @brief Print full format output
 * @param proc Pointer to process information
 */
void print_full_format(struct process_info *proc) {
    char uptime_str[16];
    
    /* Format uptime (system boot time) */
    if (proc->start_time > 0) {
        /* Show system uptime in hours:minutes format */
        ULONG hours = proc->start_time / 3600;
        ULONG minutes = (proc->start_time % 3600) / 60;
        sprintf(uptime_str, "%lu:%02lu", hours, minutes);
    } else {
        strcpy(uptime_str, "??:??");
    }
    
    printf("%-10s %5d %5d %5d %s %-12s %s %s\n",
           "amiga",                  /* UID - username (not available on Amiga) */
           proc->pid,                /* PID */
           0,                        /* PPID - parent PID (not available on Amiga) */
           0,                        /* C - CPU usage (not available on Amiga) */
           uptime_str,               /* STIME - system uptime */
           "CON:",                   /* TTY */
           "00:00:00",              /* TIME */
           proc->command);           /* CMD */
}

/**
 * @brief Print enhanced format output with AmigaOS-specific information
 * @param proc Pointer to process information
 */
void print_enhanced_format(struct process_info *proc) {
    char uptime_str[16];
    
    /* Format uptime (system boot time) */
    if (proc->start_time > 0) {
        /* Show system uptime in hours:minutes format */
        ULONG hours = proc->start_time / 3600;
        ULONG minutes = (proc->start_time % 3600) / 60;
        sprintf(uptime_str, "%lu:%02lu", hours, minutes);
    } else {
        strcpy(uptime_str, "??:??");
    }
    
    printf("%5d %3d %5s %6lu %2s %3s %-8s %-8s %s %s\n",
           proc->pid,                /* PID */
           proc->priority,           /* PRI - priority */
           proc->state,              /* STATE - process state */
           proc->stack_size,         /* STACK - stack size in bytes */
           proc->background ? "BG" : "FG", /* BG - background/foreground */
           proc->interactive ? "INT" : "NO", /* INT - interactive */
           "CON:",                   /* TTY */
           uptime_str,               /* UPTIME - system uptime */
           proc->command);           /* CMD */
}

/**
 * @brief Copy a BSTR to a C char string
 * @param bptr BSTR pointer
 * @param buffer Destination buffer
 * @param maxlen Maximum length to copy
 */
void moveBSTR(BSTR bptr, char *buffer, int maxlen) {
    register char *ptr;
    register unsigned int len, i;
    unsigned char l;

    ptr = (char *)BADDR(bptr);
    l = (unsigned int)(*ptr++);

    if (!(len = l)) {
        *buffer = '\0';
        return;
    }
    if (len > maxlen)
        len = maxlen;
    for (i = 0; i < len; i++)
        *buffer++ = *ptr++;

    if (i < maxlen)
        *buffer = '\0';
}

/**
 * @brief Display usage information
 * @param program Program name
 */
void usage(const char *program) {
    fprintf(stderr, "Version: %s\n", &vertag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS]\n", program);
    fprintf(stderr, "Usage (Amiga): %s [ALL/S] [LONG/S] [FULL/S] [VERBOSE/S] [FORMAT/K] [POSIX/K/F] [HELP/S]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -a          show all processes\n");
    fprintf(stderr, "  -l          long format\n");
    fprintf(stderr, "  -f          full format\n");
    fprintf(stderr, "  -v          verbose output (enhanced AmigaOS format)\n");
    fprintf(stderr, "  -h, -V      display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Report a snapshot of the current processes.\n");
    fprintf(stderr, "  Note: Some POSIX features are not available on AmigaOS.\n");
    fprintf(stderr, "  Enhanced format shows AmigaOS-specific information.\n");
    exit(FAILURE);
}

/**
 * @brief Get system boot time using multiple methods (from uptime.c)
 * @return System boot time in seconds since epoch, or 0 if unable to determine
 */
ULONG get_system_boot_time(void) {
    ULONG boot_time = 0;
    struct DateStamp current_time;
    char ram_path[256];
    BPTR ram_lock;
    struct FileInfoBlock fib;
    
    /* Method 1: Try RAM: volume creation date (most reliable) */
    strcpy(ram_path, "RAM:");
    ram_lock = Lock(ram_path, ACCESS_READ);
    if (ram_lock) {
        if (Examine(ram_lock, &fib)) {
            /* Get current time */
            DateStamp(&current_time);
            
            /* Calculate boot time from RAM: creation date */
            /* Note: This is approximate as RAM: is created at boot */
            boot_time = (current_time.ds_Days - fib.fib_Date.ds_Days) * 24 * 60 * 60 +
                       (current_time.ds_Minute - fib.fib_Date.ds_Minute) * 60 +
                       (current_time.ds_Tick - fib.fib_Date.ds_Tick) / 50;
            
            /* Ensure positive value */
            if (boot_time < 0) {
                boot_time = 0;
            }
        }
        UnLock(ram_lock);
    }
    
    /* Method 2: Try T:uupat file (legacy method) */
    if (boot_time == 0) {
        FILE *fp;
        time_t boot_timestamp;
        
        fp = fopen("T:uupat", "r");
        if (fp) {
            if (fscanf(fp, "%ld", &boot_timestamp) == 1) {
                boot_time = (ULONG)boot_timestamp;
            }
            fclose(fp);
        }
    }
    
    return boot_time;
}

/**
 * @brief Display version information
 */
void version(void) {
    printf("ps version %s\n", &vertag[6]);
    exit(SUCCESS);
}
