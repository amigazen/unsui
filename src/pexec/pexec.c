/*
 * pexec - Parallel execution utility for AmigaOS
 * 
 * Based on the existing Amiga infrastructure from AmigaPerl
 * Uses the same patterns as spawn.c and mypopen.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>

#include <common.h>

/* Amiga-specific includes */
#include <exec/tasks.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <libraries/dosextens.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/utility.h>
#include <dos/dos.h>
#include <dos/dostags.h>
#include <dos/dosextens.h>

/* Configuration constants */
#define MAX_LINE_LENGTH 1024
#define MAX_COMMAND_LENGTH 512
#define MAX_PROCESSES 10
#define PIPEPATH "PIPE:pexecXXXXXXXX"

/* Return codes */
#define SUCCESS 0
#define FAILURE 1

/* Version tag for Amiga */
static const char *verstag = "$VER: pexec 1.0 (11/09/25)\n";

/* Stack cookie for Amiga */
static const char *stack_cookie = "$STACK: 8192";

/* Global variables */
static int max_procs = 1;
static int verbose = 0;
static int dry_run = 0;
static char *command_template = NULL;
static char *input_file = NULL;

/* Process tracking */
typedef struct {
    int pid;
    char *command;
    char *pipe_name;
    int job_num;
} process_info_t;

static process_info_t *processes = NULL;
static int num_processes = 0;

/* Function prototypes */
static void usage(const char *program);
static int execute_commands(char **input_lines, int num_lines);
static char *expand_template(const char *template, const char *input);
static int spawn_command(const char *command, int job_num);
static int wait_for_processes(void);
static void cleanup_processes(void);

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

/* Define getpid if not available */
#ifndef getpid
int getpid(void)
{
    struct Task *current_task;
    current_task = FindTask(NULL);
    return (int)current_task; /* Use task address as PID */
}
#endif

char *mktemp(char *origs)
{
    char pidbuf[12], *pb = pidbuf, *s = origs;
    static int lastletter = 'z';
    static int generation = 0;
    int i;
    BPTR lk;

    if (!s) return (NULL);
    sprintf (pidbuf, "%02d%02x", getpid(), (generation & 0xFF));
    
    while (*s) {
	if (*s == 'X') {
	    if (!*pb) break;	/* We've run out of "pid" and found an 'X' */
	    *s = *pb++;		/* Drop in the "pid" over 'X's */
	}
        s++;
    }

    if (!*s) return(NULL);	/* Not enough 'X's */
    *(s + 1) = '\0';		/* Chop off remaining 'X's and anything else. */
    i = lastletter;
    for (;;) {
	if (++lastletter > 'z') {
	    lastletter = 'a';
	    generation++;
	}
	*s = lastletter;
	if (!(lk = Lock(origs, ACCESS_READ)))
	    return(origs);
	else
	    UnLock(lk);

	if (i == lastletter) return (NULL);
    }
}

int main(int argc, char *argv[])
{
    char line[MAX_LINE_LENGTH];
    char **input_lines = NULL;
    int num_lines = 0;
    int max_lines = 0;
    int result = SUCCESS;
    FILE *input_fp = stdin;
    char *program;
    int c;
    
    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);
    
    /* Parse command line arguments manually */
    {
        int i;
        char *arg;
        
        for (i = 1; i < argc; i++) {
            arg = argv[i];
            
            if (arg[0] == '-') {
                if (strcmp(arg, "-n") == 0) {
                    if (i + 1 < argc) {
                        max_procs = atoi(argv[++i]);
                        if (max_procs <= 0 || max_procs > MAX_PROCESSES) {
                            fprintf(stderr, "pexec: invalid max processes: %d (max: %d)\n", 
                                    max_procs, MAX_PROCESSES);
                            exit(FAILURE);
                        }
                    } else {
                        fprintf(stderr, "pexec: -n requires a value\n");
                        exit(FAILURE);
                    }
                } else if (strcmp(arg, "-v") == 0) {
                    verbose = 1;
                } else if (strcmp(arg, "-i") == 0) {
                    if (i + 1 < argc) {
                        input_file = argv[++i];
                    } else {
                        fprintf(stderr, "pexec: -i requires a value\n");
                        exit(FAILURE);
                    }
                } else if (strcmp(arg, "-d") == 0) {
                    dry_run = 1;
                } else if (strcmp(arg, "-h") == 0) {
                    usage(program);
                    exit(SUCCESS);
                } else {
                    fprintf(stderr, "pexec: unknown option: %s\n", arg);
                    usage(program);
                    exit(FAILURE);
                }
            } else {
                /* Command template */
                command_template = arg;
                break;
            }
        }
        
        if (!command_template) {
            fprintf(stderr, "pexec: command required\n");
            usage(program);
            exit(FAILURE);
        }
    }
    
    /* Open input file if specified */
    if (input_file) {
        input_fp = fopen(input_file, "r");
        if (!input_fp) {
            fprintf(stderr, "%s: cannot open input file '%s': %s\n", 
                    program, input_file, strerror(errno));
            exit(FAILURE);
        }
    }
    
    /* Read input lines */
    while (fgets(line, sizeof(line), input_fp)) {
        /* Remove trailing newline */
        line[strcspn(line, "\n")] = '\0';
        
        if (num_lines >= max_lines) {
            max_lines = max_lines ? max_lines * 2 : 100;
            input_lines = realloc(input_lines, max_lines * sizeof(char*));
            if (!input_lines) {
                fprintf(stderr, "%s: out of memory\n", program);
                exit(FAILURE);
            }
        }
        
        input_lines[num_lines] = strdup(line);
        if (!input_lines[num_lines]) {
            fprintf(stderr, "%s: out of memory\n", program);
            exit(FAILURE);
        }
        num_lines++;
    }
    
    if (input_fp != stdin) {
        fclose(input_fp);
    }
    
    if (num_lines == 0) {
        fprintf(stderr, "%s: no input provided\n", program);
        exit(FAILURE);
    }
    
    /* Allocate process tracking */
    processes = calloc(max_procs, sizeof(process_info_t));
    if (!processes) {
        fprintf(stderr, "%s: out of memory\n", program);
        exit(FAILURE);
    }
    
    /* Execute commands */
    result = execute_commands(input_lines, num_lines);
    
    /* Cleanup */
    {
        int i;
        for (i = 0; i < num_lines; i++) {
            free(input_lines[i]);
        }
        free(input_lines);
    }
    cleanup_processes();
    
    return result;
}

static void usage(const char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage: %s [OPTIONS] COMMAND\n", program);
    fprintf(stderr, "Execute commands in parallel\n\n");
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -n N        maximum number of processes (default: 1)\n");
    fprintf(stderr, "  -v          verbose output\n");
    fprintf(stderr, "  -i FILE     input file (default: stdin)\n");
    fprintf(stderr, "  -d          dry run (show what would be executed)\n");
    fprintf(stderr, "  -h          show this help\n\n");
    fprintf(stderr, "COMMAND is the command template to execute. Use {} as placeholder\n");
    fprintf(stderr, "for input lines, or {} will be appended if not present.\n\n");
    fprintf(stderr, "Examples:\n");
    fprintf(stderr, "  echo -e 'file1\\nfile2' | %s -n 2 'echo {}'\n", program);
    fprintf(stderr, "  %s -n 2 -i files.txt 'grep pattern {}'\n", program);
    exit(FAILURE);
}


static int execute_commands(char **input_lines, int num_lines)
{
    int result = SUCCESS;
    int i;
    
    for (i = 0; i < num_lines; i++) {
        char *expanded_command = expand_template(command_template, input_lines[i]);
        if (!expanded_command) {
            fprintf(stderr, "pexec: failed to expand command template\n");
            result = FAILURE;
            break;
        }
        
        if (dry_run) {
            printf("Would execute: %s\n", expanded_command);
            free(expanded_command);
            continue;
        }
        
        /* Wait for a process slot if we're at max capacity */
        while (num_processes >= max_procs) {
            if (wait_for_processes() != SUCCESS) {
                free(expanded_command);
                return FAILURE;
            }
        }
        
        /* Spawn new process */
        if (spawn_command(expanded_command, i) != SUCCESS) {
            fprintf(stderr, "pexec: failed to spawn process for: %s\n", expanded_command);
            free(expanded_command);
            result = FAILURE;
            break;
        }
        
        free(expanded_command);
    }
    
    /* Wait for all remaining processes */
    while (num_processes > 0) {
        if (wait_for_processes() != SUCCESS) {
            result = FAILURE;
            break;
        }
    }
    
    return result;
}

static char *expand_template(const char *template, const char *input)
{
    char *result;
    const char *placeholder = "{}";
    char *pos;
    int template_len, input_len, result_len;
    
    template_len = strlen(template);
    input_len = strlen(input);
    
    /* Check if template contains placeholder */
    pos = strstr(template, placeholder);
    if (pos) {
        /* Replace placeholder with input */
        result_len = template_len - 2 + input_len + 1;
        result = malloc(result_len);
        if (!result) return NULL;
        
        strncpy(result, template, pos - template);
        result[pos - template] = '\0';
        strcat(result, input);
        strcat(result, pos + 2);
    } else {
        /* Append input to template */
        result_len = template_len + 1 + input_len + 1;
        result = malloc(result_len);
        if (!result) return NULL;
        
        strcpy(result, template);
        strcat(result, " ");
        strcat(result, input);
    }
    
    return result;
}

static int spawn_command(const char *command, int job_num)
{
    int i;
    char pipe_name[64];
    char full_command[MAX_COMMAND_LENGTH];
    struct Task *current_task;
    LONG result;
    
    /* Find available process slot */
    for (i = 0; i < max_procs; i++) {
        if ((processes + i)->pid == 0) break;
    }
    
    if (i >= max_procs) {
        return FAILURE;
    }
    
    /* Create unique pipe name using mktemp */
    strcpy(pipe_name, PIPEPATH);
    if (!mktemp(pipe_name)) {
        fprintf(stderr, "pexec: failed to create unique pipe name\n");
        return FAILURE;
    }
    
    /* Build command with pipe redirection */
    snprintf(full_command, sizeof(full_command), 
             "run >%s <NIL: %s", pipe_name, command);
    
    if (verbose) {
        printf("Executing: %s\n", full_command);
    }
    
    /* Execute command using AmigaOS SystemTags function (like nice.c) */
    result = SystemTags(full_command, SYS_Input, Input(), SYS_Output, Output(), TAG_DONE);
    if (result != 0) {
        /* Get current task address for PID */
        current_task = FindTask(NULL);
        
        /* Store process info */
        (processes + i)->pid = (int)current_task; /* Use task address as PID */
        (processes + i)->command = strdup(command);
        (processes + i)->pipe_name = strdup(pipe_name);
        (processes + i)->job_num = job_num;
        
        num_processes++;
        
        if (verbose) {
            printf("Started process %d (PID: %p): %s\n", job_num, (void*)current_task, command);
        }
        
        return SUCCESS;
    } else {
        PrintFault(IoErr(), "pexec");
        fprintf(stderr, "pexec: Execute failed for: %s\n", command);
        return FAILURE;
    }
}

static int wait_for_processes(void)
{
    int result = SUCCESS;
    int i;
    
    /* On Amiga, we can't really wait for processes like on Unix */
    /* This is a simplified implementation that just marks processes as complete */
    
    for (i = 0; i < max_procs; i++) {
        if ((processes + i)->pid != 0) {
            /* Simulate process completion */
            (processes + i)->pid = 0;
            num_processes--;
            
            if (verbose) {
                printf("Process %d (PID: %p) completed\n", (processes + i)->job_num, (void*)(processes + i)->pid);
            }
            
            /* Clean up pipe file if it exists */
            if ((processes + i)->pipe_name) {
                /* Could read from pipe here if needed */
                free((processes + i)->pipe_name);
                (processes + i)->pipe_name = NULL;
            }
            
            break;
        }
    }
    
    return result;
}

static void cleanup_processes(void)
{
    if (processes) {
        int i;
        for (i = 0; i < max_procs; i++) {
            if ((processes + i)->command) {
                free((processes + i)->command);
            }
            if ((processes + i)->pipe_name) {
                free((processes + i)->pipe_name);
            }
        }
        free(processes);
        processes = NULL;
    }
}