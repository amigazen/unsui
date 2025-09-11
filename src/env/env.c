/*
 * env - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * This version integrates AmigaDOS ReadArgs and standard POSIX getopt
 * for command-line parsing, providing both POSIX compatibility and
 * Amiga native functionality using dos.library environment functions.
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
#include <dos/var.h>

#include "common.h"
#include "getopt.h"

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

/* Version tag for Amiga */
static const char *verstag = "$VER: env 1.0 (11/09/25)\n";
static const char *stack_cookie = "$STACK: 4096";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_IGNORE_ENV,
    ARG_NULL,
    ARG_UNSET,
    ARG_VERBOSE,
    ARG_COMMAND,
    ARG_POSIX,
    ARG_COUNT
};

/* Maximum number of environment variables we can handle */
#define MAX_ENV_VARS 256
#define MAX_ENV_SIZE 4096

/* Global options structure */
typedef struct {
    BOOL ignore_env_flag;       /* -i: ignore environment */
    BOOL null_flag;             /* -0: end output with null instead of newline */
    BOOL verbose_flag;          /* -v: verbose output */
    char **unset_vars;          /* Variables to unset */
    int unset_count;            /* Number of variables to unset */
    char **env_vars;            /* Environment variables to set */
    int env_count;              /* Number of environment variables to set */
    char **command_args;        /* Command and arguments to execute */
    int command_count;          /* Number of command arguments */
    int exit_code;              /* Exit code */
} EnvOptions;

/* Function declarations for forward references */
void usage(const char *program);
void print_version(const char *program);
int run_env_logic(EnvOptions *options, const char *program);
void parse_getopt_args(int argc, char **argv, EnvOptions *options, int *command_start, const char *program);
void init_options(EnvOptions *options);
void cleanup_options(EnvOptions *options);
int print_environment(EnvOptions *options);
int set_environment_vars(EnvOptions *options);
int unset_environment_vars(EnvOptions *options);
int execute_command(EnvOptions *options, const char *program);
char *get_env_var(const char *name);
int set_env_var(const char *name, const char *value);
int unset_env_var(const char *name);
void print_env_var(const char *name, const char *value, EnvOptions *options);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    EnvOptions options;
    int command_start = 1;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "IGNORE-ENV/S,NULL/S,UNSET/K,VERBOSE/S,COMMAND/K,POSIX/K/F";
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

    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);

    /* Initialize options */
    init_options(&options);

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &options, &command_start, program);
        
        return run_env_logic(&options, program);
        
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
            printf("IGNORE/S,NULL/S,UNSET/K,VERBOSE/S,COMMAND/K,POSIX/K/F: ");
            
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
                        parse_getopt_args(new_argc, new_argv, &options, &command_start, program);
                        ret_code = run_env_logic(&options, program);
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
                        if (arg_array[ARG_IGNORE_ENV]) options.ignore_env_flag = TRUE;
                        if (arg_array[ARG_NULL]) options.null_flag = TRUE;
                        if (arg_array[ARG_VERBOSE]) options.verbose_flag = TRUE;
                        
                        if (arg_array[ARG_UNSET]) {
                            /* Parse unset variables */
                            char *unset_str = (char *)arg_array[ARG_UNSET];
                            char *token = strtok(unset_str, ",");
                            while (token && options.unset_count < MAX_ENV_VARS) {
                                options.unset_vars[options.unset_count] = strdup(token);
                                options.unset_count++;
                                token = strtok(NULL, ",");
                            }
                        }
                        
                        if (arg_array[ARG_COMMAND]) {
                            /* Parse command and arguments */
                            char *cmd_str = (char *)arg_array[ARG_COMMAND];
                            char *token = strtok(cmd_str, " ");
                            while (token && options.command_count < MAX_TEMPLATE_ITEMS) {
                                options.command_args[options.command_count] = strdup(token);
                                options.command_count++;
                                token = strtok(NULL, " ");
                            }
                        }
                        
                        ret_code = run_env_logic(&options, program);
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
                if (arg_array[ARG_IGNORE_ENV]) options.ignore_env_flag = TRUE;
                if (arg_array[ARG_NULL]) options.null_flag = TRUE;
                if (arg_array[ARG_VERBOSE]) options.verbose_flag = TRUE;
                
                if (arg_array[ARG_UNSET]) {
                    /* Parse unset variables */
                    char *unset_str = (char *)arg_array[ARG_UNSET];
                    char *token = strtok(unset_str, ",");
                    while (token && options.unset_count < MAX_ENV_VARS) {
                        options.unset_vars[options.unset_count] = strdup(token);
                        options.unset_count++;
                        token = strtok(NULL, ",");
                    }
                }
                
                if (arg_array[ARG_COMMAND]) {
                    /* Parse command and arguments */
                    char *cmd_str = (char *)arg_array[ARG_COMMAND];
                    char *token = strtok(cmd_str, " ");
                    while (token && options.command_count < MAX_TEMPLATE_ITEMS) {
                        options.command_args[options.command_count] = strdup(token);
                        options.command_count++;
                        token = strtok(NULL, " ");
                    }
                }
                
                /* Check for POSIX escape hatch */
                if (arg_array[ARG_POSIX]) {
                    posix_str = (char *)arg_array[ARG_POSIX];
                    
                    /* Build new argv for getopt parsing */
                    new_argv[0] = program;
                    new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;
                    
                    /* Parse with getopt */
                    parse_getopt_args(new_argc, new_argv, &options, &command_start, program);
                }
                
                ret_code = run_env_logic(&options, program);
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
 * @param command_start Index where command starts in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, EnvOptions *options, int *command_start, const char *program) {
    int c;
    int i;
    char *equals_pos;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "i0vVh")) != -1) {
        switch (c) {
            case 'i':
                options->ignore_env_flag = TRUE;
                break;
            case '0':
                options->null_flag = TRUE;
                break;
            case 'v':
                options->verbose_flag = TRUE;
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
    
    *command_start = optind;
    
    /* Parse remaining arguments for environment variables and command */
    for (i = optind; i < argc; i++) {
        if (argv[i] && (equals_pos = strchr(argv[i], '='))) {
            /* This is an environment variable assignment */
            if (options->env_count < MAX_ENV_VARS) {
                options->env_vars[options->env_count] = strdup(argv[i]);
                options->env_count++;
            }
        } else {
            /* This is the start of the command */
            if (options->command_count < MAX_TEMPLATE_ITEMS) {
                options->command_args[options->command_count] = strdup(argv[i]);
                options->command_count++;
            }
        }
    }
}

/**
 * @brief Core env logic separated from argument parsing
 * @param options Options structure
 * @param program Program name for error messages
 * @return Exit code
 */
int run_env_logic(EnvOptions *options, const char *program) {
    int result = SUCCESS;
    
    /* If ignore environment flag is set, clear all environment variables */
    if (options->ignore_env_flag) {
        /* Clear all environment variables by setting them to empty */
        /* This is a simplified approach - in a real implementation,
           we would need to iterate through all existing variables */
    }
    
    /* Unset specified variables */
    if (options->unset_count > 0) {
        if (unset_environment_vars(options) != SUCCESS) {
            result = FAILURE;
        }
    }
    
    /* Set specified environment variables */
    if (options->env_count > 0) {
        if (set_environment_vars(options) != SUCCESS) {
            result = FAILURE;
        }
    }
    
    /* If no command specified, print environment */
    if (options->command_count == 0) {
        if (print_environment(options) != SUCCESS) {
            result = FAILURE;
        }
    } else {
        /* Execute command with modified environment */
        if (execute_command(options, program) != SUCCESS) {
            result = FAILURE;
        }
    }
    
    return result;
}

/**
 * @brief Initialize options structure
 * @param options Options structure to initialize
 */
void init_options(EnvOptions *options) {
    options->ignore_env_flag = FALSE;
    options->null_flag = FALSE;
    options->verbose_flag = FALSE;
    options->unset_vars = calloc(MAX_ENV_VARS, sizeof(char *));
    options->unset_count = 0;
    options->env_vars = calloc(MAX_ENV_VARS, sizeof(char *));
    options->env_count = 0;
    options->command_args = calloc(MAX_TEMPLATE_ITEMS, sizeof(char *));
    options->command_count = 0;
    options->exit_code = SUCCESS;
}

/**
 * @brief Clean up options structure
 * @param options Options structure to clean up
 */
void cleanup_options(EnvOptions *options) {
    int i;
    
    if (options->unset_vars) {
        for (i = 0; i < options->unset_count; i++) {
            if (options->unset_vars[i]) {
                free(options->unset_vars[i]);
            }
        }
        free(options->unset_vars);
    }
    
    if (options->env_vars) {
        for (i = 0; i < options->env_count; i++) {
            if (options->env_vars[i]) {
                free(options->env_vars[i]);
            }
        }
        free(options->env_vars);
    }
    
    if (options->command_args) {
        for (i = 0; i < options->command_count; i++) {
            if (options->command_args[i]) {
                free(options->command_args[i]);
            }
        }
        free(options->command_args);
    }
}

/**
 * @brief Print all environment variables
 * @param options Options structure
 * @return SUCCESS or FAILURE
 */
int print_environment(EnvOptions *options) {
    struct Process *process;
    struct LocalVar *local_var;
    BPTR lock;
    struct FileInfoBlock fib;
    char env_path[256];
    char var_value[256];
    FILE *f;
    int len;
    int found_any = 0;
    
    /* Get current process */
    process = (struct Process *)FindTask(NULL);
    if (!process) {
        fprintf(stderr, "env: cannot get current process\n");
        return FAILURE;
    }
    
    /* Print local variables from pr_LocalVars list */
    local_var = (struct LocalVar *)process->pr_LocalVars.mlh_Head;
    while (local_var->lv_Node.ln_Succ) {
        /* Check if this is a variable (not an alias) */
        if (local_var->lv_Node.ln_Type == LV_VAR) {
            /* Create a null-terminated copy of the value */
            if (local_var->lv_Value && local_var->lv_Len > 0) {
                int copy_len = (local_var->lv_Len < sizeof(var_value) - 1) ? 
                               local_var->lv_Len : sizeof(var_value) - 1;
                strncpy(var_value, local_var->lv_Value, copy_len);
                var_value[copy_len] = '\0';
                
                
                /* Print the local variable */
                print_env_var(local_var->lv_Node.ln_Name, var_value, options);
                found_any = 1;
            }
        }
        local_var = (struct LocalVar *)local_var->lv_Node.ln_Succ;
    }
    
    /* Print global variables from ENV: directory (top-level files only) */
    if ((lock = Lock("ENV:", ACCESS_READ))) {
        /* Read all environment variables from ENV: */
        /* First, examine the directory to get the first entry */
        if (Examine(lock, &fib)) {
            
            /* Skip . and .. entries */
            if (strcmp(fib.fib_FileName, ".") != 0 && strcmp(fib.fib_FileName, "..") != 0) {
                /* Only process files, not directories (top-level only) */
                if (fib.fib_DirEntryType <= 0) {
                    /* Build full path to environment variable file */
                    snprintf(env_path, sizeof(env_path), "ENV:%s", fib.fib_FileName);
                    
                    /* Read the environment variable value */
                    if ((f = fopen(env_path, "r"))) {
                        len = fread(var_value, 1, sizeof(var_value) - 1, f);
                        fclose(f);
                        
                        if (len > 0) {
                            /* Remove trailing newline if present */
                            if (var_value[len - 1] == '\n') {
                                len--;
                            }
                            var_value[len] = '\0';
                            
                            /* Print the global environment variable */
                            print_env_var(fib.fib_FileName, var_value, options);
                            found_any = 1;
                        }
                    }
                }
            }
            
            /* Now continue with ExNext for remaining entries */
            while (ExNext(lock, &fib)) {
                
                /* Skip . and .. entries */
                if (strcmp(fib.fib_FileName, ".") == 0 || strcmp(fib.fib_FileName, "..") == 0) {
                    continue;
                }
                
                /* Only process files, not directories (top-level only) */
                if (fib.fib_DirEntryType <= 0) {
                    /* Build full path to environment variable file */
                    snprintf(env_path, sizeof(env_path), "ENV:%s", fib.fib_FileName);
                    
                    /* Read the environment variable value */
                    if ((f = fopen(env_path, "r"))) {
                        len = fread(var_value, 1, sizeof(var_value) - 1, f);
                        fclose(f);
                        
                        if (len > 0) {
                            /* Remove trailing newline if present */
                            if (var_value[len - 1] == '\n') {
                                len--;
                            }
                            var_value[len] = '\0';
                            
                            /* Print the global environment variable */
                            print_env_var(fib.fib_FileName, var_value, options);
                            found_any = 1;
                        }
                    }
                }
            }
        }
        UnLock(lock);
    }
    
    /* If no variables found, print a message */
    if (!found_any) {
        printf("No environment variables found.\n");
    }
    
    return SUCCESS;
}

/**
 * @brief Set environment variables
 * @param options Options structure
 * @return SUCCESS or FAILURE
 */
int set_environment_vars(EnvOptions *options) {
    int i;
    char *equals_pos;
    char *name;
    char *value;
    
    for (i = 0; i < options->env_count; i++) {
        if (options->env_vars[i]) {
            equals_pos = strchr(options->env_vars[i], '=');
            if (equals_pos) {
                *equals_pos = '\0';
                name = options->env_vars[i];
                value = equals_pos + 1;
                
                if (set_env_var(name, value) != SUCCESS) {
                    fprintf(stderr, "env: cannot set environment variable '%s'\n", name);
                    return FAILURE;
                }
                
                if (options->verbose_flag) {
                    printf("env: setting %s=%s\n", name, value);
                }
            }
        }
    }
    
    return SUCCESS;
}

/**
 * @brief Unset environment variables
 * @param options Options structure
 * @return SUCCESS or FAILURE
 */
int unset_environment_vars(EnvOptions *options) {
    int i;
    
    for (i = 0; i < options->unset_count; i++) {
        if (options->unset_vars[i]) {
            if (unset_env_var(options->unset_vars[i]) != SUCCESS) {
                fprintf(stderr, "env: cannot unset environment variable '%s'\n", options->unset_vars[i]);
                return FAILURE;
            }
            
            if (options->verbose_flag) {
                printf("env: unsetting %s\n", options->unset_vars[i]);
            }
        }
    }
    
    return SUCCESS;
}

/**
 * @brief Execute command with modified environment
 * @param options Options structure
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
int execute_command(EnvOptions *options, const char *program) {
    char *command_line;
    int i;
    int cmd_len = 0;
    int result;
    
    if (options->command_count == 0) {
        return SUCCESS;
    }
    
    /* Build command line */
    for (i = 0; i < options->command_count; i++) {
        if (options->command_args[i]) {
            cmd_len += strlen(options->command_args[i]) + 1;
        }
    }
    
    command_line = malloc(cmd_len + 1);
    if (!command_line) {
        fprintf(stderr, "%s: out of memory\n", program);
        return FAILURE;
    }
    
    command_line[0] = '\0';
    for (i = 0; i < options->command_count; i++) {
        if (options->command_args[i]) {
            if (i > 0) {
                strcat(command_line, " ");
            }
            strcat(command_line, options->command_args[i]);
        }
    }
    
    /* Execute the command using system() - it will inherit our modified environment */
    result = system(command_line);
    
    if (result == -1) {
        fprintf(stderr, "%s: cannot execute command '%s'\n", program, command_line);
        free(command_line);
        return FAILURE;
    }
    
    free(command_line);
    
    return SUCCESS;
}

/**
 * @brief Get environment variable value
 * @param name Variable name
 * @return Variable value or NULL if not found
 */
char *get_env_var(const char *name) {
    char *value = NULL;
    int len;
    
    value = malloc(MAX_ENV_SIZE);
    if (value) {
        /* GetVar by default checks local first, then global */
        len = GetVar((STRPTR)name, value, MAX_ENV_SIZE - 1, LV_VAR);
        
        if (len > 0) {
            value[len] = '\0';
        } else {
            free(value);
            value = NULL;
        }
    }
    
    return value;
}

/**
 * @brief Set environment variable
 * @param name Variable name
 * @param value Variable value
 * @return SUCCESS or FAILURE
 */
int set_env_var(const char *name, const char *value) {
    /* SetVar by default creates/updates local variables first */
    if (SetVar((STRPTR)name, (STRPTR)value, -1, LV_VAR)) {
        return SUCCESS;
    }
    
    /* If local fails, try global */
    if (SetVar((STRPTR)name, (STRPTR)value, -1, GVF_GLOBAL_ONLY | LV_VAR)) {
        return SUCCESS;
    }
    
    return FAILURE;
}

/**
 * @brief Unset environment variable
 * @param name Variable name
 * @return SUCCESS or FAILURE
 */
int unset_env_var(const char *name) {
    /* DeleteVar by default tries local first, then global */
    if (DeleteVar((STRPTR)name, LV_VAR)) {
        return SUCCESS;
    }
    
    return FAILURE;
}

/**
 * @brief Print environment variable in appropriate format
 * @param name Variable name
 * @param value Variable value
 * @param options Options structure
 */
void print_env_var(const char *name, const char *value, EnvOptions *options) {
    if (options->null_flag) {
        printf("%s=%s%c", name, value, '\0');
    } else {
        printf("%s=%s\n", name, value);
    }
}

/**
 * @brief Print usage information
 * @param program Program name
 */
void usage(const char *program) {
    printf("Usage (POSIX): %s [OPTION]... [-] [NAME=VALUE]... [COMMAND [ARG]...]\n", program);
    printf("Usage (Amiga): %s IGNORE/S NULL/S UNSET/K VERBOSE/S COMMAND/K POSIX/K/F\n", program);
    printf("Set each NAME to VALUE in the environment and run COMMAND.\n\n");
    printf("A mere - implies -i.  If no COMMAND, print the resulting environment.\n\n");
    printf("  -i, --ignore-environment  start with an empty local environment\n");
    printf("  -0, --null               end each output line with NUL, not newline\n");
    printf("  -v, --verbose            print verbose information for each processing step\n");
    printf("  -h, --help               display this help and exit\n");
    printf("  -V, --version            output version information and exit\n\n");
    printf("If no COMMAND is specified, print the resulting environment.\n\n");
}

/**
 * @brief Print version information
 * @param program Program name
 */
void print_version(const char *program) {
    printf("%s", verstag);
}
