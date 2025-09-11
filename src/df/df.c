/*
 * df - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * This version integrates AmigaDOS ReadArgs and standard POSIX getopt
 * for command-line parsing, providing both POSIX compatibility and
 * Amiga native functionality using dos.library Info API.
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
#include <exec/lists.h>
#include <utility/utility.h>

#include <proto/dos.h>
#include <proto/exec.h>
#include <proto/utility.h>

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

/* Version tag for Amiga */
static const char *verstag = "$VER: df 1.0 (10/09/25)\n";
static const char *stack_cookie = "$STACK: 4096";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_FILESYSTEMS,
    ARG_ALL,
    ARG_KILOBYTES,
    ARG_PORTABILITY,
    ARG_INODES,
    ARG_TYPE,
    ARG_POSIX,
    ARG_COUNT
};

/* Global options structure */
typedef struct {
    BOOL all_flag;              /* -a: show all filesystems including zero-size */
    BOOL kilobytes_flag;        /* -k: use 1K blocks instead of 512-byte blocks */
    BOOL portability_flag;      /* -P: use POSIX output format */
    BOOL inodes_flag;           /* -i: show inode usage instead of block usage */
    char **fs_types;            /* -t: filesystem types to include/exclude */
    int fs_type_count;          /* Number of filesystem types */
    int exit_code;              /* Exit code */
} DfOptions;

/* Filesystem information structure */
typedef struct {
    char name[32];              /* Device name (e.g., "DF0:") */
    char mount_point[256];      /* Mount point or volume name */
    char fs_type[32];           /* Filesystem type */
    ULONG total_blocks;         /* Total blocks */
    ULONG used_blocks;          /* Used blocks */
    ULONG free_blocks;          /* Free blocks */
    ULONG block_size;           /* Block size in bytes */
    ULONG total_inodes;         /* Total inodes (if available) */
    ULONG used_inodes;          /* Used inodes (if available) */
    ULONG free_inodes;          /* Free inodes (if available) */
    BOOL is_mounted;            /* TRUE if mounted */
} FilesystemInfo;

/* Function declarations for forward references */
void usage(const char *program);
void print_version(const char *program);
int run_df_logic(DfOptions *options, int fs_count, char **filesystems, const char *program);
void parse_getopt_args(int argc, char **argv, DfOptions *options, int *fs_start, const char *program);
void init_options(DfOptions *options);
void cleanup_options(DfOptions *options);

/* Static function declarations */
static int enumerate_filesystems(DfOptions *options, FilesystemInfo **fs_list, int *fs_count, const char *program);
static void print_df_header(DfOptions *options);
static void print_filesystem_info(FilesystemInfo *fs, DfOptions *options);
static void print_error(const char *program, const char *filesystem, LONG error_code);
static int get_filesystem_info(const char *device_name, FilesystemInfo *fs, DfOptions *options, const char *program);
static void format_size(ULONG size, DfOptions *options, char *buffer, int buffer_size);
static int calculate_capacity(ULONG used, ULONG total);
static int parse_blocksize_env(void);
static BOOL matches_fs_type(const char *fs_type, DfOptions *options);
static void add_fs_type(DfOptions *options, const char *type);
static void free_fs_types(DfOptions *options);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    DfOptions options;
    int fs_start = 1;
    char **filesystems = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FS=FILESYSTEMS/M,ALL/S,KB=KILOBYTES/S,P=PORTABILITY/S,INODES/S,TYPE/K,POSIX/K/F";
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
    int fs_count;
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
        parse_getopt_args(argc, argv, &options, &fs_start, program);
        filesystems = &argv[fs_start];
        fs_count = argc - fs_start;
        
        return run_df_logic(&options, fs_count, filesystems, program);
        
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
                        parse_getopt_args(new_argc, new_argv, &options, &fs_start, program);
                        filesystems = &new_argv[fs_start];
                        fs_count = new_argc - fs_start;
                        
                        ret_code = run_df_logic(&options, fs_count, filesystems, program);
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
                        if (arg_array[ARG_ALL]) options.all_flag = TRUE;
                        if (arg_array[ARG_KILOBYTES]) options.kilobytes_flag = TRUE;
                        if (arg_array[ARG_PORTABILITY]) options.portability_flag = TRUE;
                        if (arg_array[ARG_INODES]) options.inodes_flag = TRUE;
                        if (arg_array[ARG_TYPE]) {
                            char *type_str = (char *)arg_array[ARG_TYPE];
                            add_fs_type(&options, type_str);
                        }
                        
                        if (arg_array[ARG_FILESYSTEMS]) {
                            filesystems = (char **)arg_array[ARG_FILESYSTEMS];
                            /* Count the number of filesystems */
                            fs_count = 0;
                            while (filesystems[fs_count]) fs_count++;
                            
                            ret_code = run_df_logic(&options, fs_count, filesystems, program);
                        } else {
                            /* No specific filesystem requested, show all */
                            ret_code = run_df_logic(&options, 0, NULL, program);
                        }
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
                if (arg_array[ARG_ALL]) options.all_flag = TRUE;
                if (arg_array[ARG_KILOBYTES]) options.kilobytes_flag = TRUE;
                if (arg_array[ARG_PORTABILITY]) options.portability_flag = TRUE;
                if (arg_array[ARG_INODES]) options.inodes_flag = TRUE;
                if (arg_array[ARG_TYPE]) {
                    char *type_str = (char *)arg_array[ARG_TYPE];
                    add_fs_type(&options, type_str);
                }
                
                if (arg_array[ARG_FILESYSTEMS]) {
                    filesystems = (char **)arg_array[ARG_FILESYSTEMS];
                    /* Count the number of filesystems */
                    fs_count = 0;
                    while (filesystems[fs_count]) fs_count++;
                    
                    ret_code = run_df_logic(&options, fs_count, filesystems, program);
                } else {
                    /* No specific filesystem requested, show all */
                    ret_code = run_df_logic(&options, 0, NULL, program);
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
 * @param fs_start Index where filesystems start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, DfOptions *options, int *fs_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "aikPt:hV")) != -1) {
        switch (c) {
            case 'a':
                options->all_flag = TRUE;
                break;
            case 'i':
                options->inodes_flag = TRUE;
                break;
            case 'k':
                options->kilobytes_flag = TRUE;
                break;
            case 'P':
                options->portability_flag = TRUE;
                break;
            case 't':
                add_fs_type(options, optarg);
                break;
            case 'h':
                usage(program);
                exit(SUCCESS);
                break;
            case 'V':
                print_version(program);
                exit(SUCCESS);
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    *fs_start = optind;
}

/**
 * @brief Core df logic separated from argument parsing
 * @param options Options structure
 * @param fs_count Number of filesystems to process (0 = all)
 * @param filesystems Array of filesystem names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_df_logic(DfOptions *options, int fs_count, char **filesystems, const char *program) {
    FilesystemInfo *fs_list = NULL;
    int total_fs_count = 0;
    int i;
    int result = SUCCESS;
    
    /* Enumerate all filesystems */
    if (enumerate_filesystems(options, &fs_list, &total_fs_count, program) != SUCCESS) {
        return FAILURE;
    }
    
    /* Print header */
    print_df_header(options);
    
    /* Print filesystem information */
    for (i = 0; i < total_fs_count; i++) {
        /* If specific filesystems requested, check if this one matches */
        if (fs_count > 0 && filesystems) {
            int j;
            BOOL found = FALSE;
            for (j = 0; j < fs_count; j++) {
                if (filesystems[j]) {
                    /* Check for exact device name match (e.g., "DF0:" matches "df0:") */
                    if (Stricmp(fs_list[i].name, filesystems[j]) == 0) {
                        found = TRUE;
                        break;
                    }
                    /* Check for device name without colon (e.g., "DF0" matches "df0:") */
                    if (strlen(filesystems[j]) > 0 && filesystems[j][strlen(filesystems[j])-1] != ':' &&
                        Stricmp(fs_list[i].name, filesystems[j]) == 0) {
                        found = TRUE;
                        break;
                    }
                    /* Check for mount point match */
                    if (Stricmp(fs_list[i].mount_point, filesystems[j]) == 0) {
                        found = TRUE;
                        break;
                    }
                }
            }
            if (!found) continue;
        }
        
        /* Check filesystem type filtering */
        if (options->fs_type_count > 0 && !matches_fs_type(fs_list[i].fs_type, options)) {
            continue;
        }
        
        /* Skip zero-size filesystems unless -a flag is set */
        if (!options->all_flag && fs_list[i].total_blocks == 0) {
            continue;
        }
        
        print_filesystem_info(&fs_list[i], options);
    }
    
    /* Check if any requested filesystems were not found */
    if (fs_count > 0 && filesystems) {
        int j, k;
        BOOL found_any = FALSE;
        for (j = 0; j < fs_count; j++) {
            if (filesystems[j]) {
                BOOL found = FALSE;
                for (k = 0; k < total_fs_count; k++) {
                    if (Stricmp(fs_list[k].name, filesystems[j]) == 0 ||
                        (strlen(filesystems[j]) > 0 && filesystems[j][strlen(filesystems[j])-1] != ':' &&
                         Stricmp(fs_list[k].name, filesystems[j]) == 0) ||
                        Stricmp(fs_list[k].mount_point, filesystems[j]) == 0) {
                        found = TRUE;
                        found_any = TRUE;
                        break;
                    }
                }
                if (!found) {
                    fprintf(stderr, "%s: %s: No such file or directory\n", program, filesystems[j]);
                    result = FAILURE;
                }
            }
        }
    }
    
    /* Free filesystem list */
    if (fs_list) {
        free(fs_list);
    }
    
    return result;
}

/**
 * @brief Initialize options structure
 * @param options Options structure to initialize
 */
void init_options(DfOptions *options) {
    options->all_flag = FALSE;
    options->kilobytes_flag = FALSE;
    options->portability_flag = FALSE;
    options->inodes_flag = FALSE;
    options->fs_types = NULL;
    options->fs_type_count = 0;
    options->exit_code = SUCCESS;
    
    /* Check BLOCKSIZE environment variable */
    if (!options->kilobytes_flag && !options->portability_flag) {
        int env_blocksize = parse_blocksize_env();
        if (env_blocksize == 1024) {
            options->kilobytes_flag = TRUE;
        }
    }
}

/**
 * @brief Clean up options structure
 * @param options Options structure to clean up
 */
void cleanup_options(DfOptions *options) {
    free_fs_types(options);
}

/**
 * @brief Enumerate all available filesystems using Amiga DOS
 * @param options Options structure
 * @param fs_list Pointer to filesystem list (will be allocated)
 * @param fs_count Pointer to count of filesystems found
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
static int enumerate_filesystems(DfOptions *options, FilesystemInfo **fs_list, int *fs_count, const char *program) {
    struct DosList *ndl, *dlist;
    int max_fs = 32;  /* Start with reasonable number */
    int current_fs = 0;
    char name[108];
    
    /* Allocate initial filesystem list */
    *fs_list = (FilesystemInfo *)malloc(max_fs * sizeof(FilesystemInfo));
    if (!*fs_list) {
        fprintf(stderr, "%s: out of memory\n", program);
        return FAILURE;
    }
    
    /* Lock list of devices and volumes */
    ndl = dlist = LockDosList(LDF_DEVICES|LDF_VOLUMES|LDF_READ);
    if (!dlist) {
        fprintf(stderr, "%s: cannot access filesystem list\n", program);
        free(*fs_list);
        return FAILURE;
    }
    
    /* Enumerate devices first */
    while (ndl = NextDosEntry(ndl, LDF_DEVICES|LDF_READ)) {
        /* Skip if no file device */
        if (!ndl->dol_Task) continue;
        
        /* Check if we need to expand the list */
        if (current_fs >= max_fs) {
            max_fs *= 2;
            *fs_list = (FilesystemInfo *)realloc(*fs_list, max_fs * sizeof(FilesystemInfo));
            if (!*fs_list) {
                fprintf(stderr, "%s: out of memory\n", program);
                UnLockDosList(LDF_DEVICES|LDF_VOLUMES|LDF_READ);
                return FAILURE;
            }
        }
        
        /* Get device name - following InfoQ.c pattern */
        {
            TEXT *str = ((TEXT *)BADDR(ndl->dol_Name));
            CopyMem(str+1, name, *str);
            name[*str] = ':';
            name[*str+1] = '\0';
        }
        
        /* Initialize filesystem info */
        memset(&(*fs_list)[current_fs], 0, sizeof(FilesystemInfo));
        strcpy((*fs_list)[current_fs].name, name);
        strcpy((*fs_list)[current_fs].mount_point, name);
        strcpy((*fs_list)[current_fs].fs_type, "DOS");  /* Amiga DOS filesystem */
        (*fs_list)[current_fs].is_mounted = TRUE;
        
        /* Get filesystem information - always try to get info */
        get_filesystem_info(name, &(*fs_list)[current_fs], options, program);
        current_fs++;
    }
    
    /* Enumerate volumes - only if we want to show all filesystems */
    if (options->all_flag) {
        ndl = dlist;
        while (ndl = NextDosEntry(ndl, LDF_VOLUMES|LDF_READ)) {
            /* Check if we need to expand the list */
            if (current_fs >= max_fs) {
                max_fs *= 2;
                *fs_list = (FilesystemInfo *)realloc(*fs_list, max_fs * sizeof(FilesystemInfo));
                if (!*fs_list) {
                    fprintf(stderr, "%s: out of memory\n", program);
                    UnLockDosList(LDF_DEVICES|LDF_VOLUMES|LDF_READ);
                    return FAILURE;
                }
            }
            
            /* Get volume name */
            {
                TEXT *str = ((TEXT *)BADDR(ndl->dol_Name));
                CopyMem(str+1, name, *str);
                name[*str] = '\0';
            }
            
            /* Initialize filesystem info */
            memset(&(*fs_list)[current_fs], 0, sizeof(FilesystemInfo));
            strcpy((*fs_list)[current_fs].name, name);
            strcpy((*fs_list)[current_fs].mount_point, name);
            strcpy((*fs_list)[current_fs].fs_type, "VOLUME");  /* Amiga volume */
            (*fs_list)[current_fs].is_mounted = (ndl->dol_Task != NULL);
            
            /* For volumes, we can't get detailed info easily, so set defaults */
            (*fs_list)[current_fs].total_blocks = 0;
            (*fs_list)[current_fs].used_blocks = 0;
            (*fs_list)[current_fs].free_blocks = 0;
            (*fs_list)[current_fs].block_size = 512;
            (*fs_list)[current_fs].total_inodes = 0;
            (*fs_list)[current_fs].used_inodes = 0;
            (*fs_list)[current_fs].free_inodes = 0;
            
            current_fs++;
        }
    }
    
    /* Unlock list */
    UnLockDosList(LDF_DEVICES|LDF_VOLUMES|LDF_READ);
    
    *fs_count = current_fs;
    return SUCCESS;
}

/**
 * @brief Get filesystem information for a specific device
 * @param device_name Name of the device
 * @param fs Filesystem info structure to populate
 * @param options Options structure
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
static int get_filesystem_info(const char *device_name, FilesystemInfo *fs, DfOptions *options, const char *program) {
    BPTR lock;
    struct InfoData id;
    char name_buf[256];
    
    /* Try to lock the device */
    lock = Lock((STRPTR)device_name, ACCESS_READ);
    if (!lock) {
        /* Device not available - check error type */
        LONG error = IoErr();
        if (error == ERROR_NO_DISK || error == ERROR_NOT_A_DOS_DISK) {
            /* Device exists but no disk or unreadable - still show it */
            fs->total_blocks = 0;
            fs->used_blocks = 0;
            fs->free_blocks = 0;
            fs->block_size = 512;
            fs->is_mounted = FALSE;
            return SUCCESS;
        }
        return FAILURE;
    }
    
    /* Get filesystem information */
    if (Info(lock, &id)) {
        fs->total_blocks = id.id_NumBlocks;
        fs->used_blocks = id.id_NumBlocksUsed;
        fs->free_blocks = id.id_NumBlocks - id.id_NumBlocksUsed;
        fs->block_size = id.id_BytesPerBlock;
        fs->is_mounted = TRUE;
        
        /* Get volume name if possible */
        if (NameFromLock(lock, name_buf, sizeof(name_buf))) {
            /* Remove trailing colon if present */
            int len = strlen(name_buf);
            if (len > 0 && name_buf[len-1] == ':') {
                name_buf[len-1] = '\0';
            }
            strcpy(fs->mount_point, name_buf);
        } else {
            /* Fallback to device name */
            strcpy(fs->mount_point, device_name);
        }
        
        UnLock(lock);
        return SUCCESS;
    } else {
        UnLock(lock);
        return FAILURE;
    }
}

/**
 * @brief Print df header
 * @param options Options structure
 */
static void print_df_header(DfOptions *options) {
    if (options->inodes_flag && !options->portability_flag) {
        printf("Filesystem            Inodes   IUsed   IFree  %%IUsed");
    } else {
        if (options->portability_flag) {
            /* POSIX format - exactly as specified */
            printf("Filesystem         %s  Used Available Capacity",
                options->kilobytes_flag ? "1024-blocks" : " 512-blocks");
        } else {
            printf("Filesystem         %s  Used Available Capacity",
                options->kilobytes_flag ? "1024-blocks" : " 512-blocks");
        }
    }
    printf(" Mounted on\n");
}

/**
 * @brief Print filesystem information in df format
 * @param fs Filesystem information
 * @param options Options structure
 */
static void print_filesystem_info(FilesystemInfo *fs, DfOptions *options) {
    char size_buf[32];
    char used_buf[32];
    char avail_buf[32];
    int capacity;
    
    if (options->inodes_flag && !options->portability_flag) {
        /* Inode format - not fully supported on Amiga */
        printf("%-20s %8ld %7ld %7ld %5d%% %s\n",
            fs->name[0] ? fs->name : "unknown",
            fs->total_inodes,
            fs->used_inodes,
            fs->free_inodes,
            0,  /* Capacity calculation for inodes */
            fs->mount_point[0] ? fs->mount_point : "unknown");
    } else {
        /* Block format - POSIX compliant */
        format_size(fs->total_blocks, options, size_buf, sizeof(size_buf));
        format_size(fs->used_blocks, options, used_buf, sizeof(used_buf));
        format_size(fs->free_blocks, options, avail_buf, sizeof(avail_buf));
        
        capacity = calculate_capacity(fs->used_blocks, fs->total_blocks);
        
        if (options->portability_flag) {
            /* POSIX format - exactly one line per filesystem */
            printf("%-20s %8s %8s %8s %5d%% %s\n",
                fs->name[0] ? fs->name : "unknown",
                size_buf,
                used_buf,
                avail_buf,
                capacity,
                fs->mount_point[0] ? fs->mount_point : "unknown");
        } else {
            printf("%-20s %8s %8s %8s %5d%% %s\n",
                fs->name[0] ? fs->name : "unknown",
                size_buf,
                used_buf,
                avail_buf,
                capacity,
                fs->mount_point[0] ? fs->mount_point : "unknown");
        }
    }
}

/**
 * @brief Format size for display
 * @param size Size in blocks
 * @param options Options structure
 * @param buffer Output buffer
 * @param buffer_size Size of output buffer
 */
static void format_size(ULONG size, DfOptions *options, char *buffer, int buffer_size) {
    ULONG display_size;
    
    if (options->kilobytes_flag) {
        /* Convert to 1K blocks */
        display_size = (size * 512) / 1024;
    } else {
        /* Use 512-byte blocks */
        display_size = size;
    }
    
    snprintf(buffer, buffer_size, "%ld", display_size);
}

/**
 * @brief Calculate capacity percentage
 * @param used Used blocks
 * @param total Total blocks
 * @return Capacity percentage (0-100)
 */
static int calculate_capacity(ULONG used, ULONG total) {
    if (total == 0) return 0;
    return (int)((100 * used + total - 1) / total);
}

/**
 * @brief Print error message based on Amiga error code
 * @param program Program name
 * @param filesystem Name of filesystem that caused error
 * @param error_code Amiga error code from IoErr()
 */
static void print_error(const char *program, const char *filesystem, LONG error_code) {
    switch (error_code) {
        case ERROR_NO_DISK:
            fprintf(stderr, "%s: %s: No disk present\n", program, filesystem);
            break;
        case ERROR_NOT_A_DOS_DISK:
            fprintf(stderr, "%s: %s: Unreadable disk\n", program, filesystem);
            break;
        case ERROR_OBJECT_NOT_FOUND:
            fprintf(stderr, "%s: %s: No such file or directory\n", program, filesystem);
            break;
        case ERROR_OBJECT_EXISTS:
            fprintf(stderr, "%s: %s: Object exists\n", program, filesystem);
            break;
        case ERROR_DISK_NOT_VALIDATED:
            fprintf(stderr, "%s: %s: Disk not validated\n", program, filesystem);
            break;
        case ERROR_WRITE_PROTECTED:
            fprintf(stderr, "%s: %s: Write protected\n", program, filesystem);
            break;
        case ERROR_OBJECT_IN_USE:
            fprintf(stderr, "%s: %s: Object in use\n", program, filesystem);
            break;
        default:
            fprintf(stderr, "%s: %s: Error %ld\n", program, filesystem, error_code);
            break;
    }
}

/**
 * @brief Print usage information
 * @param program Program name
 */
void usage(const char *program) {
    printf("Usage (POSIX): %s [OPTION]... [FILE]...\n", program);
    printf("Usage (Amiga): %s [FS=FILESYSTEM/M] [ALL/S] [KB=KILOBYTES/S] [P=PORTABILITY/S] [INODES/S] [TYPE/K] [POSIX/K/F]\n", program);
    printf("Show information about the file system on which each FILE resides,\n");
    printf("or all file systems by default.\n\n");
    printf("  -a, --all          include pseudo, duplicate, inaccessible file systems\n");
    printf("  -i, --inodes       list inode information instead of block usage\n");
    printf("  -k, --kilobytes    use 1024-byte blocks instead of 512-byte blocks\n");
    printf("  -P, --portability  use the POSIX output format\n");
    printf("  -t, --type TYPE    limit listing to filesystems of type TYPE\n");
    printf("  -h, --help         display this help and exit\n");
    printf("  -V, --version      output version information and exit\n\n");
    printf("Values are in 512-byte blocks unless BLOCKSIZE environment variable\n");
    printf("is set or POSIXLY_CORRECT is set, in which case 1024-byte blocks are used.\n\n");
    printf("FILE is a disk device name (e.g., DF0:) or volume name.\n");
}

/**
 * @brief Print version information
 * @param program Program name
 */
void print_version(const char *program) {
    printf("%s", verstag);
}

/**
 * @brief Parse BLOCKSIZE environment variable
 * @return Block size (512 or 1024)
 */
static int parse_blocksize_env(void) {
    int size;
    char *blocksize = getenv("BLOCKSIZE");
    if (!blocksize) return 512;
    
    size = atoi(blocksize);
    if (size == 1024) return 1024;
    if (size == 512) return 512;
    
    /* Default to 512 for other values */
    return 512;
}

/**
 * @brief Check if filesystem type matches filter
 * @param fs_type Filesystem type to check
 * @param options Options structure
 * @return TRUE if matches, FALSE otherwise
 */
static BOOL matches_fs_type(const char *fs_type, DfOptions *options) {
    int i;
    
    if (!options->fs_types || options->fs_type_count == 0) {
        return TRUE;  /* No filtering */
    }
    
    for (i = 0; i < options->fs_type_count; i++) {
        if (strcmp(fs_type, options->fs_types[i]) == 0) {
            return TRUE;
        }
    }
    
    return FALSE;
}

/**
 * @brief Add filesystem type to filter list
 * @param options Options structure
 * @param type Filesystem type to add
 */
static void add_fs_type(DfOptions *options, const char *type) {
    if (!type || strlen(type) == 0) return;
    
    /* Reallocate array */
    options->fs_types = (char **)realloc(options->fs_types, 
        (options->fs_type_count + 1) * sizeof(char *));
    if (!options->fs_types) return;
    
    /* Add new type */
    options->fs_types[options->fs_type_count] = strdup(type);
    if (options->fs_types[options->fs_type_count]) {
        options->fs_type_count++;
    }
}

/**
 * @brief Free filesystem type list
 * @param options Options structure
 */
static void free_fs_types(DfOptions *options) {
    int i;
    
    if (options->fs_types) {
        for (i = 0; i < options->fs_type_count; i++) {
            if (options->fs_types[i]) {
                free(options->fs_types[i]);
            }
        }
        free(options->fs_types);
        options->fs_types = NULL;
        options->fs_type_count = 0;
    }
}
