/*
 * cp - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on ACP by Fred Cassirer, placed in Public Domain.
 * 
 * This version integrates AmigaDOS ReadArgs and standard POSIX getopt
 * for command-line parsing.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */


#include "cp.h"
#include "common.h"
#include "getopt.h"

/* Define stat macros if not available */
#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

#ifndef S_ISDIR
#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif

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


/* Version tag for Amiga */
static const char *verstag = "$VER: cp 2.0 (30/08/25)\n"
;

/* Global variables for getopt - these are defined in getopt.c */
extern char *optarg;
extern int optind;
extern int opterr;
extern int optopt;

/* External references */
extern struct ExecBase *SysBase;

/* Function declarations for forward references */
int copy(const char *filename, const char *destination, int buffersize);
char *scdir(const char *pattern);
void scdir_abort(void);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    int interactive = FALSE;
    int force = FALSE;
    int recursive = FALSE;
    int verbose = FALSE;
    int preserve = FALSE;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "SOURCE/M,DESTINATION/M,INTERACTIVE/S,FORCE/S,RECURSIVE/S,VERBOSE/S,PRESERVE/S,POSIX/K/F";
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
    int file_start;
    int file_count;
    char *dest;
    char **new_files;
    char initial_args_str[256];
    char user_input_buf[256];
    char *temp_str;
    size_t combined_len;

    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);

    if (argc == 1) {
        /* No arguments, show usage */
        usage(program);
        return FAILURE;
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        int file_start;
        parse_getopt_args(argc, argv, &interactive, &force, &recursive, &verbose, &preserve, &file_start, program);
        files = &argv[file_start];
        return run_cp_logic(argc - file_start, files, interactive, force, recursive, verbose, preserve, program);
        
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
            fprintf(stderr, "%s: out of memory for command string\n", program);
            FreeDosObject(DOS_RDARGS, rdargs);
            return FAILURE;
        }

        /* Set up ReadArgs to parse from our string */
        rdargs->RDA_Source.CS_Buffer = cmd_string;
        rdargs->RDA_Source.CS_Length = strlen(cmd_string);
        rdargs->RDA_Source.CS_CurChr = 0;
        rdargs->RDA_Flags |= RDAF_NOPROMPT;

        if (!ReadArgs(template, arg_array, rdargs)) {
            PrintFault(IoErr(), program);
            ret_code = FAILURE;
        } else {
            /* Check for POSIX/F override first */
            if (arg_array[ARG_POSIX]) {
                posix_str = (char *)arg_array[ARG_POSIX];

                /* Tokenize the string and build a new argv for getopt */
                new_argv[0] = program;
                new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;

                parse_getopt_args(new_argc, new_argv, &interactive, &force, &recursive, &verbose, &preserve, &file_start, program);
                files = &new_argv[file_start];
                ret_code = run_cp_logic(new_argc - file_start, files, interactive, force, recursive, verbose, preserve, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_INTERACTIVE]) {
                    interactive = TRUE;
                }
                if (arg_array[ARG_FORCE]) {
                    force = TRUE;
                }
                if (arg_array[ARG_RECURSIVE]) {
                    recursive = TRUE;
                }
                if (arg_array[ARG_VERBOSE]) {
                    verbose = TRUE;
                }
                if (arg_array[ARG_PRESERVE]) {
                    preserve = TRUE;
                }
                
                if (arg_array[ARG_SOURCE] && arg_array[ARG_DESTINATION]) {
                    /* Count source files and allocate array */
                    file_count = 0;
                    while (((char **)arg_array[ARG_SOURCE])[file_count] != NULL) {
                        file_count++;
                    }
                    
                    files = (char **)arg_array[ARG_SOURCE];
                    dest = (char *)arg_array[ARG_DESTINATION];
                    
                    /* Create a new argv-style array for the core logic */
                    new_files = malloc((file_count + 2) * sizeof(char *));
                    if (new_files) {
                        for (i = 0; i < file_count; i++) {
                            new_files[i] = files[i];
                        }
                        new_files[file_count] = dest;
                        new_files[file_count + 1] = NULL;
                        
                        ret_code = run_cp_logic(file_count + 1, new_files, interactive, force, recursive, verbose, preserve, program);
                        free(new_files);
                    } else {
                        ret_code = FAILURE;
                    }
                } else {
                    /* Missing required arguments */
                    fprintf(stderr, "%s: missing file operands\n", program);
                    ret_code = FAILURE;
                }
            }
        }

        /* Clean up */
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
    }

    return ret_code;
}

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param interactive Flag for interactive mode
 * @param force Flag to force overwrite
 * @param recursive Flag for recursive copy
 * @param verbose Flag for verbose output
 * @param preserve Flag to preserve attributes
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, int *interactive, int *force, int *recursive, int *verbose, int *preserve, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "ifRrvphV")) != -1) {
        switch (c) {
            case 'i':
                *interactive = TRUE;
                break;
            case 'f':
                *force = TRUE;
                break;
            case 'R':
            case 'r':
                *recursive = TRUE;
                break;
            case 'v':
                *verbose = TRUE;
                break;
            case 'p':
                *preserve = TRUE;
                break;
            case 'h':
            case 'V':
                usage(program);
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    *file_start = optind;
}

/**
 * @brief Core cp logic separated from argument parsing
 * @param argc Number of arguments (files)
 * @param argv Array of file names (last one is destination)
 * @param interactive Flag for interactive mode
 * @param force Flag to force overwrite
 * @param recursive Flag for recursive copy
 * @param verbose Flag for verbose output
 * @param preserve Flag to preserve attributes
 * @param program Program name for error messages
 * @return Exit code
 */
int run_cp_logic(int argc, char **argv, int interactive, int force, int recursive, int verbose, int preserve, const char *program) {
    int i;
    char *destination;
    int ret_code = SUCCESS;
    
    if (argc < 2) {
        fprintf(stderr, "%s: missing file operands\n", program);
        return FAILURE;
    }
    
    destination = argv[argc - 1];
    
    /* Check if destination is a directory */
    if (is_directory(destination)) {
        /* Copy multiple files to directory */
        for (i = 0; i < argc - 1; i++) {
            if (verbose) {
                printf("'%s' -> '%s'\n", argv[i], destination);
            }
            
            if (is_directory(argv[i])) {
                if (recursive) {
                    if (copy_directory(argv[i], destination, recursive, force, verbose) != SUCCESS) {
                        ret_code = FAILURE;
                    }
                } else {
                    fprintf(stderr, "%s: -r not specified; omitting directory '%s'\n", program, argv[i]);
                    ret_code = FAILURE;
                }
            } else {
                if (copy_file(argv[i], destination, DEFAULT_BUFFER_SIZE, force, verbose) != SUCCESS) {
                    ret_code = FAILURE;
                }
            }
        }
    } else {
        /* Single file copy */
        if (argc > 2) {
            fprintf(stderr, "%s: target '%s' is not a directory\n", program, destination);
            return FAILURE;
        }
        
        if (verbose) {
            printf("'%s' -> '%s'\n", argv[0], destination);
        }
        
        if (is_directory(argv[0])) {
            if (recursive) {
                if (copy_directory(argv[0], destination, recursive, force, verbose) != SUCCESS) {
                    ret_code = FAILURE;
                }
            } else {
                fprintf(stderr, "%s: -r not specified; omitting directory '%s'\n", program, argv[0]);
                ret_code = FAILURE;
            }
        } else {
            if (copy_file(argv[0], destination, DEFAULT_BUFFER_SIZE, force, verbose) != SUCCESS) {
                ret_code = FAILURE;
            }
        }
    }
    
    return ret_code;
}

/**
 * @brief Copy a single file
 * @param source Source file path
 * @param dest Destination path (file or directory)
 * @param buffer_size Buffer size for copying
 * @param force Force overwrite
 * @param verbose Verbose output
 * @return Exit code
 */
int copy_file(const char *source, const char *dest, int buffer_size, int force, int verbose) {
    char dest_path[512];
    char response[10];
    struct stat source_stat, dest_stat;
    
    /* Get source file info */
    if (stat(source, &source_stat) != 0) {
        fprintf(stderr, "cp: cannot stat '%s': %s\n", source, strerror(errno));
        return FAILURE;
    }
    
    /* Check if source is a regular file */
    if (!S_ISREG(source_stat.st_mode)) {
        fprintf(stderr, "cp: '%s' is not a regular file\n", source);
        return FAILURE;
    }
    
    /* Determine destination path */
    if (is_directory(dest)) {
        /* Destination is directory, append source filename */
        char *basename = strrchr(source, '/');
        if (basename) {
            basename++; /* Skip the slash */
        } else {
            basename = (char *)source;
        }
        
        snprintf(dest_path, sizeof(dest_path), "%s/%s", dest, basename);
    } else {
        strncpy(dest_path, dest, sizeof(dest_path) - 1);
        dest_path[sizeof(dest_path) - 1] = '\0';
    }
    
    /* Check if destination exists and handle force flag */
    if (stat(dest_path, &dest_stat) == 0) {
        if (!force) {
            fprintf(stderr, "cp: overwrite '%s'? ", dest_path);
            if (fgets(response, sizeof(response), stdin)) {
                if (response[0] != 'y' && response[0] != 'Y') {
                    if (verbose) {
                        printf("skipped '%s'\n", dest_path);
                    }
                    return SUCCESS;
                }
            }
        }
    }
    
    /* Use the existing Amiga copy function */
    return do_copy(source, dest_path, buffer_size) ? SUCCESS : FAILURE;
}

/**
 * @brief Copy a directory recursively
 * @param source Source directory path
 * @param dest Destination path
 * @param recursive Recursive flag (should be TRUE)
 * @param force Force overwrite
 * @param verbose Verbose output
 * @return Exit code
 */
int copy_directory(const char *source, const char *dest, int recursive, int force, int verbose) {
    struct DPTR *dp;
    int status;
    char *filename;
    char dest_path[1024];
    
    if (verbose) {
        printf("cp: copying directory '%s' to '%s'\n", source, dest);
    }
    
    /* Create destination directory if it doesn't exist */
    if (mkdir(dest) != 0 && errno != EEXIST) {
        fprintf(stderr, "cp: cannot create directory '%s': %s\n", dest, strerror(errno));
        return FAILURE;
    }
    
    /* Open source directory */
    dp = dopen(source, &status);
    if (!dp) {
        fprintf(stderr, "cp: cannot open directory '%s'\n", source);
        return FAILURE;
    }
    
    /* Copy all files in the directory */
    while ((filename = scdir(source)) != NULL) {
        snprintf(dest_path, sizeof(dest_path), "%s/%s", dest, filename);
        
        if (verbose) {
            printf("cp: copying '%s' to '%s'\n", filename, dest_path);
        }
        
        /* Copy the file */
        if (!do_copy(filename, dest_path, DEFAULT_BUFFER_SIZE)) {
            fprintf(stderr, "cp: failed to copy '%s'\n", filename);
            dclose(dp);
            return FAILURE;
        }
    }
    
    dclose(dp);
    return SUCCESS;
}

/**
 * @brief Check if a path is a directory
 * @param path Path to check
 * @return TRUE if directory, FALSE otherwise
 */
int is_directory(const char *path) {
    struct stat st;
    if (stat(path, &st) == 0) {
        return S_ISDIR(st.st_mode);
    }
    return FALSE;
}

/**
 * @brief Check if a path is a regular file
 * @param path Path to check
 * @return TRUE if regular file, FALSE otherwise
 */
int is_regular_file(const char *path) {
    struct stat st;
    if (stat(path, &st) == 0) {
        return S_ISREG(st.st_mode);
    }
    return FALSE;
}

/*
 * Display usage information
 */
void usage(const char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] SOURCE... DEST\n", program);
    fprintf(stderr, "Usage (Amiga): %s SOURCE/M DESTINATION/M [INTERACTIVE/S] [FORCE/S] [RECURSIVE/S] [VERBOSE/S] [PRESERVE/S]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -f, --force          if an existing destination file cannot be\n");
    fprintf(stderr, "                        opened, remove it and try again\n");
    fprintf(stderr, "  -i, --interactive    prompt before overwrite\n");
    fprintf(stderr, "  -R, -r, --recursive  copy directories recursively\n");
    fprintf(stderr, "  -v, --verbose        explain what is being done\n");
    fprintf(stderr, "  -p, --preserve       preserve the specified attributes\n");
    fprintf(stderr, "  -h, -V               display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Copy SOURCE to DEST, or multiple SOURCE(s) to DIRECTORY.\n");
    fprintf(stderr, "  With no FILE, or when FILE is -, read standard input.\n");
    exit(FAILURE);
}

/*
 * dopen(), dclose() taken from Matt Dillon's shell
 *
 * Matthew Dillon, 28 Apr 1986
 *
 */

struct DPTR *
dopen(const char *name, int *stat)
{
   struct DPTR *dp;
   int namelen, endslash = 0;

   namelen = strlen(name);
   if (namelen && name[namelen - 1] == '/') {
      name[namelen - 1] = '\0';
      endslash = 1;
   }
   *stat = 0;
   if (*name == '\0')
     return(NULL);
   else {
    dp = (struct DPTR *)malloc(sizeof(struct DPTR));
    dp->lock = Lock (name, ACCESS_READ);
   }
  if (endslash)
      name[namelen - 1] = '/';
   if (dp->lock == NULL) {
      free (dp);
      return (NULL);
   }
   dp->fib = (struct FileInfoBlock *)
         AllocMem(sizeof(struct FileInfoBlock), MEMF_PUBLIC);
   if (!Examine (dp->lock, dp->fib)) {
      dclose (dp);
      return (NULL);
   }
   if (dp->fib->fib_DirEntryType >= 0)
      *stat = 1; 
   return (dp);
}

int dclose(struct DPTR *dp)
{
   if (dp == NULL)
      return (1);
   if (dp->fib)
      FreeMem (dp->fib, sizeof(*dp->fib));
   if (dp->lock)
      UnLock (dp->lock);
   free (dp);
   return (1);
}

/*
 *   Written by Fred Cassirer, September 2, 1986.  Placed in Public Domain.
 *   This program can be used in any way shape or form as long as:
 *     1) I still get some credit for writing it.
 *     2) It's not used for profit.
 *     3) I'm not held responsible for any bugs, or problems with it's use.
 *
 *      (That should about cover it)
 *
 *   copy() - Copy a file to a destination using unbuffered I/O with
 *            a caller specified buffer size.
 *
 *            If "filename" is the null string, use stdout (not used here).
 *            Ditto for "destination".
 *
 *            If "destination" is a directory, the file is copied under
 *            the same name to the directory, else if the "destination"
 *            is a file, it is (re)created and the "filename" is copied to
 *            it.
 *
 */

 int copy(const char *filename, const char *destination, int buffersize)
 {
  int From,To;
  char to_name[80],*s,*buffer;
  struct DPTR *fdptr,*tdptr;
  int status,flen,dlen,xfersz;

  fdptr = tdptr = NULL;
  flen = strlen(filename);
  status = 0;
  if (flen)
   if (!(fdptr=dopen(filename,&status)))
    return(1);  /* Does not exist */

  if (status == 1)  {
   dclose(fdptr);
   return(2);  /* input cannot be a directory */
  }

  strcpy(to_name,destination);
  dlen = strlen(destination);

  if (dlen)
   if (!(tdptr=dopen(destination,&status)))   /* if destination doesn't exist .. */
    if (destination[dlen - 1] == '/') {
      dclose(fdptr);
      return(3);
    }

   if (status == 1) {  /* destination is directory */
     if ((destination[dlen - 1] != ':') && (destination[dlen-1] != '/'))
      strcat(to_name,"/");
     s = filename + strlen(filename) - 1;
     while ( (*s != '/') && (*s != ':') && (s != filename) ) s--;
     strcat(to_name,s);
   }

  /* Ok, now we have a target file in "to_name" */

  buffer = (char *) AllocMem(buffersize, MEMF_PUBLIC);

  if (!buffer) {
   dclose(fdptr);
   dclose(tdptr);
   return(4);
  }

  if (flen) {
   if ((From = open(filename,O_RDONLY)) == -1) {
    dclose(fdptr);
    dclose(tdptr);
    FreeMem(buffer,buffersize);
    return(5);
   }
  }
  else From = 0; /* copy from standard in */

  if (dlen) {  /* If destination is not stdout */
   if ( (To = open(to_name,O_CREAT | O_WRONLY)) == -1) {
    dclose(fdptr);
    dclose(tdptr);
    FreeMem(buffer,buffersize);
    return(6);
   }
  }
  else To = 1; /* Use standard out */

  /* Whew .... now we're set .... copy the file */

  while ( xfersz = read(From,buffer,buffersize))
      if (write(To,buffer,xfersz) == -1) {
        dclose(fdptr);
        dclose(tdptr);
        FreeMem(buffer,buffersize);
        return(7);
      }

   FreeMem(buffer,buffersize);
   if (flen) close(From);
   if (dlen) close(To);
   dclose(fdptr);
   dclose(tdptr);
   return(0);  /* Successful copy */
}

 int do_copy(const char *s, const char *destination, int xtramem)
 {
  int status;

     if ( status = copy(s,destination,xtramem)) {
        switch (status) {
         case 1: printf("\"%s\" does not exist!\n",s);
                 break;
         case 2: printf("\"%s\" cannot be a directory!\n",s);
                 break;
         case 3: printf("\"%s\" directory does not exist!\n",destination);
                 break;
         case 4: printf("Can't get the %dKB of bufferspace\n",xtramem/1024);
                 break;
         case 5: printf("Open failed for \"%s\"\n",s);
                 break;
         case 6: printf("Open/create failed for \"%s\"\n",destination);
                 break;
         case 7: printf("Error during write\n");
                 break;
        }
      return(0);
     }
    return(1);
 }

/* scdir function for wildcard expansion - based on Ben Eng's implementation */
static struct AnchorPath AnPath;
static char *oldwild = NULL;
static LONG failure = 0;
static char *result = NULL;

void scdir_abort(void) {
    if (result) {
        free(result);
        result = NULL;
    }
    if (oldwild) {
        free(oldwild);
        oldwild = NULL;
        if (!failure && SysBase->lib_Version >= 36L)
            MatchEnd(&AnPath);
        failure = -1;
    }
}

static char *scdir_result(const char *wild, const char *name) {
    long size;

    if (result) {
        free(result);
        result = NULL;
    }
    if (name) {
        size = strlen(wild) + strlen(name) + 1;
        if ((result = (char *)malloc(size))) {
            strcpy(result, wild);
            strcpy(my_basename(result), name);
            return result;
        }
        if (SysBase->lib_Version >= 36L)
            MatchEnd(&AnPath);
    }
    return NULL;
}

char *scdir(const char *wild) {
    int diff = 0;

    if (oldwild) {
        diff = strcmp(oldwild, wild);
        if (!diff && failure) return scdir_result(wild, NULL);
    }
    
    if (SysBase->lib_Version < 36L) {
        BPTR lock;
        lock = Lock(wild, MODE_OLDFILE);
        if (lock) UnLock(lock);
        if (oldwild) free(oldwild);
        oldwild = strdup(wild);
        if (result) free(result);
        result = strdup(wild);
        return result;
    }
    
    if (!oldwild || diff) {
        if (oldwild) free(oldwild);
        oldwild = strdup(wild);

        AnPath.ap_BreakBits = SIGBREAKF_CTRL_C;
        failure = MatchFirst(wild, &AnPath);

        if (!failure && AnPath.ap_Info.fib_DirEntryType <= 0) {
            return scdir_result(wild, AnPath.ap_Info.fib_FileName);
        }
    }

    /* same wildcard as before */
    while (!failure) {
        if ((failure = MatchNext(&AnPath))) break;
        if (AnPath.ap_Info.fib_DirEntryType <= 0) {
            return scdir_result(wild, AnPath.ap_Info.fib_FileName);
        }
    }
    
    /* failure - cleanup */
    MatchEnd(&AnPath);
    return scdir_result(wild, NULL);
}
