/*
 * touch - Unsui POSIX runtime for Amiga
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on touch by Dave Schreiber.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#include <string.h>
#include <stdlib.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <dos/exall.h>
#include <proto/dos.h>
#include <proto/exec.h>

#include "touch.h"
#include "common.h"
#include "getopt.h"

extern struct DosLibrary *DOSBase;

/* Version tag for Amiga */
static const char *verstag = "$VER: touch 2.1 (14/03/26)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

#define PATH_BUFFER_SIZE  1024
#define PATTERN_SIZE      512
#define EXALL_ENTRIES     20

enum {
    ARG_FILE,
    ARG_ACCESS,
    ARG_MODIFY,
    ARG_CREATE,
    ARG_POSIX,
    ARG_COUNT
};

static void errmsg(BPTR out, const char *program, const char *msg);
void usage(const char *program);
int run_touch_logic(int access_flag, int modify_flag, int create_flag, int file_count, char **files, const char *program);
int parse_getopt_args(int argc, char **argv, int *access_flag, int *modify_flag, int *create_flag, int *file_start, const char *program);
void touch_file(char *filename, int access_flag, int modify_flag, int create_flag, BPTR out);

int main(int argc, char **argv)
{
    int access_flag = FALSE;
    int modify_flag = FALSE;
    int create_flag = TRUE;
    int file_start = 1;
    char **files = NULL;
    char *program;
    const char *template = "FILE/M,ACCESS/S,MODIFY/S,CREATE/S,POSIX/K/F";
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
    char *temp_str;
    size_t combined_len;
    int parse_ret;

    for (i = 0; i < ARG_COUNT; i++) {
        arg_array[i] = 0;
    }

    if (argc < 1) {
        return FAILURE;
    }

    program = my_basename(argv[0]);

    if (argc == 1) {
        usage(program);
        return FAILURE;
    }

    if (is_getopt_style(argc, argv)) {
        parse_ret = parse_getopt_args(argc, argv, &access_flag, &modify_flag, &create_flag, &file_start, program);
        if (parse_ret == 1) {
            return SUCCESS;
        }
        if (parse_ret == 2) {
            return FAILURE;
        }
        files = &argv[file_start];
        return run_touch_logic(access_flag, modify_flag, create_flag, argc - file_start, files, program);
    }

    interactive_help = FALSE;
    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "?") == 0) {
            interactive_help = TRUE;
            break;
        }
    }

    rdargs = AllocDosObject(DOS_RDARGS, NULL);
    if (!rdargs) {
        errmsg(Output(), program, "out of memory for RDArgs");
        return FAILURE;
    }

    if (interactive_help) {
        initial_args_str[0] = '\0';
        user_input_buf[0] = '\0';
        temp_str = build_command_string(argc, argv, "?");
        if (temp_str) {
            strncpy(initial_args_str, temp_str, 255);
            initial_args_str[255] = '\0';
            free(temp_str);
        }
        FPuts(Output(), template);
        FPuts(Output(), ": ");
        if (FGets(Input(), user_input_buf, (long)sizeof(user_input_buf))) {
            combined_len = strlen(initial_args_str) + strlen(user_input_buf) + 2;
            cmd_string = (char *)AllocVec((ULONG)combined_len, 0);
            if (cmd_string) {
                strcpy(cmd_string, initial_args_str);
                if (initial_args_str[0] != '\0' && user_input_buf[0] != '\n') {
                    strcat(cmd_string, " ");
                }
                strcat(cmd_string, user_input_buf);
            }
        } else {
            cmd_string = (char *)AllocVec((ULONG)(strlen(initial_args_str) + 2), 0);
            if (cmd_string) {
                strcpy(cmd_string, initial_args_str);
                strcat(cmd_string, "\n");
            }
        }
    } else {
        cmd_string = build_command_string(argc, argv, NULL);
    }

    if (!cmd_string) {
        errmsg(Output(), program, "out of memory for command string");
        FreeDosObject(DOS_RDARGS, rdargs);
        return FAILURE;
    }

    rdargs->RDA_Source.CS_Buffer = cmd_string;
    rdargs->RDA_Source.CS_Length = (long)strlen(cmd_string);
    rdargs->RDA_Source.CS_CurChr = 0;
    rdargs->RDA_Flags |= RDAF_NOPROMPT;

    if (!ReadArgs(template, arg_array, rdargs)) {
        PrintFault(IoErr(), program);
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
        return FAILURE;
    }

    if (arg_array[ARG_POSIX]) {
        posix_str = (char *)arg_array[ARG_POSIX];
        new_argv[0] = program;
        new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;
        parse_ret = parse_getopt_args(new_argc, new_argv, &access_flag, &modify_flag, &create_flag, &file_start, program);
        if (parse_ret == 1) {
            FreeDosObject(DOS_RDARGS, rdargs);
            free(cmd_string);
            return SUCCESS;
        }
        if (parse_ret == 2) {
            FreeDosObject(DOS_RDARGS, rdargs);
            free(cmd_string);
            return FAILURE;
        }
        files = &new_argv[file_start];
        ret_code = run_touch_logic(access_flag, modify_flag, create_flag, new_argc - file_start, files, program);
    } else {
        if (arg_array[ARG_ACCESS]) {
            access_flag = TRUE;
        }
        if (arg_array[ARG_MODIFY]) {
            modify_flag = TRUE;
        }
        if (arg_array[ARG_CREATE]) {
            create_flag = TRUE;
        }
        if (!access_flag && !modify_flag) {
            access_flag = TRUE;
            modify_flag = TRUE;
        }
        if (arg_array[ARG_FILE]) {
            file_count = 0;
            while (((char **)arg_array[ARG_FILE])[file_count] != NULL) {
                file_count++;
            }
            files = (char **)arg_array[ARG_FILE];
            ret_code = run_touch_logic(access_flag, modify_flag, create_flag, file_count, files, program);
        } else {
            errmsg(Output(), program, "FILE argument required");
            ret_code = FAILURE;
        }
    }

    FreeDosObject(DOS_RDARGS, rdargs);
    free(cmd_string);
    return ret_code;
}

static void errmsg(BPTR out, const char *program, const char *msg)
{
    FPuts(out, program);
    FPuts(out, ": ");
    FPuts(out, msg);
    FPuts(out, "\n");
}

int parse_getopt_args(int argc, char **argv, int *access_flag, int *modify_flag, int *create_flag, int *file_start, const char *program)
{
    int c;
    int ret = 0;

    reset_getopt();
    while ((c = getopt(argc, argv, "acmhV")) != -1) {
        switch (c) {
        case 'a':
            *access_flag = TRUE;
            break;
        case 'c':
            *create_flag = FALSE;
            break;
        case 'm':
            *modify_flag = TRUE;
            break;
        case 'h':
        case 'V':
            usage(program);
            return 1;
        case '?':
            return 2;
        default:
            break;
        }
    }
    *file_start = optind;
    if (!*access_flag && !*modify_flag) {
        *access_flag = TRUE;
        *modify_flag = TRUE;
    }
    return ret;
}

int run_touch_logic(int access_flag, int modify_flag, int create_flag, int file_count, char **files, const char *program)
{
    BPTR out;
    struct DateStamp ds;
    char *main_buf = NULL;
    char *fileName = NULL;
    char *pattern = NULL;
    char *touchName = NULL;
    struct ExAllData *eab = NULL;
    struct ExAllControl *eac = NULL;
    int i;
    LONG fileNameType;
    BPTR dirLock;
    LONG goOn;
    BOOL doneOnce;
    struct ExAllData *ead;
    ULONG mySig;
    BOOL breaked = FALSE;
    size_t pathBufSize;

    out = Output();
    if (file_count == 0) {
        errmsg(out, program, "FILE argument required");
        return FAILURE;
    }

    pathBufSize = (PATH_BUFFER_SIZE + PATTERN_SIZE + PATH_BUFFER_SIZE);
    main_buf = (char *)AllocVec((ULONG)pathBufSize, MEMF_CLEAR);
    if (!main_buf) {
        errmsg(out, program, "out of memory for path buffer");
        return FAILURE;
    }
    fileName = main_buf;
    pattern = main_buf + PATH_BUFFER_SIZE;
    touchName = main_buf + PATH_BUFFER_SIZE + PATTERN_SIZE;

    eab = (struct ExAllData *)AllocVec((ULONG)(sizeof(struct ExAllData) * EXALL_ENTRIES), MEMF_CLEAR);
    if (!eab) {
        errmsg(out, program, "out of memory for directory scan");
        FreeVec(main_buf);
        return FAILURE;
    }
    eac = (struct ExAllControl *)AllocDosObject(DOS_EXALLCONTROL, NULL);
    if (!eac) {
        errmsg(out, program, "out of memory for ExAll control");
        FreeVec(eab);
        FreeVec(main_buf);
        return FAILURE;
    }

    DateStamp(&ds);

    for (i = 0; i < file_count && !breaked; i++) {
        mySig = CheckSignal(SIGBREAKF_CTRL_C);
        if (mySig & SIGBREAKF_CTRL_C) {
            breaked = TRUE;
            break;
        }

        strcpy(fileName, files[i]);
        fileNameType = ParsePatternNoCase(FilePart(fileName), pattern, PATTERN_SIZE);

        if (fileNameType == 0) {
            touch_file(files[i], access_flag, modify_flag, create_flag, out);
        } else {
            *(PathPart(fileName)) = '\0';
            dirLock = Lock(fileName, ACCESS_READ);
            if (dirLock) {
                doneOnce = FALSE;
                goOn = TRUE;
                eac->eac_LastKey = 0;
                eac->eac_MatchString = pattern;
                eac->eac_MatchFunc = NULL;

                do {
                    mySig = CheckSignal(SIGBREAKF_CTRL_C);
                    if (mySig & SIGBREAKF_CTRL_C) {
                        breaked = TRUE;
                        goOn = FALSE;
                    } else {
                        goOn = ExAll(dirLock, eab, (LONG)(sizeof(struct ExAllData) * EXALL_ENTRIES), ED_NAME, eac);
                        if (!goOn && IoErr() != ERROR_NO_MORE_ENTRIES) {
                            PrintFault(IoErr(), program);
                        }
                        if (eac->eac_Entries == 0) {
                            if (!doneOnce) {
                                FPuts(out, program);
                                FPuts(out, ": No pattern match for ");
                                FPuts(out, files[i]);
                                FPuts(out, "\n");
                            }
                            goOn = FALSE;
                        } else {
                            doneOnce = TRUE;
                            ead = eab;
                            while (ead && !breaked) {
                                mySig = CheckSignal(SIGBREAKF_CTRL_C);
                                if (mySig & SIGBREAKF_CTRL_C) {
                                    breaked = TRUE;
                                } else {
                                    touchName[0] = '\0';
                                    AddPart(touchName, fileName, PATH_BUFFER_SIZE);
                                    AddPart(touchName, ead->ed_Name, PATH_BUFFER_SIZE);
                                    SetFileDate(touchName, &ds);
                                    ead = ead->ed_Next;
                                }
                            }
                        }
                    }
                } while (goOn && !breaked);

                UnLock(dirLock);
            }
        }
    }

    FreeDosObject(DOS_EXALLCONTROL, eac);
    FreeVec(eab);
    FreeVec(main_buf);

    if (breaked) {
        FPuts(out, program);
        FPuts(out, ": ^C\n");
        return FAILURE;
    }
    return SUCCESS;
}

void touch_file(char *filename, int access_flag, int modify_flag, int create_flag, BPTR out)
{
    BPTR file;
    struct DateStamp currentDate;
    BPTR origDir;
    char temp[514];

    file = Open(filename, MODE_READWRITE);
    if (file != (BPTR)NULL) {
        origDir = CurrentDir(file);
        DateStamp(&currentDate);
        if (access_flag) {
            SetFileDate(filename, &currentDate);
        }
        if (modify_flag) {
            SetFileDate(filename, &currentDate);
        }
        CurrentDir(origDir);
        Close(file);
    } else {
        if (create_flag) {
            if (ParsePatternNoCase(filename, temp, (LONG)sizeof(temp)) == 0) {
                file = Open(filename, MODE_NEWFILE);
                if (file != (BPTR)NULL) {
                    Close(file);
                } else {
                    FPuts(out, "touch: cannot create ");
                    FPuts(out, filename);
                    FPuts(out, "\n");
                }
            } else {
                FPuts(out, "touch: cannot create ");
                FPuts(out, filename);
                FPuts(out, " (pattern)\n");
            }
        } else {
            FPuts(out, "touch: cannot touch ");
            FPuts(out, filename);
            FPuts(out, " (no such file)\n");
        }
    }
}

void usage(const char *program)
{
    BPTR out = Output();

    FPuts(out, "Version: ");
    FPuts(out, &verstag[6]);
    FPuts(out, "\n");
    FPuts(out, "Usage (POSIX): ");
    FPuts(out, program);
    FPuts(out, " [OPTIONS] FILE...\n");
    FPuts(out, "Usage (Amiga): ");
    FPuts(out, program);
    FPuts(out, " FILE/M [ACCESS/S] [MODIFY/S] [CREATE/S]\n");
    FPuts(out, "               ");
    FPuts(out, program);
    FPuts(out, " ? for template\n");
    FPuts(out, "OPTIONS:\n");
    FPuts(out, "  -a          change only the access time\n");
    FPuts(out, "  -c          do not create any files\n");
    FPuts(out, "  -m          change only the modification time\n");
    FPuts(out, "  -h, -V      display this help and version\n");
    FPuts(out, "DESCRIPTION:\n");
    FPuts(out, "  Update the access and modification times of each FILE to the current time.\n");
    FPuts(out, "  A FILE argument that does not exist is created empty. Patterns (e.g. #?.c) touch all matches.\n");
}
