/*
 * ps.h - Header file for POSIX ps command
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#ifndef PS_H
#define PS_H

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>

#include <exec/types.h>
#include <exec/memory.h>
#include <exec/tasks.h>
#include <exec/interrupts.h>

#include <dos/dosextens.h>
#include <dos/dos.h>

#include <proto/exec.h>
#include <proto/dos.h>

#include "common.h"
#include "getopt.h"

/* Task state constants if not defined in exec/tasks.h */
#ifndef TS_READY
#define TS_READY 0
#endif
#ifndef TS_WAITING
#define TS_WAITING 1
#endif
#ifndef TS_SLEEP
#define TS_SLEEP 2
#endif

/* Process information structure */
struct process_info {
    int tasknum;
    int priority;
    void *address;
    char command[256];
    char directory[256];
    char state[16];
    int pid;
    /* Enhanced information */
    ULONG stack_size;
    ULONG start_time;
    int background;
    int interactive;
    char arguments[256];
    char command_file[256];
    int return_code;
    int fail_level;
    char prompt[64];
    /* Memory information */
    BPTR stack_base;
    BPTR seg_list;
    /* Process flags */
    ULONG flags;
};

/* Command line options */
struct ps_options {
    int all_flag;           /* -a: show all processes */
    int long_flag;          /* -l: long format */
    int full_flag;          /* -f: full format */
    int verbose_flag;       /* -v: verbose output */
    int help_flag;          /* -h: help */
    int version_flag;       /* -V: version */
    char *format_string;    /* -o format string */
};

/* Function declarations */
void init_options(struct ps_options *opts);
void parse_getopt_args(int argc, char **argv, struct ps_options *opts, const char *program);
int run_ps_logic(struct ps_options *opts, const char *program);
void usage(const char *program);
void version(void);
void moveBSTR(BSTR bptr, char *buffer, int maxlen);
void print_process_header(struct ps_options *opts);
void print_process_line(struct process_info *proc, struct ps_options *opts);
void print_long_format(struct process_info *proc);
void print_full_format(struct process_info *proc);
void print_enhanced_format(struct process_info *proc);
int get_process_list(struct process_info *procs, int max_procs);
ULONG get_system_boot_time(void);

#endif /* PS_H */
