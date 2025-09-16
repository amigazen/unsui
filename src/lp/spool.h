/*
 * spool.h - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#ifndef SPOOL_H
#define SPOOL_H

/* Spool system message port names */
#define SPOOL_ME "SPOOL system"
#define GIMME_A_FILE "SPOOL output"
#define LOGIN_PRINTER "SPOOL login"

/* Log status codes */
#define LOG_IN '+'
#define LOG_OUT '-'
#define LOGGED 'X'

/* The message structure - same for both message ports */
typedef struct {
    struct Message minfo;
    char log_status;
    char filename[128];
} SPOOLmsg;

#define SHUTDOWN "-shutdown"

/* Spool system priorities */
#define SPOOLER_PRIORITY 4
#define PRTSPOOL_PRIORITY (SPOOLER_PRIORITY-1)

#endif /* SPOOL_H */
