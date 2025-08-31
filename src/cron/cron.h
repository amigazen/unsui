/*
 * cron.h - Consolidated header for UNSUI POSIX cron command
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 */

#ifndef CRON_H
#define CRON_H

/* Standard C headers */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Amiga system headers */
#include <proto/dos.h>
#include <proto/exec.h>
#include <proto/timer.h>
#include <clib/alib_protos.h>
#include <dos/dos.h>
#include <exec/types.h>
#include <exec/nodes.h>
#include <exec/lists.h>
#include <exec/memory.h>
#include <exec/interrupts.h>
#include <exec/ports.h>
#include <exec/libraries.h>
#include <exec/io.h>
#include <exec/tasks.h>
#include <exec/execbase.h>
#include <exec/devices.h>
#include <devices/timer.h>

/* Constants */
#define SUCCESS 0
#define FAILURE 1

/* Configuration */
#define CRONTAB "S:CronTab"	/* Default crontab path */
#define MAXLINE 132
#define SIZE 64

/* Function declarations */
/* Using Amiga.lib TimeDelay API instead of custom timer functions */

#endif /* CRON_H */
