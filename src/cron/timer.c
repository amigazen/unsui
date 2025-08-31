/*
 * timer.c - Timer compatibility layer for cron command
 *
 * This file serves as a compatibility layer since we're using
 * the Amiga.lib TimeDelay API instead of custom timer functions.
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 */

#include "cron.h"

/*
 * Note: This file is now largely obsolete since we're using the
 * Amiga.lib TimeDelay API directly. However, it's kept for
 * potential future use or compatibility with existing code.
 *
 * The main cron.c file now calls:
 *   TimeDelay(UNIT_VBLANK, seconds, 0)
 *
 * This uses the standard Amiga system timer function which is
 * more reliable and efficient than custom timer implementations.
 */

/* Placeholder for any future timer-related functionality */
void timer_init(void)
{
    /* Initialize timer system if needed in the future */
}

void timer_cleanup(void)
{
    /* Cleanup timer system if needed in the future */
}
