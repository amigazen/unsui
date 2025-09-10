/*
 * stamp.c - Amiga DateStamp to Unix time conversion functions
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#include <libraries/dosextens.h>
#include <time.h>

/* Unix time is a long integer (time_t) of seconds since a base of 1 Jan 1970.
 *
 * Amiga timebase is 1 Jan 1978.  An Amiga DateStamp contains three longwords:
 * (1) Days		Days since timebase.
 * (2) Minute		Minutes since midnight today.
 * (3) Tick		Ticks since the beginning of the minute.
 *
 * A Tick is 1/50th of a second and the "Tick" field is always a multiple of
 * 50, so this field really holds "seconds * 50".
 */

/* Seconds in a day */
#define DSEC (24 * 60 * 60)
/* Seconds in the 8 years between time bases */
#define S70T78 ((4*365 + 1) * 2 * DSEC)

time_t Stamp_to_mtime(struct DateStamp *d)
{
    return (d->ds_Days*DSEC + d->ds_Minute*60 + d->ds_Tick/50 + S70T78);
}

/* Take unix-style seconds since 1/1/70 and convert into Amiga. */
int mtime_to_Stamp(time_t m, struct DateStamp *d)
{
    m -= S70T78;		/* Adjust from 1970 to 1978 */
    d->ds_Days = m / DSEC;	/* Number of days in remaining seconds */
    m %= DSEC;			/* remainder is Seconds since midnight */
    d->ds_Minute = m / 60;	/* Number of minutes since midnight */
    m %= 60;			/* Remainder is Seconds since minute */
    d->ds_Tick = m * 50;	/* Number of ticks since minute */
    return 0;
}
