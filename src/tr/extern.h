/*
 * SPDX-License-Identifier: BSD-4-Clause-UC
 * See LICENSE.md for full license text.
 */

typedef struct {
	enum { STRING1, STRING2 } which;
	enum { EOS, INFINITE, NORMAL, RANGE, SEQUENCE, SET } state;
	int	 cnt;			/* character count */
	int	 lastch;		/* last character */
	int	equiv[2];		/* equivalence set */
	int	*set;			/* set of characters */
	char	*str;			/* user's string */
} STR;

#include <limits.h>
#define	NCHARS	(UCHAR_MAX + 1)		/* Number of possible characters. */
#define	OOBCH	(UCHAR_MAX + 1)		/* Out of band character value. */

void	 err (const char *fmt, ...);
int	 next (STR *);
char	*strerror(int errnum);
long	 strtol(const char *nptr, char **endptr, int base);
