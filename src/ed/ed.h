/* ed.h */
#define FATAL	(ERR-1)
struct	line {
	int		l_stat;		/* empty, mark */
	struct line	*l_prev;
	struct line	*l_next;
	char		l_buff[1];
};

typedef struct line	LINE;

#define LINFREE	1	/* entry not in use */
#define LGLOB	2       /* line marked global */
#define LEXCL	4	/* line marked exclude */

#define MAXLINE	256	/* max number of chars per line */
#define MAXPAT	256	/* max number of chars per replacement pattern */
#define MAXFNAME 256	/* max file name size */

extern LINE	line0;
extern int	curln, lastln, line1, line2, nlines;
extern int	nflg;		/* print line number flag */
extern int	lflg;		/* print line in verbose mode */
extern int	pflg;		/* print current line after each command */
extern char	*inptr;			/* tty input buffer */
extern char	linbuf[], *linptr;	/* current line */
extern int	truncflg;	/* truncate long line flag */
extern int	eightbit;	/* save eighth bit */
extern int	nonascii;	/* count of non-ascii chars read */
extern int	nullchar;	/* count of null chars read */
extern int	truncated;	/* count of lines truncated */
extern int	fchanged;	/* file changed */

#define nextln(l)	((l)+1 > lastln ? 0 : (l)+1)
#define prevln(l)	((l)-1 < 0 ? lastln : (l)-1)

extern char	*getfn();
extern LINE	*getptr();
extern char	*gettxt();
extern char	*maksub();
extern TOKEN	*optpat();

extern char	*catsub();

extern char	*strcpy();
extern int	*malloc();
/*********************************************************************/
