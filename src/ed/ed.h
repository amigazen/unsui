/* ed.h */
#ifndef ED_H
#define ED_H

#include <stdio.h>

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

/* Function prototypes */
char	*getfn(void);
LINE	*getptr(int line);
char	*gettxt(int line);
TOKEN	*optpat(TOKEN *pat);

char	*catsub(char *from, char *to, char *sub, char *new, char *newend);

/* Forward declarations for functions used in ed.c */
int	append(int line, int glob);
int	ckglob(void);
int	doglob(void);
int	docmd(int glob);
int	doprnt(int from, int to);
int	doread(int lin, char *fname);
int	dowrite(int from, int to, char *fname, int apflg);
int	getlst(void);
int	ins(char *str);
int	del(int from, int to);
int	set(void);
int	show(char *arg);
int	getrhs(char *sub);
char	*maksub(char *sub, int subsz);
int	subst(TOKEN *pat, char *sub, int gflg, int pflag);
int	egets(char *str, int size, FILE *stream);
int	find(TOKEN *pat, int dir);
int	move(int num);
int	getnum(void);
int	getone(void);
int	deflt(int def1, int def2);
int	prntln(char *str, int vflg, int lin);
int	putcntl(char c, FILE *stream);
int	ed_setbuf(void);
void	relink(LINE *a, LINE *x, LINE *y, LINE *b);
void	clrbuf(void);

#endif /* ED_H */
