/* vi:ts=4:sw=4
 *
 *
 * VIM - Vi IMproved
 *
 * Code Contributions By:	Bram Moolenaar			mool@oce.nl
 *							Tim Thompson			twitch!tjt
 *							Tony Andrews			onecom!wldrdg!tony 
 *							G. R. (Fred) Walter		watmath!watcgl!grwalter 
 */
/*
 * archie.c -- RISC OS + UnixLib specific code.
 *
 * A lot of this file was written by Juergen Weigert.
 *
 * It was then hacked to pieces by Alun Jones to work on the Acorn
 * Archimedes!
 */

#include "vim.h"
#include "globals.h"
#include "param.h"
#include "proto.h"

#include <fcntl.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/os.h>
#include <signal.h>

#include <termio.h>

static int	Read __ARGS((char *, long));
static int	WaitForChar __ARGS((int));
static int	RealWaitForChar __ARGS((int));
static void fill_inbuf __ARGS((void));

static int do_resize = FALSE;

/* I'm sure this should be defined in UnixLib, but it ain't!
 */
short ospeed;

	void
mch_write(s, len)
	char	*s;
	int		len;
{
	int i;
	for (i=0; i<len; i++)
	{
		os_vdu(s[i]);
	}
}

/*
 * GetChars(): low level input funcion.
 * Get a characters from the keyboard.
 * If time == 0 do not wait for characters.
 * If time == n wait a short time for characters.
 * If time == -1 wait forever for characters.
 */
	int
GetChars(buf, maxlen, time)
	char	*buf;
	int		maxlen;
	int		time;
{
	if (time >= 0)
	{
		if (WaitForChar(time) == 0)		/* no character available */
			return 0;
	}
	else		/* time == -1 */
	{
	/*
	 * If there is no character available within 2 seconds (default)
	 * write the autoscript file to disk
	 */
		if (WaitForChar((int)p_ut) == 0)
			updatescript(0);
	}

	WaitForChar(-1);
	return Read(buf, (long)maxlen);
}

	void
vim_delay()
{
	clock_t	now;

	now = clock();
	while (clock() < now + CLOCKS_PER_SEC/2);
}

/*
 * No job control. Fake it by starting a new shell.
 */
	void
mch_suspend()
{
	outstr("new shell started\n");
	call_shell(NULL, 0, 1);
}

	void
mch_windinit()
{
	Columns = 80;
	Rows = 24;

	flushbuf();

	mch_get_winsize();
}

/*
 * Check_win checks whether we have an interactive window.
 */

	void
check_win(argc, argv)
	int argc;
	char **argv;
{
	if (!isatty(0) || !isatty(1))
    {
		fprintf(stderr, "VIM: no controlling terminal\n");
		exit(2);
    }
}

/*
 * fname_case(): Set the case of the filename, if it already exists.
 *				 This will cause the filename to remain exactly the same.
 */
	void
fname_case(name)
	char *name;
{
}

	void
settitle(str)
	char *str;
{
}

	void
resettitle()
{
}

/*
 * Get name of current directory into buffer 'buf' of length 'len' bytes.
 * Return non-zero for success.
 */
	int 
dirname(buf, len)
	char *buf;
	int len;
{
	extern int		errno;
	extern char		*sys_errlist[];

	if (getcwd(buf,len) == NULL)
	{
	    strcpy(buf, sys_errlist[errno]);
	    return 0;
	}
    return 1;
}

/*
 * get absolute filename into buffer 'buf' of length 'len' bytes
 */
	int 
FullName(fname, buf, len)
	char *fname, *buf;
	int len;
{
	int		l;
	char	olddir[MAXPATHL];
	char	*p;
	int		c;
	int		retval = 1;

	if (fname == NULL)	/* always fail */
		return 0;

	*buf = 0;
	if (*fname != '/')
	{
		/*
		 * If the file name has a path, change to that directory for a moment,
		 * and then do the getwd() (and get back to where we were).
		 * This will get the correct path name with "../" things.
		 */
		if ((p = strrchr(fname, '/')) != NULL)
		{
			if (getcwd(olddir, MAXPATHL) == NULL)
			{
				p = NULL;		/* can't get current dir: don't chdir */
				retval = 0;
			}
			else
			{
				c = *p;
				*p = NUL;
				chdir("\\");		/* Try to maintain PSD */
				if (chdir(fname))
					retval = 0;
				else
					fname = p + 1;
				*p = c;
			}
		}
		if (getcwd(buf, len) == NULL)
		{
			retval = 0;
			*buf = NUL;
		}
		l = strlen(buf);
		if (l && buf[l - 1] != '/')
			strcat(buf, "/");
		if (p)
		{
			chdir("\\");			/* Maintain PSD */
			chdir(olddir);
		}
	}
	strcat(buf, fname);
	return retval;
}

/*
 * get file permissions for 'name'
 */
	long 
getperm(name)
	char *name;
{
	struct stat statb;

	if (stat(name, &statb))
		return -1;
	return statb.st_mode;
}

/*
 * set file permission for 'name' to 'perm'
 */
	int
setperm(name, perm)
	char *name;
	int perm;
{
	return chmod(name, perm);
}

/*
 * check if "name" is a directory
 */
	int 
isdir(name)
	char *name;
{
	struct stat statb;

	if (stat(name, &statb))
		return -1;
	return (statb.st_mode & S_IFMT) == S_IFDIR;
}

	void
mch_windexit(r)
	int r;
{
	settmode(0);
	stoptermcap();
	flushbuf();
	stopscript();					/* remove autoscript file */
	exit(r);
}

	void
mch_settmode(raw)
	int				raw;
{
	static	int old225, old226, old4;
	int		retvals[3];
	static struct termio told;
		   struct termio tnew;

	if (raw)
	{
		/* Make arrow keys act as function keys.
		 */
		os_byte(4, 2, 0, retvals);
		old4 = retvals[1];
		/* Now make function keys return NULL followed by a character.
		 * Remember the old value for resetting.
		 */
		os_byte(225, 0xC0, 0, retvals);
		old225 = retvals[1];
		os_byte(226, 0xD0, 0, retvals);
		old226 = retvals[1];

		ioctl(0, TCGETA, &told);
		tnew = told;
		tnew.c_iflag &= ~(ICRNL | IXON);		/* ICRNL enables typing ^V^M */
												/* IXON enables typing ^S/^Q */
		tnew.c_lflag &= ~(ICANON | ECHO | ISIG | ECHOE);
		tnew.c_cc[VMIN] = 1;			/* return after 1 char */
		tnew.c_cc[VTIME] = 0;			/* don't wait */
		ioctl(0, TCSETA, &tnew);
	}
	else
	{
		os_byte(4, old4, 0, retvals);
		os_byte(225, old225, 0, retvals);
		os_byte(226, old226, 0, retvals);
		ioctl(0, TCSETA, &told);
	}
}

/*
 * Try to get the current window size:
 * 1. with an ioctl(), most accurate method
 * 2. from the environment variables LINES and COLUMNS
 * 3. from the termcap
 * 4. keep using the old values
 */
	int
mch_get_winsize()
{
	int			old_Rows = Rows;
	int			old_Columns = Columns;
	char		*p;

	Columns = 0;
	Rows = 0;

/*
 * 1. try using an ioctl. It is the most accurate method.
 */
	{
		struct winsize	ws;

	    if (ioctl(0, TIOCGWINSZ, &ws) == 0)
	    {
			Columns = ws.ws_col;
			Rows = ws.ws_row;
	    }
	}

/*
 * 2. get size from environment
 */
	if (Columns == 0 || Rows == 0)
	{
	    if ((p = (char *)getenv("LINES")))
			Rows = atoi(p);
	    if ((p = (char *)getenv("COLUMNS")))
			Columns = atoi(p);
	}

/*
 * 3. try reading the termcap
 */
	if (Columns == 0 || Rows == 0)
	{
		extern void getlinecol();

		getlinecol();	/* get "co" and "li" entries from termcap */
	}

/*
 * 4. If everything fails, use the old values
 */
	if (Columns <= 0 || Rows <= 0)
	{
		Columns = old_Columns;
		Rows = old_Rows;
		return 1;
	}
	debug2("mch_get_winsize: %dx%d\n", (int)Columns, (int)Rows);

	Rows_max = Rows;				/* remember physical max height */

	check_winsize();
	script_winsize();

/* if size changed: screenalloc will allocate new screen buffers */
	return (0);
}

	void
mch_set_winsize()
{
	/* should try to set the window size to Rows and Columns */
}

	int 
call_shell(cmd, dummy, cooked)
	char	*cmd;
	int		dummy;
	int		cooked;
{
	int		x;
	char	newcmd[1024];

	flushbuf();

	if (cooked)
		settmode(0); 				/* set to cooked mode */

	if (cmd == NULL)
		x = system(p_sh);
	else
	{
		sprintf(newcmd, "*%s", cmd);
		x = system(newcmd);
	}
	if (x == 127)
	{
		emsg("Cannot execute shell sh");
		outchar('\n');
	}
	else if (x)
	{
		smsg("%d returned", x);
		outchar('\n');
	}

	if (cooked)
		settmode(1); 						/* set to raw mode */
	return x;

}

/*
 * The input characters are buffered to be able to check for a CTRL-C.
 * This should be done with signals, but I don't know how to do that in
 * a portable way for a tty in RAW mode.
 */

#define INBUFLEN 50
static unsigned char		inbuf[INBUFLEN];	/* internal typeahead buffer */
static int					inbufcount = 0;		/* number of chars in inbuf[] */

	static int
Read(buf, maxlen)
	char	*buf;
	long	maxlen;
{
	if (inbufcount == 0)		/* if the buffer is empty, fill it */
		fill_inbuf();
	if (maxlen > inbufcount)
		maxlen = inbufcount;
	memmove(buf, inbuf, maxlen);
	inbufcount -= maxlen;
	if (inbufcount)
		memmove(inbuf, inbuf + maxlen, inbufcount);
	return (int)maxlen;
}

	void
breakcheck()
{
/*
 * check for CTRL-C typed by reading all available characters
 */
	if (RealWaitForChar(0))		/* if characters available */
		fill_inbuf();
}

	static void
fill_inbuf()
{
	int		len;

	if (inbufcount >= INBUFLEN)		/* buffer full */
		return;

	for (len=0; len < INBUFLEN-inbufcount; len++)
	{
		int key;

		key = os_inkey(0);
		if (key==-1)
		{
			break;
		}
		inbuf[inbufcount+len] = key;
	}

	while (len-- > 0)
	{
		/*
		 * if a CTRL-C was typed, remove it from the buffer and set got_int
		 */
		if (inbuf[inbufcount] == 3)
		{
			/* remove everything typed before the CTRL-C */
			memmove(inbuf, inbuf + inbufcount, len + 1);
			inbufcount = 0;
			got_int = TRUE;
		}
		++inbufcount;
	}
}

/* 
 * Wait "ticks" until a character is available from the keyboard or from inbuf[]
 * ticks = -1 will block forever
 */

	static int
WaitForChar(ticks)
	int ticks;
{
	if (inbufcount)		/* something in inbuf[] */
		return 1;
	return RealWaitForChar(ticks);
}

/* 
 * Wait "ticks" until a character is available from the keyboard
 * ticks = -1 will block forever
 */
	static int
RealWaitForChar(ticks)
	int ticks;
{
	int	key;

	if (ticks == -1)
	{
		key = os_get();
	}
	else
	{
		key = os_inkey(ticks/10);
	}
debug3("RWFC(%d) got %d (%c)\n", ticks, key, key);
	
	if (key != -1)
	{
		/* Unfortunately the key has now been taken from the
		 * buffer, so we need to put it in outselves. It's a 
		 * shame, but the other way I can think of involves a
		 * keyboard scan, and this would return for SHIFT, etc.
		 */
		 if (inbufcount < INBUFLEN)
		 {
		 	inbuf[inbufcount++] = key;
		}
	}
	return (key != -1);
}

/*
 * ExpandWildCard() - this code does wild-card pattern matching using the shell
 *
 * Mool: return 0 for success, 1 for error (you may loose some memory) and
 *       put an error message in *file.
 *
 * num_pat is number of input patterns
 * pat is array of pointers to input patterns
 * num_file is pointer to number of matched file names
 * file is pointer to array of pointers to matched file names
 * On Unix we do not check for files only yet
 * list_notfound is ignored
 */

extern char *mktemp __ARGS((char *));
#ifndef SEEK_SET
# define SEEK_SET 0
#endif
#ifndef SEEK_END
# define SEEK_END 2
#endif

	int
ExpandWildCards(num_pat, pat, num_file, file, files_only, list_notfound)
	int 			num_pat;
	char		  **pat;
	int 		   *num_file;
	char		 ***file;
	int				files_only;
	int				list_notfound;
{
	char	tmpname[TMPNAMELEN];
	char	*command;
	int		i;
	int		dir;
	size_t	len;
	FILE	*fd;
	char	*buffer;
	char	*p;

	*num_file = 0;		/* default: no files found */
	*file = (char **)"";

	/*
	 * If there are no wildcards, just copy the names to allocated memory.
	 * Saves a lot of time, because we don't have to run glob.
	 */
	if (!have_wildcard(num_pat, pat))
	{
		*file = (char **)alloc(num_pat * sizeof(char *));
		if (*file == NULL)
		{
			*file = (char **)"";
			return 1;
		}
		for (i = 0; i < num_pat; i++)
			(*file)[i] = strsave(pat[i]);
		*num_file = num_pat;
		return 0;
	}

/*
 * get a name for the temp file
 */
	strcpy(tmpname, TMPNAME2);
	if (*mktemp(tmpname) == NUL)
	{
		emsg(e_notmp);
	    return 1;
	}

	len = TMPNAMELEN + 10;
	for (i = 0; i < num_pat; ++i)		/* count the length of the patterns */
		len += strlen(pat[i]) + 1;
	command = (char *)alloc(len);
	if (command == NULL)
		return 1;
	strcpy(command, "glob >");			/* built the shell command */
	strcat(command, tmpname);
	for (i = 0; i < num_pat; ++i)
	{
		strcat(command, " ");
		strcat(command, pat[i]);
	}
	i = call_shell(command, 0, FALSE);		/* execute it */
	free(command);
	if (i)									/* call_shell failed */
	{
		remove(tmpname);
		sleep(1);			/* give the user a chance to read error messages */
		must_redraw = CLEAR;				/* probably messed up screen */
		return 1;
	}

/*
 * read the names from the file into memory
 */
 	fd = fopen(tmpname, "r");
	if (fd == NULL)
	{
		emsg(e_notopen);
		return 1;
	}

	fseek(fd, 0L, SEEK_END);
	len = ftell(fd);				/* get size of temp file */
	fseek(fd, 0L, SEEK_SET);
	buffer = (char *)alloc(len + 1);
	if (buffer == NULL)
	{
		remove(tmpname);
		fclose(fd);
		return 1;
	}
	i = fread(buffer, 1, len, fd);
	fclose(fd);
	remove(tmpname);
	if (i != len)
	{
		emsg(e_notread);
		free(buffer);
		return 1;
	}

	buffer[len] = NUL;					/* make sure the buffers ends in NUL */
	i = 0;
	for (p = buffer; p < buffer + len; ++p)
		if (*p == NUL)					/* count entry */
			++i;
	if (len)
		++i;							/* count last entry */

	*num_file = i;
	*file = (char **)alloc(sizeof(char *) * i);
	if (*file == NULL)
	{
		free(buffer);
		*file = (char **)"";
		return 1;
	}
	p = buffer;

	for (i = 0; i < *num_file; ++i)
	{
		(*file)[i] = p;
		while (*p && p < buffer + len)		/* skip entry */
			++p;
		++p;								/* skip NUL */
	}
	for (i = 0; i < *num_file; ++i)
	{
		dir = (isdir((*file)[i]) > 0);
		if (dir < 0)			/* if file doesn't exist don't add '.' */
			dir = 0;
		p = alloc((unsigned)(strlen((*file)[i]) + 1 + dir));
		if (p)
		{
			strcpy(p, (*file)[i]);
			if (dir)
				strcat(p, ".");
		}
		(*file)[i] = p;
	}
	free(buffer);
	return 0;
}

	void
FreeWild(num, file)
	int		num;
	char	**file;
{
	if (file == NULL || num == 0)
		return;
	while (num--)
		free(file[num]);
	free(file);
}

	int
has_wildcard(p)
	char *p;
{
	return strpbrk(p, "*#") != NULL;
}

	int
have_wildcard(num, file)
	int		num;
	char	**file;
{
	register int i;

	for (i = 0; i < num; i++)
		if (has_wildcard(file[i]))
			return 1;
	return 0;
}
