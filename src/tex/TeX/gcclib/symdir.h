/*
 * header file for functions dealing with symbolic link directories and
 * other kinds of filename translations.
 */

#ifndef _SYMDIR_H
#define _SYMDIR_H

/* structure for special symbolic link directory entry */

typedef struct _SENTRY {
	struct _SENTRY	*next;
	unsigned short	flags;		/* see below */
	char		*linkto;
	char		*cflags;	/* pointer to flags character string */
	char		linkname[0];
} SYMENTRY;

#define SD_AUTO		0x0001		/* link was created automatically */

typedef struct _SDIR {
	struct _SDIR *s_nxt;
	SYMENTRY    *s_dir;
	char	    *s_pth;
} SYMDIR;

/*
 * various flags to control file name translation
 */

extern char 	*_lDIR;		/* name of symlink directories */
extern char 	_lOK,		/* OK to use symlinks */
		_lAUTO,		/* make symbolic links for unwieldy filenames */
		_lHIDE;		/* hide the symlink directory */

extern char	_tSLASH,	/* allow '/' as a directory seperator */
		_tCASE,		/* don't translate upper <-> lower case */
		_tDOTS,		/* translate '.' into this character, if set */
		_tUNLIMITED,	/* OS doesn't mind long file names */
		_tROOT,		/* root filesystem, for paths starting with / */
		_tDEV;		/* allow /dev/specialfile names */

/*
 * return codes from _unx2dos
 */
#define _NM_OK		0	/* file name can be recovered by _dos2unx */
#define _NM_CHANGE	1	/* file name changed, _dos2unx won't work */
#define _NM_LINK	2	/* file name was a symbolic link	  */
#define _NM_DEV		3	/* file name was a device (e.g. CON:)	  */

extern char __link_name[], __link_path[], __link_to[];
extern int __link_flags;

#if defined(__STDC__) && !defined(__NO_PROTO__)

int 	_make_autolink(char *, char *);
SYMDIR 	*_read_symdir(char *);
SYMENTRY *_symdir_lookup(SYMDIR *, const char *);
int 	_write_symdir(char *, SYMDIR *);

void 	_set_unixmode(char *mode);
int	_unx2dos(const char *, char *);
int	_dos2unx(const char *, char *);

#else

extern SYMDIR *_read_symdir();
extern SYMENTRY *_symdir_lookup();

#endif

#endif /* _SYMDIR_H */
