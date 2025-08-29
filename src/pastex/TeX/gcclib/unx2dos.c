#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <osbind.h>
#include <device.h>
#include <types.h>
#include <dirent.h>
#include "symdir.h"

#if 1	/* ndef NAME_MAX */
#define NAME_MAX 32
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

char _lOK;			/* symbolic links are OK 		   */
char *_lDIR = ".dir";		/* name of symbolic link directory file    */
char _lAUTO;			/* make automatic symbolic links 	   */
char _lHIDE;			/* hide the _lDIR file from searches 	   */

char _tCASE;			/* translate filenames to lower case	   */
char _tSLASH;			/* '/' seperates directories like '\' does */
char _tDOTS;			/* if != 0, translate '.' into this        */
char _tUNLIMITED;		/* filenames are not restricted to 8.3	   */
char _tDEV;			/* allow /dev/filename for special files   */
char _tROOT;			/* use this drive as default root directory */

/* translations to assume if there is no UNIXMODE environment variable
 * it's assumed that the user will set this via a definition
 *    char *_default_unixmode = whatever
 * if no such definition is present (i.e. _default_unixmode is 0) then
 * "/" is assumed
 */

char *_default_unixmode;
int _unixmode = 1;	/* this will go away someday */

/*
 * _set_unixmode(mode): set Unix emulation modes. Normally "mode" will come
 * from the environment variable UNIXMODE. These settings should only be
 * changed at the explicit request of the user!
 * The characters in "mode" have the following meanings:
 * 	b	Open all files in binary mode (no CR, only LF).
 *	c	Assume case is already significant to the operating system.
 *	d	Allow special file names like /dev/console.
 *	r drv	Filenames starting with '/' are rooted from this drive.
 *	u	Assume the OS allows unlimited length file names already.
 *	/	Allow '/' as a directory seperator.
 *	. char	Translate extra dots in a filename to 'char'.
 *	L	Allow symbolic links.
 *
 * The following characters are meaningful only if 'L' is present.
 *	A	Automatically create symbolic links for files whose names
 *		are changed by the _unx2dos routine (e.g. filenames that
 *		don't match the TOS 8 character + 3 character extension
 *		rule). Such automatic symbolic links are much "tighter"
 *		than normal symbolic links, and are effectively aliases
 *		for the old name.
 *	H	Hide the .dir file from directory searches. Explicit
 *		requests for it (e.g. unlink()) still find it, though.
 */

void _set_unixmode(mode)
	char *mode;
{
	char c;

	if (!mode && !(mode = _default_unixmode)) {
#if 0
/* this is what we will eventually do */
		mode = "/";
#else
/* compatibility with older versions of the library */
		switch(_unixmode) {
		case 0: mode = ""; break;
		case 1: mode = "/"; break;
		case 2: mode = "/.,"; break;
		default: mode = "/.,LAHd"; break;
		}
#endif
	}
	_tSLASH = _tDOTS = _tUNLIMITED = _tDEV = FALSE;
	_tCASE = TRUE;
	_tROOT = 0;
	_lOK = _lHIDE = _lAUTO = FALSE;

	while (c = *mode++) {
		switch(c) {
		case 'b': _binmode(TRUE); break;
		case 'c': _tCASE = FALSE; break;
		case 'd': _tDEV = TRUE; break;
		case 'u': _tUNLIMITED = TRUE; break;
		case '/': _tSLASH = TRUE; break;
		case 'L': _lOK = TRUE; break;
		case 'A': _lAUTO = TRUE; break;
		case 'H': _lHIDE = TRUE; break;
		case '.':
			if (*mode && !isalpha(*mode))
				_tDOTS = *mode++;
			break;
		case 'r':
			if (*mode && isalpha(*mode))
				_tROOT = *mode++;
		default:
			break;
		}
	}
}

/*
 * Pseudo-device interface stuff. New devices can be hooked into the chain
 * that's rooted at __devices. All unx2dos functions use this chain in
 * determining whether files are devices or disk files, as does stat().
 * The major device number of user-supplied devices must *NOT* be 0 or 0xff,
 * both of which are reserved.
 */

static struct _device prn_dev = {
"PRN:", "lp", 0xfffd, NULL, NULL, NULL, NULL, NULL, 0
};

static struct _device aux_dev = {
"AUX:", "tty1", 0xfffe, NULL, NULL, NULL, NULL, NULL, &prn_dev
};

static struct _device con_dev = {
"CON:", "console", 0xffff, NULL, NULL, NULL, NULL, NULL, &aux_dev
};

struct _device *__devices = &con_dev;

/*
 * install a new device
 */

void
_install_device(d)
	struct _device *d;
{
	d->next = __devices;
	__devices = d;
}

/*
 * find a device based on it's file descriptor
 */

struct _device *
_dev_fd(fd)
	int fd;
{
	struct _device *d;

	for (d = __devices; d; d = d->next)
		if (d->dev == fd) break;
	return d;
}

/*
 * find a device based on its DOS or UNIX name.
 */

struct _device *
_dev_dosname(dosnm)
	const char *dosnm;
{
	struct _device *d;

	for (d = __devices; d; d = d->next) {
		if (!strcmp(dosnm, d->dosnm)) break;
	}
	return d;
}

struct _device *
_dev_unxname(unm)
	const char *unm;
{
	struct _device *d;

	for (d = __devices; d; d = d->next) {
		if (!strcmp(unm, d->unxnm)) break;
	}
	return d;
}

/*
 * _uniquefy(name): change name so that it's not the name of any existing
 * file. If the file already exists, then we try changing the file's
 * extension in the following ways:
 * put '$' as the second character
 * put '0'-'9' as the last character (with '$' still in second place).
 * put 'A'-'Z' as the last character (with '$' still in second place).
 * if all of the above fail, put '$$' in the last two characters and
 * give up.
 */

void
_uniquefy(dos)
	char *dos;
{
	char *ext, c;
	struct dirent *_do_stat(char *);

	if (!_do_stat(dos))	/* file not found, so "dos" is unique */
		return;

/* make 'ext' point to the (last two characters of) the extension */

	ext = strrchr(dos, '\\'); if (!ext) ext = dos;
	ext = strrchr(ext, '.');
	if (ext && ext[1]) {
		ext+=2;
		if (!*ext) {
			ext[0] = 0; ext[1] = 0;
		}
	}
	else {
		for(ext=dos; *ext; ext++);
		strcpy(ext, ".$0"); ext++;
	}

	*ext = '$';
	if (!_do_stat(dos))
		return;

	ext[2] = 0;
	for (c = '0'; c <= '9'; c++) {
		ext[1] = c;
		if (!_do_stat(dos))
			return;
	}
	for (c = 'A'; c <= 'Z'; c++) {
		ext[1] = c;
		if (!_do_stat(dos))
			return;
	}
	/* at this point we're sunk; punt and try '$$' in the extension */
	ext[1] = '$';
}

/*
 * adjust(name): adjusts a directory entry to be acceptable to DOS.
 * if _tCASE is on, it is converted to upper case.
 * if _tDOTS is on, '.' is converted to _tDOTS wherever necessary to avoid
 *    conflicts with the DOS 8.3 naming convention.
 * unless _tUNLIMITED is set, only an 8 character base name and 3 character
 *    extension are kept.
 * returns: _NM_CHANGE if the name has been transformed in some irrevocable way
 *    i.e. if dos2unx cannot recover the same name
 *          _NM_OK otherwise
 */

static int
_adjust(name, result)
	char *name;
	char *result;
{
	char tmp[NAME_MAX];
	char *n, *eos, *lastdot = 0;
	int count, change;

#ifdef DEBUG
printf("_adjust(%s)", name); fflush(stdout);
#endif
	strcpy(tmp, name);
	change = 0;

/* first, do case and dot conversion */
	for (n = tmp; *n; n++) {
		if (_tDOTS) {
			if(*n == '.')
				*n = _tDOTS;
			else if (*n == _tDOTS)
				change++;
		}
		if (_tCASE) {
			if (islower(*n))
				*n = toupper(*n);
			else if (isupper(*n))
				change++;
		}
	}

	eos = n;	/* end of the "tmp" string */

/* if dots were converted, change the last one possible back into a '.'
 * e.g. if _tDOTS == '_', file.c.Z -> file_c_Z -> file.c_Z
 */
	if (_tDOTS && !_tUNLIMITED) {
		for (n = tmp; *n; n++) {
			if (*n == _tDOTS) {
				if ( (eos - n) <= 4 ) {
					*n = '.';
					goto dot_was_set;
				}
				else lastdot = n;
			}
		}
/* if we found no good place, and if the name is too long to fit without
 * an extension, we just put the period in the last possible place
 */
		if (strlen(tmp) >= 8 && lastdot)
			*lastdot = '.' ;
	}

dot_was_set:
/*
 * here we enforce the 8 character name + 3 character extension rule
 */
	if (_tUNLIMITED)
		strcpy(result, name);
	else {
		eos = result;
		n = tmp;
		for (count = 0; count < 8; count++) {
			if (!*n || *n == '.') break;
			*eos++ = *n++;
		}
		while (*n && *n != '.') {
			change++; n++;
		}
		if (*n == '.') *eos++ = *n++;
		for (count = 0; count < 3; count++) {
			if (!*n) break;
			*eos++ = *n++;
		}
		*eos++ = 0;
		change += strlen(n);
	}
#ifdef DEBUG
	printf("->(%s)\n", result);
#endif
	return change ? _NM_CHANGE : _NM_OK;
}

/*
 * _canon(base, path, result, level):
 * find a canonical form for the given path, based in the directory "base"
 * (e.g. the current directory); the result is copied into "result".
 * "level" is the level of recursion, and is used to prevent infinite
 * loops in symbolic links.
 *
 * "base" must already be in a canonical form; the return value from the
 * GEMDOS Dgetpath() call is almost suitable, but the drive letter must
 * be prefixed to it. No checking of "base" is done.
 * Note that "base" and "result" may be pointers to the same array!
 *
 * returns:
 *    _NM_LINK	if the last component is a symbolic link
 *    _NM_OK	if the name of the last component is recoverable by _dos2unx
 *    _NM_CHANGE otherwise
 *
 * Also, the global variables __link_path and __link_name are set
 * to the path of the last filename component, and the name of
 * the file, respectively (the canonical form of the file name
 * to is returned in result, and will NOT be __link_path\__link_name if
 * __link_name was a symbolic link). If it was a symbolic link, __link_flags
 * is set to the flags associated with the link (e.g. whether it was
 * auto-created), and __link_to is set to the link contents, which were
 * followed in the course of creating the canonical name.
 *
 * All this is done so that functions like "creat" and "rename" that need
 * access to the path and "real" name of the file (if it was a symbolic link)
 * don't have to duplicate work we've already done.
 */

#define DIRSEP(p) ((p) == '\\' || (_tSLASH && (p) == '/'))
#define MAXRECURSE 12

char __link_path[FILENAME_MAX], __link_name[NAME_MAX], __link_to[NAME_MAX];
int __link_flags = 0;

static int
_canon(base, path, result, level)
	char *base, *path;
	char *result;
	int level;
{
	char tmp[NAME_MAX], name[NAME_MAX];
	SYMDIR *dir;
	SYMENTRY *ent;
	char *n, *t;
	int found, change = _NM_OK, needschange = _NM_OK;

/* FIX_ME: we really ought to flag an error of some sort here */
	if (level > MAXRECURSE) {
		return _NM_OK;
	}

#ifdef DEBUG
	printf("_canon: [%s] + [%s]\n", base, path);
#endif
	if (!*path) {
		if (result != base)
			strcpy(result, base);
		return _NM_OK;
	}

/* check for paths relative to root of current drive 		*/
/* if _tROOT is set, then such paths should start at _tROOT 	*/

	if (DIRSEP(path[0])) {
		if (result != base)
			strcpy(result, base);
		if (_tROOT) {
			if (_tCASE)
				result[0] = toupper(_tROOT);
			else
				result[0] = _tROOT;
		}
		result[2] = 0;	/* the drive is already in the first part */
		path++;
	}
/* check for absolute paths with drive letter given */
	else if (path[1] == ':') {
		result[0] = _tCASE ? toupper(path[0]) : path[0];
		result[1] = ':';
		result[2] = 0;
		path += 2;
		if (DIRSEP(path[0])) {
			path++;
		}
	}
	else if (result != base)
		strcpy(result, base);

/* now path is relative to what's currently in "result" */

	while(*path) {
/* get next name in path */
		n = name;
#if 0
		while (*path && !DIRSEP(*path))
			*n++ = *path++;
		*n++ = 0;
#else /* (br) */
		while (*path && !DIRSEP(*path)) {
			if( n >= &name[NAME_MAX-1] )
				break;
			*n++ = *path++;
		}
		*n = 0;
#endif
		if (*path) path++;
		change = _NM_OK;	/* assume this is a regular name */

/* check for "." and ".." */
		if (!strcmp(name, ".")) continue;
		if (!strcmp(name, "..")) {
			n = strrchr(result, '\\');
			if (n) *n = 0;
			continue;
		}

/* see if "name" is a symbolic link */
		found = 0;
		dir = _read_symdir(result);
#ifdef DEBUG
if (!dir) printf("unx2dos: _read_symdir(%s) failed\n", result);
#endif
		ent = dir ? dir->s_dir : 0;

		while (ent) {
			if (!strcmp(ent->linkname, name)) {
				change = _NM_LINK;
				strcpy(__link_to, ent->linkto);
#ifdef DEBUG
printf("...following link (%s)->(%s)\n", name, ent->linkto);
#endif

				if (level == 0) {
				    strcpy(__link_path, result);
				    __link_flags = ent->flags;
				}
/*
 * note that _free_symdir caches the directories it frees, so it's OK
 * to use ent->linkto afterwards (otherwise we'd have to strdup() it.
 * Also note that we *do* want to free it before recursively calling
 * ourselves; otherwise it won't be in the cache.
 */
				found = 1;
				_free_symdir(dir);
/*
 * We should follow normal symbolic links all the way through, since it's
 * OK to have a link to a link.
 * Auto symbolic links, however, must be linked to a real GEMDOS file.
 */
				if (ent->flags & SD_AUTO) {
					strcat(result, "\\");
					_adjust(ent->linkto, tmp);
					strcat(result, tmp);
				}
				else {
				    _canon(result, ent->linkto, result, level+1);
				}

				break;	/* out of the search */
			}
/*
 * if the name we're searching for is the target of an automatic link,
 * then we should have been using that name instead. to prevent
 * name conflict problems, flag the name as needing to be changed;
 * _unx2dos can then try to sort out what to do.
 */
			else if (_lAUTO && !strcmp(ent->linkto, name)) {
				if (ent->flags & SD_AUTO)
					needschange = _NM_CHANGE;
			}
			ent = ent->next;
		}

/* if found == 1, then "result" has already been appropriately updated */
/* otherwise, append "name" to path, adjusting appropriately */
		if (!found) {
			_free_symdir(dir);
			strcat(result, "\\");
			change = _adjust(name, tmp);
			strcat(result, tmp);
			if (needschange) change = needschange;
		}
	}
	if (level == 0)
		strcpy(__link_name, name);
	return change;
}

/*
 * translate a Unix style filename to a DOS one 
 * returns:
 * _NM_DEV	if the name was of a device
 * _NM_LINK 	if the name is a symbolic link
 * _NM_OK 	if the last component can be recovered via _dos2unx
 * _NM_CHANGE	if the last component is irrevocably changed
 */

int
_unx2dos(unx, dos)
	const char *unx;
	char *dos;
{
	char path[FILENAME_MAX];
	static char _old_unixmode = 1;
	struct _device *d;
	int change;

/* compatibility code: will go away someday soon */
	if (_unixmode != _old_unixmode) {
		_old_unixmode = _unixmode;
		_set_unixmode(NULL);
	}

/* check for a TOS device name */
	if (d = _dev_dosname(unx)) {
		strcpy(dos, unx);
		return _NM_DEV;
	}
/* get current directory */
	path[0] = Dgetdrv() + 'A';
	path[1] = ':';
	Dgetpath(path+2, 0);

	if (_tDEV && !strncmp(unx, "/dev/", 5)) {
	/* check for a unix device name */
		if (d = _dev_unxname(unx+5)) {
			strcpy(dos, d->dosnm);
			return _NM_DEV;
		}
	/* check for a path like /dev/A/somefile */
		else if (isalpha(unx[5]) && ((unx[6] == '/') || (!unx[6]))) {
			path[0] = unx[5];
			path[2] = 0;
			unx = (unx[6]) ? &unx[7] : &unx[6];
		}
	}
	change = _canon(path, unx, dos, 0);
/*
 * If automatic symbolic links are on, then we should try to remap weird
 * filenames into new, unique ones.
 */
	if (change == _NM_CHANGE && _lOK && _lAUTO) {
		_uniquefy(dos);
	}
	return change;
}

/*
 * translate a DOS style filename to Unix. Note that this function does
 * *NOT* look up symbolic links; for that, call _full_dos2unx.
 * The return value may be given a useful purpose someday;
 * for now it's always 0.
 */

int
_dos2unx(dos, unx)
	const char *dos;
	char *unx;
{
	char c;
#ifdef DEBUG
char *oldunx = unx;
printf("dos2unx(%s)->", dos); fflush(stdout);
#endif
	while(c = *dos++) {
		if (_tSLASH && c == '\\')
			*unx++ = '/';
		else if (_tDOTS && c == _tDOTS)
			*unx++ = '.';
		else if (_tCASE)
			*unx++ = tolower(c);
		else
			*unx++ = c;
	}
	*unx++ = 0;
#ifdef DEBUG
printf("(%s)\n", oldunx);
#endif
	return 0;
}

int
_full_dos2unx(dos, unx)
	char *dos, *unx;
{
	SYMDIR *dir;
	SYMENTRY *ent;
	char *n, *curdir, *lastslash, name[NAME_MAX], tmp[NAME_MAX];
	char slash;

	curdir = dos;
	lastslash = 0;
	slash = _tSLASH ? '/' : '\\';
	if (dos[0] && dos[1] == ':') {
		*unx++ = tolower(*dos); dos++;
		*unx++ = *dos++;
	}
	if (*dos == '\\') {
		*unx++ = slash; lastslash = dos; dos++;
	}
	else if (*dos) {	/* something is wrong -- punt */
		return _dos2unx(curdir, unx);
	}
	while (*dos) {
		n = tmp;
		while (*dos && *dos != '\\') {
			*n++ = *dos++;
		}
		*n++ = 0;
		_dos2unx(tmp, name);
		*lastslash = 0;		/* tie off current directory */
/* see if the DOS file has a Unix alias */
		dir = _read_symdir(curdir);
		ent = dir ? dir->s_dir : 0;
		while (ent) {
			if ((ent->flags & SD_AUTO) && 
			    !strcmp(ent->linkto, name)) {
				strcpy(name, ent->linkname);
				break;
			}
			ent = ent->next;
		}
		_free_symdir(dir);
		for (n = name; *n; n++)
			*unx++ = *n;
		if (*dos)
			*unx++ = slash;
		*lastslash = '\\';	/* restore path */
		lastslash = dos;
		if (*dos) dos++;
	}
	*unx++ = 0;
}

/*
 * the stuff below this line is for compatibility with the old unx2dos, and
 * will go away someday
 */

/* system default file name mapping function pointers */
typedef int (*fnmapfunc_t)(const char *, char *);

static fnmapfunc_t ux2dos = _unx2dos;
static fnmapfunc_t dos2ux = _dos2unx;

/* set user specified filename mapping function
 *	NULL => use system default mapping function
 */
void fnmapfunc(u2dos, dos2u)
fnmapfunc_t u2dos, dos2u;
{
	ux2dos = (u2dos == NULL) ? _unx2dos : u2dos;
	dos2ux = (dos2u == NULL) ? _dos2unx : dos2u;
}

/* mapping functions -- call the mapping functions via pointers */

int unx2dos(u, d)
const char *u;
char *d;
{
	return (*ux2dos)(u, d);
}

int dos2unx(d, u)
const char *d;
char *u;
{
	return (*dos2ux)(d, u);
}
