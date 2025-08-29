/* stat.c   for TeX 3.1 [ST] */
/* only using `_do_stat()' (br) */

#ifndef atarist
#error Please use this file only for GCC Lib (PatchLevel 62) on Atari ST !!!
#endif


/*
 * stat, fstat, lstat emulation for TOS
 *
 * written by Eric R. Smith and Jwahar Bammi, based on the original
 * from the old GCC library (which original was copyright 1988
 * Memorial University).
 */

#include	<assert.h>
#include	<stdio.h>
#include	<types.h>
#include	<stat.h>
#include	<ctype.h>
#include	<errno.h>
#include	<osbind.h>
#include	<unistd.h>
#include	<support.h>
#include	<string.h>
#include	<dirent.h>
#include	<device.h>
#include	"symdir.h"
#include	<time.h>
#include	"lib.h"

ino_t	__inode = 32;		/* used in readdir also */
int	_x_Bit_set_in_stat = 0;	/* default 0, need change in emacs:sysdep.c */

DIR *__opendir_chain = 0;	/* chain of open directories from opendir */

/* date for files (like root directories) that don't have one */
#define OLDDATE unixtime(0,0)

/*
 * check file cache for the file whose canonical (unx2dos) name is fullname.
 * this is only valid for TOS files, i.e. files that are NOT symbolic
 * links. If file is not found in cache, use Fsfirst to look it up.
 * All open directories (via opendir) are cached (we don't lose much by
 * doing this, and it's a good heuristic for which files are going to
 * need stat() in the near future; plus, opendir did Fsfirst()/Fsnext()
 * already. Programs like "ls" should win big time.
 *
 * BUG/FEATURE: if the _lHIDE flag is set, opendir() never records the
 * existence of .dir, and so stat() in an open directory won't find it.
 */

struct dirent *
_do_stat(fullname)
char *fullname;
{
	char path[FILENAME_MAX], name[NAME_MAX], *tmp;
	DIR *dir;
	struct dirent *d;
	static struct dirent dtmp;
	static struct _dta	dtabuf;
	struct _dta *olddta;
	int r;

	strcpy(path, fullname);
	if((tmp = strrchr(path, '\\'))) {
		*tmp++ = 0;
		_dos2unx(tmp, name);
	}
	else {
		_dos2unx(path, name);
		path[0] = 0;
	}

/*
 * search through the chain of open directories, 1 at a time
 */
	for (dir = __opendir_chain; dir; dir = dir->D_nxtdir) {
		if (!strcmp(path, dir->D_path)) {
			for (d = dir->D_list; d; d = d->d_next) {
				if (!strcmp(name,d->d_name) &&
				    d->d_attribute != 0xff) {
					return d;
				}
			}
		}
	}
	d = &dtmp;
	olddta = (struct _dta *)Fgetdta();
	Fsetdta(&dtabuf);
	r = Fsfirst(fullname, FA_SYSTEM|FA_HIDDEN|FA_DIR);
	Fsetdta(olddta);
	if (r < 0) {
		errno = -r;
		return NULL;
	}
	d->d_ino = __inode++;
	d->d_time = dtabuf.dta_time;
	d->d_date = dtabuf.dta_date;
	d->d_attribute = dtabuf.dta_attribute;
	d->d_size = dtabuf.dta_size;

	return d;
}

#if 0
int
stat(_path, st)
	const char	*_path;
	struct stat	*st;
{
	int	rval, nval;
	char	path[FILENAME_MAX];
	char	*ext, drv;
	int	fd;
	short	magic;
	struct dirent *d;
	struct _device *device;

	if (!_path) {
		errno = EFAULT;
		return -1;
	}

/*
 * NOTE: the new unx2dos gives a complete, canonical TOS pathname
 * (i.e. stripped of . and .., and with a drive letter). It also returns
 * some useful information about what the given filename was (e.g. symlink,
 * or device)
 */
	nval = _unx2dos(_path, path);
	if (nval == _NM_DEV) {
		device = _dev_dosname(path);
		st->st_mode = S_IFCHR | 0600;
		st->st_attr = 0xfe;
		st->st_ino = st->st_rdev = device->dev;
		st->st_mtime = st->st_ctime = st->st_atime = 
			time((time_t *)0) - 2;
		st->st_dev = 0;
	/* if _tDEV is on, /dev/console & CON: are the same file */
		st->st_nlink = _tDEV ? 2 : 1;
		st->st_uid = geteuid();
		st->st_gid = getegid();
		st->st_blksize = 1024;
		return 0;
	}

	/* Deal with the root directory of a logical drive */
	if (path[0] == '\\' && path[1] == 0) {
		drv = Dgetdrv() + 'A';
		goto rootdir;
	}

	if ( (drv = path[0]) && path[1] == ':' &&
	     (path[2] == 0 || (path[2] == '\\' && path[3] == 0)) ) {
rootdir:
		st->st_mode = S_IFDIR | 0755;
		st->st_attr = FA_DIR;
		st->st_ino = isupper(drv) ? drv - 'A' : drv - 'a';
		st->st_mtime = st->st_ctime = st->st_atime = OLDDATE;
		goto fill_dir;
	}

	/* forbid wildcards in path names */
	if (index(path, '*') || index(path, '?')) {
		errno = ENOENT;
		return -1;
	}

	if (!(d = _do_stat(path))) {
	/* errno was set by _do_stat */
		if (errno == ENOENT && nval == _NM_LINK) {
		/* here we have a symbolic link to a deleted file */
			st->st_mode = S_IFLNK | 0644;
			st->st_ino = ++__inode;
			st->st_attr = 0xff;
			st->st_mtime = st->st_ctime = st->st_atime = OLDDATE;
			st->st_size = strlen(__link_to);
			st->st_blocks = 1;
			st->st_nlink = 1;
			goto fill_rest;
		}
		return -1;
	}
	st->st_mtime = st->st_ctime = st->st_atime =
		unixtime(d->d_time, d->d_date);
	st->st_ino = d->d_ino;
	st->st_attr = d->d_attribute;
	st->st_mode = 0644 | (d->d_attribute & FA_DIR ?
			      S_IFDIR | 0111 : S_IFREG);
	if (d->d_attribute & FA_RDONLY)
		st->st_mode &= ~0222;	/* no write permission */
	if (d->d_attribute & FA_HIDDEN)
		st->st_mode &= ~0444;	/* no read permission */

/* check for a file with an executable extension */
	ext = strrchr(_path, '.');
	if (ext) {
		if (!strcmp(ext, ".ttp") || !strcmp(ext, ".prg") ||
		    !strcmp(ext, ".tos") || !strcmp(ext, ".g") ||
		    !strcmp(ext, ".sh")	 || !strcmp(ext, ".bat")) {
			st->st_mode |= 0111;
		}
	}
	if ( (st->st_mode & S_IFMT) == S_IFREG) {
		if (_x_Bit_set_in_stat) {
			if ((fd = Fopen(path,0)) < 0) {
				errno = -rval;
				return -1;
			}
			(void)Fread(fd,2,(char *)&magic);
			(void)Fclose(fd);
			if (magic == 0x601A) st->st_mode |= 0111;
		}
		st->st_size = d->d_size;
		st->st_blocks = (d->d_size + 1023) / 1024;
		st->st_nlink = 1; /* we dont have hard links */
	} else {
fill_dir:
		st->st_size = 1024;
		st->st_blocks = 1;
		st->st_nlink = 2;	/* "foo" && "foo/.." */
	}

fill_rest:
	if ((drv = *path) && path[1] == ':')
		st->st_dev = islower(drv) ? drv - 'a' : drv - 'A';
	else
		st->st_dev = Dgetdrv();
	st->st_rdev = 0;
	st->st_uid = geteuid();	/* the current user owns every file */
	st->st_gid = getegid();
	st->st_blksize = 1024;
	return 0;
}

#include <fcntl.h>

int fstat(fd, st)
int fd;
struct stat *st;
{
    int old;
    int r;

    fd = __OPEN_INDEX(fd);
    if((fd >= 0) && (fd < __NHANDLES))
    {
/* we should turn off links, because the name we're going to give
 * to 'stat' has already been unx2dos'd
 */
	old = _lOK;
	_lOK = 0;
	r = stat(__open_stat[fd].filename, st);
	_lOK = old;
	return r;
    }
    errno = EBADF;
    return -1;
}

/*
 * "stat" that doesn't follow symbolic links
 * important exception: if _lAUTO is set and the link is an automatic
 * link, follow it anyways. This allows automatic links to be aliases
 * for real files (stat will correctly return the info about the
 * automatic link if the real file has been deleted).
 */

int
lstat(_path, st)
	const char *_path;
	struct stat *st;
{
	char path[FILENAME_MAX];
	int n, r, linksiz;
	SYMDIR *dir;
	SYMENTRY *ent;

/* _unx2dos returns _NM_LINK if the last path component was a symbolic link
 * in this case, __link_name and __link_path are set to the name and path
 * of the symbolic link file, and __link_to to its contents
 */
	n = _unx2dos(_path, path);
	linksiz = strlen(__link_to);

	if (n == _NM_LINK && _lAUTO && (dir = _read_symdir(__link_path))) {
		ent = _symdir_lookup(dir, __link_name);
		assert(ent != 0);
		if (ent->flags & SD_AUTO)
			n = _NM_OK;	/* pretend that it wasn't a link*/
		_free_symdir(dir);
	}

	r = stat(_path, st);

	if (n != _NM_LINK || r < 0)		/* not a symlink */
		return r;
	st->st_size = linksiz;
	st->st_blocks = (st->st_size + 1023)/1024;
	st->st_mode = ((st->st_mode & ~S_IFMT) | S_IFLNK);
	return 0;
}
#endif
