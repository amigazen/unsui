/**** flmt.h		(c) Georg Hessmann 11.02.90 		****/


#define LNKMAGIC	(((long)'F'<<24) | ((long)'L'<<16) | ((long)'N'<<8) | (long)'K')
#define DOSNAMESIZE	100
#define MAXLINKLEVELS	20

/************ structure definitions ***************/
/* fontlib directory structures */
struct flib_dirent { char mname[FILENAMELEN];		/* old version */
                     long size;		/* size of pk-module in bytes */
                     long where;	/* position in flib-file */
                   };

struct new_flib_dirent { char  mname[NEWFILENAMELEN];	/* new version */
			 unsigned short checksum;
			 long  size;		/* size of pk-module in bytes */
			 long  where;		/* position in flib-file */
                       };

union direntry { struct flib_dirent	old;
		 struct new_flib_dirent	new;
	       };


/* internal representation of the flib-directory */
struct dirlist { struct new_flib_dirent dirent;
                 struct dirlist *next;
	       };

struct dir { long	    total;
	     long	    alloc;
	     int	    version;	/* old or new flib */
             struct dirlist *dirlist;
	   };


 /* open a flib and test the magic-number			*/
FILE *open_flib			Args((char *name,
				      char *mode,
				      int  *version,
				      short levels));
 /* read the directory from the flib (file-pointer must be on the first dir)       */
 /* only this directory entries a correct where "where != 0" the others are unused */
int   read_dir			Args((FILE *f,
				      struct dir *directory));
 /* find a specified module in the dirlist and return a pointer to it */
struct dirlist *findmod		Args((struct dir *libdirptr,
				      char *mname));


#ifdef AZTEC_C
/* #pragma regcall ( a0 = open_flib(a0,a1,a2) ) */
#pragma regcall ( /* d0 = */ read_dir(a0,a1) )
/* #pragma regcall ( a0 = findmod(a0,a1) ) */
#endif

