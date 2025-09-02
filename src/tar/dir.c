#include <dirent.h>

DIR *
opendir(char *dirname)
{
    struct FileLock *alock;
    struct FileInfoBlock *fib;
    DIR *dp;

    if (alock = Lock(dirname, ACCESS_READ))
    {
	if ((fib = (struct FileInfoBlock *) malloc(sizeof(*fib))) == NULL)
	{
	    UnLock(alock);
	    return(NULL);	/* malloc failed */
	}
	if (Examine(alock, fib) && (fib->fib_DirEntryType > 0))
	{
	    if (dp = (DIR *) malloc(sizeof(*dp)))
	    {
	    	if ((dp->dd_dirent =
		    (struct dirent *) malloc(DIRENTSIZ(MAXNAMELEN))) == NULL)
		{
	    	    UnLock(alock);
		    free(fib);
		    free(dp);
		    return(NULL);	/* malloc failed */
	    	}
		dp->dd_lock = alock;
		dp->dd_fib = fib;
	    	dp->dd_loc = 0;
	    	return(dp);
    	    }
	    else
	    {
	    	UnLock(alock);
		free(fib);
	    	return(NULL);		/* malloc failed */
	    }
	}
	else
	{
	    UnLock(alock);
	    free(fib);
	    return(NULL);		/* not a directory */
	}
    }
    else
	return(NULL);			/* couldn't access file */
}


struct dirent *
readdir(DIR *dirp)
{
    struct dirent *dp;

    if ((dirp == NULL) || (dirp->dd_dirent == NULL))
	return(NULL);			/* malloc failed */
    if (ExNext(dirp->dd_lock, dirp->dd_fib))
    {
	dirp->dd_loc++;
	dp = dirp->dd_dirent;
	dp->d_ino = 0;
	dp->d_reclen = strlen(dirp->dd_fib->fib_FileName);
	strcpy(dp->d_name, dirp->dd_fib->fib_FileName);
        return(dirp->dd_dirent);
    }
    else
	return(NULL);			/* hit end */
}

int
closedir(DIR *dirp)
{
    if ((dirp == NULL) || (dirp->dd_dirent == NULL))
	return(1);
    UnLock(dirp->dd_lock);
    free(dirp->dd_dirent);
    free(dirp->dd_fib);
    free(dirp);
    return(0);
}
