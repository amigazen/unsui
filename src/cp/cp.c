/*
       ACP - "A Copy Program".  Written by Fred Cassirer.
       This program placed in the Public Domain, Sept 28, 1986.

       This program will copy any number of input files to a given
       destination.

       Usage is: acp [-i][-msize][-tdestination] file1 file2 ..[destination]

       Available options are:

         -i  -  Prompt for (yes/no).
         -t  -  Use argument as destination rather then last argument.
         -m  -  Use decimal argument for buffer size (1K default).

      The program uses unbuffered I/O to do the copy.  The bigger the
      buffer the fewer the reads/writes. (there must be a tradeoff here
      somewhere)

     The program was written using the Manx C compiler, it probably won't
     work with lattice, since I use the Manx "scdir()" routine to look
     up wildcards.  This could be replaced by a call to expand() from
     Matt Dillon's shell, but it would require a little bit more work.
     As a result, wild card lookup's can be done UNIX style with or
     without Matt's shell (ie under CLI).

     I find this program much easier to use then the standard COPY because
     of the UNIX like wildcards, and that it excepts multiple arguments
     on the command line a little more easily then COPY (at least I think
     so).

     Thanx to Matt Dillon for his shell, and his directory routines used
     here.

     Any problems >> /dev/null:  ... it works for me.

     To build acp:

     cc +L acp.c
     ln acp.c -lc32
*/


#include <stdio.h>

struct DPTR {                    /* Format of directory fetch pointer */
   struct FileLock *lock;        /* lock on directory   */
   struct FileInfoBlock *fib;    /* mod'd fib for entry */
};

#define YEPPER 1
#define NOPE 0

 main(argc,argv)
 int argc;
 char *argv[];
 {
  int xtramem,i,flags,got_one,interactive;
  int lastarg;
  char *s,*destination,reply[5];

  xtramem = 2*1024; /* Buffer size ... default 2KB */
  interactive = NOPE; /* Non interactive */
  destination = argv[argc-1]; /* last arg is target unless -t is used */
  flags = 0; i = 1; lastarg = argc - 1; /* Assume target is last arg */
  while ( *(s = argv[i]) == '-') {
    s++;i++;
    switch (*s++) {
    case 'i': interactive = YEPPER;
              flags++;
              break;
    case 'm':
               if (!(sscanf(s,"%d",&xtramem))) {
               printf("Try a numeric buffer size ...\n");
               exit(1);
              }
              flags++;
              xtramem *= 1024; /* Make it KB */
              break;
    case 't': if (!(*(destination = s) == '-')) flags++;
              lastarg = argc; /* all args are input files */
              break;
    default:  s--;
              printf("Unknown option \"%s\"\n",s);
              break;
   }
  }
  if ( (argc-flags-1) == 0) {
    printf("Usage is %s [-i][-m size][-t destination] file1 file2 [destination]\n",
     argv[0]);
    exit(1);
  }

  got_one = YEPPER;

  while ((i < (lastarg)) && got_one) {
   got_one = NOPE;
   while (s = scdir(argv[i])) { /* I could have used Matt's expand() and
                                     dnext() but I didn't */
     got_one = YEPPER;
     reply[0] = 'Y';
     if (interactive) {
      reply[0] = ' ';
      while ((reply[0] != 'N') && (reply[0] != 'Y')) {
       printf("Copy \"%s\" to %s? ",s,destination);
        scanf("%4s",reply);
       reply[0] = toupper(reply[0]);
       }
      }
     if (reply[0] == 'Y')
      if (!do_copy(s,destination,xtramem)) exit(2);
    }
   i++;
  }
}

/*
 * dopen(), dclose() taken from  Matt Dillon's shell
 *
 * Matthew Dillon, 28 Apr 1986
 *
 */


#include <exec/types.h>
#include <exec/exec.h>
#include <libraries/dos.h>
#include <libraries/dosextens.h>

#define HM_STR 0
#define HM_REL 1
#define HM_ABS 2

extern struct FileLock *Lock();

/* Disk directory routines
 *
 * dptr = dopen(name, stat)
 *    struct DPTR *dptr;
 *    char *name;
 *    int *stat;
 *
 * dnext(dptr, name, stat)
 *    struct DPTR *dptr;
 *    char **name;
 *    int  *stat;
 *
 * dclose(dptr)                  -may be called with NULL without harm
 *
 * dopen() returns a struct DPTR, or NULL if the given file does not
 * exist.  stat will be set to 1 if the file is a directory.  If the
 * name is "", then the current directory is openned.
 *
 * dnext() returns 1 until there are no more entries.  The **name and
 * *stat are set.  *stat = 1 if the file is a directory.
 *
 * dclose() closes a directory channel.
 *
 */

struct DPTR *
dopen(name, stat)
char *name;
int *stat;
{
   struct DPTR *dp;
   int namelen, endslash = 0;

   namelen = strlen(name);
   if (namelen && name[namelen - 1] == '/') {
      name[namelen - 1] = '\0';
      endslash = 1;
   }
   *stat = 0;
   if (*name == '\0')
     return(NULL);
   else {
    dp = (struct DPTR *)malloc(sizeof(struct DPTR));
    dp->lock = Lock (name, ACCESS_READ);
   }
  if (endslash)
      name[namelen - 1] = '/';
   if (dp->lock == NULL) {
      free (dp);
      return (NULL);
   }
   dp->fib = (struct FileInfoBlock *)
         AllocMem(sizeof(struct FileInfoBlock), MEMF_PUBLIC);
   if (!Examine (dp->lock, dp->fib)) {
      dclose (dp);
      return (NULL);
   }
   if (dp->fib->fib_DirEntryType >= 0)
      *stat = 1; 
   return (dp);
}

int dclose(dp)
struct DPTR *dp;
{
   if (dp == NULL)
      return (1);
   if (dp->fib)
      FreeMem (dp->fib, sizeof(*dp->fib));
   if (dp->lock)
      UnLock (dp->lock);
   free (dp);
   return (1);
}

#include <fcntl.h>


/*
 *   Written by Fred Cassirer, September 2, 1986.  Placed in Public Domain.
 *   This program can be used in any way shape or form as long as:
 *     1) I still get some credit for writing it.
 *     2) It's not used for profit.
 *     3) I'm not held responsible for any bugs, or problems with it's use.
 *
 *      (That should about cover it)
 *
 *   copy() - Copy a file to a destination using unbuffered I/O with
 *            a caller specified buffer size.
 *
 *            If "filename" is the null string, use stdout (not used here).
 *            Ditto for "destination".
 *
 *            If "destination" is a directory, the file is copied under
 *            the same name to the directory, else if the "destination"
 *            is a file, it is (re)created and the "filename" is copied to
 *            it.
 *
 */

 int copy(filename,destination,buffersize)
 char *filename,*destination;
 int buffersize;
 {
  int From,To;
  char to_name[80],*s,*buffer;
  struct DPTR *fdptr,*tdptr;
  int status,flen,dlen,xfersz;

  fdptr = tdptr = NULL;
  flen = strlen(filename);
  status = 0;
  if (flen)
   if (!(fdptr=dopen(filename,&status)))
    return(1);  /* Does not exist */

  if (status == 1)  {
   dclose(fdptr);
   return(2);  /* input cannot be a directory */
  }

  strcpy(to_name,destination);
  dlen = strlen(destination);

  if (dlen)
   if (!(tdptr=dopen(destination,&status)))   /* if destination doesn't exist .. */
    if (destination[dlen - 1] == '/') {
      dclose(fdptr);
      return(3);
    }

   if (status == 1) {  /* destination is directory */
     if ((destination[dlen - 1] != ':') && (destination[dlen-1] != '/'))
      strcat(to_name,"/");
     s = filename + strlen(filename) - 1;
     while ( (*s != '/') && (*s != ':') && (s != filename) ) s--;
     strcat(to_name,s);
   }

  /* Ok, now we have a target file in "to_name" */

  buffer = (char *) AllocMem(buffersize, MEMF_PUBLIC);

  if (!buffer) {
   dclose(fdptr);
   dclose(tdptr);
   return(4);
  }

  if (flen) {
   if ((From = open(filename,O_RDONLY)) == -1) {
    dclose(fdptr);
    dclose(tdptr);
    FreeMem(buffer,buffersize);
    return(5);
   }
  }
  else From = 0; /* copy from standard in */

  if (dlen) {  /* If destination is not stdout */
   if ( (To = open(to_name,O_CREAT | O_WRONLY)) == -1) {
    dclose(fdptr);
    dclose(tdptr);
    FreeMem(buffer,buffersize);
    return(6);
   }
  }
  else To = 1; /* Use standard out */

  /* Whew .... now we're set .... copy the file */

  while ( xfersz = read(From,buffer,buffersize))
      if (write(To,buffer,xfersz) == -1) {
        dclose(fdptr);
        dclose(tdptr);
        FreeMem(buffer,buffersize);
        return(7);
      }

   FreeMem(buffer,buffersize);
   if (flen) close(From);
   if (dlen) close(To);
   dclose(fdptr);
   dclose(tdptr);
   return(0);  /* Successful copy */
}

 int do_copy(s,destination,xtramem)
 char *s,*destination;
 int xtramem;
 {
  int status;

     if ( status = copy(s,destination,xtramem)) {
        switch (status) {
         case 1: printf("\"%s\" does not exist!\n",s);
                 break;
         case 2: printf("\"%s\" cannot be a directory!\n",s);
                 break;
         case 3: printf("\"%s\" directory does not exist!\n",destination);
                 break;
         case 4: printf("Can't get the %dKB of bufferspace\n",xtramem/1024);
                 break;
         case 5: printf("Open failed for \"%s\"\n",s);
                 break;
         case 6: printf("Open/create failed for \"%s\"\n",destination);
                 break;
         case 7: printf("Error during write\n");
                 break;
        }
      return(0);
     }
    return(1);
 }
