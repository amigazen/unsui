@x
#ifdef AMIGA
#include "search_protos.h"
@y
#include "search_protos.h"
@z

@x
#endif
#ifdef OS2
#include <stdlib.h>
FILE *fat_fopen();
#endif

#if defined(SYSV) || defined(VMS) || defined(__THINK__) || defined(MSDOS) || defined(OS2) || defined(ATARIST) || defined(AMIGA)
@y
@z

@x
#else
#include <sys/param.h>          /* for MAXPATHLEN */
#endif
#ifndef AMIGA
#if !defined(MSDOS) && !defined(OS2)
#ifndef VMS
#ifndef MVSXA
#ifndef VMCMS /* IBM: VM/CMS */
#ifndef __THINK__
#ifndef ATARIST
#include <pwd.h>
#endif
#endif
#endif
#endif
#endif  /* IBM: VM/CMS */
#endif
#endif
@y
@z

@x
search(path, file, mode)
        char *path, *file, *mode ;
{
#ifndef AMIGA
   extern char *getenv(), *newstring() ;
#endif
   register char *nam ;                 /* index into fname */
   register FILE *fd ;                  /* file desc of file */
   char fname[MAXPATHLEN] ;             /* to store file name */
   static char *home = 0 ;              /* home is where the heart is */
#ifdef MVSXA
char fname_safe[256];
register int i, firstext, lastext, lastchar;
#endif
#ifdef VMCMS /* IBM: VM/CMS - we don't have paths or dirsep's but we strip off
                             filename if there is a Unix path dirsep    */
   register char *lastdirsep ;
   lastdirsep = strrchr(file, '/') ;
   if ( NULL != lastdirsep ) file = lastdirsep + 1 ;
   if ((fd=fopen(file,mode)) != NULL) {
      return(fd) ;
   } else {
      return(NULL) ;
   }
#else
#ifdef AMIGA
   if (*file == DIRSEP || strchr(file,VOLSEP)) { /* if full path name */
#else
   if (*file == DIRSEP) {               /* if full path name */
#endif /* AMIGA */
      if ((fd=fopen(file,mode)) != NULL) {
         strcpy(realnameoffile, file) ;
         return(fd) ;
      } else
         return(NULL) ;
   }
#endif   /* IBM: VM/CMS */

#if defined MSDOS || defined OS2 || defined(ATARIST)
   if ( isalpha(file[0]) && file[1]==':' ) {   /* if full path name */
      if ((fd=fopen(file,mode)) != NULL) {
         strcpy(realnameoffile, file) ;
         return(fd) ;
      } else
         return(NULL) ;
   }
   if (*file == '/') {/* if full path name with unix DIRSEP less drive code */
      if ((fd=fopen(file,mode)) != NULL) {
         strcpy(realnameoffile, file) ;
         return(fd) ;
      } else
         return(NULL) ;
   }
#endif

   do {
      /* copy the current directory into fname */
      nam = fname;
      /* copy till PATHSEP */
#ifdef AMIGA
      if (*path == '.' && (*(path+1) == PATHSEP || *(path+1) == '\0'))
         path++ ; /* '.' represents current dir ("") on the Amiga */
#endif
      if (*path == '~') {
         char *p = nam ;
         path++ ;
         while (*path && *path != PATHSEP && *path != DIRSEP)
            *p++ = *path++ ;
         *p = 0 ;
         if (*nam == 0) {
            if (home == 0) {
               if (0 != (home = getenv("HOME")))
                  home = newstring(home) ;
               else
#ifdef AMIGA
                  home = "" ;
#else
                  home = "." ;
#endif
            }
            strcpy(fname, home) ;
         } else {
#ifdef AMIGA
            error("! ~username in path???") ;
#else
#if defined MSDOS || defined OS2
            error("! ~username in path???") ;
#else
#ifdef VMS
            error("! ~username in path???") ;
#else
#ifdef ATARIST
            error("! ~username in path???") ;
#else
#ifdef VMCMS  /* IBM: VM/CMS */
            error("! ~username in path???") ;
#else
#ifdef MVSXA  /* IBM: MVS/XA */
            error("! ~username in path???") ;
#else
#ifdef __THINK__
            error("! ~username in path???") ;
#else
            struct passwd *pw = getpwnam(fname) ;
            if (pw)
               strcpy(fname, pw->pw_dir) ;
            else
               error("no such user") ;
#endif
#endif  /* IBM: VM/CMS */
#endif
#endif
#endif
#endif
#endif
         }
         nam = fname + strlen(fname) ;
      }
      while (*path != PATHSEP && *path) *nam++ = *path++;
      *nam = 0 ;
#ifndef AMIGA /* AMIGA */
#ifndef VMS
#ifndef __THINK__
      if (nam == fname) *nam++ = '.';   /* null component is current dir */

      if (*file != '\0') {
         if ((nam != fname) && *(nam-1) != DIRSEP) /* GNW 1992.07.09 */
            *nam++ = DIRSEP;                  /* add separator */
         (void)strcpy(nam,file);                   /* tack the file on */
      }
      else
         *nam = '\0' ;
#else
      (void)strcpy(nam,file);                   /* tack the file on */
#endif
#else
      (void)strcpy(nam,file);                   /* tack the file on */
#endif
#else /* AMIGA */
      if (*file != '\0') {
         if (nam != fname && *(nam-1) != DIRSEP && *(nam-1) != VOLSEP)
            *nam++ = DIRSEP ;                   /* add separator */
         strcpy(nam,file) ;                     /* tack the file on */
      }
#endif /* AMIGA */
#ifdef MVSXA
nam = fname;
if (strchr(nam,'=') != NULL) {
   (void) strcpy(fname_safe,fname);  /* save fname */
   firstext = strchr(nam, '=') - nam + 2;
   lastext = strrchr(nam, '.') - nam + 1;
   lastchar  = strlen(nam) - 1;

   (void) strcpy(fname,"dd:");  /* initialize fname */
   nam=&fname[3];
   for (i=lastext; i<=lastchar; i++) *nam++ = fname_safe[i] ;
           *nam++  = '(' ;
   for (i=firstext; i<lastext-1; i++) *nam++ = fname_safe[i] ;
           *nam++  = ')' ;
           *nam++  = 0   ;
   }
   else {
      if (fname[0] == '/') {
         fname[0] = '\'';
         strcat(&fname[strlen(fname)],"\'");
      }
      if (fname[0] == '.') fname[0] = ' ';
      if (fname[1] == '.') fname[1] = ' ';
   }
#endif

      /* belated check -- bah! */
      if ((nam - fname) + strlen(file) + 1 > MAXPATHLEN)
         error("! overran allocated storage in search()");

#ifdef DEBUG
      if (dd(D_PATHS))
         (void)fprintf(stderr,"search: Trying to open %s\n", fname) ;
#endif
      if ((fd=fopen(fname,mode)) != NULL) {
         strcpy(realnameoffile, fname) ;
         return(fd);
      }

   /* skip over PATHSEP and try again */
   } while (*(path++));

   return(NULL);

}               /* end search */
@y
search(struct EnvVarPath *env_var, char *file, char *mode)
{
   register FILE *fd ;                  /* file desc of file */
   char *s, fname[MAXPATHLEN] ;         /* to store file name */

   if (s = EVP_FileSearch(file, env_var, fname, MAXPATHLEN))
   {
#ifdef DEBUG
      if (dd(D_PATHS))
         (void)fprintf(stderr,"search: Trying to open %s\n", fname) ;
#endif
      if ((fd=fopen(fname,mode)) != NULL)
      {
         strcpy(realnameoffile, fname) ;
         return(fd);
      }
      else
         return(NULL); 
   }
   else
   {
      if (strcmp(mode,"r") && strcmp(mode,"rb"))
      {
         if ((fd=fopen(file,mode)) != NULL)	/* this statement could appear useless */
         {                                 	/* but is needed if the mode is a */
            strcpy(realnameoffile, file) ; 	/* write-mode, because EVP_FileSearch */
            return(fd);                    	/* look only for readable files. */
         }
         else
            return(NULL); 
      }
   }

}               /* end search */
@z

@x l.438
         error("! overran allocated storage in search()");
@y
         error("! overran allocated storage in pksearch()");
@z
