/*************************************************************************
 ***                              move.c                  John Bickers ***
 *** Date begun: 13 Jun 1992.                                          ***
 *** Last modified: 26 Jun 1992.                                       ***
 *************************************************************************/
/* A move command for AmigaDOS 2.0.                                      *
 *************************************************************************/

#include <exec/types.h>
#include <exec/memory.h>
#include <dos/dos.h>

#include <proto/exec.h>
#include <proto/dos.h>

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "funcs.h"

typedef struct ANCHP {
    struct AnchorPath ap;
    char    ext[256];
} ANCHP;

typedef struct ANCHS {
    struct AnchorPath __aligned ap;
} ANCHS;

#define ARGT    "FROM/M,TO/A,ALL/S,BUF=BUFFER/K/N,CLONE/S,QUIET/S,TEST/S"
#define AFROM   0
#define ATO     1
#define AALL    2
#define ABUF    3
#define ACLONE  4
#define AQUIET  5
#define ATEST   6
LONG    args[7];
struct RDArgs *argh;
ANCHP __aligned anch;

int     isdir;          /* 1 means dest. is a directory */
int     samevol;        /* 1 means FROM vol == TO vol */
int     inmatch;        /* 1 means in matching sequence */
char    *dest;          /* Destination name */
LONG    destvol;        /* Destination volume task pointer */
BPTR    cpi,cpo;        /* File handles used during copy */
char    buf[1024];
char    base[1024];
char    toname[1024];

UBYTE   *cb;
long    cbsize;

#define MAXSUB  64
ANCHS   asub[MAXSUB];
int     sub = -1;

static const char *version_tag = "$VER: Move 47.1 (26/08/25)\n";
static const char *stack_cookie = "$STACK: 4096";

void    mprintf(char *cp,...) /*=========================================*/
{
va_list vararg;
    if (!cp) return;  /* NULL pointer check */
    
    va_start(vararg,cp);
    /* Limit output to buffer size to prevent overflow */
    if (strlen(cp) < sizeof(buf) - 100) {  /* Leave room for formatting */
        vsprintf(buf,cp,vararg);
        PutStr(buf);
    } else {
        /* Truncate message if too long */
        strncpy(buf, "move: message too long", sizeof(buf) - 1);
        buf[sizeof(buf) - 1] = '\0';
        PutStr(buf);
    }
    va_end(vararg);
}

LONG    getvol(name) /*==================================================*/
char    *name;
{
char    *cp;
BPTR    lock;
LONG    ret;
char    c;

    if (!name) return 0;  /* NULL pointer check */

    for (cp = name; *cp && *cp != ':'; cp++) ;
    if (!*cp) lock = Lock(":",ACCESS_READ);
    else {
        cp++;
        c = *cp;
        *cp = 0;
        lock = Lock(name,ACCESS_READ);
        *cp = c;
    }
    if (!lock) {
        mprintf("move: getvol(%s) Lock() failed\n", name ? name : "NULL");
        return 0;
    }

    ret = (LONG)((struct FileHandle *)(lock << 2))->fh_Port;
    UnLock(lock);
    return(ret);
}

void    makebase(from) /*================================================*/
char    *from;
{
char    *cp;
int     s;

    if (!from) return;  /* NULL pointer check */

    strcpy(base,from);
    while (1) {
        for (cp = base; *cp; cp++) ;
        for (cp--; *cp != '/' && *cp != ':' && cp >= base; cp--) ;
        if (cp < base) {
            base[0] = 0;
            break;
        }
        if (*cp == ':') {
            *(cp + 1) = 0;
            break;
        }
      /* Else *cp == '/' */
        *cp = 0;
        s = ParsePattern(base,buf,sizeof(buf));
        if (s == 0) {
            strcpy(cp,"/");     /* Restore trailing slash */
            break;
        }
        if (s == -1) {
            mprintf("move: ParsePattern error\n");
            return;  /* Return instead of calling cleanup */
        }
    }
    samevol = (getvol(base) == destvol);
}

void    checkpath(name) /*===============================================*/
char    *name;
{
char    *cp;
BPTR    lock;

    if (!name) return;  /* NULL pointer check */

    cp = name;
    while (1) {
        for (; *cp && *cp != '/'; cp++) ;
        if (!*cp) break;

        *cp = 0;
        lock = Lock(name,ACCESS_READ);
        if (lock) {
            UnLock(lock);
            *cp++ = '/';
            continue;
        }

        lock = CreateDir(name);
        if (!lock) {
            mprintf("move: failed to create directory %s\n",name);
            return;  /* Return instead of calling cleanup */
        }
        UnLock(lock);
        if (!args[AQUIET]) mprintf("move: %s created\n",name);

        *cp++ = '/';
    }
}

void    maketoname(from) /*==============================================*/
char    *from;
{
int     len;

    if (!from) return;  /* NULL pointer check */

    strcpy(toname,dest);
    if (!isdir) return;

    len = strlen(base);
    AddPart(toname,from + len,sizeof(toname));
}

/*** addfrom() --------------------------------------------------------***
 * Each FROM argument passes through this routine. If it's a pattern, we *
 * call addpat(), otherwise we call addit(). An empty from argument is   *
 * equivalent to the pattern "#?".                                       *
 ***-------------------------------------------------------------------***/
void    addfrom(from) /*=================================================*/
char    *from;
{
    if (!from) return;  /* NULL pointer check */
    
    if (!*from) {
        addpat("#?");
        return;
    }
    switch ( ParsePattern(from,buf,sizeof(buf)) ) {
        case (0):
            addit(from);
            break;
        case (1):
            addpat(from);
            break;
        default:
            mprintf("move: ParsePattern(\"%s\") error\n",from);
            return;  /* Return instead of calling cleanup */
    }
}

/*** addit() ----------------------------------------------------------***
 * Add a name to the list. The name will be a non-pattern one from the   *
 * FROM list.                                                            *
 ***-------------------------------------------------------------------***/
void    addit(from) /*===================================================*/
char    *from;
{
char    *cp;
BPTR    lock;
struct FileInfoBlock __aligned fib;

    if (!from) return;  /* NULL pointer check */

    if (CheckSignal(SIGBREAKF_CTRL_C)) {
        mprintf("move: *** break\n");
        return;  /* Return instead of calling cleanup */
    }

    lock = Lock(from,ACCESS_READ);
    if (!lock || !Examine(lock,&fib)) {
        if (!args[AQUIET]) mprintf("move: can't find %s\n",from);
        if (lock) UnLock(lock);
        return;
    }
    UnLock(lock);

    if (fib.fib_DirEntryType > 0) {
        if (!samevol) {
            if (!isdir) isdir = 1;

            strcpy(base,from);
            addslash(base);

            sprintf(buf,"0%s",base);    /* 0: basename */
            if (!l_add(buf)) {
                mprintf("move: out of RAM\n");
                return;  /* Return instead of calling cleanup */
            }

            sub = -1;
            addsub(NULL);
        }
        else {
            for (cp = dest; *cp; cp++) ;
            if (cp > dest && *(cp - 1) == ':') {
                strcpy(base,from);
                addslash(base);

                sprintf(buf,"4%s",base);
                if (!l_add(buf)) {
                    mprintf("move: out of RAM\n");
                    return;  /* Return instead of calling cleanup */
                }

                sub = -1;
                addsub(NULL);
            }
            else {
                sprintf(buf,"2%s",from);
                noslash(buf);
                if (!l_add(buf)) {
                    mprintf("move: out of RAM\n");
                    return;  /* Return instead of calling cleanup */
                }
            }
        }
    }
    else {
        sprintf(buf,"%c%s",(samevol)? '2': '3',from);   /* 2/3: full name */
        noslash(buf);
        if (!l_add(buf)) {
            mprintf("move: out of RAM\n");
            return;  /* Return instead of calling cleanup */
        }
    }
}

char    *noslash(sp) /*==================================================*/
char    *sp;
{
char    *cp;
    if (!sp) return NULL;  /* NULL pointer check */
    
    for (cp = sp; *cp; *cp++) ;
    if (cp > sp && *(cp - 1) == '/') *(cp - 1) = 0;
    return(sp);
}

char    *addslash(sp) /*=================================================*/
char    *sp;
{
char    *cp;
    if (!sp) return NULL;  /* NULL pointer check */
    
    for (cp = sp; *cp; *cp++) ;
    if (cp > sp && (*(cp - 1) == ':')) return(sp);
    *cp++ = '/';
    *cp = 0;
    return(sp);
}    

/*** addsub() ---------------------------------------------------------***
 * Recursive routine that adds all files under from to the name list. We *
 * add the part of the name following the base, and prepend a '1'.       *
 ***-------------------------------------------------------------------***/
void    addsub(from) /*==================================================*/
char    *from;
{
struct AnchorPath *ap;
char    *cp;
int     i,s;

    if (sub + 1 >= MAXSUB) {
        mprintf("move: addsub() too deep\n");
        return;  /* Return instead of calling cleanup */
    }

    strcpy(buf,base);
    if (from) {
        strcat(buf,from);
        if (sub >= 0) strcat(buf,"/");
    }
    for (i = 0; i <= sub; i++) {
        strcat(buf,asub[i].ap.ap_Info.fib_FileName);
        if (i < sub) strcat(buf,"/");
    }
    for (cp = buf; *cp; cp++) ;
    if (cp == buf || *(cp - 1) == ':' || *(cp - 1) == '/') strcpy(cp,"#?");
    else strcpy(cp,"/#?");

    ap = &(asub[sub+1].ap);
    memset(ap,0,sizeof(struct AnchorPath));
    ap->ap_Flags = APF_DOWILD;

    sub++;
    s = MatchFirst(buf,ap);
    while (!s) {
        if (CheckSignal(SIGBREAKF_CTRL_C)) {
            mprintf("move: *** break\n");
            sub--;  /* Restore sub level before returning */
            return;  /* Return instead of calling cleanup */
        }

        if (ap->ap_Info.fib_DirEntryType > 0) addsub(from);
        else {
            sprintf(buf,"1%s",asub[0].ap.ap_Info.fib_FileName);
            for (i = 1; i <= sub; i++) {
                strcat(buf,"/");
                strcat(buf,asub[i].ap.ap_Info.fib_FileName);
            }

            if (!l_add(buf)) {
                mprintf("move: out of RAM\n");
                sub--;  /* Restore sub level before returning */
                return;  /* Return instead of calling cleanup */
            }
        }
        s = MatchNext(ap);
    }
    MatchEnd(ap);
    sub--;

    sprintf(buf,"9%s",base);
    if (from) {
        strcat(buf,from);
        if (sub >= 0) strcat(buf,"/");
    }
    for (i = 0; i <= sub; i++) {
        strcat(buf,"/");
        strcat(buf,asub[i].ap.ap_Info.fib_FileName);
        if (i < sub) strcat(buf,"/");
    }
    for (cp = buf; *cp; cp++) ;
    if (cp > buf && *(cp - 1) == '/') *(cp - 1) = 0;
    if (!l_add(buf)) {
        mprintf("move: out of RAM\n");
        return;  /* Return instead of calling cleanup */
    }
}

void    addpat(from) /*==================================================*/
char    *from;
{
int     len,s;

    if (!from) return;  /* NULL pointer check */

    sprintf(buf,"%c%s",(samevol)? '5': '6',base);
    if (!l_add(buf)) {
        mprintf("move: out of RAM\n");
        return;  /* Return instead of calling cleanup */
    }

    len = strlen(base);
    sub = -1;

    memset(&anch,0,sizeof(anch));
    anch.ap.ap_Flags = APF_DOWILD;
    anch.ap.ap_Strlen = sizeof(anch.ext);

    inmatch = 1;
    s = MatchFirst(from,&anch);
    while (!s) {
        if (CheckSignal(SIGBREAKF_CTRL_C)) {
            mprintf("move: *** break\n");
            inmatch = 0;  /* Reset flag before returning */
            return;  /* Return instead of calling cleanup */
        }

        if (anch.ap.ap_Info.fib_DirEntryType > 0) {
            if (args[AALL]) {
                sprintf(buf,"8%s",anch.ap.ap_Buf + len);
                if (!l_add(buf)) {
                    mprintf("move: out of RAM\n");
                    inmatch = 0;  /* Reset flag before returning */
                    return;  /* Return instead of calling cleanup */
                }
                addsub(anch.ap.ap_Buf + len);
            }
        }
        else {
            sprintf(buf,"7%s",anch.ap.ap_Buf + len);
            if (!l_add(buf)) {
                mprintf("move: out of RAM\n");
                inmatch = 0;  /* Reset flag before returning */
                return;  /* Return instead of calling cleanup */
            }
        }
        s = MatchNext(&anch);
    }
    MatchEnd(&anch);
    inmatch = 0;
}

void    movelist() /*====================================================*/
{
char    *basep,*dirp,*sp;

    basep = "";
    dirp  = NULL;
    for (sp = l_first(); sp; sp = l_next()) {
        if (CheckSignal(SIGBREAKF_CTRL_C)) {
            mprintf("move: *** break\n");
            return;  /* Return instead of calling cleanup */
        }
        switch (*sp) {
            case ('0'):     /* !samevol, basename for subdir, ensure dir is created */
                basep = sp + 1;
                dirp = NULL;
                samevol = 0;
                break;
            case ('1'):     /* extension for 0 or 4 or 8 */
                moveit(basep,dirp,sp + 1);
                break;
            case ('2'):     /* samevol, name == base */
                dirp = NULL;
                samevol = 1;
                moveit(sp + 1,NULL,NULL);
                break;
            case ('3'):     /* !samevol, name == base */
                dirp = NULL;
                samevol = 0;
                moveit(sp + 1,NULL,NULL);
                break;
            case ('5'):     /* samevol, basename for pattern */
                basep = sp + 1;
                dirp = NULL;
                samevol = 1;
                break;
            case ('6'):     /* !samevol, basename for pattern */
                basep = sp + 1;
                dirp = NULL;
                samevol = 0;
                break;
            case ('7'):     /* extension for 5 or 6 */
                dirp = NULL;
                moveit(basep,NULL,sp + 1);
                break;
            case ('8'):     /* dirp extension for 5 or 6 */
                dirp = sp + 1;
                break;
            case ('9'):     /* directory from addsub() */
                forcedir(basep,sp + 1);
                break;
        }
    }
}

/*** moveit() ---------------------------------------------------------***
 * Move a file given by basep and nodep to the destination. We use       *
 * base[] as our "from" name space.                                      *
 ***-------------------------------------------------------------------***/
void    moveit(basep,dirp,nodep) /*======================================*/
char    *basep,*dirp,*nodep;
{
    if (!basep) return;  /* NULL pointer check */

    strcpy(base,basep);
    if (dirp) AddPart(base,dirp,sizeof(base));
    if (nodep) AddPart(base,nodep,sizeof(base));
    else nodep = FilePart(basep);

    strcpy(toname,dest);
    if (isdir) {
        if (dirp) AddPart(toname,dirp,sizeof(toname));
        AddPart(toname,nodep,sizeof(toname));
    }

    if (samevol) renameit(base,toname);
    else copyit(base,toname);

    if (!args[AQUIET]) mprintf("move: %s moved to %s\n",base,toname);
}

void    forcedir(basep,nodep) /*=========================================*/
char    *basep,*nodep;
{
BPTR    lock;
int     len,s;
struct FileInfoBlock __aligned fib;

    if (!basep || !nodep) return;  /* NULL pointer check */

  /* basep contains the base part, nodep actually contains the whole thing */
    len = strlen(basep);

    strcpy(toname,dest);
    if (isdir) AddPart(toname,nodep + len,sizeof(toname));

    lock = Lock(toname,ACCESS_READ);
    if (lock) {
        s = Examine(lock,&fib);
        UnLock(lock);
        if (!s || fib.fib_DirEntryType < 0) {
            mprintf("move: can't create directory %s\n",toname);
            return;  /* Return instead of calling cleanup */
        }
    }
    else {
        lock = CreateDir(toname);
        if (!lock) {
            checkpath(toname);
            lock = CreateDir(toname);
            if (!lock) {
                mprintf("move: can't create directory %s\n",toname);
                return;  /* Return instead of calling cleanup */
            }
        }
        UnLock(lock);
        if (!args[AQUIET]) mprintf("move: created %s\n",toname);
    }

    lock = Lock(nodep,ACCESS_READ);
    if (!lock) return;

    s = Examine(lock,&fib);
    UnLock(lock);
    if (!s) return;

    if (args[ACLONE]) {
        if (fib.fib_Comment[0]) SetComment(toname,fib.fib_Comment);
        SetFileDate(toname,&fib.fib_Date);
        SetProtection(toname,fib.fib_Protection);
    }
    s = DeleteFile(nodep);
    if (!s && !args[AQUIET]) mprintf("move: can't delete %s\n",nodep);
}

void    renameit(fname,tname) /*=========================================*/
char    *fname,*tname;
{
BPTR    lock;

    if (!fname || !tname) return;  /* NULL pointer check */

    lock = Lock(fname,ACCESS_READ);     /* So DeleteFile() will fail if tname == fname */
    DeleteFile(tname);
    if (lock) UnLock(lock);

    if (Rename(fname,tname)) return;

    checkpath(tname);
    if (!Rename(fname,tname)) {
        mprintf("move: failed to rename %s to %s\n",fname,tname);
        return;  /* Return instead of calling cleanup */
    }
}

void    copyit(fname,tname) /*===========================================*/
char    *fname,*tname;
{
BPTR    lock;
long    s,size;
struct FileInfoBlock __aligned fib;

    if (!fname || !tname) return;  /* NULL pointer check */

    lock = Lock(fname,ACCESS_READ);
    if (!lock) {
        mprintf("move: can't find %s\n",fname);
        return;  /* Return instead of calling cleanup */
    }

    if (!Examine(lock,&fib)) {
        mprintf("move: can't examine %s\n",fname);
        UnLock(lock);
        return;  /* Return instead of calling cleanup */
    }
    UnLock(lock);

    cpi = Open(fname,MODE_OLDFILE);
    if (!cpi) {
        mprintf("move: can't read %s\n",fname);
        return;  /* Return instead of calling cleanup */
    }

    cpo = Open(tname,MODE_NEWFILE);
    if (!cpo) {
        checkpath(tname);
        cpo = Open(tname,MODE_NEWFILE);
        if (!cpo) {
            mprintf("move: can't write %s\n",tname);
            Close(cpi);  /* Clean up input file handle */
            cpi = 0;
            return;  /* Return instead of calling cleanup */
        }
    }

    size = args[ABUF];
    if (size < 1024) size = fib.fib_Size;
    if (size < 1024) size = 1024;

    if (!cb || cbsize < size) {
        if (cb) FreeMem(cb,cbsize);
        cbsize = size;
        cb = AllocMem(cbsize,0L);
        if (!cb) {
            mprintf("move: out of RAM for copy (needed %ld bytes)\n",cbsize);
            Close(cpo);  /* Clean up output file handle */
            Close(cpi);  /* Clean up input file handle */
            cpo = 0;
            cpi = 0;
            DeleteFile(tname);  /* Remove partial output file */
            return;  /* Return instead of calling cleanup */
        }
    }

    while (1) {
        size = Read(cpi,cb,cbsize);
        if (size <= 0) break;

        s = Write(cpo,cb,size);
        if (s != size) {
            Close(cpo);
            cpo = 0;
            DeleteFile(tname);
            mprintf("move: error writing %s, file removed\n",tname);
            Close(cpi);  /* Clean up input file handle */
            cpi = 0;
            return;  /* Return instead of calling cleanup */
        }
        if (size < cbsize) break;
    }

    s = Close(cpo);
    cpo = 0;
    if (!s) {
        DeleteFile(tname);
        mprintf("move: error closing %s, file removed\n",tname);
        Close(cpi);  /* Clean up input file handle */
        cpi = 0;
        return;  /* Return instead of calling cleanup */
    }

    Close(cpi);
    cpi = 0;

    if (!DeleteFile(fname) && !args[AQUIET]) mprintf("move: can't delete %s\n",fname);

    if (args[ACLONE]) {
        if (fib.fib_Comment[0]) SetComment(tname,fib.fib_Comment);
        SetFileDate(tname,&fib.fib_Date);
        SetProtection(tname,fib.fib_Protection);
    }
}

void    main() /*=======================================================*/
{
char    **from;

    if (DOSBase->dl_lib.lib_Version < 37) {
        Write(Output(),"move: requires KS2.0\n",21);
        exit(1);
    }

    argh = ReadArgs(ARGT,args,NULL);
    if (!argh || !args[AFROM] || !args[ATO]) {
        mprintf("move: required argument missing\n");
        cleanup(NULL);
        return;
    }

  /* Process TO argument first */
    dest = (char *)(args[ATO]);
    destvol = getvol(dest);
    if (!*dest) isdir = 1;
    else {
    BPTR    lock;
    struct FileInfoBlock __aligned fib;
        if (ParsePattern(dest,buf,sizeof(buf)) == 1) {
            mprintf("move: destination cannot be a pattern\n");
            cleanup(NULL);
            return;
        }
        lock = Lock(dest,ACCESS_READ);
        if (lock && Examine(lock,&fib) && fib.fib_DirEntryType > 0) isdir = 1;
        if (lock) UnLock(lock);
    }

    from = (char **)(args[AFROM]);
    if (from[1] || ParsePattern(from[0],buf,sizeof(buf)) == 1) isdir = 1;

    while (*from) {
        makebase(*from);
        addfrom(*from);
        from++;
    }

    if (!args[ATEST]) movelist();
    else {
    char    *cp;
        for (cp = l_first(); cp; cp = l_next()) mprintf("move: %c %s\n",*cp,cp + 1);
    }

    cleanup(NULL);
}

int     CXBRK() { return(0); }          /* Disable SAS ^C check */

void    cleanup(char *cp,...) /*=========================================*/
{
va_list vararg;
int     i;

    if (cp && *cp) {
        va_start(vararg,cp);
        vsprintf(buf,cp,vararg);
        PutStr(buf);
        va_end(vararg);
    }

    for (i = 0; i <= sub; i++) MatchEnd(&(asub[i].ap));

    l_close();
    if (cb) FreeMem(cb,cbsize);
    if (cpo) Close(cpo);
    if (cpi) Close(cpi);
    if (inmatch) MatchEnd(&anch);
    if (argh) FreeArgs(argh);

    /* Don't call exit() - let calling function handle it */
}
