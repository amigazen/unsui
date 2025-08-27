
/*
 *  lsfont.c
 *
 *  Author: Georg Heﬂmann (Hessmann@Informatik.Uni-Hamburg.De)
 *
 *  Copyright: source/program is FD (freely distributable) © Georg Heﬂmann 1993
 *             It's part of the PasTeX distribution.
 *
 *  Why do I need this program?
 *
 *    If you work with PasTeX and you don't have enough disk space,
 *    you might want only the pk-files you really need on the disk.
 *
 *    Here helps this program, together with the MARK option of
 *    ShowDVI and DVIprint. Start ShowDVI and DVIprint always
 *    with the MARK option (put it into ENVARC:SHOWDVI and 
 *    ENVARC:DVIPRINT) and the drivers will mark every font they
 *    used with an use-count and a date-stamp.
 *
 *    After some time, if you want get rid of old, seldom used fonts,
 *    start "lsfont TeX:pk TO ram:fontlist" and look into the generated
 *    list. The first number is the use count. So often is the font
 *    used from a driver (started with MARK).
 *
 *    If you want delete some fonts of the list, do 
 *    "lsfont TeX:pk TO ram:fontlist DELETE" and edit the file (delete all
 *    fonts out of the file which you *don't* want delete from your
 *    harddisk). After that, you can "execute ram:fontlist" delete the
 *    old fonts.
 *
 *    Other idea to use the list:
 *    Do copy the fonts first on some floppy-disks and delete them after
 *    that from the hard-disk. Now, if you put your backup floppies into
 *    TeX:config/FontVols, the drivers will find them on your floppies.
 *    
 *
 *  Version history:
 *
 *  0.1  18.Jul.93  First try.
 *  1.0  16.Mar.94  Release it with PasTeX 1.4 -- BETA 1 --
 *
 */


#define VERSION "1.0"

static const char version[] = "$VER: lsfont " VERSION " (16.03.94)";


/*
 * Program is 2.04 or higher only!!
 *
 */

long __oslibversion = 37;



#include "lsfont.h"


/*
 * Template for ReadArgs()
 */

#define TEMPLATE	"NAME/M,TO/K,DELETE/S"


/*
 * Used Libraries
 */

extern struct Library * DOSBase;
extern struct Library * UtilityBase;


/*
 * File list structure
 *
 */

struct FLst {
  long               flst_Used;
  BPTR               flst_Lock;
  struct DateStamp   flst_DateStamp;
  struct FLst      * flst_Next;
};


/*
 * Global Vars
 *
 */

static char          NameBuf[1024];
static struct FLst * FileLstRoot = NULL;
static int           SizeFileLst = 0;
static FILE        * OutputFile;
static int           OutDelete;



/*
 * Disable SAS/C CTRL-C functions
 *
 */

int CXBRK(void) { return(0); }
int _CXBRK(void) { return(0); }
void chkabort(void) { return; }



/*
 * Usefull macro to check Ctrl-C
 */

#define IsCtrlC		(SetSignal(0L,SIGBREAKF_CTRL_C) & SIGBREAKF_CTRL_C)




/*
 * Prototypes of the help functions
 */

static int  DoArg        (char * name);
static int  DoDir        (BPTR lck);
static int  DoFile       (BPTR lck, struct FileInfoBlock * fib);
static int  CompareFiles (const void * A, const void * B);
static int  SortFileLst  (void);
static void FreeFileLst  (void);
static int  CheckCtrlC   (int);




/*
 * Main function. 
 */

int main(int argc, char * argv[])
{
  long rc = RETURN_OK;
  long options[3];
  struct RDArgs * args;

  memset(options, 0, sizeof(options));

  args = ReadArgs(TEMPLATE, options, NULL);
  if (args) {
    char ** dirs = (char **)options[0];
    char * NoArg[2];
    
    OutDelete = options[2];
    
    if (options[1]) {
      OutputFile = fopen((char *)options[1], "w");
      if (!OutputFile) {
        PrintFault(IoErr(), (char *)options[1]);
        rc = RETURN_ERROR;
      }
    }
    else {
      OutputFile = stdout;
    }

    if (rc == RETURN_OK && !dirs) {
      // wenn keine Argumente angegeben, dann Name des akt. Dirs. verwenden
      char * namebuf = malloc(500);
      if (!namebuf) {
        PrintFault(ERROR_NO_FREE_STORE, NULL);
        rc = RETURN_ERROR;
      }
      else {
        BPTR lock = Lock("", ACCESS_READ);
        if (!lock) {
          PrintFault(IoErr(), "\"\"");
          rc = RETURN_ERROR;
        }
        else {
          if (!NameFromLock(lock, namebuf, 499)) {
            PrintFault(IoErr(), "\"\"");
            rc = RETURN_ERROR;
          }
          else {
            NoArg[0] = namebuf;		// Name des aktuellen Directories
            NoArg[1] = NULL;
            dirs = NoArg;
          }
          UnLock(lock);
        }
      }
    }

    if (rc == RETURN_OK) {
      char * PatBuf = NULL;
      char * ptr;
  
      for (; rc == RETURN_OK && (ptr = *dirs); dirs++) {
        int isWild;

        if (PatBuf == NULL) PatBuf = malloc(1024);
        if (PatBuf == NULL) {
          PrintFault(ERROR_NO_FREE_STORE, NULL);
          rc = RETURN_ERROR;
          break;
        }
        isWild = ParsePatternNoCase(ptr, PatBuf, 1023);
        
        if (isWild < 0) {
          PrintFault(IoErr(), NULL);
          rc = RETURN_ERROR;
          break;
        }
        else {
          if (isWild == 0) {
            // Kein Pattern
            rc = DoArg(ptr);
            if (rc != RETURN_OK) break;
          }
          else {
            // Pattern
            char __aligned buf[sizeof(struct AnchorPath)+200];
            struct AnchorPath * ap = (struct AnchorPath *)buf;

            memset(ap, 0, sizeof(struct AnchorPath));
            ap->ap_Strlen = 200;

            if (MatchFirst(PatBuf, ap) == 0) {
              rc = DoArg(ap->ap_Buf);
              while (rc == RETURN_OK && MatchNext(ap) == 0) {
                rc = DoArg(ap->ap_Buf);
                if (rc != RETURN_OK) MatchEnd(ap);
              }
              if (rc != RETURN_OK) break;
            }
            else {
              PrintFault(IoErr(), NULL);
              rc = RETURN_FAIL;
            }
          }
        }

      }
      
      if (rc == RETURN_OK) {
        rc = SortFileLst();
      }

      FreeFileLst();
      FreeArgs(args);
    }
  }
  else {
    PrintFault(IoErr(), NULL);
    rc = RETURN_ERROR;
  }
  
  return rc;
}



/*
 * DoArg()
 *
 * Nimm einen Namen (Dir oder File) und teste ihn.
 *
 */

static int DoArg(char * name)
{
  long ret;
  __aligned struct FileInfoBlock fib;
  BPTR lck;
  
  lck = Lock(name, ACCESS_READ);
  if (lck) {
    if (Examine(lck, &fib)) {
      if (fib.fib_DirEntryType > 0) {
        ret = DoDir(lck);
      }
      else {
        ret = DoFile(lck, &fib);
      }
    }
    else {
      PrintFault(IoErr(), name);
      ret = RETURN_ERROR;
    }
    UnLock(lck);
  }
  else {
    PrintFault(IoErr(), name);
    ret = RETURN_ERROR;
  }
  
  return ret;    
}


/*
 * DoDir()
 * 
 * Durchsuche ein ganzes Directory.
 *
 */

static int DoDir(BPTR lck)
{
  int ret = RETURN_OK;
  BPTR newlck = DupLock(lck);
  int err;
  __aligned struct FileInfoBlock newfib;

  if (newlck) {
    if (Examine(newlck, &newfib)) {
      while (ret == RETURN_OK && ExNext(newlck, &newfib)) {
        BPTR olddir = CurrentDir(newlck);
        BPTR new = Lock(newfib.fib_FileName, ACCESS_READ);
        if (new) {
          if (newfib.fib_DirEntryType > 0) {
            ret = DoDir(new);
          }
          else {
            ret = DoFile(new, &newfib);
          }
          UnLock(new);
        }
        else {
          PrintFault(IoErr(), newfib.fib_FileName);
          ret = RETURN_ERROR;
        }
        CurrentDir(olddir);
        ret = CheckCtrlC(ret);
      }

      if (ret == RETURN_OK) {
        err = IoErr();
        if (err != ERROR_NO_MORE_ENTRIES) {
          PrintFault(err, NULL);
          ret = RETURN_ERROR;
        }
      }
    }
    UnLock(newlck);
  }


  return ret;
}


/*
 * DoFile()
 * 
 * Ein File abtesten.
 *
 */

static int DoFile(BPTR lck, struct FileInfoBlock * fib)
{
  int ret = RETURN_OK;
  long num = 0;
  struct FLst * flst;
  
  flst = malloc(sizeof(struct FLst));
  if (!flst) {
    PrintFault(ERROR_NO_FREE_STORE, NULL);
    return RETURN_ERROR;
  }
  
  if (!strncmp(fib->fib_Comment, "FontUsed: ", 10)) {
    if (sscanf(fib->fib_Comment+10, "%d (%d %d %d)", 
        &num, &flst->flst_DateStamp.ds_Days, &flst->flst_DateStamp.ds_Minute, 
	&flst->flst_DateStamp.ds_Tick) != 4) {
      num = 0;
    }
  }
  
  if (num == 0) {
    // nehme als Zeit das Datum des Files
    flst->flst_DateStamp = fib->fib_Date;
  }
  
  flst->flst_Used = num;
  flst->flst_Lock = DupLock(lck);
  
  if (!flst->flst_Lock) {
    PrintFault(IoErr(), NULL);
    return RETURN_ERROR;
  }
  
  // in die Liste einhaengen
  flst->flst_Next = FileLstRoot;
  FileLstRoot = flst;
  SizeFileLst++;

  return ret;
}



/*
 * CompareFiles()
 *
 * Compare to FLst structures. Will be called from qsort()
 *
 */

static int CompareFiles(const void * A, const void * B)
{
  struct FLst * a = *(struct FLst **)A;
  struct FLst * b = *(struct FLst **)B;
  
  if (a->flst_Used == b->flst_Used) {
    return CompareDates(&b->flst_DateStamp, &a->flst_DateStamp);
  }
  else {
    return a->flst_Used - b->flst_Used;
  }
}




/*
 * SortFileLst()
 *
 * Soriere die aufgebaute Liste von Files und gib sie aus
 *
 */

static int SortFileLst(void)
{
  int ret = RETURN_OK;
  __aligned struct FileInfoBlock fib;
  struct FLst *  flst;
  struct FLst ** SortArr;
  int i;
  
  SortArr = malloc(SizeFileLst * sizeof(struct FLst *));
  if (!SortArr) {
    PrintFault(ERROR_NO_FREE_STORE, NULL);
    return RETURN_ERROR;
  }

  for (flst = FileLstRoot, i=0; flst; flst = flst->flst_Next, i++) {
    SortArr[i] = flst;
  }

  ret = CheckCtrlC(ret);
  
  /*
   * Sortiere SortArr:
   * Nach unten kommen die haeufig benuzten, bzw. vor kurem benutzten Files.
   *
   */

  if (ret == RETURN_OK) {
    qsort(SortArr, SizeFileLst, sizeof(struct FLst *), CompareFiles);
  }
  
  for (i=0; ret == RETURN_OK && i<SizeFileLst; i++) {
    flst = SortArr[i];

    if (Examine(flst->flst_Lock, &fib)) {
      if (NameFromLock(flst->flst_Lock, NameBuf, sizeof(NameBuf)-1)) {
        if (OutDelete) {
          fprintf(OutputFile, "delete %s FORCE\n", NameBuf);          
        }
        else {
          fprintf(OutputFile, "%d\t%s\n", flst->flst_Used, NameBuf);          
        }
      }
      else {
        PrintFault(ERROR_BUFFER_OVERFLOW, NULL);
        ret = RETURN_ERROR;
      }
    }
    else {
      PrintFault(IoErr(), NULL);
      ret = RETURN_ERROR;
    }
    
    ret = CheckCtrlC(ret);
  }
  
  free(SortArr);
  
  return ret;
}



/*
 * FreeFileLst()
 *
 * Free all Lock's in the list.
 *
 */

static void FreeFileLst(void)
{
  struct FLst * flst;
  
  for (flst = FileLstRoot; flst; flst = flst->flst_Next) {
    UnLock(flst->flst_Lock);
  }
}



/*
 * Test for CTRL-C and return ERROR_WARN
 *
 */

static int CheckCtrlC(int ret)
{
  if (ret == RETURN_OK && IsCtrlC) {
    PrintFault(ERROR_BREAK, NULL);
    ret = RETURN_WARN;
  }
  
  return ret;
}
