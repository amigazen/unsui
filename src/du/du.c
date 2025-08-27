
/*
 *  du.c
 *
 *  Author: Georg Hessmann (hessmann@fmi.uni-passau-de)
 *
 *  Copyright: source is public domain, no copyright
 *
 *  Version history:
 *
 *  1.0  01.Nov.92  First release.
 *
 */


#define VERSION "1.0"

static const char version[] = "\0$VER: du " VERSION " (11/01/92)";




#define SysBase		pb->pb_SysBase
#define DOSBase		pb->pb_DOSBase
#define UtilityBase	pb->pb_UtilityBase

#undef __USE_SYSBASE
#define NOINFO


/*
 * Amiga-Includes
 */

#include <exec/types.h>
#include <exec/lists.h>
#include <exec/memory.h>
#include <dos/dos.h>
#include <dos/dosasl.h>
#include <dos/exall.h>

#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <clib/utility_protos.h>
#if defined(__SASC_60) && defined(__USE_SYSBASE)
# include <pragmas/exec_sysbase_pragmas.h>
#else
# include <pragmas/exec_pragmas.h>
#endif
#include <pragmas/dos_pragmas.h>
#include <pragmas/utility_pragmas.h>


#pragma libcall DOSBase ParsePatternNoCase 3C6 32103
#pragma libcall DOSBase ExAllEnd 3DE 5432105



/*
 * Define SAS/C builtin function
 */

#define strlen __builtin_strlen
#define memset __builtin_memset
#define memcpy __builtin_memcpy

extern int strlen(char *);
extern void *memset(void *, int, unsigned);
extern void *memcpy(void *, void *, unsigned);



/*
 * Define amiga.lib function.
 */

void __stdargs NewList(struct List * list);


/*
 * Template for ReadArgs()
 */

#if defined(NOINFO)
# define TEMPLATE	"DIR/M,BYTE/S,KBYTE/S,ALL/S,SHORT/S,NOINFO/S"
#else
# define TEMPLATE	"DIR/M,BYTE/S,KBYTE/S,ALL/S,SHORT/S"
#endif



/*
 * Used error strings
 */

#define MSG_NOT_FOUND	"%s not found\n"
#define MSG_PAT_ERROR   "Error in pattern %s\n"
#define MSG_NO_MEM      "Not enough memory\n"
#define MSG_NO_EXAMINE  "Can't examine %s\n"
#define MSG_NO_LOCK	"Can't lock %s\n"
#define MSG_NO_NAMELOCK "Can't get name from lock\n"


/*
 * Usefull macro to check Ctrl-C
 */

#define IsCtrlC		(SetSignal(0L,SIGBREAKF_CTRL_C) & SIGBREAKF_CTRL_C)


/*
 * Stack of the 'to processed' directories
 */

struct DirStack {
  struct Node   ds_Node;
  short         ds_Pad;
  BPTR          ds_Lock;
  long          ds_Size;		/* bytes */
  long          ds_Blocks;
  struct List   ds_Childs;		/* Unterdirectories (falls vorhanden) */
};


/*
 * Defines the current path
 */

struct CurPath {
  struct Node   cp_Node;
  char        * cp_Path;
};


/*
 * List of all allocated memory
 */

struct MemLst {
  void          * ml_Data;
  long            ml_Size;
  struct MemLst * ml_Next;
};


/*
 * Structure of all pseudo global variables
 */

enum pval   { blocks, bytes, kbytes };

struct ParamBlock {
  struct Library       * pb_DOSBase;
  struct Library       * pb_SysBase;
  struct Library       * pb_UtilityBase;
  int                    pb_BlockSize;
  struct ExAllControl  * pb_EAC;
  struct FileInfoBlock * pb_Fib;
  enum pval              pb_PrtVal;
  int                    pb_All;
  int                    pb_Short;
  int                    pb_PrintIt;	// fuer SHORT
#if defined(NOINFO)
  int                    pb_NoInfo;
#endif
  int                    pb_NeedSlash;
  struct List            pb_Path;
  int                    pb_EASize;
  char                 * pb_EAData;
  char                 * pb_MemHandle;
  char                 * pb_MemEnd;	// zeigt auf das letzte Byte
  char                 * pb_CurMem;
  struct MemLst        * pb_MemList;
};



/*
 * Prototypes of the help functions
 */

static int  DoDir        (struct ParamBlock * pb, char * name);
static int  ListOneTree  (struct ParamBlock * pb, struct DirStack * ds, BPTR lock);
static int  ListOneDir   (struct ParamBlock * pb, struct DirStack * ds, BPTR lock);
static int  InsertNewDir (struct ParamBlock * pb, struct DirStack * ds, char * name);
static int  AppendPath   (struct ParamBlock * pb, struct DirStack * ds);
static void PrintPath    (struct ParamBlock * pb);
static int  PrintIt      (struct ParamBlock * pb, long blk, long bte, char * name);
static char * MyStrDup   (struct ParamBlock * pb, char * str);
static void * MyAlloc    (struct ParamBlock * pb, long size);
static void MyFree       (struct ParamBlock * pb);




/*
 * Main function. Must be the first in the module.
 */

int __saveds du(void)
{
  long rc = RETURN_OK;
  long options[6];
  struct RDArgs * args;
  char * PatBuf = NULL;
  struct FileInfoBlock __aligned fib;  // wird nur an pb weiter gereicht
  struct ParamBlock Params;
  struct ParamBlock * pb = &Params;

#if defined(__USE_SYSBASE) || !defined(__SASC_60)
  Params.pb_SysBase  = *(struct Library **)(4);
#endif
  Params.pb_DOSBase     = OpenLibrary("dos.library", 37);
  Params.pb_UtilityBase = OpenLibrary("utility.library", 37);
  if (!Params.pb_DOSBase || !Params.pb_UtilityBase) return RETURN_FAIL;

  Params.pb_MemHandle = NULL;
  Params.pb_MemEnd    = NULL;
  Params.pb_CurMem    = NULL;
  Params.pb_MemList   = NULL;
  Params.pb_EAData    = MyAlloc(pb, 2000);
  Params.pb_EASize    = 2000;
  Params.pb_Fib       = &fib;
  Params.pb_PrtVal    = blocks;
  Params.pb_PrintIt   = TRUE;

  memset(options, 0, sizeof(options));

  args = ReadArgs(TEMPLATE, options, NULL);
  if (args) {
    char ** dirs = (char **)options[0];
    char * NoArg[2];			

    Params.pb_EAC = AllocDosObject(DOS_EXALLCONTROL, NULL);
    if (Params.pb_EAC) {

      if (options[1]) Params.pb_PrtVal = bytes;
      if (options[2]) Params.pb_PrtVal = kbytes;
      Params.pb_All       = options[3];
      Params.pb_Short     = options[4];
#if defined(NOINFO)
      Params.pb_NoInfo    = options[5];
#endif
      
      if (Params.pb_EAData) {
      
        if (!dirs) {
          // wenn keine Argumente angegeben, dann Name des akt. Dirs. verwenden
          char * namebuf = MyAlloc(pb, 500);
          if (!namebuf) {
              PutStr(MSG_NO_MEM);
              rc = RETURN_ERROR;
          }
          else {
            BPTR lock = Lock("", ACCESS_READ);
            if (!lock) {
              char * temp = "\"\"";
              VPrintf(MSG_NO_LOCK, (long *)&temp);
              rc = RETURN_ERROR;
            }
            else {
              if (!NameFromLock(lock, namebuf, 499)) {
                PutStr(MSG_NO_NAMELOCK);
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
          char * ptr;
  
          for (; ptr = *dirs; dirs++) {
            int isWild;

            if (PatBuf == NULL) PatBuf = MyAlloc(pb, 1024);
            if (PatBuf == NULL) {
              PutStr(MSG_NO_MEM);
              rc = RETURN_ERROR;
              break;
            }
            isWild = ParsePatternNoCase(ptr, PatBuf, 1023);
            
            if (isWild < 0) {
              VPrintf(MSG_PAT_ERROR, (long*)&ptr);
              rc = RETURN_ERROR;
              break;
            }
            else {
              if (isWild == 0) {
                // Kein Pattern
                rc = DoDir(pb, ptr);
                if (rc != RETURN_OK) break;
              }
              else {
                // Pattern
                char __aligned buf[sizeof(struct AnchorPath)+200];
                struct AnchorPath * ap = (struct AnchorPath *)buf;

                memset(ap, 0, sizeof(struct AnchorPath));
                ap->ap_Strlen = 200;

                if (MatchFirst(PatBuf, ap) == 0) {
                  rc = DoDir(pb, ap->ap_Buf);
                  while (rc == RETURN_OK && MatchNext(ap) == 0) {
                    rc = DoDir(pb, ap->ap_Buf);
                    if (rc != RETURN_OK) MatchEnd(ap);
                  }
                  if (rc != RETURN_OK) break;
                }
              }
            }

          }
          FreeArgs(args);
        }
      }
      else {
        PutStr(MSG_NO_MEM);
        rc = RETURN_ERROR;
      }

      FreeDosObject(DOS_EXALLCONTROL, Params.pb_EAC);
    }
    else {
      PutStr(MSG_NO_MEM);
      rc = RETURN_ERROR;
    }

    MyFree(pb);

  }
  else {
    PrintFault(IoErr(), NULL);
    rc = RETURN_ERROR;
  }
  
  CloseLibrary(DOSBase);

  return rc;
}



/*
 * DoDir()
 *
 * Main function to 'du' one directory or file.
 *
 */

static int DoDir(struct ParamBlock * pb, char * name)
{
  long ret = RETURN_OK;
  BPTR lock;
  struct DirStack   ds;
  struct InfoData __aligned idata;

  memset(&ds, 0, sizeof(struct DirStack));
  NewList(&ds.ds_Childs);

  pb->pb_PrintIt   = TRUE;
  pb->pb_BlockSize = 512;
  NewList(&pb->pb_Path);
  
  ds.ds_Node.ln_Name = name;
  lock = Lock(name, ACCESS_READ);
  
  if (lock) {
    if (Info(lock, &idata)) {
      pb->pb_BlockSize = idata.id_BytesPerBlock;
      //VPrintf("BS: %ld\n", (long*)&pb->pb_BlockSize);
    }

    if (ListOneTree(pb, &ds, lock)) {
      ret = RETURN_ERROR;
    }
    UnLock(lock);
  }
  else {
    VPrintf(MSG_NOT_FOUND, (long*)&name);
    ret = RETURN_ERROR;
  }

  return ret;    
}



/*
 * ListOneTree()
 *
 * List one file or one directory.
 * If the directory has sub-directory, ListOneTree()
 * calls itself recursively.
 *
 */

static int ListOneTree(struct ParamBlock * pb, struct DirStack * ds, BPTR lock)
{
  register struct List * dl;
  BPTR olock, nlock;
  int ret;
  int oPrintIt = pb->pb_PrintIt;
#define child(x)	((struct DirStack *)(x)->lh_Head)
  
  if (!Examine(lock, pb->pb_Fib)) {
    VPrintf(MSG_NO_EXAMINE, (long *)&ds->ds_Node.ln_Name);
    return -1;
  }
  
  if (pb->pb_Fib->fib_DirEntryType > 0) {
    // es ist ein Directory
  
    olock = CurrentDir(lock);
    AppendPath(pb, ds);

    ret = ListOneDir(pb, ds, lock);
    
    dl = &ds->ds_Childs;
    while (!ret && !IsListEmpty(dl)) {

      nlock = Lock(child(dl)->ds_Node.ln_Name, ACCESS_READ);
      if (!nlock) {
        VPrintf(MSG_NO_LOCK, (long *)&child(dl)->ds_Node.ln_Name);
        ret = -1;
      }
      else {
        if (pb->pb_Short) pb->pb_PrintIt = FALSE;
        ret = ListOneTree(pb, child(dl), nlock);
        UnLock(nlock);
      }
      
      ds->ds_Size += child(dl)->ds_Size;
      ds->ds_Blocks += child(dl)->ds_Blocks + 1 +1 ;

      RemHead(dl);
    }
    
    pb->pb_PrintIt = oPrintIt;

    if (!ret) ret = PrintIt(pb, ds->ds_Blocks, ds->ds_Size, NULL);
    CurrentDir(olock);
    RemTail(&pb->pb_Path);

  }
  else {
    // es ist ein normales File
    ds->ds_Blocks = UDivMod32(pb->pb_Fib->fib_Size, pb->pb_BlockSize) + 1 + 1;
    ds->ds_Size = pb->pb_Fib->fib_Size;

    ret = PrintIt(pb, ds->ds_Blocks, ds->ds_Size, ds->ds_Node.ln_Name);
  }

  return ret;
}


/*
 * ListOneDir()
 *
 * List all files/directories of one directory and builds the
 * list of the child directories.
 *
 */

static int ListOneDir(struct ParamBlock * pb, struct DirStack * ds, BPTR lock)
{
   struct ExAllControl * eac = pb->pb_EAC;
   struct ExAllData * ead;
   long size = 0;
   long blocks = 0;
   int more;
   
   eac->eac_LastKey = 0;
   do {
       more = ExAll(lock, (struct ExAllData *)pb->pb_EAData, pb->pb_EASize, ED_SIZE, eac);
       if ((!more) && (IoErr() != ERROR_NO_MORE_ENTRIES)) {
           /* ExAll failed abnormally */
           break;
       }
       if (eac->eac_Entries == 0) {
           /* ExAll failed normally with no entries */ 
           continue;                   /* ("more" is *usually* zero) */
       }
       ead = (struct ExAllData *)pb->pb_EAData;

       do {
       
           if (ead->ed_Type > 0) {
             /* ist ein Directory */
             if (InsertNewDir(pb, ds, ead->ed_Name)) {
               /* Fehler beim Eintragen */
               // ExAllEnd bring GURU
               //if (pb->pb_DOSBase->lib_Version >= 39) {
               //  ExAllEnd(lock, (struct ExAllData *)pb->pb_EAData, pb->pb_EASize, ED_SIZE, eac);
               //}
               return -1;
             }
           }
           else {
             long blk = UDivMod32(ead->ed_Size, pb->pb_BlockSize) + 1 + 1;
             if (pb->pb_All) {
               if (PrintIt(pb, blk, ead->ed_Size, ead->ed_Name)) {
                 // CTRL-C wurde gedrueckt
                 // ExAllEnd bring GURU
                 //if (pb->pb_DOSBase->lib_Version >= 39) {
                 //  ExAllEnd(lock, (struct ExAllData *)pb->pb_EAData, pb->pb_EASize, ED_SIZE, eac);
                 //}
                 return -1;
               }
             }
             size += ead->ed_Size; 
             blocks += blk;

             // das mit den File-Ext. Bloecken funkt. noch nicht so ganz
             //if (ead->ed_Size > 72 * pb->pb_BlockSize) blocks += ead->ed_Size / (72 * pb->pb_BlockSize) ;
           }
           /* get next ead */
           ead = ead->ed_Next;

       } while (ead);

   } while (more);

   
   ds->ds_Size = size;
   ds->ds_Blocks = blocks;
   
   return 0;
}



/*
 * InsertNewDir()
 *
 * Add one sub-directory into the child-list of the current directory.
 *
 */

static int InsertNewDir(struct ParamBlock * pb, struct DirStack * ds, char * name)
{
  struct DirStack * nds = MyAlloc(pb, sizeof(struct DirStack));
  
  if (!nds) {
    PutStr(MSG_NO_MEM);
    return -1;
  }
  NewList(&nds->ds_Childs);
  
  nds->ds_Node.ln_Name  = MyStrDup(pb, name);
  if (!nds->ds_Node.ln_Name) {
    PutStr(MSG_NO_MEM);
    return -1;
  }

  AddTail(&ds->ds_Childs, &nds->ds_Node);
  
  return 0;
}


/*
 * AppendPath()
 *
 * Add a new directory to the current path.
 *
 */

static int AppendPath(struct ParamBlock * pb, struct DirStack * ds)
{
  struct CurPath * cp = MyAlloc(pb, sizeof(struct CurPath));
  
  if (!cp) {
    PutStr(MSG_NO_MEM);
    return -1;
  }

  cp->cp_Path = ds->ds_Node.ln_Name;
  cp->cp_Node.ln_Name = ds->ds_Node.ln_Name;
  AddTail(&pb->pb_Path, &cp->cp_Node);
}


/*
 * PrintPath()
 *
 * Print the current path to stdout.
 *
 */

static void PrintPath(struct ParamBlock * pb)
{
  struct CurPath * cp;
  char * s;
  char c;
  
  pb->pb_NeedSlash = FALSE;

  if (IsListEmpty(&pb->pb_Path)) return;

  for (cp = (struct CurPath *)pb->pb_Path.lh_Head;
       cp != (struct CurPath *)pb->pb_Path.lh_TailPred;
       cp = (struct CurPath *)cp->cp_Node.ln_Succ) {

    s = cp->cp_Path;
    if (s) {
      if (s[strlen(s)-1] == ':' || s[strlen(s)-1] == '/') {
        PutStr(s);
      }
      else {
        PutStr(s);
        PutStr("/");
      }
    }
  }
  PutStr(cp->cp_Path);
  c = cp->cp_Path[strlen(cp->cp_Path)-1];
  pb->pb_NeedSlash = c != ':' && c != '/';
}



/*
 * PrintIt()
 *
 * Print the data of a directory (name==NULL) or a file (name!=NULL)
 * Check for Ctrl-C.
 *
 */

static int PrintIt(struct ParamBlock * pb, long blk, long bte, char * name)
{ 
  long t;

  if (IsCtrlC) {
    PrintFault(ERROR_BREAK, NULL);
    return -1;
  }

  if (!pb->pb_PrintIt) return 0;
  
#if defined(NOINFO)
  if (name) {
    long len = strlen(name);
    if (pb->pb_NoInfo &&
        name[len-5] == '.' &&
        name[len-4] == 'i' &&
        name[len-3] == 'n' &&
        name[len-2] == 'f' &&
        name[len-1] == 'o') return 0;
  }
#endif

  switch (pb->pb_PrtVal) {
    case blocks:
      t = blk;
      break;
    case bytes:
      t = bte;
      break;
    case kbytes:
      t = UDivMod32((bte+512), 1024);
      break;
  }
  VPrintf("%ld\t", &t);
  PrintPath(pb);
  if (name) {
    if (pb->pb_NeedSlash) PutStr("/");
    PutStr(name);
  }
  PutStr("\n");
  
  return 0;
}


/*
 * MyStrDup()
 *
 * Make a copy of a string.
 *
 */

static char * MyStrDup(struct ParamBlock * pb, char * str)
{
  int len = strlen(str);
  char * new = MyAlloc(pb, len+1);
  CopyMem(str, new, len+1);
  
  return new;
}


/*
 * MyAlloc()
 *
 * Alloc a pice of memory.
 *
 */

static void * MyAlloc(struct ParamBlock * pb, long size)
{
# define MEMBLCK  (5*1024)

  size = (size+3) & 0xFFFFFFFC;		// damit der naechste wieder aligned ist
  					// struct MemList sollte aligned-Groesse haben

  if (size > MEMBLCK-sizeof(struct MemList)) return NULL; // geht nicht, brauch ich nicht

  if (pb->pb_MemHandle == NULL || pb->pb_CurMem+size > pb->pb_MemEnd) {
    if ((pb->pb_MemHandle = AllocMem(MEMBLCK, MEMF_PUBLIC | MEMF_CLEAR)) == NULL) {
      return NULL;
    }
    else {
      struct MemLst * ml = (struct MemLst *)pb->pb_MemHandle;
      pb->pb_CurMem = pb->pb_MemHandle + sizeof(struct MemLst);
      pb->pb_MemEnd = pb->pb_MemHandle + MEMBLCK - 1;
      ml->ml_Data = (void *)pb->pb_MemHandle;
      ml->ml_Size = MEMBLCK;
      ml->ml_Next = pb->pb_MemList;
      pb->pb_MemList = ml;
      return MyAlloc(pb, size);
    }
  }
  else {
    void * t = pb->pb_CurMem;
    pb->pb_CurMem += size;
    return t;		// immer long aligned
  }
}


/*
 * MyFree()
 *
 * Frees _all_ allocated memory (MyStrDup/MyAlloc).
 *
 */

static void MyFree(struct ParamBlock * pb)
{
  struct MemLst * ml1, * ml2;

  for (ml1 = pb->pb_MemList; ml1 != NULL; ml1 = ml2) {
    //if ((char *)ml1 == (char *)ml1->ml_Data) PutStr("ok\n");
    //else PutStr("NOOO!\n");
    ml2 = ml1->ml_Next;
    FreeMem(ml1->ml_Data, ml1->ml_Size);
  }
}


