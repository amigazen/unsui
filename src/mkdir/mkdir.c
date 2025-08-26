
/*
 *  mkdir.c
 *
 *  Author: Georg Hessmann (hessmann@fmi.uni-passau-de)
 *
 *  Copyright: source is public domain, no copyright
 *
 *  Version history:
 *
 *  1.0  25.Sep.92  First release.
 *
 *  1.1  04.Okt.92  Add DEFAULT_ICON define.
 *                  If the define is defined, than mkdir creates always
 *                  a icon, exept NOICON is given.
 *                  If DEFAULT_ICON is not defined, mkdir works like the 
 *                  c:makedir, exept the keyword ICON is given.
 *
 *  1.2  27.Okt.92  Add SAS/C 6.0 support. Initial function needs __saveds!
 *
 *  1.3  01.Nov.92  Borrowed a idea of James McDonald's and Mark McPherson's
 *                  mmdir. Now mkdir can create multiple directories at once.
 *                  (e.g. ram:not_exist_1/not_exist_2 will create both dirs)
 *                  mkdir is a good example of the power of C:
 *                  mkdir does more than mmdir, but is smaller than mmdir.
 *                  But mmdir is completly in ASM, mkdir in C.
 *
 */


#define VERSION "1.3"

static const char version[] = "\0$VER: mkdir " VERSION " (11/01/92)";


#define SysBase		pb->pb_SysBase
#define DOSBase		pb->pb_DOSBase
#define IconBase	pb->pb_IconBase


/*
 * Amiga-Includes
 */

#include <exec/types.h>
#include <dos/dos.h>
#include <dos/dosasl.h>
#include <workbench/workbench.h>

#include <clib/dos_protos.h>
#include <clib/exec_protos.h>
#include <clib/icon_protos.h>
#include <pragmas/dos_pragmas.h>
#include <pragmas/exec_pragmas.h>
#include <pragmas/icon_pragmas.h>


/*
 * Define SAS/C builtin function
 */

#define strlen __builtin_strlen
extern int strlen(char *);


/*
 * Usefull macro to check Ctrl-C
 */

#define IsCtrlC		(SetSignal(0L,SIGBREAKF_CTRL_C) & SIGBREAKF_CTRL_C)



/*
 * Used error strings
 */

#define MSG_DI_OBJ	"Can't create icon %s.info\n"
#define MSG_ERR_CRE	"Can't create directory %s\n"
#define MSG_ALR_EX	"%s already exists\n"
#define MSG_NO_ARG	"No name given\n"


/*
 * Template for ReadArgs()
 */

#if defined(DEFAULT_ICON)
#  define TEMPLATE	"NI=NOICON/S,NAME/M"
#else
#  define TEMPLATE	"ICON/S,NAME/M"
#endif



/*
 * Structure of pseudo global variables.
 */

struct ParamBlock {
#if defined(__USE_SYSBASE) || !defined(__SASC_60)
  struct Library       * pb_SysBase;
#endif
  struct Library       * pb_DOSBase;
  struct Library       * pb_SysBase;
  struct Library       * pb_IconBase;
  int                    pb_CreateIcons;
};


/*
 * Prototypes of help functions
 */

static int CreateTree(struct ParamBlock * pb, char * ptr);
static int CreateOneDir(struct ParamBlock * pb, char * ptr);


/*
 * Main function. Must be the first in the module
 */

long __saveds main(void)
{
  long rc = RETURN_OK;
  long options[2];
  struct RDArgs * args;
  struct ParamBlock Params;
  struct ParamBlock * pb = &Params;

#if defined(__USE_SYSBASE) || !defined(__SASC_60)
  Params.pb_SysBase  = *(struct Library **)(4);
#endif
  Params.pb_DOSBase  = OpenLibrary("dos.library", 37);
  Params.pb_IconBase = OpenLibrary("icon.library", 37);
  if (!(Params.pb_DOSBase && Params.pb_IconBase)) return RETURN_FAIL;

  options[0] = options[1] = 0L;
  args = ReadArgs(TEMPLATE, options, NULL);
  if (args) {
    char ** dirs = (char **)options[1];
    if (dirs) {
      char * ptr;

#if defined(DEFAULT_ICON)
      Params.pb_CreateIcons = !options[0];	// key -- NOICON
#else
      Params.pb_CreateIcons = options[0];	// key -- ICON
#endif

      for (; rc == RETURN_OK && (ptr = *dirs); dirs++) {
        BPTR lock;

        lock = Lock(ptr, ACCESS_READ);
        if (lock) {
          UnLock(lock);
          VPrintf(MSG_ALR_EX, (long*)&ptr);
          rc = RETURN_ERROR;
        }
        else {
          rc = CreateTree(pb, ptr);
        }
      }
    
      FreeArgs(args);
    }
    else {
      PutStr(MSG_NO_ARG);
      rc = RETURN_ERROR;
    }
  }
  else {
    PrintFault(IoErr(), NULL);
    rc = RETURN_ERROR;
  }
  
  CloseLibrary(Params.pb_IconBase);
  CloseLibrary(Params.pb_DOSBase);

  return rc;
}


/*
 *  CreateTree()
 *
 *  Create all not existing directories of path 'ptr'.
 *  Find recursive the first existing dir of the path.
 *  Than with the solving of the recursion, create all
 *  needed directories.
 *  Check for Ctrl-C, too.
 *
 */

static int CreateTree(struct ParamBlock * pb, char * ptr)
{
  BPTR lock;
  int rc;
  char * t;

  /* first: search a existing super-dir */
  for (t=ptr+strlen(ptr)-1;
       t >= ptr && *t != '/' && *t != ':';
       t--) ;

  if (IsCtrlC) {
    PrintFault(ERROR_BREAK, NULL);
    rc = RETURN_WARN;
  }
  else {

    if (*t == '/') {
      *t = '\0';				// nun ist der Pfad etwas kuerzer
      lock = Lock(ptr, ACCESS_READ);
      if (lock) {
        // das uebergeordnete Dir. existiert, also erzeuge dieses Dir. nun
        UnLock(lock);
        *t = '/';				// urspruenglichen Zustand wieder herstellen
        // und nun Directory erzeugen (wurde nach hinten verlegt)
      }
      else {
        // dies ist immer noch nicht das Ueber-Directory, also weitersuchen
        rc = CreateTree(pb, ptr);
        *t = '/';				// urspruenglichen Zustand wieder herstellen
        // und nun (falls rc == RETURN_OK) Directory erzeugen (wurde nach hinten verlegt)
      }
    }
    else {
      // kein '/' gefunden (':' oder gar nichts)
      // dann erzeuge einfach den kompletten String als Dir.
      // und nun Directory erzeugen (wurde nach hinten verlegt)
    }

  }

  if (rc == RETURN_OK) {  
    rc = CreateOneDir(pb, ptr);
  }

  return rc;
}


/*
 *  CreateOneDir()
 *
 *  Create directory 'ptr' and give it a .info file, if 
 *  pb->pb_CreateIcons is set.
 *
 */

static int CreateOneDir(struct ParamBlock * pb, char * ptr)
{
  BPTR lock;
  int rc = RETURN_OK;

  lock = CreateDir(ptr);

  if (lock) {
    if (pb->pb_CreateIcons) {
      struct DiskObject * di = GetDefDiskObject(WBDRAWER);
      if (di && PutDiskObject(ptr, di)) {
        FreeDiskObject(di);
      }
      else {
        VPrintf(MSG_DI_OBJ, (long *)&ptr);
        PrintFault(IoErr(), NULL);
        rc = RETURN_ERROR;
      }
    }
    UnLock(lock);
  }
  else {
    VPrintf(MSG_ERR_CRE, (long*)&ptr);
    PrintFault(IoErr(), NULL);
    rc = RETURN_ERROR;
  }

  return rc;
}
