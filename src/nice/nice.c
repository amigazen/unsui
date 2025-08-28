//****************************************************************
//
// Name : Nice.c
//
// Date : 20/12/96
//
// Author : Tak Tang
//
// Usage : Nice priority command
//
// Synopsis : Nice runs a command at a different priority.  The
//            default is at 3 below the current.  DOS only.
//
//****************************************************************

//******** Definitions

#define __USE_SYSBASE

#define DOSLIB  "dos.library"
#define DOSVER  36L

#define THISPROC   ((struct Process *)(SysBase->ThisTask))
#define Result2(x) THISPROC->pr_Result2 = x

#define BUFLEN 256

#define MSG_FAILED "Nice failed"

#define TEMPLATE  "-S=STICKY/S,PRI=PRIORITY/N/K,ABS=ABSOLUTE/S,CMD=COMMAND/F"
#define OPT_STICK 0
#define OPT_PRI   1
#define OPT_ABS   2
#define OPT_CMD   3
#define OPT_COUNT 4


//******** Header files

#include <dos/dos.h>
#include <dos/dostags.h>
#include <dos/rdargs.h>
#include <dos/dosextens.h>

#include <exec/types.h>
#include <exec/execbase.h>
#include <exec/memory.h>

#include <string.h>

#include <proto/dos.h>
#include <proto/exec.h>


//******** Storage

UBYTE VersTag[]="\0$VER: Nice 1.0 (20/12/96)";


//******** Main entry

int cmd_nice(void) {
   struct ExecBase *SysBase = (*((struct ExecBase **) 4));
   struct DosLibrary *DOSBase;
   struct RDArgs *rdargs;
   struct Task *task;
   LONG opts[OPT_COUNT];
   LONG rc, newpri, oldpri;

   rc = RETURN_FAIL;

   if ((DOSBase = (struct DosLibrary *)OpenLibrary(DOSLIB, DOSVER))) {
     memset((char *)opts, 0, sizeof(opts));
     rdargs = ReadArgs(TEMPLATE, opts, NULL);
     if (rdargs == NULL) {
       PrintFault(IoErr(), NULL);
     } else {
       if (opts[OPT_PRI]) newpri= *(LONG *)opts[OPT_PRI];
       else               newpri= -3;
       task=FindTask(NULL);
       if ( opts[OPT_ABS] == NULL) newpri+=task->tc_Node.ln_Pri;
       if (newpri < -128) newpri= -128;
       if (newpri >  127) newpri=  127;
       oldpri=SetTaskPri(task,newpri);

       if ( opts[OPT_CMD] ) {
	 rc= SystemTags( (char *)opts[OPT_CMD], SYS_Input, Input(),
		SYS_Output, Output(), TAG_DONE);
       }

       if ( opts[OPT_STICK] == NULL ) SetTaskPri(task,oldpri);
       FreeArgs(rdargs);
     } // if rdargs
     CloseLibrary((struct Library *)DOSBase);
   } else {
     Result2(ERROR_INVALID_RESIDENT_LIBRARY);
   } // if open library(dos)
   return(rc);
} // cmd_nice

//******** End of file

