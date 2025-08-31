#define ANSIGR
#define EIGHTBIT

#ifdef AMIGA
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/utility.h>
#include <proto/intuition.h>
#include <clib/alib_protos.h>
#endif
/*
 * Standard include file for "less".
 */

/*
 * Language details.
 */
#define public          /* PUBLIC FUNCTION */

/*
 * Special types and constants.
 */
typedef long            POSITION;
/*
 * {{ Warning: if POSITION is changed to other than "long",
 *    you may have to change some of the printfs which use "%ld"
 *    to print a variable of type POSITION. }}
 */

#define NULL_POSITION   ((POSITION)(-1))

#define FILENAME        128     /* Max size of a filename */


#ifndef NULL
#define NULL            (0)
#endif

/* How quiet should we be? */
#define NOT_QUIET       0       /* Ring bell at eof and for errors */
#define LITTLE_QUIET    1       /* Ring bell only for errors */
#define VERY_QUIET      2       /* Never ring bell */

/* How should we prompt? */
#define PR_SHORT        0       /* Prompt with colon */
#define PR_MEDIUM       1       /* Prompt with message */
#define PR_LONG         2       /* Prompt with longer message */

/* How should we handle backspaces? */
#define BS_SPECIAL      0       /* Do special things for underlining and bold */
#define BS_NORMAL       1       /* \b treated as normal char; actually output */
#define BS_CONTROL      2       /* \b treated as control char; prints as ^H */

/* Special chars used to tell put_line() to do something special */
#define UL_CHAR         0x81  /* Enter underline mode */
#define UE_CHAR         0x82  /* Exit underline mode */
#define BO_CHAR         0x83  /* Enter boldface mode */
#define BE_CHAR         0x84  /* Exit boldface mode */
#define IT_CHAR         0x85  /* Enter italic mode */
#define IE_CHAR         0x86  /* Exit italic mode */
#define NV_CHAR         0x87  /* Enter inverse video mode */
#define NE_CHAR         0x88  /* Exit inverse video mode */

#define CONTROL(c)              ((c)&037)
#define SIGNAL(sig,func)        signal(sig,func)

/* Library function declarations */
#ifdef AMIGA
#include <fcntl.h>
typedef long offset_t;
#else
offset_t lseek();
#endif

#include "funcs.h"

#ifdef AMIGA
/* version is recognizable by AmigaDos 'version' command, because it
   begins with $VER:.  Ver is the same thing, but without the $VER:.
*/
extern char version[];
#define Ver (version + 6)

#define S_INTERRUPT     01

extern int called_from_WB;
extern int IsV2; /* Non-zero if AmigaDos 2.00 or more */
extern BOOL UseCLI; /* 0 = Open new window; 1 = use CLI window */

extern struct IntuitionBase *IntuitionBase;

#endif
