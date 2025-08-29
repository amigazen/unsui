/*
 *   Includes for minrexx.c; please refer to that file for
 *   further documentation.
 */
#include <rexx/rxslib.h>


/*
 *   This is the list of functions we can access.  (Cheap forward
 *   declarations, too.)
 */
long upRexxPort(char *s, struct rexxCommandList *rcl, char *exten, long (*uf)(struct RexxMsg *, struct rexxCommandList *, char *)) ;
void dnRexxPort(void) ;
long dispRexxPort(void) ;
struct RexxMsg *sendRexxCmd(char *s,APTR f, STRPTR p1, STRPTR p2, STRPTR p3) ;
/**
struct RexxMsg *sendRexxCmd(char *s, int (*f)(struct RexxMsg *, struct rexxCommandList *, char *),
				STRPTR p1, STRPTR p2, STRPTR p3) ;
**/
struct RexxMsg *syncRexxCmd(char *s, struct RexxMsg *msg) ;
struct RexxMsg *asyncRexxCmd(char *s) ;
void replyRexxCmd(struct RexxMsg *msg, long primary, long secondary, char *string) ;


/*
 *   Maximum messages that can be pending, and the return codes
 *   for two bad situations.
 */
#define MAXRXOUTSTANDING (50)
#define RXERRORIMGONE (100)
#define RXERRORNOCMD (30)

/*
 *   This is the association list you build up (statically or
 *   dynamically) that should be terminated with an entry with
 *   NULL for the name . . .
 */
struct rexxCommandList {
   char *name ;
   APTR userdata ;
} ;

STRPTR		CreateArgstring(STRPTR, long);
void		DeleteArgstring(STRPTR);
struct RexxMsg	*CreateRexxMsg(struct MsgPort *, STRPTR, STRPTR);
void		DeleteRexxMsg(struct RexxMsg *);

#pragma libcall RexxSysBase CreateArgstring 7e 0802
#pragma libcall RexxSysBase DeleteArgstring 84 801
#pragma libcall RexxSysBase CreateRexxMsg   90 09803
#pragma libcall RexxSysBase DeleteRexxMsg   96 801
