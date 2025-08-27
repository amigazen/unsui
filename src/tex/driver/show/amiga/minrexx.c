/*
 *   This is an example of how REXX messages might be handled.  This is
 *   a `minimum' example that both accepts asynchronous REXX messages and
 *   can request REXX service.
 *
 *   Read this entire file!  It's short enough.
 *
 *   It is written in such a fashion that it can be attached to a program
 *   with a minimum of fuss.  The only external symbols it makes available
 *   are the seven functions and RexxSysBase.
 *
 *   This code is by Radical Eye Software, but it is put in the public
 *   domain.  I would appreciate it if the following string was left in
 *   both as a version check and as thanks from you for the use of this
 *   code.
 *
 *   If you modify this file for your own use, don't bump the version
 *   number; add a suffix, such as 1.0a or 1.0.3 or something, so we
 *   don't have fake `versions' floating around.
 */
static char *blurb = "Radical Eye MinRexx 0.4.1" ;
/*
 *   We read in our own personal little include.
 */

#include <stdio.h>

#include "defines.h"
#include "globals.h"

#include "minrexx.h"
#include "amscreen.h"

#include "globals.i"
#include "am_requ.i"

#include <stdio.h>
#include <string.h>

#include <clib/exec_protos.h>
#include <clib/alib_protos.h>
#include <pragmas/exec_pragmas.h>



/*
 * Fuer die locale-Library:
 *
 * Hier duerfen *nur* die MSG_#? Nummern eingebunden werden!
 * Achtung:
 * Es muss/sollte 'multiple-include' erlaubt sein!
 */
#include "local.i"

#undef  CATCOMP_ARRAY
#undef  CATCOMP_BLOCK
#undef  CATCOMP_STRINGS
#define CATCOMP_NUMBERS
#include "localstr.h"



/*
 *   All of our local globals, hidden from sight.
 */
static struct MsgPort *rexxPort ;          /* this is *our* rexx port */
static int bringerdown ;                   /* are we trying to shut down? */
static struct rexxCommandList *globalrcl ; /* our command association list */
static long stillNeedReplies ;             /* how many replies are pending? */
static long rexxPortBit ;                  /* what bit to wait on for Rexx? */
static char *extension ;                   /* the extension for macros */
static long (*userdisp)(struct RexxMsg *, struct rexxCommandList *, char *) ;
			                   /* the user's dispatch function */
static struct RexxMsg *oRexxMsg ;          /* the outstanding Rexx message */
/*
 *   Our library base.  Don't you dare close this!
 */
struct RxsLib *RexxSysBase = NULL;


/* prototypes */
static void closeRexxLib(void);
static int  cmdcmp(register char *c, register char *m);
static int  openRexxLib(void);
static void replytoit(struct RexxMsg *msg);


/*
 *   This is the main entry point into this code.
 */
long upRexxPort(s, rcl, exten, uf)
/*
 *   The first argument is the name of your port to be registered;
 *   this will be used, for instance, with the `address' command of ARexx.
 */
char *s ;
/*
 *   The second argument is an association list of command-name/user-data
 *   pairs.  It's an array of struct rexxCommandList, terminated by a
 *   structure with a NULL in the name field. The commands are case
 *   sensitive.  The user-data field can contain anything appropriate,
 *   perhaps a function to call or some other data.
 */
struct rexxCommandList *rcl ;
/*
 *   The third argument is the file extension for ARexx macros invoked
 *   by this program.  If you supply this argument, any `primitive' not
 *   in the association list rcl will be sent out to ARexx for
 *   interpretation, thus allowing macro programs to work just like
 *   primitives.  If you do not want this behavior, supply a `NULL'
 *   here, and those commands not understood will be replied with an
 *   error value of RXERRORNOCMD.
 */
char *exten ;
/*
 *   The fourth argument is the user dispatch function.  This function
 *   will *only* be called from rexxDisp(), either from the user calling
 *   this function directly, or from dnRexxPort().  Anytime a command
 *   match is found in the association list, this user-supplied function
 *   will be called with two arguments---the Rexx message that was
 *   received, and a pointer to the association pair.  This function
 *   should return a `1' if the message was replied to by the function
 *   and a `0' if the default success code of (0, 0) should be returned.
 *   Note that the user function should never ReplyMsg() the message;
 *   instead he should indicate the return values with replyRexxCmd();
 *   otherwise we lose track of the messages that still lack replies.
 */
long (*uf)(struct RexxMsg *, struct rexxCommandList *, char *) ;
/*
 *   upRexxPort() returns the signal bit to wait on for Rexx messages.
 *   If something goes wrong, it simply returns a `0'.  Note that this
 *   function is safe to call multiple times because we check to make
 *   sure we haven't opened already.  It's also a quick way to change
 *   the association list or dispatch function.
 */
{
   struct MsgPort *FindPort() ;
   struct MsgPort *CreatePort() ;

/*
 *   Some basic error checking.
 */
   if (rcl == NULL || uf == NULL)
      return(0L) ;
/*
 *   If we aren't open, we make sure no one else has opened a port with
 *   this name already.  If that works, and the createport succeeds, we
 *   fill rexxPortBit with the value to return.
 *
 *   Note that rexxPortBit will be 0 iff rexxPort is NULL, so the check
 *   for rexxPort == NULL also insures that our rexxPortBit is 0.
 */
   if (rexxPort == NULL) {
      Forbid() ;
      if (FindPort(s)==NULL)
         rexxPort = CreatePort(s, 0L) ;
      Permit() ;
      if (rexxPort != NULL)
         rexxPortBit = 1L << rexxPort->mp_SigBit ;
   }
/*
 *   Squirrel away these values for our own internal access, and return
 *   the wait bit.
 */
   globalrcl = rcl ;
   extension = exten ;
   userdisp = uf ;
   return(rexxPortBit) ;
}

/*
 *   This function closes the rexx library, but only if it is open
 *   and we aren't expecting further replies from REXX.  It's
 *   *private*, but it doesn't have to be; it's pretty safe to
 *   call anytime.
 */
static void closeRexxLib() {
   if (stillNeedReplies == 0 && RexxSysBase) {
      CloseLibrary(&(RexxSysBase->rl_Node)) ;
      RexxSysBase = NULL ;
   }
}

/*
 *   This function closes down the Rexx port.  It is always safe to
 *   call, and should *definitely* be made a part of your cleanup
 *   routine.  No arguments and no return.  It removes the Rexx port,
 *   replies to all of the messages and insures that we get replies
 *   to all the ones we sent out, closes the Rexx library, deletes the
 *   port, clears a few flags, and leaves.
 */
void dnRexxPort() {
   if (rexxPort) {
      RemPort(rexxPort) ;
      bringerdown = 1 ;
/*
 *   A message still hanging around?  We kill it off.
 */
      if (oRexxMsg) {
         oRexxMsg->rm_Result1 = RXERRORIMGONE ;
         ReplyMsg(&(oRexxMsg->rm_Node)) ;
         oRexxMsg = NULL ;
      }
      while (stillNeedReplies) {
         WaitPort(rexxPort) ;
         dispRexxPort() ;
      }
      closeRexxLib() ;
      DeletePort(rexxPort) ;
      rexxPort = NULL ;
   }
   rexxPortBit = 0 ;
}

/*
 *   Here we dispatch any REXX messages that might be outstanding.
 *   This is the main routine for handling Rexx messages.
 *   This function is fast if no messages are outstanding, so it's
 *   pretty safe to call fairly often.
 *
 *   If we are bring the system down and flushing messages, we reply
 *   with a pretty serious return code RXERRORIMGONE.
 *
 *   No arguments, no returns.
 */
long dispRexxPort() {
   register struct RexxMsg *RexxMsg ;
   register struct rexxCommandList *rcl ;
   register char *p ;
   register int dontreply ;
   long ret = 0L;

/*
 *   If there's no rexx port, we're out of here.
 */
   if (rexxPort == NULL)
      return ret;
/*
 *   Otherwise we have our normal loop on messages.
 */
   while ((bringerdown || ret == 0) && (RexxMsg = (struct RexxMsg *)GetMsg(rexxPort))) {
/*
 *   If we have a reply to a message we sent, we look at the second
 *   argument.  If it's set, it's a function we are supposed to call
 *   so we call it.  Then, we kill the argstring and the message
 *   itself, decrement the outstanding count, and attempt to close
 *   down the Rexx library.  Note that this call only succeeds if
 *   there are no outstanding messages.  Also, it's pretty quick, so
 *   don't talk to me about efficiency.
 */
      if (RexxMsg->rm_Node.mn_Node.ln_Type == NT_REPLYMSG) {
         if (RexxMsg->rm_Result1 != 0) {
	   Message(MSG_AREXX_REPLY_CODE, RexxMsg->rm_Result1);
	 }
         if (RexxMsg->rm_Args[1]) {
            /* prototype only for replytoit !! */
            ((int (*)(struct RexxMsg *msg))(RexxMsg->rm_Args[1]))(RexxMsg);
         }
         DeleteArgstring(RexxMsg->rm_Args[0]) ;
         DeleteRexxMsg(RexxMsg) ;
         stillNeedReplies-- ;
         closeRexxLib() ;
/*
 *   The default case is we got a message and we need to check it for
 *   primitives.  We skip past any initial tabs or spaces and initialize
 *   the return code fields.
 */
      } else {
         p = (char *)RexxMsg->rm_Args[0] ;
         while (*p > 0 && *p <= ' ')
            p++ ;
         RexxMsg->rm_Result1 = 0 ;
         RexxMsg->rm_Result2 = 0 ;
/*
 *   If somehow the reply is already done or postponed, `dontreply' is
 *   set.
 */
         dontreply = 0 ;
/*
 *   If the sky is falling, we just blow up and replymsg.
 */
         if (bringerdown) {
            RexxMsg->rm_Result1 = RXERRORIMGONE ;
/*
 *   Otherwise we cdr down our association list, comparing commands,
 *   until we get a match.  If we get a match, we call the dispatch
 *   function with the appropriate arguments, and break out.
 */
         } else {
            oRexxMsg = RexxMsg ;
            for (rcl = globalrcl; rcl->name; rcl++) {
               if (cmdcmp(rcl->name, p) == 0) {
                  ret = userdisp(RexxMsg, rcl, p+strlen(rcl->name)) ;
                  break ;
               }
            }
/*
 *   If we broke out, rcl will point to the command we executed; if we
 *   are at the end of the list, we didn't understand the command.  In
 *   this case, if we were supplied an extension in upRexxPort, we know
 *   that we should send the command out, so we do so, synchronously.
 *   The synchronous send takes care of our reply.  If we were given a
 *   NULL extension, we bitch that the command didn't make sense to us.
 */
            if (rcl->name == NULL) {
               if (extension) {
                  syncRexxCmd(RexxMsg->rm_Args[0], RexxMsg) ;
                  dontreply = 1 ;
               } else {
                  RexxMsg->rm_Result1 = RXERRORNOCMD ;
               }
            }
         }
/*
 *   Finally, reply if appropriate.
 */
         oRexxMsg = NULL ;
         if (! dontreply)
            ReplyMsg(&(RexxMsg->rm_Node)) ;
      }
   }

   return ret;
}

/*
 *   This is the function we use to see if the command matches
 *   the command string.  Not case sensitive, and the real command only
 *   need be a prefix of the command string.  Make sure all commands
 *   are given in lower case!
 */
static int cmdcmp(c, m)
register char *c, *m ;
{
   while (*c && ((*c == *m) || (*c == *m + 32 && ('a' <= *c && *c <= 'z')))) {
      c++ ;
      m++ ;
   }
   return (int)(*c) ;
}

/*
 *   Opens the Rexx library if unopened.  Returns success (1) or
 *   failure (0).  This is another function that is *private* but
 *   that doesn't have to be.
 */
static int openRexxLib() {
   if (RexxSysBase)
      return(1) ;
   return((RexxSysBase = (struct RxsLib *)OpenLibrary(RXSNAME, 0L)) != NULL) ;
}

/*
 *   This is the general ARexx command interface, but is not the one
 *   you will use most of the time; ones defined later are easier to
 *   understand and use.  But they all go through here.
 */
struct RexxMsg *sendRexxCmd(s, f, p1, p2, p3)
char *s ;
/*
 *   The first parameter is the command to send to Rexx.
 */
/* int (*f)() ; */
APTR f;
/*
 *   The second parameter is either NULL, indicating that the command
 *   should execute asynchronously, or a function to be called when the
 *   message we build up and send out here finally returns.  Please note
 *   that the function supplied here could be called during cleanup after
 *   a fatal error, so make sure it is `safe'.  This function always is
 *   passed one argument, the RexxMsg that is being replied.
 */
STRPTR p1, p2, p3 ;
/*
 *   These are up to three arguments to be stuffed into the RexxMsg we
 *   are building up, making the values available when the message is
 *   finally replied to.  The values are stuffed into Args[2]..Args[4].
 */
{
   register struct MsgPort *rexxport ;
   register struct RexxMsg *RexxMsg ;

/*
 *   If we have too many replies out there, we just return failure.
 *   Note that you should check the return code to make sure your
 *   message got out!  Then, we forbid, and make sure that:
 *      - we have a rexx port open
 *      - Rexx is out there
 *      - the library is open
 *      - we can create a message
 *      - we can create an argstring
 *
 *   If all of these succeed, we stuff a few values and send the
 *   message, permit, and return.
 */
   if (rexxPort == NULL || stillNeedReplies > MAXRXOUTSTANDING-1)
      return(NULL) ;
   RexxMsg = NULL ;
   if (openRexxLib() && (RexxMsg =
             CreateRexxMsg(rexxPort, extension, rexxPort->mp_Node.ln_Name)) &&
             (RexxMsg->rm_Args[0] = CreateArgstring(s, (long)strlen(s)))) {
      RexxMsg->rm_Action  = RXCOMM | RXFF_NOIO;
      RexxMsg->rm_Args[1] = (STRPTR)f ;
      RexxMsg->rm_Args[2] = p1 ;
      RexxMsg->rm_Args[3] = p2 ;
      RexxMsg->rm_Args[4] = p3 ;
      RexxMsg->rm_Node.mn_Node.ln_Name = RXSDIR ;
      Forbid() ;
      if (rexxport = FindPort(RXSDIR))
         PutMsg(rexxport, &(RexxMsg->rm_Node)) ;
      Permit() ;
      if (rexxport) {
         stillNeedReplies++ ;
         return(RexxMsg) ;
      } else
         DeleteArgstring(RexxMsg->rm_Args[0]) ;
   }
   if (RexxMsg)
      DeleteRexxMsg(RexxMsg) ;
   closeRexxLib() ;
   return(NULL) ;
}

/*
 *   This function is used to send out an ARexx message and return
 *   immediately.  Its single parameter is the command to send.
 */
struct RexxMsg *asyncRexxCmd(s)
char *s ;
{
   return(sendRexxCmd(s, NULL, NULL, NULL, NULL)) ;
}

/*
 *   This function sets things up to reply to the message that caused
 *   it when we get a reply to the message we are sending out here.
 *   But first the function we pass in, which actually handles the reply.
 *   Note how we get the message from the Args[2]; Args[0] is the command,
 *   Args[1] is this function, and Args[2]..Args[4] are any parameters
 *   passed to sendRexxCmd() as p1..p3.  We pass the result codes right
 *   along.
 */
static void replytoit(msg)
struct RexxMsg *msg ;
{
   register struct RexxMsg *omsg ;

   if (msg != NULL) {
     omsg = (struct RexxMsg *)(msg->rm_Args[2]) ;
     replyRexxCmd(omsg, msg->rm_Result1, msg->rm_Result2, NULL) ;
     ReplyMsg(&(omsg->rm_Node)) ;
   }
}

/*
 *   This function makes use of everything we've put together so far,
 *   and functions as a synchronous Rexx call; as soon as the macro
 *   invoked here returns, we reply to `msg', passing the return codes
 *   back.
 */
struct RexxMsg *syncRexxCmd(s, msg)
char *s ;
struct RexxMsg *msg ;
{
   return(sendRexxCmd(s, (APTR)&replytoit, (STRPTR)msg, NULL, NULL)) ;
}

/*
 *   There are times when you want to pass back return codes or a
 *   return string; call this function when you want to do that,
 *   and return `1' from the user dispatch function so the main
 *   event loop doesn't reply (because we reply here.)  This function
 *   always returns 1.
 */
void replyRexxCmd(msg, primary, secondary, string)
/*
 *   The first parameter is the message we are replying to.
 */
struct RexxMsg *msg ;
/*
 *   The next two parameters are the primary and secondary return
 *   codes.
 */
long primary, secondary ;
/*
 *   The final parameter is a return string.  This string is only
 *   returned if the primary return code is 0, and a string was
 *   requested.
 *
 *   We also note that we have replied to the message that came in.
 */
char *string ;
{
/*
 *   Note how we make sure the Rexx Library is open before calling
 *   CreateArgstring . . . and we close it down at the end, if possible.
 */
   if (primary == 0 && (msg->rm_Action & (1L << RXFB_RESULT))) {
      if (string && openRexxLib())
         secondary = (long)CreateArgstring(string, (long)strlen(string)) ;
      else
         secondary = 0L ;
   }
   msg->rm_Result1 = primary ;
   msg->rm_Result2 = secondary ;
   closeRexxLib() ;
}
