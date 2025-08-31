/* low level ARexx code for use in amiga version of Emacs.
   Copyright (C) 1993 Christian E. Hopps.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "SimpleRexx.h"
#include <proto/exec.h>
#undef NULL
#include "config.h"
#include "lisp.h"

#include "amiga.h"

#define XLRXMSG(x) ((struct LispRexxMsg *) XPNTR((x)))
#define XSETLRXMSG(x,v) (XSET((x), Lisp_Int, v))

static AREXXCONTEXT far handle;
static int amiga_arexx_initialized;

/* This structure should be allocated with emacs_malloc() its pointer will be */
/* used as an msgid. (emacs XUINT())*/
struct LispRexxMsg {
  struct MinNode  lrm_Node;			  /* A node for tracking messages. */
  struct RexxMsg *lrm_Msg;			  /* The actual Rexx Msg. */
  ULONG  lrm_Flags;
};
/* Flags for LispRexxMessage indicating what to do with it. */
#define LRMF_SENTCMD (1L << 0)			  /* this msg originated here. */
#define LRMF_DOERRORS (1L << 1)			  /* handle error replies */
#define LRMF_DORESULTS (1L << 2)		  /* handle result strings */

struct LispRexxList {
    struct MinList lrl_List;
    int    lrl_Count;
};

struct LispRexxList pending;			  /* The list of pending */
						  /* (outgoing) Rexx Messages. */
struct LispRexxList returned;			  /* The list of pending */
						  /* (outgoing) Rexx Messages */
						  /* that have been received. */
struct LispRexxList incoming;			  /* The message that are */
						  /* incoming to Emacs (sent */
						  /* from some other rexx host. */

/* allocate a rexx message properly */
Lisp_Object alloc_rexx_msg(ULONG flags)
{
    Lisp_Object rm = Qnil;
    struct LispRexxMsg *lrm = (struct LispRexxMsg *)malloc(sizeof(*lrm));
    if(lrm) {
	lrm->lrm_Flags = flags;
	XSETLRXMSG(rm,lrm);
	return(rm);
    }
    return(Qnil);
}

/* free an arexx message allocated with alloc_arexx_msg() */
void free_rexx_msg (Lisp_Object rm)
{
    if(!NULL(rm)) {
	void *mem = XLRXMSG(rm);
	free(mem);
    }
}

/* The next 2 functions imlement FIFO lists. */

/* add LispRexxMsg to a LispRexxLisp Tail. */
void add_rexx_msg_to_tail(struct LispRexxList *rl, Lisp_Object rm)
{
    AddTail((struct List *)rl,(struct Node *)XPNTR(rm));
    rl->lrl_Count++;
}

/* remove LispRexxMsg from head of a LispRexxLisp. */
Lisp_Object remove_rexx_msg_from_head(struct LispRexxList *rl)
{
    Lisp_Object rm = (Lisp_Object)RemHead((struct List *)rl);
    if(rm != 0) {
	rl->lrl_Count--;
	return(rm);
    } else {
    	return Qnil;
    }
}

void remove_rexx_msg(struct LispRexxList *rl, Lisp_Object rm)
{
    Remove((struct Node *)XPNTR(rm));
    rl->lrl_Count--;
}


/* find a rexx message on a list given an msgid (ptr) */
int is_rexx_msgid_on_list(struct LispRexxList *rl,
				    Lisp_Object id)
{
    struct MinNode *mn = rl->lrl_List.mlh_Head;
    for(mn; mn->mln_Succ; mn = mn->mln_Succ) {
	Lisp_Object cmpid = Qnil;
	XSETLRXMSG(cmpid,mn);
	if( EQ(cmpid,id)) {
	    return(1);
	}
    }
    return(0);
}

Lisp_Object find_rexx_msg_on_list(struct LispRexxList *rl,
				  struct RexxMsg *msg)
{
    Lisp_Object pnt = Qnil;
    struct MinNode *mn = rl->lrl_List.mlh_Head;
    for(mn; mn->mln_Succ; mn = mn->mln_Succ) {
	if( ((struct LispRexxMsg *)mn)->lrm_Msg == msg) {
	    XSETLRXMSG(pnt,mn);
	}
    }
    return(pnt);
}

/* This function is given a RexxMsg and it goes and find (or doesn't) the */
/* corisponding pending message, removes it from the list and sets up the lisp */
/* list for return values.  if it is not found nil is returned. (it deals with */
/* the errors for incoming messages properly.  Some thought is needed on how to */
/* handle errors from replied sent commands that were not asking for results. */
Lisp_Object handle_rexx_msg_replied(struct RexxMsg *msg)
{
    Lisp_Object rm = find_rexx_msg_on_list(&pending, msg);
    if(!NULL(rm)) {
	/* Process the command.  If it was requesting results strings handle */
	/* them otherwise just delete. */
	struct LispRexxMsg *lrm = XLRXMSG(rm);
	remove_rexx_msg(&pending, rm);

	if(msg->rm_Result1 == 0) {
	    if(lrm->lrm_Flags & LRMF_DORESULTS) {
		/* add to returned so that result can be fetched. */
		add_rexx_msg_to_tail(&returned,rm);
	    } else {
		/* simply delete rexx message. */
		DeleteARexxMsg(handle,msg);
		free_rexx_msg(rm);
	    }
	} else {
	    /* an error occured with our message. */
	    if(lrm->lrm_Flags & LRMF_DOERRORS) {
		/* add to returned so that error can be fetched. */
		add_rexx_msg_to_tail(&returned,rm);
	    } else {
		/* simply delete rexx message. */
		DeleteARexxMsg(handle,msg);
		free_rexx_msg(rm);
	    }
	}
    } else {
	/* This should never happen we received a rexx message reply */
	/* that we never sent out. */
	DeleteARexxMsg(handle,msg);
    }
}

/* This function takes incoming messages and place them on the incoming msg */
/* list.  */
Lisp_Object handle_rexx_msg_received(struct RexxMsg *msg)
{
    Lisp_Object rm = alloc_rexx_msg(LRMF_DORESULTS|LRMF_DOERRORS);
    if(!NULL(rm)) {
	/* Add message to incoming list. */
	struct LispRexxMsg *lrm = XLRXMSG(rm);
	lrm->lrm_Msg = msg;			  /* set msg pointer. */
	add_rexx_msg_to_tail(&incoming,rm);
    } else {
	/* This should never happen we received a rexx message but ran out of */
	/* memory.  Set last error msg. and reply with fail. */
	SetARexxLastError(handle, msg, "Out of emacs memory.");
	ReplyARexxMsg(handle, msg, 0, 20);
    }
}

/* Almost the same as old one, but we now call handle_pending_arexx_reply() for */
/* replied messages that we sent, so that we can setup result strings and such. */
int check_arexx(int force, int kbd)
{
    struct RexxMsg *msg;
    int msg_received = FALSE;
    while (msg = GetARexxMsg(handle)) {
	msg_received = TRUE;
	if(msg->rm_Node.mn_Node.ln_Type == NT_REPLYMSG)	{
	    /* This is a reply to a rexx command we send out. */
	    handle_rexx_msg_replied(msg);
	} else {
	    handle_rexx_msg_received(msg);
	}
    }
    if ((kbd && amiga_arexx_initialized)) {
	/* if we got a message or we have some out, or we have some waiting to */
	/* be processes then enque the Key sequence that will call the rexx */
	/* message handler.  We obviously don't do this for returned commands :^) */
	if ((msg_received || force && incoming.lrl_Count > 0) &&
	    get_ttycount() == 0) {
	    enque(AMIGASEQ, FALSE); enque('X', FALSE);
	}
    }
    return msg_received;
}

DEFUN ("amiga-arexx-wait", Famiga_arexx_wait, Samiga_arexx_wait, 0, 0, 0,
       "Wait for an ARexx event (command or reply) before proceeding.")
    ()
{
    while (!check_arexx(FALSE, FALSE)) Wait(ARexxSignal(handle));
}

DEFUN ("amiga-arexx-check-command",
       Famiga_arexx_check_command, Samiga_arexx_check_command, 1, 1, 0,
       "Return t if command ID has finished, nil otherwise.")
    (id)
Lisp_Object id;
{
    CHECK_NUMBER(id,0);

    if(is_rexx_msgid_on_list(&pending,id)) {
	/* still on pending return false. */
	return Qnil;
    } else if(is_rexx_msgid_on_list(&returned,id)) {
	/* is waiting to be processed return true. */
	return Qt;
    }

    /* is nowhere to be found. error. */
    error("id not found.");
    return Qnil;
}

DEFUN ("amiga-arexx-get-next-msg", Famiga_arexx_get_next_msg,
       Samiga_get_next_msg, 0, 0, 0,
"Returns the oldest arexx msg sent to emacs rexx port.\n\
When you are through with this message call (amiga-arexx-reply).\n\
if the msg is not replied this function will continue to\n\
return that msg until it has been replied to.")
  ()
{
    struct RexxMsg *rmsg;

    check_arexx(FALSE, FALSE);
    if (incoming.lrl_Count) {
	struct RexxMsg *msg = ((struct LispRexxMsg *)
			       incoming.lrl_List.mlh_Head)->lrm_Msg;
	return build_string(ARG0(msg));
    }
    /* nothing to be gotten. */
    return Qnil;
}

DEFUN("amiga-arexx-get-msg-results", Famiga_arexx_get_msg_results,
      Samiga_arexx_get_msg_results, 1,1,0,
"Returns the results from MSGID. will be a list of the form:\n\
  (msgid resultcode secondary)\n\n\
If resultcode is 0 then secondary will be a string or nil.\n\
else resulcode will be greater than 0 and secondary will be\n\
an error-code (int).\n\n\
If MSGID has not yet completed nil is returned.\n\
if MSGID has been dealt with or is invalid and error will occur.")
    (msgid)
Lisp_Object msgid;
{
    CHECK_NUMBER(msgid,0);

    if(is_rexx_msgid_on_list(&returned,msgid)) {
	/* msgid has completed build list and delete LispRexxMsg. */
	struct LispRexxMsg *lrm = XLRXMSG(msgid);
	Lisp_Object rc, error_or_string, ret;
	struct RexxMsg *msg = lrm->lrm_Msg;

	remove_rexx_msg(&returned,msgid);

	rc = make_number(msg->rm_Result1);
	if(msg->rm_Result1 == 0) {
	    error_or_string = msg->rm_Result2 ? build_string(msg->rm_Result2) : 0;
	} else {
	    /* error occurred */
	    error_or_string = make_number(msg->rm_Result2); /* save error code. */
	}
	free_rexx_msg(msgid);			  /* free our rexx msg. */
	DeleteARexxMsg(handle,msg);		  /* free ARexx msg proper */

	/* build lisp list. */
	ret = Fcons( msgid, Fcons( rc, Fcons(error_or_string, Qnil)));
	if(NULL(ret)) {
	    error("Couldn't get memory.");
	}
	return(ret);
    } else if(is_rexx_msgid_on_list(&pending,msgid)) {
	return Qnil;				  /* this msgid has not yet completed. */
    } else {
	error("Unknown MSGID.");
	return Qnil;
    }
}

DEFUN ("amiga-arexx-reply", Famiga_arexx_reply, Samiga_arexx_reply,
       2, 2, 0,
"Replies to the first arexx message (the one got via amiga-arexx-get-event)\n\
with RC as return code.\n\
If RC=0, TEXT is the result, otherwise it is the error text. It can be nil.")
    (rc, text)
Lisp_Object rc, text;
{
    int retcode, ok = TRUE;
    char *result;
    struct RexxMsg *rmsg;
    Lisp_Object rm = remove_rexx_msg_from_head(&incoming);
    struct LispRexxMsg *lrm = XLRXMSG(rm);

    if (NULL(rm))
	error("No ARexx message to reply to.");

    rmsg = lrm->lrm_Msg;

    CHECK_NUMBER(rc, 0);
    retcode = XINT(rc);

    if (!NULL (text)) {
	CHECK_STRING(text, 0);
	result = XSTRING (text)->data;
    } else {
	result = 0;
    }
    if (retcode && result)
	ok = SetARexxLastError(handle, rmsg, result);
    ReplyARexxMsg(handle, rmsg, result, retcode);

    if (!ok)
	error("Failed to set ARexx error message.");

    return Qnil;
}

Lisp_Object send_rexx_command(Lisp_Object str, Lisp_Object as_file,
			      ULONG flags)
{
    struct RexxMsg *rmsg;
    int i;
    Lisp_Object id, rm;
    struct LispRexxMsg *lrm;

    rm = alloc_rexx_msg(flags);
    if(NULL(rm)) {
	error("Failed to send command to ARexx.");
	return Qnil;
    }

    CHECK_STRING (str, 0);
    if (!(rmsg = SendARexxMsg(handle, XSTRING (str)->data,!NULL (as_file),
			      (flags & LRMF_DORESULTS ? 1 : 0)))) {
	free_rexx_msg(rm);
	error("Failed to send command to ARexx.");
	return Qnil;
    }
    lrm = XLRXMSG(rm);
    lrm->lrm_Msg = rmsg;			  /* set rexx message pointer. */
    add_rexx_msg_to_tail(&pending,rm);		  /* add to pending list. */

    return(rm);
}

DEFUN ("amiga-arexx-send-command", Famiga_arexx_send_command,
       Samiga_arexx_send_command, 1, 2, 0,
"Sends a command to ARexx for execution.\n\
If the second arg is non-nil, the command is directly interpreted.\n\
Returns an integer that uniquely identifies this message.  This must\n\
then be used to get the results from the command.\n\
NOTE: this is very different from old way things worked.\n\
      earlier versions of emacs discarded successful results\n\
      and errors always got replied to becuase they caused failures\n\
      Neither of these are true now.\
This function is also no longer interactive.\n\
Use (amiga-arexx-do-command)\n")
    (str, as_file)
Lisp_Object str, as_file;
{
    return(send_rexx_command(str,as_file,
			     LRMF_DORESULTS|
			     LRMF_DOERRORS|
			     LRMF_SENTCMD));
}

void init_amiga_rexx(void)
{
    extern ULONG inputsig;
    int i;

    handle = InitARexx("Emacs", "elx");
    inputsig |= ARexxSignal(handle);

    /* init exec lists. */
    NewList((struct List *)&incoming.lrl_List);
    incoming.lrl_Count = 0;

    NewList((struct List *)&pending.lrl_List);
    pending.lrl_Count = 0;

    NewList((struct List *)&returned.lrl_List);
    returned.lrl_Count = 0;
}

void cleanup_amiga_rexx(void)
{
    /* Delete and reply all rexx messages we have gotten. */
    Lisp_Object rm = remove_rexx_msg_from_head(&returned);
    while(!NULL(rm)) {
	struct LispRexxMsg *lrm = XLRXMSG(rm);
	DeleteARexxMsg(handle,lrm->lrm_Msg);
	free_rexx_msg(rm);
	rm = remove_rexx_msg_from_head(&returned);
    }

    rm = remove_rexx_msg_from_head(&incoming);
    while(!NULL(rm)) {
	struct LispRexxMsg *lrm = XLRXMSG(rm);
	ReplyARexxMsg(handle, lrm->lrm_Msg, 0, 20);
	free_rexx_msg(rm);
	rm = remove_rexx_msg_from_head(&incoming);
    }

    /* Free the rest of rexx, will wait for pending msgs to return */
    FreeARexx(handle);
}

void syms_of_amiga_rexx(void)
{
    DEFVAR_BOOL ("amiga-arexx-initialized", &amiga_arexx_initialized,
		 "Set this to t when Emacs is ready to respond to ARexx messages.\n"
		 "(ie C-\ X causes all pending ARexx messages to be answered)");
    amiga_arexx_initialized = 0;

    defsubr(&Samiga_arexx_send_command);
    defsubr(&Samiga_arexx_reply);
    defsubr(&Samiga_get_next_msg);
    defsubr(&Samiga_arexx_get_msg_results);
    defsubr(&Samiga_arexx_check_command);
    defsubr(&Samiga_arexx_wait);
}
