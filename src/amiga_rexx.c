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

#include <signal.h>
#include <assert.h>
#include "config.h"
#ifdef AMIGA_REXX
#include "amiga_simplerexx.h"
#include "amiga_defs.h"
#include <proto/exec.h>
#include "lisp.h"
#include "amiga.h"

#include "blockinput.h"
#include "syssignal.h"

/* #undef static */
#define XLRXMSG(x) ((struct LispRexxMsg *) XPNTR((x)))
#define XSETLRXMSG(x,v) (XSETINT (x, 0), XSETPNTR(x, v))
#define LRXMSGP(x) INTEGERP (x)

#ifndef AMIGA_IWIN_SOFTINT
extern long win_softint_fwd_sigbit;
#define AMIGA_AREXX_SIGBIT (win_softint_fwd_sigbit)
#else /* AMIGA_IWIN_SOFTINT */
you lose
#endif /* AMIGA_IWIN_SOFTINT */


typedef void (*exit_fp)(void);
typedef int (*atexit_fp)(exit_fp);
bool Arxp_init_module (atexit_fp);
void Arxp_exit_module (void);

static AREXXCONTEXT handle;
struct MsgPort *WIN_AREXX_PORT;

static int amiga_arexx_initialized;
static bool Arxp_initialized;
static int Arxp_sigblock_count;
static int Arxp_sigmask;

static inline void Arxp_blink () { BLINK; }

#define Arxp_block() ((Arxp_sigblock_count++)				\
		    || (Arxp_blink (),(Arxp_sigmask = sigblock (sigmask (SIGINT)))))
#define Arxp_unblock() ((assert (Arxp_sigblock_count > 0),(--Arxp_sigblock_count)) \
		      || (Arxp_blink (),(sigsetmask (Arxp_sigmask))))
#define Arxp_totally_unblock()						\
 ((Arxp_sigblock_count == 0) 						\
 ||  ((Arxp_sigblock_count = 0),(Arxp_blink ()),(sigsetmask (Arxp_sigmask)),1))

#define error Arxp_totally_unblock(), (error)

/* This structure should be allocated with emacs_malloc() its pointer will be */
/* used as an msgid. (emacs XUINT()) */
struct LispRexxMsg
  {
    struct MinNode lrm_Node;	/* A node for tracking messages. */
    struct RexxMsg *lrm_Msg;	/* The actual Rexx Msg. */
    ULONG lrm_Flags;
  };
/* Flags for LispRexxMessage indicating what to do with it. */
#define LRMF_SENTCMD (1L << 0)	/* this msg originated here. */
#define LRMF_DOERRORS (1L << 1)	/* handle error replies */
#define LRMF_DORESULTS (1L << 2)	/* handle result strings */

struct LispRexxList
  {
    struct MinList lrl_List;
    int lrl_Count;
  };
/* The list of pending  (outgoing) Rexx Messages. */
struct LispRexxList pending;	
/* The list of pending (outgoing) Rexx Messages that have been received. */
struct LispRexxList returned;
/* The message that are incoming to Emacs (sent from some other rexx host.) */
struct LispRexxList incoming;

/* allocate a rexx message properly */
static struct LispRexxMsg *
alloc_rexx_msg (ULONG flags)
{
  struct LispRexxMsg *lrm; /* RESULT */

  lrm  = (struct LispRexxMsg *) malloc (sizeof *lrm);
  if (!lrm)
    return 0;
  lrm->lrm_Flags = flags;
  return lrm;
}

/* free an arexx message allocated with alloc_arexx_msg() */
void 
free_rexx_msg (struct LispRexxMsg * msg)
{
  free (msg);
}

/* The next 2 functions implement FIFO lists. */
#define ARXP_L_PUSH_BACK(LIST, MSG) (add_rexx_msg_to_tail ((LIST), (MSG)))
#define ARXP_L_POP_FRONT(LIST) (void)(remove_rexx_msg_from_head (LIST))
#define ARXP_L_FRONT(LIST) (((struct LispRexxMsg *) (LIST)->lrl_List.mlh_Head))
#define ARXP_L_EMPTY_P(LIST) ((LIST)->lrl_List.mlh_Head->mln_Succ == 0)
#define ARXP_L_FOREACH(lh, i)					\
for ((i)=(void *)((struct MinList *)(lh))->mlh_Head;	\
((struct MinNode *)(i))->mln_Succ; (i)=(void *)((struct MinNode *)(i))->mln_Succ)


/* add LispRexxMsg to a LispRexxLisp Tail. */
static void 
add_rexx_msg_to_tail (struct LispRexxList *rl, struct LispRexxMsg *msg)
{
  local_precond (rl && msg);

  AddTail ((struct List *) rl, (struct Node *) msg);
  ++rl->lrl_Count;
}

/* remove LispRexxMsg from head of a LispRexxLisp. */
static inline struct LispRexxMsg *
remove_rexx_msg_from_head (struct LispRexxList *rl)
{
  void *p;

  local_precond (rl);

  if ((p = RemHead ((struct List *) rl)))
    --rl->lrl_Count;
  return p;
}

static void 
remove_rexx_msg (struct LispRexxList *rl, struct LispRexxMsg * msg)
{
  local_precond (rl && msg);

  Remove ((struct Node *) msg);
  rl->lrl_Count--;
}

static inline struct LispRexxMsg *
find_rexx_msg_on_list (struct LispRexxList * rl, struct RexxMsg * msg)
{
  struct LispRexxMsg *p;	/* RESULT */

  local_precond (rl && msg);

  ARXP_L_FOREACH (rl, p)
    if (p->lrm_Msg == msg)
      return p;
  return 0;
}

/* find a rexx message on a list given an msgid (ptr) */
static bool
is_rexx_msgid_on_list (struct LispRexxList *rl, Lisp_Object id)
{
  local_precond (rl && LRXMSGP (id));

  return find_rexx_msg_on_list (rl, XLRXMSG (id)->lrm_Msg) != 0;
}


/* This function is given a RexxMsg and it goes and find (or doesn't) the */
/* corisponding pending message, removes it from the list and sets up the lisp */
/* list for return values.  if it is not found nil is returned. (it deals with */
/* the errors for incoming messages properly.  Some thought is needed on how to */
/* handle errors from replied sent commands that were not asking for results. */
static Lisp_Object 
handle_rexx_msg_replied (struct RexxMsg * msg)
{
  struct LispRexxMsg *lrm;

  local_precond (msg);

  lrm = find_rexx_msg_on_list (&pending, msg);
  if (lrm)
    {
      /* Process the command.  If it was requesting results strings handle */
      /* them otherwise just delete. */
      remove_rexx_msg (&pending, lrm);

      if (msg->rm_Result1 == 0)
	{
	  if (lrm->lrm_Flags & LRMF_DORESULTS)
	    {
	      /* add to returned so that result can be fetched. */
	      add_rexx_msg_to_tail (&returned, lrm);
	    }
	  else
	    {
	      /* simply delete rexx message. */
	      DeleteARexxMsg (handle, msg);
	      free_rexx_msg (lrm);
	    }
	}
      else
	{
	  /* an error occured with our message. */
	  if (lrm->lrm_Flags & LRMF_DOERRORS)
	    {
	      /* add to returned so that error can be fetched. */
	      add_rexx_msg_to_tail (&returned, lrm);
	    }
	  else
	    {
	      /* simply delete rexx message. */
	      DeleteARexxMsg (handle, msg);
	      free_rexx_msg (lrm);
	    }
	}
    }
  else
    {
      /* This should never happen we received a rexx message reply */
      /* that we never sent out. */
      DeleteARexxMsg (handle, msg);
    }
}

/* This function takes incoming messages and place them on the
   incoming msg list.  */
static Lisp_Object 
handle_rexx_msg_received (struct RexxMsg *msg)
{
  struct LispRexxMsg *lrm;

  local_precond (msg);

  lrm = alloc_rexx_msg (LRMF_DORESULTS | LRMF_DOERRORS);
  if (lrm)
    {
      /* Add message to incoming list. */
      lrm->lrm_Msg = msg;	/* set msg pointer. */
      add_rexx_msg_to_tail (&incoming, lrm);
    }
  else
    {
      /* This should never happen we received a rexx message but ran out of */
      /* memory.  Set last error msg. and reply with fail. */
      SetARexxLastError (handle, msg, "Out of emacs memory.");
      ReplyARexxMsg (handle, msg, 0, 20);
    }
}

/* Almost the same as old one, but we now call
   handle_pending_arexx_reply() for replied messages that we sent, so
   that we can setup result strings and such.

   Both parameters FORCE and KBD parameter are obsolete now.  The
   returned value is now `2' if we have received at least one command,
   '1' if we haven't received any command but at least one reply and
   '0' if we have received nothing. -bw/19-Jun-98 */
int 
check_arexx ()
{
  bool got_reply = false, got_command = false;
  struct RexxMsg *msg;

  BLOCK_INPUT;
  Arxp_block ();

  /* move incoming messages into on of reply or command list */
  while (msg = GetARexxMsg (handle))
    if (msg->rm_Node.mn_Node.ln_Type == NT_REPLYMSG)
      (got_reply = true), handle_rexx_msg_replied (msg);
    else
      (got_command = true), handle_rexx_msg_received (msg);

  Arxp_unblock ();
  UNBLOCK_INPUT;

  return ((got_command) ? 2 
	  : ((got_reply) ? 1 
	     : 0));
}

DEFUN ("amiga-arexx-wait", Famiga_arexx_wait, Samiga_arexx_wait, 0, 0, 0,
       "Wait for an ARexx event (command or reply) before proceeding.")
()
{
  BLOCK_INPUT;
  Arxp_block ();

  if (AMIGA_AREXX_SIGBIT != -1)
    while (!check_arexx ())
      Wait (1 << AMIGA_AREXX_SIGBIT);

  Arxp_unblock ();
  UNBLOCK_INPUT;

  return Qnil;
}

DEFUN ("amiga-arexx-check-command",
       Famiga_arexx_check_command, Samiga_arexx_check_command, 1, 1, 0,
       "Return t if command ID has finished, nil otherwise.")
(id)
     Lisp_Object id;
{
  Lisp_Object result = Qnil;

  CHECK_NUMBER (id, 0);

  BLOCK_INPUT;
  Arxp_block ();

  if (is_rexx_msgid_on_list (&pending, id))
    ; /* still on pending return false. */
  else if (is_rexx_msgid_on_list (&returned, id))
    /* is waiting to be processed return true. */
    result = Qt;
  else
    {
      /* is nowhere to be found. error. */
      error ("id not found.");
      /* NOTREACHED */
    }

  Arxp_unblock ();
  UNBLOCK_INPUT;
  return result;
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
  Lisp_Object result = Qnil;

  BLOCK_INPUT;
  Arxp_block ();

  check_arexx ();

  if (!ARXP_L_EMPTY_P (&incoming))
    result = build_string (ARG0 (ARXP_L_FRONT (&incoming)->lrm_Msg));

  Arxp_unblock ();
  UNBLOCK_INPUT;
  return result;
}

DEFUN ("amiga-arexx-get-msg-results", Famiga_arexx_get_msg_results,
       Samiga_arexx_get_msg_results, 1, 1, 0,
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
  Lisp_Object result;

  CHECK_NUMBER (msgid, 0);

  BLOCK_INPUT;
  Arxp_block ();
  result = Qnil;

  if (is_rexx_msgid_on_list (&returned, msgid))
    /* msgid has completed. build list and delete LispRexxMsg. */
    {
      struct LispRexxMsg *lrm = XLRXMSG (msgid);
      Lisp_Object rc, error_or_string;
      struct RexxMsg *msg = lrm->lrm_Msg;

      remove_rexx_msg (&returned, lrm);

      rc = make_number (msg->rm_Result1);
      if (msg->rm_Result1 == 0)
	{
	  error_or_string = (msg->rm_Result2 
			     ? build_string ((char *) msg->rm_Result2) 
			     : make_number (0));
	}
      else
	{
	  /* error occurred - save error code. */
	  error_or_string = make_number (msg->rm_Result2);
	}
      free_rexx_msg (lrm);
      DeleteARexxMsg (handle, msg);

      /* build lisp list. */
      result = Fcons (msgid, Fcons (rc, Fcons (error_or_string, Qnil)));
      if (NILP (result))
	{
	  error ("Couldn't get memory.");
	}
    }
  else if (is_rexx_msgid_on_list (&pending, msgid))
    {
      /* this msgid has not yet completed. */
    }
  else
    {
      error ("Unknown MSGID.");
    }

  Arxp_unblock ();
  UNBLOCK_INPUT;
  return result;
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

  CHECK_NUMBER (rc, 0);

  BLOCK_INPUT;
  Arxp_block ();

  if (ARXP_L_EMPTY_P (&incoming))
    {
      error ("No ARexx message to reply to.");
      /* NOTREACHED */
    }

  /* Note: Before ReplyMsg() unlink it from our list */
  (rmsg = ARXP_L_FRONT (&incoming)->lrm_Msg), ARXP_L_POP_FRONT (&incoming);

  retcode = XINT (rc);

  if (!NILP (text))
    {
      CHECK_STRING (text, 0);
      result = XSTRING (text)->data;
    }
  else
    result = 0;

  if (retcode && result)
    ok = SetARexxLastError (handle, rmsg, result);
  ReplyARexxMsg (handle, rmsg, result, retcode);

  if (!ok)
    {
      error ("Failed to set ARexx error message.");
      /* NOTREACHED */
    }

  Arxp_unblock ();
  UNBLOCK_INPUT;
  return Qnil;
}

/* Reply all incoming ARexx messages with (rc=20 ; result="quit") */
/* Called by Fsignal() which may throw away our ARexx messages.
   ???-bw/09-Mar-99: What about recursive edit?  */
void
Arxp_handle_quit ()
{
  BLOCK_INPUT;
  Arxp_block ();

  while (!ARXP_L_EMPTY_P (&incoming))
    {
      int return_code = 20;
      char *result = "quit";
      struct RexxMsg *rmsg;

      /* Note: Before ReplyMsg() unlink it from our list */
      (rmsg = ARXP_L_FRONT (&incoming)->lrm_Msg), ARXP_L_POP_FRONT (&incoming);
      (void)SetARexxLastError (handle, rmsg, result);

      ReplyARexxMsg (handle, rmsg, result, return_code);
    }

  Arxp_unblock ();
  UNBLOCK_INPUT;
}

static Lisp_Object 
send_rexx_command (Lisp_Object str, Lisp_Object as_file,
		   ULONG flags)
{
  struct RexxMsg *rmsg;
  int i;
  Lisp_Object id, rm;
  struct LispRexxMsg *lrm;

  lrm = alloc_rexx_msg (flags);
  if (!lrm)
    {
      error ("Failed to send command to ARexx.");
      /* NOTREACHED */
    }

  CHECK_STRING (str, 0);
  rmsg = SendARexxMsg (handle, XSTRING (str)->data, !NILP (as_file),
		       ((flags & LRMF_DORESULTS) ? 1 : 0));
  if (!rmsg)
    {
      free_rexx_msg (lrm);
      error ("Failed to send command to ARexx.");
      /* NOTREACHED */
    }

  lrm->lrm_Msg = rmsg;		/* set rexx message pointer. */
  add_rexx_msg_to_tail (&pending, lrm);	/* add to pending list. */

  XSETLRXMSG (rm, lrm);
  return rm;
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
  Lisp_Object result;

  BLOCK_INPUT;
  Arxp_block ();

  result = send_rexx_command (str, as_file, (LRMF_DORESULTS | LRMF_DOERRORS
					     | LRMF_SENTCMD));
  Arxp_unblock ();
  UNBLOCK_INPUT;
  return result;
}

void 
Arxp_exit_module ()
{
  struct LispRexxMsg *lrm;

  BLOCK_INPUT;
  Arxp_block ();

  WIN_AREXX_PORT = 0;
  amiga_arexx_initialized = 0;
  Arxp_initialized = false;

  /* Delete and reply all rexx messages we have gotten. */
  
  while (!ARXP_L_EMPTY_P (&returned))
    {
      (lrm = ARXP_L_FRONT (&returned)), ARXP_L_POP_FRONT (&returned);
      DeleteARexxMsg (handle, lrm->lrm_Msg);
      free_rexx_msg (lrm);
    }

  while (!ARXP_L_EMPTY_P (&incoming))
    {
      (lrm = ARXP_L_FRONT (&incoming)), ARXP_L_POP_FRONT (&incoming);
      ReplyARexxMsg (handle, lrm->lrm_Msg, 0, 20);
      free_rexx_msg (lrm);
    }

  /* Free the rest of rexx, will wait for pending msgs to return */
  FreeARexx (handle);

  Arxp_unblock ();
  UNBLOCK_INPUT;
}

bool
Arxp_init_module (atexit_fp reg)
{
  int i;

  BLOCK_INPUT;
  Arxp_block ();

  check_precond (!Arxp_initialized);

  handle = InitARexx ("EMACS", "elx");
  if (!handle)
    return false;

  /* init exec lists. */
  NewList ((struct List *) &incoming.lrl_List);
  incoming.lrl_Count = 0;

  NewList ((struct List *) &pending.lrl_List);
  pending.lrl_Count = 0;

  NewList ((struct List *) &returned.lrl_List);
  returned.lrl_Count = 0;

  if (!reg || (*reg) (Arxp_exit_module) != -1)
    {
      Arxp_initialized = true;
      amiga_arexx_initialized = 1;
      WIN_AREXX_PORT = ARexxPort (handle);
      Arxp_unblock ();
      UNBLOCK_INPUT;
      return true;
    }

  Arxp_exit_module ();

  Arxp_unblock ();
  UNBLOCK_INPUT;
  return false;
}

void 
syms_of_amiga_rexx (void)
{
  DEFVAR_BOOL ("amiga-arexx-initialized", &amiga_arexx_initialized,
	 "Set this to t when Emacs is ready to respond to ARexx messages.\n"
	     "(ie C-\ X causes all pending ARexx messages to be answered)");
  amiga_arexx_initialized = 0;

  defsubr (&Samiga_arexx_send_command);
  defsubr (&Samiga_arexx_reply);
  defsubr (&Samiga_get_next_msg);
  defsubr (&Samiga_arexx_get_msg_results);
  defsubr (&Samiga_arexx_check_command);
  defsubr (&Samiga_arexx_wait);
}



#ifdef AMIGA_DUMP
Admp_tbl (amiga_rexx) = {
  Admp_ndobj (handle),
  Admp_ndobj (pending),
  Admp_ndobj (returned),
  Admp_ndobj (incoming),
  0};
#endif

#endif /* AMIGA_REXX */
