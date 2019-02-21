#include "config.h"
#ifdef AMIGA_REXX
#define AMIGA_REXX_SOFTINT

/*
 * Simple ARexx interface by Michael Sinz
 *
 * This is a very "Simple" interface to the world of ARexx...
 * For more complex interfaces into ARexx, it is best that you
 * understand the functions that are provided by ARexx.
 * In many cases they are more powerful than what is presented
 * here.
 *
 * This code is fully re-entrant and self-contained other than
 * the use of SysBase/AbsExecBase and the ARexx RVI support
 * library which is also self-contained...
 */

#include	"amiga_simplerexx.h"

#ifdef emacs
#include "amiga_defs.h"
#include "amiga.h"
#endif

#include <exec/types.h>
#include <exec/nodes.h>
#include <exec/lists.h>
#include <exec/ports.h>
#include <exec/interrupts.h>
#include <exec/memory.h>
#include <rexx/storage.h>
#include <rexx/rxslib.h>

#include <proto/exec.h>
#include <proto/alib.h>

#define __NOLIBBASE__
#define REXXSYSLIB_BASE_NAME rxh->rexxSysBase	/* use the local library base [bw] */

#include <proto/rexxsyslib.h>

#include <string.h>
#include <ctype.h>


/*
 * A structure for the ARexx handler context
 * This is *VERY* *PRIVATE* and should not be touched...
 */
struct ARexxContext
  {
    struct MsgPort *ARexxPort;	/* The port messages come in at... */
    struct RxsLib *rexxSysBase;	/* We will hide the library pointer here... */
    long Outstanding;		/* The count of outstanding ARexx messages... */
    char PortName[24];		/* The port name goes here... */
    char ErrorName[28];		/* The name of the <base>.LASTERROR... */
    char Extension[8];		/* Default file name extension... */
  };



#define	AREXXCONTEXT	struct ARexxContext *

#ifdef AMIGA_REXX_SOFTINT
void __interrupt __saveds
Arxp_softint_handler (struct MsgPort *port __asm("a1"))
{
#ifdef AMIGA_HAVE_INTUI
  extern long win_softint_fwd_sigbit;
  extern struct Task *win_proc;
  extern int interrupt_input_pending;

  if (!win_proc) /* TODO-bw/10-Mar-98: REXX should also work with console.  */
    return;

  /* wake up the possible sleeping Emacs (ix_select()) */
  Signal ((struct Task*) win_proc, (1<<win_softint_fwd_sigbit));
  /* announce the pending input to the non sleeping Emacs */
  interrupt_input_pending = 1;
#endif
}
#endif /* AMIGA_REXX_SOFTINT */
  
/*
 * This function returns the port name of your ARexx port.
 * It will return NULL if there is no ARexx port...
 *
 * This string is *READ ONLY*  You *MUST NOT* modify it...
 */
char const *
ARexxName (AREXXCONTEXT rxh)
{
  check_precond (rxh);
  return rxh->PortName;
}

/*
 * This function returns the signal mask that the Rexx port is
 * using.  It returns NULL if there is no signal...
 *
 * Use this signal bit in your Wait() loop...
 */
ULONG 
ARexxSignal (AREXXCONTEXT rxh)
{
  check_precond (rxh);
  return rxh->ARexxPort->mp_SigBit;
}

struct MsgPort *
ARexxPort (AREXXCONTEXT rxh)
{
  check_precond (rxh);
  return rxh->ARexxPort;
}

/*
 * This function returns a structure that contains the commands sent from
 * ARexx or the results of commands you sent.  You will need to parse it 
 * and return the structure back so that the memory can be freed.
 *
 * This returns NULL if there was no message.
 */
struct RexxMsg *
GetARexxMsg (AREXXCONTEXT rxh)
{
  check_precond (rxh);
  return (struct RexxMsg *) GetMsg (rxh->ARexxPort);
}

/* Use this to delete a message sent via SendARexxMsg and that has now been
   returned to you.
 */
void 
DeleteARexxMsg (AREXXCONTEXT rxh, struct RexxMsg *rmsg)
{
  check_precond (rxh && rmsg);

  /*
   * Free the arguments and the message...
   */
  if ((rmsg->rm_Action & RXFF_RESULT) 
      && rmsg->rm_Result1 == 0 && rmsg->rm_Result2)
    DeleteArgstring ((STRPTR) rmsg->rm_Result2);

  DeleteArgstring (rmsg->rm_Args[0]);
  DeleteRexxMsg (rmsg);
  --rxh->Outstanding;
}

/*
 * Use this to return a ARexx message...
 *
 * If you wish to return something, it must be in the RString.
 * If you wish to return an Error, it must be in the Error.
 * If there is an error, the RString is ignored.
 */
void 
ReplyARexxMsg (AREXXCONTEXT rxh, struct RexxMsg *rmsg,
	       char *RString, LONG Error)
{
  check_precond (rxh);

  if (rmsg && rmsg != REXX_RETURN_ERROR)
    {
      rmsg->rm_Result2 = 0;
      if (!(rmsg->rm_Result1 = Error))
	{
	  /*
	   * if you did not have an error we return the string
	   */
	  if (rmsg->rm_Action & (1L << RXFB_RESULT))
	    if (RString)
	      {
		rmsg->rm_Result2 = (LONG) CreateArgstring (RString,
						   (LONG) strlen (RString));
	      }
	}
      /*
       * Reply the message to ARexx...
       */
      ReplyMsg ((struct Message *) rmsg);
    }
}

/*
 * This function will set an error string for the ARexx
 * application in the variable defined as <appname>.LASTERROR
 *
 * Note that this can only happen if there is an ARexx message...
 *
 * This returns TRUE if it worked, FALSE if it did not...
 */
short 
SetARexxLastError (AREXXCONTEXT rxh,
		   struct RexxMsg *rmsg,
		   char *ErrorString)
{
  check_precond (rxh);
  /*
   * Note that SetRexxVar() has more than just a TRUE/FALSE
   * return code, but for this "basic" case, we just care if
   * it works or not.
   */
  if (rmsg
      && IsRexxMsg (rmsg)	/* was CheckRexxMsg(alib) [bw] */
      && !SetRexxVar ((struct Message *) rmsg, rxh->ErrorName,
		      ErrorString, strlen (ErrorString)))
    return TRUE;
  else
    return FALSE;
}

/*
 * This function will send a string to ARexx...
 *
 * The default host port will be that of your task...
 *
 * If you set StringFile to TRUE, it will set that bit for the message...
 *
 * Returns the message sent, or NULL in case of error.
 */
struct RexxMsg *
SendARexxMsg (AREXXCONTEXT rxh, char *RString,
	      short StringFile, short results)
{
  register struct MsgPort *RexxPort;
  register struct RexxMsg *rmsg;
  register short flag = FALSE;

  check_precond (rxh);

  if (RString && (rmsg = CreateRexxMsg (rxh->ARexxPort,
					       rxh->Extension,
					       rxh->PortName)))
    {
      rmsg->rm_Action = RXCOMM | (StringFile ?
				  (1L << RXFB_STRING) : 0);
      rmsg->rm_Action |= (results ? RXFF_RESULT : 0);
      if (rmsg->rm_Args[0] = CreateArgstring (RString,
					      (LONG) strlen (RString)))
	{
	  /*
	   * We need to find the RexxPort and this needs
	   * to be done in a Forbid()
	   */
	  Forbid ();
	  if (RexxPort = FindPort (RXSDIR))
	    {
	      /*
	       * We found the port, so put the
	       * message to ARexx...
	       */
	      PutMsg (RexxPort, (struct Message *) rmsg);
	      rxh->Outstanding += 1;
	      flag = TRUE;
	    }
	  else
	    {
	      /*
	       * No port, so clean up...
	       */
	      DeleteArgstring (rmsg->rm_Args[0]);
	      DeleteRexxMsg (rmsg);
	    }
	  Permit ();
	}
      else
	DeleteRexxMsg (rmsg);
    }
  return flag ? rmsg : NULL;
}

int 
PendingCommands (AREXXCONTEXT rxh)
{
  check_precond (rxh);
  return rxh->Outstanding;
}

/*
 * This function closes down the ARexx context that was opened
 * with InitARexx...
 */
void 
FreeARexx (AREXXCONTEXT rxh)
{
  register struct RexxMsg *rmsg;

  if (!rxh)
    return;

  /*
   * Clear port name so it can't be found...
   */
  rxh->PortName[0] = '\0';

  /*
   * Clean out any outstanding messages we had sent out...
   */
  while (rxh->Outstanding)
    {
      WaitPort (rxh->ARexxPort);
      while (rmsg = GetARexxMsg (rxh))
	{
	  if (rmsg != REXX_RETURN_ERROR)
	    {
	      /*
	       * Any messages that come now are blown
	       * away...
	       */
	      SetARexxLastError (rxh, rmsg, "99: Port Closed!");
	      /* removed ReplyARexxMsg() this was a bug that would */
	      /* obviously cause a loop (we would continue to reply and */
	      /* then get the message.  It now deletes the message as */
	      /* should be done.  -ch5/10/93. */
	      DeleteARexxMsg (rxh, rmsg);
	    }
	}
    }

  /*
   * Clean up the port and delete it...
   */
  if (rxh->ARexxPort)
    {
      while (rmsg = GetARexxMsg (rxh))
	{
	  /*
	   * Any messages that still are coming in are
	   * "dead"  We just set the LASTERROR and
	   * reply an error of 100...
	   */
	  SetARexxLastError (rxh, rmsg,
			     "99: Port Closed!");
	  ReplyARexxMsg (rxh, rmsg, NULL, 100);
	}
      RemPort (rxh->ARexxPort);
#ifndef AMIGA_REXX_SOFTINT
      /* cleanup */
      DeleteMsgPort (rxh->ARexxPort);
#else
      Asys_destroy_softint (rxh->ARexxPort);
#endif
    }
  CloseLibrary ((struct Library *) rxh->rexxSysBase);
  FreeMem (rxh, sizeof (struct ARexxContext));
}

/*
 * This routine initializes an ARexx port for your process
 * This should only be done once per process.  You must call it
 * with a valid application name and you must use the handle it
 * returns in all other calls...
 *
 * NOTE:  The AppName should not have spaces in it...
 *        Example AppNames:  "MyWord" or "FastCalc" etc...
 *        The name *MUST* be less that 16 characters...
 *        If it is not, it will be trimmed...
 *        The name will also be UPPER-CASED...
 *
 * NOTE:  The Default file name extension, if NULL will be
 *        "rexx"  (the "." is automatic)
 */
AREXXCONTEXT
InitARexx (char *baseName, char *suffix)
{
  AREXXCONTEXT r = 0;
  unsigned i = 0, tmp = 0;

  if (r = AllocMem (sizeof (struct ARexxContext), MEMF_PUBLIC | MEMF_CLEAR))
    {
      if ((r->rexxSysBase = (struct RxsLib *) OpenLibrary ("rexxsyslib.library", 36)))
	{
	  if (r->ARexxPort = 
#ifndef AMIGA_REXX_SOFTINT
	      CreateMsgPort ()
#else
	      Asys_create_softint (&Arxp_softint_handler)
#endif
	      )
	    {
	      strncpy (r->Extension, (suffix ? suffix : "rexx"), 7)[7] = '\0';
	      strncpy (r->PortName, baseName, 16)[16] = '\0';
	      strcat (strcpy (r->ErrorName, r->PortName), ".LASTERROR");
	      r->ARexxPort->mp_Node.ln_Name = r->PortName;
	      r->ARexxPort->mp_Node.ln_Pri = 0;
	      tmp = strlen (r->PortName);
	      r->PortName[tmp + 1] = '\0';

	      Forbid ();
	      for (i = '1'; i <= '9'; ++i)
		{
		  r->PortName[tmp] = i;
		  if (!FindPort (r->PortName))
		    {
		      AddPort (r->ARexxPort);
		      Permit ();
		      return r;	/* successful return */
		    }
		}
	      Permit ();

#ifndef AMIGA_REXX_SOFTINT
	      /* cleanup */
	      DeleteMsgPort (r->ARexxPort);
#else
	      Asys_destroy_softint (r->ARexxPort);
#endif
	    }
	  CloseLibrary ((struct Library *) r->rexxSysBase);
	}
      FreeMem (r, sizeof (struct ARexxContext));
    }
  return (0);
}

#endif /* AMIGA_REXX */
