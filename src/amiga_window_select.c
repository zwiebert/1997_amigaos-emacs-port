/* amiga_window_select.c - Amiga-OS clipboard support for GNU-Emacs 
  Author: ???

  Note: The code is taken from GNU-Emacs-18.59 by David Gay.  I made
  only few, mainly cosmetic, changes. -bw/09-Mar-98
  */

/* The rest of the file is within this conditional.  */
#include "config.h"

#ifdef AMIGA_IWIN_CLIPBOARD

#include "amiga_window_.h"
#include "amiga_window_defs.h"

#undef LONGBITS

#include <exec/types.h>
#include <exec/io.h>
#include <devices/clipboard.h>
#include <libraries/iffparse.h>
#include <utility/hooks.h>

#define ID_FTXT  MAKE_ID ('F','T','X','T')
#define ID_CHRS  MAKE_ID ('C','H','R','S')

void Awsl_exit_module (void);
bool Awsl_init_module (void);
Lisp_Object Famiga_set_clipboard_data (), Famiga_get_clipboard_data ();

static ULONG __saveds __interrupt clip_change ();
static Lisp_Object clip_unwind (Lisp_Object dummy);
static int clip_protect (void);
static long clip_check (long err);
static void cut (char *str, int size);

/*
 * Text error messages for possible IFFERR_#? returns from various
 * IFF routines.  To get the index into this array, take your IFFERR code,
 * negate it, and subtract one.
 *  idx = -error - 1;
 */
static char *ifferrormsgs[] =
{
  "End of file (not an error).",
  "End of context (not an error).",
  "No lexical scope.",
  "Insufficient memory.",
  "Stream read error.",
  "Stream write error.",
  "Stream seek error.",
  "File is corrupt.",
  "IFF syntax error.",
  "Not an IFF file.",
  "Required call-back hook missing.",
  "Return to client.  You should never see this."
};

Lisp_Object amiga_new_clip;

struct
  {
    struct IFFHandle *iff;
    struct IOClipReq *io;
  }
a_wsl;


struct Hook awsl_hook = 
{
  {0,}, &clip_change, 0, 0
};

static ULONG __saveds __interrupt
clip_change (struct Hook *hook __asm ("a0"),
	     APTR object __asm ("a2"),
	     ULONG * message __asm ("a1"))
{
  amiga_new_clip = 1;
  return 0;
}

static Lisp_Object
clip_unwind (Lisp_Object dummy)
{
  CloseIFF (a_wsl.iff);
  CloseClipboard ((struct ClipboardHandle *) a_wsl.iff->iff_Stream);
  return Qnil;
}

static int
clip_protect ()
{
  int count;

  count = specpdl_ptr - specpdl;
  record_unwind_protect (clip_unwind, Qnil);
  return count;
}

static long
clip_check (long err)
{
  if (!err)
    return err;

  error ("Clipboard IO failed, error %ld: %s\n", err, ifferrormsgs[-err - 1]);
  /* NOTREACHED */
  abort ();
}


static void
cut (char *str, int size)
{
  int count;

  local_precond (str && size >= 0);

  if (!(a_wsl.iff->iff_Stream = (ULONG) OpenClipboard (0)))
    error ("Clipboard open failed.");

  count = clip_protect ();

  /* Open clipbaord */
  InitIFFasClip (a_wsl.iff);
  clip_check (OpenIFF (a_wsl.iff, IFFF_WRITE));

  /* Write data */
  clip_check (PushChunk (a_wsl.iff, ID_FTXT, ID_FORM, IFFSIZE_UNKNOWN));
  clip_check (PushChunk (a_wsl.iff, 0, ID_CHRS, IFFSIZE_UNKNOWN));
  if (WriteChunkBytes (a_wsl.iff, str, size) != size)
    clip_check (IFFERR_WRITE);
  clip_check (PopChunk (a_wsl.iff));
  clip_check (PopChunk (a_wsl.iff));

  /* & close */
  unbind_to (count, Qnil);
}

DEFUN ("amiga-set-clipboard-data", Famiga_set_clipboard_data, Samiga_set_clipboard_data,
       1, 1, 0,
       "Copy string into Amiga clipboard.")
(arg)
     Lisp_Object arg;
{
  struct Lisp_String *p;

  CHECK_STRING (arg, 0);

  p = XSTRING (arg);

  BLOCK_INPUT;
  cut (p->data, p->size);
  UNBLOCK_INPUT;

  return Qnil;
}

DEFUN ("amiga-get-clipboard-data", Famiga_get_clipboard_data, Samiga_get_clipboard_data,
       0, 0, 0,
  "Returns text currently in the Amiga clipboard, or NIL if there is none.")
()
{
  long err = 0;
  Lisp_Object result = Qnil;
  struct ContextNode *cn;
  int count;

  BLOCK_INPUT; /* XXX-bw/09-Mar-98: CHECKME */

  if (!(a_wsl.iff->iff_Stream = (ULONG) OpenClipboard (0)))
    error ("Clipboard open failed.");

  count = clip_protect ();

  /* Open clipbaord */
  InitIFFasClip (a_wsl.iff);
  clip_check (OpenIFF (a_wsl.iff, IFFF_READ));
  clip_check (StopChunk (a_wsl.iff, ID_FTXT, ID_CHRS));

  /* Find the first FTXT CHRS chunks */
  while (result == Qnil)
    {
      long err = ParseIFF (a_wsl.iff, IFFPARSE_SCAN);

      if (err == IFFERR_EOC)
	continue;		/* enter next context */
      else if (err == IFFERR_EOF)
	break;
      else
	clip_check (err);

      /* We only asked to stop at FTXT CHRS chunks
       * If no error we've hit a stop chunk
       * Read the CHRS chunk data
       */
      cn = CurrentChunk (a_wsl.iff);

      if ((cn) && (cn->cn_Type == ID_FTXT) && (cn->cn_ID == ID_CHRS))
	{
	  int size = cn->cn_Size, rlen;

	  result = make_string ("", size);

	  if ((rlen = ReadChunkBytes (a_wsl.iff, XSTRING (result)->data, size)) != size)
	    if (rlen < 0)
	      clip_check (rlen);
	    else
	      clip_check (IFFERR_EOC);
	}
    }
  unbind_to (count, Qnil);

  UNBLOCK_INPUT;

  return result;
}

void
syms_of_amiga_window_select ()
{
  DEFVAR_BOOL ("amiga-new-clip", &amiga_new_clip,
	    "Set to t every time a new clip is put in the Amiga clipboard");
  amiga_new_clip = 0;

  defsubr (&Samiga_set_clipboard_data);
  defsubr (&Samiga_get_clipboard_data);
}

bool
Awsl_init_module ()
{
  struct MsgPort *mp;

  check_precond (IFFPARSE_BASE_NAME);
  check_precond (!a_wsl.io && !a_wsl.iff);

  if (!(a_wsl.iff = AllocIFF ()))
    ASYSE_SET_MISC ("AllocIFF()", "no memory");
  else if (!(mp = CreateMsgPort ()))
    ASYSE_SET_MISC ("CreateMsgPort()", "");
  else if (!(a_wsl.io = CreateIORequest (mp, sizeof (struct IOClipReq))))
    DeleteMsgPort (mp);
  else if (OpenDevice ("clipboard.device", PRIMARY_CLIP,
		       (struct IORequest *)a_wsl.io, 0))
    ASYSE_SET_MISC ("OpenDevice", "clipboard.device missing !?");
  else
    {
      a_wsl.io->io_Command = CBD_CHANGEHOOK;
      a_wsl.io->io_Length = 1;	/* install */
      a_wsl.io->io_Data = (APTR) & awsl_hook;
      if (!DoIO ((struct IORequest *) a_wsl.io))
	return true;
    }

  Awsl_exit_module ();
  return false;
}

void
Awsl_exit_module ()
{
  if (a_wsl.io)
    {
      a_wsl.io->io_Command = CBD_CHANGEHOOK;
      a_wsl.io->io_Length = 0;	/* remove */
      a_wsl.io->io_Data = (APTR) & awsl_hook;
      DoIO ((struct IORequest *) a_wsl.io);

      CloseDevice ((struct IORequest *) a_wsl.io);
      dispose_ptr (DeleteMsgPort, a_wsl.io->io_Message.mn_ReplyPort);
      dispose_ptr (DeleteIORequest, a_wsl.io);
    }

  dispose_ptr (FreeIFF, a_wsl.iff);
}


#ifdef AMIGA_DUMP
Admp_tbl (amiga_window_select) =
{
  Admp_ndobj (awsl_hook),
  Admp_ndobj (ifferrormsgs),
  Admp_ndobj (a_wsl),
  0
};
#endif

#endif /* AMIGA_IWIN_CLIPBOARD */
