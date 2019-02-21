/* Interfaces to system-dependent kernel and library entries.
   Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY.  No author or distributor
   accepts responsibility to anyone for the consequences of using it
   or for whether it serves any particular purpose or works at all,
   unless he says so in writing.  Refer to the GNU Emacs General Public
   License for full details.

   Everyone is granted permission to copy, modify and redistribute
   GNU Emacs, but only under the conditions described in the
   GNU Emacs General Public License.   A copy of this license is
   supposed to have been given to you along with GNU Emacs so you
   can know your rights and responsibilities.  It should be in a
   file named COPYING.  Among other things, the copyright notice
   and this notice must be preserved on all copies.  */

/* avoid name clash for X11 type `Object' and Intuition `Object' */
#define Object IntuiObject
#include <intuition/classusr.h>

#include <exec/types.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <dos/var.h>
#include <exec/execbase.h>
#include <exec/tasks.h>
#include <utility/tagitem.h>
#include <workbench/startup.h>
#include <workbench/workbench.h>
#include <proto/exec.h>
#include <proto/dos.h>

#include <dos/dosextens.h>
#include <intuition/intuition.h>

#define SEQ_RIS "\x1B\x63"
#define SEQ_ITALIC "\x1B\x5B\x33\x6D"
#define SEQ_BOLD "\x1B\x5B\x31\x6D"
#define SEQ_UNDERSCORE "\x1B\x5B\x34\x6D"


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/resource.h>

/* ixemul library: ix_filehandle */
#include <ix.h>

#undef LONGBITS
#include "config.h"
#include "lisp.h"
#include "syssignal.h"

#ifndef AMIGA_V_XEMACS_20_4
#include "blockinput.h"
#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"
#include "dispextern.h"
#ifdef AMIGA_HAVE_INTUI
#undef Object
#include "xterm.h"
#include "amiga_window_defs.h"
#endif

/* Some defines used by XEmacs. ##exp## XXX-bw/05-Aug-98: They may be
   wrong */
#define string_length(s) ((s)->size)
#define XSTRING_LENGTH(s) string_length (XSTRING (s))
#define string_data(s) ((s)->data + 0)
#define XSTRING_DATA(s) string_data (XSTRING (s))
#define string_byte(s, i) ((s)->data[i] + 0)
#define XSTRING_BYTE(s, i) string_byte (XSTRING (s), i)
#define string_byte_addr(s, i) (&((s)->data[i]))
#define set_string_length(s, len) ((void) ((s)->size = (len)))
#define set_string_data(s, ptr) ((void) ((s)->data = (ptr)))
#define set_string_byte(s, i, c) ((void) ((s)->data[i] = (c)))
#else
/* blocking is needed in Emacs but seems uneeded in in XEmacs (at
   least xmalloc does no blocking of any kind) -bw/05-Aug-98 */
#define BLOCK_INPUT
#define UNBLOCK_INPUT
/* abort may defined to assert by "src/lisp.h" */
#undef abort
/* assert may defined by "src/lisp.h" */
#undef assert
/* this typedefs does exist in Emacs but may not in XEmacs */
typedef int SIGMASKTYPE;
/* map non existing Emacs lisp defines to XEmacs names */
#define INTEGERP INTP
#define XFASTINT XINT
#endif

#include <assert.h>
#include "puresize.h"
#include "amiga.h"
#include "frame.h"

/* export interface */
extern void Aerr_print_msg (char const *fmt,...);
void Asys_exit_module (void);
bool Asys_init_module (void);
/* local interface */
static struct TextFont *Asys_set_font (struct TextFont *textFont);


#define swap_ptr(a,b) ({void *tmp; tmp= (a); (a)=(b); (b)=tmp; })
#define dispose_ptr(close, ptr) ({ if (ptr) { close ((void*) ptr); ptr=0; } })
#define dispose_field(close, ptr, field) \
({ if ((ptr) && ((ptr)->field)) { close ((void*) ((ptr)->field)); ((ptr)->field)=0; } })


/* === data === */

/* Subprocess stack size  (n/i) */
int amiga_process_stack_size;

static struct
{
  struct TextFont *old_console_font;
  const struct TextFont *new_console_font;
  bool exit_flag;
#ifdef SET_EMACS_PRIORITY
  int shellproc_priority;
#endif
  struct Library *diskfont_base;
  struct GfxBase *gfx_base;
  struct Library *icon_base;
  int amiga_iwin;
  char *ficon_tooltypes[2];
  struct DiskObject *ficon_dobj;
  struct DiskObject orig_ficon_dobj;  /* to restore old pointers before FreeDiskObject() */
} a_sys;

#define DISKFONT_BASE_NAME (a_sys.diskfont_base)
#define GRAPHICS_BASE_NAME (a_sys.gfx_base)
#define ICON_BASE_NAME (a_sys.icon_base)

#define __NOLIBBASE__
#include <proto/icon.h>
#include <proto/graphics.h>
#include <proto/diskfont.h>

#define ASYSE_MSG (Asys_errmsgs[Asys_errno])
#define ASYSE_RESOURCE (Asys_err_resouurce)
const char *Asys_err_resource;
#define ASYSE_SET(resource, code) \
({ Asys_errno = code; Asys_err_resource = resource; })
#define ASYSE_SET_MISC(resource, msg) \
({ Asys_errno = ASYSE_MISC; Asys_errmsgs[ASYSE_MISC] = msg; })

int Asys_errno;

const char *Asys_errmsgs [] = {
  "no error",
#define ASYSE_MISC 1
  "",
#define ASYSE_NO_MEMORY 2
  "virtual memory exhausted",
#define ASYSE_NO_SHARED_LIB 3
  "cannot open shared library",
#define ASYSE_NO_FONT 4
  "cannot open font",
#define ASYSE_NO_WINDOW 5
  "cannot open window",
#define ASYSE_DOS 6
  "-- call Fault(dos) to get an real error message! --",
};

/* Flags. */
int amiga_initialized; /* Set after dumping. */
int amiga_create_icons; /* If true, we create icons when saving files */
int selecting;
/* XXX #exp# */
int amiga_expand_path;
int amiga_paths;
int amiga_working_killpg;
int amiga_working_jobctrl;
int amiga_exp_use_usg_jobctrl;
extern int inhibit_window_system;


/* === debug aids === */
/* Assertions: check_postcond() should usual disabled. check_precond()
   shoud enabled if the caller is not trustworthy. */
#define  NO_PRECOND   0
#define  NO_POSTCOND  1
#include "amiga_defs.h"


#define AMIGA_DEFAULT_FICON_TOOL "Ed"
#define FICON_DOBJ a_sys.ficon_dobj

/* === code === */

bool
Asys_init_module ()
{
  SIGMASKTYPE sig_mask;

  sig_mask = sigblock (sigmask (SIGINT));

  if (!(GRAPHICS_BASE_NAME = (void*)OpenLibrary("graphics.library", 37)))
    ASYSE_SET ("graphics.library-37", ASYSE_NO_SHARED_LIB);
  else if (!(ICON_BASE_NAME = (void*)OpenLibrary("icon.library", 37)))
    ASYSE_SET ("icon.library-37", ASYSE_NO_SHARED_LIB);
  else if (!(DISKFONT_BASE_NAME = OpenLibrary("diskfont.library", 37))) 
    ASYSE_SET ("diskfont.library-37", ASYSE_NO_SHARED_LIB);
  else
    {
#ifdef AMIGA_DEFAULT_FICON
      FICON_DOBJ = GetDiskObject (AMIGA_DEFAULT_FICON);
#endif /* AMIGA_DEFAULT_FICON */
      if (FICON_DOBJ)
	a_sys.orig_ficon_dobj = *FICON_DOBJ;
      else if ((FICON_DOBJ = GetDefDiskObject (WBPROJECT)))
	{
	  a_sys.orig_ficon_dobj = *FICON_DOBJ;
	  FICON_DOBJ->do_DefaultTool = AMIGA_DEFAULT_FICON_TOOL;
	  a_sys.ficon_tooltypes[0] = "FILETYPE=TEXT";
	  FICON_DOBJ->do_ToolTypes = a_sys.ficon_tooltypes;
	}
#ifdef SET_EMACS_PRIORITY
      a_sys.shellproc_priority = getpriority (PRIO_PROCESS, 0);
#endif
      sigsetmask (sig_mask);
      return true;
      /* failure */
    }
  Asys_exit_module ();
  sigsetmask (sig_mask);
  return false;
}


void
Asys_exit_module ()
{
  SIGMASKTYPE sig_mask;

  sig_mask = sigblock (sigmask (SIGINT));

  if (a_sys.exit_flag)
    {
      sigsetmask (sig_mask);
      return;
    }

  a_sys.exit_flag = true;

  /* restore old process prioritey */
#ifdef SET_EMACS_PRIORITY
  setpriority (PRIO_PROCESS, 0, a_sys.shellproc_priority);
#endif

  /* Restore old font. */
  /* FIXME-bw: Because this resets the console, it may delete printed
     error messages important for the user to read. */
  if (a_sys.old_console_font)
    {
      struct TextFont *oldFont = Asys_set_font (a_sys.old_console_font);
      if (!oldFont)
	{
	  fprintf (stderr, "debug: %s\n", ASYSE_MSG);
	  CloseFont (a_sys.old_console_font);
	}
      else
	{
	  if (oldFont == a_sys.new_console_font)
	    CloseFont (oldFont);
	  else
	    {
	      if (Asys_set_font (oldFont))
		CloseFont (a_sys.old_console_font);
	      else
		CloseFont (oldFont);
	    }
	}
    }

  if (FICON_DOBJ)
    {
      *FICON_DOBJ = a_sys.orig_ficon_dobj;
      dispose_ptr (FreeDiskObject, FICON_DOBJ);
    }

  dispose_ptr (CloseLibrary, GRAPHICS_BASE_NAME);
  dispose_ptr (CloseLibrary, DISKFONT_BASE_NAME);
  dispose_ptr (CloseLibrary, ICON_BASE_NAME);

  sigsetmask (sig_mask);
}



static struct TextFont *
Asys_set_font (struct TextFont *textFont)
{
  struct MsgPort *conTaskPort;
  struct InfoData *infoData;
  struct Window *win;
  struct TextFont *oldFont = 0; /* RESULT */

  infoData = calloc (sizeof *infoData, 1);
  if (!infoData)
    ASYSE_SET ("infoData", ASYSE_NO_MEMORY);
  else
    {
      if ((long)infoData & 3)	/* needs 4 byte alignment */
	abort ();
      conTaskPort = (struct MsgPort *)
	((struct Process *)FindTask (0))->pr_ConsoleTask;
      if (!conTaskPort)
	ASYSE_SET_MISC ("FindTask(0)", "pr_ConsoleTask is not set");
      else
	{
	  if (!((DoPkt) (conTaskPort, ACTION_DISK_INFO,
			 MKBADDR (infoData), 0, 0, 0, 0)))
	    ASYSE_SET ("DoPkt()", ASYSE_DOS);
	    {
	      win = (struct Window *)infoData->id_VolumeNode;
	      if (!win)
		ASYSE_SET_MISC ("pr_ConsoleTask", "internal error: No window attached");
	      else if (!win->IFont)
		ASYSE_SET_MISC ("id_VolumeNode", "internal error: Window has no IFont");
	      else
		{
		  Forbid ();
		  SetFont (win->RPort, textFont);
		  oldFont = win->IFont;
		  win->IFont = textFont;
		  Permit ();
		  write (1, SEQ_RIS, 2);
		}
	    }
	}
      free (infoData);
    }
  return oldFont;
}

static int
Asys_set_font_by_name (const char *name, int size)
{
  struct TextAttr textAttr;
  struct TextFont *textFont;
  int error = -1;		/* RESULT */

  textAttr.ta_Name = (STRPTR)name;
  textAttr.ta_YSize = size;
  textAttr.ta_Style = FS_NORMAL;
  textAttr.ta_Flags = 0;

  textFont = OpenDiskFont (&textAttr);
  if (!textFont)
    ASYSE_SET ("", ASYSE_NO_FONT);
  else if (FPF_PROPORTIONAL & textFont->tf_Flags)
    ASYSE_SET_MISC ("", "Font is proportional");
  else
    {
      struct TextFont *oldFont;

      error = 0;

      oldFont = Asys_set_font (textFont);
      if (!oldFont)
	CloseFont (textFont);
      else
	{
	  if (a_sys.new_console_font == oldFont)
	    CloseFont (oldFont);
	  else
	    {
	      /* Font has changed from extern */
	      if (a_sys.old_console_font)
		CloseFont (a_sys.old_console_font);
	      a_sys.old_console_font = oldFont;
	    }
	  a_sys.new_console_font = textFont;
	}
    }
  return error;
}

#ifdef AMIGA_HAVE_INTUI
DEFUN ("amiga-load-color-file", Famiga_load_color_file, Samiga_load_color_file,
       1, 1, "fFile: ",
  "Update list of color names from an external file (ie. rgb.txt).\n\
The file should define one named RGB color per line like so:\
  R G B   name\n\
where R,G,B are numbers between 0 and 255 and name is an arbitrary string.")
    (filename)
    Lisp_Object filename;
{
  Lisp_Object result;

  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);
  if (NILP (Ffile_readable_p (filename)))
    error ("Cannot acces color file \"%s\"", XSTRING_DATA (filename));
  if (inhibit_window_system)
    return Qnil;

  BLOCK_INPUT;
  result = AITrgb_txt_prepend (XSTRING_DATA (filename)) ? Qt : Qnil;
  UNBLOCK_INPUT;

  return result;
}

DEFUN ("amiga-popup-file-request", Famiga_popup_file_request,
       Samiga_popup_file_request, 0, 3, 0,
       "Open an ASL File Requester and return a list of file names or nil\n\
Optional parameters DIR, FILE and SAVE. Is SAVE is non nil\n\
a saving  requester will be used")
     (dir, file, save)
     Lisp_Object dir, file, save;
{
  extern Lisp_Object Awin_request_file (const char *dir, const char *file, bool save, struct Window *parent_win);
  const char *c_dir="", *c_file="";
  bool c_save=false;
  struct Window *parent_win = 0;
  extern struct frame *selected_frame;

  if (!NILP (dir))
    {
      CHECK_STRING (dir, 0);
      c_dir = XSTRING_DATA (dir);
    }
  if (!NILP (file))
    {
      CHECK_STRING (file, 0);
      c_file = XSTRING_DATA (file);
    }
  if (!NILP (save))
      c_save = true;

  if (inhibit_window_system)
    return Qnil;

  {
    Lisp_Object result;
    BLOCK_INPUT;
    if (!inhibit_window_system)
      parent_win = FRAME_WIN (selected_frame); /* XXX */
    result = Awin_request_file (c_dir, c_file, c_save, parent_win);
    UNBLOCK_INPUT;
    return result;
  }
}

DEFUN ("amiga-popup-font-request", Famiga_popup_font_request,
       Samiga_popup_font_request, 0, 0, 0,
       "Open an ASL Font Requester and return the value as cons"
       " of font name and font size.") ()
{
  struct Window *parent_win = 0;
  extern Lisp_Object Awin_request_font (struct Window *parent_win);

  if (inhibit_window_system)
    return Qnil;
  else
    {
      Lisp_Object result;
      int old_pa;
      BLOCK_INPUT;
#if 0 /* ASL fontreqs hangs bug filereqs seems to work -bw/31-Jul-98 */
      if (!inhibit_window_system)
	{
	  struct MsgPort *mp;
	  parent_win = FRAME_WIN (selected_frame); /* XXX */
	  mp = parent_win->UserPort;
	  old_pa = (mp->mp_Flags & PF_ACTION);
	  mp->mp_Flags &= ~PF_ACTION;
	  mp->mp_Flags |= PA_SIGNAL;
	  result = Awin_request_font (parent_win);
	  mp->mp_Flags &= ~PF_ACTION;
	  mp->mp_Flags |= old_pa;
	}
      else
#endif
	result = Awin_request_font (0);
      UNBLOCK_INPUT;
      return result;
    }
  return Awin_request_font (0);
}
#endif /* AMIGA_HAVE_INTUI */

#ifdef AMIGA_DUMP
DEFUN ("amiga--data-file", Famiga__data_file, Samiga__data_file, 0, 0, 0,
       "Return the name of data file (aka dump file)")
     ()
{
  const char *name = Admp_data_file_name ();
#if 1
  if (!name)
    error ("No data file name defined");
#endif
  return build_string (name ? name : "");
}
#endif /* AMIGA_DUMP */

#ifndef AMIGA_V_XEMACS_20_4
DEFUN ("amiga-set-font", Famiga_set_font, Samiga_set_font, 2, 2,
       "sFont: \n\
nSize: ",
       "Set font used for window to FONT with given HEIGHT.\n\
The font used must be non-proportional.")
(font, size)
     Lisp_Object font, size;
{
  char *font_name;		/* FONT + ".font" suffix */
  const struct Lisp_String *font_str;
  
  CHECK_STRING (font, 0);
  CHECK_NUMBER (size, 0);


  font_str = XSTRING (font);
  font_name = alloca (font_str->size + sizeof ".font");
  strcpy (font_name, string_data (font_str));
  strcat (font_name, ".font");

  BLOCK_INPUT;
  
  if (Asys_set_font_by_name (font_name, XFASTINT (size)) < 0)
    {
      error ("%s: \"%s\" %d", ASYSE_MSG, string_data (font_str), XFASTINT (size));
      UNBLOCK_INPUT;
      return Qnil;
    }
  UNBLOCK_INPUT;

  kill (getpid (), SIGWINCH);
  /* If the font size has not changed we must redraw the display explicit.  */
  Fredraw_display ();

  return Qt;
}


DEFUN ("amiga-put-icon", Famiga_put_icon, Samiga_put_icon, 2, 2, 0,
       "Create an icon for FILE.\n\
If FORCE is non-nil create it unconditionally, otherwise only if one doesn't exist.\n\
Returns t if an icon was created, nil otherwise.")
(file, force)
     Lisp_Object file, force;
{
  char *fname;
  struct DiskObject *obj;

  CHECK_STRING (file, 0);
  fname = XSTRING_DATA (file);

  if (NILP (force) && (obj = GetDiskObject (fname)))
    {
      /* Icon exists, don't overwrite */
      FreeDiskObject (obj);
      return Qnil;
    }
  if (PutDiskObject (fname, FICON_DOBJ))
    return Qt;
  error ("Icon for %s couldn't be created", fname);
}
#endif /* not AMIGA_V_XEMACS_20_4 */


/* Make data file name available to loadup.el.  This avoids patching
   dump_data() -bw/20-Jan-98 */
Lisp_Object Vamiga_data_file;

void
syms_of_amiga ()
{
#ifndef AMIGA_V_XEMACS_20_4
#ifdef AMIGA_DUMP
  /* obsolet - use function amiga--data-file */
  DEFVAR_LISP ("amiga-data-file", &Vamiga_data_file,
       "Name of data dump file used by data_dump() or on startup.\n\
Don't change the value from LISP.  (Use Environment variable EMACS_DATA_FILE or\n\
command line option `--data-file' to set it)");
  Vamiga_data_file = build_string ((char*)Admp_data_file_name ());
  DEFVAR_BOOL ("amiga-initialized", &amiga_initialized, "");  
  defsubr (&Samiga__data_file);
#endif

  DEFVAR_BOOL ("amiga-expand-path", &amiga_expand_path,
	       "Non-nil causes expanding drives to volumes and\n\
assigns to absolute paths by `substitute-in-file-name'");
  /*  amiga_expand_path = 1; */

  DEFVAR_BOOL ("amiga-paths", &amiga_paths,
	       "Non-nil allows use of amiga path syntax");
  amiga_paths = 1;

  DEFVAR_BOOL ("amiga-working-killpg", &amiga_working_killpg,
	       "Experimental debug variable: nil means changes sign of gid for kill()");
  /*  amiga_working_killpg = 1; */

#ifdef AMIGA_HAVE_INTUI
  {
    extern int win_kp_numlock, win_simple_refresh;

    DEFVAR_BOOL ("amiga--nk-numlock", &win_kp_numlock,
	       "To control meaning of key on numeric keypad:\n\
nil - functions (like home, kp-up)\n\
 t  - ordinary numbers (nk-0, nk-1, ...)");
  /*  win_kp_numlock = 1; */

  DEFVAR_BOOL ("amiga-simple-refresh", &win_simple_refresh, 
	       "Switch to enable experimental frame refreshing on Intui\n\
If non nil, it enables simple refresh which is faster than smart refresh.\n\
At least on my machine having AGA-graphics.\n\
###Status: not fully implemented yet.###");  
  }
#endif /* AMIGA_HAVE_INTUI */

  DEFVAR_BOOL ("amiga-working-jobctrl", &amiga_working_jobctrl,
	       "Enable this if SIGTSTP is implemented by both the current ixemul\n\
and the used shell.\n\
Setting it to nil causes `suspend-emacs' to spawn a subshell");
  /*  amiga_working_jobctrl = 1; */

  DEFVAR_BOOL ("amiga-exp-use-usg-jobctrl", &amiga_exp_use_usg_jobctrl,
	       "Experimental flag:  If 'amiga-working-jobctrl' is nil then\n\
USG_JOBCTRL is used (ptrace(0,0,0,0); killpg(gid,SIGQUIT");
  /*  amiga_exp_use_usg_jobctrl = 1; */

  DEFVAR_BOOL ("amiga-create-icons", &amiga_create_icons,
	       "If non-nil, create icons when saving files.");
  defsubr (&Samiga_put_icon);

#ifdef AMIGA_HAVE_INTUI
  defsubr (&Samiga_popup_file_request);
  defsubr (&Samiga_popup_font_request);
  defsubr (&Samiga_load_color_file);
#endif

  defsubr (&Samiga_set_font);
  amiga_process_stack_size = 0;
  DEFVAR_INT ("amiga-process-stack-size", &amiga_process_stack_size,
	      "Size of stack for called processes. 0 means same size as emacs stack.");
  syms_of_amiga_malloc ();

#ifdef AMIGA_IWIN_CLIPBOARD
  syms_of_amiga_window_select ();
#endif /* AMIGA_IWIN_CLIPBOARD */
#ifdef AMIGA_REXX
  syms_of_amiga_rexx ();
#endif /* AMIGA_REXX */

#if 0 //def AMIGA_V_EMACS_20_2
  { /* Avoid invalid pointers in dump/patch code. -bw/27-Jan-98 */
    extern Lisp_Object re_match_object;
    staticpro (&re_match_object);
  }
#endif /* AMIGA_V_EMACS_20_2 */
#endif /* not AMIGA_V_XEMACS_20_4 */
}


/* Amiga system pecific initializing; called from main() */
void
Asys_early_init (int *acp, char ***avp)
{
  int argc = *acp;
  char **argv = *avp;

  check_precond (acp >= 0 && avp && (*avp)[*acp] == 0);

  /* This should be initialized at first (and therefore destroyed by
     atexit last) because it implements (m]re]c)alloc() and free() */
  if (!Asma_init_module ())
    exit (EXIT_FAILURE);

  if (!Asys_init_module ())
    {
      fprintf (stderr, "%s\n", ASYSE_MSG);
      exit (EXIT_FAILURE);
    }

  atexit (Asys_exit_module);

#ifndef AMIGA_V_XEMACS_20_4
  if (!Amal_init_module ())
    exit (EXIT_FAILURE);
#endif
#ifdef AMIGA_DUMP
  if (!Admp_init_module ())
    exit (EXIT_FAILURE);
#endif
#ifndef AMIGA_V_XEMACS_20_4
  /* increment/decremnt default pure space size */
  if (argc > 2 && 0 == strcmp (argv[1], "-pure"))
    {
      puresize = DEF_PURESIZE + atoi (argv[2]);
      assert (puresize > 0);
      argc -= 2;
      argv += 2;
    }
#endif /* AMIGA_V_XEMACS_20_4 */

#ifdef AMIGA_DUMP
  /* override default data dump file -bw/15-Jan-98 */
  if (argc > 2 && 0 == strcmp (argv[1], "--data-file"))
    {
      Admp_set_data_file_name (argv[2]);
      argc -= 2;
      argv += 2;
    }
  else
    {
      const char *data_file = getenv ("EMACS_DATA_FILE");
      if (data_file)
	Admp_set_data_file_name (data_file);
    }
#endif /* AMIGA_DUMP */

  /* Patch real argc, argv to hide arguments we used */
  argv[0] = (*avp)[0];
  *avp = argv;
  *acp = argc;

  /* Set or unixify HOME environment variable if required. */
  {
    char *h = getenv ("HOME");
#ifdef AMIGA_DEFAULT_HOME
    if (!h)
      setenv ("HOME", AMIGA_DEFAULT_HOME, 1);
#endif
    if (h && h[0] != '/')
      {
	int hsz = strlen (h);
	char *tmp = (char*)Asys_absolute_dos_to_unix (alloca (hsz + 2),
						      hsz + 2, h, hsz);
	if (tmp)
	  setenv ("HOME", tmp, 1);
      }
  }

  /* Set defaults for TERM/TERMCAP if unset. */
#ifdef AMIGA_DEFAULT_TERM
  setenv ("TERM", AMIGA_DEFAULT_TERM, 0);
#endif
#ifdef AMIGA_DEFAULT_TERMCAP
  setenv ("TERMCAP", AMIGA_DEFAULT_TERMCAP, 0);
#endif

}

#ifdef AMIGA_HAVE_INTUI
void
Asys_init_display ()
{
  if (!Awin_init_module (atexit))
    {
      fprintf (stderr, "%s\n", ASYSE_MSG);
      exit (EXIT_FAILURE);
    }
}
#endif /* AMIGA_HAVE_INTUI */

void
Asys_init_rexx ()
{
#ifdef AMIGA_REXX
  if (!(Arxp_init_module (atexit) 
	&& (!inhibit_window_system 
	    || Awin_arexx_tty_init (atexit))))
    {
      fprintf (stderr, "%s\n", ASYSE_MSG);
      exit (EXIT_FAILURE);
    }
#endif
}

void 
cleanup_amiga (void)
{
}

void 
amiga_undump_reinit (void)
/* Post-undump initialisation */
{
  amiga_create_icons = 0;	/* = _WBenchMsg != 0;  FIXME-bw; */
}

/* following stuff temporarilly added by [bw] */
#ifdef __amigaos
#include "amiga.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void 
Aerr_print_msg (char const *fmt,...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fputc('\n', stderr);
}

void 
Aerr_fail (char const *fmt,...)
{
  va_list ap;
  FILE *conerr = 0;
  
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);

  conerr = fopen ("con://200//EmacsFailure/WAIT", "w");
  if (conerr)
    {
      vfprintf (conerr, fmt, ap);
      fputc('\n', conerr);
      Aerr_print_backtrace (0, 100, conerr);
      fclose (conerr);
    }
  va_end (ap);
  fputc('\n', stderr);
  abort();
}

void 
no_memory (void)
{
  Aerr_fail ("No memory");
}


/* PID encoding */

/* Make Lisp_String holding encoded C integer */
Lisp_Object
sys_pid_encode (int n)
{
  Lisp_Object result;

  XSETINT (result, n);
  if (XFASTINT (result) != n)
    {
      unsigned char buf[4] = { n & 0xff, (n>>8) & 0xff, (n>>16) & 0xff, (n>>24) & 0xff};
      result = make_string (buf, 4);
    }
  return result;
}

/* Decode Lisp_String holding encoded C integer */
int
sys_pid_decode (Lisp_Object pid)
{
  if (INTEGERP (pid))
    return XFASTINT (pid);
  else if (STRINGP (pid))
    {
      unsigned char const *cv = XSTRING_DATA (pid);
      return ((int)cv[0] | ((int)cv[1] << 8) | ((int)cv[2] << 16) | ((int)cv[3] << 24));
    }
  else
    wrong_type_argument (Qintegerp, (pid)); /* XXX */
  /* NOTREACHED */
  return 0;
}

int
sys_killpg (int gid, int signo)
{
  if (!amiga_working_killpg)
    gid = -gid;
  return kill (-gid, signo);
}



/* Make stdin a separate file.  */
/* This is called if "--terminal" option is used.  We need this,
   because some commands in C: don produce any output, if Input() and
   Output() are both the same file.  -bw/14-Sep-97  */
static void *Asys_old_console_task;
static void
Asys_restore_console_task ()
{
  if (Asys_old_console_task)
    SetConsoleTask (Asys_old_console_task);
  Asys_old_console_task = 0;
}

void
Asys_separate_stdin ()
{
  long fh;
  void *port;
  int term;

#if 1				/* Which one is better? */
  if (!isatty (STDIN_FILENO))
    return;
  fh = ix_filehandle (STDIN_FILENO);
  if (!fh)
    return;
#else
  fh = Input ();
  if (! (fh && IsInteractive (fh)))
    return;
#endif

  /* Change Console Task */
  port = ((int**)(fh<<2))[2];
  port = (void*) SetConsoleTask (port); 

  term = open ("*", O_RDONLY);
  if (term >= 0)
    dup2 (term, STDIN_FILENO);
  close (term);

#if 0
  /* Restore Console Task */
  SetConsoleTask (port); 
#else
  Asys_old_console_task = port;
  atexit (Asys_restore_console_task);
#endif
}

/* Make fd the new controlling TTY of this process. */
/* Note: If this not a (forked) subprocess (if it called from CLI),
   you have to restore the old TTY.  For doing this you should open a
   "/dev/tty" before calling this function the first time. Use the
   descriptor with this function in exit(3) installing it with
   atexit(3).  But this is no solution, if you change the
   program-image (with exec(2)) in such a "unforked" process.  We
   would need the help of the Ixemul-server (But I'm not sure about
   that at all) -bw/16-Sep-97 */
int
Asys_new_tty (int fd)
{
  BPTR fh;
  void *port;

  if (!isatty (fd))
    {
      errno = ENOTTY;
      return -1;
    }
  
  fh = ix_filehandle (fd);
  if (!fh)
    {
      errno = ENOTTY;		/* Is there a better one? */
      return -1;
    }

  port = ((void **)((long)fh<<2))[2];

  port = SetConsoleTask (port);

  return 0;
}


/* section: experimental code */

#if 1
#undef CurrentTime /* defined for both X and Intuition */
#include <proto/intuition.h>

/* Print panic requester to allow attaching gdb(1). */
/* This should be done by ixemul already, but I think it does not work
   because of the SIGABRT signalhandler in emacs.c.  But I still
   puzzled about what really happens there.  -bw/18-Sep-97 */

/* Show panic requester and wait to user confirm. */
#define USER_REQUESTED_ABORT 0
#define USER_REQUESTED_CONTINUE 1
static int
Asys_ix_panic (const char *msg, ...)
{
#ifndef AMIGA_HAVE_INTUI
  return -1;
#else /* AMIGA_HAVE_INTUI */
  extern int noninteractive, inhibit_window_system;
  struct IntuitionBase *IntuitionBase;
  int choice;			/* RESULT */
  va_list ap;

  if (noninteractive || inhibit_window_system)
    return -1;

#undef INTUITION_BASE_NAME
#define INTUITION_BASE_NAME IntuitionBase
  INTUITION_BASE_NAME = (struct IntuitionBase *)OpenLibrary ("intuition.library", 0);
  if (!INTUITION_BASE_NAME)
    return -1;

  va_start(ap, msg);
  {
    struct EasyStruct panic 
      = { sizeof(struct EasyStruct), 0, "Emacs Message", 0, "Continue|Abort" };
       
    panic.es_TextFormat = (char *)msg;
    choice = EasyRequestArgs (NULL, &panic, NULL, ap);
   }
  va_end(ap);
  CloseLibrary ((struct Library *) INTUITION_BASE_NAME);
#undef INTUITION_BASE_NAME
  return choice;
#endif /* AMIGA_HAVE_INTUI */
}


int
Asys_invalid_precond (const char *file, unsigned line, const char *function,
		      const char *bool_expr)
{
  char buf[50];

  fprintf (stderr, "%s:%u:%s: invalid precond (%s)\n",
	   file, line, function, bool_expr);

  Aerr_print_backtrace (0, 100, stderr);
  fputc('\n', stderr);

  /* display only first 50 characters of bool_expr in GUI requester */
  strncpy (buf, bool_expr, (sizeof buf) - 1);
  buf [(sizeof buf) - 1] = '\0';
  if (Asys_ix_panic ("File: %s\nLine: %lu\nFunction: %s\n"
		     "Error-Type: invalid precond:\n (%s)\n",
		     file, line, function, buf)
      == USER_REQUESTED_CONTINUE)
    return 0;			/* user want continue  */

  exit (EXIT_FAILURE);		/* FIXME-bw: should be abort */
}


/* Pasted ixemul:abort(2) but it shows the panic requester before and
   not after kill(2) */
void
abort (void)
{
  /* XXX-bw/24-Oct-97 */
  Aerr_print_backtrace (0, 100, stderr);
  fputc('\n', stderr);

  /* This allows examining of machine addresses (Owner, Wack) or
     attaching Gdb to this process */
  if (Asys_ix_panic ("Abort!\n") == USER_REQUESTED_CONTINUE)
    return;

  kill (0, SIGABRT);
  /* if this should be caught, do exit directly... */
  /* ???-bw/19-May-98: better call _exit()? */
  exit(20);
}

#endif

struct softint
{
  struct MsgPort mp;
  struct Interrupt is;
};

struct MsgPort *
Asys_create_softint (void (*handler) ())
{
  struct softint *sint;

  check_precond (handler);

  sint = AllocMem (sizeof (struct softint), MEMF_PUBLIC | MEMF_CLEAR);
  if (!sint)
    return 0;

  sint->mp.mp_Node.ln_Type = NT_MSGPORT;
  sint->mp.mp_Flags = PA_SOFTINT;
  sint->mp.mp_SoftInt = &sint->is;
  NewList (&sint->mp.mp_MsgList);

  sint->is.is_Node.ln_Type = NT_INTERRUPT;
  sint->is.is_Node.ln_Pri = 0; /* XXX-bw/02-Apr-98: CHECKME: prio was formerly -32 */
  sint->is.is_Data = &sint->mp;
  sint->is.is_Code = (APTR) handler;

  return (struct MsgPort *) sint;
}

void
Asys_destroy_softint (struct MsgPort *mp)
{

  check_precond (mp);

  {
    struct Message *msg;
    while ((msg = GetMsg (mp)))
      ReplyMsg (msg);
  }

  FreeMem (mp, sizeof (struct softint));
}


#ifdef AMIGA_DUMP
Admp_tbl (amiga_sysdep) = {
  Admp_ndobj (a_sys),
  Admp_ndobj (Asys_old_console_task),
  Admp_ndobj (Asys_errmsgs),
  Admp_ndobj (Asys_errno),
  0};
#endif

#endif /* __amigaos */
