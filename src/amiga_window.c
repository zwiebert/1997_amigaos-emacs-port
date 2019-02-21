/*
   TODO:
   -   V39: ObtainBestPen(gfx)/ReleasePen(gfx)/SetWriteMask(gfx)
*/
#include "amiga_window_.h" /* internal component header */
#include "amiga_window_defs.h"
#include "syssignal.h"
#include "xterm.h"


/* module configuration */

/* Define AMIGA_IWIN_SOFTINT to let the window user port generating
   soft interrupts instead of signals.  The interrupt handler sets
   Vquit_flag=Qt if he found an raw key event which would produce the
   interrupt_char.  Also it sets a signal, for which ix_select() does
   waiting. -bw/10-Nov-97 */
#define AMIGA_IWIN_SOFTINT



void AITwin_softint_handler ();
void AITwin_wbapp_softint_handler ();

extern struct frame *pending_autoraise_frame; /* in amiga_window_input.c */

static Visual win_default_visual = { PseudoColor };
static struct Process *win_proc;
static pid_t win_pid;  /* The process  created the Intuition windows */
long win_softint_fwd_sigbit;
short quit_char_rk_code, quit_char_rk_qual;


Visual *
select_visual (Display *dpy, int screen, int *n_planes)
{
  check_precond (WD_VALID_P (screen) && WD_SCREEN_P (screen));

  if (n_planes)
    *(n_planes)=  WD_SCREEN (screen)->RastPort.BitMap->Depth;

  return &win_default_visual;
}     



/* flags */
volatile sig_atomic_t Awin_input_sigmask; /* Exec-signal mask for IDCMP events */
static int volatile input_signal_count;
static int volatile Awin_input_pending;	/* set by SIGMSG handler. get by Awin_select()  */
static int Awin_buggy_ix_select_sigbit = -1; /* see Awin_select() */
static bool Awin_initialized;
static int x_initialized;
static int x_noop_count;
extern Lisp_Object Vcommand_line_args, Vsystem_name;
extern int amiga_i_windows;	/* in xfns.c */

/* input Amiga sigbit mask */
long Awin_input_signal_mask = (SIGBREAKF_CTRL_E | SIGBREAKF_CTRL_D);
/* last result of ConnectionNumber */
long Awin_connection_fd;
/* During an update, nonzero if chars output now should be highlighted.  
extern int highlight;



extern struct frame *updating_frame; /* don't use this? */


/* window state */
#include "amiga_window_gc.c"
#include "amiga_window_color.c"



#ifdef emacs

void
Awin_hook_out ()
{

  check_precond (selected_frame);

  change_line_highlight_hook = 0;
  clear_end_of_line_hook = 0;
  clear_frame_hook = 0;
  clear_to_end_hook = 0;
  condemn_scroll_bars_hook = 0;
  cursor_to_hook = 0;
  delete_glyphs_hook = 0;
  frame_raise_lower_hook  = 0;
  frame_rehighlight_hook  = 0;
  frame_up_to_date_hook  = 0;
  ins_del_lines_hook = 0;
  insert_glyphs_hook = 0;
  judge_scroll_bars_hook = 0;
  mouse_position_hook  = 0;
  read_socket_hook  = 0;
  reassert_line_highlight_hook = 0;
  redeem_scroll_bar_hook = 0;
  reset_terminal_modes_hook = 0;
  ring_bell_hook = 0;
  set_terminal_modes_hook = 0;
  set_terminal_window_hook = 0;
  set_vertical_scroll_bar_hook = 0;
  update_begin_hook = 0;
  update_end_hook = 0;
  write_glyphs_hook = 0;
}  
#else 
#define x_initialize()
#define Awin_hook_out()
#endif


void
Awin_input_available_signal (long signo, long exec_sigmask)
{
  int old_errno;

#if 0
  kprintf ("sighandler (%ld, 0x%lx)\n", signo, exec_sigmask);
#endif
  if (!(signo & SIGMSG))
    return;

  old_errno = errno; /* save errno, because we are a signal handler.  */


  if (exec_sigmask & SIGBREAKF_CTRL_C)
    {
      SetSignal (0, SIGBREAKF_CTRL_C);
      kill (getpid (), SIGINT);
      DB_TRACE;
    }

  /* Map our input signal to SIGIO if it blocked, else call
     read_avail_input() directly. */
  if (exec_sigmask & Awin_input_sigmask)
    {
      sigset_t set, oset;
      SetSignal (0, Awin_input_sigmask);
      set = SIGIO;
      if (sigprocmask (SIG_BLOCK, &set, &oset) >= 0)
	{
	  if (oset & SIGIO)	/* If was SIGIO already blocked... */
	    {
#if 0
	      kill (getpid (), SIGIO); /* ...then map to deferred SIGIO
					  (???-bw) */
#else
	      interrupt_input_pending = 1; /* ???-bw/01-Nov-97:##exp## */
#endif
	    }
	  else
	    {
	      read_avail_input (1);
	      /* SIGIO is blocked, but we are in an interrupt anyway. */
	      read_avail_input (1);
	      sigprocmask (SIG_UNBLOCK, &set, 0);
	    }
	}
      else
	{
	  DB_TRACE;
	  assert (0);
	}
      DB_TRACE;
    }
  errno = old_errno;
}

void
Awin_exit_module ()
{
  int i;
  SIGMASKTYPE sig_mask;

  BLOCK_INPUT;
  sig_mask = sigblock (sigmask (SIGINT));

#ifdef AMIGA_IWIN_CLIPBOARD
  Awsl_exit_module ();
#endif /* AMIGA_IWIN_CLIPBOARD */

  Awin_hook_out ();

  /** don't use any screen pointer after freeing the WDs! **/
  for (i=0; i < WD_MAX; ++i)
    {
      if (WD_USED_P (i))
	win__destroy (i);
    }

  win__gx_free_all ();

  dispose_ptr (CloseLibrary, INTUITION_BASE_NAME);
  dispose_ptr (CloseLibrary, ICON_BASE_NAME);
#ifdef AMIGA_IWIN_CLIPBOARD
  dispose_ptr (CloseLibrary, IFFPARSE_BASE_NAME);
#endif /* AMIGA_IWIN_CLIPBOARD */
  dispose_ptr (CloseLibrary, KEYMAP_BASE_NAME);
  dispose_ptr (CloseLibrary, GRAPHICS_BASE_NAME);
  dispose_ptr (CloseLibrary, DISKFONT_BASE_NAME);
  dispose_ptr (CloseLibrary, UTILITY_BASE_NAME);
  dispose_ptr (CloseLibrary, WB_BASE_NAME);
  dispose_ptr (CloseLibrary, LAYERS_BASE_NAME);

#ifndef AMIGA_IWIN_SOFTINT
  dispose_ptr (DeleteMsgPort, WIN_COMMON_USER_PORT);
  dispose_ptr (DeleteMsgPort, WIN_WB_APP_PORT);
#else
  if (win_softint_fwd_sigbit != -1)
    FreeSignal (win_softint_fwd_sigbit), (win_softint_fwd_sigbit = -1);

  dispose_ptr (Asys_destroy_softint, WIN_COMMON_USER_PORT);
  dispose_ptr (Asys_destroy_softint, WIN_WB_APP_PORT);
#endif

  if (Awin_buggy_ix_select_sigbit != -1)
    FreeSignal (Awin_buggy_ix_select_sigbit), (Awin_buggy_ix_select_sigbit = -1);

  Awin_initialized = false;

  sigsetmask (sig_mask);
  UNBLOCK_INPUT;
}

/* A hack to enable AREXX for TTYs without doing much work. */
static void
Awin_arexx_tty_exit ()
{
  if (Awin_buggy_ix_select_sigbit != -1)
    FreeSignal (Awin_buggy_ix_select_sigbit), (Awin_buggy_ix_select_sigbit = -1);
#ifdef AMIGA_IWIN_SOFTINT
  if (win_softint_fwd_sigbit != -1)
    FreeSignal (win_softint_fwd_sigbit), (win_softint_fwd_sigbit = -1);
#endif
}
bool
Awin_arexx_tty_init (atexit_fp reg)
{
  if ((Awin_buggy_ix_select_sigbit = AllocSignal (-1)) == -1)
    ASYSE_SET_MISC ("AllocSignal(exec)",  "cannot alloc dummy signal");
  else if ((win_softint_fwd_sigbit = AllocSignal (-1)) == -1)
    ASYSE_SET_MISC ("AllocSignal(exec)",  "cannot alloc signal");
  else if ((*reg) (Awin_arexx_tty_exit) < 0)
    ASYSE_SET_MISC ("atexit(3)",  "out of memory");
  else
    {
      win_pid = getpid ();
      win_proc = (struct Process *) FindTask (0);
      Awin_input_sigmask = (1<<win_softint_fwd_sigbit);
      return true;
    }
  Awin_arexx_tty_exit ();
  return false;
}

bool
Awin_init_module (atexit_fp reg)
{
  SIGMASKTYPE sig_mask;

  BLOCK_INPUT;			/* I'm not sure if its needed. -bw/22-Oct-97 */
  sig_mask = sigblock (sigmask (SIGINT));

  check_precond (!Awin_initialized);

  Awin__km_init ();

  if (!(GRAPHICS_BASE_NAME = (void*)OpenLibrary("graphics.library", 39)))
    ASYSE_SET ("graphics.library-39", ASYSE_NO_SHARED_LIB);
  else if (!(ICON_BASE_NAME = (void*)OpenLibrary("icon.library", 37)))
    ASYSE_SET ("icon.library-37", ASYSE_NO_SHARED_LIB);
#ifdef AMIGA_IWIN_CLIPBOARD
  else if (!(IFFPARSE_BASE_NAME = (void*)OpenLibrary("iffparse.library", 37)))
    ASYSE_SET ("iffparse.library-37", ASYSE_NO_SHARED_LIB);
#endif /* AMIGA_IWIN_CLIPBOARD */
  else if (!(INTUITION_BASE_NAME = (void*)OpenLibrary("intuition.library", 37)))
    ASYSE_SET ("intuition.library-37", ASYSE_NO_SHARED_LIB);
  else if (!(KEYMAP_BASE_NAME = OpenLibrary("keymap.library", 37))) 
    ASYSE_SET ("keymap.library-37", ASYSE_NO_SHARED_LIB);
  else if (!(DISKFONT_BASE_NAME = OpenLibrary("diskfont.library", 37))) 
    ASYSE_SET ("diskfont.library-37", ASYSE_NO_SHARED_LIB);
  else if (!(UTILITY_BASE_NAME = (void*)OpenLibrary("utility.library", 37))) 
    ASYSE_SET ("utility.library-37", ASYSE_NO_SHARED_LIB);
  else if (!(WB_BASE_NAME = (void*)OpenLibrary("workbench.library", 37)))
    ASYSE_SET ("workbench.library-37", ASYSE_NO_SHARED_LIB);
  else if (!(LAYERS_BASE_NAME = (void*)OpenLibrary("layers.library", 37)))
    ASYSE_SET ("layers.library-37", ASYSE_NO_SHARED_LIB);
#ifndef AMIGA_IWIN_SOFTINT
  else if (!(WIN_COMMON_USER_PORT = CreateMsgPort ()))
    ASYSE_SET_MISC ("CreateMsgPort(exec)",  "cannot open message port");
  else if (!(WIN_WB_APP_PORT = CreateMsgPort ()))
    ASYSE_SET_MISC ("CreateMsgPort(exec)",  "cannot open message port");
#else /* AMIGA_IWIN_SOFTINT */
  else if (!(WIN_COMMON_USER_PORT = Asys_create_softint (&AITwin_softint_handler)))
    ASYSE_SET_MISC ("Asys_create_softint()",  "cannot open user-port");
  else if (!(WIN_WB_APP_PORT = Asys_create_softint (&AITwin_wbapp_softint_handler)))
    ASYSE_SET_MISC ("Asys_create_softint()",  "cannot open WBApp port");
  else if ((win_softint_fwd_sigbit = AllocSignal (-1)) == -1)
    ASYSE_SET_MISC ("AllocSignal(exec)",  "cannot alloc signal");
#endif /* AMIGA_IWIN_SOFTINT */
  else if ((Awin_buggy_ix_select_sigbit = AllocSignal (-1)) == -1)
    ASYSE_SET_MISC ("AllocSignal(exec)",  "cannot alloc dummy signal");
#ifdef AMIGA_IWIN_CLIPBOARD
  else if (!Awsl_init_module ())
    ;
#endif /* AMIGA_IWIN_CLIPBOARD */
  else if (!Awcol_init_module (reg))
    ;
  else
    {

#ifdef AMIGA_IWIN_SOFTINT
      win_pid = getpid ();
      win_proc = (struct Process *) FindTask (0);
      Awin_input_sigmask = (1<<win_softint_fwd_sigbit);
      WIN_COMMON_USER_PORT->mp_SigBit = win_softint_fwd_sigbit;
#else /* not AMIGA_IWIN_SOFTINT */
      Awin_input_sigmask = (1 << WIN_COMMON_USER_PORT->mp_SigBit);
      Awin_input_sigmask |= (1 << WIN_WB_APP_PORT->mp_SigBit);
#endif /* not AMIGA_IWIN_SOFTINT */

#ifdef AMIGA_SET_QUIT
      /* init the fast quit_char check code */
      {
	extern int quit_char;
        char c = quit_char;
	unsigned char buf[2];

	if (MapANSI (&c, 1, buf, 1, 0) == 1)
	  {
	    quit_char_rk_code = buf[0];
	    quit_char_rk_qual = buf[1];
	  }
      }
#endif /* AMIGA_SET_QUIT */

      /* The SIGMSG handler breaks opendir(3) of ixemul-47.1 */
#ifndef AMIGA_IWIN_SOFTINT
      signal (SIGMSG, (void (*)()) Awin_input_available_signal);
#endif

      if (!reg || (*reg) (Awin_exit_module) != -1)
	{
	  Awin_initialized = true;
	  sigsetmask (sig_mask);
	  UNBLOCK_INPUT;
	  return true;
	}
    }

  Awin_exit_module ();
  sigsetmask (sig_mask);
  UNBLOCK_INPUT;
  return false;
}


/* This code does partly the same as an SIGMSG-handler would do.  (The
   SIGMSG is disabled, because it breaks
   opendir(ixemul:directory). Unfortunally we cannot call
   gobble_input() like a signal handler (is this really
   true?). -bw/10-Nov-97 */
#ifdef AMIGA_IWIN_SOFTINT
extern struct MsgPort win_softint_port;

void
__interrupt __saveds
AITwin_softint_handler (struct MsgPort *port __asm("a1"))
{
#ifdef AMIGA_SET_QUIT
  /* Test the last event in PORT's message queue for being a raw event
     which results in the "keyboard.c:quit_char".  This could fail, if
     there are more than one new event in the list.  But it works good
     enough .-bw/10-Nov-97 */
  if (port->mp_MsgList.lh_TailPred->ln_Succ)
    {
      extern Lisp_Object Vquit_flag;

      if (IWIN_TEST_KBQUIT ((struct IntuiMessage *)port->mp_MsgList.lh_TailPred))
	  Vquit_flag = Qt;
    }
#endif /* AMIGA_SET_QUIT */
  interrupt_input_pending = 1;

  /* Signal the main process the new incoming event.  The main task
     uses ix_select(ixemul) to wait for it.  -bw/10-Nov-97 */
  Signal ((struct Task*) win_proc, (1<<win_softint_fwd_sigbit));
}

void
__interrupt __saveds
AITwin_wbapp_softint_handler (struct MsgPort *port __asm("a1"))
{
  /* wake up the possible sleeping Emacs (ix_select()) */
  Signal ((struct Task*) win_proc, (1<<win_softint_fwd_sigbit));
  /* announce the pending input to the non sleeping Emacs */
  interrupt_input_pending = 1;
}
#endif /* AMIGA_IWIN_SOFTINT */



/* Return non-zero if FONT1 and FONT2 have the same width.
   We do not check the height, because we can now deal with
   different heights.
   We assume that they're both character-cell fonts.  */

int
same_size_fonts (XFontStruct  *f1, XFontStruct *f2)
{
  check_precond (f1 && f2);
  return (FONT_WIDTH (f1) == FONT_WIDTH (f2));
}


/* FIXME-bw: move me */
extern struct x_display_info *x_display_list;
extern Lisp_Object x_display_name_list;
extern Lisp_Object Qvendor_specific_keysyms;
/* The scroll bar in which the last X motion event occurred.

   If the last X motion event occurred in a scroll bar, we set this
   so XTmouse_position can know whether to report a scroll bar motion or
   an ordinary motion.

   If the last X motion event didn't occur in a scroll bar, we set this
   to Qnil, to tell XTmouse_position to return an ordinary motion event.  */
extern Lisp_Object last_mouse_scroll_bar;


int WhitePixel(Display *dpy, int screen_nmb) { return COLOR_START+0; }
int BlackPixel(Display *dpy, int screen_nmb) { return COLOR_START+1; }

Window
RootWindowOfScreen (Window screen)
{
  check_precond (WD_VALID_P (screen) && WD_SCREEN_P (screen));
  return screen;
}

#include "amiga_window_defs.h"

struct x_display_info *
x_term_init (display_name, xrm_option, resource_name)
     Lisp_Object display_name;
     char *xrm_option;
     char *resource_name;
{
  Lisp_Object frame;
  char *defaultvalue;
  int connection;
  Display *dpy;
  struct x_display_info *dpyinfo;
  XrmDatabase xrdb;

  BLOCK_INPUT;

  if (!(Awin_initialized || Awin_init_module (atexit)))
    {
      UNBLOCK_INPUT;
      return 0;
    }
    
  if (!x_initialized)
    {
      x_initialize ();
      x_initialized = 1;
    }

  dpy = XOpenDisplay (XSTRING (display_name)->data);

  /* Detect failure.  */
  if (dpy == 0)
    {
      UNBLOCK_INPUT;
      return 0;
    }

  /* We have definitely succeeded.  Record the new connection.  */

  dpyinfo = (struct x_display_info *) xmalloc (sizeof (struct x_display_info));

#ifdef MULTI_KBOARD
  {
    struct x_display_info *share;
    Lisp_Object tail;

    for (share = x_display_list, tail = x_display_name_list; share;
	 share = share->next, tail = XCONS (tail)->cdr)
      if (same_x_server (XSTRING (XCONS (XCONS (tail)->car)->car)->data,
			 XSTRING (display_name)->data))
	break;
    if (share)
      dpyinfo->kboard = share->kboard;
    else
      {
	dpyinfo->kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
	init_kboard (dpyinfo->kboard);
	if (!EQ (XSYMBOL (Qvendor_specific_keysyms)->function, Qunbound))
	  {
	    char *vendor = ServerVendor (dpy);
	    dpyinfo->kboard->Vsystem_key_alist
	      = call1 (Qvendor_specific_keysyms,
		       build_string (vendor ? vendor : ""));
	  }

	dpyinfo->kboard->next_kboard = all_kboards;
	all_kboards = dpyinfo->kboard;
	/* Don't let the initial kboard remain current longer than necessary.
	   That would cause problems if a file loaded on startup tries to
	   prompt in the minibuffer.  */
	if (current_kboard == initial_kboard)
	  current_kboard = dpyinfo->kboard;
      }
    dpyinfo->kboard->reference_count++;
  }
#endif

  /* Put this display on the chain.  */
  dpyinfo->next = x_display_list;
  x_display_list = dpyinfo;

  /* Put it on x_display_name_list as well, to keep them parallel.  */ 
  x_display_name_list = Fcons (Fcons (display_name, Qnil),
			       x_display_name_list);
  dpyinfo->name_list_element = XCONS (x_display_name_list)->car;

  dpyinfo->display = dpy;

#if 0
  XSetAfterFunction (x_current_display, x_trace_wire);
#endif /* ! 0 */

  dpyinfo->x_id_name
#ifndef STRING_BYTES
    = (char *) xmalloc (XSTRING (Vinvocation_name)->size
			+ XSTRING (Vsystem_name)->size
			+ 2);
#else
    = (char *) xmalloc (STRING_BYTES (XSTRING (Vinvocation_name))
			+ STRING_BYTES (XSTRING (Vsystem_name))
			+ 2);
#endif

  sprintf (dpyinfo->x_id_name, "%s@%s",
	   XSTRING (Vinvocation_name)->data, XSTRING (Vsystem_name)->data);

#ifdef BW
  /* Figure out which modifier bits mean what.  */
  x_find_modifier_meanings (dpyinfo);
#endif

#ifdef BW
  /* Get the scroll bar cursor.  */
  dpyinfo->vertical_scroll_bar_cursor
    = XCreateFontCursor (dpyinfo->display, XC_sb_v_double_arrow);
#endif
#ifdef BW
  xrdb = x_load_resources (dpyinfo->display, xrm_option,
			   resource_name, EMACS_CLASS);
#endif
#ifdef HAVE_XRMSETDATABASE
  XrmSetDatabase (dpyinfo->display, xrdb);
#else
  dpyinfo->display->db = xrdb;
#endif
  /* Put the rdb where we can find it in a way that works on
     all versions.  */
  dpyinfo->xrdb = xrdb;

  dpyinfo->screen = ScreenOfDisplay (dpyinfo->display,
				     DefaultScreen (dpyinfo->display));
  dpyinfo->visual = select_visual (dpyinfo->display, dpyinfo->screen,
				   &dpyinfo->n_planes);

  dpyinfo->height = HeightOfScreen (dpyinfo->screen);
  dpyinfo->width = WidthOfScreen (dpyinfo->screen);
  dpyinfo->root_window = RootWindowOfScreen (dpyinfo->screen);
  dpyinfo->grabbed = 0;
  dpyinfo->reference_count = 0;
  dpyinfo->icon_bitmap_id = -1;
  dpyinfo->n_fonts = 0;
  dpyinfo->font_table_size = 0;
  dpyinfo->bitmaps = 0;
  dpyinfo->bitmaps_size = 0;
  dpyinfo->bitmaps_last = 0;
  dpyinfo->scratch_cursor_gc = 0;
  dpyinfo->mouse_face_mouse_frame = 0;
  dpyinfo->mouse_face_deferred_gc = 0;
  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_face_id = 0;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_mouse_x = dpyinfo->mouse_face_mouse_y = 0;
  dpyinfo->mouse_face_defer = 0;
  dpyinfo->x_focus_frame = 0;
  dpyinfo->x_focus_event_frame = 0;
  dpyinfo->x_highlight_frame = 0;

#ifdef NBW
  dpyinfo->Xatom_wm_protocols
    = XInternAtom (dpyinfo->display, "WM_PROTOCOLS", False);
  dpyinfo->Xatom_wm_take_focus
    = XInternAtom (dpyinfo->display, "WM_TAKE_FOCUS", False);
  dpyinfo->Xatom_wm_save_yourself
    = XInternAtom (dpyinfo->display, "WM_SAVE_YOURSELF", False);
  dpyinfo->Xatom_wm_delete_window
    = XInternAtom (dpyinfo->display, "WM_DELETE_WINDOW", False);
  dpyinfo->Xatom_wm_change_state
    = XInternAtom (dpyinfo->display, "WM_CHANGE_STATE", False);
  dpyinfo->Xatom_wm_configure_denied
    = XInternAtom (dpyinfo->display, "WM_CONFIGURE_DENIED", False);
  dpyinfo->Xatom_wm_window_moved
    = XInternAtom (dpyinfo->display, "WM_MOVED", False);
  dpyinfo->Xatom_editres
    = XInternAtom (dpyinfo->display, "Editres", False);
  dpyinfo->Xatom_CLIPBOARD
    = XInternAtom (dpyinfo->display, "CLIPBOARD", False);
  dpyinfo->Xatom_TIMESTAMP
    = XInternAtom (dpyinfo->display, "TIMESTAMP", False);
  dpyinfo->Xatom_TEXT
    = XInternAtom (dpyinfo->display, "TEXT", False);
  dpyinfo->Xatom_DELETE
    = XInternAtom (dpyinfo->display, "DELETE", False);
  dpyinfo->Xatom_MULTIPLE
    = XInternAtom (dpyinfo->display, "MULTIPLE", False);
  dpyinfo->Xatom_INCR
    = XInternAtom (dpyinfo->display, "INCR", False);
  dpyinfo->Xatom_EMACS_TMP
    = XInternAtom (dpyinfo->display, "_EMACS_TMP_", False);
  dpyinfo->Xatom_TARGETS
    = XInternAtom (dpyinfo->display, "TARGETS", False);
  dpyinfo->Xatom_NULL
    = XInternAtom (dpyinfo->display, "NULL", False);
  dpyinfo->Xatom_ATOM_PAIR
    = XInternAtom (dpyinfo->display, "ATOM_PAIR", False);
#endif
  dpyinfo->cut_buffers_initialized = 0;

  connection = ConnectionNumber (dpyinfo->display);
  dpyinfo->connection = connection;

  {
    char null_bits[1];

    null_bits[0] = 0x00;

#ifdef NBW
    dpyinfo->null_pixel
      = XCreatePixmapFromBitmapData (dpyinfo->display, dpyinfo->root_window, 
				     null_bits, 1, 1, (long) 0, (long) 0,
				     1);
#endif
  }

#ifdef subprocesses
  /* This is only needed for distinguishing keyboard and process input.  */
  if (connection != 0)
    add_keyboard_wait_descriptor (connection);
#endif

#ifndef F_SETOWN_BUG
#ifdef F_SETOWN
#ifdef F_SETOWN_SOCK_NEG
  /* stdin is a socket here */
  fcntl (connection, F_SETOWN, -getpid ());
#else /* ! defined (F_SETOWN_SOCK_NEG) */
  fcntl (connection, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN_SOCK_NEG) */
#endif /* ! defined (F_SETOWN) */
#endif /* F_SETOWN_BUG */


#if 1 /* now we use the reading end of a unused pipe as input descriptor
       #ifndef AMIGA_HAVE_INTUI /* We use SIGMSG or ix_select(ixemul).  SIGIO
			   may be raised by our own code.  Currently
			   it's done so, but it may
			   unessary. -bw/25-Oct-97 */
#ifdef SIGIO
  if (interrupt_input)
    init_sigio (connection);
#endif /* ! defined (SIGIO) */
#endif /* not AMIGA_HAVE_INTUI */

  UNBLOCK_INPUT;

  return dpyinfo;
}


int
AIT_HeightOfScreen (int screen)
{
  check_precond (WD_VALID_P (screen) && WD_SCREEN_P (screen));
  return WD_HEIGHT (screen);
}

int
AIT_WidthOfScreen (int screen)
{
  check_precond (WD_VALID_P (screen) && WD_SCREEN_P (screen));
  return WD_WIDTH (screen);
}

/* FIXME: ??? */
int
AIT_DefaultScreen(Display *dpy)
{
  check_precond (dpy);
  return dpy->screen;
}

/* FIXME: ??? */
int
AIT_ScreenOfDisplay (Display *dpy, int screen)
{
  return (WD_NIL != dpy->screen) ? dpy->screen : screen;
}

void
XSetIconName (Display *dpy, Window wd, const char *name)
{
  int i;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (name);

  WD_CHILD_FOREACH (wd, i)
    if (WD_ICON_P (i))
      WD_SET_NAME (i, STRING_CLONE (name));
}

void
XStoreName (Display *dpy, Window wd, const char *name)
{
  char *old_name;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (name);

  old_name = WD_NAME (wd);
  WD_SET_NAME (wd, STRING_CLONE (name));
  
  if (WD_WIN_P (wd))
    {
      SetWindowTitles (WD_WIN (wd), WD_NAME (wd), (UBYTE *)~0);
    }
  else if (WD_NWIN_P (wd))
    {
      WD_NWIN (wd)->Title = WD_NAME (wd);
    }

  STRING_FREE (old_name);
}


char *ServerVendor(Display *dpy) { DB_TRACE; return "unknown"; }

int DisplayCells () { DB_TRACE; return 0; }
int DoesBackingStore () { DB_TRACE; return 0; }
int DoesSaveUnders () { DB_TRACE; return 0; }
int HeightMMOfScreen () { DB_TRACE; return 0; }
int ProtocolRevision () { DB_TRACE; return 0; }
int ProtocolVersion () { DB_TRACE; return 0; }
int ScreenCount () { DB_TRACE; return 0; }
int VendorRelease () { DB_TRACE; return 0; }
int WidthMMOfScreen () { DB_TRACE; return 0; }
int XDefineCursor () { DB_TRACE; return 0; }
int XScreenNumberOfScreen () { DB_TRACE; return 0; }
int XSync () { DB_TRACE; return 0; }
int XSynchronize () { DB_TRACE; return 0; }
int x_bitmap_icon (struct frame *a, Lisp_Object b) { DB_TRACE; return 0; }
void x_check_errors (Display *a, char *b) { DB_TRACE; }
int x_catch_errors (Display *a) { DB_TRACE; return 0; }
int x_had_errors_p (Display *a) { DB_TRACE; return 0; }
int x_text_icon (struct frame *a, char *b) { DB_TRACE; return 0; }
void x_uncatch_errors (Display *a, int b) { DB_TRACE; }

char *
x_get_string_resource (rdb, name, class)
     XrmDatabase rdb;
     char *name, *class;
{
  DB_TRACE;
  if (getenv ("EMACS_DEV"))
    fprintf (stderr, "%s (%d, %s, %s)\n", __PRETTY_FUNCTION__,  rdb, name, class);
  return 0;
}

void x_clear_frame_selections (struct frame *f) { DB_TRACE; }

void x_set_mouse_position (struct frame *a, int b, int c) { DB_TRACE; }
x_clear_errors () { DB_TRACE; return 0; }
void x_set_mouse_pixel_position (struct frame *a, int b, int c) { DB_TRACE; }
void x_handle_selection_request (struct input_event *a) { DB_TRACE; }
void x_handle_selection_clear (struct input_event *a) { DB_TRACE; }
x_get_keysym_name () { DB_TRACE; return 0; }


/* input experimental code */

/* This would only entirely work, if our SIGMSG handler would called
   while we are stay in stdio syscalls like read(2). It would be
   useful to hook/link our select() function to ixemul library.  But
   after all we would only need a socket connected window process. We
   could even pass the MsgPort address through the socket/pipe an do
   the message processing in Emacs process. -bw/25-Oct-97 */
int
Awin_select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
	    struct timeval *timeout)
{
  int result;
  sig_atomic_t old_input_pending = Awin_input_pending; /* memorize  */
  long sigmask;			/* The SIGMSG handler is not called
				   when we are in select() */

#undef select			/* it's defined to ourself */

  sigmask = Awin_input_sigmask;	/* usual only the IDCMP port sigbit */
  sigmask |= (1 << Awin_buggy_ix_select_sigbit);

#if 0 /* -bw/07-Jan-98:##exp## input_fd now refers to a unused pipe */
  /* Clear the keyboard input descriptor, because it refers to
     ("/dev/null", O_WRONLY).  */
  if (readfds && FD_ISSET (Awin_connection_fd, readfds))
    {
      FD_CLR (Awin_connection_fd, readfds);
      if (Awin_input_pending)
	timeout = 0;
    }
#endif

  DB_TRACE;
  result = ix_select (nfds, readfds, writefds, exceptfds, timeout, &sigmask);
  
  /* if error (interrupted) keep the old Awin_input_pending */
#if 0 /* -bw/07-Jan-98:##exp## */
  /* Fake keyboard input descriptor  (???-bw:useless?) */
  if (readfds && result > -1 && Awin_input_pending)
    {
      FD_SET (Awin_connection_fd, readfds); /* fake fd */
      ++result;
      Awin_input_pending -= old_input_pending;
      DB_TRACE;
    }
#endif
  if (result == -1 && sigmask)
    if (!(sigmask & (1 << Awin_buggy_ix_select_sigbit)))
      {
	gobble_input (1); /* must block SIGIO because we are not in a signal handler  */
	DB_TRACE;
      }
#if 0
  /* debug output */
  if (result == -1)
    kprintf ("Awin_select() result == %ld;  errno == %ld;\n", result, strerror (errno));
  else
    kprintf ("Awin_select() result == %ld;\n", result);
#endif
  return result;
}

extern struct MsgPort *WIN_AREXX_PORT;

int
Awin_softint_block ()
{
#ifdef AMIGA_IWIN_SOFTINT
  struct MsgPort *mp;
  int result;

  result = 0;
  if ((mp = WIN_COMMON_USER_PORT))
    {
      result |= mp->mp_Flags;
      mp->mp_Flags = PA_IGNORE;
    }
  result <<= (sizeof (UBYTE) * 8);
  if ((mp = WIN_WB_APP_PORT))
    {
      result |= mp->mp_Flags;
      mp->mp_Flags = PA_IGNORE;
    }

  result <<= (sizeof (UBYTE) * 8);
  if ((mp = WIN_AREXX_PORT))
    {
      result |= mp->mp_Flags;
      mp->mp_Flags = PA_IGNORE;
    }

  return result;
#else
  return 0;
#endif
}

void
Awin_softint_set (int flags)
{
#ifdef AMIGA_IWIN_SOFTINT
  struct MsgPort *mp;

  if ((mp = WIN_AREXX_PORT))
    {
      mp->mp_Flags = (UBYTE)flags;
    }
  flags >>= (sizeof (UBYTE) * 8);
  if ((mp = WIN_WB_APP_PORT))
    {
      mp->mp_Flags = (UBYTE)flags;
    }
  flags >>= (sizeof (UBYTE) * 8);
  if ((mp = WIN_COMMON_USER_PORT))
    {
      mp->mp_Flags = (UBYTE)flags;
    }
#endif
}

void
Awin_ports_state (int state)
{
  int i;
  struct MsgPort *mp[] = 
  {
    WIN_COMMON_USER_PORT,
    WIN_AREXX_PORT,
    WIN_WB_APP_PORT
  };

  for (i=0; i < 3; ++i)
    if (mp[i])
      {
#ifdef AMIGA_IWIN_SOFTINT
	mp[i]->mp_Flags = (state) ? PA_SOFTINT : PA_IGNORE;
#else
	mp[i]->mp_Flags = (state) ? PA_SIGNAL : PA_IGNORE;
#endif
      }
}

#define __NOLIBBASE__
#define ASL_BASE_NAME asl_base
#include <proto/asl.h>

Lisp_Object
Awin_request_font (struct Window *parent_win)
{
  LONG Top = 0, Left = 0;
  Lisp_Object result = Qnil;
  struct FontRequester *req = 0;
  struct Library *ASL_BASE_NAME;
  struct Hook *parent_hook = 0;	/* call back for unknown IDCMP messages */

#ifdef AMIGA_HAVE_INTUI
  {
    extern struct Hook awinp_asl_idcmp_hook;
    if (Awin_initialized)
      parent_hook = &awinp_asl_idcmp_hook;
  }
#endif

  /* positioning */
  if (parent_win)
    {
      Top = parent_win->TopEdge + parent_win->MouseY - 75;
      Left = parent_win->LeftEdge + parent_win->MouseX - 160;
    }

  if (!(ASL_BASE_NAME = OpenLibrary ("asl.library", 0)))
    return Qnil;

  if ((req = AllocAslRequestTags (ASL_FontRequest,
				  ASL_Hail, (ULONG) "Emacs Font Request",
				  ASL_FuncFlags, FONF_FIXEDWIDTH,
				  TAG_DONE))
      && AslRequestTags (req,
			 ASL_TopEdge, Top,
			 ASL_LeftEdge, Left,
			 ASL_Height, 250,
			 parent_win ? ASLFO_Window : TAG_IGNORE, (ULONG)parent_win,
			 /* this will work for >=V38 only!. there is a
                            generic hook for V36.  XXX-bw/29-Jul-98 */
			 parent_hook ? ASLFO_IntuiMsgFunc : TAG_IGNORE, (ULONG)parent_hook,
			 TAG_DONE))
    {
      char const *name, *pos;

      name = req->fo_Attr.ta_Name;
      pos = strstr (name, ".font");
      if (pos)
	result = Fcons (make_string (name, pos - name),
			make_number (req->fo_Attr.ta_YSize));
    }

  FreeAslRequest (req);
  CloseLibrary (ASL_BASE_NAME);

  return result;
}

static Lisp_Object
Awin__wbarg_to_lisp (struct WBArg *wa, unsigned nmb)
{
  Lisp_Object flist; /* RESULT */
  char file_name[256];
  int i;

  flist = Qnil;
  i = nmb;
  while (i-- > 0)
    {
      if (wa[i].wa_Lock && NameFromLock (wa[i].wa_Lock, file_name, 256))
	{
	  if (wa[i].wa_Name)
	    AddPart (file_name, wa[i].wa_Name, 256);
	  flist = Fcons (build_string (file_name), flist);
	}
    }
  return flist;
}

static Lisp_Object
Awin__lispy_add_part (const char *dir, const char *file)
{
  char file_name[256];

  strncpy (file_name, dir, 255);
  file_name[255] = '\0';
  AddPart (file_name, file, 256);

  return build_string (file_name);
}

Lisp_Object
Awin_request_file (const char *dir, const char *file, bool save,
		   struct Window *parent_win)
{
  LONG Top = 0, Left = 0;
  Lisp_Object result = Qnil;
  struct FileRequester *req = 0;
  struct Library *ASL_BASE_NAME;
  struct Hook *parent_hook = 0;	/* call back for unknown IDCMP messages */

#ifdef AMIGA_HAVE_INTUI
  {
    extern struct Hook awinp_asl_idcmp_hook;
    if (Awin_initialized)
      parent_hook = &awinp_asl_idcmp_hook;
  }
#endif

  /* positioning */
  if (parent_win)
    {
      Top = parent_win->TopEdge + parent_win->MouseY - 75;
      Left = parent_win->LeftEdge + parent_win->MouseX - 160;
    }

  if (!(ASL_BASE_NAME = OpenLibrary ("asl.library", 0)))
    return Qnil;

  if ((req = AllocAslRequestTags (ASL_FileRequest,
				  ASL_Hail, (ULONG) "Emacs File Request",
				  ASL_FuncFlags, FONF_FIXEDWIDTH,
				  TAG_DONE))
      && AslRequestTags (req,
			 ASL_TopEdge, Top,
			 ASL_LeftEdge, Left,
			 ASL_Height, 250,
			 ASLFR_DoSaveMode, save ? TRUE : FALSE,
			 ASLFR_DoMultiSelect, save ? FALSE : TRUE,
			 ASLFR_InitialDrawer, (ULONG) dir,
			 ASLFR_InitialFile, (ULONG) file,
			 parent_win ? ASLFR_Window : TAG_IGNORE, (ULONG)parent_win,
			 /* this will work for >=V38 only!. there is a
                            generic hook for V36.  XXX-bw/29-Jul-98 */
			 parent_hook ? ASLFR_IntuiMsgFunc : TAG_IGNORE, (ULONG)parent_hook,
			 TAG_DONE))
    {
      if (req->fr_NumArgs)
	result = Awin__wbarg_to_lisp (req->fr_ArgList, req->fr_NumArgs);
      else if (req->fr_File)
	result = Fcons (Awin__lispy_add_part (req->fr_Drawer, req->fr_File), Qnil);
    }
  FreeAslRequest (req);
  CloseLibrary (ASL_BASE_NAME);

  return result;
}
