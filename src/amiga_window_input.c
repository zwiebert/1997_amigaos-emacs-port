#include <config.h>

/* interfaces: */

/* Keymap Amiga => Emacs */
#define KM_AMIGA_TO_EMACS_NUML(rk) (Awin_km_rk_to_emacs[(unsigned char) (rk)] +0)
#define KM_AMIGA_TO_EMACS(rk) (Awin_km_nk_to_emacs[(unsigned char) (rk)] +0)

/* The numeric tables means a keyboard which switch the numeric
   keyboard by hardware.  I'm not sure about it's usefulness. -bw/29-Oct-97 */

/* To control meaning of key on numeric keypad: 0 - functions (home,
   nk-up, ..); 1 - numbers (nk-0,...); -bw/21-May-98 */
int win_kp_numlock;

/* construct key table */
extern void Awin__km_init ();

/* Don't use this directly! */
extern unsigned char Awin_km_rk_to_emacs[256];
extern unsigned char Awin_km_nk_to_emacs[256];
 

/* implementations: */


/* keymap Emacs => Amiga */
#include <intuition/intuition.h>

#define NKP_0 (0x0f)
#define NKP_1 (0x1d)
#define NKP_2 (0x1e)
#define NKP_3 (0x1f)
#define NKP_4 (0x2d)
#define NKP_5 (0x2e)
#define NKP_6 (0x2f)
#define NKP_7 (0x3d)
#define NKP_8 (0x3e)
#define NKP_9 (0x3f)
#define NKP_POINT (0x3c)
#define NKP_ENTER (0x43)
#define NKP_MINUS (0x4a)
#define NKP_LBRACK (0x5a)
#define NKP_RBRACK (0x5b)
#define NKP_SLASH (0x5c)
#define NKP_ASTERISK (0x5d)
#define NKP_PLUS (0x5e)

#define RK_F1 (0x50)
#define RK_F2 (0x51)
#define RK_F3 (0x52)
#define RK_F4 (0x53)
#define RK_F5 (0x54)
#define RK_F6 (0x55)
#define RK_F7 (0x56)
#define RK_F8 (0x57)
#define RK_F9 (0x58)
#define RK_F10 (0x59)
#define RK_HELP (0x5f)
#define RK_BACKSPACE (0x41)
#define RK_DEL (0x46)
#define RK_ESC (0x45)
#define RK_TAB (0x42)
#define RK_RETURN (0x44)

#define XK_KP_INSERT (0x9e)
#define XK_KP_ENTER (0x8d)
#define XK_KP_SUBTRACT (0xad)
#define XK_KP_DELETE (0x9f)

#define XK_KP_END (0x9c)
#define XK_KP_DOWN (0x99)
#define XK_KP_NEXT (0x9b)
#define XK_KP_LEFT (0x96)
#define XK_KP_RIGHT (0x98)
#define XK_KP_HOME (0x95)
#define XK_KP_UP (0x97)
#define XK_KP_PRIOR (0x9a)
#define XK_KP_NUMLOCK (0x7f)
#define XK_KP_DIVIDE (0xaf)
#define XK_KP_MULTIPLY (0xaa)
#define XK_KP_ADD (0xab)
#define XK_KP_PAGE_DOWN XK_KP_NEXT
#define XK_KP_PAGE_UP XK_KP_PRIOR

#define XK_KP_0 (0xb0)
#define XK_KP_1 (0xb1)
#define XK_KP_2 (0xb2)
#define XK_KP_3 (0xb3)
#define XK_KP_4 (0xb4)
#define XK_KP_5 (0xb5)
#define XK_KP_6 (0xb6)
#define XK_KP_7 (0xb7)
#define XK_KP_8 (0xb8)
#define XK_KP_9 (0xb9)

#define XK_F1 (0xbe)
#define XK_F2 (0xbf)
#define XK_F3 (0xc0)
#define XK_F4 (0xc1)
#define XK_F5 (0xc2)
#define XK_F6 (0xc3)
#define XK_F7 (0xc4)
#define XK_F8 (0xc5)
#define XK_F9 (0xc6)
#define XK_F10 (0xc7)
#define XK_F11 (0xc8)
#define XK_F12 (0xc9)
#define XK_HELP (0x6a)
#define XK_BACKSPACE (0x08)
#define XK_TAB (0x09)
#define XK_LINEFEED (0x0a)
#define XK_RETURN (0x0d)
#define XK_ESCAPE (0x1b)
#define XK_DELETE (0xff)
#define XK_UP (0x52)
#define XK_LEFT (0x51)
#define XK_RIGHT (0x53)
#define XK_DOWN (0x54)

/* This table is for initialize the real lookup table.  */
struct Awin_km_defs
{
  unsigned char raw_key;	/* Amiga raw key code */
  unsigned char emacs_key;	/* offset in keyboard.c table */
} 
Awin_km_nk_defaults []
= {
    {NKP_0, XK_KP_INSERT},
    {NKP_POINT, XK_KP_DELETE},
    {NKP_0, XK_KP_INSERT},
    {NKP_1, XK_KP_END},
    {NKP_2, XK_KP_DOWN},
    {NKP_3, XK_KP_PAGE_DOWN},
    {NKP_4, XK_KP_LEFT},
    {NKP_5, XK_KP_5},
    {NKP_6, XK_KP_RIGHT},
    {NKP_7, XK_KP_HOME},
    {NKP_8, XK_KP_UP},
    {NKP_9, XK_KP_PAGE_UP},
    {NKP_LBRACK, XK_KP_NUMLOCK},
    // {NKP_RBRACK, XK_KP_---},
    {NKP_SLASH, XK_KP_DIVIDE},
    {NKP_ASTERISK, XK_KP_MULTIPLY},
    {NKP_MINUS, XK_KP_SUBTRACT},
    {NKP_PLUS, XK_KP_ADD},
    {NKP_ENTER, XK_KP_ENTER},
    {0}				/* TERMINATOR */
  },
Awin_km_rk_defaults[]
= {
    {NKP_0, XK_KP_0},
    {NKP_1, XK_KP_1},
    {NKP_2, XK_KP_2},
    {NKP_3, XK_KP_3},
    {NKP_4, XK_KP_4},
    {NKP_5, XK_KP_5},
    {NKP_6, XK_KP_6},
    {NKP_7, XK_KP_7},
    {NKP_8, XK_KP_8},
    {NKP_9, XK_KP_9},
    {RK_BACKSPACE, XK_BACKSPACE},
    {RK_DEL, XK_DELETE},
    {RK_ESC, XK_ESCAPE},
    {RK_TAB, XK_TAB},
    {RK_RETURN, XK_RETURN},
    {RK_F1, XK_F1},
    {RK_F2, XK_F2},
    {RK_F3, XK_F3},
    {RK_F4, XK_F4},
    {RK_F5, XK_F5},
    {RK_F6, XK_F6},
    {RK_F7, XK_F7},
    {RK_F8, XK_F8},
    {RK_F9, XK_F9},
    {RK_F10, XK_F10},
    {RK_HELP, XK_HELP},
    {CURSORUP, XK_UP},
    {CURSORRIGHT, XK_RIGHT},
    {CURSORLEFT, XK_LEFT},
    {CURSORDOWN, XK_DOWN},
    {0}				/* TERMINATOR */
  };

unsigned char Awin_km_rk_to_emacs[256];
unsigned char Awin_km_nk_to_emacs[256];

void
Awin__km_init ()
{
  int i, k;
  unsigned char raw_key;
  unsigned char *tbls[] = { Awin_km_rk_to_emacs, Awin_km_nk_to_emacs, 0};
  struct Awin_km_defs *defs[] = { Awin_km_rk_defaults, Awin_km_nk_defaults, 0};

  for (i=0; tbls[i]; ++i)
    if (defs[i])
      for (k=0; (raw_key = (defs[i])[k].raw_key); ++k)
	(tbls[i])[raw_key] = (defs[i])[k].emacs_key; 

  /* Make unused fields (zero) look trough the very first keymap */
  for (k=0; k < 256; ++k)
    for (i=1; tbls[i]; ++i)
      if ((tbls[i])[k] == 0)
	(tbls[i])[k] = (tbls[0])[k];
}

#ifdef KM__TEST
int main ()
{
  Awin__km_init ();
  /* use gdb to look at it */
  return 0;
}
#endif



/* emacs input loop */

#include "amiga_window_.h"
#include "amiga_window_defs.h"
#include <workbench/startup.h>
#include "xterm.h"
#include "syssignal.h"

#define IM_TIME_STAMP(IM) ((unsigned long)((IM)->Seconds * 1000ul + (IM)->Micros / 1000ul))
#define AM_TIME_STAMP(AM) ((unsigned long)((AM)->am_Seconds * 1000ul + (AM)->am_Micros / 1000ul))

/* this structure is only a quick hack to make it easier to reuse the
   original X code. */
struct win_user_data
{
  /* we could keep a frame pointer instead, but I'm not sure it's
     allowed to do that (does GC moves frame objects?)
     ???-bw/19-May-98 */
  struct x_display_info *dpy_info; /* we don't have one MsgPort per DPY */
  Window wd;
#define WIN_DPY_INFO(WIN) (assert ((WIN) && (WIN)->UserData), \
 (((struct win_user_data *)(WIN)->UserData)->dpy_info + 0))
#define WIN_WD(WIN) (assert ((WIN) && (WIN)->UserData), \
 (((struct win_user_data *)(WIN)->UserData)->wd + 0))
};

FRAME_PTR Awin_iwin_to_frame (struct Window *win, Lisp_Object *frame_result);

static bool
win_init_user_data (struct Window *win)
{
  struct win_user_data *data;
  FRAME_PTR f;

  local_precond (win->UserData == 0);

  f = Awin_iwin_to_frame (win, 0);
  if (!f)
    return false;

  data = calloc (1, sizeof (struct win_user_data));
  if (!data)
    return false;

  data->dpy_info = FRAME_X_DISPLAY_INFO (f);
  data->wd = FRAME_X_WINDOW (f);

  assert (WD_MAPPED_P (data->wd));

  win->UserData = (void *)data;
  return true;
}

FRAME_PTR Awin_wd_to_frame (Window wd, Lisp_Object *frame_result);

static inline bool
win_x_get_im (struct MsgPort *mp, struct IntuiMessage **im_ptr)
{
  struct Window *win;
  struct Message *msg;

  if (!mp)
    {
      *im_ptr = 0;
      return false;
    }

  for  (; (msg = GetMsg (mp)) && Awin_event_test (msg); ReplyMsg (msg))
    {
      struct win_event *ev = (struct win_event *)msg;
      struct frame *f = Awin_wd_to_frame (ev->wd, 0);
      if (!f)
	continue;

      if (ev->key == WD_EVENT_MAP)
	{  
	  // WD_MAP_NOTIFY_SET (FRAME_X_WINDOW (f), false);

	  f->async_visible = 1;
	  f->async_iconified = 0;
#ifdef AMIGA_V_EMACS_20_2
	  f->output_data.x->has_been_visible = 1;
#endif /* AMIGA_V_EMACS_20_2 */

	  /* wait_reading_process_input will notice this and update
	     the frame's display structures.  */
	  SET_FRAME_GARBAGED (f);

	  if (f->iconified)
	    {
	      f->async_visible = 1;
	      f->async_iconified = 0;
#if 0 /* TODO-bw/19-Jun-98 */
	      bufp[1] = bufp[0]; /* copies fields frame_or_window, time */
	      bufp->kind = deiconify_event;
	      ++bufp;
#endif
	    }
	  else if (! NILP (Vframe_list)
		   && ! NILP (XCONS (Vframe_list)->cdr))
	    /* Force a redisplay sooner or later
	       to update the frame titles
	       in case this is the second frame.  */
	    record_asynch_buffer_change ();
#if 0
	  /* Workaround when opening an AutoAdjust window not
	     fitting on the screen. */
	  if ((f->output_data.x->pixel_width >
	       (im->IDCMPWindow->WScreen->Width 
		- im->IDCMPWindow->BorderLeft 
		- im->IDCMPWindow->BorderRight))
	      || (f->output_data.x->pixel_height >
		  (im->IDCMPWindow->WScreen->Height 
		   - im->IDCMPWindow->BorderTop 
		   - im->IDCMPWindow->BorderBottom)))
	    Awinp_check_resize (f, im->IDCMPWindow);
#endif
	}
      else if (ev->key == WD_EVENT_UNMAP)
	{
	  WD_MAP_NOTIFY_SET (FRAME_X_WINDOW (f), false);
	  /* While a frame is unmapped, display generation is
	     disabled; you don't want to spend time updating a
	     display that won't ever be seen.  */
	  f->async_visible = 0;
#if 0
	  /* We can't distinguish, from the event, whether the window
	     has become iconified or invisible.  So assume, if it
	     was previously visible, than now it is iconified.
	     But x_make_frame_invisible clears both
	     the visible flag and the iconified flag;
	     and that way, we know the window is not iconified now.  */
	  if (FRAME_VISIBLE_P (f) || FRAME_ICONIFIED_P (f))
	    {
	      f->async_iconified = 1;
	      bufp->kind = iconify_event;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp++;
	    }
#endif
	}
    }

  if (!(*im_ptr = (void *)msg))
    return false;

  win = (*im_ptr)->IDCMPWindow;
  if (!win->UserData)
    win_init_user_data (win);

  return true;
}

/* Configure numeric keypad.  (0==F-keys, 1==ASCII) */
bool Awin_input_num_lock;

extern int quit_char;

int
Awin_iecode_to_ebutton (int button)
{
  switch (button & ~IECODE_UP_PREFIX)
    {
    case IECODE_LBUTTON: return 0;
    case IECODE_MBUTTON: return 1;
    case IECODE_RBUTTON: return 2;
    default:
      return 3;  /* XXX-bw/30-Dec-97: We cannot use abort() because of
		    future compatibility */
    }
}

/* Find the wd which belongs to WIN.  */
/* This may use the WIN->UserData if it initialized assuming that it
   points to an <struct win_user_data> object */
Window
Awin_iwin_to_wd (struct Window *win)
{
  int i;

  if (win->UserData)
    return WIN_WD (win);

  for (i=WD_MIN; i < WD_GREATEST; ++i)
    if (WD_WIN_P (i) && WD_WIN (i) == win)
      return i;

  return WD_NIL;
}

Window
Awin_iwin_to_wd_or_subwd (struct Window *win, int x, int y)
{
  Window wd;

  wd = Awin_iwin_to_wd (win);
  if (wd != WD_NIL)
    {
      Window sub_wd = Awin_find_subwin (wd, x, y);
      if (sub_wd != WD_NIL)
	wd = sub_wd;
    }

  return wd;
}

/* Return the subwindow containing X,Y or WD_NIL */
Window
Awin_iwin_to_subwd (struct Window *win, int x, int y)
{
  Window wd;

  wd = Awin_iwin_to_wd (win);
  if (wd != WD_NIL)
    wd = Awin_find_subwin (wd, x, y);

  return wd;
}

     
FRAME_PTR
Awin_iwin_to_frame (struct Window *win, Lisp_Object *frame_result)
{
  Lisp_Object tail, frame;
  struct frame *f;
  Window wd;

  check_precond (win);

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCONS (tail)->cdr)
    {
      frame = XCONS (tail)->car;

      if (!GC_FRAMEP (frame))
        continue;

      f = XFRAME (frame);
      if (f->output_data.nothing == 1) /* note: only `1' means `true'
				      because of a union */
	continue;
      
      wd = FRAME_WD (f);
      if (!WD_VALID_P (wd) || !WD_WIN_P (wd) || win != WD_WIN (wd))
	continue;

      frame_result && (*frame_result = frame);
      return f;
    }

  frame_result && (*frame_result = Qnil);
  return 0;
}

FRAME_PTR
Awin_wd_to_frame (Window wd, Lisp_Object *frame_result)
{
  Lisp_Object tail, frame;
  struct frame *f;

  check_precond (WD_VALID_P (wd) || WD_NIL_P (wd));

  if (wd != WD_NIL && WD_MAINWIN_P (wd))
  for (tail = Vframe_list; GC_CONSP (tail); tail = XCONS (tail)->cdr)
    {
      frame = XCONS (tail)->car;

      if (!GC_FRAMEP (frame))
        continue;

      f = XFRAME (frame);
      if (f->output_data.nothing == 1) /* note: only `1' means `true'
				      because of a union */
	continue;
      
      if (wd != FRAME_WD (f))
	continue;

      frame_result && (*frame_result = frame);
      return f;
    }

  frame_result && (*frame_result = Qnil);
  return 0;
}


/* like x_window_to_scroll_bar */

static struct scroll_bar *
Awin_wd_to_scroll_bar (Window wd, Lisp_Object *result)
{
  Lisp_Object tail, frame;

  if (wd != WD_NIL && WD_SUBWIN_P (wd))
  for (tail = Vframe_list;
       XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      Lisp_Object frame, bar, condemned;

      frame = XCONS (tail)->car;
      /* All elements of Vframe_list should be frames.  */
      if (! GC_FRAMEP (frame))
	abort ();

      /* Scan this frame's scroll bar list for a scroll bar with the
         right window ID.  */
      condemned = FRAME_CONDEMNED_SCROLL_BARS (XFRAME (frame));
      for (bar = FRAME_SCROLL_BARS (XFRAME (frame));
	   /* This trick allows us to search both the ordinary and
              condemned scroll bar lists with one loop.  */
	   ! GC_NILP (bar) || (bar = condemned,
			       condemned = Qnil,
			       ! GC_NILP (bar));
	   bar = XSCROLL_BAR (bar)->next)
	if (wd == SCROLL_BAR_X_WINDOW (XSCROLL_BAR (bar)))
	  {
	    result && (*result = bar);
	    return XSCROLL_BAR (bar);
	  }
    }

  result && (*result = Qnil);
  return 0;
}

/* Return visible subwin of WD on positon (X, Y)  if any.  */
static int
Awin_find_subwin (int wd, int x, int y)
{
  int child;
  /* Currently (?) only one subwindow level supported. */
  WD_CHILD_FOREACH (wd, child)
    if (WD_SUBWIN_P (child) && WD_MAPPED_P (child)
	&& WD_LEFT (child) <= x && x <= WD_RIGHT (child)
	&& WD_TOP (child) <= y && y <= WD_BOTTOM (child))
      return child;

  return WD_NIL;
}


#if 0 //ndef AMIGA_NEW_WBAPP
/* Extract a list of file name strings from the file locks in AM */
Lisp_Object
Awin_wbapp_get_file_args (struct AppMessage *am)
{
  Lisp_Object flist; /* RESULT */
  char file_name[256];
  struct WBArg *wa;
  int i;

  flist = Qnil;
  i = am->am_NumArgs;
  while (i-- > 0)
    {
      wa = &am->am_ArgList[i];
      if (wa->wa_Lock && NameFromLock (wa->wa_Lock, file_name, 256))
	{
	  if (wa->wa_Name)
	    AddPart (file_name, wa->wa_Name, 256);
	  flist = Fcons (build_string (file_name), flist);
	}
    }
  return flist;
}
#else /* AMIGA_NEW_WBAPP */
char *Awin_wbapp_iterate (void *obj, char ***iterator);
void Awin_wbapp_free_file_args (void *obj);
static void *Awin_wbapp_get_file_args (struct AppMessage *am);

char *
Awin_wbapp_iterate (void *obj, char ***iterator)
{
  check_precond (iterator);

  if (!obj)
    return 0;

   if (!*iterator)
     return *(*iterator = (char **)obj);
   else
     return *(++*iterator);
}

void
Awin_wbapp_free_file_args (void *obj)
{
  int i;
  char **p;

  if (!obj)
    return;

  p = obj;

  for (i=0; p[i]; ++i)
    free (p[i]);
  free (p);
}

/* Expand the file names in AM and return they as NULL terminated
   array of char pointer.  The caller is responsible to call
   Awin_wbapp_free_file_args() to free the result.  */
static void *
Awin_wbapp_get_file_args (struct AppMessage *am)
{
  char **result;
  char file[265];
  struct WBArg *wa;
  int i, k;

  if (!am->am_NumArgs)
    return 0;

  /* Note: We need storage on addres fitting in a LISP integer */
  result = (void *) Asma_low_alloc (sizeof *result * (am->am_NumArgs + 1));
  if (!result)
    return 0;

  bzero (result, sizeof *result * (am->am_NumArgs + 1));

  for (i=0, k=0; i < am->am_NumArgs; ++i)
    {
      wa = &am->am_ArgList[i];

      if (wa->wa_Lock && NameFromLock (wa->wa_Lock, file, 256))
	if (wa->wa_Name)
	  AddPart (file, wa->wa_Name, 256);

      if (file[0] && (result[k] = (void*)malloc (strlen (file) + 1)))
	{
	  strcpy (result[k], file);
	  ++k;
	}
    }
  return result;
}

#endif /* AMIGA_NEW_WBAPP */

/* This is a frame waiting to be autoraised, within XTread_socket.  */
struct frame *pending_autoraise_frame; /* TODO-bw/24-Oct-97  */

/* The main X event-reading loop - XTread_socket.  */

/* Timestamp of enter window event.  This is only used by XTread_socket,
   but we have to put it out here, since static variables within functions
   sometimes don't work.  */
static Time enter_timestamp;

/* Position and subwindow on which mouse button was pressed.  After
   releasing, it contains {0, 0, 0}. */
static int win_mouse_start_x, win_mouse_start_y;

#if 0
/* This holds the state XLookupString needs to implement dead keys
   and other tricks known as "compose processing".  _X Window System_
   says that a portable program can't use this, but Stephen Gildea assures
   me that letting the compiler initialize it to zeros will work okay.

   This must be defined outside of XTread_socket, for the same reasons
   given for enter_timestamp, above.  */
static XComposeStatus compose_status;
#endif

/* Set this to nonzero to fake an "X I/O error"
   on a particular display.  */
struct x_display_info *XTread_socket_fake_io_error;

/* A copy of the last mouse move message. Note that this must also
   updated at every IDCMP_ACTIVATEWINDOW event! */
struct Window *win_mouse_pos_iwin;
int win_mouse_pos_x, win_mouse_pos_y;
unsigned long win_mouse_pos_time;

/* frame of the last down button event */
extern FRAME_PTR last_mouse_frame;
extern XRectangle last_mouse_glyph;
/* contains the scrollbar while button is hold down */
extern Lisp_Object last_mouse_scroll_bar;

void
note_mouse_movement (FRAME_PTR frame, struct IntuiMessage *im, bool subwindow)
{

  local_precond (frame && im);

  /* make a copy (note: The reader must block
     interrupt input (BLOCK_INPUT)) */
  win_mouse_pos_iwin = im->IDCMPWindow;
  win_mouse_pos_x = im->MouseX;
  win_mouse_pos_y = im->MouseY;
  win_mouse_pos_time = IM_TIME_STAMP (im);

  WIN_REAL_TO_GZZ_XY (win_mouse_pos_iwin, win_mouse_pos_x, win_mouse_pos_y);

  if (subwindow)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;

      note_mouse_highlight (frame, -1, -1);
    }
  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  else if (win_mouse_pos_x < last_mouse_glyph.x
	   || win_mouse_pos_x >= last_mouse_glyph.x + last_mouse_glyph.width
	   || win_mouse_pos_y < last_mouse_glyph.y
	   || win_mouse_pos_y >= last_mouse_glyph.y + last_mouse_glyph.height)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;

      note_mouse_highlight (frame, win_mouse_pos_x, win_mouse_pos_y);
    }

#if 0
  if (frame->mouse_moved)
    ReportMouse (false, FRAME_WIN (frame));
  /* ???-bw/30-Dec-97: How about im->IDCMPWindow (would be safer but incorrect (?)) */
#endif
}

#if (WD_NIL != 0)
#error "Error: You have to add explicit initialization code for all `Window' globals!"
#endif
#define N_BUTTONS 5
static Window win_mouse_start_wd [N_BUTTONS];
static unsigned long Awin_button_up_time [N_BUTTONS];
static unsigned Awin_button_mask;


/* Update Awin_button_mask according to IM.  IM has to be a
   IDCMP_MOUSEBUTTONS event  -bw/23-Jul-98 */
void
Awin_button_note_change (struct IntuiMessage *im)
{
  check_precond (im->Class == IDCMP_MOUSEBUTTONS);

  if (im->Code & IECODE_UP_PREFIX)
    Awin_button_mask &= ~(1 << Awin_iecode_to_ebutton (im->Code));
  else
    Awin_button_mask |= (1 << Awin_iecode_to_ebutton (im->Code));
}

/* This is true as long we are in menu code.  This is currently not
   used by anyone.  It's set by Awin_notify_menu_enter and cleared by
   Awin_notify_menu_leave */
static int win_menu_open;
/* As long this is true, ignore all button-up events.  This is set by
  Awin_notify_menu_leave and may reset by the next button-down
  event. */
static int win_menu_was_open;

static bool
win_valid_button (struct IntuiMessage *im)
{
  int idx;
  bool down;
  bool valid; /* RESULT */
  unsigned long time_stamp;
  struct Window *win;
  local_precond (im);

  down = !(im->Code & IECODE_UP_PREFIX);

  valid = true;

  if (!down && win_menu_was_open)
    valid = false;

  idx = Awin_iecode_to_ebutton (im->Code);
  time_stamp = IM_TIME_STAMP (im);
  win = im->IDCMPWindow;

  if (down)
    {
      win_menu_was_open = false;

      if (Awin_button_up_time [idx] > (time_stamp - 75)  /* "Prellen" */
	  /* bad code to catch window activating by clicking on border
	     -bw/07-Apr-98 */
	  || (im->Code == IECODE_LBUTTON
	      && (im->MouseX < win->BorderLeft
		  || im->MouseX > (win->Width - win->BorderRight)
		  || im->MouseY > (win->Height - win->BorderBottom))))
	valid = false;
    }
  else
    {
      Awin_button_up_time [idx] = time_stamp;
      if (!(Awin_button_mask & (1<<idx))) /* already (faked) up  */
	valid = false;
    }

  Awin_button_note_change (im);

  return valid;
}

/* Get the wd which belongs to the event IM.  If the mouse is pressed,
   it returns the wd on the position the button was pressed first. */
Window
win_event_wd (struct IntuiMessage *im)
{
  Window wd = WD_NIL;
  int i;

  /* set or clear win_mouse_start_wd[] according to IM */
  if (im->Class == IDCMP_MOUSEBUTTONS)
    {
      if (!(im->Code & IECODE_UP_PREFIX))
	wd = Awin_iwin_to_wd_or_subwd (im->IDCMPWindow, im->MouseX, im->MouseY);
      win_mouse_start_wd[ Awin_iecode_to_ebutton (im->Code) ] = wd;
    }

  if (!WD_NIL_P (wd))
    return wd;
  
  for (i=0; i < 5; ++i)
    if (! WD_NIL_P (wd = win_mouse_start_wd [i]))
      return wd;

  wd = Awin_iwin_to_wd_or_subwd (im->IDCMPWindow, im->MouseX, im->MouseY);
  return wd;
}

struct win_notify_menu_context
{
  unsigned long flags;
  struct Requester req;		/* null requester to block main window IDCMP */
  int proc_prio;		/* to restore old process priority */
#define WIN_NMC_REQ (01)
#define WIN_NMC_PRIO (02)
};


/* getpriority/setpriority */
#include <sys/time.h>
#include <sys/resource.h>

/* function pair called by menu creation code */
struct win_notify_menu_context *
Awin_notify_menu_enter (FRAME_PTR f)
{
  struct win_notify_menu_context *context;
  struct Window *win;

  check_precond (f);

  win = FRAME_WIN (f);
  if (!win)
    return 0;

  context = malloc (sizeof *context);
  if (!context)
    return 0;

  context->flags = 0;

  errno = 0;
  context->proc_prio = getpriority (PRIO_PROCESS, 0);
  if (!errno && context->proc_prio > -5)
    {
      context->flags |= WIN_NMC_PRIO;
      setpriority (PRIO_PROCESS, 0, context->proc_prio - 1);
    }
  
  InitRequester (&context->req);

  if (Request (&context->req, win))
    context->flags |= WIN_NMC_REQ;

  SetWindowPointer (win,
		    WA_BusyPointer, TRUE,
		    WA_PointerDelay, TRUE,
		    TAG_DONE);
  /* seepd up Intuition */
  win__detach_clip_iwin (FRAME_WD (f));
  win_menu_open = true;
  return context;
}

void
Awin_notify_menu_leave (struct win_notify_menu_context *context, FRAME_PTR f)
{
  int i;
  struct Window *win;

  check_precond (f);

  win = FRAME_WIN (f);

  /* Fake all buttons up.  This will cause ignoring immediate
     following up-button events. */
  Awin_button_mask = 0;

  for (i=0; i < N_BUTTONS; ++i)
    {
      win_mouse_start_wd [i] = WD_NIL;
      /* Avoid "Prellen" after menu close. (???-bw/07-Apr-98) */
      //     Awin_button_up_time [i] = event_time;
    }

  FRAME_X_DISPLAY_INFO (f)->grabbed = 0;

  if (context)
    {
      if (win && context->flags & WIN_NMC_REQ)
	{
	  EndRequest (&context->req, win);
	  SetWindowPointer (win, TAG_DONE);
	}
      /* restore old priority */
      if (context->flags & WIN_NMC_PRIO)
	setpriority (PRIO_PROCESS, 0, context->proc_prio );
      free (context);
    }
  if (WIN_COMMON_USER_PORT)
    {
      struct IntuiMessage *msg;
      struct Node *succ;
      struct MsgPort *mp;

      mp = WIN_COMMON_USER_PORT;
      /* #exp#: We have a problem with unwanted mouse-button
	 events when closing menus.  This code is not a fix but
	 may help to isolate the problem. (btw: It would be nice
	 to do a similar thing on the Emacs event queue to avoid
	 queuing events leading to menu popups. --bw/15-Jul-98*/
      Forbid();
      for (msg = (struct IntuiMessage *) mp->mp_MsgList.lh_Head;
	   ((succ =  msg->ExecMessage.mn_Node.ln_Succ));
	   msg = (struct IntuiMessage *) succ)
	if(Awin_event_test ((void *)msg))
	  ;			/* not a IDCMP message, do nothing */
	else if (msg->Class == IDCMP_MOUSEBUTTONS)
	  {
	    Remove ((struct Node *) msg);
	    ReplyMsg ((struct Message *) msg);
	  }
      Permit();
    }

  win_menu_was_open = true;
  win_menu_open = false;
}

/* State of mouse buttons (XXX-bw/23-Jul-98: multi-mouse?).  The
   result is a bitmask.  If a bit is set, it means that the button
   described by bit-number is pressed.  If REMOVE_EVENTS is non zero
   all pending mouse button events will be removed+replied.

STATE: experimental */
unsigned
Awin_button_status (int remove_events)
{
  unsigned register result = Awin_button_mask;

  if (WIN_COMMON_USER_PORT)
    {
      struct IntuiMessage *msg;
      struct Node *succ;
      struct MsgPort *mp;

      mp = WIN_COMMON_USER_PORT;
      Forbid();
      for (msg = (struct IntuiMessage *) mp->mp_MsgList.lh_Head;
	   ((succ =  msg->ExecMessage.mn_Node.ln_Succ));
	   msg = (struct IntuiMessage *) succ)
	if(Awin_event_test ((void *)msg))
	  ;			/* not a IDCMP message, do nothing */
	else if (msg->Class == IDCMP_MOUSEBUTTONS)
	  {
	    if (msg->Code & IECODE_UP_PREFIX)
	      result |= (1 << Awin_iecode_to_ebutton (msg->Code));
	    else
	      result &= ~(1 << Awin_iecode_to_ebutton (msg->Code));
	    if (remove_events)
	      {
		Remove ((struct Node *) msg);
		ReplyMsg ((struct Message *) msg);
	      }
	  }
      Permit();
    }
  return (Awin_button_mask = result);
}

int dbsw_resize;
static void
Awinp_check_resize (FRAME_PTR f, struct Window *win)
{
  Window wd;
  bool new_size = false, new_pos = false;
  /* indicate whether the change was caused by user or by program */
  bool user_new_size = false, user_new_pos;
  int rows, columns;

  local_precond (f && win);

  wd = FRAME_WD (f);
  if (wd == WD_NIL || !WD_WIN_P (wd) || WD_WIN (wd) != win)
    return;

  rows = PIXEL_TO_CHAR_HEIGHT (f, WIN_GZZ_HEIGHT (win));
  columns = PIXEL_TO_CHAR_WIDTH (f, WIN_GZZ_WIDTH (win));

  new_size = (columns != f->width
	      || rows != f->height
	      || f->output_data.x->pixel_width != WIN_GZZ_WIDTH (win)
	      || f->output_data.x->pixel_height != WIN_GZZ_HEIGHT (win));

  new_pos = (f->output_data.x->left_pos != win->LeftEdge
	     || f->output_data.x->top_pos != win->TopEdge);

  user_new_size = (WD_WIDTH (wd) != win->Width 
		   || WD_HEIGHT (wd) != win->Height);
  user_new_pos = (WD_LEFT (wd) != win->LeftEdge
		  || WD_TOP (wd) != win->TopEdge);

  if ((new_size && !dbsw_resize)
      || (user_new_size && dbsw_resize))
    {
      change_frame_size (f, rows, columns, 0, 1);
      SET_FRAME_GARBAGED (f);
#ifdef AMIGA_V_EMACS_20_2
      cancel_mouse_face (f);
#endif /* AMIGA_V_EMACS_20_2 */
    }

  if (user_new_pos)
    {
      WD_LEFT (wd) = win->LeftEdge;
      WD_TOP (wd) = win->TopEdge;
    }
  if (user_new_size)
    {
      WD_WIDTH (wd) = win->Width;
      WD_HEIGHT (wd) = win->Height;
      /* remake clip region */
      win__note_resize (wd);
    }
  if (new_size)
    {
      /* GZZ width (innner rectangle) */
      f->output_data.x->pixel_width = WIN_GZZ_WIDTH (win);
      f->output_data.x->pixel_height =  WIN_GZZ_HEIGHT (win);
    }
  if (new_pos)
    {
      /* real positions (outer rectangle) */
      f->output_data.x->left_pos = win->LeftEdge;
      f->output_data.x->top_pos = win->TopEdge;
    }
}

/* Process all outstanding expose events. */
/* To gain more X compatibility we produce expose events by
   XConfigureWindow() and XClearArea().  They are not send to our
   MsgPort but stored in a static circular buffer */
void
Awinp_process_expose_events ()
{
  Window sub_wd;
  int code;
  void *data;

  while ((sub_wd = win_subwin_mp_pop (&code, &data)) != WD_NIL)
    if (code & WD_SUBWIN_EVENT_EXPOSE)
      {
	struct scroll_bar *bar = Awin_wd_to_scroll_bar (sub_wd, 0);
	if (bar)
	  x_scroll_bar_expose (bar, 0);
      }
    else if (code & WD_WIN_EVENT_EXPOSE)
      {
	struct IBox *box = data;
	struct frame *f = Awin_wd_to_frame (sub_wd, 0);
	if (f)
	  {
	    if (f->async_visible == 0)
	      {
		f->async_visible = 1;
		f->async_iconified = 0;
#ifdef AMIGA_V_EMACS_20_2
		f->output_data.x->has_been_visible = 1;
#endif /* AMIGA_V_EMACS_20_2 */
		SET_FRAME_GARBAGED (f);
	      }
	    else
	      {
		DPY_REAL_TO_GZZ_XY (FRAME_X_DISPLAY (f),
				    box->Left, box->Top);
		dumprectangle (f, box->Left, box->Top,
			       box->Width, box->Height);
	      }
	  }
      }
#if 0
    else if (code & WD_WIN_EVENT_MAP_NOTIFY)
      {
	struct frame *f = Awin_wd_to_frame (sub_wd, 0);
#if 0
	/* We use x_top_window_to_frame because map events can come
	   for subwindows and they don't mean that the frame is visible.  */
	f = x_top_window_to_frame (dpyinfo, event.xmap.window);
#endif
	if (f)
	  {
	    f->async_visible = 1;
	    f->async_iconified = 0;
	    f->output_data.x->has_been_visible = 1;

	    /* wait_reading_process_input will notice this and update
	       the frame's display structures.  */
	    SET_FRAME_GARBAGED (f);

	    if (f->iconified)
	      {
		bufp->kind = deiconify_event;
		XSETFRAME (bufp->frame_or_window, f);
		bufp++;
		count++;
		numchars--;
	      }
	    else if (! NILP (Vframe_list)
		     && ! NILP (XCONS (Vframe_list)->cdr))
	      /* Force a redisplay sooner or later
		 to update the frame titles
		 in case this is the second frame.  */
	      record_asynch_buffer_change ();
	  }
      }
#endif
}

static void
Awinp_produce_damage_expose_events (Window wd)
{
  struct Region *reg;
  struct RegionRectangle *rr;
  Window i;

  local_precond (WD_WIN_P (wd));

  reg = WD_LAYER (wd)->DamageList;

  if (!reg)
    return;

  for (rr = WD_LAYER (wd)->DamageList->RegionRectangle; rr; rr = rr->Next)
    {
      struct IBox box = { reg->bounds.MinX + rr->bounds.MinX,
			  reg->bounds.MinY + rr->bounds.MinY,
			  reg->bounds.MinX + rr->bounds.MaxX - rr->bounds.MinX + 1,
			  reg->bounds.MinY + rr->bounds.MaxY - rr->bounds.MinY + 1 };
      win_subwin_mp_push (wd, WD_WIN_EVENT_EXPOSE, &box);
      WD_CHILD_FOREACH (wd, i)
	if (!WD_MAPPED_P (i)
	    /* test of non overlapping */
	    || WD_RIGHT (i) < reg->bounds.MinX + rr->bounds.MinX
	    || WD_LEFT (i) > reg->bounds.MinX + rr->bounds.MaxX
	    || WD_BOTTOM (i) < reg->bounds.MinY + rr->bounds.MinY
	    || WD_TOP (i) > reg->bounds.MinY + rr->bounds.MaxY)
	  continue;
	else
	  win_subwin_mp_push (i, WD_SUBWIN_EVENT_EXPOSE, 0);
    }
}

static inline void
Awinp_handle_refresh_window_event (struct Window *win)
{
  Window wd = WIN_WD (win), i;
  struct Region *region;
  local_precond (WD_WIN_P (wd)
		 && WD_WIN (wd) == win
		 && WD_MAPPED_P (wd)
		 && WD_SCREEN_P (WD_PARENT (wd)));

  SetAPen (win->RPort,
	   win_color_pen (WD_COLCON (WD_PARENT (wd)),
			  WD_COLOR_B (wd)));
  region = InstallClipRegion (win->RPort->Layer, 0);
  BeginRefresh (win);
  RectFill (win->RPort,
	    win->BorderLeft, win->BorderTop,
	    win->BorderLeft + win->GZZWidth - 1,
	    win->BorderTop + win->GZZHeight - 1);
  Awinp_produce_damage_expose_events (wd);
  EndRefresh (win, TRUE);
  InstallClipRegion (win->RPort->Layer, region);
}

/* called by menu code when closing a menu box */
void
Awinp_refresh_windows (Window parent)
{
  Window i;
  check_precond (WD_SCREEN_P (parent));

  WD_CHILD_FOREACH (parent, i)
    if (WD_MAPPED_P (i) && WD_WIN_P (i) 
	&& (WD_WIN (i)->Flags & WFLG_SIMPLE_REFRESH))
      Awinp_handle_refresh_window_event (WD_WIN (i));      
  Awinp_process_expose_events ();
}

/* process refresh events while in ASL request */
ULONG __saveds __interrupt
Awinp_asl_idcmp_hook (struct Hook *hook __asm("a0"),
		      struct IntuiMessage *im __asm("a1"),
		      struct FileRequester *req __asm("a2"))
{
  if (im->Class == IDCMP_REFRESHWINDOW)
    Awinp_handle_refresh_window_event (im->IDCMPWindow);
  Awinp_process_expose_events ();
  return 0;
}
struct Hook awinp_asl_idcmp_hook = {{0,}, (void*)Awinp_asl_idcmp_hook, };

/* Macros to get/reply messages doing some caching for our
   last_event_frame. */
static struct Window *last_event_win;
static FRAME_PTR last_event_frame;
static Window last_event_wd;

#define SET_LAST_EVENT_WIN_(win_)				\
  ((void)((last_event_win == win_)				\
	  || ((last_event_win = win_), (last_event_frame = 0),	\
	      (last_event_wd = WD_NIL))))
 /* Allow message reply inside the switch body */
#define XREPLY_MSG(MSG) ((MSG) && (ReplyMsg ((struct Message*)(MSG)),1) && (MSG=0))
 /* Get IMessage and update cache */
#define XGET_IMSG(IM) (win_x_get_im (WIN_COMMON_USER_PORT, &(IM)) \
		       && ((SET_LAST_EVENT_WIN_ ((IM)->IDCMPWindow), 1)))

/* optimized processing of mouse move events */
/* The returned value is the first non-mouse-move event (IM itself or
   one of its successors) or 0 */
static inline struct IntuiMessage *
Awinp_process_mouse_move (struct IntuiMessage *im)
{
  /* IM2 holds the latest MOUSEMOVE event of a group of such events */
  struct IntuiMessage *im2;

  for (im2=0; (im && im->Class == IDCMP_MOUSEMOVE); XGET_IMSG (im))
    {
      XREPLY_MSG(im2);
      im2 = im;
    }

  if (im2)
    {
      Window wd;
      struct x_display_info *dpyinfo;
      struct frame *f;

      assert (WD_WIN_P (WIN_WD (im2->IDCMPWindow)));
      dpyinfo = WIN_DPY_INFO (im2->IDCMPWindow);
      wd = win_event_wd (im2);

      f = ((dpyinfo && dpyinfo->grabbed
	    && last_mouse_frame && FRAME_LIVE_P (last_mouse_frame))
	   ? last_mouse_frame
	   : ((WD_WIN_P (wd))
	      ? Awin_wd_to_frame (wd, 0) 
	      : 0));

      if (f)
	note_mouse_movement (f, im2, false);
      else
	{
	  struct scroll_bar *bar;

	  if ((bar = Awin_wd_to_scroll_bar  (wd, 0)))
	    x_scroll_bar_note_movement (bar, im2);

	  /* If we move outside the frame,
	     then we're certainly no longer on any text in the frame.  */
	  if (dpyinfo)
	    clear_mouse_face (dpyinfo);
	}
      XREPLY_MSG (im2);
    }
  return im;
}

/* Read events coming from the X server.
   This routine is called by the SIGIO handler.
   We return as soon as there are no more events to be read.

   Events representing keys are stored in buffer BUFP,
   which can hold up to NUMCHARS characters.
   We return the number of characters stored into the buffer,
   thus pretending to be `read'.

   WAITP is nonzero if we should block until input arrives.
   EXPECTED is nonzero if the caller knows input is available.  */

int
XTread_socket (int sd, struct input_event *bufp, int numchars,
#ifndef AMIGA_V_EMACS_20_1
 /* In Emacs-19 this parameter exist but is not used by xterm.c:XTread_socket */
int waitp,
#endif /* not AMIGA_V_EMACS_20_1 */
 /* In Emacs-19/20 this parameter exist but is not used by xterm.c:XTread_socket */
 int expected)
{
#define waitp 0 /* disable waitp handling */
  struct input_event *buf_startp, *buf_endp; /* RESULT == (bufp - buf_startp) */
  long pmask;
  extern volatile int interrupt_input_pending;
  extern volatile int input_signal_count; /* actually a <static volatile int> */
  SIGMASKTYPE sig_mask;

  check_precond (sd >= 0);
  check_precond (bufp);
  check_precond (numchars > 0);
  check_precond (WIN_COMMON_USER_PORT);

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
  BLOCK_INPUT;
  sig_mask = sigblock (sigmask (SIGINT));

  /* So people can tell when we have read the available input.  */
  input_signal_count++;

  last_event_win = 0;

#ifdef AMIGA_SET_QUIT
  {
    /* Clear quit_flag set by soft interrupt. XXX-bw/16-Nov-97:
       This seems to be the wrong way. */
    extern Lisp_Object Vquit_flag;
    Vquit_flag = Qnil;
  }
#endif /* AMIGA_SET_QUIT */
  buf_startp = bufp;
  buf_endp = bufp + numchars;

#if 0
  if (expected)
    {
      char c;
      read (sd, &c, 1);		/* Empty dummy input file. XXX */
    }
#endif

#define SET_FRAME_AND_TIME_							 \
  ({										 \
     if (last_event_frame)							 \
       XSETFRAME (bufp->frame_or_window, (f=last_event_frame));			 \
     else if (!(f = last_event_frame						 \
		= Awin_iwin_to_frame (im->IDCMPWindow, &bufp->frame_or_window))) \
       continue;								 \
     bufp->timestamp = IM_TIME_STAMP (im);			 \
  })

  pmask  = Awin_input_sigmask;	/* fake IDCMP signal (it does no harm anyway) */
  while (1)			/* only once when !waitp */
    {
     if (pmask & SIGBREAKF_CTRL_C)
	kill (getpid (), SIGINT);

      if (pmask & SIGBREAKF_CTRL_F)
	;			/* do deiconify/iconify ??? */

      /* process expose events.  this will done below again. */
      Awinp_process_expose_events ();

      if (pmask & (Awin_input_sigmask))
	{
	  struct IntuiMessage *im;

	  for (; ((bufp+1 < buf_endp) /* 1 Amiga event may produce 2 Emacs events */
		  && XGET_IMSG (im));
	       XREPLY_MSG(im), interrupt_input_pending = 0)
	    {
	      FRAME_PTR f;
	      struct x_display_info *dpyinfo;

	      im = Awinp_process_mouse_move (im);
	      if (!im)
		break;

	      assert (WD_WIN_P (WIN_WD (im->IDCMPWindow)));
	      dpyinfo = WIN_DPY_INFO (im->IDCMPWindow);
	      f = 0;

	      switch (im->Class) 
		{
		case IDCMP_NEWSIZE: /* frame resize */
#if 0
		  SET_FRAME_AND_TIME_;
		  if (f)
		    {
		      SET_FRAME_GARBAGED (f);
		      // win__note_resize (FRAME_WD (f));
		    }
#endif
		  break;
		  
		case IDCMP_CHANGEWINDOW:
		    if (im->Code == CWCODE_MOVESIZE)
		      {
			SET_FRAME_AND_TIME_;
			Awinp_check_resize (f, im->IDCMPWindow);
		      }
		    break;

		case IDCMP_REFRESHWINDOW:
		  {
		    struct Window *win = im->IDCMPWindow;
		    if ((win->Flags & WFLG_SIMPLE_REFRESH)
			&& win->RPort->Layer->DamageList /* paranoia */
			&& win->RPort->Layer->DamageList->RegionRectangle)
		      Awinp_handle_refresh_window_event (win);
		    else
		      {
		        BeginRefresh (win);
		        EndRefresh (win, TRUE);
		      }
		  }
		  break;

		case IDCMP_CLOSEWINDOW:
		  /* iconify_event  or delete_window_event */
		  SET_FRAME_AND_TIME_;
		  if (f)
		    {
		      if (im->Qualifier & (IEQUALIFIER_LSHIFT | IEQUALIFIER_RSHIFT))
			{
			  bufp->kind = delete_window_event;
			  ++bufp;
			}
		      else
			/* Iconify */
			{
			  struct Screen *screen;
			  Window wd;

			  screen = im->IDCMPWindow->WScreen;
			  wd = Awin_iwin_to_wd (im->IDCMPWindow);
			  if (wd == WD_NIL)
			    continue; /* =>GetMsg() loop */

			  XREPLY_MSG (im); /* because we close the window now */

			  XIconifyWindow (FRAME_X_DISPLAY (f), wd, screen);
			  WD_MAP_NOTIFY_SET (wd, false);
			  f->async_visible = 0;
			  f->async_iconified = 1;
#ifdef AMIGA_V_EMACS_20_2
			  f->output_data.x->has_been_visible = 1;
#endif /* AMIGA_V_EMACS_20_2 */
			  bufp->kind = iconify_event;
			  XSETFRAME (bufp->frame_or_window, f);
			  ++bufp; 
			}
		    }
		  break;

		case IDCMP_RAWKEY:
		  {
		    unsigned emacs_key;
		    int modifiers_filter=0; /* bits to clear in
					       bufp->modifiers before
					       return */

		    /* skip upstrokes */
		    if (im->Code & IECODE_UP_PREFIX)
		      continue; /* => GetMsg () loop */

		    bufp->kind = non_ascii_keystroke; /* may overridden in switch*/
		    bufp->modifiers = 0; /* ored by mouse and at end */
		    emacs_key = ((!win_kp_numlock) 
				 ? KM_AMIGA_TO_EMACS (im->Code)
				 : KM_AMIGA_TO_EMACS_NUML (im->Code));

		    if (emacs_key)
		      bufp->code = 0xff00 + emacs_key;
		    else
		      {
			struct InputEvent ie;
			char c;
			long emacs_quals;

			emacs_quals = (ALTLEFT  /* Meta key */
				       | IEQUALIFIER_CONTROL 
				       | AMIGALEFT  /* Hyper key */
				       | AMIGARIGHT); /* Super key */

			bufp->kind = ascii_keystroke;

			ie.ie_Class = IECLASS_RAWKEY;
			ie.ie_SubClass = 0;
			ie.ie_Code = im->Code;
			ie.ie_Qualifier = im->Qualifier & ~emacs_quals;
			/* ie_EventAddress represents here the dead
                           key info struct, which is member of the
                           same 4 byte union */
			ie.ie_EventAddress = (APTR *) *((ULONG *)im->IAddress);
			/* avoid dead key conflicts (M-g i, M-h u, ...) */
			ie.ie_Prev1DownQual &= ~emacs_quals;
			ie.ie_Prev2DownQual &= ~emacs_quals;

			if (MapRawKey(&ie, &c, 1, 0) != 1)
			  continue; /* error => GetMsg() loop */
			bufp->code = c;
			modifiers_filter |= CHAR_ALT; /* disable ALT  */
		      }
		    bufp->modifiers 
		      = x_x_to_emacs_modifiers (0 /* dpyinfo */, im->Qualifier);
		    bufp->modifiers &= ~modifiers_filter;

		    SET_FRAME_AND_TIME_;
		    ++bufp;
		  }
		  break;

		case IDCMP_MOUSEBUTTONS:
		  if (!win_valid_button (im))
		    continue;

		    win_mouse_pos_iwin = im->IDCMPWindow;
		    win_mouse_pos_x = im->MouseX;
		    win_mouse_pos_y = im->MouseY;
		    win_mouse_pos_time = IM_TIME_STAMP (im);
		    WIN_REAL_TO_GZZ_XY (win_mouse_pos_iwin, win_mouse_pos_x,
					win_mouse_pos_y);
	      {
		/* If we decide we want to generate an event to be seen
		   by the rest of Emacs, we put it here.  */
		Window wd;
		struct input_event emacs_event;

		emacs_event.kind = no_event;
		
#ifdef NBW
		bzero (&compose_status, sizeof (compose_status));
#endif /* NBW */
		  wd = win_event_wd (im);
		  f = (WD_WIN_P (wd)) ? Awin_wd_to_frame (wd, 0) : 0;

		  if (dpyinfo && dpyinfo->grabbed
		      && last_mouse_frame && FRAME_LIVE_P (last_mouse_frame))
		    f = last_mouse_frame;

		if (f)
		  {
		    if (dpyinfo 
			&& (!dpyinfo->x_focus_frame || f == dpyinfo->x_focus_frame))
		      construct_mouse_click (&emacs_event, im, f);
		  }
		else
		  {
		    struct scroll_bar *bar = Awin_wd_to_scroll_bar  (wd, 0);

		    if (bar)
		      x_scroll_bar_handle_click (bar, im, &emacs_event);
		  }

		if (f)
		  if (!(im->Code & IECODE_UP_PREFIX))
		    {
		      FRAME_X_DISPLAY_INFO (f)->grabbed |= (1 << Awin_iecode_to_ebutton (im->Code));
		      last_mouse_frame = f;
		    }
		  else
		    {
		      int button = Awin_iecode_to_ebutton (im->Code);
		      FRAME_X_DISPLAY_INFO (f)->grabbed &= ~(1 << button);
		      win_mouse_start_wd [button] = WD_NIL;
		    }

		if (emacs_event.kind != no_event)
		  {
		    bcopy (&emacs_event, bufp, sizeof (struct input_event));
		    ++bufp;
		  }
	      }
	      break;

		  /* TODO-bw/10-Nov-97: check the activate/inactivate code */
		case IDCMP_ACTIVEWINDOW:
		  SET_FRAME_AND_TIME_;

		  /* MapNotify */
		  if (f && WD_MAP_NOTIFY (FRAME_X_WINDOW (f)))
		    {
		      WD_MAP_NOTIFY_SET (FRAME_X_WINDOW (f), false);
		      f->async_visible = 1;
		      f->async_iconified = 0;

#ifdef AMIGA_V_EMACS_20_2
		      f->output_data.x->has_been_visible = 1;
#endif /* AMIGA_V_EMACS_20_2 */

		      /* wait_reading_process_input will notice this and update
			 the frame's display structures.  */
		      SET_FRAME_GARBAGED (f);

		      if (f->iconified)
			{
			  f->async_visible = 1;
			  f->async_iconified = 0;
			  bufp[1] = bufp[0]; /* copies fields frame_or_window, time */
			  bufp->kind = deiconify_event;
			  ++bufp;
			}
		      else if (! NILP (Vframe_list)
			       && ! NILP (XCONS (Vframe_list)->cdr))
			/* Force a redisplay sooner or later
			   to update the frame titles
			   in case this is the second frame.  */
			record_asynch_buffer_change ();

		  /* Workaround when opening an AutoAdjust window not
                     fitting on the screen. */
		      if ((f->output_data.x->pixel_width >
		       (im->IDCMPWindow->WScreen->Width 
			- im->IDCMPWindow->BorderLeft 
			- im->IDCMPWindow->BorderRight))
		      || (f->output_data.x->pixel_height >
			  (im->IDCMPWindow->WScreen->Height 
			   - im->IDCMPWindow->BorderTop 
			   - im->IDCMPWindow->BorderBottom)))
                    Awinp_check_resize (f, im->IDCMPWindow);
		    }

		  /* EnterNotify */

		  bzero (&win_mouse_start_wd, sizeof win_mouse_start_wd);
		  {
		    int i;
		    unsigned long event_time;
		    /* calculated by SET_FRAME_AND_TIME_ */
		    
		    event_time = bufp->timestamp;

#if 0 /* Reset grabbed info, to fix problems with unpaired button
events caused by pop up menues XXX-bw/07-Apr-98: CHECKME */
		    dpyinfo->grabbed = 0;
		    for (i=0; i < N_BUTTONS; ++i)
		      {
			win_mouse_start_wd [i] = WD_NIL;
			Awin_button_mask &= ~(1<<i);
			/* Avoid "Prellen" after menu close. (???-bw/07-Apr-98) */
			Awin_button_up_time [i] = event_time;
		      }
#endif /* 1 */

		    if (1) /* event.xcrossing.focus) /* Entered Window */
		      {
			/* Avoid nasty pop/raise loops. */
			if (f && (!(f->auto_raise)
				  || !(f->auto_lower)
				  || (event_time - enter_timestamp) > 500))
			  {
			    x_new_focus_frame (dpyinfo, f);
			    enter_timestamp = event_time;
			  }
		      }
		    else if (f == dpyinfo->x_focus_frame)
		      x_new_focus_frame (dpyinfo, 0);

		    /* EnterNotify counts as mouse movement,
		       so update things that depend on mouse position.  */
		    if (f)
		      note_mouse_movement (f, im, false);

		  }
		  break;

		case IDCMP_INACTIVEWINDOW:
		  SET_FRAME_AND_TIME_;

		  /* LeaveNotify */
		    if (f == dpyinfo->mouse_face_mouse_frame)
		      /* If we move outside the frame,
			 then we're certainly no longer on any text in the frame.  */
		      clear_mouse_face (dpyinfo);

		    if (0)	/* event.xcrossing.focus) */
		      x_mouse_leave (dpyinfo);
		    else
		      {
			if (f == dpyinfo->x_focus_event_frame)
			  dpyinfo->x_focus_event_frame = 0;
			if (f == dpyinfo->x_focus_frame)
			  x_new_focus_frame (dpyinfo, 0);
		      }
		  /* XXX-bw/06-Mar-98: Remove _all_ (!) clip-regions
                     for performance. */
		  win__detach_clip_iwin (FRAME_WD (f));
	      break;
		default:
		OTHER:
	      break;
		}
	    }
	}
      Awinp_process_expose_events ();


     if (pmask & (Awin_input_sigmask))
       {
	 struct AppMessage *am;

	 for ( ; (bufp < buf_endp) 
	      && (am = (struct AppMessage *) GetMsg (WIN_WB_APP_PORT));
	      XREPLY_MSG (am))
	   switch (am->am_Type)
	     {
	     case AMTYPE_APPWINDOW:
	       {
		 FRAME_PTR f;

		 f = Awin_wd_to_frame (am->am_ID, &bufp->frame_or_window);
		 if (f)
		   {
		     bufp->kind = amiga_app_message_event;
		     /* the time stamp in am seems invalid TODO-bw/07-Nov-97 */
		     bufp->timestamp = AM_TIME_STAMP (am);
		     XSETFASTINT (bufp->x, PIXEL_TO_CHAR_COL (f, am->am_MouseX));
		     XSETFASTINT (bufp->y, PIXEL_TO_CHAR_ROW (f, am->am_MouseY));
		     bufp->modifiers = 0; /* drop on frame */
#if 0 //ndef AMIGA_NEW_WBAPP
		     bufp->code = Awin_wbapp_get_file_args (am);
		     ++bufp;
#else
		     XSETINT (bufp->code, Awin_wbapp_get_file_args (am));
		     if (XINT (bufp->code))
		       ++bufp;
#endif
		   }
	       }
	       break;

	     case AMTYPE_APPICON:
	       {
		 FRAME_PTR f;
		 f = Awin_wd_to_frame (WD_PARENT (am->am_ID), &bufp->frame_or_window);
		 if (f)
		   {
		     Window wd, icon;
		     int i;

		     /* deiconify */
		     icon = am->am_ID;
		     wd = WD_PARENT (icon);

		     // XREPLY_MSG (am); /* really needed here? */

		     /* XDestroyWindow (icon); */
		     XMapRaised (FRAME_X_DISPLAY (f), wd);

		     f->async_visible = 1;
		     f->async_iconified = 0;

		     /* wait_reading_process_input will notice this and update
			the frame's display structures.  */
		     SET_FRAME_GARBAGED (f);

		     if (f->iconified)
		       {
			 /* the time stamp in am seems invalid TODO-bw/07-Nov-97 */
			 bufp->timestamp =  AM_TIME_STAMP (am);
			 bufp->kind = deiconify_event;
			 bufp++;
		       }
		     else
		       /* Force a redisplay sooner or later
			  to update the frame titles
			  in case this is the second frame.  */
		       record_asynch_buffer_change ();
		   }
		     bufp->kind = amiga_app_message_event;
		     bufp->modifiers = 1; /* drop on icon */
		     /* the time stamp in am seems invalid TODO-bw/07-Nov-97 */
		     bufp->timestamp =  AM_TIME_STAMP (am);
		     XSETFASTINT (bufp->x, am->am_MouseX);
		     XSETFASTINT (bufp->y, am->am_MouseY);
#if 0 //ndef AMIGA_NEW_WBAPP
		     bufp->code = Awin_wbapp_get_file_args (am);
		     ++bufp;
#else
		     XSETINT (bufp->code, Awin_wbapp_get_file_args (am));
		     if (XINT (bufp->code))
		       ++bufp;
#endif
	       }
	       break;
	     }
       }

      /* reinit Exec signals */
      if (waitp && (bufp == buf_startp))
	{
	  pmask = Awin_input_sigmask | SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_F;
	  ix_wait (&pmask);
	}
      else
	break;
    }

  /* If the focus was just given to an autoraising frame,
     raise it now.  */
  /* ??? This ought to be able to handle more than one such frame.  */
  if (pending_autoraise_frame)
    {
      x_raise_frame (pending_autoraise_frame);
      pending_autoraise_frame = 0;
    }

  sigsetmask (sig_mask);
  UNBLOCK_INPUT;
  return bufp - buf_startp;
}
