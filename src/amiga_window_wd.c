#include "amiga_window_.h" /* internal component header */
#include "amiga_window_defs.h"

#ifdef emacs
#include "syssignal.h"
#endif

#include <ix.h>			/* for ix_wait() */
#include <time.h>
#include <unistd.h>

struct DiskObject *
win__icon_dobj_make_from_bits (unsigned char *bits, int pix_per_row, int nmb_rows);
void win__icon_dobj_destroy (struct DiskObject *dobj);
void win__prepare_1 (int wd, struct RastPort *rp, GC gc, int mask, void *colcon);

#include "gnu.h"
#define win__icon_dobj_create() \
 win__icon_dobj_make_from_bits (gnu_bits, gnu_width, gnu_height)

/* hold pointers to different type (use WD_TYPE()) of objects.  */
struct MsgPort *win_user_port, *win_wb_app_port;
void *win_desc_tbl[WD_MAX];
volatile struct win_info win_info_tbl[WD_MAX];
int win_desc_greatest;		/* the greatest in use wd */
struct ExtNewWindow win_new_win;
struct TagItem win_pref[100], *win_pp = win_pref;
int win_fid_index;
struct TextFont *win_font_tbl[FONT_MAX];
unsigned char win_font_style_tbl[FONT_MAX];
/* options */
int win_simple_refresh;


#define WD_SUBWIN_PORT_SIZE (100)
#define WD_SUBWIN_PORT_EMPTY() (win_subwin_port_front == win_subwin_port_back)
static struct 
{
  short wd, code;
  union 
  {
    struct IBox box;		/* exposing area */
  } data;
} win_subwin_port [WD_SUBWIN_PORT_SIZE];
static short win_subwin_port_front, win_subwin_port_back;

void
win_subwin_mp_push (int wd, int code, void *data)
{
  int current;

  current = win_subwin_port_back++;
  win_subwin_port_back %= WD_SUBWIN_PORT_SIZE;

  win_subwin_port[current].wd = wd;
  win_subwin_port[current].code = code;
  if (code == WD_WIN_EVENT_EXPOSE && data)
    win_subwin_port[current].data.box = *(struct IBox *)data;

  interrupt_input_pending = 1;
}

int
win_subwin_mp_pop (int *code_return, void *data_return)
{
  int current;

  if (! WD_SUBWIN_PORT_EMPTY ())
    {
      int current = win_subwin_port_front++;
      win_subwin_port_front %= WD_SUBWIN_PORT_SIZE;
      if (code_return)
	*code_return = win_subwin_port[current].code;
      if (data_return)
	*(void **)data_return = &win_subwin_port[current].data;
      return win_subwin_port[current].wd;
    }
  return WD_NIL;
}

/* remove all events related to WD */
void
win_subwin_mp_remove_all (Window wd)
{
  int i, old_back;

  old_back = win_subwin_port_back;
  for (i = win_subwin_port_front; i != old_back; 
       ++i, (i %= WD_SUBWIN_PORT_SIZE))
    if (win_subwin_port[i].wd != wd)
      {
	win_subwin_port[win_subwin_port_back++] = win_subwin_port[i];
	win_subwin_port_back %= WD_SUBWIN_PORT_SIZE;
      }
  win_subwin_port_front = old_back;
}


/* sending of Map events */
struct win_event win_events[20];
/* test whether MSG is both valid and produced by Awin_put_event() */
bool
Awin_event_test (const struct Message *msg)
{
  return (msg->mn_ReplyPort == 0
	  && (char *)win_events <= (char *)msg
	  && (char *)msg < (char *)win_events + sizeof win_events
	  && msg->mn_Node.ln_Type != NT_FREEMSG);
}

bool
Awin_event_put (Window wd, int key)
{
  int i;

  if (WIN_COMMON_USER_PORT)
    for (i=0; i < sizeof win_events / sizeof win_events[0]; ++i)
      if (win_events[i].msg.mn_Node.ln_Type == NT_FREEMSG
	  || (win_events[i].msg.mn_Node.ln_Type == 0
	      && win_events[i].time_stamp == 0))
	{
	  struct win_event *event = &win_events[i];
	  ULONG secs, micros;

	  CurrentTime (&secs, &micros);
	  event->time_stamp = secs * 1000 + micros / 1000;
	  event->wd = wd;
	  event->key = key;
	  event->msg.mn_Length = sizeof *event;
	  PutMsg (WIN_COMMON_USER_PORT, &event->msg);
	  return true;
	}
  return false;
}


/* experimental OO dispatcher table */

struct wd_methods
{
  bool (*wd_create) (int wd);
  void (*wd_destroy) (int wd);
  bool (*wd_attach) (int child, int parent);
  void (*wd_detach) (int child);
  bool (*wd_map) (int wd);
  bool (*wd_un_map) (int wd);
  void (*wd_flush) (int wd);
  struct RastPort *(*wd_prepare) (int wd, GC gc, int mask, int *x, int *y);
};

#define wd_declare(class_) \
bool win__create_##class_ (int wd); \
void win__destroy_##class_ (int wd); \
bool win__attach_##class_ (int child, int parent); \
void win__detach_##class_ (int child); \
bool win__map_##class_ (int wd); \
bool win__un_map_##class_ (int wd); \
void win__flush_##class_ (); \
struct RastPort *win__prepare_##class_ (int wd, GC gc, int mask, int *x, int *y);

wd_declare (void_);
wd_declare (iscreen);
wd_declare (iwin);
wd_declare (nwin);
wd_declare (boopsi);
wd_declare (icon);
wd_declare (subwin);

#define wd_define(create_, destroy_, attach_, detach_, map_, un_map_, flush_, prepare_)	\
{ win__create_##create_, win__destroy_##destroy_,				\
  win__attach_##attach_, win__detach_##detach_,					\
  win__map_##map_, win__un_map_##un_map_,					\
  win__flush_##flush_,								\
  win__prepare_##prepare_,							\
}
struct RastPort *win__prepare_noop_ (int wd, GC gc, int mask, int *x, int *y)
 { return 0; }

struct wd_methods wd_subclasses[]
= {
    /* WT_FREE (== void) */
    wd_define (void_, void_, void_, void_, void_, void_, void_, noop_),
    /* WT_ISCREEN */
    wd_define (iscreen, iscreen, void_, void_, void_, void_, void_, noop_),
    /* WT_IWIN */
    wd_define (iwin, iwin, void_, void_, void_, iwin, iwin, iwin),
    /* WT_NWIN */
    wd_define (nwin, nwin, nwin, nwin, nwin, void_, void_, noop_),
    /* WT_BOOPSI */
    wd_define (boopsi, boopsi, boopsi, boopsi, void_, void_, boopsi, noop_),
    /* WT_ICON */
    wd_define (icon, icon, icon, icon, void_, void_, void_, noop_),
    /* WT_IMAGE */
    wd_define (void_, void_, void_, void_, void_, void_, void_, noop_),
    /* WT_SUBWIN */
    wd_define (subwin, subwin, subwin, subwin, subwin, subwin, subwin, subwin),
  };

#define wd_message(wd, msg, args...) \
(*wd_subclasses[WD_INFO(wd)->type].wd_##msg) ((wd) ,##args)

bool win__create_void_ (int wd) { assert (! "win__create_void_"); }
void win__destroy_void_ (int wd) { assert (! "win__destroy_void_()"); }
bool win__attach_void_ (int child, int parent) { assert (! "win__attach_void_"); }
void win__detach_void_ (int child) { assert (! "win__detach_void_()"); }
bool win__map_void_ (int wd) { assert (! "win__map_void_"); }
bool win__un_map_void_ (int wd) { assert (! "win__un_map_void_()"); }
void win__flush_void_ (int wd) { assert (! "win__flush_void_()"); }
struct RastPort *win__prepare_void_ (int wd, GC gc, int mask, int *x, int *y)
 { assert (! "win__prepare_void_()"); }
;

/* virtual interface */
bool
win__map (int wd)
{
  SIGMASKTYPE sig_mask;
  bool result;

  sig_mask = sigblock (sigmask (SIGINT));

  result = wd_message (wd, map); 
  
  sigsetmask (sig_mask);
  return result;
}

bool
win__un_map (int wd)
{
   SIGMASKTYPE sig_mask;
   bool result;

  sig_mask = sigblock (sigmask (SIGINT));

  result = wd_message (wd, un_map); 

  sigsetmask (sig_mask);
  return result;
}

void
win__detach (Window wd) 
{
  SIGMASKTYPE sig_mask;

  check_precond (WD_VALID_P (wd));

  sig_mask = sigblock (sigmask (SIGINT));

  wd_message (wd, detach);

  sigsetmask (sig_mask);
}

/* unlink both from system lists and our owns */
bool
win__attach (int wd, int child)
{
  SIGMASKTYPE sig_mask;
  bool result;

  check_precond (WD_VALID_P (child) && WD_VALID_P (wd));
  check_precond ((WD_SCREEN_P (wd) && WD_NWIN_P (child)) 
		 || ((WD_WIN_P (wd) || WD_NWIN_P (wd)) 
		     && (WD_SUBWIN_P (child) || WD_ICON_P (child) 
			 || WD_BOOPSI_P (child))));
  
  sig_mask = sigblock (sigmask (SIGINT));

  result = wd_message (child, attach, wd);

  sigsetmask (sig_mask);
  return result;
}

/* Change buffer */
/* Because Emacs assumes X, we implement buffering for window-box
   changing.  Otherwise the window may resized several times by one
   function (like set-default-font)
   FIXME-bw/04-Dec-97 (Not true! Remove this code))*/
int win_change_tbl[WD_CHANGE_TBL_SIZE], win_change_idx;

int win_flushing;
void
win__flush ()
{
  int i;

  if (win_flushing)
    return;
  else
    win_flushing = 1;

  /* FIXME-bw:##exp## use faster algorithm */
  for (i=0; i < win_change_idx; ++i)
    if (WD_BOOPSI_P (win_change_tbl[i]) || WD_SUBWIN_P (win_change_tbl[i]))
	wd_message (win_change_tbl[i], flush);

  for (i=0; i < win_change_idx; ++i)
    if (!(WD_BOOPSI_P (win_change_tbl[i]) || WD_SUBWIN_P (win_change_tbl[i])))
      wd_message (win_change_tbl[i], flush);
  
  win_change_idx = 0;

    win_flushing = 0;
}

void
win__wd_flush (int wd)
{
  if (!win_flushing)
    wd_message (wd, flush);
}

struct RastPort * /* 0==invisible */
win__prepare (int wd, GC gc, int mask, int *x, int *y)
{
  SIGMASKTYPE sig_mask;
  struct RastPort *rp;		/* RESULT */

  sig_mask = sigblock (sigmask (SIGINT));

  if (!win_flushing && win_change_idx)
    win__flush (); //wd_message (wd, flush);
  rp = wd_message (wd, prepare, gc, mask, x, y);

  sigsetmask (sig_mask);
  return rp;
}


/* Background fill hook */
#ifndef WIN_BACKFILL
#define WIN_BACKFILL_HOOK 0
#else
#define WIN_BACKFILL_HOOK (&background_hook + 0)
struct fill;
static ULONG __saveds __interrupt
fill_background (struct RastPort *obj __asm("a2"),
		 struct fill *msg __asm("a1"));
struct Hook background_hook
= { {0,}, (ULONG (*)())fill_background, 0, 0};
#endif /* WIN_BACKFILL */

static int
win__pref ()
{
  struct ExtNewWindow *nw;

  nw = &win_new_win;
  OBJ_ZERO (nw);

  nw->LeftEdge = WIN_LEFT;
  nw->TopEdge = WIN_TOP;
  nw->Width = WIN_WIDTH;
  nw->Height = WIN_HEIGHT;
  nw->DetailPen = 1; /* XXX-bw/06-Apr-98 */
  nw->BlockPen = 0;
  nw->IDCMPFlags = 0;
  nw->Flags 
    = (WFLG_NW_EXTENDED
       | (WIN_SIMPLE_REFRESH
	  ? (WFLG_SIMPLE_REFRESH)
	  : (WFLG_NOCAREREFRESH))
       | (WIN_MENU 
	  ? (WFLG_NEWLOOKMENUS)
	  : (WFLG_RMBTRAP))
       | (WIN_BACKDROP 
	  ? (WFLG_BACKDROP | WFLG_BORDERLESS)
	  : (WFLG_DRAGBAR | WFLG_CLOSEGADGET | WFLG_DEPTHGADGET
	     | WFLG_SIZEBBOTTOM | WFLG_SIZEGADGET)));

#if (WIN_TITLE)
  if (!WIN_BACKDROP)
    nw->Title = WIN_TITLE;
#endif
  nw->Screen = 0;
  nw->MinWidth = 50;
  nw->MinHeight = 50;
  nw->MaxWidth = ~0;;
  nw->MaxHeight = ~0;
  nw->Type = 0;
  nw->Extension = win_pref;

  win__wa_push (WA_AutoAdjust, TRUE);
  win__wa_push (WA_NewLookMenus, TRUE);
  win__wa_push (WA_MenuHelp, TRUE);
  if (WIN_BACKFILL_HOOK)
    win__wa_push (WA_BackFill, WIN_BACKFILL_HOOK);
  return 0;
}

static bool awin_db_in_clip_reg_calc;

static bool
win___clip_reg_calc (int wd)
{
  bool success;
  struct Rectangle rect;
  struct Region *tmp;
  struct Window *win;
  int i;

  check_precond (awin_db_in_clip_reg_calc == 0), ++awin_db_in_clip_reg_calc;

  local_precond (WD_VALID_P (wd) && WD_WIN_P (wd));
  local_precond (WD__CLIP_REGION (wd));

  win = WD_WIN (wd);

  InstallClipRegion (WD_LAYER (wd), 0);
  WD_CLIPPED_SET (wd, false);

  ClearRegion (WD__CLIP_REGION (wd));
  WD_CHILD_FOREACH (wd, i)
      if (WD_MAPPED_P (i))
	{
	  WD_CLIPPED_SET (i, false);
	  WD_RECTANGLE (i, &rect);
	  if (!OrRectRegion (WD__CLIP_REGION (wd), &rect))
	    goto fail;
	  if (WD__CLIP_REGION (i)) 
	    {	    
	      ClearRegion (WD__CLIP_REGION (i));
	      if (!OrRectRegion (WD__CLIP_REGION (i), &rect))
		goto fail;
	      WD_IRECTANGLE (wd, &rect);
	      AndRectRegion (WD__CLIP_REGION (i), &rect);
	    }
	}

  tmp = NewRegion ();
  if (!tmp)
    goto fail;

  WD_IRECTANGLE (wd, &rect);
  /* inverse region inside inner window */
  success = (OrRectRegion (tmp, &rect)
	     && XorRegionRegion (tmp, WD__CLIP_REGION (wd)));
  DisposeRegion (tmp);

  /* clip inner window */
  AndRectRegion (WD__CLIP_REGION (wd), &rect);

  --awin_db_in_clip_reg_calc;
  return success;
  /* XXX-bw/02-Nov-97: handle error somehow */
fail:
  Aerr_fail ("%s: clipping failed", __PRETTY_FUNCTION__);
  --awin_db_in_clip_reg_calc;
  return false;			/* NOTREACHED */
}

/* Recompute clipping regions.  Keep installed clipping regions
   installed. */
void
win__note_resize (int wd)
{
  check_precond (WD_VALID_P (wd) && WD_WIN_P (wd));

  if (WD__CLIP_REGION (wd))
    {
      int last_clip_wd, i;

      /* remember WD of last installed clip region */
      if (WD_CLIPPED_P (wd))
	last_clip_wd = wd;
      else
	{
	  last_clip_wd = WD_NIL;
	  WD_CHILD_FOREACH (wd, i)
	    if (WD_CLIPPED_P (i) && WD_MAPPED_P (i))
	      {
		last_clip_wd = i;
		break;
	      }
	}

      /* recalc regions of WD and its childs */
      win___clip_reg_calc (wd);

      /* reinstall clip region */
      if (last_clip_wd != WD_NIL && WD__CLIP_REGION (last_clip_wd))
	{
	  void *reg;
	  reg = InstallClipRegion (WD_LAYER (wd), WD__CLIP_REGION (last_clip_wd));
	  check_postcond (reg == 0);
	  WD_CLIPPED_SET (last_clip_wd,  WD__CLIP_REGION (wd)!=0);
	}
    }
}

bool
win__activate (int wd)
{
  check_precond (WD_VALID_P (wd));

  if (WD_WIN_P (wd))
    {
      ActivateWindow (WD_WIN (wd));
      return true;
    }

  /* FIXME-bw: should we activate parent? */
  return false;
}

static int
win__wd_alloc ()
{
  int i;
  int wd = WD_NIL;		/* RESULT */

  /* note: window descriptor 0 will not used */
  for (i=1; i < WD_MAX; ++i)
    if (!WD_USED_P(i))
      {
	wd = i;
	break;
      }

  if (WD_NIL_P (wd))
    ASYSE_SET_MISC ("alloc wd", "descriptor table full");
  else if (WD_GREATEST < wd) 
    WD_GREATEST_SET (wd);

  return wd;
}


void
win__wd_free (int wd)
{
  local_precond (WD_INFO (wd)->type != WT_FREE);

  STRING_FREE (WD_INFO (wd)->name);
  bzero (&win_info_tbl[wd], sizeof win_info_tbl[wd]);
  WD_SET_FREE (wd);
  if (WD_GREATEST == wd)
    {
      int i;
      for (i = wd; i > WD_NIL; --i)
	if (WD_USED_P (i))
	  break;
      WD_GREATEST_SET (i);
    }
}



/* class win */
static int
win__create (int type)
{
  SIGMASKTYPE sig_mask;
  int wd;

  check_precond (type < WT_end);

  sig_mask = sigblock (sigmask (SIGINT));

  if (!(wd = win__wd_alloc ()))
    ASYSE_SET_MISC ("win__wd_alloc()", "too many windows");
  else {
    WD_INFO (wd)->type = type;
    if (wd_message (wd, create))
      {
	sigsetmask (sig_mask);
	return wd;
      }
    win__wd_free (wd);
  }
  sigsetmask (sig_mask);
  return 0;
}

/* Let WD adopt the CHILD if WD is a valid descriptr, or udno a
   formerly attach othewise */

/* utility: unlink a child from its list */
static void
win___unlink_child (Window child)
{
  int i;
  Window wd;

  wd = WD_PARENT (child);

  if (WD_CHILD (wd) == child)
    WD_CHILD_SET (wd, WD_NEXT (child));
  else
    for (i=WD_CHILD (wd); i != WD_NIL; i=WD_NEXT(i))
      if (WD_NEXT (i) == child)
	{
	  WD_NEXT_SET (i, WD_NEXT (child));
	  break;
	}
}

struct RastPort *
win__prepare_iwin (int wd, GC gc, int mask, int *x, int *y)
{
  local_precond (WD_VALID_P (wd) && WD_WIN_P (wd) 
		 && (GX_VALID_P (gc) || GX_NIL_P (gc)));

  if (!WD_CLIPPED_P (wd))
    {
      int child;
      WD_CHILD_FOREACH (wd, child)
	WD_CLIPPED_SET (child, false);
      InstallClipRegion (WD_LAYER (wd), WD__CLIP_REGION (wd));
      WD_CLIPPED_SET (wd,  WD__CLIP_REGION (wd)!=0);
    }

#ifdef XAPI_GZZ
  if (x) 
    *x += WD_WIN (wd)->BorderLeft;
  if (y) 
    *y += WD_WIN (wd)->BorderTop;
#endif

  if (mask)
    win__prepare_1 (wd, WD_RPORT (wd), gc, mask, WD_COLCON (WD_PARENT (wd)));

  return WD_RPORT (wd);
}

/* Detach clip region of main window and its subwindows This function
   is called to increase Intuition performance. -bw/04-Apr-98

   Note: It's ok */
void
win__detach_clip_iwin (int wd)
{
  int i;

  check_precond (WD_VALID_P (wd) && WD_MAINWIN_P (wd));

  if (!WD_MAPPED_P (wd) && WD_WIN_P (wd)) 
    BLINK;

  if (!WD_MAPPED_P (wd) || !WD_WIN_P (wd))
    return;

  InstallClipRegion (WD_LAYER (wd), 0);

  if (WD_CLIPPED_P (wd))
    WD_CLIPPED_SET (wd,  false);
  else
    WD_CHILD_FOREACH (wd, i)
      WD_CLIPPED_SET (i, false);
}


/* subclass win-iwin */

/* remove and reply all IntuiMessages on a port that
 * have been sent to a particular window
 * (note that we don't rely on the ln_Succ pointer
 *  of a message after we have replied it)
 */
static void
StripIntuiMessages (mp, win)
     struct MsgPort *mp;
     struct Window *win;
{
  struct IntuiMessage *msg;
  struct Node *succ;
     
  msg = (struct IntuiMessage *) mp->mp_MsgList.lh_Head;
  while ((succ =  msg->ExecMessage.mn_Node.ln_Succ)) 
    {
      if(Awin_event_test ((void *)msg))
	{
	  /* do nothing */
	}
      else if (msg->IDCMPWindow == win) 
	{
	  /* Intuition is about to free this message.
	   * Make sure that we have politely sent it back.
	   */
	  Remove ((struct Node *) msg);
	  ReplyMsg ((struct Message *) msg);
	}
      msg = (struct IntuiMessage *) succ;
    }
}

static bool
win__create_iwin (int wd)
{
  return (win__create_nwin (wd)
	  && win__map_nwin (wd));
}

static void
win__destroy_iwin (Window wd)
{
  int i;
  struct Window *win;

  local_precond (WD_WIN_P (wd));

#if 0  /* This object isn't destroyed really, but turned into a NWIN
	  and remains a child of its parent. (for Iconify=>Deiconify)
	  -bw/06-Apr-98*/
  if (WD_PARENT (wd) != WD_NIL)
    win___unlink_child (wd), WD_PARENT_SET (wd, WD_NIL);
#endif

  if (win = WD_WIN (wd))
    {
      dispose_ptr (free, win->UserData);

      if (WIN_COMMON_USER_PORT)
	{
	  Forbid();
	  StripIntuiMessages (win->UserPort, win);
	  win->UserPort = 0;
	  ModifyIDCMP (win, 0);
	  Permit();
	}
      InstallClipRegion (win->RPort->Layer, 0);
      if (WD__CLIP_REGION (wd))
	{
	  DisposeRegion (WD__CLIP_REGION (wd));
	  WD__CLIP_REGION_SET (wd, 0);
	}
      if (WD__WBAPP_WINDOW (wd))
	{
	  RemoveAppWindow (WD__WBAPP_WINDOW (wd));
	  WD__WBAPP_WINDOW_SET(wd, 0);
	}
      CloseWindow (win);
      WD_PTR_SET (wd, 0);
    }

  win_subwin_mp_remove_all (wd);
  check_postcond (!WD_PTR (wd) 
		  && !WD__WBAPP_WINDOW (wd)
		  && !WD__CLIP_REGION (wd));
}

/* Close window only temporarily.  Call win__map() to open a similar window. */
/* This turns a NT_IWIN into NT_NWIN (design error?) -bw/10-Nov-97 */
static bool
win__un_map_iwin (int wd)
{
  struct ExtNewWindow *nw;
  struct Window *win;

  check_precond (WD_VALID_P (wd));
  check_precond (WD_WIN_P (wd));
  check_precond (WD_SCREEN_P (WD_PARENT (wd)));

  if (!OBJ_OBTAIN (nw))
    ASYSE_SET ("ExtNewWindow",  ASYSE_NO_MEMORY);
  else
    {
      int i;
      void *user_data;

      /* detach gadgets on Intuition level XXX-bw/04-Nov-97 */
      WD_CHILD_FOREACH (wd, i)
	if (WD_BOOPSI_P (i))
	  RemoveGadget (WD_WIN (wd), WD_BOOPSI (i));

      win = WD_WIN (wd);

      assert (win && nw);

      nw->IDCMPFlags = win->IDCMPFlags;
      nw->Flags = win->Flags;
      nw->Title = win->Title;
      nw->Screen = win->WScreen;
      nw->MinWidth = win->MinWidth;
      nw->MinHeight = win->MinHeight;
      nw->MaxWidth = win->MaxWidth;
      nw->MaxHeight = win->MaxHeight;
      nw->Type = (win->WScreen->Flags & SCREENTYPE);

      user_data = win->UserData;
      win->UserData = 0;	/* avoid free()ing it */

      WD_MAP_NOTIFY_SET (wd, true);
      WD_MAPPED_SET (wd, false); /* protect against signal handlers */
      win__destroy_iwin (wd);
      WD_SET_NWIN (wd, nw);
      WD__USER_DATA_SET (wd, user_data);  /* (union with iwin data) */

#if 0 /* very slow (uses ReadPixel(gfx)) */
      /* redefer all colors except those used by mapped brothers */
      win_redefer_unused_colors (WD_PARENT (wd));
#else
      /* If we were the last visible window then release ("re-defer")
         the shared pens. */
      {
	bool mapped_brother = false;

	WD_CHILD_FOREACH (WD_PARENT (wd), i)
	  if (WD_MAPPED_P (i))
	    {
	      mapped_brother = true;
	      break;
	    }
	
	if (!mapped_brother)
	  win__color_redefer (WD_COLCON (WD_PARENT (wd)), -1); /* XXX-bw/08-Apr-98 */
      }
#endif
      return true;
    }
  return false;
}

void
win__flush_iwin (int wd)
{
  int change_flags;
  check_precond (WD_WIN_P (wd));

  change_flags = WD_INFO (wd)->change_flags;
  WD_INFO (wd)->change_flags = 0;

  if (change_flags & (WDI_CH_SIZE | WDI_CH_POS))
    ChangeWindowBox (WD_WIN (wd),
		     WD_OUT_LEFT (wd), WD_OUT_TOP (wd),
		     WD_OUT_WIDTH (wd), WD_OUT_HEIGHT (wd));

  /* Must called after changing the real window, because it uses the
     fields in the Intuition window structure!. */
  if (change_flags & WDI_CH_SIZE)
    win__note_resize (wd);

  if (change_flags & WDI_CH_GADGETS)
    {
      /* This does't work. Parts of the old scroller are already
	 overwritten by glyphs. Maybe we could change that. */
#if 0 

      /* clear all subwindows (we assume that only gadgets are subwindows) */
      {
	struct Rectangle rect;
	struct Region *reg;

	WD_IRECTANGLE (wd, &rect);
	reg = InstallClipRegion (WD_LAYER (wd), 0);
	if (!reg)
	  abort ();
	XorRectRegion (reg, &rect);
	InstallClipRegion (WD_LAYER (wd), reg);

	SetAPen (WD_RPORT (wd), WD_BG_PEN (wd));
	SetDrMd (WD_RPORT (wd), JAM1);
	RectFill (WD_RPORT (wd), 0, 0, WD_RIGHT (wd), WD_BOTTOM (wd));
      }
#endif /* 0 */
      win___clip_reg_calc (wd);
#if 0
	RefreshGadgets (WD_WIN (wd)->FirstGadget, WD_WIN (wd), 0);
      /* we could also use the last gadget-child as first gadget */
#else
      {
	int i;
	WD_CHILD_FOREACH (wd, i)
	  if (WD_BOOPSI_P (i) && (WD_INFO (i)->change_flags & WDI_CH_GADGETS))
	    {
	      AddGadget (WD_WIN (wd), WD_BOOPSI (i), ~0); /* ! */
	      RefreshGList (WD_BOOPSI (i), WD_WIN (wd), 0, 1);
	      WD_INFO (i)->change_flags &= ~WDI_CH_GADGETS;
	    }
      }
#endif
    }

  if (change_flags & WDI_CH_FRAME)
    RefreshWindowFrame (WD_WIN (wd));
}


/* subclass win-icon */
static bool
win__create_icon (int wd)
{
  struct ExtNewWindow *nw;
  struct DiskObject *dobj;

  nw = &win_new_win;
  dobj = win__icon_dobj_create ();
  if (!dobj)
    return false;

  WD_ICON_SET (wd, dobj);

  WD_GEOMETRY_SET (wd, nw->LeftEdge, nw->TopEdge,
		   nw->Width,	/* XXX-bw/06-Nov-97 */
		   nw->Height);

  return true;
}

static void
win__destroy_icon (Window wd)
{
  check_precond (WD_VALID_P (wd));

  STRING_FREE (WD_INFO (wd)->name);

  if (WD_PARENT (wd) != WD_NIL)
    win__detach_icon (wd), WD_PARENT_SET (wd, WD_NIL);

  win__icon_dobj_destroy (WD_ICON (wd));
  win___unlink_child (wd);
}  

static bool
win__attach_icon (int child, int parent)
{
  struct AppIcon *icon;
  char *name;
  {
    check_precond (WIN_WB_APP_PORT);
    check_precond (WD_USED_P (child));
    local_precond (WD_ICON_P (child));
    check_precond (WD_INFO (child)->u.icon.app_icon == 0);
  }

  /* XXX-bw/03-Apr-98: This will not work with XSetIconName().  That
     would need an extra icon-namefield in PARENT.  */
  if (WD_NAME (child))
    name = WD_NAME (child);
  else
    {
      name = WD_NAME (parent) ? WD_NAME (parent) : "Emacs";
      name = STRING_CLONE (name);
      WD_SET_NAME (child, name);
    }

#ifndef __GNUC__
  icon = AddAppIconA (child, 0, name, WIN_WB_APP_PORT, 0, WD_ICON (child), 0);
#else
  {
    /* Use stub to avoid spilling registers */
    extern struct AppIcon *AddAppIconA_ (unsigned long id, unsigned long userdata,
					 UBYTE *text, struct MsgPort *msgport,
					 struct FileLock *lock,
					 struct DiskObject *diskobj,
					 struct TagItem *taglist);
    icon = AddAppIconA_ (child, 0, name, WIN_WB_APP_PORT, 0, WD_ICON (child), 0);
  }
#endif
  if (!icon)
    return false;

  WD_PARENT_SET (child, parent);
  WD_NEXT_SET (child, WD_CHILD (parent));
  WD_CHILD_SET (parent, child);

  WD_INFO (child)->u.icon.app_icon = icon;
}


#ifdef __GNUC__
/* Stub */
struct AppIcon *
AddAppIconA_ (unsigned long id, unsigned long userdata,
	      UBYTE *text, struct MsgPort *msgport, struct FileLock *lock,
	      struct DiskObject *diskobj, struct TagItem *taglist)
{
  return  AddAppIconA (id, userdata, text, msgport, lock, diskobj, taglist);
}
#endif

static void
win__detach_icon (int child)
{
  local_precond (WD_VALID_P (child));
  local_precond (WD_ICON_P (child));
  check_precond (WIN_WB_APP_PORT); /* to catch cleanup bugs */
  check_precond (WD_INFO (child)->u.icon.app_icon != 0);

  if (RemoveAppIcon (WD_INFO (child)->u.icon.app_icon))
    WD_INFO (child)->u.icon.app_icon = 0;
}


/* sub-subclass win-boopsi-vscroll ??? */

static struct TagItem win_prop_map[]
= {
    /*    {PGA_Top, ICSPECIAL_CODE}, /* copy value of attribute PGA_Top to im->Code */
  {GA_ID, ICSPECIAL_CODE},   /* ???-bw/31-Oct-97 I assume this is the default? */
  {TAG_DONE,}
  };

static bool
win__create_vscroll (int wd)
{
#ifdef AMIGA_SCROLL_GADGET
  struct Gadget *gad;
  struct ExtNewWindow *nw;
  GC gc;

  nw = &win_new_win;
  gc = GX_NIL;

  if (GX_NIL == (gc = win__gx_alloc ()))
    ASYSE_SET_MISC ("win__gx_alloc()", "cannot alloc gfx context");
  else if (!(gad = (void *) NewObject 
	     (0, "propgclass", 
	      /* useder defined ID number */
	      GA_ID, wd,
	      /* appearance */
	      PGA_Freedom, FREEVERT,
	      PGA_NewLook, TRUE,
	      PGA_Borderless, FALSE,

	      /* state */
	      PGA_Total, nw->MaxHeight, /* buffer height (char rows) */
	      PGA_Visible, nw->MinHeight, /* window height (char rows) */
	      PGA_Top, nw->MinWidth, /* line at top of window */

	      /* placing */
	      GA_Top, nw->TopEdge, /* pixel positions... */
	      GA_Left, nw->LeftEdge,
	      GA_Width, nw->Width,
	      GA_Height, nw->Height,
	      GA_RightBorder, FALSE, /* TRUE, /* place in the window border */
	      /* I/O */
	      ICA_TARGET, ICTARGET_IDCMP,
	      ICA_MAP, (ULONG)win_prop_map, /* PGA_Top => im->Code  */
	      TAG_DONE)))
    ASYSE_SET_MISC ("win__create_vscroll()", "cannot alloc gadget object");
  else
    {
      WD_BOOPSI_SET (wd, gad);

      WD_GEOMETRY_SET (wd, nw->LeftEdge, nw->TopEdge, nw->Width, nw->Height);

      /* Default values for all gc fields */
      GX_COLOR_A_SET(gc, COLOR_BLACK);
      GX_COLOR_B_SET(gc, COLOR_WHITE);
      GX_FID_SET (gc, 1);
      GX_DRAW_MODE_SET(gc, JAM1);
      GX_MASK(gc) = ~0;
      WD_INFO(wd)->gc = gc;

      return true;
    }

  if (gc != GX_NIL)
    win__gx_free (gc);

#endif /* AMIGA_SCROLL_GADGET */
  return false;
}


/* subclass win-boopsi */

static bool
win__create_boopsi (int wd)
{
  switch (win_new_win.Type)
    {
    case BT_VSCROLLBAR:
      return win__create_vscroll (wd);
    default:
      ASYSE_SET_MISC ("win__wd_alloc()", "unknown BOOPSI type");
    }
  return false;
}

static void
win__destroy_boopsi (Window wd)
{
  /* if attached then detach it first */
  if (!WD_NIL_P (WD_PARENT (wd)))
    win__detach (wd);
  DisposeObject (WD_BOOPSI (wd));
}

bool
win__attach_boopsi (int child, int parent)
{
  local_precond (WD_WIN_P (parent));
  local_precond (WD_BOOPSI_P (child));

  WD_PARENT_SET (child, parent);
  WD_NEXT_SET (child, WD_CHILD (parent));
  WD_CHILD_SET (parent, child);

  if (WD_WIN_P (parent))
    win__map_boopsi (child);	/* XXX-bw/10-Nov-97: obsolete */

  return true;			/* (cannot fail) */
}

static void
win__detach_boopsi (int child)
{
  local_precond (WD_VALID_P (child));
  check_precond (WD_NWIN_P (WD_PARENT (child)) || WD_WIN_P (WD_PARENT (child)));

  win___unlink_child (child);

  if (WD_WIN_P (WD_PARENT (child)))
    win__un_map_boopsi (child);

  WD_PARENT_SET (child, WD_NIL);
}

static bool
win__map_boopsi (int wd)
{
  int parent;

  check_precond (WD_VALID_P (wd) && WD_BOOPSI_P (wd));
  check_precond (WD_WIN_P (WD_PARENT (wd))); /* TODO-bw/10-Nov-97 */

  parent = WD_PARENT (wd);

  AddGadget (WD_WIN (parent), WD_BOOPSI (wd), ~0);
  RefreshGList (WD_BOOPSI (wd), WD_WIN (parent), 0, 1);
  win___clip_reg_calc (parent);

  WD_MAPPED_SET (wd, true);

  return true;
}

static bool
win__un_map_boopsi (int wd)
{
  int parent;

  check_precond (WD_VALID_P (wd) && WD_BOOPSI_P (wd));
  check_precond (WD_WIN_P (WD_PARENT (wd))); /* TODO-bw/10-Nov-97 */

  parent = WD_PARENT (wd);

  RemoveGadget (WD_WIN (parent), WD_BOOPSI (wd));
  WD_MAPPED_SET (wd, false);
  win___clip_reg_calc (parent);
}

void
win__flush_boopsi (int wd)
{
  int parent;

  check_precond (WD_BOOPSI_P (wd));

  /* We update the complete IBox rather than only changed values */
  if (WD_INFO (wd)->change_flags & (WDI_CH_SIZE | WDI_CH_POS))
    {
      struct TagItem tags[]
	= {
	    GA_Top, WD_TOP (wd),
	    GA_Left, WD_LEFT (wd),
	    GA_Width, WD_WIDTH (wd),
	    GA_Height, WD_HEIGHT (wd),
	    TAG_DONE
	  };

      parent = WD_PARENT (wd);
      if (parent != WD_NIL && WD_WIN_P (parent))
	{
//	  SetGadgetAttrsA (WD_BOOPSI (wd), WD_WIN (parent), 0, tags);
	  /* Request RefreshGadgets from parent window at _end_ of win_flush */
          WDI_CH_ADD (parent, WDI_CH_GADGETS);
	  /* Mark this gadget for individual refresh */
	  WD_INFO (wd)->change_flags |= WDI_CH_GADGETS;
	}
//      else
	SetAttrsA (WD_BOOPSI (wd), tags);
    }
  WD_INFO (wd)->change_flags &= WDI_CH_GADGETS;
}


/* subclass win-subwin */

static bool
win__create_subwin (int wd)
{
  GC gc;

  if (GX_NIL == (gc = win__gx_alloc ()))
    ASYSE_SET_MISC ("win__gx_alloc()", "cannot alloc gfx context");
  else
    {
      /* Default values for all gc fields */
      GX_COLOR_A_SET(gc, COLOR_BLACK);
      GX_COLOR_B_SET(gc, COLOR_WHITE);
      GX_FID_SET (gc, 1);
      GX_DRAW_MODE_SET (gc, JAM1);
      GX_MASK(gc) = ~0;
      WD_INFO(wd)->gc = gc;

      WD_SUBWIN_SET (wd, (void*)~0);
      return true;
    }
  return false;
}

static void
win__destroy_subwin (Window wd)
{
  /* if attached then detach it first */
  if (!WD_NIL_P (WD_PARENT (wd)))
    win__detach (wd);
  win_subwin_mp_remove_all (wd);
}

bool
win__attach_subwin (int child, int parent)
{
  local_precond (WD_WIN_P (parent));
  local_precond (WD_SUBWIN_P (child));

  WD_PARENT_SET (child, parent);
  WD_NEXT_SET (child, WD_CHILD (parent));
  WD_CHILD_SET (parent, child);

  return true;			/* (cannot fail) */
}

static void
win__detach_subwin (int child)
{
  local_precond (WD_VALID_P (child));
  check_precond (WD_MAINWIN_P (WD_PARENT (child)));

  win___unlink_child (child);

  if (WD_MAPPED_P (child))
    win__un_map_subwin (child);

  WD_PARENT_SET (child, WD_NIL);
}

static bool
win__map_subwin (int wd)
{
  int parent;

  check_precond (WD_VALID_P (wd) && WD_SUBWIN_P (wd));
  check_precond (WD_WIN_P (WD_PARENT (wd))); /* TODO-bw/10-Nov-97 */

  if (WD_MAPPED_P (wd))
    return false;

  parent = WD_PARENT (wd);
  WD_MAPPED_SET (wd, true);
  if (!WD__CLIP_REGION (wd))
    WD__CLIP_REGION_SET (wd, NewRegion ());
  win___clip_reg_calc (parent);

  win_subwin_mp_push (wd, (WD_SUBWIN_EVENT_EXPOSE | WD_SUBWIN_EVENT_MAP), 0);

  return true;
}

static bool
win__un_map_subwin (int wd)
{
  int parent;

  check_precond (WD_VALID_P (wd) && WD_SUBWIN_P (wd));
  check_precond (WD_MAINWIN_P (WD_PARENT (wd)));

  if (! WD_MAPPED_P (wd))
    return false;

  parent = WD_PARENT (wd);
  WD_MAPPED_SET (wd, false);
  
  if (WD_CLIPPED_P (wd))
    {
      if (WD_WIN_P (parent) && WD_MAPPED_P (parent))
	InstallClipRegion (WD_WIN (parent)->WLayer, 0);
      WD_CLIPPED_SET (wd, false);
    }

  if (WD__CLIP_REGION (wd))
    {
      DisposeRegion (WD__CLIP_REGION (wd));
      WD__CLIP_REGION_SET (wd, 0);
    }

  if (WD_MAPPED_P (parent))
    {
      struct IBox box = { WD_LEFT (wd), WD_TOP (wd), WD_WIDTH (wd), WD_HEIGHT (wd) };
      win___clip_reg_calc (parent);
      win_subwin_mp_push (parent, WD_WIN_EVENT_EXPOSE, &box);
    }
}

void
win__flush_subwin (int wd)
{
  int parent;

  check_precond (WD_SUBWIN_P (wd));

  /* We update the complete IBox rather than only changed values */
  if (WD_INFO (wd)->change_flags & (WDI_CH_SIZE | WDI_CH_POS))
    {
    }
}

struct RastPort *
win__prepare_subwin (int wd, GC gc, int mask, int *x, int *y)
{
  int parent;

  local_precond (WD_VALID_P (wd) && WD_SUBWIN_P (wd) 
		 && (GX_VALID_P (gc) || GX_NIL_P (gc)));

  parent = WD_PARENT (wd);
  if (!WD_WIN_P (parent))
    return 0;

  if (!WD_CLIPPED_P (wd))
    {
      int child;
      WD_CHILD_FOREACH (parent, child)
	WD_CLIPPED_SET (child, false);

      InstallClipRegion (WD_WIN(parent)->WLayer, WD__CLIP_REGION (wd));
      WD_CLIPPED_SET (wd, WD__CLIP_REGION (wd)!=0);
      WD_CLIPPED_SET (parent, false);
    }

  if (mask)
    win__prepare_1 (wd, WD_RPORT (WD_PARENT (wd)), gc, mask,
		    WD_COLCON (WD_PARENT (WD_PARENT (wd))));

  x && (*x += WD_LEFT (wd));
  y && (*y += WD_TOP (wd));

  return WD_RPORT (WD_PARENT (wd));
}

/* subclass win-nwin (obsolet?) */

static bool
win__create_nwin (int wd)
{
  struct ExtNewWindow *nw;
  GC gc;

  if (!OBJ_CLONE (nw, &win_new_win))
    ASYSE_SET ("window", ASYSE_NO_MEMORY);
  else if (GX_NIL == (gc = win__gx_alloc ()))
    ASYSE_SET_MISC ("win__gx_alloc()", "cannot alloc gfx context");
  {

    if (nw->Extension && !(nw->Extension = CloneTagItems (nw->Extension)))
      {
	ASYSE_SET ("CloneTagItems(utility)", ASYSE_NO_MEMORY);
	win__gx_free (gc);
	return false;
      }

    if (nw->IDCMPFlags & IDCMP_MOUSEMOVE)
      nw->Flags |= WFLG_REPORTMOUSE;
    if (!(nw->Flags & WFLG_NOCAREREFRESH))
      nw->IDCMPFlags |= IDCMP_REFRESHWINDOW;

    WD_SET_NWIN (wd, nw);
    WD_GEOMETRY_SET (wd, nw->LeftEdge, nw->TopEdge, nw->Width, nw->Height);

    /* Default values for all gc fields */
    GX_COLOR_A_SET(gc, COLOR_BLACK);
    GX_COLOR_B_SET(gc, COLOR_WHITE);
    GX_FID_SET (gc, 1);
    GX_DRAW_MODE_SET (gc, JAM1);
    GX_MASK(gc) = ~0;
    WD_INFO(wd)->gc = gc;

    return true;
  }

  return false;
}

static void
win__destroy_nwin (Window wd)
{
  if (WD_PARENT (wd) != WD_NIL)
    win__detach_nwin (wd), WD_PARENT_SET (wd, WD_NIL);
  
  if (WD__USER_DATA (wd))
    {
      free (WD__USER_DATA (wd));
      WD__USER_DATA_SET (wd, 0);
    }

  dispose_field (FreeTagItems, WD_NWIN(wd), Extension);
  OBJ_DISPOSE (WD_NWIN (wd));
  win___unlink_child (wd);
}  

static bool
win__attach_nwin (int child, int parent)
{
  local_precond (WD_SCREEN_P (parent));
  local_precond (WD_NWIN_P (child));

  WD_PARENT_SET (child, parent);
  WD_NEXT_SET (child, WD_CHILD (parent));
  WD_CHILD_SET (parent, child);
  return true;
}

static void
win__detach_nwin (int child)
{
  local_precond (WD_VALID_P (child));
  check_precond (WD_SCREEN_P (WD_PARENT (child)));

  win___unlink_child (child);
  WD_PARENT_SET (child, WD_NIL);
}

/* This turns a WT_NWIN to WT_IWIN (design error?!) */
static bool
win__map_nwin (int wd)
{
  ULONG old_IDCMPFlags;
  struct ExtNewWindow *nw;
  int i;
  struct Window *win;
  struct Region *reg;

  check_precond (WD_VALID_P (wd) && WD_NWIN_P (wd));
  check_precond (WD_WIDTH (wd) > 0 && WD_HEIGHT (wd) > 0);
  check_precond (WD_SCREEN_P (WD_PARENT (wd)));
  /* ???-bw/03-Nov-97: Or shall I use the parent sizes */
  
  nw = WD_NWIN (wd);

  nw->Screen = WD_SCREEN (WD_PARENT (wd));
  if (WD_INFO(WD_PARENT (wd))->wi_flags = WF_LOCKED_PUB_SCREEN)
    nw->Type = PUBLICSCREEN;
  else
    nw->Type = CUSTOMSCREEN;

  /* XXX-bw/26-Oct-97: we use this function also for first opening
     from a WD_INFO(wd). Maybe using the mask field to distinguish
     from a really reopen.  */
  nw->DetailPen = 1; /* XXX-bw/06-Apr-98 */
  nw->BlockPen = 0;

  old_IDCMPFlags = 0;
  swap (old_IDCMPFlags, nw->IDCMPFlags);

  nw->LeftEdge = WD_OUT_LEFT (wd);
  nw->TopEdge = WD_OUT_TOP (wd);
  nw->Width =  WD_OUT_WIDTH (wd);
  nw->Height = WD_OUT_HEIGHT (wd);

  if (!(reg = NewRegion ()))
    ASYSE_SET (__PRETTY_FUNCTION__, ASYSE_NO_MEMORY);
  if (!(win = OpenWindowTagList ((struct NewWindow *)nw, nw->Extension)))
    ASYSE_SET (__PRETTY_FUNCTION__, ASYSE_NO_WINDOW);
  else
    {
      win->UserData = WD__USER_DATA (wd);  /* (union with iwin data) */
      WD__USER_DATA_SET (wd, 0);

      if (WIN_COMMON_USER_PORT)
	win->UserPort = WIN_COMMON_USER_PORT;

      if (old_IDCMPFlags)
	ModifyIDCMP (win, old_IDCMPFlags | WD_IDCMP_MAND (wd));

      WD_SET_WIN (wd, win);
      WD__CLIP_REGION_SET (wd, reg);
      WD__WBAPP_WINDOW_SET (wd, AddAppWindowA (wd, 0, win, WIN_WB_APP_PORT, 0));

      /* set the actual positions if we had requested WA_AutoAdjust */
      WD_GEOMETRY_SET (wd, win->LeftEdge, win->TopEdge, win->Width, win->Height);

      if (nw)
	{
	  if (nw->Extension)
	    FreeTagItems (nw->Extension);
	  OBJ_DISPOSE (nw);
	}

      /* attach gadgets on Intuition level XXX-bw/04-Nov-97 */
      WD_CHILD__FOREACH (i, wd)
	if (WD_BOOPSI_P (i))
	  AddGadget (WD_WIN (wd), WD_BOOPSI (i), ~0);
      /* redraw gadgets */
      WD_CHILD__FOREACH (i, wd)
	if (WD_BOOPSI_P (i))
	  {
	    RefreshGadgets (WD_BOOPSI (i), WD_WIN (wd), 0);
	    break;
	  }

      WD_MAP_NOTIFY_SET (wd, true);
      WD_MAPPED_SET (wd, true);
      win___clip_reg_calc (wd);
      return true;
    }
  dispose_ptr (DisposeRegion, reg);
  return false;
}

/* subclass win-iscreen */

/* Open screen */
/* TODO-bw/31-Oct-97: currently it "opens" only the default public
   screen */
static bool
win__create_iscreen (int wd)
{
  struct Screen *screen;
  GC gc;

  if (GX_NIL == (gc = win__gx_alloc ()))
    ASYSE_SET_MISC ("win__gx_alloc()", "cannot alloc gfx context");
  else if (!(screen = LockPubScreen (win_new_win.Title)))
    {
      win__gx_free (gc);
      ASYSE_SET_MISC ("LockPubScreen(intuition)",  "cannot lock public screen");
    }
  else if (!(WD_INFO(wd)->u.screen.colcon = win_color_context_create (screen)))
    {
      UnlockPubScreen (0, screen);
      win__gx_free (gc);
      ASYSE_SET_MISC (win_color_context_create, "no memory or OS < V39");
    }
  else
    {
      WD_SET_SCREEN (wd, screen);
      WD_INFO(wd)->wi_flags = WF_LOCKED_PUB_SCREEN;

      WD_GEOMETRY_SET (wd, screen->LeftEdge, screen->TopEdge,
		       screen->Width, screen->Height);

      /* Default values for all gc fields */
      GX_COLOR_A_SET(gc, COLOR_BLACK);
      GX_COLOR_B_SET(gc, COLOR_WHITE);
      GX_FID_SET (gc, 1);
      GX_DRAW_MODE_SET(gc, JAM1);
      GX_MASK(gc) = ~0;
      WD_INFO(wd)->gc = gc;

      return true;
    }

  return false;
}

static void
win__destroy_iscreen (Window wd)
{
  int flags = WD_INFO(wd)->wi_flags;

  if (WD_INFO(wd)->u.screen.colcon)
    win_color_context_destroy (WD_INFO(wd)->u.screen.colcon);

  if (flags & WF_LOCKED_PUB_SCREEN)
    {
      UnlockPubScreen (0, WD_SCREEN (wd));
    }
  else
    assert (!"not implemented");
}  

static void
win__destroy (int wd)
{
  SIGMASKTYPE sig_mask;
  int i, tmp;
  bool mapped;


  check_precond (WD_VALID_P (wd));
  check_precond (WD_INFO(wd)->type < WT_end);

  sig_mask = sigblock (sigmask (SIGINT));

  /* find and detach any childs first */
  WD_CHILD_FOREACH_RM (wd, i, tmp)
    {
      assert (WD_PARENT (i) == wd);
      win__destroy (i);
    }

  /* detach ourself from parent - (but keep parent field!) */
  if (WD_PARENT (wd) != WD_NIL)
    win___unlink_child (wd);

  wd_message (wd, destroy);

  if (!GX_NIL_P (WD_GX (wd)))
    win__gx_free (WD_GX (wd));

  win__wd_free (wd);

  sigsetmask (sig_mask);
}

/* Fill in an array of pens used in RP inside the rectangle
   LEFT,TOP,WIDTH,HEIGHT.  Return the array in (*STORE_PIXELS) and the
   number of valid pens in that array as return value.  If an error
   occur then -1 will returned and (*STORE_PIXELS) remains untouched.
   If succesful the caller is responsible to free (*STORE_PIXEL) using
   free(2).  */
static int
rp_find_pixels (struct RastPort *rp,
	     unsigned left, unsigned top, unsigned width, unsigned height,
	     long **store_pixels)
{
  unsigned long *results, *p;
  long x, y;
  int i, n, size;

  n = size = 0;
  results = 0;

  for (x = left; x < width; ++x)
    for (y = top; y < height; ++y)
      {
	ULONG pixel;

	pixel = (ULONG)ReadPixel (rp, x, y);
	if (pixel == -1UL)
	  continue;
	for (i = 0; i < n; ++i)
	  if (pixel == results [i])
	    goto next_y;
	
	if (n >= size)
	  p = realloc (results, size += sizeof *results * 20);
	if (!p)
	  {
	    free (results);
	    return -1;
	  }
	results = p;

	results [n++] = pixel;

      next_y: continue;
      }

  *store_pixels = results;
  return n;
}

/* like rp_find_pixels, but used on entire WIN except its borders. */
static int
win_find_pixels (struct Window *win, long **store_pixels)
{
  int result;
  long *tmp;

  result = rp_find_pixels (win->RPort,
			   win->BorderLeft,
			   win->BorderTop,
			   win->Width - win->BorderLeft - win->BorderTop,
			   win->Height - win->BorderTop - win->BorderBottom,
			   &tmp);

  if (result > 0)
    *store_pixels = tmp;

  return result;
}

/* Redefer unused colors for the screen WD */
int
win_redefer_unused_colors (int wd)
{
  int nmb_win, i, n;
  struct { int size; long *pens; } *arrays;

  check_precond (WD_VALID_P (wd) && WD_SCREEN_P (wd));

  /* alloc array for results */
  nmb_win = 0;
  WD_CHILD_FOREACH (wd, i)
    if (WD_WIN_P (i) && WD_MAPPED_P (i))
      ++nmb_win;

    /* when no mapped childs simply redefer all colors */
  if (!nmb_win)
    {
      win__color_redefer (WD_COLCON (wd), -1);
      return 0;
    }

  arrays = alloca (nmb_win * sizeof *arrays);

  /* find used pens for all mapped childs of type IWIN */
  n = 0;
  WD_CHILD_FOREACH (wd, i)
    if (WD_WIN_P (i) && WD_MAPPED_P (i))
      if ((arrays[n].size = win_find_pixels (WD_WIN (i), &arrays[n].pens)) < 0)
	goto fail;
      else
	++n;

  {
    int i, k, j, ip_size;
    long *ign_pens;

    ip_size = 0;
    for (i = 0; i < nmb_win; ++i)
      ip_size += arrays[i].size;
    ign_pens = alloca (ip_size * sizeof *ign_pens);

    for (i = 0, j = 0; i < nmb_win; ++i)
      for (k = 0; k < arrays[i].size; ++k, ++j)
	ign_pens[j] = arrays[i].pens[k];

    while (--n >= 0)
      free (arrays[n].pens);

    win__color_redefer2 (WD_COLCON (wd), ign_pens, ip_size);
  }

  return 0;
 fail:
  while (--n >= 0)
    free (arrays[n].pens);
  return -1;
}

/* end of iscreen */


USHORT win_fill_style_opaque_stipple[] 
= {0x5555, 0xaaaa};
USHORT win_fill_style_stipple[]
= {0x5555, 0xaaa};

struct  {
  int size;
  USHORT *patt;
} win_fill_styles[]
= {
  {0, 0}, /* solid */
  {1, win_fill_style_stipple},
  {1, win_fill_style_opaque_stipple},  /* ???-bw/14-Nov-97: JAM2 ? */
};
    

/* if gc equal 0 (GX_NIL) we use WD_GX(wd) */
void
win__prepare_1 (int wd, struct RastPort *rp, GC gc, int mask, void *colcon)
{
  int draw_mode = 0;
  int pen_a = -1, pen_b = -1;

  check_precond (WD_VALID_P (wd));
  check_precond (GX_NIL_P (gc) || (GX_VALID_P (gc) && GX_USED_P (gc)));
  check_precond (rp && (WD_WIN_P (wd) || WD_SUBWIN_P (wd)));

  if (GX_NIL_P (gc))
    gc = WD_GX (wd);

  /* FIXME-bw: rethink this code */

  draw_mode |= ((mask & GCBackground) && (mask & GCForeground)) ? JAM2 : JAM1;

  check_precond (colcon);
  if (mask & GCForeground)
    pen_a = win_color_pen (colcon, WD_GX_COLOR_A (wd, gc));
  if (mask & GCBackground)
    pen_b = win_color_pen (colcon, WD_GX_COLOR_B (wd, gc));

  if (pen_b < 0)
    pen_b = 0;

  if (pen_a < 0)
    pen_a = pen_b;

  if (mask & GCFunction)
    draw_mode |= WD_GX_DRAW_MODE (wd, gc);
  if (mask & GCInverse)
    draw_mode ^= INVERSVID;

  SetABPenDrMd (rp, pen_a, pen_b, draw_mode);

  if (mask & GCFont)
    {
      fid_type fid = WD_GX_FID (wd, gc);

      SetFont (rp, FID_FONT (fid));
      /* Set and clear previous set softstyles */
      SetSoftStyle (rp, FID_STYLE (fid), ~0);
    }

  if (mask & GCStipple)
    {
      
      if (!GX_MASK_STIPPLE (gc))
	{
	  rp->AreaPtSz = 0;
	  rp->AreaPtrn = 0;
	}
      else
	{
#if 0
	  rp->AreaPtSz = PIXMAP_SIZE_LOG2 (GX_STIPPLE (gc));
#else
	  rp->AreaPtSz = PIXMAP_WIDTH (GX_STIPPLE (gc)) / 16;
#endif
	  rp->AreaPtrn = PIXMAP_DATA (GX_STIPPLE (gc));
	}
    }

  /* The X techique is much more complicated here: The Pixmap contains
     only zeros (cursor_bits) and seems only required to allow
     stipple. It seems we have to merge the Pixmap with a stipple
     pattern.  */

  if (mask & GCFillStyle)
    switch (WD_GX_FILL_STYLE (wd, gc))
      {
      case FillSolid:
	rp->AreaPtSz = 0;
	rp->AreaPtrn = 0;
	SetDrMd (rp, (draw_mode & ~JAM2) | JAM1);
	break;
      case FillStippled:
	SetDrMd (rp, (draw_mode & ~JAM2) | JAM1);
	break;
      case FillOpaqueStippled:
	SetDrMd (rp, (draw_mode & ~JAM1) | JAM2);
	break;
      default:
	abort ();
      }
}


#ifdef WIN_BACKFILL
struct fill
{
  struct Layer *layer;
  struct Rectangle bounds;
  WORD offsetx, offsety;
};

static ULONG __saveds __interrupt
fill_background (struct RastPort *rp __asm("a2"),
		 struct fill *msg __asm("a1"))
{
  struct Layer *layer = rp->Layer;
#if 1
  /* We need a copy of RP with cleared Layer field if we do layered
     rendering */
  struct RastPort rp_cpy = *rp;
  rp = &rp_cpy;
#endif
  rp->Layer = 0;

  SetDrMd (rp, JAM1 | INVERSVID);
  SetWriteMask (rp, 0xff);	/* (V39) */
  RectFill (rp, msg->bounds.MinX, msg->bounds.MinY,
	    msg->bounds.MaxX, msg->bounds.MaxY);

  rp->Layer = layer;		/* restore layer if rp was not copied */
  return 0;
}
#endif


/* Icon creation */

/* convert X windows pixel data from byte aligned string to word
   aligned string. Do reverse bit order. */
/* SRC ist the start of the byte string.  DST is the start of a word
   string.  The size of DST must equal or greater than the size of
   SRC.  SIZE ist the size of SCR in counted in bytes (not in pixel!).
   */
void
win___pixrow_x_to_intui (const void *src, void *dst, unsigned size)
{
  int i;
  const char *s=src;
  short *d = dst;

  for (i=0; i < size; ++i, ++d, ++s)
    {
      *d = 0;

      (*s & 0x01) && (*d |= 0x8000);
      (*s & 0x02) && (*d |= 0x4000);
      (*s & 0x04) && (*d |= 0x2000);
      (*s & 0x08) && (*d |= 0x1000);
      (*s & 0x10) && (*d |= 0x0800);
      (*s & 0x20) && (*d |= 0x0400);
      (*s & 0x40) && (*d |= 0x0200);
      (*s & 0x80) && (*d |= 0x0100);

      if (++i >= size)
	break;

      ++s;
      (*s & 0x01) && (*d |= 0x0080);
      (*s & 0x02) && (*d |= 0x0040);
      (*s & 0x04) && (*d |= 0x0020);
      (*s & 0x08) && (*d |= 0x0010);
      (*s & 0x10) && (*d |= 0x0008);
      (*s & 0x20) && (*d |= 0x0004);
      (*s & 0x40) && (*d |= 0x0002);
      (*s & 0x80) && (*d |= 0x0001);
    }
}

/* Make Amiga bitplane data from X bitplane array */
/* BITS must point to X bit data array.  WIDTH is the size of valid
   pixel in each row (But each row starts byte aligned).  HEIGHT is
   the number of rows. 
   (sizeof BITS == (HEIGHT * ((WIDTH / 16) + ((WIDTH % 16) !=0)))) */
unsigned short *
win___bitplane_x_to_intui (const unsigned char *bits, int width, int height)
{
  unsigned short *dst, *dst_line;
  const unsigned char *src_line;
  int i;
  int width_word = (width / 16) + ((width % 16) != 0);
  int width_byte = (width / 8) + ((width % 8) != 0);

  dst = AllocVec (width_word * 2 * height, MEMF_CHIP | MEMF_CLEAR);

  if (!dst)
    return 0;

  src_line = bits;
  dst_line = dst;
  
  for (i=0; i < height; ++i)
    {	
      win___pixrow_x_to_intui (src_line, dst_line, width_byte);
      src_line += width_byte;
      dst_line += width_word;
      check_precond (dst_line <= (dst + (width_word * height))); /* XXX */
    }

  return dst;
}

/* Create icon (struct DiskObject) from X bitmap data for use with
   AddAppIconA(workbench) */
struct DiskObject *
win__icon_dobj_make_from_bits (unsigned char *raw_bits, int pix_per_row, int nmb_rows)
{
  struct { struct DiskObject dobj; struct Image img; } *ptr; /* RESULT */
  void *img_data;

  ptr = calloc (1, sizeof *ptr);
  if (!ptr)
    return 0;

  img_data = win___bitplane_x_to_intui (raw_bits, pix_per_row, nmb_rows);

  if (!img_data)
    {
      free (ptr);
      return 0;
    }

  ptr->dobj.do_Gadget.Width = pix_per_row;
  ptr->dobj.do_Gadget.Height = nmb_rows;

  ptr->dobj.do_Gadget.GadgetRender = &ptr->img;
  ptr->img.Width = pix_per_row;
  ptr->img.Height = nmb_rows;
  ptr->img.Depth = 1;
  ptr->img.ImageData = img_data;
  ptr->img.PlanePick = (1<<ptr->img.Depth) - 1;	/* ???-bw */

  ptr->dobj.do_CurrentX = NO_ICON_POSITION;
  ptr->dobj.do_CurrentY = NO_ICON_POSITION;

  return (struct DiskObject *) ptr;
}

/* Destroy icon (struct Diskobject) created by
   win__icon_dobj_make_from_bits() */
void
win__icon_dobj_destroy (struct DiskObject *dobj)
{
  check_precond (dobj);

  FreeVec (((struct Image*)dobj->do_Gadget.GadgetRender)->ImageData);
  free (dobj);
}


/* A replacement for Intuition::DisplayAlert() which flashes the
   Window border (needed for windows with white backgrounds). */
void
win__flash (Window wd)
{
  struct Region *old_reg;
  struct Window *win;
  
  check_precond (WD_MAINWIN_P (wd) && WD_SCREEN_P (WD_PARENT (wd)));

  /* if not mapped flash display */
  if (!WD_WIN_P (wd))
    {
      DisplayBeep (WD_SCREEN (WD_PARENT (wd)));
      return;
    }

  /* flash border synchronous */
  win = WD_WIN (wd);

  old_reg = InstallClipRegion (win->RPort->Layer, 0);
  SetDrMd (win->RPort, COMPLEMENT);
  RectFill (win->RPort, 0, 0, win->Width - 1, win->BorderTop - 1);
  RectFill (win->RPort, 0, win->Height - win->BorderBottom,
	    win->Width - 1, win->Height - 1);
  Delay (5);
  RectFill (win->RPort, 0, 0, win->Width - 1, win->BorderTop - 1);
  RectFill (win->RPort, 0, win->Height - win->BorderBottom,
	    win->Width - 1, win->Height - 1);
  InstallClipRegion (win->RPort->Layer, old_reg);
}
