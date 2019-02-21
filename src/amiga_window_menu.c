/* amiga_window_menu.c - universal pop-up menus (written for GNU Emacs)

   Features: vertical scrolling; sub menus;  keyboard navigation;
   Misfeatures: implemented by Intuition windows (go inactive, if you click outside)
   Author: Bert Winkelmann <bertw@in-brb.de>
   Date: 1997, 1998
  */

/* SD enables simple refresh Window */
#define SD

/* XXX-bw/10-May-98: */
#if 0 //ndef AMIGA_MENU_2D
#define AMIGA_MENU_3D
#endif
/* use separate column for shortcuts. print all columns left justified
   expcept submenu arrows. */
#define AMIGA_MENU_COLUMNS

  /* -bw/16-May-98:##exp## try to speed up menus by using super bitmap
     windows */
  /* #define AMIGA_MENU_SBM */

static struct
{
  long normal_font_style;
  short submenu_opener; /* always (0), mouse_click (1) */
  
} cfg; 

/* have to click (or release button) to pop up an submenu */
#define MENU_CLICK_SUBMENU_P(OBJ) (cfg.submenu_opener == 1)

/* MS-Windows like click menu (no mousemoves).  That will complicate
   scrolling --bw/02-Aug-98 */
#if 0 //def TEST
#define MENU_CLICK_MENU_P(OBJ) (1)
#else
#define MENU_CLICK_MENU_P(OBJ) (0)
#endif

/* Problems:

When AMIGA_MENU_INACTIVE is defined, we would lose if a Sun-mouse
commodity is installed. */
#define AMIGA_MENU_INACTIVE

struct DrawInfo *draw_info;
#define PEN(n_) (draw_info->dri_Pens[(n_)] + 0)

#define RK_HELP (0x5f)
#define RK_BACKSPACE (0x41)
#define RK_DEL (0x46)
#define RK_ESC (0x45)
#define RK_TAB (0x42)
#define RK_RETURN (0x44)
#define NKP_1 (0x1d)
#define NKP_2 (0x1e)
#define NKP_3 (0x1f)
#define NKP_4 (0x2d)
#define NKP_5 (0x2e)
#define NKP_6 (0x2f)
#define NKP_7 (0x3d)
#define NKP_8 (0x3e)
#define NKP_9 (0x3f)

//#define MAX_ASSIGN(a,b) ((void)(((a) < (b)) && ((a) = (b))))
#define MAX_ASSIGN(a,b)				\
({ typeof (b) b_ = (b);				\
 ((void)(((a) < (b_)) && ((a) = (b_))));	\
})
#define MIN_ASSIGN(a,b)				\
({ typeof (b) b_ = (b);				\
 ((void)(((a) < (b_)) || ((a) = (b_))));	\
})
#ifndef emacs

#include <proto/intuition.h>
struct IntuitionBase *IntuitionBase;
#include <proto/graphics.h>
struct GfxBase *GfxBase;
#include <proto/layers.h>
struct Library *LayersBase;
#include <proto/exec.h>
#include <proto/dos.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#define check_precond assert
#define local_precond assert
#define xclose(close, ptr) ({ if (ptr) { close ((void*) ptr); ptr=0; } })
#define xclose2(close, ptr, field) \
({ if ((ptr) && ((ptr)->field)) { close ((void*) ((ptr)->field)); ((ptr)->field)=0; } })
#define dispose_ptr xclose
#define dispose_field xclose2

#ifndef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif
#define swap(a,b) ({ typeof (a) tmp=a; a=b; b=tmp; }) 

typedef enum { false, true } bool;

#define DB_TRACE (fprintf(stderr, "%s:%d: trace %s\n", \
			  __FILE__, __LINE__,  __PRETTY_FUNCTION__))

#ifdef TEST
#define malloc(SIZE) ((++db_nmb_allocs),(malloc)(SIZE))
#define calloc(NMB,SIZE) ((++db_nmb_allocs),(calloc)(NMB,SIZE))
#define realloc(M,SIZE) ((++db_nmb_allocs),(db_nmb_frees+=(M!=0)), (realloc)(M, SIZE))
#define free(M) ((db_nmb_frees+=(M!=0)), (free)(M))
int db_nmb_allocs, db_nmb_frees;
#endif

#else /* emacs */

#include "amiga_window_.h"
#include "amiga_window_defs.h"
#include "xterm.h"
#include "syssignal.h"

#endif /* emacs */

#ifdef AMIGA_MENU_3D
#define MENU_STRIP_FG_PEN(obj) (1)
#define MENU_TITLE_PEN(obj) (1)
#define MENU_TITLE_FS(obj) (MENU_ITEM_FS (obj))
#define MENU_ITEM_SEL_BG_PEN(obj) (PEN (FILLPEN))
#define MENU_ITEM_SEL_FG_PEN(obj) (1)
#define MENU_ITEM_SEL_NW_PEN(obj) (PEN (SHINEPEN))
#define MENU_ITEM_SEL_SE_PEN(obj) (PEN (SHADOWPEN))
     /* move text from normal position (for 3D effect) */
#define MENU_ITEM_SEL_3D_XOFFS(obj) (0)
#define MENU_ITEM_SEL_3D_YOFFS(obj) (0)

#define MENU_ITEM_FG_PEN(obj) (1)
#define MENU_ITEM_BG_PEN(obj) (0)
#define MENU_ITEM_FS(obj) (cfg.normal_font_style + 0)
#define MENU_ITEM_3D_XOFFS(obj) (-1)
#define MENU_ITEM_3D_YOFFS(obj) (-1)

#define MENU_ITEM_GHOST_FG_PEN(obj) (obj->ghost_pen)
#define MENU_ITEM_GHOST_BG_PEN(obj) (0)

#define MENU_FRAME_WIDTH (6)
#define MENU_FRAME_HEIGHT (6)
#define MENU_ITEM_XOFFS (3)
#define MENU_ITEM_YOFFS (1)
#define MENU_ITEM_HEIGHT(RP) ((RP)->TxHeight + 2 * MENU_ITEM_YOFFS)
#define MENU_ITEM_BASE(RP) ((RP)->TxBaseline /* + MENU_ITEM_YOFFS */)
#define MENU_ITEM_WIDTH(rp_, txt_)							\
     ({ const char *s_ = txt_;								\
     (((s_) ? TextLength (rp_, (STRPTR)s_, strlen (s_)) : 0) + 2 * MENU_ITEM_XOFFS);	\
       })

#define MENU_STRIP_NW_PEN(obj) (PEN (SHINEPEN))
#define MENU_STRIP_SE_PEN(obj) (PEN (SHADOWPEN))
#define MENU_STRIP_BG_PEN(obj) (0)
/* pixels child overlaps its parent to show the hierarchy of sub menus */
#define MENU_STRIP_OVERLAP_3D(obj) (4)
#define MENU_STRIP_BORDER_WIDTH(obj) (1)
#define MENU_STRIP_BORDER_HEIGHT(obj) (1)

#else
     /* non 3D menu (>=V39!) */
#define MENU_TITLE_PEN(obj) (MENU_STRIP_FG_PEN (obj))
#define MENU_TITLE_FS(obj) (MENU_ITEM_FS (obj))

#define MENU_ITEM_SEL_BG_PEN(obj) (MENU_STRIP_FG_PEN (obj))
#define MENU_ITEM_SEL_FG_PEN(obj) (MENU_STRIP_BG_PEN (obj))
#define MENU_ITEM_SEL_NW_PEN(obj) (MENU_ITEM_SEL_BG_PEN (obj))
#define MENU_ITEM_SEL_SE_PEN(obj) (MENU_ITEM_SEL_BG_PEN (obj))
     /* move text from normal position (for 3D effect) */
#define MENU_ITEM_SEL_3D_XOFFS(obj) (0)
#define MENU_ITEM_SEL_3D_YOFFS(obj) (0)

#define MENU_ITEM_FG_PEN(obj) (MENU_STRIP_FG_PEN (obj))
#define MENU_ITEM_BG_PEN(obj) (MENU_STRIP_BG_PEN (obj))
#define MENU_ITEM_FS(obj) (cfg.normal_font_style + 0)
#define MENU_ITEM_3D_XOFFS(obj) (0)
#define MENU_ITEM_3D_YOFFS(obj) (0)

#define MENU_ITEM_GHOST_FG_PEN(obj) (obj->ghost_pen)
#define MENU_ITEM_GHOST_BG_PEN(obj) (MENU_ITEM_BG_PEN (obj))

#define MENU_FRAME_WIDTH (6)
#define MENU_FRAME_HEIGHT (6)
#define MENU_ITEM_XOFFS (2)
#define MENU_ITEM_YOFFS (0)
#define MENU_ITEM_HEIGHT(RP) ((RP)->TxHeight + 2 * MENU_ITEM_YOFFS)
#define MENU_ITEM_BASE(RP) ((RP)->TxBaseline /* + MENU_ITEM_YOFFS */)
#define MENU_ITEM_WIDTH(rp_, txt_)							\
     ({ const char *s_ = txt_;								\
     (((s_) ? TextLength (rp_, (STRPTR)s_, strlen (s_)) : 0) + 2 * MENU_ITEM_XOFFS);	\
       })
#define MENU_STRIP_NW_PEN(obj) (MENU_STRIP_FG_PEN (obj))
#define MENU_STRIP_SE_PEN(obj) (MENU_STRIP_FG_PEN (obj))
#define MENU_STRIP_FG_PEN(obj) (PEN (BARDETAILPEN))
#define MENU_STRIP_BG_PEN(obj) (PEN (BARBLOCKPEN))
/* pixels child overlaps its parent to show the hierarchy of sub menus */
#define MENU_STRIP_OVERLAP_3D(obj) (4)
#define MENU_STRIP_BORDER_WIDTH(obj) (2)
#define MENU_STRIP_BORDER_HEIGHT(obj) (2)
#endif

#define MENU_ITEM(OBJ, N) (&(OBJ)->data[(N)] + 0)
#define MENU_RP_BUTTON_WIDTH(RP) ((RP)->TxWidth * 2)
#define MENU_STRIP_BUTTON_WIDTH(OBJ) (MENU_RP_BUTTON_WIDTH (STRIP__RPORT(OBJ)))

#define MENU_ITEM_SEP_P(obj)							\
(!SMN_GHOSTED_P (&(obj)->data[item])						\
 SMN_DISABLED_P (&(obj)->data[item])						\
 && *(obj)->data[item].mn_label && !strcspn ((obj)->data[item].mn_label, "-"))

typedef struct menu_node smn;

/* abstract menu strip builder */
/* You have to provide at least a create function.  Normally this
   struct will embedded at start of a more specific struct containing
   data used by the methods. */
struct menu_create_hook
{
  void (*create) ();
  void (*destroy) ();
  void (*select) ();		/* callback useful for multiselect menus */
  /* The res_... fields must filled by the create method.  If create
     fails, then res_strip will contain NULL */
  smn *res_strip;
  int res_nrows;
};

struct menu_node
{
  char *mn_label, *mn_kbequiv;
  short mn_id;			/* this value will returned to the caller */
  short mn_flags;
  struct menu_create_hook *mn_sub; /* Submenue builder or NULL */
#define SMN_GHOSTED (01)
#define SMN_DISABLED (02)
#define SMN_SEP (04)
#define SMN_BUTTON (010)	/* check box */
#define SMN_STATE (020)		/* indicate the state of buttons, ... */
#define SMN_RADIO (040)	/* check box */
#define SMN_GHOSTED_P(smn_) ((smn_)->mn_flags & SMN_GHOSTED)
#define SMN_DISABLED_P(smn_) ((smn_)->mn_flags & SMN_DISABLED)
#define SMN_SEP_P(smn_) ((smn_)->mn_flags & SMN_SEP)
#define SMN_BUTTON_P(SMN) ((SMN)->mn_flags & SMN_BUTTON)
#define SMN_BUTTON_ACTIVE_P(SMN) \
(((SMN)->mn_flags & SMN_STATE) && SMN_BUTTON_P (SMN)) 
};


#define IBOX__RIGHT(box_) ((box_)->Left + (box_)->Width - 1)  
#define IBOX__BOTTOM(box_) ((box_)->Top + (box_)->Height - 1)  


#define XREPLY_MSG(msg_) \
((void)(msg_ && (ReplyMsg ((struct Message*)msg_),1) && (msg_=0)))

struct strip
{
  /* model */
  const char *title; /* currently not needed */
  smn *data;
  struct strip *parent;
  int nmb_rows;
  int selected;
  struct menu_create_hook *creator;
  /* view/control */
  int horiz;			/* actual a menu bar */
  int highlighted;		/* mirror of model.selected */
  int vc_nrows;			/* if window is too small we scroll */
  int vc_first;
  struct Window *win;
#ifdef AMIGA_MENU_SBM
  struct BitMap *bm;		/* super bitmap */
#endif
  struct IBox box;
  struct TextFont *font;	/* own font or font of parent for submenus */
  struct IBox item_box;		/* private */
  long ghost_pen;		/* used by ghosted items */
#ifdef AMIGA_MENU_COLUMNS
  int col2_start;		/* pos of shortcut relative to item box left edge */
  int col3_start;		/* pos of submenu arrow relative to item box left edge */
#endif
#define STRIP__RPORT(o_) ((o_)->win->RPort + 0)
#define STRIP__LABEL(o_) ((o_)->data[(o_)->selected].mn_label + 0)
#define STRIP__NITEMS(o_) ((o_)->nmb_rows + 0)
  /* useless */
#define STRIP__DATA_FOREACH(obj_, i) \
 for (i=&(obj_)->data; i < &(obj_)->data + STRIP__NITEMS (obj_); ++i)

  /* indicate whether ITEM is currently showed by the view */
#define STRIP_V_ITEM_VISIBLE_P(obj_, item_) \
 ((obj_)->vc_first <= (item_) && (item_) < ((obj_)->vc_first + (obj_)->vc_nrows))
  /* indicate whether the view has invisble items */
#define STRIP_V_ITEM_INVISIBLE_ANY_P(obj_) ((obj_)->nmb_rows > (obj_)->vc_nrows)
  /* indicate whether the view has invisble items on the top */
#define STRIP_V_ITEM_INVISIBLE_TOP_P(obj_) ((obj_)->vc_first != 0)
  /* indicate whether the view has invisble items on the bottom  */
#define STRIP_V_ITEM_INVISIBLE_BOTTOM_P(obj) \
  (((obj)->vc_first + (obj)->vc_nrows) < (obj)->nmb_rows)
  /* needed by auto scrolling */
#define STRIP_V_TICKS_DISABLE(obj)						\
  (void)((obj->win->IDCMPFlags & IDCMP_INTUITICKS) 				\
	 && ModifyIDCMP (obj->win, (obj->win->IDCMPFlags & ~IDCMP_INTUITICKS)))
#define STRIP_V_TICKS_ENABLE(obj)						\
  (void)((obj->win->IDCMPFlags & IDCMP_INTUITICKS) 				\
	 || ModifyIDCMP (obj->win, (obj->win->IDCMPFlags | IDCMP_INTUITICKS)))

#define STRIP_SUBMENU(obj_, item_)				\
(local_precond ((item_) >= 0 && (item_) <= (obj_)->nmb_rows),	\
 ((obj_)->data[(item_)].mn_sub) != 0)
#define STRIP_SUBMENU_LEFT_POS(obj, item) ((obj)->win->LeftEdge + (obj)->win->Width)
       /* if there is not enough room, use an alternative position */

#define STRIP_SUBMENU_TOP_POS(obj, item) \
       ((obj)->win->TopEdge + (obj)->item_box.Top)
       /* XXX-bw/08-May-98: item_box holds the position of current selected item */
};

short stipple_pattern[] = {0x5555, 0xaaaa};
#define RPORT_STIPPLE_SET(rp_) \
((void)(((rp_)->AreaPtrn = stipple_pattern), ((rp_)->AreaPtSz = 1)))
#define RPORT_STIPPLE_CLEAR(rp_) \
((void)(((rp_)->AreaPtrn = 0), ((rp_)->AreaPtSz = 0)))


static void
item__draw_submenu_arrow (struct RastPort *rp)
{
  int h2 = rp->TxBaseline / 2;
  int w = rp->TxWidth;
  
  /* draw triangle */
  while (1)
    {
      Draw (rp, rp->cp_x + w, rp->cp_y - h2);
      Draw (rp, rp->cp_x - w, rp->cp_y - h2);
      Draw (rp, rp->cp_x, rp->cp_y + 2 * h2);

      if ((--h2) < 0 || (w -= 2) < 0)
	break;
      Move (rp, rp->cp_x + 1, rp->cp_y - 1);
    }
}


void
item_separator (struct strip *obj, int item)
{
  struct RastPort *rp;
  int y;

  rp = STRIP__RPORT (obj);
  obj->item_box.Top = obj->box.Top + obj->item_box.Height * (item - obj->vc_first);

  SetABPenDrMd (rp, MENU_ITEM_BG_PEN (obj), 1, JAM1);
  RectFill (rp, obj->item_box.Left, obj->item_box.Top,
	    IBOX__RIGHT (&obj->item_box), IBOX__BOTTOM (&obj->item_box));

  RPORT_STIPPLE_SET (rp);
  SetAPen (rp, MENU_STRIP_FG_PEN (obj));

  y = obj->box.Top + obj->item_box.Height * (item - obj->vc_first)
    + obj->item_box.Height / 2;
  RectFill (rp, obj->item_box.Left, y,
	    obj->item_box.Left + obj->item_box.Width - 1, y + 1);
  RPORT_STIPPLE_CLEAR (rp);
}

void
item_text (struct strip *obj, int item, int pen, int xoffs, int yoffs)
{
  struct RastPort *rp;
  const char *label, *shortcut;
  int xstart, ybase;

  rp = STRIP__RPORT (obj);
  label = MENU_ITEM (obj, item)->mn_label;
  shortcut = MENU_ITEM (obj, item)->mn_kbequiv;
  xstart = obj->item_box.Left + xoffs + MENU_ITEM_XOFFS;
  ybase = obj->item_box.Top + MENU_ITEM_BASE (rp) + yoffs + MENU_ITEM_YOFFS;
  if (SMN_BUTTON_P (MENU_ITEM (obj, item)))
    {
      int left = xstart, top = ybase - rp->TxBaseline + 1,
	right = left + rp->TxWidth - 1, bottom = ybase;

      SetAPen (rp, pen);
      Move (rp, left, top);
      Draw (rp, right, top);
      Draw (rp, right, bottom);
      Draw (rp, left, bottom);
      Draw (rp, left, top);

      if (SMN_BUTTON_ACTIVE_P (MENU_ITEM (obj, item)))
	{
	  Draw (rp, right, bottom);
	  Move (rp, right, top);
	  Draw (rp, left, bottom);
	}
      xstart += MENU_STRIP_BUTTON_WIDTH (obj);
    }

  SetAPen (rp, pen);
  Move (rp, xstart, ybase);
  Text (rp, (STRPTR) label, strlen (label));

  if (shortcut)
    {
#ifndef AMIGA_MENU_COLUMNS
      int txt_len = TextLength (rp, (STRPTR)shortcut, strlen (shortcut));
      Move (rp, IBOX__RIGHT (&obj->item_box) - txt_len + xoffs - MENU_ITEM_XOFFS,
	    obj->item_box.Top + MENU_ITEM_BASE (rp) + yoffs + MENU_ITEM_YOFFS);
#else
      Move (rp, obj->item_box.Left + obj->col2_start + xoffs - MENU_ITEM_XOFFS,
	    obj->item_box.Top + MENU_ITEM_BASE (rp) + yoffs + MENU_ITEM_YOFFS);
#endif
      Text (rp, (STRPTR) shortcut, strlen (shortcut));

    }
  else if (STRIP_SUBMENU (obj, item))
    {
#ifndef AMIGA_MENU_COLUMNS
      Move (rp, IBOX__RIGHT (&obj->item_box) + xoffs - MENU_ITEM_XOFFS - rp->TxWidth,
	    obj->item_box.Top + MENU_ITEM_BASE (rp) + yoffs + MENU_ITEM_YOFFS);
#else
      Move (rp, obj->item_box.Left + obj->col3_start + xoffs - MENU_ITEM_XOFFS,
	    obj->item_box.Top + MENU_ITEM_BASE (rp) + yoffs + MENU_ITEM_YOFFS);
#endif
      item__draw_submenu_arrow (rp);
    }
}

void
item_select  (struct strip *obj)
{
  struct RastPort *rp;
  struct IBox *box;
  int item;

  rp = STRIP__RPORT (obj);
  box = &obj->item_box;
  item = obj->highlighted;

  obj->item_box.Top = obj->box.Top + obj->item_box.Height * (item - obj->vc_first);

  Move (rp, box->Left, box->Top + MENU_ITEM_BASE (rp));

  if (rp->BitMap->Depth == 1)
    {
      /* Monochrome screen */
      SetABPenDrMd (rp, MENU_STRIP_FG_PEN (obj), 0, JAM1);
      RectFill (rp, box->Left, box->Top, IBOX__RIGHT (box), IBOX__BOTTOM (box));
      item_text (obj, item, MENU_STRIP_BG_PEN (obj), 0, 0);
    }
  else
    {
      /* Color screen */
      SetABPenDrMd (rp, MENU_ITEM_SEL_BG_PEN (obj), 0, JAM1);
      RectFill (rp, box->Left, box->Top, IBOX__RIGHT (box), IBOX__BOTTOM (box));
      item_text (obj, item, MENU_ITEM_SEL_FG_PEN (obj),
		 MENU_ITEM_SEL_3D_XOFFS (obj), MENU_ITEM_SEL_3D_YOFFS (obj));

      SetAPen (rp, MENU_ITEM_SEL_NW_PEN (obj));
      Move (rp, IBOX__RIGHT (box), box->Top);
      Draw (rp, IBOX__RIGHT (box), IBOX__BOTTOM (box));
      Draw (rp, box->Left, IBOX__BOTTOM (box));

      SetAPen (rp, MENU_ITEM_SEL_SE_PEN (obj));
      Draw (rp, box->Left, box->Top);
      Draw (rp, IBOX__RIGHT (box), box->Top);
    }
}

void
item_unselect (struct strip *obj, int item)
{
  struct RastPort *rp;

  rp = STRIP__RPORT (obj);
  obj->item_box.Top = obj->box.Top + obj->item_box.Height * (item - obj->vc_first);

  SetABPenDrMd (rp, MENU_ITEM_BG_PEN (obj), 1, JAM1);
  RectFill (rp, obj->item_box.Left, obj->item_box.Top,
	    IBOX__RIGHT (&obj->item_box), IBOX__BOTTOM (&obj->item_box));

  if (rp->BitMap->Depth == 1)
    item_text (obj, item, MENU_ITEM_FG_PEN (obj), 0, 0);
  else
    item_text (obj, item, MENU_ITEM_FG_PEN (obj),
	       MENU_ITEM_3D_XOFFS (obj), MENU_ITEM_3D_YOFFS (obj));
}

void
item_ghosted (struct strip *obj, int item)
{
  struct RastPort *rp;
  struct IBox *box;
  short pattern[] = {0x5555, 0xaaaa};

  rp = STRIP__RPORT (obj);
  box = &obj->item_box;

  obj->item_box.Top = obj->box.Top + obj->item_box.Height * (item - obj->vc_first);

  if (rp->BitMap->Depth == 1 || rp->TxHeight >= 11)
    SetSoftStyle (rp, MENU_ITEM_FS (obj) | FSF_BOLD, ~0);

  if (rp->BitMap->Depth == 1)
    {
      SetABPenDrMd (rp, MENU_ITEM_BG_PEN (obj), 1, JAM1);
      RectFill (rp, box->Left, box->Top, IBOX__RIGHT (box), IBOX__BOTTOM (box));

      item_text (obj, item, MENU_ITEM_FG_PEN (obj), 0, 0);
    }
  else
    {
      SetABPenDrMd (rp, MENU_ITEM_GHOST_BG_PEN(obj), 1, JAM1);
      RectFill (rp, box->Left, box->Top, IBOX__RIGHT (box), IBOX__BOTTOM (box));
      item_text (obj, item, MENU_ITEM_GHOST_FG_PEN (obj), 0, 0);
    }

  if (rp->BitMap->Depth == 1 || rp->TxHeight >= 11)
    {
      rp->AreaPtrn = pattern;
      rp->AreaPtSz = 1;		/* 2**1 */
      SetAPen (rp, MENU_ITEM_BG_PEN (obj));
      RectFill (rp, box->Left, box->Top, IBOX__RIGHT (box), IBOX__BOTTOM (box));
      rp->AreaPtrn = 0;
      rp->AreaPtSz = 0;
    }
  SetSoftStyle (rp, MENU_ITEM_FS (obj), ~0);
}

void
item_draw (struct strip *obj, int item)
{
  local_precond (STRIP_V_ITEM_VISIBLE_P(obj, item));

  if (item == obj->selected)
    item_select (obj);
  else
    {
      int xxx_old_ibtop = obj->item_box.Top;
      if (SMN_SEP_P (&obj->data[item]))
	item_separator (obj, item);
      else if (SMN_GHOSTED_P (&obj->data[item]))
	item_ghosted (obj, item);
      else
	item_unselect (obj, item);
      obj->item_box.Top = xxx_old_ibtop;
    }
}

void
item_make_visible (struct strip *obj, int item)
{
  int scroll_nmb, i;

  scroll_nmb = ((item < obj->vc_first) ? obj->vc_first - item
		: ((obj)->vc_nrows + (obj)->vc_first) - 1 - item);
  if (abs (scroll_nmb) < obj->vc_nrows)
    {
      SetBPen (STRIP__RPORT (obj), MENU_STRIP_BG_PEN (obj));
      ScrollRaster (STRIP__RPORT (obj), 0, -scroll_nmb * obj->item_box.Height,
		    obj->box.Left,
		    obj->box.Top,
		    IBOX__RIGHT (&obj->box),
		    IBOX__BOTTOM (&obj->box));
    }
  obj->vc_first -= scroll_nmb;
  if (scroll_nmb > 0)
    for (i=0; i < scroll_nmb && i < obj->vc_nrows; ++i)
      item_draw (obj, i + obj->vc_first);
  else
    for (i=0; i < -scroll_nmb && i < obj->vc_nrows; ++i)
      item_draw (obj, obj->vc_first + obj->vc_nrows - 1 - i);
}

/* like CloseWindow but for windows used a foreign UserPort 

   If your windows share the UserPort of your main window then don't
   call this for the main window unless you create/destroy the message
   port yourself -bw/03-Aug-98 */
void
CloseWindowSafely (struct Window *win)
{
  struct IntuiMessage *im, *next;
  struct MsgPort *mp = win->UserPort;

  Forbid();

  /* Remove and reply all messages belonging to WIN */
  for (im = (struct IntuiMessage *) mp->mp_MsgList.lh_Head;
       (next =  (struct IntuiMessage *) im->ExecMessage.mn_Node.ln_Succ);
       im = next)
    if (im->IDCMPWindow == win) 
      Remove ((struct Node *) im), ReplyMsg ((struct Message *) im);

  win->UserPort = 0;
  ModifyIDCMP (win, 0);
  Permit();

  CloseWindow (win);
}

void
strip_destroy (struct strip *obj)
{
  check_precond (obj);

  if (obj->win && obj->ghost_pen != PEN (SHINEPEN)) // XXX-bw/27-Apr-98
    ReleasePen (obj->win->WScreen->ViewPort.ColorMap, obj->ghost_pen);

  if (obj->parent)
    dispose_ptr (CloseWindowSafely, obj->win);
  else
    dispose_ptr (CloseWindow, obj->win);

#ifdef AMIGA_MENU_SBM
  dispose_ptr (FreeBitMap, obj->bm);
#endif
  if (!obj->parent)
    dispose_ptr (CloseFont, obj->font);
  else
    obj->font = 0;

  if (obj->creator)
    (*obj->creator->destroy) (obj->creator);
}

/* draw an entire menu box. called by strip_create and for refresh
   events */
static void
strip_draw (struct strip *obj)
{
  int i;

  local_precond (obj);

  DB_TRACE;

  /* draw frame */
  SetFont (STRIP__RPORT (obj), obj->font);
  SetSoftStyle (STRIP__RPORT (obj), MENU_ITEM_FS (obj), ~0);

  if (MENU_STRIP_BG_PEN (obj) != 0)
    {
      SetABPenDrMd (STRIP__RPORT (obj), MENU_STRIP_BG_PEN (obj), 0, JAM1);
      RectFill (STRIP__RPORT (obj), 0, 0, obj->win->Width - 1, obj->win->Height - 1);
    }

  /* draw title */
  if (obj->title)
    {
      int txt_len, y;
      struct RastPort *rp = STRIP__RPORT (obj);

      /* print title text centered */
      txt_len = MENU_ITEM_WIDTH (rp, obj->title);
      SetABPenDrMd (rp, MENU_TITLE_PEN (obj), 0, JAM1);
      SetSoftStyle (rp, MENU_TITLE_FS (obj), ~0);
      Move (rp, (obj->win->Width - txt_len) / 2,
	    MENU_FRAME_HEIGHT + rp->TxBaseline);
      Text (rp, (STRPTR)obj->title, strlen (obj->title));
      SetSoftStyle (rp, MENU_ITEM_FS (obj), ~0);

      /* draw wide horizontal rule */
      y = obj->box.Top - MENU_ITEM_HEIGHT (rp) / 2;

      RPORT_STIPPLE_SET (rp);
      SetAPen (rp, MENU_STRIP_FG_PEN (obj));
      RectFill (rp, MENU_STRIP_BORDER_WIDTH (obj), y,
		obj->win->Width - 1 - MENU_STRIP_BORDER_WIDTH (obj),
		y + 1);
      RPORT_STIPPLE_CLEAR (rp);
    }

  SetDrMd (STRIP__RPORT (obj), JAM1);

  /* draw borders */
  if (MENU_STRIP_BORDER_WIDTH (obj) > 0 && MENU_STRIP_BORDER_HEIGHT (obj) > 0)
    {
      if (MENU_STRIP_BORDER_WIDTH (obj) > 1 && MENU_STRIP_BORDER_HEIGHT (obj) > 1)
	RPORT_STIPPLE_SET (STRIP__RPORT (obj));

      SetAPen (STRIP__RPORT (obj), MENU_STRIP_SE_PEN (obj));
      /* bottom border */
      RectFill (STRIP__RPORT (obj),
		MENU_STRIP_BORDER_WIDTH (obj) - 1,
		obj->win->Height - MENU_STRIP_BORDER_HEIGHT (obj),
		obj->win->Width - 1,
		obj->win->Height - 1);
      /* right border */
      RectFill (STRIP__RPORT (obj),
		obj->win->Width - MENU_STRIP_BORDER_WIDTH (obj),
		MENU_STRIP_BORDER_HEIGHT (obj) - 1,
		obj->win->Width - 1,
		obj->win->Height - 1);
      SetAPen (STRIP__RPORT (obj), MENU_STRIP_NW_PEN (obj));

      /* left border */
      RectFill (STRIP__RPORT (obj),
		0,
		0,
		MENU_STRIP_BORDER_WIDTH (obj) - 1,
		obj->win->Height - 1);
      /* top border */
      RectFill (STRIP__RPORT (obj),
		0,
		0,
		obj->win->Width - 1,
		MENU_STRIP_BORDER_HEIGHT (obj) - 1);

      if (MENU_STRIP_BORDER_WIDTH (obj) > 1 && MENU_STRIP_BORDER_HEIGHT (obj) > 1)
	RPORT_STIPPLE_CLEAR (STRIP__RPORT (obj));
    }

  /* draw items */
  {
    for (i=0; i < obj->vc_nrows; ++i)
      item_draw (obj, i);
  }
}

int
strip_create (struct strip *obj, struct strip *parent,
	      const char *title,
	      smn *data, int nmb_rows,
	      /* gfx */
	      struct Screen *screen,
	      int x, int y)
{
  int i;
  int win_height, win_width, title_height;
  struct RastPort rport, *rp;		/* for calc text sizes */

  check_precond (obj && data && screen);

  bzero (obj, sizeof (*obj));
  obj->highlighted = obj->selected = -1;
  obj->title = title;
  obj->data = data;
  obj->parent = parent;
  obj->nmb_rows = nmb_rows;
  obj->ghost_pen = -1;
  obj->font = 0;

  if (parent)
    obj->font = parent->font;
#ifdef emacs
  else
    {
      Lisp_Object key, val, font, font_name, font_size;
      if (Fboundp (key = intern ("amiga-menu-font"))
	  && !NILP (val = Feval (key))
	  && CONSP (val)
	  && STRINGP (font_name = XCONS (val)->car)
	  && INTEGERP (font_size = XCONS (val)->cdr))
	{
	  struct TextAttr ta;

	  ta.ta_YSize = XINT (font_size);
	  ta.ta_Style = FS_NORMAL; /* XXX-bw/27-May-98 */
	  ta.ta_Flags = 0;

	  ta.ta_Name = alloca (XSTRING (font_name)->size + sizeof ".font");
	  strcpy (ta.ta_Name, XSTRING (font_name)->data);
	  strcat (ta.ta_Name, ".font");
	  obj->font = OpenDiskFont (&ta);
	}
    }
#endif /* emacs */

  if (!obj->font && !(obj->font = OpenFont (screen->Font)))
    goto fail;

  /* setup rastport needed to calc font dependent sizes */
  InitRastPort (&rport);
  SetFont (&rport, obj->font);
  rp = &rport;

  win_width = MENU_ITEM_WIDTH (rp, title);

  /* find out maximal item width in two different ways. */
  {
    int max_width = 0;
    int max_width_1 = 0, max_width_2 = 0;
    int max_width_3 = 0;

    for (i=0; i < STRIP__NITEMS (obj); ++i)
      {
	int w1, w2;
      
	/* pixel width of item title */
	w1 = MENU_ITEM_WIDTH (rp, data[i].mn_label);
	if (SMN_BUTTON_P (&data[i]))
	  w1 += MENU_RP_BUTTON_WIDTH (rp);

	/* pixel width of keyboard shortcut or submenu arrow */
	w2 = ((data[i].mn_kbequiv)
	      ? TextLength (rp, data[i].mn_kbequiv,
			    strlen (data[i].mn_kbequiv)) : 0);
#define COL_SEP_SPACE (rp->TxWidth * 2)
#define COL2_SEP_SPACE (2)
	MAX_ASSIGN (max_width_1, w1);
	MAX_ASSIGN (max_width_2, w2);
	MAX_ASSIGN (max_width_3, (data[i].mn_sub != 0) * rp->TxWidth * 1);

#ifdef AMIGA_MENU_COLUMNS
	if (!w2 && data[i].mn_sub != 0)
	  w2 = rp->TxWidth * 2;
#endif
	MAX_ASSIGN (max_width, w1 + w2 + (w2 != 0) * COL_SEP_SPACE);
      }
#ifdef AMIGA_MENU_COLUMNS
    obj->col2_start = max_width_1 + (max_width_2 != 0) * COL_SEP_SPACE;
    obj->col3_start = (obj->col2_start + max_width_2 
		       + (max_width_3 != 0) * COL2_SEP_SPACE);

    MAX_ASSIGN (win_width, (max_width_1 
			    + (max_width_2 != 0) * COL_SEP_SPACE + max_width_2
			    + (max_width_3 != 0) * COL2_SEP_SPACE + max_width_3));
#else
    MAX_ASSIGN (win_width, max_width);
#endif
  }

  /* right+left border */
  win_width += 2 * MENU_FRAME_WIDTH;

  MIN_ASSIGN (win_width, screen->Width);

  title_height = 2 * MENU_ITEM_HEIGHT (rp) * (title != 0);

  /* compute desired window height */
  win_height = (title_height
		/* space for items */
		+ ((obj->nmb_rows + 1) * MENU_ITEM_HEIGHT (rp) - rp->TxBaseline)
		/* top+bottom border */
		+ (2 * MENU_FRAME_HEIGHT));

  /* shrink window to fit on screen */
  obj->vc_nrows = obj->nmb_rows;
  while (win_height > screen->Height)
    {
       --obj->vc_nrows;
       win_height -= MENU_ITEM_HEIGHT (rp);
    }

  /* calc box containing visible items */
  obj->box.Left = MENU_FRAME_WIDTH;
  obj->box.Top = MENU_FRAME_HEIGHT + (obj->title ? 2 * MENU_ITEM_HEIGHT (rp) : 0);
  obj->box.Width = win_width - 2 * MENU_FRAME_WIDTH;
  /* calc item_box, which describes a single item by Left, Width,
     Height.  ("Top" holds the top of the currently selected item) */
  obj->item_box = obj->box;
  obj->item_box.Height = MENU_ITEM_HEIGHT (rp);
  obj->box.Height = obj->vc_nrows * obj->item_box.Height;

  DB_TRACE;

  /* move window if necessary */
  if (parent)
  /* If we are a submenu and there is not enough room for use between
     parent menu and right screen border, then try the left side. If
     there isn't enought room either, then try other postions let
     the parent menu uncovered -bw/08-May-98 */
    {
      /* constants for spaces between parent window borders and screen
         borders */
      int gap_west, gap_east, gap_north, gap_south;

      gap_north = parent->win->TopEdge;
      gap_south = (parent->win->WScreen->Height 
		   - parent->win->TopEdge - parent->win->Height);
      gap_west = parent->win->LeftEdge;
      gap_east = (parent->win->WScreen->Width
		  - parent->win->LeftEdge - parent->win->Width);

#ifdef TEST
      fprintf (stderr, "gn: %d ge: %d gs %d gw %d\n",
	       gap_north, gap_east, gap_south, gap_west);
#endif

      /* Find out bestmost postion ... */
      if (gap_east >= win_width)
	{
	  x -= MENU_STRIP_OVERLAP_3D (obj);
	}
      else if (gap_west >= win_width)
	{
	  x = parent->win->LeftEdge - win_width;
	  x += MENU_STRIP_OVERLAP_3D (obj);
	}
      else if (gap_south >= win_height)
	{
	  y = parent->win->TopEdge + parent->win->Height;
	  y -= MENU_STRIP_OVERLAP_3D (obj);
	}
      else if (gap_north >= win_height)
	{
	  y = parent->win->TopEdge - win_height;
	  y += MENU_STRIP_OVERLAP_3D (obj);
	}
      else
	{
	  if (gap_west > gap_east)
	    x = 0;
	  if (gap_north > gap_south)
	    y = 0;
	}
    }
  else				/* no sub menu */
    {
      /* treat X as center (or even right edge?) of main menu for two
	 reasons:

	 1. we need place for submenu of one side, but not of both
	 sides.

	 2. our mouse pointer is nearer to the side where submenus
	 usually pop ups.

	 -bw/09-Jun-98 */
      x -= (win_width * 2) / 3;
      /* dont' cover title by mouse pointer XXX-bw/10-Jun-98 */
      y -= title_height;
    }

  MAX_ASSIGN (x, 0);
  MAX_ASSIGN (y, 0);

#ifndef AMIGA_MENU_INACTIVE
#define AMIGA_MENU_IDCMP (IDCMP_ACTIVEWINDOW | IDCMP_MOUSEBUTTONS \
| IDCMP_MOUSEMOVE | IDCMP_RAWKEY | IDCMP_REFRESHWINDOW)
#else
#define AMIGA_MENU_IDCMP \
(IDCMP_ACTIVEWINDOW | IDCMP_MOUSEBUTTONS | IDCMP_MOUSEMOVE \
| IDCMP_RAWKEY | IDCMP_INACTIVEWINDOW | IDCMP_REFRESHWINDOW)
#endif

#ifdef AMIGA_MENU_SBM
  obj->bm = AllocBitMap(win_width, win_height, screen->BitMap.Depth, BMF_CLEAR, 0);
#define  TAG_PAIR_SUPER_BITMAP \
((obj->bm) ? WA_SuperBitMap : TAG_IGNORE), (ULONG)(obj->bm)
#else
#define  TAG_PAIR_SUPER_BITMAP TAG_IGNORE, 0
#endif

#ifdef SD
#define REFRESH_WFLG WFLG_SIMPLE_REFRESH
#else
#define REFRESH_WFLG WFLG_NOCAREREFRESH
#endif

  obj->win = OpenWindowTags 
    (0,
     WA_IDCMP, (parent ? 0 : AMIGA_MENU_IDCMP),
     WA_Flags, (REFRESH_WFLG
		| (MENU_CLICK_MENU_P (obj) ? 0 : WFLG_REPORTMOUSE)
		| WFLG_RMBTRAP),
     WA_PubScreen, (ULONG)screen, /* XXX */
     WA_Borderless, TRUE,
     TAG_PAIR_SUPER_BITMAP,
     WA_Activate, TRUE,
     WA_AutoAdjust, TRUE,
     WA_Left, x,
     WA_Top, y,
     WA_Width, win_width,
     WA_Height, win_height,
     TAG_DONE);

  DB_TRACE;

  if (!obj->win)
    goto fail;

  if (parent)
    {
      obj->win->UserPort = parent->win->UserPort;
      ModifyIDCMP (obj->win, AMIGA_MENU_IDCMP);
    }

  obj->win->UserData = (void *)obj;

  /* calculate a color for ghosted items with brightness between BG
     and FG.  XXX-bw/30-May-98 */
  if (STRIP__RPORT (obj)->BitMap->Depth > 1)
    {
      ULONG rgb[3];
      int i;

      if (!obj->win->WScreen->ViewPort.ColorMap)
	abort ();

      GetRGB32 (obj->win->WScreen->ViewPort.ColorMap, 0, 1, rgb);

      for (i=0; i < 3; ++i)
	rgb[i] /= 2;

      obj->ghost_pen = ObtainBestPen (obj->win->WScreen->ViewPort.ColorMap,
				      rgb[0], rgb[1], rgb[2],
				      TAG_DONE);
      if (obj->ghost_pen == -1)
	obj->ghost_pen = PEN (SHINEPEN);
    }

  strip_draw (obj);

  return 1;
 fail:
  strip_destroy (obj);
  return 0;
}

int
strip_create_2 (struct strip *obj, struct strip *parent,
		const char *title,
		struct menu_create_hook *creator,      
		struct Screen *screen,
		int x, int y)
{
  int result;

  (*creator->create) (creator);
  if (!creator->res_strip)
    return 0;
  result = strip_create (obj, parent, title,
			 creator->res_strip, creator->res_nrows,
			 screen, x, y);
  /* We used to do this before strip_create() but it was
     overwritten. -bw/14-Jun-98 */
  obj->creator = creator;
  return result;
}

int
strip_item_find (struct strip *obj,  int x, int y)
{
  STRIP_V_TICKS_DISABLE (obj);
  if (obj->box.Left <= x && x <= IBOX__RIGHT (&obj->box))
    if (obj->box.Top <= y && y <= IBOX__BOTTOM (&obj->box))
      {
	int pos;
	pos = (y - obj->box.Top) / obj->item_box.Height;
	pos = max (0, min (obj->nmb_rows-1, pos));

	pos += obj->vc_first;
	if (!SMN_DISABLED_P (&obj->data[pos]))
	  return pos;
      }
  else if (STRIP_V_ITEM_INVISIBLE_BOTTOM_P (obj)
	   && IBOX__BOTTOM (&obj->box) < y
	   && y < IBOX__BOTTOM (&obj->box) + obj->item_box.Height)
    {
      Delay (24 / (y - IBOX__BOTTOM (&obj->box)));
      STRIP_V_TICKS_ENABLE (obj);
      return obj->vc_first + obj->vc_nrows;
    }
  else if (STRIP_V_ITEM_INVISIBLE_TOP_P(obj)
	   && (obj->box.Top - obj->item_box.Height) <= y
	   && y < obj->box.Top)
    {
      Delay (24 / (obj->box.Top - y));
      STRIP_V_TICKS_ENABLE (obj);
      return obj->vc_first - 1;
    }
    
  return -1;
}

#define STRIP__ABS_MOUSE_TO_ITEM(strip_, x_, y_)	\
 (strip_item_find ((strip_),				\
		   (x_) - (strip_)->win->LeftEdge,	\
		   (y_) - (strip_)->win->TopEdge))

/* Update all `views' of OBJ to reflect state changes in `model' of
   OBJ */
static void
strip_view_update (struct strip *obj)
{
  local_precond (obj);

  /* return if up to date */
  if (obj->highlighted == obj->selected)
    return;

  /* update hightlight (select bar) and do scrolling if necessary to
     make the selected item visible */
  if (obj->highlighted != -1)
    item_unselect (obj, obj->highlighted);
  obj->highlighted = obj->selected;
  if (obj->highlighted != -1)
    {
      if (!(STRIP_V_ITEM_VISIBLE_P (obj, obj->highlighted)))
	item_make_visible (obj, obj->highlighted);
      item_select (obj);
    }
}

int strip_pop_up (const char *title,
		  int x, int y, struct menu_create_hook *creator,
		  struct Screen *screen,
		  struct strip *parent);

/* call strip_pop_up() for submenu on position OBJ->selected */
static int
strip_pop_up_sub (struct strip *obj)
{
  int id;			/* RESULT */
#ifdef AMIGA_MENU_INACTIVE
  ModifyIDCMP (obj->win, (obj->win->IDCMPFlags 
			 & ~(IDCMP_INACTIVEWINDOW)));
#endif
  id = strip_pop_up (0,
		     STRIP_SUBMENU_LEFT_POS (obj, obj->selected),
		     STRIP_SUBMENU_TOP_POS (obj, obj->selected),
		     obj->data[obj->selected].mn_sub,
		     obj->win->WScreen, obj);
#ifdef AMIGA_MENU_INACTIVE
  ModifyIDCMP (obj->win, (obj->win->IDCMPFlags 
			 | (IDCMP_INACTIVEWINDOW)));
#endif
  return id;
}

int
strip_pop_up_sub_2 (struct strip *obj)
{
  int id;			/* RESULT */
  id = strip_pop_up_sub (obj);
  {
    struct IntuiMessage *im, *next;
    struct MsgPort *mp = obj->win->UserPort;

    Forbid();
    for (im = (struct IntuiMessage *) mp->mp_MsgList.lh_Head;
	 (next =  (struct IntuiMessage *) im->ExecMessage.mn_Node.ln_Succ);
	 im = next)
      if (im->Class != IDCMP_REFRESHWINDOW) 
	Remove ((struct Node *) im), ReplyMsg ((struct Message *) im);
    Permit ();
  }
  return id;
}

/* remember last used input source to disable ACTIVEWINDOW =>
   MOUSEMOVE mapping for keyboard users */
static enum { MOUSE_INPUT, KEYBOARD_INPUT } last_move_input;

int
strip_pop_up (const char *title,
	      int x, int y, struct menu_create_hook *creator,
	      struct Screen *screen,
	      struct strip *parent)
{
  int done, id;
  struct strip obj;
  unsigned long enter_time;

  {
    ULONG secs, micros;
    CurrentTime (&secs, &micros);
    enter_time = secs * 1000 + micros / 1000;
  }

  if (!strip_create_2 (&obj, parent, title, creator, screen, x, y))
    return -1;
#define WIN_STRIP(WIN) ((struct strip *)(WIN)->UserData)

  done = 0;
  id = 0;
  obj.selected = -1;
  while (!done)
    {
      struct IntuiMessage *im;

      WaitPort (obj.win->UserPort);

      for (; (im = (struct IntuiMessage *) GetMsg  (obj.win->UserPort));
	   XREPLY_MSG (im))
	{
	  /* for foreign windows ignore all events except
             IDCMP_REFRESHWINDOW */
	  if (WIN_STRIP (im->IDCMPWindow) != &obj
	      && im->Class != IDCMP_REFRESHWINDOW)
	    continue;

	switch (im->Class)
	  {
	  case IDCMP_REFRESHWINDOW:
	    BeginRefresh (im->IDCMPWindow);
	    strip_draw (WIN_STRIP (im->IDCMPWindow));
	    EndRefresh (im->IDCMPWindow, TRUE);
	    break;

#ifdef AMIGA_MENU_INACTIVE
	  case IDCMP_INACTIVEWINDOW:
	    /* unwind single frame */
	    /* id = -1; */ id = -9999;
	    done = 1;
	    break;
#endif
	  case IDCMP_RAWKEY:
	    /* skip upstrokes */
	    if (im->Code & IECODE_UP_PREFIX)
	      continue;		/* => GetMsg () loop */
	    switch (im->Code)
	      {
	      case RK_ESC:
		done = 1;
		id = -9999;
		break;

	      case CURSORDOWN:
	      case CURSORUP:
		last_move_input = KEYBOARD_INPUT;
		{
		  int pos, i, pos_increment;

		  pos_increment = (im->Code == CURSORDOWN) ? 1 : -1;

		  if ((pos = obj.selected) == -1 && im->Code == CURSORUP)
		    pos = STRIP__NITEMS (&obj);

		  for (i = 0; i < STRIP__NITEMS (&obj); i += abs (pos_increment))
		    /* (avoid endless cycling if there are no enabled items) */
		    {
		      pos += pos_increment;

		      /* try wrap */
		      if (pos >= 0)
			pos %= STRIP__NITEMS (&obj);
		      else if ((pos = (STRIP__NITEMS (&obj) - 1)) < 0)
			break;

		      /* done */
		      if (!SMN_DISABLED_P (&obj.data[pos]))
			{
			  obj.selected = pos;
			  strip_view_update (&obj);
			  break;
			}
		    }
		}
		break;

	      case CURSORLEFT:
	      case RK_DEL:
	      case RK_BACKSPACE:
		/* unwind single frame */
		id = -1;
		done = 1;
		break;

	      case RK_RETURN:
		if (obj.selected == -1)
		  {
		    id = -1;
		    done = 1;
		    break;
		  }
		else if (!STRIP_SUBMENU (&obj, obj.selected))
		  {
		    id = obj.data[obj.selected].mn_id;
		    done = 1;
		    break;
		  }
		/* FALL THROUGH */
	      case CURSORRIGHT:
		/* open sub menu */
		if (obj.selected != -1 && STRIP_SUBMENU (&obj, obj.selected))
		  {
		    XREPLY_MSG (im);

		    id = strip_pop_up_sub_2 (&obj);
		    if (id != -1)
		      done = 1; /* either done or unwind */
		    if (id < 0)
		      ++id;	/* unwind stops when id==-1 */
		  }
		break;

	      }
	    break;
	  case IDCMP_MOUSEBUTTONS:
	    {
#ifdef emacs
	      Awin_button_note_change (im);
#endif
	      if (im->Code & IECODE_UP_PREFIX)
		{
		/* open sub menu */
		if (obj.selected != -1 && STRIP_SUBMENU (&obj, obj.selected))
		  {
		    XREPLY_MSG (im);

		    id = strip_pop_up_sub_2 (&obj);
		    if (id != -1)
		      done = 1; /* either done or unwind */
		    if (id < 0)
		      ++id;	/* unwind stops when id==-1 */

		    break;
		  }

		  if (!(im->Qualifier & (IEQUALIFIER_LSHIFT | IEQUALIFIER_RSHIFT)))
		    {
		      ULONG event_time;
		      event_time = im->Seconds * 1000 + im->Micros / 1000;
		      /* If the user releases a mousebutton within 0.75 seconds after
			 entering this function, its not used as selection.  */
		      if (!enter_time || event_time > (enter_time + 750))
			{
			  done = 1;
			  id = (obj.selected == -1) ? -1 : obj.data[obj.selected].mn_id;
			}
		      else
			enter_time = 0;
		    }
		  else
		    if (obj.selected != -1 && creator->select)
		      (*creator->select) (creator, obj.selected);
		}
#if 0 /* counter intuitive -bw/17-May-98 */
	      else if (im->Code == MENUDOWN)
		{
		  done = 1;
		  id = -1;
		}
#endif /* 0 */
	    }
	    break;

	  case IDCMP_ACTIVEWINDOW:
	    /* don't fake a mouse move while using keyboard */
	    if (last_move_input == KEYBOARD_INPUT)
	      break;
	    /* FALL THROUGH */
	  case IDCMP_MOUSEMOVE:
	    last_move_input = MOUSE_INPUT;
	    /* FALL THROUGH */
	  case IDCMP_INTUITICKS:
	    {
	      int pos;
	      pos = strip_item_find (&obj, im->MouseX, im->MouseY);
	      if (SMN_DISABLED_P (&obj.data[pos]))
		pos = -1;
	      if (pos != obj.selected)
		{
		  int old_pos;	/* avoid displaying the same submenu twice */

		  obj.selected = pos;
		  strip_view_update (&obj);

		  old_pos = -1;
		  if (!(MENU_CLICK_SUBMENU_P (&obj)
		    /* don't pop up sub-menus while scrolling */
		      || im->MouseY < obj.box.Top
		      || IBOX__BOTTOM (&obj.box) < im->MouseY))
		  /* open sub menu */
		  while (!done && obj.selected != -1
			 && old_pos != obj.selected
			 && STRIP_SUBMENU (&obj, obj.selected))
		    {
		      old_pos = obj.selected;
		      XREPLY_MSG (im);

		      id = strip_pop_up_sub_2 (&obj);

		      if (id != -1)
			done = 1; /* either done or unwind */
		      if (id < 0)
			++id;	/* unwind stops when id==-1 */
		    }
		}
	      /* check for mouse in parent windows */
	      if (pos == -1 && parent
		  && (im->MouseX < 0 || im->MouseY < 0
		      || im->MouseX > obj.win->Width 
		      || im->MouseY > obj.win->Height))
		{
		  struct strip *i;
		  int count;
		  int abs_x, abs_y;
		  struct Layer const *pos_layer; /* to check for visibility */

		  abs_x = obj.win->LeftEdge + im->MouseX;
		  abs_y = obj.win->TopEdge + im->MouseY;
		  pos_layer = WhichLayer (&im->IDCMPWindow->WScreen->LayerInfo,
					  abs_x, abs_y);
		  count = -1;
		  for (i = parent; i; i=i->parent, --count)
		    {
		      int pos;
		      if (STRIP__RPORT (i)->Layer != pos_layer)
			continue;

		      pos = STRIP__ABS_MOUSE_TO_ITEM (i, abs_x, abs_y);
		      if (pos != -1 && pos != i->selected)
			{
			  i->selected = pos;
			  strip_view_update (i);
			  done = 1;
			  id = count;
			  break;
			}
		    }
		}
	    }
	    break;
	  }
	}
    }

  strip_destroy (&obj);
  return id;
}

#ifdef emacs

/* Emacs menu builder */

#define MENU_ITEMS_PANE_NAME 1
#define MENU_ITEMS_PANE_PREFIX 2
#define MENU_ITEMS_PANE_LENGTH 3

#define MENU_ITEMS_ITEM_NAME 0
#define MENU_ITEMS_ITEM_ENABLE 1
#define MENU_ITEMS_ITEM_VALUE 2
#define MENU_ITEMS_ITEM_EQUIV_KEY 3
#define MENU_ITEMS_ITEM_DEFINITION 4
#define MENU_ITEMS_ITEM_LENGTH 5

extern int menu_items_used;
extern Lisp_Object *volatile menu_item_selection;
extern Lisp_Object menu_items;
/* The number of panes currently recorded in menu_items,
   excluding those within submenus.  */
extern int menu_items_n_panes;

struct menu_create_hook_emacs
{
  struct menu_create_hook base;
  int start_idx, end_idx;
};

void menu_create_emacs ();
void menu_destroy_emacs ();

/* get customizations from LISP and fill in a config struct for faster
   and encapsulated accessing. */
void
win_menu_fill_config ()
{
  Lisp_Object key, val;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (key, val); /* really needed? */
  bzero (&cfg, sizeof cfg);

  /* how to access a submenu using the mouse */
  if (Fboundp (key = intern ("amiga-menu-mouse-submenu")) 
      && !NILP (val = Feval (key)))
    if (val == intern ("mouse-button-up"))
      cfg.submenu_opener = 1;
    if (val == intern ("mouse-button-down"))
      cfg.submenu_opener = 2;

  if (Fboundp (key = intern ("amiga-menu-font-style")) 
      && !NILP (val = Feval (key)))
    if (val == intern ("bold"))
      cfg.normal_font_style = FSF_BOLD;
    if (val == intern ("underline"))
      cfg.normal_font_style = FSF_UNDERLINED;

  if (Fboundp (key = intern ("amiga-menu-font")) 
      && !NILP (val = Feval (key)))
    if (STRINGP (val))
    {	
      /* TODO-bw/18-May-98 */
    }
    UNGCPRO;
}

int
menu_skip_submenu (int first, int max)
{
  int i, sub_depth, size;

  i = first;
  sub_depth = 0;
  while (i < max)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	{ size = 1; ++sub_depth; }
      else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	{ size = 1; --sub_depth; }
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	{ size = 1; /* dialogbox formatting */ }
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	size = MENU_ITEMS_PANE_LENGTH;
      else
	{ i += MENU_ITEMS_ITEM_LENGTH; continue; }

      if (sub_depth == 0)
	return i;
      else
	i += size;
    }

  if (sub_depth == 0)
    return i;
  else
    return -1;
}

void
menu_create_emacs (struct menu_create_hook_emacs *data)
{
  int submenu_depth, first_pane, items_max, items_nmb, i;
  smn *items;

  /* Loop over all panes and items, filling in the tree.  */
  submenu_depth = 0;
  first_pane = 1;
  items_max = items_nmb = 0;
  items = 0;

  i = data->start_idx;
  while (i < data->end_idx)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	{
	  if (!submenu_depth++)
	    {
	      struct menu_create_hook_emacs *sub;
	      assert (items_nmb > 0);
	      sub = calloc (sizeof *sub, 1);
	      if (!sub)
		return;		/* XXX-bw/20-Nov-97 */
	      sub->base.create = menu_create_emacs;
	      sub->base.destroy = menu_destroy_emacs;
	      sub->base.select = 0;
	      items[items_nmb-1].mn_sub = &sub->base;
	      sub->start_idx = i+1;
	    }

	  ++i;
	  first_pane = 1;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	{
	  if (!--submenu_depth)
	    {
	      assert (items_nmb > 0 && items[items_nmb-1].mn_sub);
	      ((struct menu_create_hook_emacs *)items[items_nmb-1].mn_sub)
		->end_idx = i;
	    }

	  ++i;
	  first_pane = 0;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	++i;
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	/* Create a new pane.  */
	{
	  Lisp_Object pane_name, prefix;
	  char *pane_string;

	  pane_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME];
	  prefix = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
	  pane_string = (NILP (pane_name)
			 ? "" : (char *) XSTRING (pane_name)->data);

	  /* XXX-bw/24-Nov-97: This is sometimes ok, but sometimes the
             pane_name should not simply discarded (like in font
             menues with only one font */
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (/* !keymaps && */ strcmp (pane_string, ""))
	    {
	      int end;
	      end = menu_skip_submenu (i + MENU_ITEMS_PANE_LENGTH, data->end_idx);
	      if (end > 0)
		{
		  struct menu_create_hook_emacs *sub;
		  if (items_nmb == items_max)
		    {
		      void *p = realloc (items, sizeof (smn)
					 * (items_max+=30));
		      if (!p)
			{
			  /* cleanup TODO-bw/20-Nov-97 */
			  free (items);
			  return;
			}
		      items = p;
		    }

		  sub = calloc (sizeof *sub, 1);
		  if (!sub)
		    return;		/* XXX-bw/20-Nov-97 */
		  sub->base.create = menu_create_emacs;
		  sub->base.destroy = menu_destroy_emacs;
		  sub->base.select = 0;
		  items[items_nmb].mn_sub = &sub->base;
		  sub->start_idx = i + MENU_ITEMS_PANE_LENGTH;
		  sub->end_idx = end;

		  items[items_nmb].mn_label = pane_string;
		  items[items_nmb].mn_kbequiv = 0;
		  items[items_nmb].mn_flags = 0;
		  items[items_nmb].mn_id = 0;
		  ++items_nmb;

		  i = end;
		  first_pane = 0; /* ??? */
		  continue;
		}
	    }
	  else if (first_pane)
	    {
	    }

	  first_pane = 0;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else if (!submenu_depth)
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def;
	  item_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_NAME];
	  enable = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_ENABLE];
	  descrip
	    = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_EQUIV_KEY];
	  def = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_DEFINITION];

	  if (items_nmb == items_max)
	    {
	      void *p = realloc (items, sizeof (smn) * (items_max+=30));
	      if (!p)
		{
		  /* cleanup TODO-bw/20-Nov-97 */
		  free (items);
		  return;
		}
	      items = p;
	    }

	  /* find out kind of menu item */
	  if (strspn (XSTRING (item_name)->data, "- ") != XSTRING (item_name)->size)
	    /* not a separator */
	    items[items_nmb].mn_flags 
	      = (NILP (enable) ? (SMN_GHOSTED | SMN_DISABLED) : 0);
	  else if (items_nmb == 0 || (items[items_nmb - 1].mn_flags & SMN_SEP))
	    /* unwanted separator (leading item or successor of another separator */
	    {
	      i += MENU_ITEMS_ITEM_LENGTH;
	      continue;
	    }
	  else
	    /* separator */
	    items[items_nmb].mn_flags |= (SMN_SEP | SMN_DISABLED);

	  items[items_nmb].mn_label = XSTRING (item_name)->data;
	  items[items_nmb].mn_kbequiv
	    = ((STRINGP (descrip)) ? XSTRING (descrip)->data : 0);


	  items[items_nmb].mn_sub = 0; /* see above */

	  /* we use items index in menu_items[] or 0 as menu ID */ 
	  items[items_nmb].mn_id = (NILP (def)) ? 0 : i;

#if 1
	  /* workaround until xmenu supports the real thing */
	  if (strlen (items[items_nmb].mn_label) >= 3)
	    {
	      const char *p = items[items_nmb].mn_label;
	      if (p[0] == '[' && p[2] == ']')
		{
		  items[items_nmb].mn_flags |= ((p[1] == ' ') ? SMN_BUTTON 
						: (SMN_BUTTON | SMN_STATE));
		  items[items_nmb].mn_label += 3;
		  while (*items[items_nmb].mn_label == ' ')
		    ++items[items_nmb].mn_label;
		}
	      else if (p[0] == '(' && p[2] == ')')
		{
		  items[items_nmb].mn_flags |= ((p[1] == ' ') ? SMN_RADIO
						: (SMN_RADIO | SMN_STATE));
		  items[items_nmb].mn_label += 3;
		  while (*items[items_nmb].mn_label == ' ')
		    ++items[items_nmb].mn_label;
		}
	    }
#endif /* 1 */
  
	  ++items_nmb;
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
      else
	i += MENU_ITEMS_ITEM_LENGTH; /* a submenu item */
    }

  if (!items)
    return;			/* XXX-bw/20-Nov-97 */

  if (items_nmb > 0 && (items[items_nmb - 1].mn_flags & SMN_SEP))
    /* remove unwanted trailing separator */
    {
      --items_nmb;
      bzero (&items[items_nmb], sizeof *items);
    }

  /* give back unused cells */
  items = realloc (items, sizeof (smn) * items_nmb);
  if (!items)
    abort ();

  data->base.res_nrows = items_nmb;
  data->base.res_strip = items;
}

/* The screen-wd is needed for doing a refresh for all emacs frames when
   closing a menu */
static Window amenu_screen_wd;

void
menu_destroy_emacs (struct menu_create_hook_emacs *data)
{
  int i;
  for (i=0; i < data->base.res_nrows; ++i)
    if (data->base.res_strip[i].mn_sub)
      free (data->base.res_strip[i].mn_sub);
  free (data->base.res_strip);
  if (amenu_screen_wd)
    Awinp_refresh_windows (amenu_screen_wd);
}

Lisp_Object
xmenu_show (f, x, y, for_click, keymaps, title, error)
     FRAME_PTR f;
     int x;
     int y;
     int for_click;
     int keymaps;
     Lisp_Object title;
     char **error;
{
  int id;
  Lisp_Object *menu_item_selection;
  struct Screen *screen;
  SIGMASKTYPE sig_mask;
  struct menu_create_hook_emacs hook 
    = {  
	{menu_create_emacs, menu_destroy_emacs,},
	0, menu_items_used
      };

  check_precond (WD_WIN_P (f->output_data.x->window_desc));

  *error = 0;
  screen = WD_WIN (f->output_data.x->window_desc)->WScreen;
  amenu_screen_wd = WD_PARENT (f->output_data.x->window_desc);
#ifdef XAPI_GZZ
  x += WD_WIN (f->output_data.x->window_desc)->BorderLeft;
  y += WD_WIN (f->output_data.x->window_desc)->BorderTop;
#endif

  /* This could signal an error. Try to avoid allocating
     resources before this line. -bw/18-May-98 */
  win_menu_fill_config ();

  BLOCK_INPUT;
  sig_mask = sigblock (sigmask (SIGINT));

  draw_info = GetScreenDrawInfo (screen);
  if (!draw_info)
    {
      sigsetmask (sig_mask);
      UNBLOCK_INPUT;
      return Qnil;
    }
  {
    void *context;
    context = (void *)Awin_notify_menu_enter (f);

    id = strip_pop_up (STRINGP(title) ? XSTRING (title)->data : 0,
		       x + WD_LEFT (f->output_data.x->window_desc),
		       y + WD_TOP (f->output_data.x->window_desc),
		       &hook.base, screen, 0);

    Awin_notify_menu_leave (context, f);
  }

  FreeScreenDrawInfo (screen, draw_info);
  draw_info = 0;

  sigsetmask (sig_mask);
  UNBLOCK_INPUT;

  if (id <= 0)
    return Qnil;

  menu_item_selection = &XVECTOR(menu_items)->contents[id];

  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (menu_item_selection != 0)
    {
      int i;
      Lisp_Object prefix, entry;
      Lisp_Object subprefix_stack[menu_items_used];
      int submenu_depth;      

      submenu_depth = 0;
      prefix = Qnil;
      i = 0;
      while (i < menu_items_used)
	{
	  if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	    {
	      subprefix_stack[submenu_depth++] = prefix;
	      prefix = entry;
	      i++;
	    }
	  else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	    {
	      prefix = subprefix_stack[--submenu_depth];
	      i++;
	    }
	  else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	    {
	      prefix
		= XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
	      i += MENU_ITEMS_PANE_LENGTH;
	    }
	  else
	    {
	      entry
		= XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_VALUE];
	      if (menu_item_selection == &XVECTOR (menu_items)->contents[i])
		{
		  if (keymaps != 0)
		    {
		      int j;

		      entry = Fcons (entry, Qnil);
		      if (!NILP (prefix))
			entry = Fcons (prefix, entry);
		      for (j = submenu_depth - 1; j >= 0; j--)
			if (!NILP (subprefix_stack[j]))
			  entry = Fcons (subprefix_stack[j], entry);
		    }
		  return entry;
		}
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
    }
  return Qnil;
}

#else  /* not emacs */

/* test menu builder */

struct menu_create_hook_1
{
  struct menu_create_hook base;
  char *labels[40];
  struct menu_create_hook *subs[40];
  char ghosted[40];
  char *shortcuts[40];
  char button[40];
};

void
menu_create_1 (struct menu_create_hook_1 *data)
{
  int i;
  smn *strip;
  for (i=0; data->labels[i]; ++i)
    ;
  strip = data->base.res_strip = calloc (i, sizeof (smn));
  if (!strip)
    return;
  data->base.res_nrows = i;

  for (i=0; data->labels[i]; ++i)
    {
      strip[i].mn_label = data->labels[i];
      strip[i].mn_kbequiv = data->shortcuts[i];
      strip[i].mn_sub = data->subs[i];
      strip[i].mn_flags = data->ghosted[i] ? (SMN_DISABLED | SMN_GHOSTED) : 0;

      if (data->button[i] < 0)
	strip[i].mn_flags |= SMN_BUTTON;
      else if (data->button[i] > 0)
	strip[i].mn_flags |= (SMN_BUTTON | SMN_STATE);

      if (0==strcmp (strip[i].mn_label, "--"))
	strip[i].mn_flags |= (SMN_DISABLED | SMN_SEP);
    }
}

void
menu_destroy_1 (struct menu_create_hook_1 *data)
{
  free (data->base.res_strip);
}

extern struct menu_create_hook_1 menu_create_hook_main,
  menu_create_hook_sub, menu_create_hook_sub_2, menu_create_hook_sub_3,
  menu_create_hook_sub_4;

struct menu_create_hook_1 menu_create_hook_sub
= {
    { menu_create_1, menu_destroy_1, 0, 0 },
    {"sub 1", "sub 2", "subaaaaaaaaaaaaaddddddddddddffafafafafafafaf 3", "sub 4"},
    {0, 0, 0, &menu_create_hook_sub_3.base, },
    {0, 1, 0, 0},
    {0},
  };

/* big array for testing scroll feature */
struct menu_create_hook_1 menu_create_hook_sub_2
= {
    { menu_create_1, menu_destroy_1, 0, 0 },
    {
      "sub 1", "sub 2", "sub 3", "sub 4",
      "sub 1", "sub 2", "sub 3", "sub 4",
      "sub 1", "sub 2", "sub 3", "sub 4",
      "sub 1", "sub 2", "sub 3", "sub 4",
      "--", "sub 2", "sub 3", "sub 4",
      "sub 1", "sub 2", "sub 3", "sub 4",
      "sub 1", "--", "sub 3", "sub 4",
      "sub 1", "sub 2", "sub 3", "sub 4",
      "sub 1", "sub 2", "sub 3", "sub 4",
      "sub 1", "sub 2", "sub 3", "--",
    },
    {0, 0, 0, &menu_create_hook_sub.base, &menu_create_hook_sub_4.base,},
    {0},			/* ghosted */
    {0},			/* shortcuts */
    {0, 0, 1, -1, 0, 0, 1},	/* buttons */
  };

struct menu_create_hook_1 menu_create_hook_sub_3
= {
    { menu_create_1, menu_destroy_1, 0, 0 },
    {"sub 1", "sub 2", "subaaaaaaaaaaaaaddddddddddddffafafafafafafaf 3"
     "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaadddddddaa", "sub 4"},
    {0, &menu_create_hook_sub_2.base, },
    {0, 0, 1, 0},
    {0},
  };

struct menu_create_hook_1 menu_create_hook_sub_4
= {
    { menu_create_1, menu_destroy_1, 0, 0 },
    {"sub 1", "sub 2", "sub 3", "sub 4"},
    {0, &menu_create_hook_sub_2.base, },
    {1, 1, 1, 1},
    {0},
  };


struct menu_create_hook_1 menu_create_hook_main
= {
    { menu_create_1, menu_destroy_1, 0, 0 },
    {"item 1 gjAB", "item 2ypq", "item 3", "item 4", "item 5",},
    {&menu_create_hook_sub_2.base, &menu_create_hook_sub_3.base,
     &menu_create_hook_sub.base, 0},
    {0, 0, 0, 0},
    {0, 0, 0, "M-S-3", "C-x"},
  };


int
main ()
{
  struct Screen *screen;
  int result=20;

  screen = LockPubScreen (0);
  if (screen)
    {
      draw_info = GetScreenDrawInfo (screen);
      if (draw_info)
	{
	  result = strip_pop_up ("Test", 400, 100,
				 &menu_create_hook_main.base, screen, 0);
	  FreeScreenDrawInfo (screen, draw_info);
	}
      UnlockPubScreen (0, screen);
    }
  if (db_nmb_allocs != db_nmb_frees)
    fprintf (stderr, "possible memory leak: %d allocs; %d frees\n",
	     db_nmb_allocs, db_nmb_frees);
  return result;
}
#endif /* not emacs */
/*
   Local Variables:
   compile-command: "nice -n1 gcc -Wall -g -DTEST amiga_window_menu.c -lauto"
   process-connection-type: nil
   End:
 */

