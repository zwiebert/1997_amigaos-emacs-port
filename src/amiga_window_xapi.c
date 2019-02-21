/* amiga_window_xapi.c - Limited X  API emulation for GNU Emacs.

   Author: Bert Winkelmann <bertw@in-brb.de> (1997, 1998)
  */

/* x,y in XAPI from inner-edge of the parent to outer-edge of the child
   x,y in WD: from outer-edge to outer-edge.
   width,height: inner sizes.

   That means, the postion argument of XDrawRectangle is relative to the inner edge.
   -bw/11-Jun-98 */

#include "amiga_window_.h"
#include "amiga_window_defs.h"
#include <string.h>
#include <stdio.h>

extern int interrupt_input_blocked;

#define WD_DRAWABLE_P(wd_) (WD_MAINWIN_P (wd_) || WD_SUBWIN_P (wd_))

static int awxa_sync=1;

static long Awin_map_x_to_idcmp (long x_input_mask);
static Window win__rport_wd_and_more (Window wd, int *left, int *top, 
				      int *foreground, int *background);

static inline void
AWXA_WD_NOTE_CHANGE(wd, mask)
{
  if (awxa_sync && (WD_WIN_P (wd) || WD_BOOPSI_P (wd) || WD_SUBWIN_P (wd)))
    {
      WD_INFO (wd)->change_flags |= (mask);
      win__wd_flush (wd);
    }
  else
    WDI_CH_ADD ((wd), (mask));
}



bool
XQueryTree (Display *dpy, Window wd, Window *store_root, Window *store_parent,
	    Window **store_children, /* malloc allocated array, hast to
				       freed by caller */
	    int *store_nmb_children)
{
  int i;
  Window root;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (!WD_SCREEN_P (wd)); /* or should the root be its own root? */
  check_precond (store_root && store_parent && store_children && store_nmb_children);

  if (WD_CHILD (wd) == WD_NIL)
    {
      *store_children = 0;
      *store_nmb_children = 0;
    }
  else
    {
      int n_childs;
      Window childs[100];

      for (n_childs = 0, i = WD_CHILD (wd); 
	   n_childs < 100 && (i = WD_NEXT (i)) != WD_NIL;
	   ++n_childs)
	childs[n_childs] = i;

      *store_children = malloc (sizeof (Window) * (n_childs + 1));
      if (!*store_children)
	*store_nmb_children = 0;
      else
	{
	  *store_nmb_children = n_childs;
	  (*store_children) [n_childs] = WD_NIL;
	  bcopy (childs, *store_children, sizeof (Window) * n_childs);
	}
    }
  *store_parent = WD_PARENT (wd);

  for (root = WD_NIL, i = wd; (i = WD_PARENT (i)) != WD_NIL; root = i);

  *store_root = root;

  return store_children != 0;
}

/* XQueryPointer is unimplemented */
bool
XQueryPointer (Display *dpy, Window root,
		     /* The root window which contains the pointer.  */
	       Window *store_root,
		     /* Trash which we can't trust if the pointer is on
			a different screen.  */
	       Window *store_child,
	       /* The position on that root window.  */
	       int *store_root_x, int *store_root_y,
	       /* More trash we can't trust.  */
	       int *store_win_x, int *store_win_y,
	       /* Modifier keys and pointer buttons, about which
		  we don't care.  */
	       unsigned int *store_mask)
{
  check_precond (dpy);
  check_precond (store_root && store_child && store_root_x && store_root_y
		 && store_win_x && store_win_y && store_mask);
  check_precond (!"implemented");
  
  *store_root = WD_NIL;
  *store_child = WD_NIL;
  *store_root_x = *store_root_y = *store_win_x = *store_win_y =0;
  *store_mask = 0;
  return false;
}


void
XRaiseWindow  (Display *dpy, Window wd)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));

  if (WD_WIN_P (wd))
    {
      ScreenToFront (WD_WIN (wd)->WScreen); /* What about screen dragging? ???-bw/27-Apr-98 */
      WindowToFront (WD_WIN (wd));
      ActivateWindow (WD_WIN (wd));  /* ???--bw/27-Oct-97: Emacs seems to
					expect that.  I don't know if it's
					the right X behaviour.  I think it
					is.  */
    }
}

void
XLowerWindow  (Display *dpy, Window wd)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));

  if (WD_WIN_P (wd))
    WindowToBack (WD_WIN (wd));
}
  
bool
XWithdrawWindow (Display *dpy, Window wd, Screen *screen)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (screen);

  if (WD_WIN_P (wd)
      && win__un_map_iwin (wd))
    {
      assert (WD_NWIN_P (wd));
      Awin_event_put (wd, WD_EVENT_UNMAP);
      return true;
    }
  return false;
}

/* XXX */
void
XMapWindow (Display *dpy, Window wd)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));

  DB_TRACE;

  /* XXX */
  if (WD_NWIN_P (wd))
    {
      int i, tmp;

      win__map_nwin (wd);
      Awin_event_put (wd, WD_EVENT_MAP);

      /* Remove any icon attached to this X window. */
      WD_CHILD_FOREACH_RM (wd, i, tmp)
	if (WD_ICON_P (i))
	  win__destroy (i);
    }
  else if (WD_SUBWIN_P (wd))
    {
      win__map_subwin (wd);
    }
  else if (WD_WIN_P (wd))
    {
      Awin_event_put (wd, WD_EVENT_MAP);
    }
}


void
XMapRaised  (Display *dpy, Window wd)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));

  DB_TRACE;

  /* XXX */
  if (WD_NWIN_P (wd))
    {
      int i, tmp;

      win__map_nwin (wd);
      XRaiseWindow (dpy, wd);
      Awin_event_put (wd, WD_EVENT_MAP);

      /* Remove any icon attached to this X window. */
      WD_CHILD_FOREACH_RM (wd, i, tmp)
	if (WD_ICON_P (i))
	  win__destroy (i);
    }
  else if (WD_SUBWIN_P (wd))
    {
      win__map_subwin (wd);
    }
  else if (WD_WIN_P (wd))
    {
      Awin_event_put (wd, WD_EVENT_MAP);
    }
}

Window
XCreateWindow (Display *dpy,
	       Window parent,
	       int left, int top, int pix_width, int pix_height,
	       int border_width, /* unused */
	       int depth,	/* unused */
	       int in_out,	/* unused */
	       Visual *visual,
	       int attr_mask,
	       const XSetWindowAttributes *attrs)
{
  Window wd;			/* RESULT */
  int i;

  int parent_top, parent_left, parent_width, parent_height;

  check_precond (dpy && dpy->screen);
  check_precond (WD_VALID_P (parent));
  check_precond (WD_SCREEN_P (parent) || WD_WIN_P (parent));
  check_precond (pix_width >= 0 && pix_height >= 0);

  /* On Intuition the window have to placed inside the screen */
  if (left < 0) left = 0;
  if (top < 0) top = 0;

  if (WD_MAINWIN_P (parent))
    /* create subwindow */
    {
      DPY_GZZ_TO_REAL_XY (dpy, left, top);

      wd = win__create (WT_SUBWIN);
      if (!WD_NIL_P (wd))
	{

	  GX_COLOR_A_SET (WD_GX(wd), ((attrs && (attr_mask & CWForePixel)) 
				      ? attrs->foreground_pixel
				      : WD_COLOR_A (parent)));
	  
	  GX_COLOR_B_SET (WD_GX(wd), ((attrs && (attr_mask & CWBackPixel))
				      ? attrs->background_pixel
				      : WD_COLOR_B (parent)));
	  
	  WD_LEFT (wd) = left;
	  WD_TOP (wd) = top;
	  WD_WIDTH (wd) = (pix_width ? pix_width 
			   : (WD_WIDTH (parent) - WD_LEFT (wd)));
	  WD_HEIGHT (wd) = (pix_height ? pix_height 
			    : (WD_HEIGHT (parent) - WD_TOP (wd)));

	  if (!win__attach (parent, wd))
	    abort ();
	}
      return wd;
    }
  else if (WD_SCREEN_P (parent))
    /* create main window */
    {
      assert (WD_NIL_P (WD_PARENT (parent)));

#ifdef ALL_GZZ
      DPY_GZZ_TO_REAL_XY (dpy, left, top);
#endif /* not ALL_GZZ */
      /* Adjust sizes for Intuition windows from GZZ to real. */
      DPY_GZZ_TO_REAL_WH (dpy, pix_width, pix_height);

      win__init_pref ();
      /* fill win_new_screen with defaults */
      win__pref ();

      win_new_win.Screen = WD_SCREEN (parent);
      win_new_win.Type = ((win_new_win.Screen->Flags) & SCREENTYPE);

      win_new_win.LeftEdge = left + WD_LEFT (parent);
      win_new_win.TopEdge = top + WD_TOP (parent);
      win_new_win.Width = (pix_width ? pix_width 
			   : (WD_WIDTH (parent) - win_new_win.LeftEdge));
      win_new_win.Height = (pix_height ? pix_height 
			    : (WD_HEIGHT (parent)  - win_new_win.TopEdge));

      if (attrs && (attr_mask & CWEventMask))
	{
	  /* ???-bw: should we use `|=' ? */
	  win_new_win.IDCMPFlags = Awin_map_x_to_idcmp (attrs->event_mask);
	}

      wd = win__create (WT_NWIN);
      if (wd != WD_NIL)
	{
	  if (!win__attach (parent, wd))
	    assert (0);

	  GX_COLOR_A_SET (WD_GX(wd), ((attrs && (attr_mask & CWForePixel)) 
				      ? attrs->foreground_pixel
				      : WD_COLOR_A (parent)));
			
	  GX_COLOR_B_SET (WD_GX(wd), ((attrs && (attr_mask & CWBackPixel))
				      ? attrs->background_pixel
				      : WD_COLOR_B (parent)));

	  if (attr_mask & CWCursor)
	    {
	    }
	}
      return wd;
    }
}

void
XDestroyWindow (Display *dpy, Window wd)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));

  if (WD_WIN_P (wd))
    win__destroy (wd);
  else
    {
#if 1
      /* clear window and generate expose events for main window area
         and overlapping subwindows. */
      if (WD_SUBWIN_P (wd) && WD_MAPPED_P (wd))
	XClearArea (dpy, wd, 0, 0, 0, 0, True);
      win__destroy (wd);
#else
      int left, top, width, height;
      Window wd_rp;

      width = WD_WIDTH (wd);
      height = WD_HEIGHT (wd);

      wd_rp = win__rport_wd_and_more (wd, &left, &top, 0, 0);
      win__destroy (wd);

      if (wd_rp != WD_NIL)
	XClearArea (dpy, wd_rp, left, top, width, height, True);
      /* clear old scroller if not clipped (this happens if a new
         scroller is already placed on top of it */
#endif
    }
}

bool
XIconifyWindow (Display *dpy, Window wd, Screen *screen)
{
  Window icon;

  check_precond (dpy && screen);
  check_precond (WD_VALID_P (wd));
  check_precond (WD_NWIN_P (wd) || WD_WIN_P (wd));

  icon = win__create (WT_ICON);
  if (icon == WD_NIL)
    return false;

  if (!XWithdrawWindow (dpy, wd, screen))
    {
      win__destroy (icon);
      return false;
    }

  win__attach (wd, icon);
  return true;
}


bool
XConfigureWindow (Display *dpy, Window wd, int mask, XWindowChanges *wc)
{
  Window wd_clip;
  int change_flags;

  {
    check_precond (dpy);
    check_precond (WD_VALID_P (wd));
    check_precond (wc);
    check_precond (!(mask & CWX) || wc->x >= 0);
    check_precond (!(mask & CWY) || wc->y >= 0);
    check_precond (!(mask & CWWidth) || wc->width >= 0);
    check_precond (!(mask & CWHeight) || wc->height >= 0);
  }

  wd_clip = WD_NIL;
  change_flags = 0;
  
  if (mask)
    {
      int left, top, width, height;
      struct Window *win;
      struct ExtNewWindow *nw;
      if (WD_MAINWIN_P (wd))
	DPY_GZZ_TO_REAL_WH (dpy, wc->width, wc->height);
      else
	DPY_GZZ_TO_REAL_XY (dpy, wc->x, wc->y);
      win = 0;
      nw = 0;
      if (WD_WIN_P (wd))
	{
	  win = WD_WIN (wd);

	  left = win->LeftEdge;
	  top = win->TopEdge;
	  width = win->Width;
	  height = win->Height;
	  wd_clip = wd;
	}
      else if (WD_NWIN_P (wd))
	{
	  nw = WD_NWIN (wd);

	  left = nw->LeftEdge;
	  top = nw->TopEdge;
	  width = nw->Width;
	  height = nw->Height;
	}
      else if (WD_BOOPSI_P (wd))
	{
	  Window parent;

	  parent = WD_PARENT (wd);

	  if (WD_WIN_P (parent))
	    {
	      RemoveGadget (WD_WIN (parent), WD_BOOPSI (wd));
	      /* ...avoids update after clearing */

	      wd_clip = parent;

	      if (mask & (CWX | CWWidth))
		XClearArea (dpy, wd, 0, 0, 0, 0, True);
	      else if (mask & (CWY | CWHeight))
		{
#if 1
assert (0);
#else
		  int diff_top, diff_bottom;
		  int new_top, new_bottom;

		  new_top = (mask & CWY) ? wc->y : WD_TOP (wd);
		  new_bottom = ((mask & CWHeight) ? new_top + wc->height - 1 
				: WD_BOTTOM (wd));

		  if (new_top > WD_BOTTOM (wd) || new_bottom < WD_TOP (wd))
		    XClearArea (dpy, wd, 0, 0, 0, 0, True);
		  else
		    {
		      /* cut off top */
		      if (WD_TOP (wd) < new_top)
			XClearArea (dpy, wd, 0, 0, 0,
				    new_top - WD_TOP (wd),
				    True);

		      /* cut off bottom */
		      if (new_bottom < WD_BOTTOM (wd))
			XClearArea (dpy, wd, 0,
				    WD_HEIGHT (wd) - WD_BOTTOM (wd) +
				    new_bottom
		    0, 0, True);
		    }
#endif
		}

	      /* note window border trashing */
	      if (WD_RIGHT (wd) > WD_IRIGHT (parent)
		  || WD_BOTTOM (wd) > WD_IBOTTOM (parent))
		AWXA_WD_NOTE_CHANGE (parent, WDI_CH_FRAME);
	    }
	}
      else if (WD_SUBWIN_P (wd))
	{
	  Window parent;

	  parent = WD_PARENT (wd);

	  if (WD_WIN_P (parent))
	    {
	      wd_clip = parent;

	      /* clear parts no longer belong to this window */
	      if (mask & (CWX | CWWidth))
		XClearArea (dpy, wd, 0, 0, 0, 0, True);
	      else if (mask & (CWY | CWHeight))
		{
		  int new_top, new_bottom, new_height;

		  new_top = (mask & CWY) ? wc->y : WD_TOP (wd);
		  new_height = (mask & CWHeight) ? wc->height : WD_HEIGHT (wd);
		  new_bottom = new_top + new_height - 1;

		  if (new_top == WD_TOP (wd) && new_bottom == WD_BOTTOM (wd))
		    ;
		  else if (new_top <= WD_TOP (wd) && WD_BOTTOM (wd) <= new_bottom)
		    win_subwin_mp_push (wd, WD_SUBWIN_EVENT_EXPOSE, 0);
 		  else if (WD_BOTTOM (wd) < new_top || new_bottom < WD_TOP (wd))
		    XClearArea (dpy, wd, 0, 0, 0, 0, True);
		  else
		    {
		      /* cut off top */
		      if (WD_TOP (wd) < new_top)
			XClearArea (dpy, wd, 0, 0, 0,
				    new_top - WD_TOP (wd), /* height */
				    True);
		      /* cut off tail */
		      if (new_bottom < WD_BOTTOM (wd))
			XClearArea (dpy, wd, 0,
				    new_bottom - WD_TOP (wd), /* y */
				    0, 0, True);
		    }
		}
	    }
	}

      /* XXX-bw: X and Y are IMHO not the same as Left and Top. What
         about Gravity? -bw/20-Oct-97 */
      if (mask & CWX)
	{
	  WD_LEFT(wd) = left = wc->x;
	  change_flags |= WDI_CH_POS;
	}
      if (mask & CWY)
	{
	  WD_TOP(wd) = top = wc->y;
	  change_flags |= WDI_CH_POS;
	}
      if (mask & CWWidth)
	{
	  WD_WIDTH(wd) = width = wc->width;
	  change_flags |= WDI_CH_SIZE;
	}
      if (mask & CWHeight)
	{
	  WD_HEIGHT(wd) = height = wc->height;
	  change_flags |= WDI_CH_SIZE;
	}

      if (win || WD_BOOPSI (wd))
	AWXA_WD_NOTE_CHANGE (wd, change_flags);

      if (nw)
	{
	  nw->LeftEdge = left;
	  nw->TopEdge = top;
	  nw->Width = width;
	  nw->Height = height;
	}
    }

  /* update clip regions */
  if (wd_clip != WD_NIL)
    win__note_resize (wd_clip);

  return true;
}

void
XFlush () 
{
  check_precond (interrupt_input_blocked);

  win__flush ();
}

/* Move window to absolute position.  */
void
XMoveWindow (Display *dpy, Window wd, int left, int top)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (!WD_BOOPSI_P (wd)); /* TODO-bw/03-Nov-97 */

  if (WD_LEFT (wd) == left && WD_TOP (wd) == top)
    return;

#ifdef XAPI_GZZ
  if (!WD_MAINWIN_P (wd))
    DPY_GZZ_TO_REAL_XY (dpy, left, top);
#endif /* XAPI_GZZ */

  /* On Intuition, we cannot move over the screen border. */
  if (left < 0)
    left = 0;
  if (top < 0)
    top = 0;

  WD_LEFT (wd) = left;
  WD_TOP (wd) = top;

#if 1
  AWXA_WD_NOTE_CHANGE (wd, WDI_CH_POS);
#else

  if (WD_WIN_P (wd))
    {
      struct Window *win = WD_WIN (wd);
      local_precond (win);
      MoveWindow (win, left - win->LeftEdge, top - win->TopEdge);
    }
#endif
}

/* TODO: Gravitiy (needs to redefine display!) */
void
XResizeWindow (Display *dpy, Window wd, int width, int height)
{
  check_precond (width > 0);
  check_precond (height > 0);
  check_precond (!WD_BOOPSI_P (wd)); /* TODO-bw/03-Nov-97 */

  if (WD_MAINWIN_P (wd))
    DPY_GZZ_TO_REAL_WH (dpy, width, height);

  if (WD_WIDTH (wd) == width && WD_HEIGHT (wd) == height)
    return;

  WD_WIDTH (wd) = width;
  WD_HEIGHT (wd) = height;

#if 1
  AWXA_WD_NOTE_CHANGE (wd, WDI_CH_SIZE);
#else
  if (WD_WIN_P (wd))
    {
      struct Window *win = WD_WIN (wd);
      ChangeWindowBox (win, win->LeftEdge, win->TopEdge, width, height);
      win__note_resize (wd);
    }
#endif
}


/* window hints.  */
				/* ??? */
int
XGetNormalHints (Display *dpy, Window wd, XSizeHints *hints)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (hints);

  *hints = dpy->size_hints;

  hints->flags |= (PPosition | PSize);
  hints->x = WD_LEFT (wd);
  hints->y = WD_TOP (wd);

  hints->height = WD_HEIGHT (wd);
  hints->width = WD_WIDTH (wd);

  if (!(WD_MAINWIN_P (wd)))
    DPY_GZZ_TO_REAL_XY (dpy, hints->x, hints->y);
  else
    {
      struct Screen *screen = WD_SCREEN (WD_PARENT (wd));

      hints->flags |= (PMaxSize | PMinSize);
      hints->min_width = screen->WBorLeft + screen->WBorRight;
      hints->min_height = (SCREEN_W_BOR_TOP (screen) + SCREEN_W_BOR_BOTTOM (screen));
      /* this may sets it back to zero */
      DPY_REAL_TO_GZZ_WH (dpy, hints->min_width, hints->min_height);

      hints->max_width = WD_WIDTH (WD_PARENT (wd));
      hints->max_height = WD_HEIGHT (WD_PARENT (wd));
      DPY_REAL_TO_GZZ_WH (dpy, hints->max_width, hints->max_height);
      DPY_REAL_TO_GZZ_WH (dpy, hints->width, hints->height);
    }
  return 1;
}

				/* ??? */
void
XSetNormalHints (Display *dpy, Window wd, XSizeHints *hints)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (hints);
  {
    XSizeHints mh = *hints;	/* my local copy */

    if (WD_MAINWIN_P (wd))
      {
	DPY_GZZ_TO_REAL_WH (dpy, mh.min_width, mh.min_height);
	DPY_GZZ_TO_REAL_WH (dpy, mh.max_width, mh.max_height);
	DPY_GZZ_TO_REAL_WH (dpy, mh.width, mh.height);
      }
    else /* subwin */
      DPY_GZZ_TO_REAL_XY (dpy, mh.x, mh.y);

    if (WD_WIN_P (wd))
      {
	struct Window *win = WD_WIN (wd);
	if (hints->flags & (PMinSize | PMaxSize))
	  WindowLimits (win,
			(hints->flags & PMinSize) ? mh.min_width : 0,
			(hints->flags & PMinSize) ? mh.min_height : 0,
			(hints->flags & PMaxSize) ? mh.max_width : 0,
			(hints->flags & PMaxSize) ? mh.max_height : 0);
      }
    else if (WD_NWIN_P (wd))
      {
	struct ExtNewWindow *nw = WD_NWIN (wd);

	if (hints->flags & PMinSize)
	  {
	    nw->MinWidth = mh.min_width;
	    nw->MinHeight = mh.min_height;
	  }

	if (hints->flags & PMaxSize)
	  {
	    nw->MaxWidth = mh.max_width;
	    nw->MaxHeight = mh.max_height;
	  }

	if (hints->flags & PSize)
	  {
	    WD_WIDTH (wd) = mh.width;
	    WD_HEIGHT (wd) = mh.height;
	  }

	if (hints->flags & PPosition)
	  {
	    WD_LEFT (wd) = mh.x;
	    WD_TOP (wd) = mh.y;
	  }
      }
  }
  dpy->size_hints = *hints;
  DB_TRACE;
}

void
XSetWMHints (Display *dpy, Window wd, XWMHints *hints)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (hints);

  DB_TRACE;

  if (hints->flags & StateHint)
    WD_SET_ICONIFY (wd, hints->initial_state == IconicState);

#if 0
  if (hints->flags & InputHint)
    {
      long idcmp_mask;
      idcmp_mask = Awin_map_x_to_idcmp (hints->input);
      if (WD_WIN_P (wd))
	{
	  (void)ModifyIDCMP (WD_WIN (wd), idcmp_mask | WD_IDCMP_MAND (wd));
	}
      else if (WD_NWIN_P (wd))
	{
	  WD_NWIN (wd)->IDCMPFlags = idcmp_mask | WD_IDCMP_MAND (wd);
	}
    }
#endif  
}



void
XFreeGC (Display *dpy, GC gc)
{
  check_precond (dpy);
  check_precond (GX_VALID_P (gc) || GX_NIL_P (gc));

  if (GX_USED_P (gc))
    win__gx_free (gc);
}

void
XGetGCValues (Display *dpy, GC gc, int mask, XGCValues *xgcv) 
{
  check_precond (dpy);
  check_precond (GX_VALID_P (gc));
  check_precond ((~GX_MASK (gc) & mask) == 0);

  (GCForeground & mask) && (xgcv->foreground = GX_COLOR_A (gc));
  (GCBackground & mask) && (xgcv->background = GX_COLOR_B (gc));
  (GCFont & mask) && (xgcv->font = GX_FID (gc));
  (GCFunction & mask) && (xgcv->function = GX_DRAW_MODE (gc));
  (GCGraphicsExposures & mask) 
    && (xgcv->graphics_exposures = GX_EXPOSURES (gc));
  (GCFillStyle & mask) && (xgcv->fill_style = GX_FILL_STYLE (gc));
  (GCStipple & mask) && (xgcv->fill_style = GX_STIPPLE (gc));
  if (GCLineWidth & mask) abort ();
}

GC
XCreateGC (Display *dpy, Window wd, int mask, XGCValues *xgcv)
{
  GC gc, def;

  check_precond (dpy);
  check_precond (WD_USED_P (wd));
  check_precond (xgcv);


  gc = win__gx_alloc ();
  if (GX_NIL == gc)
    return GX_NIL;

  /* the following is the same like:
          XChangeGC (win__gc_clone (WD_GX (wd)), xgcv) */

  def = WD_GX (wd);

  GX_FID_SET (gc, (mask & GCFont) ? xgcv->font : GX_FID (def));
  GX_COLOR_A_SET (gc, (mask & GCForeground) ? xgcv->foreground : GX_COLOR_A (def));
  GX_COLOR_B_SET (gc, (mask & GCBackground) ? xgcv->background : GX_COLOR_B (def));
  GX_DRAW_MODE_SET (gc, (mask & GCFunction) ? xgcv->function : GX_DRAW_MODE (def));
  GX_EXPOSURES_SET (gc, (mask & GCGraphicsExposures) 
		    ? xgcv->graphics_exposures : GX_EXPOSURES (def));

  if (mask & GCStipple)
    {
      check_precond ((PIXMAP_WIDTH (xgcv->stipple) % 16) == 0);
      check_precond (PIXMAP_WIDTH (xgcv->stipple) == PIXMAP_HEIGHT (xgcv->stipple));

      GX_STIPPLE_SET (gc, xgcv->stipple);
    }

  GX_FILL_STYLE_SET (gc , (mask & GCFillStyle) ? xgcv->fill_style : 0);
  
  return gc;
}

void
XChangeGC (Display *dpy, GC gc, int mask,  XGCValues *xgcv)
{
  check_precond (xgcv);
  check_precond (GX_VALID_P(gc));
  check_precond (GX_USED_P(gc));

  (mask & GCFont) && GX_FID_SET (gc, xgcv->font);
  (mask & GCForeground) && GX_COLOR_A_SET (gc, xgcv->foreground);
  (mask & GCBackground) && GX_COLOR_B_SET (gc, xgcv->background);
  (mask & GCFunction) && GX_DRAW_MODE_SET (gc, xgcv->function);
  (mask & GCGraphicsExposures) && GX_EXPOSURES_SET (gc, xgcv->graphics_exposures);
  (mask & GCFillStyle) && GX_FILL_STYLE_SET (gc , xgcv->fill_style);

  if (mask & GCStipple)
    {
      check_precond ((PIXMAP_WIDTH (xgcv->stipple) % 16) == 0);
      check_precond (PIXMAP_WIDTH (xgcv->stipple) == PIXMAP_HEIGHT (xgcv->stipple));

      /* Is GC really the owner of stipple pixmap? (This is not true for
	 font and color.) -bw/14-Nov-97 */
      if (GX_MASK_STIPPLE (gc))
	XFreePixmap (dpy, GX_STIPPLE (gc));
      GX_STIPPLE_SET (gc, xgcv->stipple);
    }
}


void
XDrawRectangle (Display *dpy, Window wd, GC gc,
		int left, int top, int width, int height)
{
  int right, bottom;
  struct RastPort *rp;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (GX_VALID_P(gc) || GX_NIL_P (gc));
  check_precond (left >= 0 && top >= 0 && width > 0 && height > 0);

  rp = win__prepare (wd, gc, GCForeground | GCFunction, &left, &top);
  if (!rp)
    return;

  right = left + width - 1;
  bottom = top + height - 1;

  Move (rp, left, top);
  Draw (rp, right, top);
  Draw (rp, right, bottom);
  Draw (rp, left, bottom);
  Draw (rp, left, top);
}

void
XDrawLine (Display *dpy, Window wd, GC gc,
	   int x1, int y1, int x2, int y2)
{
  int right, bottom;
  struct RastPort *rp;
  int x_offs, y_offs;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (GX_VALID_P(gc) || GX_NIL_P (gc));

  /* draw outline */
  x_offs = y_offs = 0;
  rp = win__prepare (wd, gc, GCForeground | GCFunction, &x_offs, &y_offs);

  Move (rp, x1 + x_offs, y1 + y_offs);
  Draw (rp, x2 + x_offs, y2 + y_offs);
}

void
XFillRectangle (Display *dpy, Window wd, GC gc,
		int left, int top, int width, int height)
{
  struct RastPort *rp;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (WD_DRAWABLE_P (wd));
  check_precond (GX_VALID_P(gc) || GX_NIL_P (gc));
  check_precond (left >= 0 && top >= 0 && width > 0 && height > 0);

  rp = win__prepare (wd, gc, (GCForeground | GCFunction | GCFillStyle | GCStipple),
		     &left, &top);
  if (!rp)
    return;

  RectFill (rp, left, top, left + width - 1, top + height - 1);
}

/* ???-bw/25-Oct-97: XDrawString()==JAM1 and XDrawImageString()==JAM2 */
void
XDrawString (Display *dpy, Window wd, GC gc,
		  int left, int top, const char *buf, int len)
{
  struct RastPort *rp;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (GX_VALID_P(gc) || GX_NIL_P (gc));
  check_precond (left >= 0 && top >= 0);
  check_precond (buf);

  rp = win__prepare (wd, gc, (GCForeground | GCFont | GCFunction),
		     &left, &top);
  if (!rp)
    return;

  Move (rp, left, top);
  Text (rp, (char*)buf, len);
}

/* draw text with background */
void
XDrawImageString  (Display *dpy, Window wd, GC gc,
		   int left, int top, const char *buf, int len)
{
  struct RastPort *rp;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (GX_VALID_P(gc) || GX_NIL_P (gc));
  check_precond (left >= 0 && top >= 0);
  check_precond (buf);

  rp = win__prepare (wd, gc, GCForeground | GCBackground | GCFont | GCFunction,
		     &left, &top);
  if (!rp)
    return;

  Move (rp, left, top);
  Text (rp, (char*)buf, len);
}

Window
win__rport_wd_and_more (Window wd, int *left, int *top, 
			int *foreground, int *background)
{
  int i;			/* RESULT */
  int x, y, fg, bg;

  x = y = 0;
#define COLOR_NIL (COLOR_START-1)
  fg = bg = COLOR_NIL;

  for (i = wd; WD_NIL != i && !WD_WIN_P (i); i = WD_PARENT (i))
    {
      x += WD_LEFT (i);
      y += WD_TOP (i);
      if (WD_GX(i) != GX_NIL)
	{
	  if (fg == COLOR_NIL)
	    fg = WD_COLOR_A (i);
	  if (bg == COLOR_NIL)
	    bg = WD_COLOR_B (i);
	}
    }

  left && (*left = x);
  top && (*top = y);
  foreground && (*foreground = fg);
  background && (*background = bg);

  return i;
}

struct Region *
win_clip_subwindow (Window wd_rport, Window wd_child)
{
  int i;
  struct Rectangle rect;
  struct Region *reg_old, *reg_tmp;
  int errors;

  local_precond (WD_VALID_P (wd_rport) && WD_WIN_P (wd_rport));
  local_precond (WD_VALID_P (wd_child) 
		 && (WD_BOOPSI_P (wd_child) || WD_SUBWIN_P (wd_child)));

  /* overlapping areas of subwindows are treat as nobody's land */
  errors = 0;
  reg_old = NewRegion ();
  reg_tmp = NewRegion ();
  if (!(reg_tmp && reg_old))
    {
      reg_old && (DisposeRegion (reg_old),1);
      reg_tmp && (DisposeRegion (reg_tmp),1);
      return 0;
    }

  WD_CHILD_FOREACH (wd_rport, i)
    if (i != wd_child && (WD_BOOPSI_P (i) || WD_SUBWIN_P (i)))
      {
	WD_RECTANGLE (i, &rect);
	errors += !OrRectRegion (reg_tmp, &rect);
      }

  WD_IRECTANGLE (wd_rport, &rect);
  errors += !OrRectRegion (reg_old, &rect);
  errors += !XorRegionRegion (reg_tmp, reg_old);
  DisposeRegion (reg_tmp);

  if (errors)
    {
      DisposeRegion (reg_old);
      return 0;
    }

  return reg_old;
}

/* The last bolean parameter seems to ask for "expose" events.  I
   really should install the X man pages -bw/14-Nov-97 */
void
XClearArea (Display *dpy, Window wd,
	    int left, int top, int width, int height, bool notify)
{
  struct RastPort *rp;
  int right, bottom;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
  check_precond (left >= 0 && top >= 0 && width >= 0 && height >= 0);

  if (!WD_MAPPED_P (wd))
    return;

  width || (width = WD_WIDTH (wd) - left);
  height || (height = WD_HEIGHT (wd) - top);

  rp = win__prepare (wd, 0, GCBackground, &left, &top);

  if (!rp)
    return;

  right = left + width - 1;
  bottom = top + height - 1;
  RectFill (rp, left, top, right, bottom);
  /* XXX-bw/01-Jan-98: Is this used to main windows, then do both renaming
     subwin to win and handle "iwins" in process_idcmp() */
  if (notify)
    {
      int i, main_wd;

      main_wd = WD_SUBWIN_P (wd) ? WD_PARENT (wd) : wd;

      if (WD_MAPPED_P (main_wd))
	WD_CHILD_FOREACH (main_wd, i)
	  if (!WD_MAPPED_P (i)
	      /* test of non overlapping */
	      || WD_RIGHT (i) < left
	      || WD_LEFT (i) > right
	      || WD_BOTTOM (i) < top
	      || WD_TOP (i) > bottom)
	    continue;
	  else
	    win_subwin_mp_push (i, WD_SUBWIN_EVENT_EXPOSE, 0);
      {
	struct IBox box = { left, top, width , height };
	win_subwin_mp_push (main_wd, WD_WIN_EVENT_EXPOSE, &box);
      }
    }
}

void
XClearWindow (Display *dpy, Window wd)
{
  XClearArea (dpy, wd, 0, 0, 0, 0, false);
}

void
XCopyArea (Display *dpy, Window wd_src, Window wd_dst, GC gc,
	   int left, int top, int width, int height, int to_left, int to_top)
{
  struct RastPort *rp_src, *rp_dst;

  check_precond (dpy);
  check_precond (WD_VALID_P (wd_src) && WD_WIN_P (wd_src));
  check_precond (WD_VALID_P (wd_dst) && WD_WIN_P (wd_dst));
  check_precond (GX_VALID_P(gc) && GX_USED_P(gc));
  check_precond (width > 0 && height > 0 
		 && left >= 0 && top >= 0 && to_left >=0 && to_top >= 0);

  rp_src = win__prepare (wd_src, gc, 0, &left, &top);
  rp_dst = win__prepare (wd_dst, gc, 0, &to_left, &to_top);

  if (! (rp_src && rp_dst))
    return;

  ClipBlit (rp_src, left, top,
	    rp_dst, to_left, to_top,
	    width, height, (0x80 | 0x40));
}


/* GC */
void
XSetFont (Display *dpy, GC gc, fid_type fid)
{
  check_precond (dpy);
  check_precond (GX_VALID_P (gc) && GX_USED_P (gc));
  check_precond (fid);

  GX_FID_SET (gc, fid);
}

void
XSetStipple (Display *dpy, GC gc, Pixmap stipple)
{
  check_precond (dpy);
  check_precond (GX_VALID_P (gc) && GX_USED_P (gc));
  check_precond (stipple);
 
  GX_STIPPLE_SET (gc, stipple);
}

void
XSetFillStyle (Display *dpy, GC gc, int fill_style)
{
  check_precond (dpy);
  check_precond (GX_VALID_P (gc) && GX_USED_P (gc));

  GX_FILL_STYLE_SET (gc, fill_style);
}


/* Fonts */
static bool win__font_parse (const char *xfont, struct TextAttr *result);
#define FONT_YSIZE_DEF (13)

char **
XListFonts (Display *dpy, char *pattern, int max_names, int *actual_names)
{
  char name[256];
  char **result;
  int actual_table_sz;
  struct TextAttr ta;

  check_precond (dpy);
  check_precond (pattern);
  check_precond (max_names > 0);
  check_precond (actual_names);

  *actual_names = 0;
  if (!win__font_parse (pattern, &ta))
    return 0;

  actual_table_sz = 2;
  result = malloc (sizeof (char *[1]) * (actual_table_sz + 1));

  if (!result)
    {
      return 0;
    }


  /* TODO-bw/28-Oct-97 */

  strrchr (ta.ta_Name, '.') && (*strrchr (ta.ta_Name, '.') = '\0'); 
  /* cut off ".font" suffix  */

  sprintf (name, "-%s-%s-%s-%c-%s-%s-%u-120-75-75-c-60-iso8859-1",
	   "misc", /* "misc", "adobe", "sony", ... */
	   ta.ta_Name,
	   (ta.ta_Style & FSF_BOLD) ? "bold" : "medium",
	   (ta.ta_Style & FSF_ITALIC) ? 'i' : 'r',
	   "semicondensed", /* "normal" */
	   "", /* "",  "sans" */
	   ta.ta_YSize ? ta.ta_YSize : FONT_YSIZE_DEF);

  STRING_FREE (ta.ta_Name);
	   
  result [(*actual_names)++] = STRING_CLONE (name);
  result [*actual_names] = 0;

  return result;
}

/* Frees the raw storage of INFO and its contents; Does the same for NAMES  */
void
XFreeFontInfo (char **names, XFontStruct *info, int nmb)
{
  int i;
  for (i=0; i < nmb; ++i)
    {
      STRING_FREE (info[i].attrs.ta_Name);
      STRING_FREE (names[i]);
    }
  free (names);
  free (info);
}

/* Free result of XListFonts */
/* Free the storage and the contents of null terminated NAMES */
void
XFreeFontNames (char **names)
{
  int i;
  if (!names)
    return;

  for (i=0; names[i]; ++i)
    STRING_FREE (names[i]);
  free (names);
}

char **
XListFontsWithInfo (Display *dpy, char *pattern, int max_names,
			   int *actual_names, XFontStruct **actual_info)
{
  char **names;			/*  RESULT */
  int i;

  check_precond (dpy);
  check_precond (pattern);
  check_precond (max_names > 0);
  check_precond (actual_names);
  check_precond (actual_info);	/* ??? */

  names = XListFonts (dpy, pattern, max_names, actual_names);
  if (! *actual_names)
    return 0;

  *actual_info = malloc (sizeof actual_info[0] * (1 + *actual_names));

  if (! *actual_info)
    {
      XFreeFontNames (names);
      *actual_names = 0;
      return 0;
    }

  for (i=0; i < *actual_names; ++i)
    {
      actual_info [i] = XLoadQueryFont (dpy, names[i]);
    }

  return names;
}

/* Give the resource in FONT back to the system.  */
void 
XFreeFont (Display *dpy, XFontStruct *font)
{
  check_precond (dpy);
  check_precond (font);
  check_precond (FID_VALID_P (font->fid) || FID_NIL_P (font->fid));

  if (FID_USED_P (font->fid))
    FID_CLOSE (font->fid);

  STRING_FREE (font->attrs.ta_Name);

  free (font);
}

enum  
{
  XFF_FOUNDRY,
  XFF_FAMILY_NAME,
  XFF_WEIGHT_NAME,
  XFF_SLANT,
  XFF_SETWIDTH_NAME,
  XFF_ADD_STYLE_NAME,
  XFF_PIXEL_SIZE,
  XFF_POINT_SIZE,
  XFF_RESOLUTION_X,
  XFF_RESOLUTION_Y,
  XFF_SPACING,
  XFF_AVERAGE_WIDTH,
  XFF_CHARSET_REGISTRY,
  XFF_CHARSET_ENCODING
};

/* TODO-bw/10-Nov-97: use system preferences (IFF files in ENV:)*/
static struct TextAttr win_xxx_fattr_fixed = {"Courier.font", 13, FS_NORMAL, 0};
#define DEFAULT_FONT_ATTR_FIXED(dpy) (&win_xxx_fattr_fixed)

static bool
win__font_parse (const char *xfont, struct TextAttr *result)
{
  char *name_cpy;
  char *sp, *ep;
  int field_nmb;

  local_precond (xfont && result);

  bzero (result, sizeof *result);

  if (xfont[0] != '-')
    return 0;			/* name have to start with a minus sign */

  name_cpy = strcpy (alloca (strlen (xfont) + 1), xfont);

  field_nmb = 0;
  for (sp = &name_cpy[0]; sp; (sp=ep), ++field_nmb)
    {
      ++sp; 
      if ((ep = strchr (sp, '-')))
	*ep = '\0';

      if (!*sp)			/* empty fields are currently not asked by any case */
	continue;

      switch (field_nmb)
	{
	case XFF_FAMILY_NAME:  /* "fixed", "topaz", "courier", ... */
	  if (0==strcasecmp (sp, "fixed")) /* TODO--bw/04-Nov-97 */
	    result->ta_Name = STRING_CLONE (DEFAULT_FONT_ATTR_FIXED (dpy)->ta_Name);
	  else
	    {
	      char name [strlen (sp) + sizeof ".font"];

	      strcpy (name, sp);
	      strcat (name, ".font");
	      result->ta_Name = STRING_CLONE (name);
	    }
	  break;
	case XFF_WEIGHT_NAME:
	  if (0==strcasecmp (sp, "bold"))
	    result->ta_Style |= FSF_BOLD;
	  break;
	case XFF_SLANT:
	  if (0==strcasecmp (sp, "i")
	      || 0==(strcasecmp (sp, "o"))) /* ???-bw/10-Nov-97 */
	    result->ta_Style |= FSF_ITALIC;
	  break;
	case XFF_PIXEL_SIZE:
	  result->ta_YSize = atoi (sp);
	  break;
	}
    }

  return true;
}  


XFontStruct *
XLoadQueryFont (Display *dpy, const char *name)
{
  XFontStruct *xfont;		/* RESULT */

  check_precond (dpy);
  check_precond (name);

  xfont = calloc (1, sizeof *xfont);
  xfont->attrs = *DEFAULT_FONT_ATTR_FIXED (dpy);

  if (!win__font_parse (name, &xfont->attrs))
    {
      free (xfont);
      return 0;
    }

  if (getenv ("EMACS_DEV"))
    fprintf (stderr, "####-%s: fontname <%s> ####\n", __PRETTY_FUNCTION__, name);

  xfont->fid = FID_OPEN (FONT_TATTR (xfont));
  if (FID_NIL_P (xfont->fid))
    {
      free (xfont);
      return 0;
    }

  xfont->descent  /* ???-bw/02-Nov-97: need for underline */
    = FID_FONT (xfont->fid)->tf_YSize - FID_FONT (xfont->fid)->tf_Baseline;
  xfont->ascent = FID_FONT (xfont->fid)->tf_YSize - xfont->descent; /* ??? */

  /* ???-bw/15-Dec-97: In 1-byte-fonts the byte2 seems to be the used byte. */
  xfont->max_byte1 = xfont->min_byte1 = 0; /* means 1-byte font */
  xfont->min_char_or_byte2 = FID_FONT (xfont->fid)->tf_LoChar; /* ??? */
  xfont->max_char_or_byte2 = FID_FONT (xfont->fid)->tf_HiChar; /* ??? */

  xfont->max_bounds.ascent = xfont->ascent;
  xfont->max_bounds.descent = xfont->descent;
  xfont->max_bounds.width = FID_FONT (xfont->fid)->tf_XSize;
  xfont->max_bounds.height = FID_FONT (xfont->fid)->tf_YSize;

  xfont->min_bounds.ascent = xfont->ascent;
  xfont->min_bounds.descent = xfont->descent;
  xfont->min_bounds.width = FID_FONT (xfont->fid)->tf_XSize;
  xfont->min_bounds.height = FID_FONT (xfont->fid)->tf_YSize;

  return xfont;
}


/* Translate window coordinates to ancestor window coordinates and
   find the child window of W2 containing the coordinates*/
void
XTranslateCoordinates (Display *dpy, 
		       Window w1, /* from window */
		       Window w2, /* to window */
		       int w1_x, int w1_y,	/* given w1 coords */
		       int *w2_x, int *w2_y, /* returned w2 coords */
		       Window *child) /* returns the child of w2 containing the coords */
{
  int i;
  int w2_abs_left, w2_abs_top;
  
  check_precond (dpy);
  check_precond (WD_VALID_P (w1));
  check_precond (WD_VALID_P (w2));

  /* points given for main windows are relative to the inner
     rectangle and need to be corrected. */
  if (WD_MAINWIN_P (w1))
    DPY_GZZ_TO_REAL_XY (dpy, w1_x, w1_y);
  if (WD_MAINWIN_P (w2))
    DPY_GZZ_TO_REAL_XY (dpy, w2_x, w2_y);

  /* make absolute (screen relative) coords */
  for (i = w1; WD_VALID_P(i); i = WD_PARENT (i))
    {
      w1_x += WD_LEFT (i);
      w1_y += WD_TOP (i);
    }

  /* get absolute coords of {0,0} in w2 */
  for (i = w2, w2_abs_left = 0, w2_abs_top = 0; WD_VALID_P(i); i = WD_PARENT (i))
    {
      w2_abs_left += WD_LEFT (i);
      w2_abs_top += WD_TOP (i);
    }

  /* ...w1_x and w1_y are now absolute and we have the absolute coords of
     w2 */

  *w2_x = w1_x - w2_abs_left;
  *w2_y = w1_y - w2_abs_top;

  /* find the righteous child of w2 */
  if (child)
    {
      *child = None;
      WD_CHILD_FOREACH (w2, i)
	if (/* test point for being outside */
	    *w2_x < WD_LEFT (w2)
	    || *w2_x > WD_RIGHT (w2)
	    || *w2_y < WD_TOP (w2)
	    || *w2_y > WD_BOTTOM (w2))
	  continue;
	else
	  {
	    *child = i;
	    break;
	  }
    }
}


#include <fcntl.h>


static int win_connection_pipe[2];

/* Provide a dummy input descriptor */

int
ConnectionNumber (Display *dpy)
{
  if (win_connection_pipe [0] || win_connection_pipe [1])
    return win_connection_pipe [0];

  if (pipe (win_connection_pipe) < 0)
    return -1;

  return win_connection_pipe [0];
}

long
Awin_map_x_to_idcmp (long x_input_mask)
{
  long idcmp_mask = 0;
  struct { long xi, idcmp_set, idcmp_unset; } map[] 
    = {
      /* ???-TODO-bw/30-Oct-97: Which one is IDCMP_IDCMPUPDATE? */
	{ KeyPressMask, (IDCMP_RAWKEY /* | IDCMP_VANILLAKEY */) },
	{ ExposureMask, 0 },
	{ ButtonPressMask, IDCMP_MOUSEBUTTONS }, /* ?! GADGETDOWN*/
	{ ButtonReleaseMask, IDCMP_MOUSEBUTTONS }, /* ?! GADGETUP (WFLG_REPORTMOUSE) */
	{ PointerMotionMask, IDCMP_MOUSEMOVE },	/* DELTAMOVE? */
	{ PointerMotionHintMask, IDCMP_MOUSEMOVE }, /* ???? */
	{ StructureNotifyMask, IDCMP_CHANGEWINDOW }, /* ??? */
	{ FocusChangeMask, IDCMP_NEWSIZE },
	{ LeaveWindowMask, IDCMP_INACTIVEWINDOW },
	{ EnterWindowMask, IDCMP_ACTIVEWINDOW },
	{ VisibilityChangeMask, 0}, /* WA_Depth..? */
	{ ButtonMotionMask, 0 }, /* ??? */
      };
  int i;

  for (i=0; i < (sizeof map / sizeof map[0]); ++i)
    if (map[i].xi & x_input_mask)
      idcmp_mask |= map[i].idcmp_set;

  for (i=0; i < (sizeof map / sizeof map[0]); ++i)
    if (map[i].xi & x_input_mask)
      idcmp_mask &= ~(map[i].idcmp_unset);

  return idcmp_mask;
}



/* set color int gc ???-bw/25-Oct-97 */
bool
XSetForeground (Display *dpy, GC gc, unsigned color)
{
  check_precond (dpy);
  check_precond (GX_VALID_P (gc) && GX_USED_P (gc));

  GX_COLOR_A_SET (gc, color);
  return true;
}

/* set color int gc ???-bw/25-Oct-97 */
bool
XSetBackground (Display *dpy, GC gc, unsigned color)
{
  check_precond (dpy);
  check_precond (GX_VALID_P (gc) && GX_USED_P (gc));

  GX_COLOR_B_SET (gc, color);
  return true;
}

bool
XSetWindowBackground (Display *dpy, Window wd, unsigned color)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd) && WD_USED_P (wd));

  GX_COLOR_B_SET (WD_GX (wd), color);
  return true;
}

bool
XSetWindowBorder (Display *dpy, Window wd, unsigned color)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd) && WD_USED_P (wd));

  return false;
}

void
XBell (Display *dpy, int xxx)
{
  check_precond (dpy);

  DisplayBeep (0);
}


/* Pixmap */
Pixmap /* struct Image * */
XCreateBitmapFromData (Display *dpy, Window wd,
		       unsigned char *bits, int width, int height)
{
  struct Image *img;

  check_precond (dpy && WD_VALID_P (wd) && bits && width > 0 && height > 0);

  if (img = calloc (sizeof *img, 1))
    {
      img->Width = width;
      img->Height = height;
      img->Depth = 1;
      img->PlanePick = (1<<img->Depth) - 1;
      if (img->ImageData = (void*)win___bitplane_x_to_intui (bits, width, height))
	{
	  return img;
	  FreeVec (img->ImageData);
	}
      free (img);
    }
  return 0;
}

void
XFreePixmap (Display *dpy, Pixmap pm)
{
  check_precond (dpy);

  if (pm)
    {
      FreeVec (pm->ImageData);
      free (pm);
    }
}

void
XFree (void *storage)
{
  check_precond (storage);

  free (storage);
}


/* unimplemented/impossible */
#define FAIL_NOT_IMPLEMENTED() \
((fputs) ("unimplemented function called: " __PRETTY_FUNCTION__ "\n", stderr), abort(), 0)

#define WARN_NOT_IMPLEMENTED() \
((fputs) ("unimplemented function called: " __PRETTY_FUNCTION__ "\n", stderr))

void
XSetWindowBorderPixmap (Display *dpy, Window wd, /* Pixmap */ void *pm)
{
  check_precond (dpy);
  check_precond (WD_VALID_P (wd));
//  check_precond (pm);
  /* do nothing */
}

void
XrmDestroyDatabase (XrmDatabase db)
{
#if 1
  if (!db)
    return;
#else
  check_precond (db);
#endif
  /* do nothing */
}

void
XDrawString16 (Display *dpy, Window wd, GC gc,
		  int left, int top, const XChar2b *buf, int len)
{
  char *cbuf;
  int i;

  check_precond (buf); /* && XDrawString()'s preconditons */

  cbuf = alloca (len);
  for (i=0; i < len; ++i)
   cbuf[i] = buf[i].byte2;

  XDrawString (dpy, wd, gc, left, top, cbuf, len);
}

void
XDrawImageString16 (Display *dpy, Window wd, GC gc,
		    int left, int top, const XChar2b *buf, int len)
{
  char *cbuf;
  int i;

  check_precond (buf); /* && XDrawImageString()'s preconditons */

  cbuf = alloca (len);
  for (i=0; i < len; ++i)
   cbuf[i] = buf[i].byte2;

  XDrawImageString (dpy, wd, gc, left, top, cbuf, len);
}

void
XSetClipMask (Display *dpy, GC gc, int mask)
{
#if 0
  if (mask == None)
    {
      GX_MASK(gc_)&=~GCRegion;
      return;
    }
#endif
  WARN_NOT_IMPLEMENTED();
}

void
XSetClipOrigin (Display *dpy, GC gc, int x, int y)
{
#if 0
  check_precond (dpy, GX_VALID_P (gc));

  GX_OBJ (gc).xorig = x;
  GX_OBJ (gc).yorig = y;
#endif
  WARN_NOT_IMPLEMENTED();
}

/* The following functions are not very effective.  Would be faster,
   and even cleaner, to change the caller to use Amiga clipping
   functions directly. -bw/18-Mar-98 */
void
XDestroyRegion (Region region)
{
  DisposeRegion (region);
}

Region
XPolygonRegion (const XPoint *points, int nmb, int rule)
{
  if (rule == EvenOddRule && (nmb == 4)
#ifdef FULLDEBUG
      && points[0].x == points[3].x
      && points[1].x == points[2].x
      && points[0].y == points[1].y
      && points[2].y == points[3].y
#endif /* FULLDEBUG */
      )
    {
      struct Region *reg; /* RESULT */
      struct Rectangle rect;

      rect.MinX = min (points[0].x, points[1].x);
      rect.MinX = min (points[0].y, points[2].y);
      rect.MaxX = max (points[0].x, points[1].x);
      rect.MaxX = max (points[0].y, points[2].y);

      reg = NewRegion ();
      if (!reg)
	Aerr_fail ("Out of memory: cannot allocate clip region");

      AndRectRegion (reg, &rect);
      return reg;
    }

  assert (!"implemented");
  abort ();
}

void
XSetRegion (Display *dpy, GC gc, Region region)
{
  check_precond (dpy);
  check_precond (GX_VALID_P (gc));
  check_precond (region);

#if 0
  GX_REGION_SET (gc, region);
#endif
  WARN_NOT_IMPLEMENTED();
}

Display *	
XOpenDisplay (const char *name)
{
  Display *dpy;			/* RESULT */
  Window swd;			/* screen wd */
  /* -- */
  const char *screen_name = 0;
  bool custom_screen = false;

  dpy = calloc (1, sizeof *dpy);
  if (!dpy)
    goto fail;

  /* Clear tag array */
  win__init_pref ();

  if (*name == '\0' || *name == ':') /* Default public screen */
    /* default public screen */
    {
      screen_name = 0;
    }
  else
    {
      /* TODO-bw/10-Apr-98: Currently this does not open a custom
         screen, but try to lock an existing named public screen.  */
      screen_name = name;
    }

  if (!custom_screen)
    win_new_win.Title = (STRPTR)screen_name;
  else
    assert (!"implemented");

  dpy->screen = win__create (WT_ISCREEN);
  if (dpy->screen == WD_NIL)
    goto fail;

  if (COLOR_WHITE != win__color_open (WD_COLCON (dpy->screen), 65280, 65280, 65280, 16)
      || COLOR_BLACK != win__color_open (WD_COLCON (dpy->screen), 0, 0, 0, 16))
    goto fail;

  return dpy;

 fail:
  if (dpy)
    {
      if (dpy->screen != WD_NIL)
	win__destroy (dpy->screen);
      free (dpy);
    }
  return 0;
}

void
XCloseDisplay (Display *dpy)
{
  check_precond (dpy);
  win__destroy (dpy->screen);
  free (dpy);
}

void
XSetCloseDownMode (Display *dpy, int mode)
{
  check_precond (dpy);
}

int
XParseGeometry (const char *s, int *x, int *y, unsigned *width, unsigned *height)
{
  int i, n, item, val;
  int result;
  size_t len;

  check_precond (s && x && y && width && height);

  len = strlen (s);
  result = 0;

  for (item = n = i = 0; item < len; (item += n+1), (++i))
    {
      n = strspn (s + item, "0123456789");
      if (!n)
	continue;
      val = -42;
      sscanf (&s[item], "%d", &val);

      switch (i)
	{
	case 0:
	  result |= WidthValue;
	  *width = val;
	  break;
	case 1:
	  result |= HeightValue;
	  *height = val;
	  break;
	case 2:
	  if (s[item - 1] == '-')
	    (*x = -val), (result |= (XValue | XNegative));
	  else
	    (*x = val), (result |= XValue);
	  break;
	case 3:
	  if (s[item - 1] == '-')
	    (*y = -val), (result |= (YValue | YNegative));
	  else
	    (*y = val), (result |= YValue);
	  break;
	}
    }

  return result;
} 

/* Status */ int
XGetGeometry(display, d, root_return, x_return,
	     y_return, width_return,
	     height_return, border_width_return,
	     depth_return)
     Display *display;
     Drawable d;
     Window *root_return;
     int *x_return, *y_return;
     unsigned int *width_return, *height_return;
     unsigned int *border_width_return;
     unsigned int *depth_return;
{
  struct RastPort *rp;

  check_precond (display);
  check_precond (WD_DRAWABLE_P (d));
  check_precond (root_return && x_return && y_return && width_return && height_return
		 && border_width_return && depth_return);
  *x_return = WD_LEFT (d);
  *y_return = WD_TOP (d);
  *width_return = WD_WIDTH (d);
  *height_return = WD_HEIGHT (d);
  *border_width_return = (WD_WIN_P (d) ? WD_WIN (d)->BorderLeft : 0);

  *depth_return = 0;
  for (*root_return = d; (*root_return = WD_PARENT (*root_return)) != WD_NIL; )
    if (WD_SCREEN_P (*root_return))
      {
	*depth_return = WD_SCREEN (*root_return)->RastPort.BitMap->Depth;
	break;
      }
  return (*depth_return) != 0;
}
