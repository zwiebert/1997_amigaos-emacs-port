#ifndef H_EMACS_AMIGA_WINDOW_DEFS_H_
#define H_EMACS_AMIGA_WINDOW_DEFS_H_

/* GZZ<=>real */
#ifndef XAPI_GZZ
#define WIN_GZZ_WIDTH(W) ((W)->Width + 0)
#define WIN_GZZ_HEIGHT(W) ((W)->Height + 0)
#define WIN_GZZ_LEFT(W) ((W)->LeftEdge)
#define WIN_GZZ_TOP(W) ((W)->TopEdge)
#define DPY_GZZ_TO_REAL_XY(DPY, X, Y)
#define DPY_GZZ_TO_REAL_WH(DPY, W, H)
#define DPY_REAL_TO_GZZ_XY(DPY, X, Y)
#define DPY_REAL_TO_GZZ_WH(DPY, W, H)
#define WIN_REAL_TO_GZZ_XY(W, X, Y)
#define WIN_REAL_TO_GZZ_WH(W, WIDTH, H)

#define IM_GZZ_MOUSE_X(IM) ((IM)->MouseX)
#define IM_GZZ_MOUSE_Y(IM) ((IM)->MouseY)
#define WIN_GZZ_MOUSE_X(W) ((W)->MouseX)
#define WIN_GZZ_MOUSE_Y(W) ((W)->MouseY)

#else /* XAPI_GZZ */

#define IM_GZZ_MOUSE_X(IM) ((IM)->MouseX - (IM)->IDCMPWindow->BorderLeft)
#define IM_GZZ_MOUSE_Y(IM) ((IM)->MouseY - (IM)->IDCMPWindow->BorderTop)
#define WIN_GZZ_MOUSE_X(W) ((W)->GZZMouseX)
#define WIN_GZZ_MOUSE_Y(W) ((W)->GZZMouseY)

#define WIN_GZZ_WIDTH(W) ((W)->GZZWidth + 0)
#define WIN_GZZ_HEIGHT(W) ((W)->GZZHeight + 0)
#define WIN_GZZ_LEFT(W) ((W)->LeftEdge + (W)->BorderLeft)
#define WIN_GZZ_TOP(W) ((W)->TopEdge + (W)->BorderTop)

#define SCREEN_W_BOR_TOP(S) ((S)->WBorTop + 1 + (S)->Font->ta_YSize)
#define SCREEN_W_BOR_BOTTOM(S) ((S)->WBorBottom + 14)

#define DPY_GZZ_TO_REAL_X(DPY, X) \
(void)(X += (WD_SCREEN ((DPY)->screen)->WBorLeft))
#define DPY_GZZ_TO_REAL_Y(DPY, Y) \
     (void)(Y += SCREEN_W_BOR_TOP (WD_SCREEN ((DPY)->screen)))
#define DPY_GZZ_TO_REAL_XY(DPY, X, Y)					\
     (void)((X += (WD_SCREEN ((DPY)->screen)->WBorLeft)),		\
	    (Y += SCREEN_W_BOR_TOP (WD_SCREEN ((DPY)->screen))))
#define DPY_GZZ_TO_REAL_W(DPY, W)				\
  (void)(W += (WD_SCREEN ((DPY)->screen)->WBorLeft		\
		+ WD_SCREEN ((DPY)->screen)->WBorRight))
#define DPY_GZZ_TO_REAL_H(DPY, H)					\
     (void) (H += (SCREEN_W_BOR_TOP (WD_SCREEN ((DPY)->screen))		\
		   + SCREEN_W_BOR_BOTTOM (WD_SCREEN ((DPY)->screen))))
#define DPY_GZZ_TO_REAL_WH(DPY, W, H) \
  (DPY_GZZ_TO_REAL_W ((DPY), (W)), DPY_GZZ_TO_REAL_H ((DPY), (H)))
#define DPY_REAL_TO_GZZ_XY(DPY, X, Y)				\
  (void)((X -= (WD_SCREEN ((DPY)->screen)->WBorLeft)),		\
	 (Y -= SCREEN_W_BOR_TOP (WD_SCREEN ((DPY)->screen))))
#define DPY_REAL_TO_GZZ_WH(DPY, W, H)					\
  (void)((W -= (WD_SCREEN ((DPY)->screen)->WBorLeft 			\
		+ WD_SCREEN ((DPY)->screen)->WBorRight)),		\
	 (H -= (SCREEN_W_BOR_TOP (WD_SCREEN ((DPY)->screen))		\
		+ SCREEN_W_BOR_BOTTOM (WD_SCREEN ((DPY)->screen)))))

#define WIN_REAL_TO_GZZ_XY(W, X, Y)			\
(void)((X -= (W)->BorderLeft), (Y -= (W)->BorderTop))
#define WIN_REAL_TO_GZZ_WH(WIN, W, H)			\
  (void)((W -= (W)->BorderLeft + (W)->BorderRight),	\
	 (H -= (W)->BorderTop + (W)->BorderBottom))
#endif /* XAPI_GZZ */

     /* non GZZ */
#define IM_MOUSE_X(IM) ((IM)->MouseX)
#define IM_MOUSE_Y(IM) ((IM)->MouseY)
#define WIN_MOUSE_X(W) ((W)->MouseX)
#define WIN_MOUSE_Y(W) ((W)->MouseY)



#define OBJ_OBTAIN(PTR) (((PTR)=calloc (1, sizeof *(PTR)))!=0)
#if 0
#define OBJ_OBTAIN_SHARED \
(ptr) (((ptr)=AllocMem (sizeof *(ptr), MEMF_CLEAR | MEMF_PUBLIC))!=0)
#endif
#define OBJ_ZERO(PTR) (bzero ((PTR), sizeof (*PTR)))
#define OBJ_DISPOSE(PTR) ((void)(free(PTR), ((PTR)=0)))
#define OBJ_CLONE(NEW,OLD) \
(assert (OLD), \
 ((OBJ_OBTAIN(NEW)) ? bcopy((OLD), (NEW), sizeof *(NEW)),0 : 0), (NEW))

#define STRING_FREE(s) ((s) ? (xfree (s),((s)=0),1) : 0)
#define STRING_CLONE(src) ((src) ? (strcpy ((void*)xmalloc (strlen (src)+1), (src))) : 0)


/* color mapping */

typedef struct win_color_context *COLOR_CONTEXT;
/* prototpyes have nothing to do in a _defs.h file -bw/05-Aug-98 */
#if 0
/* exported interface */
COLOR_CONTEXT win_color_context_create (struct Screen *screen);
void win_color_context_destroy (COLOR_CONTEXT obj);
bool AITcolor_alloc (Display *dpy, XColor *color);
int AITcolor_free (Display *dpy, int col);
bool AITcolor_query (const char *name, XColor *color);
/* Acces methods */
int win_color_pen (COLOR_CONTEXT obj, unsigned pixel);
#endif
/* We assume that the first opened color returns COLOR_START, and the
next distinct open-color returns COLOR_START+1 as long we haven't
closed a previous openend color.  This is used to alloc the fixed
colors in Awin_init_module(). -bw/06-Apr-98 */

#define COLOR_START (0)

#define COLOR_NIL_P(c) ((c)==COLOR_NIL)
#define COLOR_VALID_P(c) you lose
#define COLOR_PEN(c) you lose
#define COLOR_USED_P(c) you lose
/* compare 2 colors. evaluates to 0 (!) for a match */
#define COLOR_CMP(a, b) you lose

/* some screen independent fixed colors */
#define COLOR_BLACK (1)
#define COLOR_WHITE (0)

/* end of color mapping */


/* fonts */
#define FONT_MAX (100)
extern struct TextFont *win_font_tbl[];
extern unsigned char win_font_syle_tbl[];
#define FID_VALID_P(fid) (0<(fid) && (fid) <= FONT_MAX)
#define FID_USED_P(fid) (win_font_tbl[(fid)])
#define FID_NIL_P(fid) ((fid)==0)
#define FID_FONT(fid) (win_font_tbl[(fid)] + 0)
#define FID_FONT_SET(fid, font, style) ((win_font_tbl[(fid)] = (font)), \
 (win_font_style_tbl[(fid)] = (style)))
#define FID_STYLE(fid) (win_font_style_tbl[(fid)] + 0)
#define FID_CLOSE(fid) \
(CloseFont (win_font_tbl[(fid)]), win_font_tbl[(fid)] = 0)
#define FID_OPEN(tattr)							\
({ void *font; int fid=0, i;						\
 if ((font=OpenDiskFont (tattr)))					\
{									\
 for (i=0; i < FONT_MAX; ++i)						\
  if (!win_font_tbl[i])							\
    {									\
      fid = i;								\
      FID_FONT_SET(fid, font, tattr->ta_Style);				\
      break;								\
    }									\
 if (FID_NIL_P (fid))							\
  CloseFont (font);							\
} assert (FID_VALID_P(fid) || FID_NIL_P(fid)); fid; })


/* windows */
extern int win_desc_greatest; /* the greatest in use wd */

#define WD_PTR(wd_) ((void*)(win_desc_tbl[wd_]))
#define WD_PTR_SET(wd_, val_) ((void)(win_desc_tbl[(wd_)] = (val_)))
/* This version of WD_PTR() isn't allowed to return NULL. It may be
   checked in debug code. */
#define WD_PTR2(wd_) WD_PTR(wd_)
#define WD_SET_PTR(wd_,ptr) ((win_desc_tbl[wd_])=(ptr))
#define WD_SET_FREE(wd_) (WD_SET_PTR(wd_, 0), WD_SET_TYPE(wd_, WT_FREE))
#define WD_INFO(wd_) (&win_info_tbl[(wd_)])
#define WD_TYPE(wd_) (WD_INFO(wd_)->type)
#define WD_SET_TYPE(wd_,type_) (WD_INFO(wd_)->type = (type_))
#define WD_NAME(wd_)  (WD_INFO(wd_)->name + 0)
#define WD_SET_NAME(wd_, name_)  (WD_INFO(wd_)->name = (name_))
#define WD_ICONIFY(wd_) (WD_INFO(wd_)->wi_flags & WF_ICONIFY)
#define WD_SET_ICONIFY(wd_, state)			\
 ((void)((state && (WD_INFO(wd_)->wi_flags |= WF_ICONIFY))	\
	 || (WD_INFO(wd_)->wi_flags &= ~WF_ICONIFY)))
#define WD_MAP_NOTIFY(wd_) (WD_INFO(wd_)->wi_flags & WF_MAP_NOTIFY)
#define WD_MAP_NOTIFY_SET(wd_, state)			\
 ((void)((state && (WD_INFO(wd_)->wi_flags |= WF_MAP_NOTIFY))	\
	 || (WD_INFO(wd_)->wi_flags &= ~WF_MAP_NOTIFY)))
#define WD_USED_P(wd_) (WD_PTR(wd_) != 0)
#define WD_MAPPED_P(wd_) (WD_INFO(wd_)->wi_flags & WF_MAPPED)
#define WD_MAPPED_SET(wd_, state)			\
 ((void)((state && (WD_INFO(wd_)->wi_flags |= WF_MAPPED))	\
	 || (WD_INFO(wd_)->wi_flags &= ~WF_MAPPED)))
#define WD_CLIPPED_P(wd_) (WD_INFO(wd_)->wi_flags & WF_CLIPPED)
#define WD_CLIPPED_SET(wd_, state)			\
 ((void)((state && (WD_INFO(wd_)->wi_flags |= WF_CLIPPED))	\
	 || (WD_INFO(wd_)->wi_flags &= ~WF_CLIPPED)))

/* note that USER_PTR and USER_ID are member of the same union */
#define WD_USER_PTR(wd_) (WD_INFO(wd_)->u.user_ptr + 0)
#define WD_USER_PTR_SET(wd_, ptr_) (WD_INFO(wd_)->u.user_ptr = (ptr_))
#define WD_USER_ID(wd_) (WD_INFO(wd_)->u.user_id + 0)
#define WD_USER_ID_SET(wd_, id_) (WD_INFO(wd_)->u.user_id = (id_))

/* Intuition Screen */
#define WD_SCREEN_P(wd_) (WD_TYPE(wd_) == WT_ISCREEN)
#define WD_SCREEN(wd_) ((struct Screen *) WD_PTR2(wd_))
#define WD_SET_SCREEN(wd_,screen) (WD_SET_PTR(wd_,screen), WD_SET_TYPE(wd_, WT_ISCREEN))

/* Intuition Window */
#define WD_WIN_P(wd_) (WD_TYPE(wd_) == WT_IWIN)
#define WD_WIN(wd_) ((struct Window *) WD_PTR2(wd_))
#define WD_SET_WIN(wd_,win) (WD_SET_PTR(wd_,win), WD_SET_TYPE(wd_, WT_IWIN))

#define WD_MAINWIN_P(w_) (WD_WIN_P (w_) || WD_NWIN_P (w_))

/* Intuition NewWindow */
#define WD_NWIN_P(wd_) (WD_TYPE(wd_) == WT_NWIN)
#define WD_NWIN(wd_) ((struct ExtNewWindow *) WD_PTR2(wd_))
#define WD_SET_NWIN(wd_,win) (WD_SET_PTR(wd_,win), WD_SET_TYPE(wd_, WT_NWIN))

/* Intuition BOOPSI (gadget) */
#ifndef AMIGA_SCROLL_GADGET
#define WD_BOOPSI_P(wd_) (WD_TYPE(wd_) == WT_BOOPSI)
#else /* no AMIGA_SCROLL_GADGET */
/* Cause gcc to optimize away unneeded code -bw/12-Mar-99 */
#define WD_BOOPSI_P(wd_) 0
#endif /* no AMIGA_SCROLL_GADGET */
#define WD_BOOPSI(wd_) ((void *) WD_PTR2(wd_))
#define WD_BOOPSI_SET(wd_,win) (WD_SET_PTR(wd_,win), WD_SET_TYPE(wd_, WT_BOOPSI))


/* Subwindow (clipped rectangle) */
#define WD_SUBWIN_P(wd_) (WD_TYPE(wd_) == WT_SUBWIN)
//#define WD_SUBWIN(wd_) ((struct Region *) WD_PTR2(wd_))
#define WD_SUBWIN_SET(wd_,win) (WD_SET_PTR(wd_,win), WD_SET_TYPE(wd_, WT_SUBWIN))
#define WD_SUBWIN_EVENT_EXPOSE (01)
#define WD_SUBWIN_EVENT_MAP (02)
#define WD_SUBWIN_EVENT_UNMAP (04)
#define WD_WIN_EVENT_EXPOSE (10)

/* Workbench Icon */
#define WD_ICON_P(wd_) (WD_TYPE(wd_) == WT_ICON)
#define WD_ICON(wd_) ((struct DiskObject *) WD_PTR2(wd_))
#define WD_ICON_SET(wd_,win) (WD_SET_PTR(wd_,win), WD_SET_TYPE(wd_, WT_ICON))

/* type checking macro versions */
#ifdef FULLDEBUG
#undef WD_PTR2
#define WD_PTR2(wd_) ((assert (WD_PTR(wd_))), WD_PTR(wd_))
#undef WD_WIN
#define WD_WIN(wd_) \
((assert (WD_WIN_P(wd_))),((struct Window *) WD_PTR2(wd_)))
#undef WD_NWIN
#define WD_NWIN(wd_) \
((assert (WD_NWIN_P(wd_))),((struct ExtNewWindow *) WD_PTR2(wd_)))
#undef WD_SCREEN
#define WD_SCREEN(wd_) \
((assert (WD_SCREEN_P(wd_))),((struct Screen *) WD_PTR2(wd_)))
#undef WD_ICON
#define WD_ICON(wd_) ((assert (WD_ICON_P(wd_))),((struct DiskObject *) WD_PTR2(wd_)))
#define WD_IWIN_INFO(WD) ((assert (WD_WIN_P(WD))),&WD_INFO(WD)->u.win)
#define WD_WIN_INFO(WD) ((assert (WD_WIN_P(WD) || WD_SUBWIN_P (WD))),&WD_INFO(WD)->u.win)
#define WD_NWIN_INFO(WD) ((assert (WD_NWIN_P(WD))),&WD_INFO(WD)->u.nwin)
#define WD_SCREEN_INFO(WD) ((assert (WD_SCREEN_P(WD))),&WD_INFO(WD)->u.screen)
#else /* not FULLDEBUG */
#define WD_WIN_INFO(WD) (&WD_INFO(WD)->u.win)
#define WD_IWIN_INFO(WD) (&WD_INFO(WD)->u.win)
#define WD_NWIN_INFO(WD) (&WD_INFO(WD)->u.nwin)
#define WD_SCREEN_INFO(WD) (&WD_INFO(WD)->u.screen)
#endif

#define WD_COLCON(wd_) (WD_SCREEN_INFO (wd_)->colcon + 0)

#define WD_RPORT(wd_) (WD_WIN(wd_)->RPort + 0)
#define WD_LAYER(wd_) (WD_RPORT(wd_)->Layer + 0)
#define WD_ITOP(wd_) (WD_WIN(wd_)->BorderTop + 0)
#define WD_ILEFT(wd_) (WD_WIN(wd_)->BorderLeft + 0)
#if 0
#define WD_IBOTTOM(wd_) (WD_WIN(wd_)->Height - 1 - WD_WIN(wd_)->BorderBottom)
#define WD_IRIGHT(wd_) (WD_WIN(wd_)->Width - 1 - WD_WIN(wd_)->BorderRight)
#else
 /* To allow calculating clip region before resizing is done */
#define WD_IBOTTOM(wd_) (WD_HEIGHT(wd_) - 1 - WD_WIN(wd_)->BorderBottom)
#define WD_IRIGHT(wd_) (WD_WIDTH(wd_) - 1 - WD_WIN(wd_)->BorderRight)
#endif

#define WD_COLOR_A(wd_)  (GX_COLOR_A(WD_GX(wd_)))
#define WD_COLOR_B(wd_)  (GX_COLOR_B(WD_GX(wd_)))
#define WD_FG_PEN(wd_)  (COLOR_PEN (WD_COLOR_A (wd_)))
#define WD_BG_PEN(wd_)  (COLOR_PEN (WD_COLOR_B (wd_)))
#define WD_FID(wd_)  (GX_FID(WD_GX(wd_)))
#define WD_FILL_STYLE(wd_)  (GX_FILL_STYLE(WD_GX(wd_)))
#define WD_STIPPLE(wd_)  (GX_STIPPLE(WD_GX(wd_)))
#define WD_DRAW_MODE(wd_)  (GX_DRAW_MODE(WD_GX(wd_)))

#define WD_GX_COLOR_A(wd_,gc_) \
((!GX_NIL_P(gc_) && GX_MASK_FG_PEN(gc_)) ? GX_COLOR_A(gc_) : WD_COLOR_A(wd_))
#define WD_GX_COLOR_B(wd_,gc_) \
((!GX_NIL_P(gc_) && GX_MASK_BG_PEN(gc_)) ? GX_COLOR_B(gc_) : WD_COLOR_B(wd_))
#define WD_GX_FID(wd_,gc_) \
((!GX_NIL_P(gc_) && GX_MASK_FID(gc_)) ? GX_FID(gc_) : WD_FID(wd_))
#define WD_GX_FILL_STYLE(wd_,gc_) \
((!GX_NIL_P(gc_) && GX_MASK_FILL_STYLE(gc_)) ? GX_FILL_STYLE(gc_) : WD_FILL_STYLE(wd_))
#define WD_GX_STIPPLE(wd_,gc_) \
((!GX_NIL_P(gc_) && GX_MASK_STIPPLE(gc_)) ? GX_STIPPLE(gc_) : WD_STIPPLE(wd_))
#define WD_GX_DRAW_MODE(wd_,gc_) \
((!GX_NIL_P(gc_) && GX_MASK_DRAW_MODE(gc_)) ? GX_DRAW_MODE(gc_) : WD_DRAW_MODE(wd_))

/* get attribute of GC or default value DEF if GC is nil or haven't
   the attribute set */
#define GX_DEF_COLOR_A(gc_,def) \
((!GX_NIL_P(gc_) && GX_MASK_FG_PEN(gc_)) ? GX_COLOR_A(gc_) : (def))
#define GX_DEF_COLOR_B(gc_,def) \
((!GX_NIL_P(gc_) && GX_MASK_BG_PEN(gc_)) ? GX_COLOR_B(gc_) : (def))
#define GX_DEF_FID(gc_,def) \
((!GX_NIL_P(gc_) && GX_MASK_FID(gc_)) ? GX_FID(gc_) : (def))
#define GX_DEF_FILL_STYLE(gc_,def) \
((!GX_NIL_P(gc_) && GX_MASK_FILL_STYLE(gc_)) ? GX_FILL_STYLE(gc_) : (def))
#define GX_DEF_STIPPLE(gc_,def) \
((!GX_NIL_P(gc_) && GX_MASK_STIPPLE(gc_)) ? GX_STIPPLE(gc_) : (def))
#define GX_DEF_DRAW_MODE(gc_,def) \
((!GX_NIL_P(gc_) && GX_MASK_DRAW_MODE(gc_)) ? GX_DRAW_MODE(gc_) : (def))

#define WD_PARENT(wd_) (WD_INFO(wd_)->parent + 0)
#define WD_PARENT_SET(wd_,par) (WD_INFO(wd_)->parent = (par))

#define WD_NEXT(wd_) (WD_INFO(wd_)->next + 0)
#define WD_NEXT_SET(wd_,next_) (WD_INFO(wd_)->next = (next_))

#define WD_CHILD(wd_) (WD_INFO(wd_)->child + 0)
#define WD_CHILD_SET(wd_,child_) (WD_INFO(wd_)->child = (child_))

/* obsolet */
#define WD_CHILD__FOREACH(i, wd_) for ((i)=WD_CHILD (wd_); (i)!=WD_NIL; (i)=WD_NEXT (i))

#define WD_CHILD_FOREACH(wd_, i) for ((i)=WD_CHILD (wd_); (i)!=WD_NIL; (i)=WD_NEXT (i))
#define WD_CHILD_FOREACH_RM(wd_, i, tmp) \
for ((i)=WD_CHILD (wd_); (i)!=WD_NIL && ((tmp)=WD_NEXT (i),1); i=tmp)

#define WD_RECTANGLE(wd_, store)			\
(((store)->MinX = WD_LEFT(wd_)),			\
 ((store)->MinY = WD_TOP(wd_)),			\
 ((store)->MaxX = WD_RIGHT(wd_)),		\
 ((store)->MaxY = WD_BOTTOM(wd_)))
#define WD_IRECTANGLE(wd_, store)		\
(((store)->MinX = WD_ILEFT(wd_)),		\
 ((store)->MinY = WD_ITOP(wd_)),			\
 ((store)->MaxX = WD_IRIGHT(wd_)),		\
 ((store)->MaxY = WD_IBOTTOM(wd_)))

#define WD__CLIP_REGION(WD) (WD_WIN_INFO(WD)->clip_region + 0)
#define WD__CLIP_REGION_SET(wd_, reg) (WD_WIN_INFO(wd_)->clip_region = (reg))
#define WD__WBAPP_WINDOW(wd_) (WD_IWIN_INFO(wd_)->wbapp_window + 0)
#define WD__WBAPP_WINDOW_SET(wd_, reg) (WD_IWIN_INFO(wd_)->wbapp_window = (reg))

#define WD__USER_DATA(WD) (WD_NWIN_INFO(WD)->user_data + 0)
#define WD__USER_DATA_SET(WD, VAL) (WD_NWIN_INFO(WD)->user_data = (VAL))

#define WD_TOP(wd_) (WD_INFO(wd_)->top)
#define WD_LEFT(wd_) (WD_INFO(wd_)->left)
#define WD_HEIGHT(wd_) (WD_INFO(wd_)->height)
#define WD_WIDTH(wd_) (WD_INFO(wd_)->width)
#define WD_RIGHT(wd_) (WD_LEFT(wd_) + WD_WIDTH(wd_) - 1)
#define WD_BOTTOM(wd_) (WD_TOP(wd_) + WD_HEIGHT(wd_) - 1)
#define WD_GEOMETRY_SET(WD, X, Y, WIDTH, HEIGHT)	\
     (void)((WD_INFO(WD)->left = (X)),			\
	    (WD_INFO(WD)->top = (Y)),			\
	    (WD_INFO(WD)->width = (WIDTH)),		\
	    (WD_INFO(WD)->height = (HEIGHT)))
/* for IWINs only: */
#define WD_OUT_TOP(WD) WD_TOP (WD)
#define WD_OUT_LEFT(WD) WD_LEFT (WD)
#define WD_OUT_WIDTH(WD) WD_WIDTH (WD)
#define WD_OUT_HEIGHT(WD) WD_HEIGHT (WD)

#define WD_GX(wd_) (WD_INFO(wd_)->gc)
#define WD_GREATEST (win_desc_greatest + 0)
#define WD_GREATEST_SET(wd_) (win_desc_greatest = (wd_))
#define WD_VALID_P(wd_) (WD_MIN <= (wd_) && (wd_) < WD_MAX)
#define WD_NIL_P(wd_) ((wd_) == WD_NIL)
#define WD_NIL (0)
#if 0
#define WD_VISIBLE_P(wd_) \
(WD_VALID_P(wd_) &&WD_WIN(wd_) != 0 && WD_WIN(wd_) != (struct Window *)-1)
#endif

#define WD_MIN (1)
#define WD_MAX (200)		/* maximal number of wd (not the maximal wd) XXX */

/* #define WD_IDCMP_MAND(wd_) (IDCMP_CLOSEWINDOW | IDCMP_IDCMPUPDATE | IDCMP_CHANGEWINDOW) */
#define WD_IDCMP_MAND(wd_) (IDCMP_CLOSEWINDOW | IDCMP_CHANGEWINDOW)

#define WIN_MENU (0)
extern int win_simple_refresh;
#define WIN_SIMPLE_REFRESH (win_simple_refresh + 0)

enum win_type 
{
  WT_FREE,			/* means descriptor is not in use */
  WT_ISCREEN,			/* <struct Screen *> */
  WT_IWIN,			/* <struct Window *> */
  WT_NWIN,			/* <struct ExtNewWindow *>  */
  WT_BOOPSI,			/* Object (see BT_xxx type) */
  WT_ICON,		        /* <struct DiskObject *> for icons */
  WT_IMAGE,			/*   */
  WT_SUBWIN,                    /* clipped rectangle in real window */
  /* terminator */
  WT_end 
};

enum boopsi_type
/* actual type of WT_BOOPSI objects */
{
  BT_VSCROLLBAR,		/* <struct Gadget *> */
};

/* win_flags */
#define WF_LOCKED_PUB_SCREEN (01)
#define WF_ICONIFY (02)		/* suggests iconify when open window */
#define WF_MAPPED (04)		/* if true, the (sub-) window is visible */
#define WF_CLIPPED (010)	/* if true, all mapped childs and also
				   the borders are clipped */
#define WF_MAP_NOTIFY (020)	/* Set when doing one of mapping
				   unmapping. May cleared by user code. */

struct win_info
{ 
  enum win_type type;
  short wi_flags;
  short left, top, width, height;
  int parent;			/* descriptor for container window or 0 */
  char *name;			/* allocated storage for name string */
  int gc;			/* default graphic context */
  int child, next;		/* gadgets, ... */
  union 
    {
      ULONG user_id;
      void *user_ptr;
      struct 
	{
	  struct Region *clip_region;
	  struct AppWindow *wbapp_window;
	} win;
      struct
      {
	void *user_data;	/* keep Window.UserData while iconified */
      } nwin;
      struct
	{
	  struct AppIcon *app_icon;
	} icon;
      struct 
	{
	  void *colcon; /* color_context */
	} screen;
    } u;
  int change_flags;
#define WDI_CH_SIZE (01)
#define WDI_CH_POS (02)
#define WDI_CH_GADGETS (04)	/* Request a call to RefreshGadgets()  */
#define WDI_CH_FRAME (010)	/* Request a call to RefreshWindowFrame()  */
}; 

#define WDI_CH_ADD(wd_, mask)			\
if (WD_WIN_P (wd_) || WD_BOOPSI_P (wd_))		\
{						\
  if (!WD_INFO (wd_)->change_flags)		\
  {						\
    if (win_change_idx >= WD_CHANGE_TBL_SIZE)	\
      abort ();					\
    win_change_tbl[win_change_idx++] = (wd_);	\
  }						\
  WD_INFO (wd_)->change_flags |= (mask);		\
}

volatile extern struct win_info win_info_tbl[];
extern void *win_desc_tbl[];
extern struct ExtNewWindow win_new_win;
#define WD_CHANGE_TBL_SIZE (300)
extern int win_change_tbl[], win_change_idx;

struct RastPort *win__prepare (int wd, GC gc, int mask, int *x, int *y);
void win__gx_free (int gc);
int win__gx_alloc (void);


/* obsolete?! */
#define win__init_pref() ((win_pp = win_pref),(win_pp->ti_Tag = TAG_DONE))
#define win__wa_push(key, data) \
({ win_pp->ti_Tag = (ULONG)(key); \
   win_pp->ti_Data = (ULONG) (data); \
   (++win_pp)->ti_Tag = TAG_DONE; })

#define win__wa_modify(key, data, operator)		\
({ struct TagItem *ti = ti__find (win_pref, key);	\
     if (ti) ti->ti_Data operator (data);		\
 })

struct TagItem *ti__find (struct TagItem ti[], ULONG key);


/* graphic context */
#ifndef FULLDEBUG
#define GX_OBJ(gc_) (&win_gx_tbl[(gc_)])
#else
#define GX_OBJ(gc_) ((assert(GX_VALID_P(gc_))), (&win_gx_tbl[(gc_)]))
#endif

#define GX_MASK(gc_) (GX_OBJ(gc_)->mask)

#define GX_FID(gc_) (GX_OBJ(gc_)->fid + 0)
#define GX_FONT_STYLE(gc_) (GX_OBJ(gc_)->font_style + 0)
#define GX_FID_SET(gc_, fid_) ((GX_OBJ(gc_)->fid = (fid_)), (GX_MASK(gc_)|=GCFont))
#define GX_MASK_FID(gc_) (GX_MASK(gc_) & GCFont)

#define GX_FILL_STYLE(gc_) (GX_OBJ(gc_)->fill_style + 0)
#define GX_FILL_STYLE_SET(gc_, fill_style_) ((GX_OBJ(gc_)->fill_style = (fill_style_)), GX_MASK(gc_)|=GCFillStyle)
#define GX_MASK_FILL_STYLE(gc_) (GX_MASK(gc_) & GCFillStyle)

#define GX_STIPPLE(gc_) (GX_OBJ(gc_)->stipple + 0)
#define GX_STIPPLE_SET(gc_, stipple_) ((GX_OBJ(gc_)->stipple = (stipple_)), GX_MASK(gc_)|=GCStipple)
#define GX_MASK_STIPPLE(gc_) (GX_MASK(gc_) & GCStipple)

#define GX_COLOR_A(gc_) (GX_OBJ(gc_)->fg_pen + 0)
#define GX_COLOR_A_SET(gc_, col) ((GX_OBJ(gc_)->fg_pen = (col)), GX_MASK(gc_)|=GCForeground)
#define GX_MASK_FG_PEN(gc_) (GX_MASK(gc_) & GCForeground)

#define GX_COLOR_B(gc_) (GX_OBJ(gc_)->bg_pen + 0)
#define GX_COLOR_B_SET(gc_, col) ((GX_OBJ(gc_)->bg_pen = (col)), GX_MASK(gc_)|=GCBackground)
#define GX_MASK_BG_PEN(gc_) (GX_MASK(gc_) & GCBackground)

#define GX_DRAW_MODE(gc_) (GX_OBJ(gc_)->draw_mode + 0)
#define GX_DRAW_MODE_SET(gc_, drmd) ((GX_OBJ(gc_)->draw_mode = (drmd)), GX_MASK(gc_)|=GCFunction)
#define GX_MASK_DRAW_MODE(gc_) (GX_MASK(gc_) & GCFunction)

#define GX_EXPOSURES(gc_) (GX_OBJ(gc_)->exposures + 0)
#define GX_EXPOSURES_SET(gc_, exp) \
((GX_OBJ(gc_)->exposures = (exp)), GX_MASK(gc_)|=GCGraphicsExposures)
#define GX_MASK_EXPOSURES(gc_) (GX_MASK(gc_) & GCGraphicExposures)

     /* The region in GX is really a rectangle to avoid overkill */ 
#define GX_REGION(gc_) GX_OBJ(gc_)->region
#if 0
#define GX_REGION_SET(gc_, exp) \
((GX_OBJ(gc_)->region = (exp)), GX_MASK(gc_)|=GCRegion)
#endif /* 0 */
#define GX_MASK_REGION(gc_) (GX_MASK(gc_) & GCRegion)


#define GX_USED_P(gc_) (GX_OBJ(gc_)->mask)
#define GX_START 1
#define GX_END (win_gx_tbl_size)
#define GX_NIL 0
#define GX_NIL_P(gc_) ((gc_)==GX_NIL)
#define GX_VALID_P(gc_) (GX_START <= (gc_) && (gc_) < GX_END)

struct win_gc
{
  fid_type fid;
  Pixmap stipple;
  unsigned short mask;
  unsigned char fg_pen, bg_pen, draw_mode, exposures;
  unsigned fill_style;
#if 0
  struct Rectangle region;
  short xorig, yorig; 
#endif
};

extern struct win_gc *win_gx_tbl;
extern int win_gx_tbl_size;


/* Pixmap */
#define PIXMAP_WIDTH(pm) ((pm)->Width)
#define PIXMAP_HEIGHT(pm) ((pm)->Height)
/* Because this is an Image the data is located in chip ram.  But this
   isn't requred for RastPort->AreaPtrn FIXME-bw/14-Nov-97 */
#define PIXMAP_DATA(pm) ((pm)->ImageData)

/* amiga_window.c */

/* TODO-bw: use keymap library to construct rawkey event for interrupt_char */
#define IWIN_TEST_KBQUIT(im)			\
(((im)->Qualifier & quit_char_rk_qual)		\
 && ((im)->Class == IDCMP_RAWKEY)		\
 && ((im)->Code == quit_char_rk_code))

#define XFONT_AMIGA_FONT(xfont) (FID_FONT ((xfont)->fid))
#define WIN_INFO (WD_INFO (WD_CURR))
#define WIN_ROW (curs_y)
#define WIN_COL (curs_y)
#define win__set_row(row) (curs_y = (row))
#define win__set_col(col) (curs_x = (col))

#define FRAME_WD(f) ((f)->output_data.x->window_desc)
#define FRAME_WIN(f) (WD_WIN (FRAME_WD(f)))
#define FRAME_RPORT(f) (FRAME_WIN(f)->RPort)

#define WIN_RP_FONT (WIN_RPORT->Font)
#define WIN_TXT_HEIGHT (WIN_RPORT->TxHeight)
#define WIN_TXT_WIDTH (WIN_RPORT->TxWidth)
#define WIN_TXT_BASE (WIN_RPORT->TxBaseline)

#define WIN_LEFT 0
#define WIN_TOP 0
#define WIN_WIDTH (80 * 6)
#define WIN_HEIGHT (24 * 13)

//#define WIN_BACKFILL
#define WIN_BACKDROP 0
#define WIN_COMMON_USER_PORT (win_user_port)
#define WIN_WB_APP_PORT (win_wb_app_port)
//#define WIN_COMMON_USER_PORT 0

extern struct TagItem win_pref[100], *win_pp;
extern struct MsgPort *win_user_port, *win_wb_app_port;


/* icon */
struct DiskObject *win__icon_dobj_create ();
void win__icon_dobj_destroy (struct DiskObject *dobj);


#endif /* H_EMACS_AMIGA_WINDOW_DEFS_H_ */
/* EOF */
