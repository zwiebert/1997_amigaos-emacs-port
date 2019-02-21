#ifndef H_EMACS_AMIGA_WINDOW_H_
#define H_EMACS_AMIGA_WINDOW_H_

#include "amiga.h"
#include <assert.h>
#undef LONGBITS
#include <intuition/screens.h>	/* for window border widths*/
#include <graphics/text.h>	/* for <struct TextFont> */

typedef void (*exit_fp)(void);
typedef int (*atexit_fp)(exit_fp);
bool Awin_init_module (atexit_fp reg);
void Awin_exit_module (void);

void Awin_hook_in (void);
void Awin_hook_out (void);

#ifdef AMIGA_HAVE_INTUI
/* Dummy types.  */
typedef int XEvent;
typedef struct { char byte1, byte2; } XChar2b;
typedef int GC;
typedef int fid_type; /* my own ivention XXX-bw/28-Oct-97 */
typedef struct Image *Pixmap; /* ??? */
//typedef struct Screen Screen;
typedef int Screen;
typedef struct Display Display;
typedef int Window;
typedef Window Drawable;
typedef int Cursor;
typedef struct AIT_gc_values XGCValues;
typedef int Atom;
typedef int XrmDatabase;
typedef struct { int class; } Visual;
typedef int Time;

/* clipping */
typedef struct Region *Region;
typedef struct 
{
  short x, y;
} XPoint;

void XDestroyRegion (Region region);
Region XPolygonRegion (const XPoint *points, int nmb, int rule);
void XSetRegion (Display *dpy, GC gc, Region region);

/* fillRule */
#define EvenOddRule		0
/* #define WindingRule		1 */

/* 
 * Bitmask returned by XParseGeometry().  Each bit tells if the corresponding
 * value (x, y, width, height) was found in the parsed string.
 */
#define NoValue		0x0000
#define XValue  	0x0001
#define YValue		0x0002
#define WidthValue  	0x0004
#define HeightValue  	0x0008
#define AllValues 	0x000F
#define XNegative 	0x0010
#define YNegative 	0x0020

int XParseGeometry (const char *s, int *x, int *y, unsigned *width, unsigned *height);

/* intensity values are in range 0...65280 for each cannon */
typedef struct { unsigned short red, green, blue; short pixel; } XColor;

typedef int Colormap;
#define PIX_TYPE int
#define XDISPLAY
#define MAXREQUEST(dpy) 42

#define True 1
#define False 0
#define None 0			/* WD_NIL_P(None) == True */

#define ScreenOfDisplay(dpy, def) AIT_ScreenOfDisplay(dpy, def)
#define DefaultScreen(dpy) AIT_DefaultScreen(dpy)
#define HeightOfScreen(screen) AIT_HeightOfScreen(screen)
#define WidthOfScreen(screen) AIT_WidthOfScreen(screen)
#define CopyFromParent (-1)
#define InputOutput (-1)
#include <devices/inputevent.h>
#define ShiftMask (IEQUALIFIER_LSHIFT | IEQUALIFIER_RSHIFT)
#define ControlMask (IEQUALIFIER_CONTROL)

#define  XSizeHints struct AIT_XSizeHints

struct AIT_XSizeHints 
{
  int x, y, height, width,
  height_inc, width_inc, max_width, max_height,
  base_width, base_height, min_width, min_height;
  int win_gravity;
  int flags;
#define PResizeInc 01		/* resize increments (row/line sizes) */
#define PMinSize 02		/* program minimal size (WA_Min...) */
#define PMaxSize 04		/* program maximal size (WA_Max...)  */
#define PSize 010		/* program/preference size ? */
#define PPosition 020		/* program position ? */
#define USPosition 040		/* user position */
#define USSize 0100		/* user size */
};

#define XWMHints struct AIT_XWMHints
struct AIT_XWMHints
{
  int initial_state;
  int flags;
  int input;
  short icon_x, icon_y;
  Pixmap icon_pixmap;
#define StateHint 01
#define InputHint 02
#define IconPositionHint 04
};

struct XColor
{
  int red, green, blue;
  int pixel;
};


struct XRectangle {
  short x, y, width, height;
};
typedef struct XRectangle XRectangle;

typedef struct
{
  unsigned char ascent, descent, width, height;
} XCharStruct;

typedef struct
{
  unsigned char descent, ascent, min_byte1, max_byte1,
    min_char_or_byte2, max_char_or_byte2;
  XCharStruct min_bounds, max_bounds;
  fid_type fid;			/* XXX */
  struct TextAttr attrs;
}  XFontStruct;

struct Display {
  struct AIT_XSizeHints size_hints;
  XrmDatabase db;
  Window screen;		/* actually this is the screen disguised as Window */
};

typedef struct
{
  unsigned short foreground_pixel, background_pixel, border_pixel;
  unsigned event_mask;
  int cursor;
  int bit_gravity;
  int backing_store;
  int save_under;
  int attribute_mask;
/*
 = StaticGravity;  = NotUseful;  = STANDARD_EVENT_SET;
*/

#define CWForePixel 01
#define CWBackPixel 02
#define CWEventMask 04
#define CWCursor 010
#define CWBorderPixel 020
#define CWBitGravity 040
#define CWBackingStore 0100
#define CWSaveUnder 0200
/* note other CW.. defines below */
}
XSetWindowAttributes;

typedef struct
{
  short x, y, width, height;	/* like XRectangle (don't change order!)*/
#define CWX 0400
#define CWY 01000
#define CWWidth 02000
#define CWHeight 04000
}
XWindowChanges;


/* input mask for an X window */
enum 
{
  KeyPressMask = 01,
  ExposureMask = 02,
  ButtonPressMask = 04,
  ButtonReleaseMask = 010,
  PointerMotionMask = 020,
  PointerMotionHintMask = 040,
  StructureNotifyMask = 0100,
  FocusChangeMask = 0200,
  LeaveWindowMask = 0400,
  EnterWindowMask = 01000,
  VisibilityChangeMask = 02000,
  ButtonMotionMask = 04000,
};

#if 0
#define JAM1	    0	      /* jam 1 color into raster */
#define JAM2	    1	      /* jam 2 colors into raster */
#define COMPLEMENT  2	      /* XOR bits into raster */
#define INVERSVID   4	      /* inverse video for drawing modes */
#endif

struct AIT_gc_values {
  unsigned char function;
  short foreground, background;
  fid_type font;		/* XXX */
  int graphics_exposures;
  Pixmap stipple;
  int fill_style;
  int line_width; 	/* Means 1 using fast algorithm.  ??? */
 /* flags indicated valid fields */
#define GCForeground 02
#define GCBackground 04
#define GCFont 010
#define GCGraphicsExposures 020
#define GCFunction 040
#define GCInverse 0100		/* internal use only */
#define GCFillStyle 0200
#define GCStipple 0400
#define GCLineWidth 01000
#define GCRegion 02000


  /* functions */
#define GXxor 2
#define GInv 4			/* ??? */
};

/* Used in ChangeCloseDownMode */

#define DestroyAll              0
#define RetainPermanent         1
#define RetainTemporary         2

/* FIXME-bw: */

int BlackPixel(Display *dpy, int screen_nmb);
int WhitePixel(Display *dpy, int screen_nmb);
char *ServerVendor(Display *dpy);

int AIT_HeightOfScreen (int screen);
int AIT_WidthOfScreen (int screen);

/* ??? */
int AIT_DefaultScreen(Display *dpy);
int AIT_ScreenOfDisplay (Display *dpy, int screen);

bool AITcolor_query (const char *name, XColor *color_ptr);
bool AITcolor_alloc (Display *dpy, XColor *color_def);
int AITcolor_free (Display *dpy, int pixel);

struct frame;
void XResizeWindow (Display *dpy, int win_desc, int pix_width, int pix_height);

 /* X API */

int ConnectionNumber (Display *dpy); /* input file descriptor */
Display *XOpenDisplay (const char *name);

bool XSetForeground (Display *dpy, GC gc, unsigned color);
bool XSetBackground (Display *dpy, GC gc, unsigned color);
bool XSetWindowBackground (Display *dpy, Window wd, unsigned color);
bool XSetWindowBorder (Display *dpy, Window wd, unsigned color);

Pixmap XCreateBitmapFromData (Display *dpy, Window wd, 
			      unsigned char *bits, int width, int height);
void XFreePixmap (Display *dpy, Pixmap pm);



bool XQueryTree (Display *dpy, Window wd, Window *store_root, Window *store_parent,
		 Window **store_children, int *store_nmb_children);
/* the caller is responsible to free *STORE_CHILDREN */

GC XCreateGC (Display *dpy, int wd, int mask, XGCValues *xgcv);
void XChangeGC (Display *dpy, GC gc, int mask,  XGCValues *xgcv); 
void XFreeGC (Display *dpy, GC gc);

void XDrawRectangle (Display *dpy, Window wd, GC gc,
		     int left, int top, int width, int height);
void XFillRectangle (Display *dpy, Window wd, GC gc,
		     int left, int top, int width, int height);
void XDrawString (Display *dpy, Window wd, GC gc,
		  int left, int top, const char *buf, int len);
void XDrawImageString  (Display *dpy, Window wd, GC gc,
			int left, int top, const char *buf, int len);
void XClearArea (Display *dpy, Window wd,
		 int left, int top, int width, int height, bool xxx);
void XClearWindow (Display *dpy, Window wd);

void XCopyArea (Display *dpy, Window wd, Window wd2,  GC gc,
		int left, int top, int width, int height, int to_left, int to_top);

int XGetNormalHints (Display *dpy, Window win_des, XSizeHints *hints);
void XSetNormalHints (Display *dpy, Window win_des, XSizeHints *hints);
void XSetWMHints (Display *dpy, Window wd, XWMHints *hints);

Window XCreateWindow (Display *dpy,
		      Window parent_des,
		      int left, int top, int pix_width, int pix_height,
		      int border_width, /* unused */
		      int depth, /* unused */
		      int in_out, /* unused */
		      Visual *visual,
		      int attr_mask, const XSetWindowAttributes *attrs);
void XDestroyWindow (Display *dpy, Window wd);
bool XWithdrawWindow (Display *dpy, Window wd, Screen *screen); /* iconify */
//bool XConfigureWindow (Display *dpy, Window wd, int mask, XWindowChanges *wc);
void XMoveWindow (Display *dpy, Window wd, int left, int top);
void XResizeWindow (Display *dpy, Window wd, int width, int height);

/* Translate window coordinates to ancestor window coordinates */
void XTranslateCoordinates (Display *dpy, Window w1, Window w2,
			    int w1_x, int w1_y,	/* given w1 coords */
			    int *w2_x, int *w2_y, /* returned w2 coords */
			    Window *child); /* RESULT */

void XSetFont (Display *dpy, GC gc, fid_type fid);
char **XListFonts (Display *dpy, char *pattern, int max_names, int *actual_names);
char **XListFontsWithInfo (Display *dpy, char *pattern, int max_names,
			   int *actual_names, XFontStruct **actual_info);

void XFreeFont (Display *dpy, XFontStruct *font);
XFontStruct *XLoadQueryFont (Display *dpy, const char *name);

/* fill style */
#define FillSolid 00
#define FillStippled 01
#define FillOpaqueStippled 02

/* gravity */
#define NorthWestGravity 01
#define SouthEastGravity 02
#define NorthEastGravity 04
#define SouthWestGravity 010
#define StaticGravity  020

/* Display classes  used in opening the connection 
 * Note that the statically allocated ones are even numbered and the
 * dynamically changeable ones are odd numbered */

#define StaticGray 0
#define GrayScale 1
#define StaticColor 2
#define PseudoColor 3
#define TrueColor 4
#define DirectColor 5

/* backing store */
#define Always 1
#define WhenMapped 2
#define NotUseful 3

/* window open state */
#define NormalState 0
#define IconicState 1

#ifndef FULLDEBUG
#define arrow(ptr,field) ((ptr)->field)
#define check_ptr(ptr) (0)
#else
#define arrow(ptr,field) ((assert(ptr)),((ptr)->field))
#define check_ptr(ptr) (assert (ptr))
#endif

/* get the Amiga structures of a given XFontStruct address */

#define FONT_TATTR(font) (& arrow ((font),attrs))
extern struct TextFont *win_font_tbl[];
#define FONT_TFONT(font) (check_ptr(font), \
 (win_font_tbl[((font)->fid)]))
#define FONT_WIDTH(font) (FONT_TFONT(font)->tf_XSize + 0)
#define FONT_HEIGHT(font) (FONT_TFONT(font)->tf_YSize + 0)
#define FONT_BASE(font) (FONT_TFONT(font)->tf_Baseline + 0)


#ifdef AMIGA_HAVE_INTUI
/* char<=>pixel conversions implemented for Amiga-Intuition windows
   using: ((WFLG_SIZEBBOTTOM | WFLG_DRAGBAR) & ~WFLG_SIZEBRIGHT)

  -bw/26-Oct-97 */

#define WIN_SIZE_GADGET_HEIGHT (8)
#define WIN_SIZE_GADGET_WIDTH (14)

/* Manipulating pixel sizes and character sizes.
   Knowledge of which factors affect the overall size of the window should
   be hidden in these macros, if that's possible.

   Return the upper/left pixel position of the character cell on frame F
   at ROW/COL.  */

#ifndef XAPI_GZZ
#define CHAR_TO_PIXEL_ROW(f, row)			\
  ((WD_SCREEN (FRAME_X_SCREEN (f))->WBorTop + 1		\
    + WD_SCREEN (FRAME_X_SCREEN (f))->Font->ta_YSize)	\
   + (f)->output_data.x->internal_border_width		\
   + (row) * (f)->output_data.x->line_height)
#define CHAR_TO_PIXEL_COL(f, col)			\
  (WD_SCREEN (FRAME_X_SCREEN (f))->WBorLeft		\
   + (f)->output_data.x->internal_border_width		\
   + (col) * FONT_WIDTH ((f)->output_data.x->font))

/* Return the pixel width/height of frame F if it has
   WIDTH columns/HEIGHT rows.  */
#define CHAR_TO_PIXEL_WIDTH(f, width)			\
  (CHAR_TO_PIXEL_COL (f, width)				\
   + WD_SCREEN (FRAME_X_SCREEN (f))->WBorRight		\
   + (f)->output_data.x->vertical_scroll_bar_extra	\
   + (f)->output_data.x->internal_border_width)
#define CHAR_TO_PIXEL_HEIGHT(f, height)		\
  (CHAR_TO_PIXEL_ROW (f, height)		\
   + WD_SCREEN (FRAME_X_SCREEN (f))->WBorBottom	\
   + WIN_SIZE_GADGET_HEIGHT			\
   + (f)->output_data.x->internal_border_width)

/* Return the row/column (zero-based) of the character cell containing 
   the pixel on FRAME at ROW/COL.  */
#define PIXEL_TO_CHAR_ROW(f, row)				\
  (((row) - (WD_SCREEN (FRAME_X_SCREEN (f))->WBorTop + 1 	\
	     + WD_SCREEN (FRAME_X_SCREEN (f))->Font->ta_YSize)	\
    - (f)->output_data.x->internal_border_width)		\
    / (f)->output_data.x->line_height)
#define PIXEL_TO_CHAR_COL(f, col)			\
   (((col) - WD_SCREEN (FRAME_X_SCREEN (f))->WBorLeft 	\
     - (f)->output_data.x->internal_border_width)	\
    / FONT_WIDTH ((f)->output_data.x->font))

/* How many columns/rows of text can we fit in WIDTH/HEIGHT pixels on
   frame F?  */
#define PIXEL_TO_CHAR_WIDTH(f, width)						\
  (PIXEL_TO_CHAR_COL (f, ((width)						\
			  - (f)->output_data.x->internal_border_width		\
			  -WD_SCREEN (FRAME_X_SCREEN (f))->WBorRight		\
			  - (f)->output_data.x->vertical_scroll_bar_extra)))
#define PIXEL_TO_CHAR_HEIGHT(f, height)					\
  (PIXEL_TO_CHAR_ROW (f, ((height)					\
			  - (f)->output_data.x->internal_border_width	\
			  - WD_SCREEN (FRAME_X_SCREEN (f))->WBorBottom	\
			  - WIN_SIZE_GADGET_HEIGHT)))
#endif /* not XAPI_GZZ */

#endif /* AMIGA_HAVE_INTUI */

#if 0
#ifndef H_EMACS_XTERM_H_
#include "xterm.h"
#endif
#endif

#endif  /* AMIGA_HAVE_INTUI */

#endif /* H_EMACS_AMIGA_WINDOW_H_ */
