/* window resource manager */
/* internal compontent header */


#include <signal.h>
#ifdef emacs
#ifndef C_XTERM_C_
#include <config.h>
#include "lisp.h"
#include "frame.h"
#include "dispextern.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"
#include "blockinput.h"
#endif /* not C_XTERM_C_ */
#undef point
#include "amiga_window.h"
#undef LONGBITS
#include "amiga_defs.h"
#endif

#include <dos/dosextens.h>
#include <sys/types.h>
#include <sys/time.h>

#include "dos/dos.h"
#include <graphics/gfxbase.h>
#include <graphics/text.h>
#include <intuition/gadgetclass.h>
#include <intuition/icclass.h>

#include <errno.h>
#include <ix.h>
#include <unistd.h>

#ifdef DB_TRACE
#undef DB_TRACE
#define DB_TRACE
#endif

struct Awin
{
  const struct Library *diskfont_base;
  const struct GfxBase *gfx_base;
  const struct Library *icon_base;
#ifdef AMIGA_IWIN_CLIPBOARD
  const struct Library *iffparse_base;
#endif /* AMIGA_IWIN_CLIPBOARD */
  const struct IntuitionBase *intuition_base;
  const struct UtilityBase *utility_base;
  const struct Library *workbench_base;
  const struct Library *keymap_base;
  const struct Library *layers_base;
};
extern struct Awin Awin;

struct Awin Awin;

#define DISKFONT_BASE_NAME (Awin.diskfont_base)
#define INTUITION_BASE_NAME (Awin.intuition_base)
#define ICON_BASE_NAME (Awin.icon_base)
#ifdef AMIGA_IWIN_CLIPBOARD
#define IFFPARSE_BASE_NAME (Awin.iffparse_base)
#endif /* AMIGA_IWIN_CLIPBOARD */
#define GRAPHICS_BASE_NAME (Awin.gfx_base)
#define UTILITY_BASE_NAME (Awin.utility_base)
#define WB_BASE_NAME (Awin.workbench_base)
#define KEYMAP_BASE_NAME (Awin.keymap_base)
#define LAYERS_BASE_NAME (Awin.layers_base)

#include <proto/diskfont.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <proto/graphics.h>
#include <proto/intuition.h>
#include <proto/icon.h>
#ifdef AMIGA_IWIN_CLIPBOARD
#include <proto/iffparse.h>
#endif /* AMIGA_IWIN_CLIPBOARD */
#include <proto/keymap.h>
#include <proto/utility.h>
#include <proto/wb.h>
#include <proto/layers.h>

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

#define Asys_perror(prefix) \
fprintf (stderr, "%s: %s: %s\n", prefix, ASYSE_RESOURCE, ASYSE_MSG)



/* create / destroy */
static int win__pref (void);	/* set preference */
static int win__create (int type);	/* open */
static void win__destroy (int wd); /* close */
static bool win__destroy_temp (int wd); /* save attributes  and close */
static bool win__reopen (int wd); /* reopen window closed by win__destroy_temp()  */
static void win___unlink_child (Window child);


struct win_event
{
  struct Message msg;
  ULONG key;
  unsigned long time_stamp;
  int wd;
#define WD_EVENT_MAP  (~1)
#define WD_EVENT_UNMAP (~2)
};

bool Awin_event_test (const struct Message *msg);
bool Awin_event_put (Window wd, int code);


extern Lisp_Object last_mouse_scroll_bar;

extern struct IntuiMessage win_mouse_pos;
extern struct MsgPort *win_user_port;
extern void *win_desc_tbl[];
extern volatile struct win_info win_info_tbl[];
extern struct ExtNewWindow win_new_win;
extern struct TagItem win_pref[], *win_pp;
extern int win_fid_index;
extern struct TextFont *win_font_tbl[];
extern unsigned char win_font_style_tbl[];
extern volatile sig_atomic_t Awin_input_sigmask;
extern struct ExtNewWindow win_new_win;
extern long Awin_connection_fd;
