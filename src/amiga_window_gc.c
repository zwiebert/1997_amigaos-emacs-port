#include "amiga_window_defs.h"

struct win_gc *win_gx_tbl;
static int win_gx_tbl_size;



static void
win__gx_free_all ()
{
  int i;

  xfree (win_gx_tbl);
  win_gx_tbl = 0;
  win_gx_tbl_size = 0;
}

static void
win__gx_grow ()
{
  int new_sz, old_sz;

  old_sz = win_gx_tbl_size;
  new_sz = old_sz + 20;

  assert (new_sz > old_sz);

  win_gx_tbl = (void*)xrealloc (win_gx_tbl, new_sz * sizeof win_gx_tbl[0]);
  bzero (&win_gx_tbl[old_sz], (new_sz - old_sz) * sizeof win_gx_tbl[0]);
  win_gx_tbl_size = new_sz;
}

int
win__gx_alloc ()
{
  int i;
  int gc=0;

  for (i=GX_START; i < GX_END; ++i)
    if (! GX_USED_P (i))
      {
	gc = i;
	break;
      }

  if (!gc)
    {
      win__gx_grow ();
      gc = i;
    }

  assert (!GX_USED_P(i));
  win_gx_tbl[gc].mask = 1;	/* mark as used */
  return gc;
}

void
win__gx_free (int gc)
{
  check_precond (GX_VALID_P (gc));
  if (win_gx_tbl[gc].stipple)
    XFreePixmap ((void *)~0, win_gx_tbl[gc].stipple); /* XXX-bw/14-Nov-97 */
  bzero (&win_gx_tbl[gc], sizeof win_gx_tbl[gc]);
}
