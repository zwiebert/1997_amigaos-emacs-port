#ifndef emacs
#include <proto/exec.h>
#include <proto/graphics.h>
#include <proto/intuition.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define check_precond assert
#define local_precond assert

typedef enum { false, true} bool;

typedef struct
{
  struct Screen *screen;
  struct win_color_context *color_context;
} Display;

typedef struct { unsigned short red, green, blue; short pixel; } XColor;
#define DISPLAY_SCREEN(dpy_) ((dpy_)->screen + 0)
#define DISPLAY_COLCON(dpy_) ((dpy_)->color_context + 0)

typedef struct win_color_context *COLOR_CONTEXT;

#else /*  emacs */
#define DISPLAY_COLCON(dpy_) (WD_COLCON ((dpy_)->screen))
#endif /*  emacs */

#define COLOR_RGB2INT(r_, g_, b_) \
(((r_ & ((1<<10)-1)) << 20) | ((g_ & ((1<<10)-1)) << 10) | ((b_ & ((1<<10)-1)) << 0))

struct win_color_context
{
  struct Screen *screen;
  int (*open) ();
  void (*close) ();
  void (*destroy) (/* COLOR_CONTEXT obj */);

  /* make a copy of OBJ on any SCREEN. */
  COLOR_CONTEXT (*clone) (/* COLOR_CONTEXT obj, struct Screen *screen */);

  /* get pen of PIXEL.  May obtain a new pen, if deferred obtaining was enabled */
  int (*pix2pen) (/* int pixel */);

  /* release pen of PIXEL or all pens if PIXEL == -1 */
  void (*redefer) (/* COLOR_CONTEXT obj, int pixel */);
  void (*redefer2) (/* COLOR_CONTEXT obj, long *ignore_pens, int ip_size */);

  union {
    struct win_color_context_v39 *v39;
  } u;
};

/* exported interface */
COLOR_CONTEXT win_color_context_create (struct Screen *screen);
void win_color_context_destroy (COLOR_CONTEXT obj);
bool AITcolor_alloc (Display *dpy, XColor *color);
int AITcolor_free (Display *dpy, int col);
bool AITcolor_query (const char *name, XColor *color);
/* Acces methods */
int win_color_pen (COLOR_CONTEXT obj, unsigned pixel);

/* XXX-bw/08-Apr-98: This global should moved to color context. */
int win_color_defer = 1; /* bool value, causing late obtaining of pens in 
			    get_pen() method*/


/* V39 specific */

/* calc size for realloc()'ing cell array */
#define COLOR_CC_SIZE_V39(N_) \
  (sizeof (struct win_color_context_v39) \
   + sizeof (struct win_color_cell) * ((N_) - 1))


struct win_color_cell
{
  unsigned long pen, rgb, count, flags;
#define WIN_COLOR_CELL_DEFER (01)
};

struct win_color_context_v39
{
  int n_cells, first_free, n_used;
  struct win_color_cell cells[1];
};

static void
win_color_free_v39 (COLOR_CONTEXT obj, int pixel)
{
  struct win_color_cell *cell;

  check_precond (pixel < obj->u.v39->n_cells);

  cell = &obj->u.v39->cells[pixel];

  bzero (cell, sizeof *cell);
  --obj->u.v39->n_used;

  if (pixel < obj->u.v39->first_free)
    obj->u.v39->first_free = pixel;
}


static int
win_color_alloc_v39 (COLOR_CONTEXT obj)
{
  int i, result;			/* RESULT */
  struct win_color_context_v39 *cc;

  cc = obj->u.v39;

  if (!cc)
    {

      cc = obj->u.v39 = realloc (0, COLOR_CC_SIZE_V39 (20));

      if (!cc)
	return -1;
      cc->n_used = 1;
      cc->first_free = 1;
      cc->n_cells = 20;
      bzero (cc->cells, 20 * sizeof *cc->cells);
      return 0;
    }
    

  result = cc->first_free;

  if (result < cc->n_cells)
    {
      for (i = result; ++i <cc->n_cells;)
	if (obj->u.v39->cells[i].count == 0)
	  break;
     cc->first_free = i;
     ++cc->n_used;
    }
  else
    {
      cc = realloc (cc,  COLOR_CC_SIZE_V39 (cc->n_cells + 20));
      if (!cc)
	return -1;
      obj->u.v39 = cc;
      bzero (cc->cells + cc->n_cells, 20 * sizeof *cc->cells);
      result = cc->n_cells;
      cc->first_free = result + 1;
      cc->n_cells += 20;
      ++cc->n_used;
    }

  return result;
}

static void
win_color_context_destroy_v39 (COLOR_CONTEXT obj)
{
  int i;

  for (i=0; i < obj->u.v39->n_cells; ++i)
    {
      struct win_color_cell *cell = &obj->u.v39->cells[i];
      if (cell->count && !(cell->flags & WIN_COLOR_CELL_DEFER))
	ReleasePen (obj->screen->ViewPort.ColorMap, cell->pen);
    }
  free (obj);
}

static void
win_color_close_v39 (COLOR_CONTEXT obj, unsigned long pixel)
{
  struct win_color_cell *cell;

  check_precond (pixel < obj->u.v39->n_cells);

  cell = &obj->u.v39->cells[pixel];

  if (cell->count && !--cell->count)
    {
      if (!(cell->flags & WIN_COLOR_CELL_DEFER))
	ReleasePen (obj->screen->ViewPort.ColorMap, cell->pen);
      win_color_free_v39 (obj, pixel);
    }
}

static int
win_color_open_v39 (COLOR_CONTEXT obj,
		    int r, int g, int b, unsigned bits)
{
  int i, k, rgb, pen, pixel;
  struct win_color_context_v39 *cc;

  check_precond (GRAPHICS_BASE_NAME && obj && bits > 0);

  pen = pixel = -1;

  cc = obj->u.v39;

  rgb = ((bits <= 10) ? COLOR_RGB2INT (r, g, b)
	 : COLOR_RGB2INT (r >> (bits - 10),
			  g >> (bits - 10),
			  b >> (bits - 10)));

  /* Try to find similar sharable color in used cells */
  for (i=0, k = (cc ? cc->n_used : 0); k && i < cc->n_cells; ++i)
    if (cc->cells[i].count)
      if (rgb != cc->cells[i].rgb)
	--k;
      else
	{
	  ++cc->cells[i].count;
	  return i;
	}

  pixel = win_color_alloc_v39 (obj);
  if (pixel < 0)
    goto fail;
  else
    cc = obj->u.v39;

  if (!win_color_defer)
    {
      pen = ObtainBestPen (obj->screen->ViewPort.ColorMap,
			   /* do left justify */
			   (((ULONG)r) << (32 - bits)),
			   (((ULONG)g) << (32 - bits)),
			   (((ULONG)b) << (32 - bits)),
			   OBP_Precision, PRECISION_EXACT,
			   TAG_DONE);
      if (pen < 0)
	goto fail;
    }

  {
    struct win_color_cell *cell;
    assert (cc);

    cell = &cc->cells[pixel];
    if (win_color_defer)
      cell->flags |= WIN_COLOR_CELL_DEFER;
    else
      cell->pen = pen;

    cell->rgb = rgb;
    cell->count = 1;
  }

  return pixel;

 fail:
  if (pen >= 0)
    ReleasePen (obj->screen->ViewPort.ColorMap, pen);
  if (pixel >= 0)
    win_color_free_v39 (obj, pen);    
  return -1;
}

int
win_color_pen_v39 (COLOR_CONTEXT obj, unsigned pixel)
{
  struct win_color_cell *cell;

  local_precond (obj);
  check_precond (pixel < obj->u.v39->n_cells && obj->u.v39->cells[pixel].count > 0);

  cell = &obj->u.v39->cells[pixel];

  if (cell->flags & WIN_COLOR_CELL_DEFER)
    {
      cell->pen =  ObtainBestPen (obj->screen->ViewPort.ColorMap,
				  /* do left justify */
				  ((((ULONG)cell->rgb) << 2) & ~((1<<10)-1)),
				  ((((ULONG)cell->rgb) << 12) & ~((1<<10)-1)),
				  ((((ULONG)cell->rgb) << 22) & ~((1<<10)-1)),
				  OBP_Precision, PRECISION_EXACT,
				  TAG_DONE);
      if (cell->pen >= 0)
	cell->flags &= ~WIN_COLOR_CELL_DEFER;
      else
	cell->pen = 1;		/* XXX-bw/08-Apr-98 */
    }

  return cell->pen;
}

void
win_color_redefer_v39 (COLOR_CONTEXT obj, int pixel)
{
  int i;

  if (pixel != -1)
    assert (!"implemented");

  for (i=0; i < obj->u.v39->n_cells; ++i)
    {
      struct win_color_cell *cell = &obj->u.v39->cells[i];

      if (cell->count && !(cell->flags & WIN_COLOR_CELL_DEFER))
	ReleasePen (obj->screen->ViewPort.ColorMap, cell->pen);
      cell->pen = 0;
      cell->flags |= WIN_COLOR_CELL_DEFER;
    }
}

/* redefer all colors whose pens aren't in IGNORE_PENS */
void
win_color_redefer2_v39 (COLOR_CONTEXT obj, long *ignore_pens, int ip_size)
{
  int i, k;

  if (!ip_size)
    win_color_redefer_v39 (obj, -1);

  assert (ignore_pens);

  for (i=0; i < obj->u.v39->n_cells; ++i)
    {
      struct win_color_cell *cell = &obj->u.v39->cells[i];

      if (cell->count && !(cell->flags & WIN_COLOR_CELL_DEFER))
	{
	  for (k=0; k < ip_size; ++k)
	    if (ignore_pens[k] == cell->pen)
	      goto next_i;
	  ReleasePen (obj->screen->ViewPort.ColorMap, cell->pen);
	  cell->flags |= WIN_COLOR_CELL_DEFER;
	}
      cell->pen = 0;

    next_i: continue;
    }
}

static COLOR_CONTEXT win_color_context_clone_v39 ();

static COLOR_CONTEXT
win_color_context_create_v39 (struct Screen *screen)
{
  COLOR_CONTEXT obj; /* RESULT */

  obj = malloc (sizeof *obj);
  if (obj)
    {
      bzero (obj, sizeof *obj);	/* not really needed */
      obj->screen = screen;
      obj->open = win_color_open_v39;
      obj->close = win_color_close_v39;
      obj->destroy = win_color_context_destroy_v39;
      obj->clone = win_color_context_clone_v39;
      obj->pix2pen = win_color_pen_v39;
      obj->redefer = win_color_redefer_v39;
      obj->redefer2 = win_color_redefer2_v39;
      obj->u.v39 = 0;
    }
  return obj;
}


static COLOR_CONTEXT
win_color_context_clone_v39 (COLOR_CONTEXT obj, struct Screen *screen)
{
  int i;
  COLOR_CONTEXT new_obj; /* RESULT */
  
  new_obj = win_color_context_create_v39 (screen);
  if (!new_obj)
    return 0;

  new_obj->u.v39 = realloc (0,  COLOR_CC_SIZE_V39 (obj->u.v39->n_cells));
  if (!new_obj->u.v39)
    {
      free (new_obj);
      return 0;
    }

  bcopy (obj->u.v39, new_obj->u.v39, COLOR_CC_SIZE_V39 (obj->u.v39->n_cells));
  
  for (i=0; i < new_obj->u.v39->n_cells; ++i)
    {
      struct win_color_cell *cell = &new_obj->u.v39->cells[i];
      if (cell->count)
	{
	  if (win_color_defer)
	    {
	      cell->pen = 0;
	      cell->flags |= WIN_COLOR_CELL_DEFER;
	      continue;
	    }

	  cell->flags &= ~WIN_COLOR_CELL_DEFER;
	  cell->pen =  ObtainBestPen (new_obj->screen->ViewPort.ColorMap,
				      /* do left justify */
				      ((((ULONG)cell->rgb) << 2) & ~((1<<10)-1)),
				      ((((ULONG)cell->rgb) << 12) & ~((1<<10)-1)),
				      ((((ULONG)cell->rgb) << 22) & ~((1<<10)-1)),
				      OBP_Precision, PRECISION_EXACT,
				      TAG_DONE);
	  if (cell->pen >= 0)
	    continue;

	  /* release already obtained pens */
	  while (i--)
	    {
	      cell = &new_obj->u.v39->cells[i];
	      if (cell->count && !(cell->flags & WIN_COLOR_CELL_DEFER))
		ReleasePen (new_obj->screen->ViewPort.ColorMap, cell->pen);
	    }
	  free (new_obj->u.v39);
	  free (new_obj);
	  return 0;
	}
    }
  return new_obj;
}



COLOR_CONTEXT
win_color_context_create (struct Screen *screen)
{
  if (((struct Library *)GRAPHICS_BASE_NAME)->lib_Version >= 39)
    return win_color_context_create_v39 (screen);
  else
    return 0; /* TODO-bw/05-Apr-98: implement it for pre V39 systems */
}

COLOR_CONTEXT
win_color_context_clone (COLOR_CONTEXT obj, struct Screen *screen)
{
  check_precond (obj && screen);

  if (!obj->clone)
    return 0;

  return (*obj->clone) (obj, screen);
}

void
win_color_context_destroy (COLOR_CONTEXT obj)
{
  check_precond (obj);

  if (obj->destroy)
    (*obj->destroy) (obj);
}

int
win_color_pen (COLOR_CONTEXT obj, unsigned pixel)
{
  check_precond (obj);
  if (obj->pix2pen)
    return (*obj->pix2pen) (obj, pixel);

  assert (!"implemented");
  abort ();
}


int
win__color_open (COLOR_CONTEXT obj,
		  int r, int g, int b, unsigned bits)
{
  check_precond (obj);

  return (*obj->open) (obj, r, g, b, bits);
}

void
win__color_close (COLOR_CONTEXT obj, int col)
{

  check_precond (obj);
  
  if (obj->close)
    (*obj->close) (obj, col);
}

void
win__color_redefer (COLOR_CONTEXT obj, int pixel)
{
  check_precond (obj);
  if (obj->redefer)
    (*obj->redefer) (obj, pixel);
}

void
win__color_redefer2 (COLOR_CONTEXT obj, int pixel, long *ignore_pens, int ip_size)
{
  check_precond (obj);
  if (obj->redefer2)
    (*obj->redefer2) (obj, pixel, ignore_pens, ip_size);
}

bool
AITcolor_alloc (Display *dpy, XColor *color)
{
  int col;			/* RESULT */

  check_precond (dpy && DISPLAY_COLCON (dpy));
  check_precond (color);

#if 0
  {
    ULONG secs, micros;
    long prev_last_time;

    prev_last_time = last_alloc_time;
    CurrentTime (&secs, &micros);
    last_alloc_time = secs * 1000 + micros / 1000;
    if (last_alloc_time > prev_last_time + 1000)
      
  }
#endif

  col = win__color_open (DISPLAY_COLCON (dpy),
			color->red , color->green, color->blue,
			16);   /* 16 bit RGB */

  color->pixel = col;		/* ??? */

  if (col < 0)
    return false;

  color->pixel = col;
  return true;
}

int
AITcolor_free (Display *dpy, int col)
{

  check_precond (dpy && DISPLAY_COLCON (dpy));

  win__color_close (DISPLAY_COLCON (dpy), col);
  return 0;
}


/* Color name => RGB mapping */
struct color_name_cell
{
#define MAXLEN_COLOR_NAME 32
  char name[MAXLEN_COLOR_NAME + 1];
  unsigned char red, green, blue; 
#define CNC_INITIALIZER(R, G, B, NAME) {(NAME), (R), (G), (B)}
};

static inline int
Awcol_color_name_cell_cmp (void *a, void *b)
{
  return strcasecmp (((struct color_name_cell *)a)->name,
		     ((struct color_name_cell *)a)->name);
}

static inline void
Awcol_color_name_cell_qsort (struct color_name_cell *array,
			    size_t nmemb)
{ 
  qsort (array, nmemb, sizeof *array, (void*)strcasecmp);
}

/* static inline */ struct color_name_cell *
Awcol_color_name_cell_find (struct color_name_cell *array,
			    size_t nmemb, const char *name)
{ 
  return bsearch (name, array, nmemb, sizeof *array, (void*)strcasecmp);
}

 /* additionally user defined color names */
struct color_name_user_map
{
  size_t size, upper_bound;
  struct color_name_user_map *next;
  struct color_name_cell tbl[0];
} *win_color_name_user_tbl;

static struct color_name_cell win_color_name_tbl[];
#include "amiga_window_color_data.c"

bool
win__color_query (const char *name, XColor *color)
{
  struct color_name_cell *tbl, *result;
  struct color_name_user_map *usr_tbl;

  check_precond (name);

  for (usr_tbl = win_color_name_user_tbl;;
       usr_tbl = usr_tbl->next)
    {
#if 1
      if (usr_tbl)
	result = Awcol_color_name_cell_find (usr_tbl->tbl, usr_tbl->upper_bound, name);
      else
	result = Awcol_color_name_cell_find (win_color_name_tbl,
					     sizeof win_color_name_tbl 
					     / sizeof *win_color_name_tbl,
					     name);
      if (!result)
	goto next;
      if (color)
	{
	  color->red = result->red * 256;
	  color->green = result->green * 256;
	  color->blue = result->blue * 256;
	  color->pixel = -1;
	}
      return true;
#else
      for (tbl = (usr_tbl) ? usr_tbl->tbl : win_color_name_tbl;
	   tbl->name[0]; ++tbl)
	{
	  if (0!=strcasecmp (name, tbl->name))
	    continue;
	  if (color)
	    {
	      color->red = tbl->red * 256;
	      color->green = tbl->green * 256;
	      color->blue = tbl->blue * 256;
	      color->pixel = -1;
	    }
	  return true;
	}
#endif
    next:
      if (!usr_tbl)
	break;
    }
  return false;
}

bool
AITcolor_query (const char *name, XColor *color)
{
  check_precond (name);

  if (strncasecmp (name, "rgb:", 4)==0)
    {
      return false;		/* TODO-bw/03-Jun-98: IMPLEMENTME */
    }
  
  return win__color_query (name, color);
}

static struct color_name_user_map *
Awcol_rgbtxt_to_table (FILE *stream, struct color_name_user_map *map)
{
  int idx=0, size=0;
  char buf[512];
  int red, green, blue;
  int num;
#ifndef NOT_UNIQ 
  XColor color;
#endif

  check_precond (stream);

  if (map)
    {
      idx = map->upper_bound;
      size = map->size;
    }

  while (fgets (buf, sizeof (buf), stream)) 
    if (sscanf (buf, "%u %u %u %n", &red, &green, &blue, &num) != 3)
      continue;
    else
      {
	int i;
	char *name = buf + num;
	/* strip trailing newline and spaces */
	for (i = strlen (name); --i >= 0; )
	  if (isspace ((unsigned char)name[i]))
	    name[i] = '\0';
	  else
	    break;

	if (!*name)
	  continue;

#ifndef NO_UNIQ
	if (AITcolor_query (name, &color)
	    && color.red == red * 256
	    && color.green == green * 256
	    && color.blue == blue * 256)
	  continue;
#endif

	/* enlarge table */
	if (idx >= size)
	  {
	    void *p = realloc (map, sizeof *map 
			       + ((size+= 200)+1) * sizeof *map->tbl);
	    if (!p)
	      {
		free (map);
		return 0;
	      }
	    map = p;
	    map->size = size;
	    if (idx == 0)
	      map->next = 0;
	  }

	/* copy values */
	strncpy (map->tbl[idx].name, name, MAXLEN_COLOR_NAME);
	map->tbl[idx].red = red;
	map->tbl[idx].green = green;
	map->tbl[idx].blue = blue;
	++idx;
      }
  if (map)
    {
      *map->tbl[idx].name = '\0';
      map->upper_bound = idx;
      Awcol_color_name_cell_qsort (map->tbl, map->upper_bound); /* XXX-bw/03-Jun-98 */
    }
  return map;
}

void
AITrgb_txt_clear ()
{
  struct color_name_user_map *usr_tbl = win_color_name_user_tbl;
  while (usr_tbl) 
    {
      void *p = usr_tbl;
      usr_tbl = usr_tbl->next;
      free (p);
    }
}

bool
AITrgb_txt_prepend (const char *rgb)
{
  struct color_name_user_map *map=0;
  FILE *stream;

  check_precond (rgb);

  if ((stream = fopen (rgb, "r")))
    {
      if ((map = Awcol_rgbtxt_to_table (stream, win_color_name_user_tbl)))
	{
#if 0 /* We use now a single growing vector table */
	  map->next = win_color_name_user_tbl;
#endif
	  win_color_name_user_tbl = map;
	}
      fclose (stream);
    }
  return map != 0;
}

void
Awcol_exit_module ()
{
  AITrgb_txt_clear ();
}

bool
Awcol_init_module ()
{
  /* sort color table */
  Awcol_color_name_cell_qsort (win_color_name_tbl, 
			       sizeof win_color_name_tbl 
			       / sizeof *win_color_name_tbl - 1);

  if (atexit (Awcol_exit_module) < 0)
    {
      Awcol_exit_module ();
      return false;
    }
  return true;
}


#ifdef TEST
struct GfxBase *GfxBase;
struct IntuitionBase *IntuitionBase;

static int
db_n_used (COLOR_CONTEXT obj)
{
  assert (obj && obj->u.v39);
  return obj->u.v39->n_used;
}

static int
db_n_cells (COLOR_CONTEXT obj)
{
  assert (obj && obj->u.v39);
  return obj->u.v39->n_cells;
}

static int
db_first_free (COLOR_CONTEXT obj)
{
  assert (obj && obj->u.v39);
  return obj->u.v39->first_free;
}

Display dpy;
XColor xcol = { 11111, 22222, 33333, 16 };

int
main ()
{
#define N 30
  int pix[N] = {0,};
  int i;
  COLOR_CONTEXT clone;
  
  assert (IntuitionBase && GfxBase);

  dpy.screen = LockPubScreen (0);
  UnlockPubScreen (0, dpy.screen);

  dpy.color_context = win_color_context_create (dpy.screen);

  assert (dpy.color_context);

  for (i=0; i < N; ++i)
    {
      assert (AITcolor_alloc (&dpy, &xcol));
      pix[i] = xcol.pixel;
      assert (db_n_used (dpy.color_context) == 1);
    }

  /* Shared pixels, because of the same color in XCOL.  */
  for (i=0; i < N-1; ++i)
    assert (pix[i] == pix[i+1]);
  

  assert (db_n_cells (dpy.color_context) == 20); /* IMPL */

  for (i=N; --i >= 0;)
    {
      assert (db_n_used (dpy.color_context) == 1);
      AITcolor_free (&dpy, pix[i]);
    }

  assert (db_n_used (dpy.color_context) == 0);

  for (i=0; i < N; ++i)
    {
      xcol.red -=1000;
      assert (AITcolor_alloc (&dpy, &xcol));
      pix[i] = xcol.pixel;
      assert (db_n_used (dpy.color_context) == i+1);
      assert (db_n_cells (dpy.color_context) > i);
    }

  /* No shared pixels, because of the different color in XCOL.  */
  for (i=0; i < N-1; ++i)
    assert (pix[i] != pix[i+1]);

  assert (db_n_cells (dpy.color_context) == 40); /* IMPL */

  clone = win_color_context_clone (DISPLAY_COLCON (&dpy), DISPLAY_SCREEN (&dpy));
  assert (clone);
  assert (db_n_cells (clone) == 40); /* IMPL */
  assert (db_n_used (clone) == db_n_used (dpy.color_context));

  for (i=N; --i >= 0;)
    {
      assert (db_n_used (dpy.color_context) == i+1);
      AITcolor_free (&dpy, pix[i]);
      assert (db_n_used (dpy.color_context) == i);
    }

  assert (db_n_cells (dpy.color_context) == 40); /* IMPL */

  clone = win_color_context_clone (DISPLAY_COLCON (&dpy), DISPLAY_SCREEN (&dpy));
  assert (clone);
  assert (db_n_cells (clone) == 40); /* IMPL */

  return 0;
}
#endif /* TEST */

/*
   Local Variables:
   compile-command: "gcc -DTEST -g -Wall amiga_window_color.c -lauto"
   process-connection-type: nil
   End:
 */
