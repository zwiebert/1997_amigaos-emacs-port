/* AmigaOS specific dump code.  Implements map_in_data(),
   map_out_data().  */

/* This code dumps/undumps parts of the process data segments and the
   dynamic allocated memory to a file.
   
   How map_out_data() works:
   =========================

   Before dumping, pointer objects are patched in place to a
   relocatible representation (segment_id|segment_offset).

   After the dump file is written the pointers will unpatched to its
   original values.  (You can set the environment variable
   EMACS_ADMP_VERIFY to enable comparison of the unpatched segments
   against the original.  This is for debugging purposes.)
 
   How map_in_data() works:
   ========================

   The dumpfile will written to the segments.  Both the malloc and
   pure storage are restored first according to the sizes/structures
   saved in the dumpfile.  Then the patched (relocatible) pointer
   values are unpatched in place.

   Note: Its needed to avoid touching any pointer which refers outside
   to the process or even to the stack segment.

   Scanning for pointer objects:
   =============================

   We must know wich parts of raw storage are used by non NULL pointer
   objects before map_out_data/map_in_data is called.  As mentioned
   above, some pointer of those are patched/unpatched (see
   amiga_dump_pl.c) and the other one are untouched by map_in_data()
   (see check_ignore() and the `Admp_tbl' macro in s/amigaos.h).

   To scan for pointers we have the function check_cands().  It
   creates a file "src/amiga_dump.cands" in your build directory.  Use
   "nm(1)" to check all offsets in this file (manually).  Note: The
   function check_cand_ignore() need to know the start and end+1
   address of FastRam on your computer. (FIXME-bw: add defines?)

   -bw/30-Aug-97 */


/* 
   Making Updates:
   handle new pointers (LISP and C) in the following structs:
    - struct frame (frame.h)
    - (maybe struct buffer, but it's partly a (selfdescribing) LISP vector)

    - search for string "UPDATEME" in this file
 */


/* MOVEME */
#define TEXT_SIZE (TEXT_END - TEXT_START)
#define DATA_SIZE (DATA_END - DATA_START)
#define BSS_SIZE (BSS_END - BSS_START)
#define PURE_SIZE (pureptr)


#include "config.h"

/* this conditional contains the rest of the file */
#if defined (AMIGA_DUMP) || defined (TEST)

#include "lisp.h"
#ifdef emacs
#include "window.h"
#include "keyboard.h"
#include "regex.h"
#include "dispextern.h"
#include "termchar.h"
#include "paths.h"
#include "frame.h"
#include "buffer.h"
#if 0
#include <sys/types.h>
#include "sysselect.h"
#endif /* 0 */
#ifdef AMIGA_V_EMACS_20_2
#include "syntax.h" /* for gl_state_s */

#include "charset.h" /* \ */
#include "ccl.h"     /*  | */
#include "coding.h" /* for <struct coding_system> and external objects */

#endif /* 20.2 */
#else
#undef static
#endif

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <setjmp.h>
#include <unistd.h>

#include "amiga.h"
#include "amiga_defs.h"
#include "amiga_smalloc.h"

#undef LONGBITS
#include <exec/types.h>
#include <proto/exec.h>

#ifdef TEST
#define Aerr_print_msg(args...) fprintf (stderr, ## args)
#define local_precond assert
#define check_precond assert
#endif

#define db_trace() fprintf (stderr, "%s:%d: trace\n", __FILE__, __LINE__)

/* a few generic helper macros */
#define min(A,B) ((A)<(B) ? (A) : (B))
#define max(A,B) ((A)>(B) ? (A) : (B))
#define PTR_MIN(A,B) ((char *)(A) < (char *)(B) ? (A) : (B))
#define PTR_MAX(A,B) ((char *)(A) > (char *)(B) ? (A) : (B))
#define PNTR_CMP(lhs, op, rhs) ((size_t)(lhs) op (size_t)(rhs))
/* Test wether A is located in storage [S,E) */
#define RANGE(a, s, e) (((char *)(a)) >= ((char *)(s)) && ((char *)(a)) < ((char *)(e)))
/* Calc byte difference between two storage addresses A and B. */
#define PTR_DIFF(a,b) (((char*)(a))-((char*)(b)))

#ifndef TEST

/* Assertions: check_postcond() should usual disabled. check_precond()
   shoud enabled if the caller is not trustworthy. */
#define NO_PRECOND 0
#define NO_POSTCOND 1
#include "amiga_defs.h"
#undef XPNTR
#define XPNTR(obj) A_FAST_XPNTR(obj)


 /* code to speed up offset<=>address mapping for the list based malloc hunk. */
static inline int
Admp_oa_map_cmp_addr (const void *a, const void *b)
{
  unsigned long key = *(unsigned long *)a;
  struct offs_addr_map_tbl const*amemb = b;
  if (key < amemb->offs)
    return -1;
  else if (amemb->offs + amemb->size <= key)
    return 1;
  else
    return 0;
}

static inline void *
Admp_oa_map_bsearch(const void *key, const void *base0, 
	size_t nmemb, size_t size, int (*dummy)())
{
	register char *base = (char *)base0;
	register int lim, cmp;
	register void *p;

	for (lim = nmemb; lim != 0; lim >>= 1) {
		p = base + (lim >> 1) * size;
#if 0
		cmp = (*compar)(key, p);
#else
		/* this allows inlining the compare (with GCC-2.7.2)
                   -bw/06-Jun-98 */
		cmp = Admp_oa_map_cmp_addr (key, p);
#endif		
		if (cmp == 0)
			return (p);
		if (cmp > 0) {	/* key > p: move right */
			base = (char *)p + size;
			lim--;
		} /* else move left */
	}
	return (NULL);
}

static inline void *
Admp_oa_map_locate_offset_bs (struct offs_addr_map *oa_map, long offset)
{
  struct offs_addr_map_tbl *amemb 
    = Admp_oa_map_bsearch (&offset, oa_map->kv_tbl, oa_map->nmemb,
			   sizeof *oa_map->kv_tbl, Admp_oa_map_cmp_addr);
  if (!amemb)
    return 0;
  else
    return (char *)amemb->addr + (offset - amemb->offs);
}

static inline void *
Admp_oa_map_locate_offset (struct offs_addr_map *oa_map, unsigned long offset)
{
  int i;
  signed long offs, curr_offs;

  for (i=0, offs = offset; i < oa_map->nmemb; ++i)
    {
      curr_offs = offs;
      offs -= oa_map->kv_tbl[i].size;
      if (offs < 0)
	return (char*)oa_map->kv_tbl[i].addr + curr_offs;
    }
  return 0;
}

static inline long
Admp_oa_map_locate_address (struct offs_addr_map *oa_map, const void *m)
{
  int i;
  for (i=0; i < oa_map->nmemb; ++i)
    if (RANGE (m, oa_map->kv_tbl[i].addr,
	       ((char *)oa_map->kv_tbl[i].addr
		+ oa_map->kv_tbl[i].size)))
      return (((char *)m - (char*)oa_map->kv_tbl[i].addr)
	      + oa_map->kv_tbl[i].offs);

  return 0; /* Note: its not -1 */
}


static struct
  {
    /* cache to speed up sequential calls to check_ignore() */
    struct {
      const void *start, *end, *tmp;
      bool meaning;		/* if true then ignore */
    } ignore_cache;
    bool initialized;		/* module status */
    bool invalid_pointers;      /* turn invalid (free()'d) pointers to NULL */
    int mode;			/* global switch: patch=0, unpatch=1 */
    void *(*patch_cptr) (void *, void **); /* fix/reloc machine pointer */
    int (*patch_lptr) (int, int *); /* fix/reloc lisp pointer */
    /* Count pointer patching/unpatching for debugging. */
    int ptr_count2;
    const char *data_file_name;
    struct offs_addr_map *oa_map;
  }
a_dmp;

/* Count pointer patching/unpatching for debugging + sanity check. */
static int Admp_ptr_count[8];

/* ID tags for dump file */
#define ADMP_MAGIC_COOKIE "Emacs data dump file for Emacs-" \
AMIGA_VERSION_SUFFIX " (" EMACS_CONFIGURATION ")\n"
#define ADMP_EOF_COOKIE "\nend of file\n"

#define DBUG 0

/* Pointers are patched before dumping.  The patch code finds the
   containing hunk for the address to which the pointer refer.  Then
   it converts the absolute machine address to an offset to its
   belonging hunk/segment and write this offset and the number of the
   hunk/segment back to the pointer.

   After patching all pointers, we dump the storage as raw memory.
   Note that some areas are not dumped, because its contents points to
   system resources unique to that process. (see check_ignore())..

  After dumping its done, all patching must undone.  The unpatching
  code extracts the offset and the hunk/segment number from a patched
  pointer.  Then the offset is added to the start address of the
  hunk/segment and the result is written back to the pointer. */

/* === config === */

/* relative pointer. */
struct rel_ptr
{
  int type: GCTYPEBITS+1;
  int hunk_id: 3;
  int offset: VALBITS - 3;
};

/* this table holds the adresses of all monolithic hunks. (e.g. all
   except MALLOC_HUNK) */
#define SIZE_ADMP_HUNK_TABLE 5
struct {
  void *start, *end;
  const char *name;
} Admp_hunk_table [8];
#define INIT_HUNK_TABLE()							\
Admp_hunk_table[0].start = 0; Admp_hunk_table[0].end = 0;			\
Admp_hunk_table[1].start = TEXT_START; Admp_hunk_table[1].end = TEXT_END;	\
Admp_hunk_table[1].name = "text";						\
Admp_hunk_table[2].start = DATA_START; Admp_hunk_table[2].end = DATA_END;	\
Admp_hunk_table[2].name = "data";						\
Admp_hunk_table[3].start = BSS_START; Admp_hunk_table[3].end = BSS_END;		\
Admp_hunk_table[3].name = "bss";						\
Admp_hunk_table[4].start = PURE_START; Admp_hunk_table[4].end = PURE_END;	\
Admp_hunk_table[4].name = "pure";						\
Admp_hunk_table[7].name = "malloc";

/* ID bits to encoding the belonging hunk/segment of an address offset */
/* Position of ID bits inside VALBITS */
#define HUNK_POS (VALBITS - 3)
/* Bitmask for ID bits */
#define HUNK_MASK (7 << HUNK_POS)
#define HUNK_TEXT (1 << HUNK_POS)
#define HUNK_DATA (2 << HUNK_POS)
#define HUNK_BSS (3 << HUNK_POS)
#define HUNK_PURE (4 << HUNK_POS)
#define HUNK_MALLOC (7 << HUNK_POS)

#define ARRAY_MARK_FLAG ((MARKBIT >> 1) & ~MARKBIT)

#define ADMP_MODE_MAP_OUT 0
#define ADMP_MODE_MAP_IN 1

/* === data === */

/* Used pure storage (alloc.c) */
extern int pureptr;
#ifndef DONT_COPY_FLAG
#define DONT_COPY_FLAG 1
#endif /* no DONT_COPY_FLAG  */

/* === code === */
/**************import interface***********************************/
extern void do_apply_cptr_list (void *(*)(void *, void **),
				void (*)(void **ptr, int offset));
extern void do_apply_lptr_list (int (*)(int, int *));
/**************export interface***********************************/
bool Admp_init_module (void);
void map_out_data (char *fn);
void map_in_data (int load);
/**************local interface************************************/
/*** error handling ***/
static void bailout (char const *data_path, char const *msg);
static void Aerr_data_file (char const *data_path, char const *msg);

/*** working code ***/

/** pointer relocation **/
static void patch (int mode);
static void Admp_check_pointer_count (void);
static void patch_buffer (struct buffer *buf);
static void patch_keyboard (struct kboard *kb);
//static void patch_pointers (Lisp_Object * objptr) __attribute__ ((stackext));
static void patch_pointers (Lisp_Object * objptr);
static void patch_chain (void **ptr, int offset);

static void *hunk_pointer (void *ptr, void **fix);
static void *make_pointer (void *ptr, void **fix);
static int hunk_lispptr (Lisp_Object a, Lisp_Object *fix);
static Lisp_Object make_lispptr (Lisp_Object obj, Lisp_Object *fix);

static int Admp_locate_address (void *m, int *offset);

/** swapping process<=>datadump **/
static int dump (int mode, char const *path);
static bool restore_hunk (void *begin, const void *end, FILE * is);
static bool save_hunk_conditional (const void *begin, const void *end,
				   bool (*ign) (const void *, const void **), FILE * os);

/** exclusion of process specific data  **/
static bool check_ignore (const void *x, const void ** next_change_result);
static const void *ignore2 (const void *p);

/*** debugging aids ***/
static void check_cands (void);
static void print_ranges (void);
void MInit (char *s);
void MClean (void);
void MEnter (char *s);
void M (char *s);
void MLeave (char *s);
/*****************************************************************/


#define FIX_TO_RELOC_P() (a_dmp.mode == ADMP_MODE_MAP_OUT)
#define CONV_CPTR(ptr) ((void*)(*a_dmp.patch_cptr) (ptr, 0))
#define CONV_LPTR(ptr) ((int)(*a_dmp.patch_lptr) (ptr, 0))

#ifndef ADMP_NO_PATCH		/* Choose real- or debug versions */

#define PATCH_CPTR(ptr) ((void)((ptr)= (ptr) ? CONV_CPTR (ptr) : 0))
#define PATCH_LPTR(ptr) ((void)((ptr)= (ptr) ? CONV_LPTR (ptr) : 0))
#define PATCH_OBJ(objptr) ((void)patch_pointers(objptr))

#define PATCH_CPTR_SET(ptr,fix) \
((ptr) = (*a_dmp.patch_cptr) ((ptr), ((void**)&(fix))))

#define PATCH_LPTR_SET(ptr,fix) \
((ptr) = (*a_dmp.patch_lptr) ((ptr), (&(fix))))

#else
/* Debug versions of PATCH_xxx macros.  The arg will evaluted (the
   preconditions are checked) but there is no assign.  */
#define PATCH_CPTR(ptr) ((void)((ptr) ? CONV_CPTR (ptr) : 0))
#define PATCH_LPTR(ptr) ((void)((ptr) ? CONV_LPTR (ptr) : 0))
#define PATCH_OBJ(objptr) ((void)patch_pointers(objptr))

#define PATCH_CPTR_SET(ptr,fix) \
({ 						\
	 (*a_dmp.patch_cptr) (ptr, 0);	        \
	 (fix)=(ptr);				\
})

#define PATCH_LPTR_SET(ptr,fix) \
({ 						\
	 (*a_dmp.patch_lptr) (ptr, 0);	        \
	 (fix)=(ptr);				\
})

#endif

#define ADMP_XMARK(a) \
((void) ({ if (FIX_TO_RELOC_P()) XMARK(a); else XUNMARK(a); }))
#define ADMP_XUNMARK(a) \
((void) ({ if (FIX_TO_RELOC_P()) XUNMARK(a); else XMARK(a); }))
#define ADMP_XMARKBIT(a) \
((FIX_TO_RELOC_P()) ? XMARKBIT(a) : !XMARKBIT(a))



/** Section: relocating pointers. */


/* Convert lisp address format to <gctypebits|hunk_id|hunk_offset>
   packed in the integer result. */

static int
hunk_lispptr (Lisp_Object a, Lisp_Object *fix)
{
  void *abs_addr;
  int gctype, hunk_comp_addr;

  abs_addr = (void *) XPNTR (a);
  gctype = a & ~VALMASK;
  hunk_comp_addr = (int) hunk_pointer (abs_addr, 0);
  if (fix)
    *fix = a;

#ifndef ADMP_NO_PATCH
  a =  gctype | hunk_comp_addr;
#endif

  return a;
}

/* alloc.c */
extern int *pure, puresize;
extern struct gcpro *gcprolist;
extern Lisp_Object *staticvec[];
extern int staticidx;
extern struct cons_block *cons_block;
extern struct Lisp_Cons *cons_free_list;
extern struct Lisp_Vector *all_vectors;
extern struct symbol_block *symbol_block;
extern struct Lisp_Symbol *symbol_free_list;
extern struct marker_block *marker_block;
extern INTERVAL interval_free_list;

struct string_block_head
  {
    struct string_block_head *next, *prev;
    int pos;
  };

struct string_block
  {
    struct string_block *next, *prev;
#if 0				/* not needed */
    int pos;
    char chars[STRING_BLOCK_SIZE];
#endif
  };

extern struct string_block *current_string_block;
extern struct string_block *first_string_block;
extern struct string_block *large_string_blocks;
#ifdef LISP_FLOAT_TYPE
extern struct float_block *float_block;
extern struct Lisp_Float *float_free_list;
#endif /* LISP_FLOAT_TYPE */

extern struct backtrace *backtrace_list;
extern struct catchtag *catchlist;
extern char *stack_copy;

extern int *kbd_macro_buffer;
extern char *read_buffer, *chars_wasted, *copybuf;

struct regexp_cache {
  struct regexp_cache *next;
  Lisp_Object regexp;
  struct re_pattern_buffer buf;
  char fastmap[0400];
  /* Nonzero means regexp was compiled to do full POSIX backtracking.  */
  char posix;
};
extern struct regexp_cache *searchbuf_head;
#if 0				/* CHFIXME */
extern int *ILcost, *DLcost, *ILncost, *DLncost;
#endif
#if 0
extern Lisp_Object MouseMap, global_map, Vglobal_map, Vesc_map, Vctl_x_map;
#else
extern Lisp_Object global_map, meta_map, control_x_map;
#endif
extern Lisp_Object selected_window;

/* buffer.c */
/* -> buffer.h */

/* regex variables */
typedef unsigned char *fail_stack_elt_t;
typedef struct
  {
    fail_stack_elt_t *stack;
    unsigned size;
    unsigned avail;		/* Offset of next open position.  */
  }
fail_stack_type;
typedef short register_info_type;

extern fail_stack_type fail_stack;
extern const char **regstart, **regend;
extern const char **old_regstart, **old_regend;
extern const char **best_regstart, **best_regend;
extern register_info_type *reg_info;
extern const char **reg_dummy;
extern void *reg_info_dummy;

extern struct input_event *kbd_fetch_ptr;
extern struct input_event volatile *kbd_store_ptr;

/* search.c */
extern struct re_registers search_regs;


/** Section: reading/writing hunks */

typedef struct dumped_hunk sdh;
struct dumped_hunk
{
  int cookie;
  char name [10];
  size_t size;			/* size of data */
  size_t offset;		/* offset of hunk start relative to segment start */
  void *startp;
  void *endp;
  bool (*ign) (const void *, const void **);   /* If not 0 use it to ask before dump any byte */
};

#define SDH_COOKIE 0x07041965
#define ADMP_NMB_HUNKS 3
sdh Admp_hunks[ADMP_NMB_HUNKS] = 
{
  {SDH_COOKIE, "data", 0, 0, DATA_FIRST, DATA_LAST, check_ignore},
  {SDH_COOKIE, "bss", 0, 0, BSS_FIRST, BSS_LAST, check_ignore},
  {SDH_COOKIE, "pure", },
};

/* Initializes parts of Admp_hunks with runtime data.  */
/* -bw/12-Sep-97 */
void
Admp_hunks_init ()
{
  check_precond (pure);
  check_precond (puresize);
  /* pure */
  Admp_hunks[2].startp = pure;
  Admp_hunks[2].endp = (char *)pure + puresize;
}

/* Dump or undump a single hunk.  */
/* MODE==0 means dump-mode.  HUNK must at least contain addresses of
   start/end.  STREAM must be in output mode if in dump-mode,
   otherwise it must in input mode. -bw/12-Sep-97 */
/* Results: 0 for success; -1 for stdio-error. 1 for unmatched data
   error */
static int
Admp_dump_hunk (int mode, sdh *hunk, FILE *stream)
{
  if (mode == ADMP_MODE_MAP_OUT)
    {
      if (fwrite (hunk, sizeof *hunk, 1, stream) != 1)
	return -1;
      if (!save_hunk_conditional (hunk->startp, hunk->endp, hunk->ign, stream))
	return -1;
    }
  else				/* mode == ADMP_MODE_MAP_IN */
    {
      sdh saved_hunk;
      if ((fread (&saved_hunk, sizeof *hunk, 1, stream) != 1))
	return -1;
      if (memcmp (hunk, &saved_hunk, offsetof (sdh, startp)) != 0)
	return 1;
      if (((char*) saved_hunk.endp - (char*)saved_hunk.startp)
	  != ((char*) hunk->endp - (char*)hunk->startp))
	return 1;
      if (!restore_hunk (hunk->startp, hunk->endp, stream))
	return 1;
    }
  return 0;
}

/* Dump or undump hunks.  */
/* Do dump if MODE==0, else do undump. If dumping then STREAM must
   point to an output stream.  If undumping its have to be an input
   stream.  -bw/12-Sep-97 */
static int
Admp_dump (int mode, FILE *stream)
{
  int i;
  int error;

  Admp_hunks_init ();
  for (i=0; i < ADMP_NMB_HUNKS; ++i)
    if ((error = Admp_dump_hunk (mode, &Admp_hunks[i], stream)) != 0)
      return error;

  if (! ((mode==ADMP_MODE_MAP_OUT) 
	 ? Asma_save_malloc (stream)
	 : Asma_restore_malloc (stream)))
    return -1;

  return 0;
}

/* write or read object */
static int
Admp_dump_obj (int mode, void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  return ((mode == ADMP_MODE_MAP_OUT)
	  ? fwrite (ptr, size, nmemb, stream)
	  : fread (ptr, size, nmemb, stream));
}



/* Replacement for the FAR-stuff of the SAS-C EMACS
   port. -bw,13-Jun-97 */

/* There are macros on the foot of some module source files. They
   produce global arrays, which points to storage areas which should
   not dumped/undumped.  If you have a machine address, pass it to
   check_ignore() and its result tells you wether this data should be
   dumped/undumped.  */
static int Admp_action_nd (struct Admp_obj *obj, void *argp);

int
Admp_tbl_dispatch (int mask,
	  int (*action)(struct Admp_obj *obj, void *argp), void *argp,
	  struct Admp_obj *tbls[])
{
  int i,k;

  for (i=0; tbls[i]; ++i)
    for (k=0; (tbls[i])[k].key != ADMP_KEY_TERMINATE; ++k)
      if (mask & (tbls[i])[k].key)
	{
	  int result;
	  if (Admp_action_nd != action) 
	    if (getenv ("EMACS_DEV"))
	      fprintf (stderr, "emacs-debug: %s: table %d: item %d\n",
		       __PRETTY_FUNCTION__, i, k);
	  result = (*action) (&(tbls[i])[k], argp);
	  if (result == 0)
	    continue;
	  else
	    return result;
	}
  return 0;
}


  extern struct Admp_obj Admp_tbl__xfns[];
  extern struct Admp_obj Admp_tbl__xfaces[];
  extern struct Admp_obj Admp_tbl__xmenu[];
  extern struct Admp_obj Admp_tbl__xterm[];
  extern struct Admp_obj Admp_tbl__amiga_rexx[];
  extern struct Admp_obj Admp_tbl__amiga_clipboard[];
  extern struct Admp_obj Admp_tbl__amiga_screen[];
  extern struct Admp_obj Admp_tbl__amiga_serial[];
  extern struct Admp_obj Admp_tbl__amiga_dump[];
  extern struct Admp_obj Admp_tbl__amiga_malloc[];
  extern struct Admp_obj Admp_tbl__amiga_sysdep[];
  extern struct Admp_obj Admp_tbl__amiga_window_select[];
  extern struct Admp_obj Admp_tbl__alloc[];
  extern struct Admp_obj Admp_tbl__emacs[];
  extern struct Admp_obj Admp_tbl__eval[];
#ifdef AMIGA_V_EMACS_20_2
  extern struct Admp_obj Admp_tbl__callint[];
  extern struct Admp_obj Admp_tbl__coding[];
#endif /* 20.2 */
  extern struct Admp_obj Admp_tbl__keyboard[];
  extern struct Admp_obj Admp_tbl__lread[];
  extern struct Admp_obj Admp_tbl__regex[];
  extern struct Admp_obj Admp_tbl__syntax[];
  extern struct Admp_obj Admp_tbl__term[];
  extern struct Admp_obj Admp_tbl__print[];
  extern struct Admp_obj Admp_tbl__process[];
  extern struct Admp_obj Admp_tbl__window[];

struct Admp_obj *Admp_tables[] = {
#ifdef AMIGA_REXX
  Admp_tbl__amiga_rexx,
#endif
#ifdef HAVE_FACES
  Admp_tbl__xfns,
  Admp_tbl__xfaces,
#endif
#ifdef HAVE_WINDOW_SYSTEM
  Admp_tbl__xterm,
#ifdef HAVE_MENUS
  Admp_tbl__xmenu,
#endif
#endif
  Admp_tbl__amiga_dump,
  Admp_tbl__amiga_malloc,
  Admp_tbl__amiga_sysdep,
#ifdef AMIGA_IWIN_CLIPBOARD
  Admp_tbl__amiga_window_select,
#endif /* AMIGA_IWIN_CLIPBOARD */

  Admp_tbl__alloc,
  Admp_tbl__emacs,
  Admp_tbl__eval,
#ifdef AMIGA_V_EMACS_20_2
  Admp_tbl__callint,
  Admp_tbl__coding,
#endif /* 20.2 */
  Admp_tbl__keyboard,
  Admp_tbl__lread,
  Admp_tbl__regex,
  Admp_tbl__syntax,
  Admp_tbl__term,
  Admp_tbl__print,
  Admp_tbl__process,
  Admp_tbl__window,
  0};


/* Patch Lisp_Object in table item.  */
static int
Admp_action_lobj (struct Admp_obj *obj, void *argp)
{
  Lisp_Object *lobj;

  local_precond (obj && obj->key == ADMP_KEY_LOBJ);

  lobj = (Lisp_Object *) obj->arg1;
  PATCH_OBJ (lobj);

  return 0;
}

/* Patch Lisp_Object[] in table item.  */
static int
Admp_action_lobj_vec (struct Admp_obj *obj, void *argp)
{
  Lisp_Object *vec_start;
  int vec_size;
  int i;

  local_precond (obj && obj->key == ADMP_KEY_LOBJ_VEC);

  (void)argp;
  vec_start = (Lisp_Object *)obj->arg1;
  vec_size = obj->arg2;

  for (i=0; i < vec_size; ++i)
    PATCH_OBJ (&vec_start[i]);

  return 0;
}

/* Locate address in table item */
static int
Admp_action_nd (struct Admp_obj *obj, void *argp)
{
  void *ign_start, *ign_end;

  local_precond (obj && obj->key == ADMP_KEY_ND);

  ign_start = (char*)obj->arg1;
  ign_end = (char*)obj->arg2;

  if (RANGE (argp, ign_start, ign_end))
    return (int) ign_end;

  /* update ignore_cache_tmp */
  if ((char *)argp < (char *)ign_start
      && ((char *)ign_start < (char *)a_dmp.ignore_cache.tmp
	  || a_dmp.ignore_cache.tmp == 0))
    a_dmp.ignore_cache.tmp = ign_start;
      
  return 0;
}

/* Locate address in tables */
/* Test whether PTR is located in a member of TBLS.  This is used to
   exclude the tables to dumped. */
static const void *
Admp_in_tbl (const void *ptr, struct Admp_obj *tbls[])
{
  int i, k;
  
  for (i=0; tbls[i]; ++i)
    {
      void *startp, *endp;
      startp = &(tbls[i])[0];
      for (k=0; (tbls[i])[k].key != ADMP_KEY_TERMINATE; ++k)
	;
      endp = tbls[i] + (k+1);
      if (RANGE (ptr, startp, endp))
	return endp;
      /* update ignore_cache.tmp */
      if ((char *)ptr < (char *)startp
	  && ((char *)startp < (char *)a_dmp.ignore_cache.tmp
	      || a_dmp.ignore_cache.tmp == 0))
	a_dmp.ignore_cache.tmp = startp;
    }
  return 0;
}



const void *
ignore2 (const void *p)
{
  const void *result;

  result = Admp_in_tbl (p, Admp_tables);

  if (!result)
    result = (void*)Admp_tbl_dispatch (ADMP_KEY_ND, Admp_action_nd, (void*)p, Admp_tables);

  return result;
}


/*
 * ignore:
 *      IconBase, interval_block_index (int)
 *      pending (list), returned (list)
 */

static bool
check_ignore (const void *x, const void **next_change_result)
{
  const void *end = 0;

#define IN_PTROBJ(obj) ((char*)&(obj) <= (char*)(x) \
			&& (char*)(x) < ((char*)&(obj) + 4))

  /* How the cache works:

     1. If we found that X has to be ignored, we get also the
     end-pointer of the ignoring block in which X was found.
     
     2.  If we found that X hasn't to be ignored, then we know that we
     visit all ignoring blocks.  While visiting we store the start
     address of the nextmost ignoring block in
     a_dmp.ignore_cache.end.  The value of X ist stored in
     a_dmp.ignore_cache.start.

     We use that cache to avoid unecessary calls to ignore2().
     But the caller could use it also to skip or save blocks rather than single characters.

     -bw/05-Jun-98 */

  if (!(a_dmp.ignore_cache.start && a_dmp.ignore_cache.end
	&& RANGE (x, a_dmp.ignore_cache.start, a_dmp.ignore_cache.end)))
    {
      a_dmp.ignore_cache.tmp = (void *) ~0;
      a_dmp.ignore_cache.start = x;

      end = ignore2 (x);
      if ((a_dmp.ignore_cache.meaning = (end != 0)))
	/* state: ignoring */
	a_dmp.ignore_cache.end = end;
      else
	/* state: not ignoring */
	a_dmp.ignore_cache.end = a_dmp.ignore_cache.tmp;
    }

  if (next_change_result)
    *next_change_result = a_dmp.ignore_cache.end;

  return a_dmp.ignore_cache.meaning;
}


#if  DBUG
static int mcol = 0;
static int mrow = 0;
static FILE *mfile;
static char spaces[] =
"                                                                                  "
"                                                                                  "
"                                                                                  ";
static int mtresh = 0;
void
MInit (char *s)
{
  mcol = 0;
  mrow = 0;
  mtresh = 0;
  mfile = fopen (s, "a");
  fprintf (mfile, "\n***START***\n");
}
void
MClean ()
{
  fprintf (mfile, "\n**END**\n");
  fclose (mfile);
}
#define P(x)
void
MEnter (char *s)
{
  if (mtresh > 100)
    return;
#if 0
  fwrite (spaces, mcol, 1, mfile);
#endif
  fprintf (mfile, "(%d) %s\n", mcol, s);
  mcol += 1;
}
void
M (char *s)
{
  if (mtresh > 100)
    return;
  mtresh++;
#if 0
  if (mcol)
    fwrite (spaces, mcol, 1, mfile);
#endif
  fprintf (mfile, "(%d) %s\n", mcol, s);
}
void
MLeave (char *s)
{
  if (mtresh > 100)
    return;

  mcol -= 1;
#if 0
  if (mcol)
    fwrite (spaces, mcol, 1, mfile);
#endif
  fprintf (mfile, "(%d) %s\n", mcol, s);
  fflush (mfile);

}
#else
#define MInit(x)
#define MClean()
#define MEnter(x)
#define M(x)
#define P(x)
#define MLeave(x)
#endif



/* Find hunk of address. */
/* Find hunk containing M and return its ID or -1 if M is not in a
   hunk.  If OFFSET is not NULL then the offset of M in its segment(!)
   (for text, data, bss) or its hunk (for malloc, pure) is stored
   there.  (Note: we test of containing in a hunk inside a segment,
   but returns the offset to the beginning of the whole segment.) */
static int
Admp_locate_address (void *m, int *offset)
{
  int i;
  int offs = 0;
  int id = -1;

  for (i=1; i < SIZE_ADMP_HUNK_TABLE; ++i)
    if (RANGE (m, Admp_hunk_table[i].start, Admp_hunk_table[i].end))
      {
	offs = PTR_DIFF (m, Admp_hunk_table[i].start);
	id = i<<HUNK_POS;
	goto done;
      }
  /* This hunk isn't monolithic. */
  if ((a_dmp.oa_map && (offs = Admp_oa_map_locate_address (a_dmp.oa_map, m)) > 0)
      || (offs = Asma_locate_address (m)) > 0)
    id = HUNK_MALLOC;

 done:
  if (id != -1 && offset)
    *offset = offs;
  check_postcond (id == -1 || 0 == (offs & ~((1 << HUNK_POS) - 1)));
  return id;
}

/* Find segment and offset for a given machine address */
/* [bw] The function hunk_pointer() takes the absolute address PTR and
   convterts it to an offset value of the hunk containing the address
   and a identifier of the containing hunk. Both values are packed in
   the returnvalue (Type void*). If no hunk contains PTR bailout() is
   called [/bw] If FIX is non zero then the value of ptr ist stored in
   the pointer object referred by it. */
static void *
hunk_pointer (void *ptr, void **fix)
{
  int id = 0;
  int offset = 0;

  if (fix)
    *fix = ptr;

  if (!ptr)
    return 0;			/* no need to relocate NULL. */

  id = Admp_locate_address (ptr, &offset);
  if (id != -1)
    {
      ++Admp_ptr_count [(id >> HUNK_POS)];
#ifndef ADMP_NO_PATCH
      return (void *) (id | offset);
#else
      return ptr;
#endif
    }

  /* Hack to convert pointers to free()d storage to NULL.  (Note: this
     would break address arithmetic but we have no other choice since
     we dropped the monolitic malloc hunk. -bw/22-Feb-98 */

  if (a_dmp.invalid_pointers)
    {
      if (fix)
	*fix = 0;
      return 0;
    }
  Aerr_print_msg ("### %s: cannot locate pointer %p ###\n", __PRETTY_FUNCTION__, ptr);
  print_ranges ();
  Aerr_print_backtrace (0, 100, stderr);
  bailout (0, "pointer refers to object on stack or outside of process");
  /* NOTREACHED */
  abort ();
}

/* Patch the pointers in a keyboard structure.  */
static void
#ifdef AMIGA_V_EMACS_20_2
patch_coding_system (struct coding_system *cs)
{
#if 0 /* this struct contains no pointer */
  if (cs->type == coding_type_iso2022)
    {
      struct iso2022_spec *iso2022 = &cs->spec.iso2022; 
    }
#endif
  if (cs->type == coding_type_ccl)
    {
      struct ccl_spec *ccl = &cs->spec.ccl;	/* Defined in ccl.h.  */
      PATCH_CPTR (ccl->decoder.prog);
      PATCH_CPTR (ccl->encoder.prog);
    }

  PATCH_LPTR (cs->symbol);
  PATCH_LPTR (cs->post_read_conversion);
  PATCH_LPTR (cs->pre_write_conversion);
  /* fields which are not always initialized */
  if (cs->type != coding_type_no_conversion 
      && cs->type != coding_type_emacs_mule
      && cs->type != coding_type_undecided
      && cs->type != coding_type_raw_text)
    {
#ifndef AMIGA_V_EMACS_20_2_97
      PATCH_LPTR (cs->character_unification_table_for_decode);
      PATCH_LPTR (cs->character_unification_table_for_encode);
#else /* 20.2.97 */
      PATCH_LPTR (cs->translation_table_for_decode);
      PATCH_LPTR (cs->translation_table_for_encode);
#endif /* 20.2.97 */
    }
};


/* Patch the pointers in a keyboard structure.  */
static void
#endif /* 20.2 */
patch_keyboard (struct kboard *kb)
{
  /* UPDATEME: Emacs-20.2 -bw/04-Jan-98 */
  PATCH_OBJ (&kb->Vprefix_arg);
  PATCH_OBJ (&kb->kbd_queue);
  PATCH_OBJ (&kb->Vlast_kbd_macro);
  PATCH_OBJ (&kb->Vsystem_key_alist);
  PATCH_OBJ (&kb->system_key_syms);

  PATCH_LPTR (kb->Voverriding_terminal_local_map); 
  PATCH_LPTR (kb->Vlast_command);
  PATCH_LPTR (kb->defining_kbd_macro);
  PATCH_LPTR (kb->Vdefault_minibuffer_frame);

  if (kb->kbd_macro_buffer) /* if zero both kbd_macro_ptr, ...end may uninitialized  */
    {
      PATCH_CPTR (kb->kbd_macro_buffer);
      PATCH_CPTR (kb->kbd_macro_ptr);
      PATCH_CPTR (kb->kbd_macro_end);
    }

  PATCH_CPTR (kb->echoptr);

}

/* Patch the pointers in a buffer structure.  */
static void
patch_buffer (buffer)
     register struct buffer *buffer;
{
  register Lisp_Object *ptr;
  struct buffer *base_buffer;

  { /* Sometimes last_thing_search->own_text.beg may point to free()'d
    storage -bw/22-Feb-98 */
    bool tmp = a_dmp.invalid_pointers;
    a_dmp.invalid_pointers = true;
    PATCH_CPTR (buffer->own_text.beg);
    a_dmp.invalid_pointers = tmp;
  }

  PATCH_OBJ (&buffer->own_text.markers);
  PATCH_CPTR (buffer->text);
  PATCH_CPTR_SET (buffer->base_buffer, base_buffer);

  /* FIXME-bw: what about patching struct region_cache.  */
  PATCH_CPTR (buffer->newline_cache);
  PATCH_CPTR (buffer->width_run_cache);

#ifdef AMIGA_V_EMACS_20_3
  PATCH_OBJ (&buffer->undo_list);
#endif
  /* This is the buffer's markbit */
  PATCH_OBJ (&buffer->name);
  ADMP_XMARK (buffer->name);

  for (ptr = &buffer->name + 1;
       (char *)ptr < (char *)buffer + sizeof (struct buffer);
       ptr++)
    PATCH_OBJ (ptr);

  /* If this is an indirect buffer, mark its base buffer.  */
  if (base_buffer && !ADMP_XMARKBIT (base_buffer->name))
    patch_buffer (base_buffer);
}

#define ADMP_MARK_ARRAY(p)				\
({ if (FIX_TO_RELOC_P()) 				\
     {							\
       (p)->size |= ARRAY_MARK_FLAG; /* Else mark it */	\
     }							\
   else							\
     {							\
       (p)->size &= ~ARRAY_MARK_FLAG;			\
     }							\
 })


/* Relocate pointer objects. */
/* Patch/unpatch in place.  The code is pasted from mark_object */
static void
patch_pointers (objptr)
     Lisp_Object *objptr;
{
  Lisp_Object obj;

 loop:
  local_precond (objptr);
  //  local_precond (!ADMP_XMARKBIT (*objptr));
  if (GC_INTEGERP (*objptr))
    return;

    PATCH_LPTR_SET (*objptr, obj);

 loop2:
  ADMP_XUNMARK (obj);

  switch (SWITCH_ENUM_CAST (XGCTYPE (obj)))
    {
    case Lisp_String:
      {
	register struct Lisp_String *ptr = XSTRING (obj);

	//	MARK_INTERVAL_TREE (ptr->intervals);
	if (ptr->size & MARKBIT) /* Admp: will never marked. -bw/11-Sep-97 */
	  {
	    fprintf (stderr, "emacs-debug:%s: %p\n",
		     __PRETTY_FUNCTION__, ptr);
	    fprintf (stderr, "hit return to continue\n"); getchar ();
	    assert (!"implemented: large strings");
	    /* A large string.  Just set ARRAY_MARK_FLAG.  */
	    ptr->size |= ARRAY_MARK_FLAG;
	  }
#if 0				/* Admp: A <struct Lisp_String *> doesn't contain any pointers. -bw/11-Sep-97 */
	else
	  {
	    /* A small string.  Put this reference
	       into the chain of references to it.
	       If the address includes MARKBIT, put that bit elsewhere
	       when we store OBJPTR into the size field.  */

	    if (ADMP_XMARKBIT (*objptr))
	      {
		XSETFASTINT (*objptr, ptr->size);
		ADMP_XMARK (*objptr);
	      }
	    else
	      XSETFASTINT (*objptr, ptr->size);

	    if ((EMACS_INT) objptr & DONT_COPY_FLAG)
	      abort ();
	    ptr->size = (EMACS_INT) objptr;
	    if (ptr->size & MARKBIT)
	      ptr->size ^= MARKBIT | DONT_COPY_FLAG;
	  }
#endif
      }
      break;

    case Lisp_Vectorlike:
      if (GC_BUFFERP (obj))
	{
	  if (!ADMP_XMARKBIT (XBUFFER (obj)->name))
	    patch_buffer (XBUFFER (obj));
	}
      else if (GC_SUBRP (obj))
	/* begin Admp -bw/11-Sep-97 */
	{
	  struct Lisp_Subr *ptr = XSUBR (obj);
	  if (ADMP_XMARKBIT (ptr->size))
	    break;
	  ADMP_XMARK (ptr->size);
	  PATCH_CPTR (ptr->function);
	  PATCH_CPTR (ptr->symbol_name);
	  PATCH_CPTR (ptr->prompt);
	  /* Make sure that not a doc offset */
	  if ((long) ptr->doc > 0)
	    PATCH_CPTR (ptr->doc);
	  break;
	}
	/* end Admp */
      else if (GC_COMPILEDP (obj))
	/* We could treat this just like a vector, but it is better
	   to save the COMPILED_CONSTANTS element for last and avoid recursion
	   there.  */
	{
	  register struct Lisp_Vector *ptr = XVECTOR (obj);
	  register EMACS_INT size = ptr->size;
	  /* See comment above under Lisp_Vector.  */
	  struct Lisp_Vector *volatile ptr1 = ptr;
	  register int i;

	  /* begin Admp */
	  if (!(size & ARRAY_MARK_FLAG) == !FIX_TO_RELOC_P())
	    break;		/* Already marked/unmarked */
	  ADMP_MARK_ARRAY(ptr);
	  size &= ~ARRAY_MARK_FLAG; /* Admp: clear markbit -bw/11-Sep-97 */
	  /* end Admp */

	  size &= PSEUDOVECTOR_SIZE_MASK;
	  for (i = 0; i < size; i++) /* and then mark its elements */
	    {
	      if (i != COMPILED_CONSTANTS)
		PATCH_OBJ (&ptr1->contents[i]);
	    }
	  /* This cast should be unnecessary, but some Mips compiler complains
	     (MIPS-ABI + SysVR4, DC/OSx, etc).  */
	  objptr = (Lisp_Object *) &ptr1->contents[COMPILED_CONSTANTS];
	  goto loop;
	}
#if defined (AMIGA_V_EMACS_20_2) || defined (MULTI_FRAME)
      else if (GC_FRAMEP (obj))
	{
	  /* See comment above under Lisp_Vector for why this is volatile.  */
	  register struct frame *volatile ptr = XFRAME (obj);
	  register EMACS_INT size = ptr->size;

	  /* begin Admp */
	  if (!(size & ARRAY_MARK_FLAG) == !FIX_TO_RELOC_P())
	    break;		/* Already marked/unmarked */
	  ADMP_MARK_ARRAY(ptr);
	  size &= ~ARRAY_MARK_FLAG; /* Admp: clear markbit -bw/11-Sep-97 */
	  /* end Admp */

	  PATCH_OBJ (&ptr->name);
	  PATCH_OBJ (&ptr->icon_name);
	  PATCH_OBJ (&ptr->title);
	  PATCH_OBJ (&ptr->focus_frame);
	  PATCH_OBJ (&ptr->selected_window);
	  PATCH_OBJ (&ptr->minibuffer_window);
	  PATCH_OBJ (&ptr->param_alist);
	  PATCH_OBJ (&ptr->scroll_bars);
	  PATCH_OBJ (&ptr->condemned_scroll_bars);
	  PATCH_OBJ (&ptr->menu_bar_items);
	  PATCH_OBJ (&ptr->face_alist);
	  PATCH_OBJ (&ptr->menu_bar_vector);
	  PATCH_OBJ (&ptr->buffer_predicate);
#ifdef AMIGA_V_EMACS_20_2
	  PATCH_OBJ (&ptr->buffer_list);
#endif /* 20.2 */
	  /* begin Admp -bw/11-Sep-97 */
	  {
	    /* UPDATEME: Emacs-20.2 -bw/04-Jan-98 */
	    /* FIXME-bw:  What about vector-like patch iteration?  */
	    PATCH_LPTR (ptr->root_window);

	    PATCH_CPTR (ptr->namebuf);

	    /* frame_glyphs substructure - seems not needed  */
#if 1
	    assert (!ptr->current_glyphs && !ptr->desired_glyphs && !ptr->temp_glyphs);
	    if (ptr->current_glyphs || ptr->desired_glyphs || ptr->temp_glyphs)
	      abort;
#else
	    {
	      int i;
	      struct frame_glyphs *v[] 
		= {ptr->current_glyphs, ptr->desired_glyphs, ptr->temp_glyphs };
	      for (i=0; i < 3; ++i)
		if (v[i])
		  {
#if defined (AMIGA_V_EMACS_20_2) || defined (MULTI_FRAME)
		    PATCH_CPTR (v[i]->frame);
#endif
// **		    PATCH_CPTR (v[i]->glpyhs); /* FIXME-bw */
		    PATCH_CPTR (v[i]->used);
		    PATCH_CPTR (v[i]->highlight);
		    PATCH_CPTR (v[i]->bufp);
#ifdef HAVE_WINDOW_SYSTEM
		    PATCH_CPTR (v[i]->top_left_x);
		    PATCH_CPTR (v[i]->top_left_y);
		    PATCH_CPTR (v[i]->pix_width);
		    PATCH_CPTR (v[i]->pix_height);
		    PATCH_CPTR (v[i]->max_ascent);
#endif /* HAVE_WINDOW_SYSTEM */
		    PATCH_CPTR (v[i]->charstarts);
// **		    PATCH_CPTR (v[i]->total_charstarts); /* FIXME-bw */
		  }
	    }
	    PATCH_CPTR (ptr->current_glyphs);
	    PATCH_CPTR (ptr->desired_glyphs);
	    PATCH_CPTR (ptr->temp_glyphs);
#endif /* not 1 */
	    PATCH_CPTR (ptr->insert_line_cost);
	    PATCH_CPTR (ptr->delete_line_cost);
	    PATCH_CPTR (ptr->insert_n_lines_cost);
	    PATCH_CPTR (ptr->delete_n_lines_cost);
#ifdef MULTI_KBOARD
	    PATCH_CPTR (ptr->kboard);
#endif
	    PATCH_CPTR (ptr->message_buf);
#ifdef AMIGA_V_EMACS_20_2
	    PATCH_CPTR (ptr->fontset_data);
#endif /* 20.2 */
	  }
	  /* end Admp */
	}
#endif /* MULTI_FRAME */
      else
	{
	  register struct Lisp_Vector *ptr = XVECTOR (obj);
	  register EMACS_INT size = ptr->size;
	  /* The reason we use ptr1 is to avoid an apparent hardware bug
	     that happens occasionally on the FSF's HP 300s.
	     The bug is that a2 gets clobbered by recursive calls to mark_object.
	     The clobberage seems to happen during function entry,
	     perhaps in the moveml instruction.
	     Yes, this is a crock, but we have to do it.  */
	  struct Lisp_Vector *volatile ptr1 = ptr;
	  register int i;

	  /* begin Admp */
	  if (!(size & ARRAY_MARK_FLAG) == !FIX_TO_RELOC_P())
	    break;		/* Already marked/unmarked */
	  ADMP_MARK_ARRAY(ptr);
	  size &= ~ARRAY_MARK_FLAG; /* Admp: clear markbit -bw/11-Sep-97 */
	  /* end Admp */

	  if (size & PSEUDOVECTOR_FLAG)
	    size &= PSEUDOVECTOR_SIZE_MASK;
	  for (i = 0; i < size; i++) /* and then mark its elements */
	    PATCH_OBJ (&ptr1->contents[i]);
	}
      break;

    case Lisp_Symbol:
      {
	/* See comment above under Lisp_Vector for why this is volatile.  */
	register struct Lisp_Symbol *volatile ptr = XSYMBOL (obj);
	struct Lisp_Symbol *ptrx;

	if (ADMP_XMARKBIT (ptr->plist)) break;
	ADMP_XMARK (ptr->plist);
	PATCH_OBJ ((Lisp_Object *) &ptr->value);
	PATCH_OBJ (&ptr->function);
	PATCH_OBJ (&ptr->plist);

	if (FIX_TO_RELOC_P())
	  {
	    struct Lisp_String *name = ptr->name;
	    ptr->name = (struct Lisp_String*) ((int)Lisp_String << VALBITS);
	    XSETPNTR (*(Lisp_Object *) &ptr->name, (unsigned)name);
	    PATCH_OBJ ((Lisp_Object *) & ptr->name);
	    ptr = ptr->next;
	    if (ptr)
	      {
		/* For the benefit of the last_marked log.  */
		objptr = (Lisp_Object *)&XSYMBOL (obj)->next;
		ptrx = ptr;	/* Use of ptrx avoids compiler bug on Sun */
		XSETSYMBOL (obj, ptrx);
		/* We can't goto loop here because *objptr doesn't contain an
		   actual Lisp_Object with valid datatype field.  */
		
		PATCH_CPTR (*(void**)objptr); /* Admp: will not patched at
					 "loop:" label. -bw/11-Sep-97 */
		goto loop2;
	      }
	  }
	else
	  {
	    PATCH_OBJ ((Lisp_Object *) & ptr->name);
	    ptr->name = XSTRING (*(Lisp_Object *) & ptr->name);
	    PATCH_CPTR (ptr->next); /* Admp: was patched above -bw/11-Sep-97 */
	    ptr = ptr->next;
	    if (ptr)
	      {
		/* For the benefit of the last_marked log.  */
		objptr = (Lisp_Object *)&XSYMBOL (obj)->next;
		ptrx = ptr;	/* Use of ptrx avoids compiler bug on Sun */
		XSETSYMBOL (obj, ptrx);
		/* We can't goto loop here because *objptr doesn't contain an
		   actual Lisp_Object with valid datatype field.  */

		goto loop2;
	      }
	  }
      }
      break;

    case Lisp_Misc:
      switch (XMISCTYPE (obj))
	{
	case Lisp_Misc_Marker:
	  /* begin Admp -bw/11-Sep-97 */
	  {
	    struct Lisp_Marker *ptr = XMARKER (obj);
	    if (ADMP_XMARKBIT (ptr->chain))
	      break;
	    ADMP_XMARK (XMARKER (obj)->chain);
	    PATCH_CPTR (ptr->buffer);
	    objptr = &ptr->chain;
	    goto loop;
	  }
	  /* end Admp */
	  break;

	case Lisp_Misc_Buffer_Local_Value:
	case Lisp_Misc_Some_Buffer_Local_Value:
	  {
#ifndef AMIGA_V_EMACS_20_3
#define realvalue car
#endif
	    register struct Lisp_Buffer_Local_Value *ptr
	      = XBUFFER_LOCAL_VALUE (obj);
	    if (ADMP_XMARKBIT (ptr->realvalue)) break;
	    ADMP_XMARK (ptr->realvalue);
#if 0
	    /* If the cdr is nil, avoid recursion for the realvalue.  */
	    if (EQ (ptr->cdr, Qnil))
	      {
		PATCH_LPTR (&ptr->cdr);	/* Admp: -bw/11-Sep-97  */
		objptr = &ptr->realvalue;
		goto loop;
	      }
#endif
	    PATCH_OBJ (&ptr->realvalue);
#ifdef AMIGA_V_EMACS_20_3
	    PATCH_OBJ (&ptr->buffer);
	    PATCH_OBJ (&ptr->frame);
#endif
	    /* See comment above under Lisp_Vector for why not use ptr here.  */
	    objptr = &XBUFFER_LOCAL_VALUE (obj)->cdr;
	    goto loop;
	  }

	  /* begin Admp -bw/11-Sep-97 */
	case Lisp_Misc_Intfwd:
	case Lisp_Misc_Boolfwd:
	case Lisp_Misc_Objfwd:
	  /* Don't need to do Lisp_Objfwd, since the places they point
	     are protected with staticpro.  */
	  {
	    /* Note: The 3 types are binary compatible. -bw/11-Sep-97 */
	    struct Lisp_Objfwd *ptr = XOBJFWD (obj);
	    PATCH_CPTR (ptr->objvar);
	  }
	  break;
	  /* end Admp */

	case Lisp_Misc_Buffer_Objfwd:
	case Lisp_Misc_Kboard_Objfwd:
	  /* Don't bother with Lisp_Buffer_Objfwd,
	     since all markable slots in current buffer marked anyway.  */
	  break;

	case Lisp_Misc_Overlay:
	  {
	    struct Lisp_Overlay *ptr = XOVERLAY (obj);
	    if (!ADMP_XMARKBIT (ptr->plist))
	      {
		ADMP_XMARK (ptr->plist);
		PATCH_OBJ (&ptr->start);
		PATCH_OBJ (&ptr->end);
		objptr = &ptr->plist;
		goto loop;
	      }
	  }
	  break;

	default:
	  abort ();
	}
      break;

    case Lisp_Cons:
      {
	register struct Lisp_Cons *ptr = XCONS (obj);
	if (ADMP_XMARKBIT (ptr->car)) break;
	ADMP_XMARK (ptr->car);
#if 0
	/* If the cdr is nil, avoid recursion for the car.  */
	if (EQ (ptr->cdr, Qnil))
	  {
	    objptr = &ptr->car;
	    goto loop;
	  }
#endif
	PATCH_OBJ (&ptr->car);
	/* See comment above under Lisp_Vector for why not use ptr here.  */
	objptr = &XCONS (obj)->cdr;
	goto loop;
      }

#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      ADMP_XMARK (XFLOAT (obj)->type);
      break;
#endif /* LISP_FLOAT_TYPE */

    case Lisp_Int:
      break;

    default:
      abort ();
    }
}

/* Patch linked node objects. */
/* PTR points to the head node.  OFFSET is the byte-offset of the link
   field for that kind of node.  This function obeys also the
   predicate FIX_TO_RELOC_P() to switch betwee patching and
   unpatching.  */
static void
patch_chain (void **ptr, int offset)
{
  void *fix;
  
  local_precond (ptr);
  
  for (;*ptr; ptr = (void **) ((char *)fix + offset))
    PATCH_CPTR_SET (*ptr, fix);
}  

/* patch - patching/unpatching pointer objects */
/* If MODE is equal 0 then patching is enabled, else unpatching.  */
static void
patch (int mode)
{
  int i;
  int sw;

  INIT_HUNK_TABLE();

  a_dmp.mode = mode;
  a_dmp.patch_cptr = (mode == ADMP_MODE_MAP_OUT) ? hunk_pointer : make_pointer;
  a_dmp.patch_lptr = (mode == ADMP_MODE_MAP_OUT) ? hunk_lispptr : make_lispptr;

  /* depending on mode we run the following code pieces forward or backward */
  
  if (mode) goto patch_global_ptrs;

 patch_statvec_objs:   /* Patch lisp object hierarchy */
  {
    /* patch objects reachable trough staticvec[] */
    for (i = 0; i < staticidx; i++)
      if (!ADMP_XMARKBIT (*staticvec[i]))
	{
	  /* FIXME-bw: could we avoid this conditional code?  */
	  if (FIX_TO_RELOC_P())
	    PATCH_OBJ (staticvec[i]), ADMP_XMARK (*staticvec[i]);
	  else
	    ADMP_XMARK (*staticvec[i]), PATCH_OBJ (staticvec[i]);
	}
  }
  if (mode) goto patch_done;

 patch_statvec_ptrs: 	/* patch pointers of staticvec[] */
  {
    for (i = 0; i < staticidx; i++)
      PATCH_CPTR (staticvec[i]);
  }
  if (mode) goto patch_statvec_objs;

 patch_global_structs:			/* Patch global structure objects
					   Ignore LISP pointers in structures! */
  {
    /* strings */
    {
      struct string_block *i;
      void *next;

      for (i = first_string_block; i; i=next)
	{
	  PATCH_CPTR_SET (i->next, next);
	  PATCH_CPTR (i->prev);
	}
    }

    /* buffers */
    {
      struct buffer *i;
      void *next;

      for (i = all_buffers; i; i=next)
	PATCH_CPTR_SET (i->next, next);
    }

    /* regexp_cache */
    {
      struct regexp_cache *i;
      void *next;

      for (i=searchbuf_head; i; i=next)
	{
	  PATCH_CPTR_SET (i->next, next);
	  // PATCH_LPTR (i->regexp); /* staticpro */
	  PATCH_CPTR (i->buf.buffer);
	  PATCH_CPTR (i->buf.fastmap);
#ifndef AMIGA_V_EMACS_20_3
	  PATCH_CPTR (i->buf.translate);  /* (char *) on 20.2.1 */
#else /* 20.3 */
	  if (i->buf.translate)
	    PATCH_LPTR (i->buf.translate);  /* <Lisp_Object> or 0 on 20.2.92 */
#endif /* 20.3 */
	}
    }

    /* keyboard */
    {
      struct kboard *i;
      void *next;

      for (i=all_kboards; i; i=next)
	{
	  PATCH_CPTR_SET (i->next_kboard, next);
	  patch_keyboard (i);
	}
    }

#if !defined (MULTI_FRAME) && !defined (AMIGA_V_EMACS_20_2)
    /* CHFIXME: use makro */
    PATCH_LPTR (the_only_frame.root_window);
#endif

#ifndef HAVE_ALLOCA
    PATCH_CPTR (fail_stack.stack);
#endif

    Admp_tbl_dispatch (ADMP_KEY_LOBJ, Admp_action_lobj, 0, Admp_tables);
    Admp_tbl_dispatch (ADMP_KEY_LOBJ_VEC, Admp_action_lobj_vec, 0, Admp_tables);

    /* buffer.c */
    patch_buffer (&buffer_local_types);
    PATCH_CPTR (buffer_local_types.next); /* superfluous */
    patch_buffer (&buffer_local_flags);
    PATCH_CPTR (buffer_local_flags.next); /* superfluous */

    PATCH_CPTR (search_regs.start);
    PATCH_CPTR (search_regs.end);

#ifdef AMIGA_V_EMACS_20_2
    /* syntax.c - gl_state */
    {
#ifdef AMIGA_V_EMACS_20_3
      PATCH_LPTR (gl_state.object);
#endif
      PATCH_LPTR (gl_state.global_code);
      PATCH_LPTR (gl_state.current_syntax_table);
      PATCH_LPTR (gl_state.old_prop); /* ???-bw/16-Dec-97: PATCH_OBJ() ? */
      /* superflous!? */
      PATCH_CPTR (gl_state.forward_i);
      PATCH_CPTR (gl_state.backward_i);
    }
#endif /* AMIGA_V_EMACS_20_2 */
  }

#ifdef AMIGA_V_EMACS_20_2
  /* coding.c */
  patch_coding_system (&terminal_coding);
  patch_coding_system (&safe_terminal_coding);
  patch_coding_system (&keyboard_coding);
#ifdef AMIGA_V_EMACS_20_3
  {
    int i;
    extern struct coding_system *coding_system_table[CODING_CATEGORY_IDX_MAX];
    struct coding_system *addr;

    for (i=0; i < CODING_CATEGORY_IDX_MAX; ++i)
      if (coding_system_table[i])
	{
	  PATCH_CPTR_SET (coding_system_table[i], addr);
	  patch_coding_system (addr);
	}
  }
#endif /* AMIGA_V_EMACS_20_3 */
#endif /* AMIGA_V_EMACS_20_2 */

  if (mode) goto patch_statvec_ptrs;

 patch_global_ptrs:			/* patch single global pointers */
  {
    do_apply_cptr_list (a_dmp.patch_cptr, patch_chain);
    do_apply_lptr_list (a_dmp.patch_lptr);
  }
  if (mode) goto patch_global_structs;
  
 patch_done:
  /* Scan for possible unpatched pointers.  */
  if (!FIX_TO_RELOC_P())
    Admp_check_pointer_count ();
  else if (getenv ("EMACS_DEV"))
    /* call slow debug code to scan for unhandled pointer objeckts */
    check_cands ();
}

static int
dump (int mode, char const *path)
{
  FILE *stream;

  local_precond (path);

  errno = 0;

  stream = fopen (path, ((mode==ADMP_MODE_MAP_OUT) ? "wb" : "rb"));

  if (stream)
    {
      size_t sizes[] = { TEXT_SIZE, DATA_SIZE, BSS_SIZE, puresize };
      char Admp_magic_cookie[] = ADMP_MAGIC_COOKIE;
      char Admp_eof_cookie[] = ADMP_EOF_COOKIE;

      /* Head cookie */
      Admp_dump_obj (mode, &Admp_magic_cookie, sizeof Admp_magic_cookie, 1, stream);
      if (memcmp (Admp_magic_cookie, ADMP_MAGIC_COOKIE, sizeof Admp_magic_cookie) != 0)
	Aerr_data_file (path, "Wrong dump file version for this Emacs binary" 
		 " (mismatching head cookie)");
      
      /* hunk sizes */
      if (Admp_dump_obj (mode, &sizes, sizeof sizes, 1, stream) != 1)
	Aerr_data_file (path, "I/O error");

      if (mode == ADMP_MODE_MAP_IN)
	{
	  if (sizes[0] != TEXT_SIZE
	      || sizes[1] != DATA_SIZE
	      || sizes[2] != BSS_SIZE)
	    Aerr_data_file (path, "Wrong dump file version (mismatching segment size)");
	      
	  /* Allocate storage for hunks. */
	  puresize = sizes[3];
	  pure = (void*)Asma_nd_alloc (puresize);
	}

      /* hunk contents */
      {
	int error = Admp_dump (mode, stream);
	if (error == 1)
	  Aerr_data_file (path, "corrupt data");
	else if (error == -1)
	  {
	    perror ("dump code");
	    Aerr_data_file (path, "I/O-error");
	  }
      }

      /* EOF cookie. */
      Admp_dump_obj (mode, &Admp_eof_cookie, sizeof Admp_eof_cookie, 1, stream);
      if (memcmp (Admp_eof_cookie, ADMP_EOF_COOKIE, sizeof Admp_eof_cookie) != 0)
	Aerr_data_file (path, "Wrong dump file version for this Emacs binary" 
		 " (mismatching tail cookie)");

      fclose (stream);
    }
  return errno == 0;
}

/* Make machine address from given segment and segment offset */
/* [bw] Decode encoded pointer from dumpfile to absolute pointer.
   This works by getting the hunk from the hunk-code bits and adding the
   offset to the current start address of this hunk.  */
static void *
make_pointer (void *ptr, void **fix)
{
  void *r=0;

  if (ptr)			/* no need to relocate NULL */
    {
      unsigned hunk = ((long) ptr & HUNK_MASK) >> HUNK_POS;
      unsigned offset = (long) ptr & (VALMASK & ~HUNK_MASK);

#ifdef ADMP_NO_PATCH
      --a_dmp.ptr_count2;
      r = ptr;
#else  
      if (hunk == (HUNK_MALLOC>>HUNK_POS))
	{
	  if (a_dmp.oa_map)
	    r = Admp_oa_map_locate_offset_bs (a_dmp.oa_map, offset);
	  if (!r && !(r = Asma_locate_offset (offset)))
	    Aerr_panic("Impossible Error: Malloc offset <%d> not exist.\n", offset);
	}
      else if (hunk < SIZE_ADMP_HUNK_TABLE)
	{
	  r = Admp_hunk_table[hunk].start + offset;
	  check_postcond (RANGE (r, Admp_hunk_table[hunk].start, Admp_hunk_table[hunk].end));
	}
      else
	{
	  Aerr_panic("Internal error: malformatted reloc-pointer "
		     "(unknown hunk ID) <%p>\n", ptr);
	  abort ();		/* NOTREACHED */
	}

      --Admp_ptr_count [hunk];
#endif
    }

  if (fix)
    *fix = r;
  return r;
}

/* Make Lisp reference from given segment, segment offset and type bits. */
static Lisp_Object
make_lispptr (Lisp_Object obj, Lisp_Object *fix)
{
#ifdef ADMP_NO_PATCH
  if (obj)
    --a_dmp.ptr_count2;
#else
  long val = XUINT (obj);
  char *ptr = make_pointer ((void *) val, 0);
  assert (ptr != 0);
  XSETPNTR (obj, (unsigned) ptr);
#endif
  if (fix)
    *fix = obj;
  return obj;
}

/* Test cookie in dump input stream.  */
static bool
match_cookie (const char *cookie, size_t len, FILE * is)
{
  char *buf = 0;

  local_precond (cookie && is);

  buf = alloca (len);
  return (len == fread (buf, 1, len, is)
	  && 0 == bcmp (cookie, buf, len));
}

static void
Admp_check_pointer_count ()
{
  int i;
  int sum=0, fail=0;

  for (i=0; i < 8; ++i)
    {
      int pct =  Admp_ptr_count [i];
      if (pct)
	{
	  sum += pct;
#ifndef ADMP_NO_PATCH
	  fprintf (stderr, 
		   "Internal error: pointer count mismatch hunk: %s  count: %d\n",
		   Admp_hunk_table[i].name, pct);
	  fail = 1;
#endif
	}
    }
#ifndef ADMP_NO_PATCH
  if (sum)
    {
      fprintf (stderr,
	       "Internal error: pointer sum mismatch: %d (expected value: 0)\n",
	       sum);
    }
#else
  if ((sum + a_dmp.ptr_count2) != 0)
    {
      fprintf (stderr,
	       "Internal error: pointer sum mismatch: %d : %d\n",
	       sum, a_dmp.ptr_count2);
      fail = 1;
    }
      
#endif
  if (fail)
    Aerr_fail ("Panic: Internal error or possible corrupt data dump file.\n");
}



/* map_out_data - Dump process data image to file.

   Dumped data are located at TEXT, DATA, BSS, malloc_hunk ore pure
   storage.  Data on stack and data of link libraries is not dumped.

   -  Call the Lisp garbage collector.

   - Hide the untouched parts of pure_hunk to avoid
   dumping it. (economy)

   -  Prepare the data for dumping (just in place).

   -  Copy the required parts of our process data to the dump file.

   -  Undo the changes made on data.

   - Set the initialized flag.  */
void
map_out_data (char *data_path)
{
  int old_puresize = puresize;
  bool verify_reloc = getenv ("EMACS_ADMP_VERIFY") != 0;
  int i;

  check_precond (data_path);

  if (amiga_initialized)
    error ("You can only dump once !");

  Fgarbage_collect ();

  if (verify_reloc)
    {
      int bss_out;
      if ((bss_out = creat ("/tmp/bss.pre", 0777)) >= 0)
	{
	  write (bss_out, BSS_START, BSS_SIZE);
	  close (bss_out);
	}
      dump (0, "/tmp/EMACS-DATA.pre");
    }

  puresize = min (puresize, pureptr + 02000);

  /* speed up locating in malloc-hunk */
  a_dmp.oa_map = Asma_offset_table ();

  patch (0);
  for (i=0; i < 8; ++i)
    if (Admp_hunk_table[i].name)
      printf ("emacs-pointer-relocate: hunk %d (%s) is referenced %d times.\n",
	      i, Admp_hunk_table[i].name, Admp_ptr_count[i]);

  if (!dump (0, data_path))
    {
      patch (1);
      Aerr_fail ("emacs hasn't been dumped (%s <%s>)", strerror (errno), data_path);
    }
  patch (1);


  puresize = old_puresize;

  if (verify_reloc)
    {
      int bss_out;
      if ((bss_out = creat ("/tmp/bss.post", 0777)) >= 0)
	{
	  write (bss_out, BSS_START, BSS_SIZE);
	  close (bss_out);
	}
      dump (0, "/tmp/EMACS-DATA.post");
      system ("diff /tmp/EMACS-DATA.pre /tmp/EMACS-DATA.post");
    }

  amiga_initialized = 1;
}


void
map_in_data (int load)
{
  if (!load)
    {
      pure = (void*)Asma_nd_alloc (puresize);
    }
  else
    {
      const char *data = a_dmp.data_file_name;

      if (!dump (1, data))
	{
	  perror ("Cannot map in data from data-dump file");
	  Aerr_data_file (data,
		   "undump() failed\n"
		   );
	}
      a_dmp.oa_map = Asma_offset_table ();
      patch (1);

      initialized = amiga_initialized = 1;

      /* CHFIXME: force errors if used but not patched */
      handlerlist = (void *) -1;
      catchlist = (void *) -1;
      backtrace_list = (void *) -1;
#if 0
      FRAME_EXTERNAL_MENU_BAR (selected_frame) = 1;	/* CHFIXME where to put? */
#endif
    }

#ifdef AMIGA_V_EMACS_20_2
  /* re_match_object is not protected, therefore the strings could be
     garbage.  I used to staticpro re_match_object in amiga_sysdep.c.
     Now we do ignore it again.  -bw/07-Mar-99 */
    {
      extern Lisp_Object re_match_object;
      re_match_object = Qnil; /* really needed? */
    }
#endif

  amiga_undump_reinit ();
}

/* Replace name of data dump file to use.  Note: The argument string
   STATIC_NAME will not copied. */
void
Admp_set_data_file_name (const char *static_name)
{
  a_dmp.data_file_name = static_name; 
}

const char *
Admp_data_file_name ()
{
  return a_dmp.data_file_name;
}

bool
Admp_init_module ()
{
  check_precond (!a_dmp.initialized);

  if (!a_dmp.data_file_name)
    a_dmp.data_file_name = AMIGA_DUMP_FILE_STRING;

  a_dmp.initialized = true;
  return true;
}


/* === debug aids === */

#if 1

/* List possible pointer object not already patched or ignored. */
/* This code looks for pointers directing to address into text, data
   or bss segment. */
static void
check_cand (FILE * os, char *hunk_name,
	    void *start_hunk, void *start, void *end)
{
  short *i = 0;

  local_precond (os && hunk_name && start_hunk && start 
		 && end && (char*)start < (char*)end);

  for (i = start; i < (short *) end; ++i)
    {
      int *x = *(int **) i;
      Lisp_Object y =  *(Lisp_Object *) i;
      void *tmp;

      if (!(x && (RANGE (x, TEXT_FIRST, TEXT_END)
		  || RANGE (x, DATA_FIRST, DATA_END)
		  || RANGE (x, BSS_FIRST, BSS_END)
		  || RANGE (x, PURE_START, PURE_END)
		  || (a_dmp.oa_map 
		      ? (Admp_oa_map_locate_address (a_dmp.oa_map, x) > 0)
		      : (Asma_locate_address (x) > 0)))
	    && !check_ignore (i, 0)))
	continue;

#define FIRST ((char*)start_hunk-(char*)0)
      fprintf (os, "%08lx %s cptr %p %p\n",
	       (unsigned long) ((char *) i - FIRST),
	       hunk_name,
	       x, (tmp = hunk_pointer (x, 0)));
      /* balance number of calls to hunk_pointer/make_pointer */
      (void)make_pointer (tmp, 0);
      
      if (!GC_INTEGERP (y) && !XMARKBIT (y) && Amal_lpntr_p (y))
	{
	  /* possible LPTR */
	  unsigned long offset;

	  offset = (unsigned long)((char *) i - FIRST);
	  if (strcmp (hunk_name, "B") != 0 
	      || !(offset & 3)) /* All possible BSS objects seems to
				   be 4-byte aligned */
	    {
	      const char *type = "unknown_lobj";
	      if (STRINGP (y))
		type = "string";
	      else if (CONSP (y))
		type = "cons";
	      else if (VECTORLIKEP (y))
		{
		  type = "vectorlike";
		  if (VECTORP (y))
		    type = "vector";
		  else if WINDOW_CONFIGURATIONP (y)
		    type = "window_configuration";
		  else if PROCESSP (y)
		    type = "process";
		  else if WINDOWP (y)
		    type = "window";
		  else if SUBRP (y)
		    type = "subr";
		  else if COMPILEDP (y)
		    type = "compiled";
		  else if BUFFERP (y)
		    type = "buffer";
		}
	      else if (SYMBOLP (y))
		type = "symbol";
	      else if (FLOATP (y))
		type = "float";
	      else if (MISCP (y))
		{
		  type = "misc";
		  if (OVERLAYP (y))
		    type = "overlay";
		  else if (MARKERP (y))
		    type = "marker";
		  else if (INTFWDP (y))
		    type = "intfwd";
		  else if (BOOLFWDP (y))
		    type = "boolfwd";
		  else if (OBJFWDP (y))
		    type = "objfwd";
		  else if (BUFFER_OBJFWDP (y))
		    type = "buffer_objfwd";
		}
	      fprintf (os, "%08lx %s %s\n", offset, hunk_name, type);
	    }
	}
    }
}

/* List possible pointer object not already patched or ignored. */
/* This is only guessing. */
static void
check_cand_ignore (FILE * os, char *hunk_name,
	    void *start_hunk, void *start, void *end)
{
  short *i = 0;

  local_precond (os && hunk_name && start_hunk && start 
		 && end && (char*)start < (char*)end);

  for (i = start; i < (short *) end; ++i)
    {
      int *x = *(int **) i;

      if (RANGE (x, (char*)0xDC00000, (char*)0xf000000)	/* XXX: machine specific! */
	  && !(RANGE (x, TEXT_START, TEXT_END)
	       || RANGE (x, DATA_START, DATA_END)
	       || RANGE (x, BSS_START, BSS_END)
	       || check_ignore (i, 0)))
	{
	  /* possible CPTR */
#define FIRST ((char*)start_hunk-(char*)0)
	  fprintf (os, "%08lx %s\n",
		   (unsigned long) ((char *) i - FIRST),
		   hunk_name);
	}
    }
}


static void
check_cands ()
{
  FILE *os;

  if ((os = fopen ("amiga_dump.cands", "wb")))
    {
      fprintf (os, "#Possible candidates for patching:'\n");
      check_cand (os, "D", DATA_START, DATA_FIRST, DATA_LAST);
      check_cand (os, "B", BSS_START, BSS_FIRST, BSS_LAST);
      fprintf (os, "#Possible candidates for ignoring:'\n");
      check_cand_ignore (os, "D", DATA_START, DATA_FIRST, DATA_LAST);
      check_cand_ignore (os, "B", BSS_START, BSS_FIRST, BSS_LAST);
      fclose (os);
    }
}
#endif


/* debug support function */
static void
print_ranges ()
{
  Aerr_print_msg ("text %08lx [%08lx %08lx] %08lx\n"
		  "data %08lx [%08lx %08lx] %08lx\n"
		  "bss %08lx [%08lx %08lx] %08lx\n"
		  "(pure %08lx ... %08lx (%lu))\n",
		  TEXT_START, TEXT_FIRST, TEXT_LAST, TEXT_END,
		  DATA_START, DATA_FIRST, DATA_LAST, DATA_END,
		  BSS_START, BSS_FIRST, BSS_LAST, BSS_END,
		  PURE_START, PURE_END, PURE_END - PURE_START);
}
#endif /* not TEST */


/** SECTION: save/restore hunk content with preserving gaps in it **/

#ifndef TEST

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

#define PUTC putc
#define GETC getc

#else /* TEST */

/* Avoid hits of boundchecking tools. */
#define PUTC fputc
#define GETC fgetc

char hunk[] =
"\xff"
"1abcdefghijklmnopqrstuvwxyz"
"2abcdefghijklmnopqrstuvwxyz"
"3abcdefghijklmnopqrstuvwxyz"
"4abcdefghijklmnopqrstuvwxyz"
"5abcdefghijklmnopqrstuvwxyz"
"6abcdefghijklmnopqrstuvwxyz"
"7abcdefghijklmnopqrstuvwxyz"
"8abcdefghijklmnopqrstuvwxyz"
 ;
void *Begin = hunk, *End = hunk + sizeof hunk;

#endif /* TEST */

enum key
  {
    k_initial,
    k_end,
    k_skip_data,
    k_raw_data,
  };

struct skip_data
  {
    char key;
    char cookie;
    unsigned count;
  };

struct raw_data
  {
    char key;
    char cookie;
    unsigned size;
    char data[0];
  };

enum
  {
    RD_COOKIE = 0x55,
    SD_COOKIE = 0x54,
  };

static bool
restore_hunk (void *begin, const void *end, FILE * is)
{
  int type = 0;
  unsigned char *i = 0;

  local_precond (begin && (char *) begin < (char *) end && is);

  for (i = begin; i < (unsigned char *) end;)
    {
      type = getc (is);

      switch (type)
	{
	case EOF:
	  assert (false);
	  abort ();
	  break;		/* NOTREACHED */
	case k_skip_data:
	  {
	    struct skip_data d;
	    fread ((char *) &d + 1, sizeof d - 1, 1, is);
	    if (!d.cookie == SD_COOKIE)
	      assert (false);
	    i += d.count;
	  }
	  break;
	case k_raw_data:
	  {
	    struct raw_data d;

	    fread ((char *) &d + 1, sizeof d - 1, 1, is);
	    if (!d.cookie == RD_COOKIE)
	      assert (false);
	    if (i + d.size > (unsigned char *) end)
	      assert (false);
#ifndef TEST
	    fread (i, 1, d.size, is);
	    i += d.size;
#else
	    {
	      int k;
	      for (k = 0; k < d.size; ++i, ++k)
		{
		  int c = getc (is);
		  if (c == EOF)
		    assert (false);
		  assert (*i == c);
		}
	    }
#endif
	  }
	  break;
	default:
	  fprintf (stderr, "%s::%s - abort\n", __FILE__, __PRETTY_FUNCTION__);
	  abort ();
	}
    }
  return true;
}

static bool
save_hunk_conditional (const void *begin, const void *end,
		       bool (*ign) (const void *, const void **), FILE * os)
{
  const char *i = 0;
  char type = k_initial;
  struct skip_data sd =
  {0};
  struct raw_data rd =
  {0};
  bool terminate = false;
  int block_size = 1;

  local_precond (begin && (char *) begin < (char *) end && os);

  sd.key = k_skip_data;
  sd.cookie = SD_COOKIE;
  rd.key = k_raw_data;
  rd.cookie = RD_COOKIE;

  errno = 0;

  for (i = begin; !errno && !terminate; i+=block_size)
    {
      char new_type = 0;
      const void *next_change = 0;

      /* Find current data type for `I' */
      if (!(i < (char *) end))
	{
	  terminate = true;
	  new_type = k_end;
	}
      else if (ign && ign (i, &next_change))
	new_type = sd.key;
      else
	new_type = rd.key;

      if (!ign)
	block_size = PTR_DIFF (end, i);
      else
	block_size = (!next_change) ? 1 : PTR_DIFF (PTR_MIN (end, next_change), i);
      assert (block_size > 0 || new_type == k_end);
      assert (block_size < 5000000); /* paranoia */

      if (type == new_type)
	{
	  /* Process `I'. */
	  if (type == sd.key)
	    {
	      sd.count += block_size;
	    }
	  else if (type == rd.key)
	    {
	      rd.size += block_size;
	      if (block_size > 1)
		fwrite (i, 1, block_size, os);
	      else
		PUTC (*i, os);
	    }
	}
      else
	/* tpye != new_type */
	{
	  /* Flush previous type header. */
	  if (type == sd.key)
	    {
	      /* Flush skip_data to file. */
	      fwrite (&sd, sizeof sd, 1, os);
	    }
	  else if (type == rd.key)
	    {
	      /* Update header of raw_data and go back to end of file. */
	      fseek (os, -rd.size - sizeof rd, SEEK_CUR);
	      fwrite (&rd, sizeof rd, 1, os);
	      fseek (os, 0, SEEK_END);
	    }

	  type = new_type;

	  /* Prepare new header and store `I'. */
	  if (type == sd.key)
	    {
	      sd.count = block_size;
	    }
	  else if (type == rd.key)
	    {
	      fwrite (&rd, sizeof rd, 1, os);	/* Placeholder. */
	      if (block_size > 1)
		fwrite (i, 1, block_size, os);
	      else
		PUTC (*i, os);
	      rd.size = block_size;
	    }
	}
    }
  return errno == 0;
}

/** Section:  Error handling */

/* Panic exit function, which is called if an fatal error occurs after
   patch(). (FIXME: the error message isn't entire right.) */

static void
bailout (char const *data_path, char const *msg)
{
  if (data_path)
    Aerr_print_msg ("Error while undumping from <%s>: %s\n", data_path, msg);
  else
    Aerr_print_msg ("Error: %s\n", msg);

  /* However, the library & the memory allocation should be ok, so
     we can exit reasonably */
  exit (EXIT_FAILURE);
}

static void
Aerr_data_file (char const *data_path, char const *msg)
{
  const char *hint =
    "==== Hint =============================================\n"
    " To make a dump file, invoke Emacs like this:\n"
    "  $ emacs -nl -batch -l loadup dump\n"
#ifdef AMIGA_V_EMACS_20_2
    " For using MBSK use the `no-mule\' argument like this:\n"
    "  $ emacs -nl -batch -l loadup dump no-mule"
#endif /* >=20.2 */
    "=======================================================\n";
  
  Aerr_print_msg ("Error while undumping from <%s>: %s\n\n%s\n",
		  data_path, msg, hint);
  exit (EXIT_FAILURE);
}


/** Section: module test */

#ifdef TEST

bool
ignore (const void *p, const void **next_change_result)
{
  static int m;

  /* tell caller that we don't know the next change */
  if (next_change_result)
    *next_change_result = 0;

  return (++m % 13 < 3);
}


int
main (int ac, char **av)
{
  FILE *stream = 0;

  if (ac > 1)
    {
      Begin = TEXT_START;
      End = TEXT_END;
    }
  stream = fopen ("/ram/x", "wb");

  if (!stream)
    perror ("fopen()"), exit (20);

  fputs ("save hunk...\n", stderr);
  if (!save_hunk_conditional (Begin, End, ignore, stream))
    perror ("write_hunk()"), exit (20);
  fputs ("...done\n", stderr);

  fclose (stream);
  stream = fopen ("/ram/x", "rb");
  if (!stream)
    perror ("fopen()"), exit (20);

  fputs ("restore hunk\n", stderr);
  if (!(restore_hunk (Begin, End, stream)))
    exit (20);
  fputs ("...done\n", stderr);

  return 0;
}

#endif /* TEST */


#ifndef TEST
#ifdef AMIGA_DUMP
Admp_tbl (amiga_dump) = {
  Admp_ndobj (a_dmp),
  Admp_ndobj (Admp_hunks),
  Admp_ndobj (Admp_tables),
  Admp_ndobj (Admp_hunk_table),
  0};
#endif
#endif

#endif /* AMIGA_DUMP || TEST */
