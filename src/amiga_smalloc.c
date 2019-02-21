/* amiga_smalloc.c - fast and safe storage manager for GNU Emacs.

   Author: Bert Winkelmann <bertw@in-brb.de> (1997)
   Features: sanity checks; stand-alone selftest; free configurable;
             save/restore storage in file; maintain statistics;
   Drawbacks: memory waste;
 */

#include <config.h>
#include "amiga_smalloc.h"

#ifdef emacs
#if 0
#include "lisp.h" /* for P_ used in blockinput.h */
#include "blockinput.h"
#endif /* 0 */
#else
#undef static
#endif
#undef LONGBITS
#include <exec/nodes.h>
#include <exec/lists.h>
#include <proto/exec.h>
#include <proto/alib.h>
#include <sys/types.h>
#include <stddef.h>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "amiga.h"		/* must behind stdio.h */
#include <machine/limits.h>

typedef struct storage_piece ssp;
typedef struct storage_block ssb;
typedef struct MinNode sln;

#ifdef emacs
/* malloc(3) may called before main. Therefore we must allow autoinit. */
#define ASMA_AUTO_INIT 1
#else
#define ASMA_AUTO_INIT 0
#endif

/*****************************************************************/
/* local functions */
static  void Asma_destroy_block (ssb * b);
static  ssb *Asma_create_block_single (size_t size, long attrib);
static  ssb *Asma_create_block (size_t s2_memb, size_t nmemb);
static  inline void Asma_ssb_invariant_check (ssb * b, const char *fn, int lnmb);
static  void *Asma_sys_alloc (size_t size, long attrib);
static  void Asma_sys_free (void *storage, size_t size);
static  void Asma_check_all_free_lists (void);
static  ssb *get_ssb (FILE *is);
static  void write_ssb (ssb *sb, FILE *os);
static inline  int const log2 (size_t size) __attribute__ ((const));

static const ssb *Asma_db_find_parent (const void *m); /* debug */
static int Asma_db_find_child (const ssb *parent, const void *m); /* debug */
static const ssp *Asma_db_find_child_address (const ssb *block, int i);	/* debug */

static void Asma_stat_waste_histo_update (size_t needed, size_t allocated);
/*****************************************************************/

/* Different modes of invariant checks. */
#define SSP_INVARIANT_FLAG  1
#define ASMA_CATCH_BUGS 1
#define ASMA_ALIGNED_P(a) ((bool)(((int)(a)) % ASMA_ALIGNMENT == 0))
#define NO_PRECOND 0
#define NO_POSTCOND 1
/* switch on/off zeroing of small objects in Asma_alloc() */
#define INITIALIZE_ALL_STORAGE 0

/* Fill memory like mungwall does. */
#ifdef FULLDEBUG
#define ASMA_DB_FILL 1
#endif

/* Switch on/of sanity check code for Asma_sys_alloc/free. */
/* Note: Its *very* cheap. Don't disable it! */
#define ASMA_SYS_ALLOC_COOKIES 1
/* bookkeeping of number of allocations, sums of allocated space, etc. */
#define ASMA_STAT 1
/* Statistics for use in gdb(1). Maybe slow. */
#define ASMA_DB_STAT 0

/* === debug aids === */

#include "amiga_defs.h"

/* OBSOLETE-bw/18-Apr-98: The caller is responsible to do necessary
   BLOCK_INPUT before invoking our alloc functions! */
#undef BLOCK_INPUT
#undef UNBLOCK_INPUT
#ifndef FULLDEBUG
int asma_sema;
#define BLOCK_INPUT { check_precond (asma_sema == 0); ++asma_sema; }
#define UNBLOCK_INPUT (--asma_sema)
#else /* FULLDEBUG */
#define BLOCK_INPUT
#define UNBLOCK_INPUT

#define ASMA_CATCH_BUGS 1
#endif /* FULLDEBUG */

#ifdef TEST
#undef ASMA_CATCH_BUGS
#define ASMA_CATCH_BUGS 1
#undef TEXT_FIRST
#define TEXT_FIRST ({ extern moncontrol(); (char*)moncontrol; })
#define Aerr_fail(fmt, args...) ({ fprintf (stderr, fmt, ## args); abort (); })

/* Avoid stdio macros in tests with a boundchecking tool. */
#define PUTC fputc
#define GETC fgetc

#else /* not TEST */
#define PUTC putc
#define GETC getc

#endif /* not TEST */

#undef min
#define min(a,b) (((a)<(b)) ? (a) : (b))
#undef max
#define max(a,b) (((a)>(b)) ? (a) : (b))
/* Test wether A is located in storage [S,E) */
#define  RANGE(a, s, e) (((char *)(a)) >= ((char *)(s)) && ((char *)(a)) < ((char *)(e)))

typedef struct stack_frame ssf;

struct stack_frame 
{
  u_int32_t data[0];		/* data[-1] ... data[-n] */
  struct stack_frame *prev;	/* previous/above frame */
  void *return_address;         /* the same as used by the RTS machine command */

  /* Getting upmost frame. */
#define SSF__CURR_FRAME() \
  ((ssf*)({register ssf *_fp; asm("movel fp,%0" : "=a" (_fp)); _fp; }))
    /* Get Caller. */
#define SSF__CALLER(sf) ((sf)->return_address)
    /* Test for init frame (usually main()) */
#define SSF__INIT_FRAME_P(sf) \
    ((int) ((char*)(sf)->return_address < TEXT_FIRST))
   /* Foreach. (from SF to init frame.) */
#define SSF__FOR(i,sf) for ((i)=(sf); ! SSF__INIT_FRAME_P(i); (i)=(i)->prev)
};

#define TEXT_OFFSET(addr) ((unsigned)((char*)(addr) - TEXT_START))

/* Print function call return locations as hex number list. */
/* The print from the frame START until main() or the frame numbered
   START+DEPTH (for DEPTH > 0).  If DEPTH is -1 then we will trace
   always until main().  The result is printed to stream OS without
   trailing newline.  The RESULT is the number of printed items. */
int
Aerr_print_backtrace (int start, int depth, FILE *os)
{
#ifdef OMIT_FRAME_POINTER
  return 0;
#else /* no OMIT_FRAME_POINTER */
  ssf *i=0, *prev=0;
  int r=0;

  check_precond (os);

  SSF__FOR(i, SSF__CURR_FRAME())
    {
      /* beware of -fomit-frame-pointer */
      if (!i || i == prev)
	return r;
      else
	prev = i;
      if (depth-- == 0)
	return r;
      if (--start < 0)
	{
	  fprintf (os, "0x%x ", TEXT_OFFSET (SSF__CALLER (i)));
	  ++r;
	}
    }
  return r;
#endif /* no OMIT_FRAME_POINTER */
}


  /* List utils. */
#define SLH__FOR(i,lh) \
for ((i)=(sln*)(lh)->lh_Head; (i)->mln_Succ; (i)=(i)->mln_Succ)
#define SLH__RFOR(i,lh) \
for ((i)=(sln*)(lh)->lh_TailPred; (i)->mln_Pred; (i)=(i)->mln_Pred)
#define SLN__FIRST_P(ln) ((bool)((ln)->mln_Pred->mln_Pred == 0))
#define SLN__LAST_P(ln) ((bool)((ln)->mln_Succ->mln_Succ == 0))
#define SLN__HEAD_P(ln) ((bool)((ln)->mln_Pred == 0))
#define SLN__TAIL_P(ln) ((bool)((ln)->mln_Succ  == 0))
#define SLH__RM_FOR(i, next, lh) \
for ((i)=(sln*)(lh)->lh_Head; ((next) = ((i)->mln_Succ)); (i)=(next))
#define SLH__empty_p(lh) ((bool)(((sln*)(lh)->lh_Head)->mln_Succ == 0))

struct storage_piece
  {
    ssb *parent;
    union
      {
	ssp *next;
	char data[0];

	long long align_longlong;
	double align_double;
	void *align_void_ptr;
      }
    u;

    /* Get the object containing the storage S. */
#define SSP__HEADER(s)  ((ssp*) ((char*)(s) - offsetof (ssp, u.data)))
    /* Get/Set the storage_block containing SP. */
#define SSP__PARENT(sp)  ((ssb*) ((sp)->parent))
#define SSP__SET_PARENT(sp,sb) ((void)((sp)->parent = (sb)))
    /* Get/Set the successor (only for *free* pieces). */
#define SSP__SUCC(sp)  ((ssp*) ((sp)->u.next))
#define SSP__SET_SUCC(sp, succ) ((void)((sp)->u.next = (succ)))
    /* Invariant checking. */
#if SSP_INVARIANT_FLAG
#define SSP__INVARIANT_INIT(sp)  ((void)(sp))
#define SSP__INVARIANT_P(sp) \
    ((bool) (SSB__DATA(SSP__PARENT(sp),0) <= (sp) \
	     && (sp) < SSB__DATA(SSP__PARENT(sp), SSP__PARENT(sp)->nmb_memb)))
#else /* not SS_PINVARIANT_FLAG */
#define SSP__INVARIANT_INIT(sp)  ((void)(sp))
#define SSP__INVARIANT_P(sp)  ((bool)true)
#endif /* not SS_PINVARIANT_FLAG */
  };


struct storage_block
  {
    sln node;                   /* link for intrusive lists */
    ssp *int_flist;             /* list of free data member */
    size_t size,		/* size of data */
    memb_size;			/* size of cotained data object type
                                   (incl. SSP_OVERHEAD) */
#if ASMA_STAT
    int stat_nget;		/* Count get calls. */
#endif
    short invariant_cookie;	/* for sanity checks */
    unsigned char memb_size2;	/* 2**memb_size2 == memb_size; */
    unsigned char flags;
#define SSB_NORELOC_F 1		/* Not count for relocation. */
#define SSB_NOSAVE_F 1		/* Exclude it in Asma_save_malloc() */
    long attribs;		/* mem-attribs (used for singles) */
    short nmb_memb,		/* maximal number of data objects */
    nmb_used;			/* number of data objects in use */

    /* Storage block.
       note: sizeof data[0] == memb_size */
    struct storage_piece data[0];

#if ASMA_STAT
#define SSB__INCR_ACCESS(sb) ((void)++(sb)->stat_nget)
#define SSB__NACCESS(sb) ((int) (sb)->stat_nget)
#else
#define SSB__INCR_ACCESS(sb)
#endif

    /* Get address of Nth element of SB->DATA. */
#define  SSB__DATA(sb, n)  ((ssp*)((char*)(sb)->data + (sb)->memb_size *(n)))
    /* Get sizeof SB */
#define  SSB__SIZE(sb)  ((size_t)((sb)->size + SSB_OVERHEAD))

    /* Get usage numbers of SB */
#define  SSB__FULL_P(sb) ((bool)((sb)->nmb_used == 0))
#define  SSB__EMPTY_P(sb) ((bool)((sb)->int_flist == NULL))

    /* Element access. */
#define  SSB__GET(sb,sp) ({ \
			      local_precond (sb); \
			      SSB__INCR_ACCESS(sb); \
				++(sb)->nmb_used; \
				  sp = (sb)->int_flist; \
				    (sb)->int_flist = SSP__SUCC((sb)->int_flist); \
			  })
#define  SSB__PUT(sb,sp) ({ \
			      local_precond (sb && sp && sb == SSP__PARENT(sp)); \
				--(sb)->nmb_used; \
				  SSP__SET_SUCC ((sp), (sb)->int_flist); \
				    (sb)->int_flist = (sp); \
			  })
    
  /* Is SB an array? */
#define  SSB__ARRAY_P(sb) ((bool)(((sb)->nmb_memb > 1) ? true : false))

  /* Object invariant */
#define  SSB__INVARIANT_INIT(sb)  ((void)(((sb)->invariant_cookie = SSB_INVARIANT_COOKIE)))
#define  SSB__INVARIANT_P(sb)  \
  ((bool)(sb != 0 \
	  && (((sb)->memb_size & 3) == 0) \
	  && (sb)->invariant_cookie == SSB_INVARIANT_COOKIE \
	  && (sb)->node.mln_Succ->mln_Pred == (&(sb)->node) \
	  && (sb)->node.mln_Pred->mln_Succ == (&(sb)->node) \
	  && ((sb)->int_flist == 0 \
	      || (SSB__DATA((sb),0) <= (sb)->int_flist \
		  && (sb)->int_flist < SSB__DATA((sb), (sb)->nmb_memb)))))

/* Test wether M is located in data */
#define  SSB__RANGE_P(sb, m) \
    (bool)((char*)SSB__DATA((sb),0) <= (char*)(m) \
	   && (char*)(m) < (char*)SSB__DATA((sb), (sb)->nmb_memb))

/* Intrusive list. */
#define SSB__SUCC(sb) ((ssb*)(sb)->node.mln_Succ)
#define SSB__PRED(sb) ((ssb*)(sb)->node.mln_Pred)
  };



enum
  {
    SSB_OVERHEAD = offsetof (ssb, data),
    SSP_OVERHEAD = offsetof (ssp, u.data),

    /* Cookie for invariant check. */
    SSB_INVARIANT_COOKIE = 0x5555,
    SSP_INVARIANT_COOKIE = 0x54545454,
    ASMA_SYS_ALLOC_START_COOKIE = 0x55555555,
    ASMA_SYS_ALLOC_END_COOKIE = 0x55555555,
    
    /* Alignment of Asma_alloc() results. */
    ASMA_ALIGNMENT = 4,

    MINLG = 2,
  };

/* Configure number of objects allocated together. */
/* For allocating one object of size X

   M = Asma_nmb_memb[N]; assert (2 ** (N-1) < X && X <= 2 ** N);
  
   if  M==0 then allocate only one object with size X;
   else if M>0 then allocate M objects with size N; */
   
static int 
Asma_nmb_memb[] = 
 {
   0,				/* 1 */
   0,				/* 2 */
   0,				/* 4 */
   220,				/* 8 */
   255,				/* 16 */
   255,				/* 32 */
   127,				/* 64 */
   63,				/* 128 */
   31,				/* 256 */
   15,				/* 512 */
   31,				/* 1K */
   7,				/* 2K */
   3,				/* 4K */
   3,				/* 8K */
 };

enum { MAXLG = sizeof Asma_nmb_memb / sizeof (int[1]) };

/* Hook to apply custom test on memory block allocated by system.*/
/* If it fails Asma_alloc() will return NULL.  This hook is intended
   as optimization to avoid checking each result of Asma_alloc()
   seperatly by the caller. */
bool (*Asma_verify_storage_hook) (void *begin, void *end);

struct
  {
    bool initialized;		/* Set by init_module(). */
    bool panic_flag;		/* Lock module if corrupt. */
    bool exit_flag;		/* Locking Asma_free() + avoid recursive exits. */
#ifdef emacs
    long addr_bit_mask;		/* Cache, tracks Asma_sys_alloc()'ed "range".*/
#endif
    FILE *logstream;		/* assigned to path in $EMACS_ASMA_LOG */
    int in_locate_count;	/* avoids recursion in error checking */
    struct
      {
	size_t total_sys_size;  /* sum of unfreed Asma_alloc()'ated
				   storage */
	size_t alloc_counter;   /* number of succesful calls to Asma_alloc() */
      } stat;
//    struct List empty_blocks;	/* List of full blocks */
#define empty_blocks free_blocks[0]
    struct List free_blocks[MAXLG]; /* List of blocks with some free storage. */
  }
a_sma;

/* Visit all storages blocks.  */
/* `I' is the iterator of type <ssb*>.  HELP_IDX is an integer helper
   variable.  (Not so good to have a 1001 different macros.)
   -bw/3-Sep-97 */
#define ASMA__FOR(i, help_idx) \
for ((k)=0; (k) < MAXLG; ++(k)) \
     for ((i) = (ssb*)a_sma.free_blocks[(k)].lh_Head; SSB__SUCC(i); (i)=SSB__SUCC(i))


/* === debug aids === */

/* Check address for being a member of our storage.  */
bool
Asma_member_p (const void *a)
{
  register ssb *i;
  register int k;
 
  /* check all lists */
  if (a)
    ASMA__FOR (i, k)
      if (SSB__RANGE_P (i, a))
	return true;

  return false;
}

/* Get Info about A:
   bit 0 set => is member
   bit 1 set => is in free list
   bit 2 set => is an single object
*/
int
Asma_address_info (const void *a)
{
  register ssb *i;
  register int k;
  register ssp *sp;

  /* check all lists */
  if (a)
    ASMA__FOR (i, k)
      if (SSB__RANGE_P (i, a))
	if (!SSB__ARRAY_P (i))
	  return (01 | 04);
	else
	  {
	    for (sp = i->int_flist; sp; sp = SSP__SUCC (sp))
	      if (RANGE (a, sp, sp + i->memb_size))
		return (01 | 02);
	    return 01;
	  }
  return 0;
}


static void
Asma_ssb_print (FILE * stream, ssb * b)
{
  check_precond (stream && b);

  fprintf (stream,
	   " (ssb*) %p => {\n"
	   " struct MinNode {\n"
	   "  mln_Succ = %p\n"
	   "  mln_Pred = %p\n"
	   "}\n"
	   "invariant_cookie = 0x%x\n"
	   "size = %lu\n"
	   "memb_size = %lu\n"
	   "memb_size2 = %u\n"
	   "nmb_memb = %u\n"
	   "nmb_used = %u\n"
	   "data = %p ...\n",
	   b,
	   b->node.mln_Succ,
	   b->node.mln_Pred,
	   b->invariant_cookie,
	   b->size,
	   b->memb_size,
	   b->memb_size2,
	   b->nmb_memb,
	   b->nmb_used,
	   b->data);
}

/* Called by possible inlined function.  splitted for performance
   reasons. -bw/25-Apr-98*/
static void
Asma_ssb_invariant_check_failed (ssb * b, const char *fn, int line)
{
  /* Panic.  Lock module against furter cleanup to avoid
     possible damage caused by dangling pointers. */
  a_sma.panic_flag = true;
  fprintf (stderr, "%s: %s:%d ### Corrupt memory block. ###\n\n",
	   PROGNAME, fn, line);
  Asma_ssb_print (stderr, b);
  Aerr_print_backtrace (0, 100, stderr);
  abort ();
}

static inline void
Asma_ssb_invariant_check (ssb * b, const char *fn, int line)
{
  local_precond (b && fn);

  if (!SSB__INVARIANT_P (b))
    Asma_ssb_invariant_check_failed (b, fn, line);
}

/* === code ===*/
/* module cleanup */
/* Its allowed to call this in uninitalized state. */
void
Asma_exit_module ()
{
  /* Free all memory.  Note that the blocks may be corrupt! */
  sln *i = 0, *next = 0;
  int k=0;
  
  if (a_sma.exit_flag && !a_sma.initialized)
    return;

  a_sma.initialized = false;
  a_sma.exit_flag = true;

  /* Do check, free content and reinit all storage lists.  */

  for (k=0; k < MAXLG; ++k) 
    {
      SLH__FOR (i,  &a_sma.free_blocks[k])
	Asma_ssb_invariant_check ((ssb*)i, __PRETTY_FUNCTION__, __LINE__);
      SLH__RM_FOR (i, next, &a_sma.free_blocks[k])
	Asma_sys_free (i, SSB__SIZE ((ssb*)i));
      NewList (&a_sma.free_blocks[k]);
    }
}

/* module init */
/* Its not allowed to call this if already initialized.  */
bool
Asma_init_module ()
{

  check_precond (ASMA_AUTO_INIT || !a_sma.initialized);
  if (ASMA_AUTO_INIT && a_sma.initialized)
    return true;

  if (a_sma.panic_flag)
    return false;

  NewList (&a_sma.empty_blocks);
  {
    int i=0; 
    for (i=0; i < MAXLG; ++i)
      NewList (&a_sma.free_blocks[i]);
  }

  atexit (Asma_exit_module);	/* XXX */

  {
    const char *log;
    if ((log = getenv ("EMACS_ASMA_LOG")))
      a_sma.logstream = fopen (log, "w");
  }

  a_sma.initialized = true;
  return true;
}

/* If FILE is not NULL open a logstream with fopen(FILE, MODE).  If
   MODE is NULL then it uses "w".  If FILE is NULL then only close the
   existing logstream.  The result is false if fopen fails otherwise
   it will be true. */
bool
Asma_log_stream (const char *file, const char *mode)
{
  if (a_sma.logstream)
    {
      fclose (a_sma.logstream);
      a_sma.logstream = 0;
    }

  if (file && !(a_sma.logstream = fopen (file, mode ? mode : "w")))
    return false;

  return true;    
}

/* Wrapper for AllocMem(exec).  */
/* The storage will can be verified with Asma_verify_storage_hook.  */
static void *
Asma_sys_alloc (size_t size, long attrib)
{
  char *r=0;  

#if ASMA_SYS_ALLOC_COOKIES
  size = (size + 8 + 3) & ~3;	/* add sanity cookies */
#endif

  r = AllocMem (size, (attrib | MEMF_CLEAR));
  if (a_sma.logstream)
    fprintf (a_sma.logstream, "+ %p %lu MEMF=%3ld\n", r, size, attrib);

  if (!r)
    goto out_of_memory;

  if (Asma_verify_storage_hook && !Asma_verify_storage_hook (r, r+size))
    {
      bzero (r, size);
      FreeMem (r, size);
      if (a_sma.logstream)
	fprintf (a_sma.logstream, "- %p %lu (unusable address) (hook:%p)\n",
		 r, size, &Asma_verify_storage_hook);
      goto out_of_memory;
    }

  assert (r != 0);
#ifdef emacs
  a_sma.addr_bit_mask |= ((long)r | (long)((char*)r + size));
#endif
#if ASMA_STAT
  a_sma.stat.total_sys_size+=size;
#endif

#if ASMA_SYS_ALLOC_COOKIES
  *(long*)r = ASMA_SYS_ALLOC_START_COOKIE;
  *(long*)(r + size - 4) = ASMA_SYS_ALLOC_END_COOKIE;
  return r + 4;
#else
  return r;
#endif

 out_of_memory:
  {
    extern int initialized;
    /* RMS told me that malloc() never fails while building emacs.
       To protect us against code which does not check result of
       malloc() (like in init_interval()) we call exit() here. Note:
       We assume that all data structures are valid. -bw/20-May-98 */
    if (!initialized)
      {
	write (2, "emacs: out of memory\n", 21);
	exit (1);
      }
    return 0;
  }
}

/* Wrapper for FreeMem(exec). */
static void
Asma_sys_free (void *storage, size_t size)
{

  local_precond (storage && size > 0);
  
#if ASMA_SYS_ALLOC_COOKIES
  {
    bool bad_start, bad_end;

    size = (size + 8 + 3) & ~3;	/* add sanity cookies */
    storage = (char *)storage - 4;

    bad_start = ((*(long*)storage != ASMA_SYS_ALLOC_START_COOKIE));
    bad_end  = (*(long*)(storage + size - 4) != ASMA_SYS_ALLOC_END_COOKIE);
    if (bad_start || bad_end)
      {
	const char *msg = 
	  ((!bad_start) ? "end of block corrupt (start seems ok)" 
	   : ((!bad_end) ? "start of block corrupt (end seems ok)"
	      : "both start and end of block corrupt"));
	/* FIXME-bw/28-Aug-97: better print only a warning and leave the
	   decision to the user */
	Aerr_fail ("Panic: sys_free(%p,%lu): %s!\n", storage, size, msg);
	abort();		/* NOTREACHED */
      }
  }
#endif

  bzero (storage, size);	/* security */
  FreeMem (storage, size);

#if ASMA_STAT
  a_sma.stat.total_sys_size-=size;
#endif

  if (a_sma.logstream)
    fprintf (a_sma.logstream, "- %p %lu\n", storage, size);
}

/* Create storage for one big object.  */
/* SIZE is *not* rounded to power of two. ATTRIB will passed to
   AllocMem(exec).  */
static ssb *
Asma_create_block_single (size_t size, long attrib)
{
  ssb *r = 0;
  size_t s = 0;

  s = size + SSB_OVERHEAD;
  r = Asma_sys_alloc (s, attrib);
  if (r)
    {
      if (a_sma.logstream)
	{
	  ssf *i=0, *prev=0;
	  SSF__FOR(i, SSF__CURR_FRAME())
	    {
	      /* beware of -fomit-frame-pointer */
	      if (!i || i == prev)
		break;
	      else
		prev = i;
	      fprintf (a_sma.logstream, "0x%x ", TEXT_OFFSET (SSF__CALLER (i)));
	    }
	  fputs("\n", a_sma.logstream);
	}
  
      r->size = size;
#if ASMA_STAT
      r->stat_nget = 0;
#endif
      r->memb_size = size;
      r->memb_size2 = ~0;
      r->attribs = attrib;
      r->nmb_memb = 1;
      r->nmb_used = 1;
      r->flags = 0;
      SSB__INVARIANT_INIT (r);
      AddHead (&a_sma.empty_blocks, (struct Node*)&r->node);
#if ASMA_CATCH_BUGS
      assert (SSB__INVARIANT_P (r)); 
#endif
      SSP__SET_PARENT (SSB__DATA (r, 0), r);
      SSP__INVARIANT_INIT (SSB__DATA (r, 0));
    }

  check_postcond (!r || SSB__INVARIANT_P (r));
  return r;
}



/* `2 ** S2_MEMB' is the size of the single object + SSP_OVERHEAD.  NMEMB is the
   number of objects to allocate. */
static ssb *
Asma_create_block (size_t s2_memb, size_t nmemb)
{
  ssb *r = 0;
  size_t smemb = 0;
  size_t size = 0;

  local_precond (s2_memb > 2 && nmemb); /* Honestly, it cannot handle
					   blocks smaller than 8 byte */
  smemb = 1 << s2_memb;
  size = smemb * nmemb + SSB_OVERHEAD;
  
  r = Asma_sys_alloc (size, 0);
  if (r)
    {
      int i = 0;

      r->size = smemb * nmemb;
      r->memb_size = smemb;
#if ASMA_STAT
      r->stat_nget = 0;
#endif
      r->memb_size2 = s2_memb;
      r->nmb_memb = nmemb;
      r->nmb_used = 0;
      r->int_flist = SSB__DATA (r,0);
      r->flags = 0;

      SSB__INVARIANT_INIT (r);
      for (i = 0; i < nmemb; ++i)
	{
	  SSP__SET_PARENT (SSB__DATA (r, i), r);
	  SSP__SET_SUCC (SSB__DATA (r, i), SSB__DATA (r, i + 1));
	  SSP__INVARIANT_INIT (SSB__DATA (r, i));
#if ASMA_CATCH_BUGS
	  assert (SSP__INVARIANT_P  (SSB__DATA (r, i)));
#endif
	}
      SSP__SET_SUCC (SSB__DATA (r, nmemb - 1), 0);
    }
  AddHead (&a_sma.free_blocks[s2_memb], (struct Node*)&r->node);

  check_postcond (SSB__INVARIANT_P (r));
  return r;
}


/* Unlink a block from all lists and free its storage. */

static void
Asma_destroy_block (ssb * b)
{
  local_precond (b!=0);

#if ASMA_CATCH_BUGS
  if (SSB__ARRAY_P (b))
    {
      int n=0;
      ssp *i;
      /* check correct number of free pieces */
      for (i=b->int_flist; i; i = SSP__SUCC(i))
	++n;
      assert (n==b->nmb_memb);
    }
#endif
  assert (SSB__INVARIANT_P(b));
  Remove ((struct Node*)&b->node);
  Asma_sys_free (b, SSB__SIZE (b));
}

/* this is a very special log2() function that knows the upper bound
   of its argument, and also automatically rounds to the next upper
   power of two */

static inline int const
log2 (size_t size)
{
  unsigned pow = MAXLG;
  unsigned lower_bound = 1 << (MAXLG - 1);
  
  while (1)
    {
      if (size > lower_bound)
        return pow;

      lower_bound >>= 1;
      pow--;
    }
}

/* Allocate a piece of raw storage. */
/* The result is aligned on 4 byte boundary. */
void *
Asma_alloc (size_t size)
{
  char *r = 0;
  int exp=0;
  bool single = false;
  bool array = false;

  check_precond (ASMA_AUTO_INIT || a_sma.initialized);
#if 1
  check_precond ((ssize_t)size >= 0);
#endif

  /* prevent possible overflow when adding overhead to size */
  if ((ssize_t)size < 0)
    return 0;

#define return you lose
#ifdef emacs
  BLOCK_INPUT;
#endif

  if (ASMA_AUTO_INIT && !a_sma.initialized && !Asma_init_module ())
    goto done;

  if (a_sma.exit_flag || a_sma.panic_flag)
    goto done;

  /* guarantee long sizes (so we can use CopyMemQuick in realloc) */
  size = (size + 3 + SSP_OVERHEAD) & ~3; /* next highest multiple of 4 */

#ifdef emacs
  /* Fast test for the most frequent allocation by Emacs: 40 Byte (+ 4
     byte overhead).  Note: This sizes are allocated only for short
     times.  There is no point in saving RAM by introducing a special
     block size of 44.  It's only a speed issue. */
  if (32 < size && size <= 64)
    {
      exp = 6;
      array = true;
    }
#endif

  if (!array)
    if ((1<<(MAXLG-1) < size) || Asma_nmb_memb[exp = log2 (size)] == 0)
      single = true;
    else
      array = true;
  
  if (single)
    {
      /* Allocate single big object. */
      ssb *b = Asma_create_block_single (size, 0);
      if (b)
	{
#if ASMA_CATCH_BUGS
	  assert (SSB__INVARIANT_P (b));
#endif
	  r = b->data->u.data;
#if (ASMA_DB_FILL)
      {
	int r_lsize = (size-SSP_OVERHEAD) >> 2;
	int i;
	for (i=0; i < r_lsize; ++i)
	  ((int*)r)[i] = 0xdeadf00d;
      }
#endif
	}
    }

  if (array)
    {
      ssp *t = 0;
      ssb *b = 0;
#if ASMA_DB_STAT
      /* Note: SIZE is already adjusted to be a mutiple of 4 */
      Asma_stat_waste_histo_update (size, (1 << exp));
#endif
      /* Get storage from existent or newly created block. */
      /*If block becomes empty remove it from freelist. */
      if (a_sma.free_blocks[exp].lh_Head->ln_Succ)
	b = (ssb*) a_sma.free_blocks[exp].lh_Head;
      else
	{
	  b = Asma_create_block (exp, Asma_nmb_memb[exp]);
	  if (!b)
	    goto done;
#if ASMA_CATCH_BUGS
	  assert (SSB__INVARIANT_P (b));
#endif
	}
      SSB__GET (b, t);
#if ASMA_CATCH_BUGS
      assert (SSP__INVARIANT_P (t));
      assert (SSB__INVARIANT_P (SSP__PARENT(t)));
#endif
      if (SSB__EMPTY_P (b))
	{
	  /* move free_blocks[n] => empty_blocks */
	  Remove ((struct Node*)&b->node);
	  AddHead (&a_sma.empty_blocks, (struct Node*)&b->node);
	}
      r = t->u.data;
#if !(ASMA_DB_FILL)
#if (INITIALIZE_ALL_STORAGE)
      bzero (r->u.data, size);
#endif
#else
      {
	int r_lsize = ((1 << exp)>>2) - SSP_OVERHEAD;
	int i;
	for (i=0; i < r_lsize; ++i)
	  ((int*)r)[i] = 0xdeadf00d;
      }
#endif
    }

#if ASMA_STAT
  if (r)
    ++a_sma.stat.alloc_counter;
#endif  

 done:
#ifdef emacs
  UNBLOCK_INPUT;
#endif
#undef return

  check_postcond (r==0 || ASMA_ALIGNED_P (r));
  return r;
}

void *
Asma_special_alloc (size_t size, long attribs, unsigned char flags)
{
  char *r = 0;

  check_precond (ASMA_AUTO_INIT || a_sma.initialized);

#define return you lose
#ifdef emacs
  BLOCK_INPUT;
#endif

  if (ASMA_AUTO_INIT && !a_sma.initialized && !Asma_init_module ())
    goto done;

  if (a_sma.exit_flag || a_sma.panic_flag)
    goto done;

  if (!a_sma.initialized)
    Asma_init_module ();

  /* guarantee long sizes (so we can use CopyMemQuick in realloc) */
  size = (size + 3 + SSP_OVERHEAD) & ~3; /* next highest multiple of 4 */
  
  {
    /* Allocate single big object. */
    ssb *b = Asma_create_block_single (size, attribs);
    if (b)
      {
	r = b->data->u.data;
	b->flags |= flags;
      }
  }
#if ASMA_STAT
  if (r)
    ++a_sma.stat.alloc_counter;
#endif  

 done:
#ifdef emacs
  UNBLOCK_INPUT;
#endif
#undef return

  check_postcond (r==0 || ASMA_ALIGNED_P (r));
  return r;
}

#ifdef emacs
/* XXX - low memory allocator. */
/* This was formerly used for intervals, but now it's only used by
   amiga_window_input.c -bw/08-Apr-99 */
void *
Asma_low_alloc (size_t size)
{
#ifdef emacs
  /* If we are initialized, there is no need to keep the low-memory
     attribute inside a memory block. Therefore we try the normal
     memory first.  To make it fast, we use a global bitmask written
     by Asma_sys_alloc().  This is more restrict than it must be, but
     since we have 28 valbits there is no need for a sophisticated
     solution.  -bw/29-Oct-97 */
  {
    extern int initialized;	/* Use 24BITDMA always before dump. */
    void *result;

    /* don't allow MEMF_24BITDMA in dumpfile except for intervals
       -bw/08-Apr-99 */
    if (!initialized)
      abort (); 

    if (!(a_sma.addr_bit_mask & ~((1<<VALBITS)-1)))
      {
	result = Asma_alloc (size);
	if (!(a_sma.addr_bit_mask & ~((1<<VALBITS)-1)))
	  return result;
	Asma_free (result);
      }
  }
#endif
  return Asma_special_alloc (size, MEMF_24BITDMA, 0);
}

/* XXX - special memory allocator. needed for intervals */
void *
Asma_alloc_iv (size_t size)
{
  void *result;
#ifdef emacs
  /* If we are initialized, there is no need to keep the low-memory
     attribute inside a memory block. Therefore we try the normal
     memory first.  To make it fast, we use a global bitmask written
     by Asma_sys_alloc().  This is more restrict than it must be, but
     since we have 28 valbits there is no need for a sophisticated
     solution.  -bw/29-Oct-97 */
  if (a_sma.logstream)
    fprintf (a_sma.logstream, "* allocate interval...\n");
  {
    extern int initialized;	/* Use 24BITDMA always before dump. */

    if (initialized)
      {
	result = Asma_alloc (size);
	if (!result)
	  return 0;
	if (Ama_verify_storage_iv (result, (char *)result + size))
	  return result;
	Asma_free (result);
      }
  }
#endif
  {
    bool (*old_hook) (void *begin, void *end) = Asma_verify_storage_hook;
    Asma_verify_storage_hook = &Ama_verify_storage_iv;
    result = Asma_special_alloc (size, MEMF_24BITDMA, 0);
    Asma_verify_storage_hook = old_hook;
  }
  return result;
}
#endif /* emacs */

/*  Allocate memory which will ignored by locate and save functions. */
void *
Asma_nd_alloc (size_t size)
{
  return Asma_special_alloc (size, 0, (SSB_NOSAVE_F | SSB_NORELOC_F));
}

/* common implementation code for Asma_free() and realloc() */
static inline void
Asma_free2 (ssb *b, ssp *p)
{
  local_precond (b && p);

  if (! SSB__ARRAY_P (b))
    {
      Asma_destroy_block (b);
    }
  else
    {
#if !(ASMA_DB_FILL)
#if (INITIALIZE_ALL_STORAGE)
      bzero (p->u.data, b->memb_size - SSP_OVERHEAD);
#endif
#else
      {
	int r_lsize = ((1 << b->memb_size2)>>2) - SSP_OVERHEAD;
	int i;
	for (i=0; i < r_lsize; ++i)
	  ((int*)p->u.data)[i] = 0xdeadbeaf;
      }
#endif
      /* Put back piece to block.  Free block if becomes full. */
      if (SSB__EMPTY_P (b))
	{
	  struct List *free_list = &a_sma.free_blocks[b->memb_size2];

	  if (!SLH__empty_p (free_list) && SSB__FULL_P ((ssb*)free_list->lh_Head))
	    Asma_destroy_block ((ssb*) free_list->lh_Head);
	  SSB__PUT (b, p);
	  /* move  empty_blocks => free_blocks[n] */
	  Remove ((struct Node*)&b->node);
	  AddHead (free_list, (struct Node*)&b->node);
	}
      else
	{
	  SSB__PUT (b, p);
	  /* Destroy block if unused and not alone in its list. */
	  if (SSB__FULL_P (b)
	      && !(SLN__FIRST_P (&b->node) 
		   && SLN__LAST_P (&b->node)))
	    Asma_destroy_block (b);
	}
    }
}

/* Like free(3). */
/* S must be a result of Asma_alloc() or NULL */
void
Asma_free (void *s)
{
  ssp *p = 0;
  ssb *b = 0;

  check_precond (s==0 || ASMA_ALIGNED_P (s));
#if 0
  check_precond (a_sma.exit_flag || a_sma.initialized);
#else
  check_precond (a_sma.initialized);
#endif

#define return you lose
#ifdef emacs
  BLOCK_INPUT;
#endif

#if 0
  /* Do nothing if we had already made a clenanup. (Its must be a call
     from external cleaup code.) */
  if (a_sma.exit_flag)
    goto done;
#endif

  if (s == 0 || a_sma.panic_flag)
    goto done;

  /* Retrieve header and container block. */
  p = SSP__HEADER (s);
  check_ref (SSP__INVARIANT_P (p));

#ifdef CATCH_BUGS
  assert (SSP__PARENT(sp) == Asma_db_find_parent (sp));
#endif /* not CATCH_BUGS */

  b = SSP__PARENT (p);
  check_ref (b != 0);
  Asma_ssb_invariant_check (b, __PRETTY_FUNCTION__, __LINE__);

  Asma_free2 (b, p);

 done:
#ifdef emacs
  UNBLOCK_INPUT;
#endif
#undef return
  return;
}

/* Like realloc(3). */
void *
Asma_realloc (void *p, size_t size)
{
  void *r = 0;

  check_precond (p==0 || ASMA_ALIGNED_P (p));
  check_precond (ASMA_AUTO_INIT || a_sma.initialized);

#if 1
  check_precond ((ssize_t)size >= 0);
#endif
  /* prevent possible overflow when adding overhead to size */
  if ((ssize_t)size < 0)
    return 0;

#define return you lose

  if (ASMA_AUTO_INIT && !a_sma.initialized && !Asma_init_module ())
    goto done;
  
  if (size == 0 && p)
    {
      Asma_free (p);
      goto done;
    }

  if (!p)
    {
      r = Asma_alloc (size);
      goto done;
    }
  else
    {
      ssp *sp;
      ssb *sb;

      sp = SSP__HEADER (p);
      check_ref (sp && SSP__INVARIANT_P (sp));

      sb = SSP__PARENT (sp);
      check_ref (sb != 0);
      Asma_ssb_invariant_check (sb, __PRETTY_FUNCTION__, __LINE__);

#ifdef ASMA_FAST_REALLOC
      if (size <=  sb->memb_size - SSP_OVERHEAD)
	{
	  r=p;
	  goto done;
	}
#endif
      /* guarantee long sizes (so we can use CopyMemQuick in realloc) */
      size = (size + 3) & ~3; /* next highest multiple of 4 */
      r = Asma_alloc (size);
      if (!r)
	goto done;

      CopyMemQuick (p, r, min (size, sb->memb_size - SSP_OVERHEAD));
      Asma_free2 (sb, sp);
    }
 done:
#undef return
  check_postcond (r==0 || ASMA_ALIGNED_P (r));
  return r;
}

#ifdef TEST

static void
Asma_print_mem_info (ssb *b)
{
  int unused = b->nmb_memb - b->nmb_used;
  int wasted = unused * b->memb_size;

  local_precond (b && a_sma.logstream);

  fprintf (a_sma.logstream,
	   "%p memb_size: %lu \tused: %u \tfree: %d \twasted: %d\n",
	   b, b->memb_size, b->nmb_used, unused, wasted);
}  

void
Asma_mem_info()
{
  ssb *i=0;
  int k=0;
  size_t total_arr=0, unused_arr=0, total_single=0;

  if (!a_sma.logstream)
    return;
  
  fprintf (a_sma.logstream, "=============arrays========================\n");
  ASMA__FOR (i, k)
    if (SSB__ARRAY_P(i))
	{
	  ssb *b= i;
	  Asma_print_mem_info(b);
	  total_arr += b->nmb_memb * b->memb_size;
	  unused_arr += (b->nmb_memb - b->nmb_used) * b->memb_size;
	}
  
  fprintf (a_sma.logstream, "=============singles========================\n");
  ASMA__FOR (i, k)
    if (!SSB__ARRAY_P(i))
      {
	ssb *b= i;
	Asma_print_mem_info(b);
	total_single += b->memb_size;
      }

  fprintf (a_sma.logstream, "==============================================\n");
  fprintf (a_sma.logstream, "tot_alloc: %lu\nalloc_counter: %lu\n",
	   a_sma.stat.total_sys_size, a_sma.stat.alloc_counter);

  fprintf (a_sma.logstream, "tot_array: %lu\nunused_array: %lu\ntotal_single: %lu\n",
	   total_arr, unused_arr, total_single);
}

#endif

/* Retuns imaginary offset of address */
/* This makes a uniqe offset like number from M. If M is located in an
   block with flag ASMA_NORELOC_F set then the result is -1.  If is M
   not located in any block the result will be also -1.  The results
   stays valid upto the next call of Asma_create_block() or calls to
   both Asma_create_block_single or Asma_destroy_block on blocks
   with  ASMA_NORELOC_F clear.
   NEW-bw/21-Jan-98: returns -2 if M is located in a free SSP */
long
Asma_locate_address (const void *m)
{
  long r=-1, offset=0;
  sln *ii;
  int i;

  check_precond (a_sma.initialized);
  check_precond (m!=0);

  for (i=0; i < MAXLG; ++i)
    SLH__RFOR (ii, &a_sma.free_blocks[i])
      {
	if (((ssb*)ii)->flags & SSB_NORELOC_F)
	  continue;		/* skip this block */
	if (!SSB__RANGE_P ((ssb*)ii, m))
	  offset += ((ssb*)ii)->size;
	else
	  {
	    offset += (char*)m - (char*)SSB__DATA ((ssb*)ii, 0);
	    r = offset;

	    /* Return error if M is located in a free piece */
	    if (SSB__ARRAY_P ((ssb*) ii))
	      {
		register ssp *sp;

		for (sp = ((ssb*)ii)->int_flist; sp; sp = SSP__SUCC (sp))
		  if (RANGE (m, sp, (char *)sp + ((ssb*)ii)->memb_size))
		    return -2;
	      }
	    
	    goto done;
	  }
      }

  return -1;			/* error */
 done:
  if (a_sma.in_locate_count == 0) /* avoid recursion */
    {
      ++a_sma.in_locate_count;
      check_postcond ((char *) Asma_locate_offset (offset) == (char*)m);
      --a_sma.in_locate_count;
    }
  return r;
}

/* The counterpart to Asma_locate_address().  */
void *
Asma_locate_offset (long offset)
{
  char *addr=0;
  int i;
  sln *ii;
  long offs = offset;

  check_precond (a_sma.initialized);

  if (offset < 0)
    return 0;			/* error */

  for (i=0; i < MAXLG; ++i)
    SLH__RFOR (ii, &a_sma.free_blocks[i])
      {
	if (((ssb*)ii)->flags & SSB_NORELOC_F)
	  continue;		/* skip this block */
	if ((offs -= ((ssb*)ii)->size) < 0)
	  {
	    addr = (char*)SSB__DATA ((ssb*)ii, 0) + ((ssb*)ii)->size + offs;
	    goto done;
	  }
      }

  return 0;			/* error */
 done:
  if (a_sma.in_locate_count == 0) /* avoid recursion */
    {
      ++a_sma.in_locate_count;
      check_postcond (Asma_locate_address (addr) == offset);
      --a_sma.in_locate_count;
    }
  return addr;
}

/* build fast lookup table */
struct {
  size_t nmemb;
  struct offs_addr_map_tbl kv_tbl[150];
} asma_offset_table;

struct offs_addr_map *
Asma_offset_table ()
{
  struct offs_addr_map *v= (void *) &asma_offset_table;	/* RESULT */
  size_t idx=0;
  int i;
  sln *ii;
  long offs = 0;

  check_precond (a_sma.initialized);

  for (i=0; i < MAXLG; ++i)
    SLH__RFOR (ii, &a_sma.free_blocks[i])
      {
	if (((ssb*)ii)->flags & SSB_NORELOC_F)
	  continue;		/* skip this block */
	v->kv_tbl[idx].offs = offs;
	v->kv_tbl[idx].addr = (long*)SSB__DATA ((ssb*)ii, 0);
	v->kv_tbl[idx].size = ((ssb*)ii)->size;
	++idx;
	if (idx >= 150)
	  {
	    v->nmemb = idx;
	    return v;
	  }
	offs += ((ssb*)ii)->size;
      }
  v->nmemb = idx;
  return v;
}


/* Structure representing a SSB header in persistent store. */
struct pers_ssb
{
  size_t size, nmb_memb, memb_size;
  union {
    unsigned char memb_size2;
    long attribs;
  } u;
};

/* Save a SSB to a file */
static void
write_ssb (ssb *sb, FILE *os)
{
#define wr(x) (fwrite (&(x), sizeof (x), 1, os)) 
#define wr2(x,sz)  (fwrite (&(x), (sz), 1, os)) 

  struct pers_ssb psb;
  int i;

  local_precond (a_sma.initialized);
  local_precond (sb && os);

  psb.size = sb->size;
  psb.memb_size = sb->memb_size;
  if (SSB__ARRAY_P (sb))
    {
      psb.nmb_memb = sb->nmb_memb;
      psb.u.memb_size2 = sb->memb_size2;
    }
  else
    {
      psb.nmb_memb = 1;
      psb.u.attribs = sb->attribs;
    }

  wr (psb);

  /* write DATA */
  for (i=0; i < sb->nmb_memb; ++i)
    {
      ssp *ii;
      ssp *curr = SSB__DATA (sb, i);
      char free_sp = 0;

      /* find out wether CURR is free */
      for (ii=sb->int_flist; ii; ii=SSP__SUCC(ii))
	{
	  check_ref (SSB__RANGE_P (sb, ii));
	  check_ref (SSP__INVARIANT_P (ii));
	  if (ii == curr)
	    {
	      free_sp = 1;
	      break;
	    }
	}

      /* write bool indicating wether CURR is free.. */
      PUTC (free_sp, os);

      /* write CURR itself if not free */
      if (!free_sp)
	wr2 (curr->u.data, sb->memb_size - SSP_OVERHEAD);
    }
#undef wr
#undef wr2
}

/* Restore a SSB from IS */
/* It retrieves an object stored by write_ssb() and link it to the
   correct list (free/single/empty) */

static ssb *
get_ssb (FILE *is)
{
#define rd(x) fread (&(x), sizeof (x), 1, is)
#define rd2(x,sz)  (fread (&(x), (sz), 1, is)) 

  struct pers_ssb psb;
  ssb *sb;

  local_precond (a_sma.initialized);
  local_precond (is);

  rd (psb);

  if (psb.nmb_memb == 1)
    {
#if 1 /* def AMIGA_IV_MASK /* XXX-bw/08-Apr-99 */
      /* quick&dirty hack to avoid using 24BITDMA for the initial
         interval */
      if (psb.u.attribs & MEMF_24BITDMA)
	{
	  /* Note: Because we allocate a single block, we don't need
             to verify for Lisp storage.  But never do this for non
             single blocks! -bw/08-Apr-99 */
	  bool (*old_hook) (void *begin, void *end) = Asma_verify_storage_hook;
	  Asma_verify_storage_hook = &Ama_verify_storage_iv;
	  /* first try without MEMF_24BITDMA, ... */
	  sb = Asma_create_block_single (psb.memb_size,
					 psb.u.attribs & ~MEMF_24BITDMA);
	  /* ... then try again with MEMF_24BITDMA. */
	  if (!sb)
	    sb= Asma_create_block_single (psb.memb_size, psb.u.attribs);
	  Asma_verify_storage_hook = old_hook;
	}
      else
	sb= Asma_create_block_single (psb.memb_size, psb.u.attribs);
#else /* not AMIGA_IV_MASK */
      sb = Asma_create_block_single (psb.memb_size, psb.u.attribs);
#endif /* not AMIGA_IV_MASK */

      if (sb)
	{
	  int is_free;
	  /* is_free is always false */
	  is_free = GETC (is);
	  if (is_free == EOF)
	    return 0;
	  if (!is_free)
	    rd2 (SSB__DATA (sb, 0)->u.data, sb->memb_size - SSP_OVERHEAD);
	}
    }
  else
    {
      sb = Asma_create_block (psb.u.memb_size2, psb.nmb_memb);
      if (sb)
	{
	  int i;

	  /* clear int_flist. we make a new one */
	  sb->int_flist = 0;

	  for (i=0; i < psb.nmb_memb; ++i)
	    {
	      int is_free;
	      ssp *sp;

	      sp = SSB__DATA (sb, i); 

	      is_free = GETC (is);
	      if (is_free == EOF)
		return 0;

	      if (!is_free)
		{
		  rd2 (sp->u.data, psb.memb_size - SSP_OVERHEAD);
		  ++sb->nmb_used;
		}
	      else
		{
		  sp->u.next = sb->int_flist;
		  sb->int_flist = sp;
		}
	    }
	  if (SSB__EMPTY_P (sb))
	    {
	      /* move free_blocks[n] => empty_blocks */
	      Remove ((struct Node*)&sb->node);
	      AddHead (&a_sma.empty_blocks, (struct Node*)&sb->node);
	    }
	}
    }
#undef rd
#undef rd2
  return sb;
}

/* Store the entire internal data structure into file */
/* Used for dumping malloc() storage. */
bool
Asma_save_malloc (FILE *os)
{
  int i;
  sln *ii;

  check_precond (a_sma.initialized);
  check_precond (os);

  errno = 0;
  for (i= MAXLG-1; i >= 0; --i)
    SLH__RFOR (ii, &a_sma.free_blocks[i])
      {
	if (((ssb*)ii)->flags & SSB_NOSAVE_F)
	  continue;		/* skip this block */
	PUTC ('#', os);	/* ssb key */
	write_ssb ((ssb*)ii, os);
      }
  PUTC ('*', os);		/* stop key */
  return errno == 0;
}

/* Restore the internal data structures saved by Asma_save_malloc() */
/* Used for undumping malloc storage.  Usually all lists should be
   empty before this call.  If not, the crated object lists are not
   full identical with the ones which was stored in IS.  But this is
   needed to unpatch all pointers to malloc() storage by dump-code. */
bool
Asma_restore_malloc (FILE *is)
{
  int key;
  int i;
  sln *ii;
  
  check_precond (a_sma.initialized);
  check_precond (is);

  /* Mark old blocks as ignored for offset count. */
  for (i= MAXLG-1; i >= 0; --i)
    SLH__RFOR (ii, &a_sma.free_blocks[i])
      ((ssb*)ii)->flags |= SSB_NORELOC_F;

  /* restore in place */
  while ((key = GETC (is)) == '#')
    {
      if (!get_ssb (is))
	return false;
    }

  return (key == '*');
}


#if ASMA_CATCH_BUGS

/* Check invariant of used storage pieces. */
static void
Asma_check_storage (void *begin[], void **end)
{
  void **i = 0;

  local_precond (begin && end && (char*)begin < (char*)end);

  for (i= begin; i != end; ++i)
    if (*i)
      {
	ssp *h = SSP__HEADER(*i);
	assert (h);
	assert (SSP__INVARIANT_P(h));
      }
}


/* Check invariants of all objects in list BEGIN. */
static void
Asma_check_free_list (ssb *begin)
{
  sln *k = 0;
  ssp *i = 0;

  local_precond (begin);

  for (k=&begin->node; k->mln_Succ; k=k->mln_Succ)
    {
      ssb *b = (ssb*) k;
      assert (SSB__INVARIANT_P(b));
      for (i=b->int_flist; i; i = SSP__SUCC(i))
	assert (SSP__INVARIANT_P(i));
    }
  
}

/* Check invariant of objects in all free lists. */
static void
Asma_check_all_free_lists()
{
  int i=0;
  for (i=0; i < MAXLG; ++i)
    if (a_sma.free_blocks[i].lh_Head->ln_Succ)
      Asma_check_free_list ((ssb*) a_sma.free_blocks[i].lh_Head);
}
#endif

#if TEST
/* compile command:
   gcc -I. -I/ebuild -g -Wall -Wno-comment -DFULLDEBUG -DTEST=3 amiga_smalloc.c

   Values for TEST:
   0: (external test/main)
   1: list management
   2: save/read
   3: save/restore patch/unpatch
   4: Asma_realloc()
   */


void *(*force_linking_malloc)() = malloc; /* for debugging with gdb(1) */


#if TEST == 2
int
main ()
{
#define WALL "abcdefghijklmnopqrstuvwxyz"
  char wall[] = WALL;
  unlink ("/t/sma_out");

  if (Asma_init_module ())
    {
      ssb *sb = Asma_create_block (6, 3);
      ssb *sb2;
      int i;
      FILE *os = fopen ("/t/sma_out", "ab+");

      atexit (Asma_exit_module);
 
      if (sb && os)
	{
	  int ct=0;
	  sb->int_flist = 0;
	  for (i=0; i < sb->nmb_memb; ++i)
	    {
	      char *d = SSB__DATA (sb, i)->u.data;
	      int size = sb->memb_size - SSP_OVERHEAD;
	      int k;

	      for (k=0; k < size; ++k)
		d[k] = ct++;
	    }
	  write_ssb (sb, os);
	  rewind (os);
	  sb2 = get_ssb (os);
	  if (sb2)
	    {
	      assert (sb->size == sb2->size);
	      assert (sb->memb_size == sb->memb_size);
	      for (i=0; i < sb->nmb_memb; ++i)
		{
		  ssp *ii=0, *curr = SSB__DATA(sb2, i);

		  for (ii = sb2->int_flist; ii; ii=SSP__SUCC(ii))
		    {
		      check_ref (SSB__RANGE_P (sb2, ii));
		      check_ref (SSP__INVARIANT_P (ii));
		      if (ii == curr)
			goto is_free;
		    }
		  assert (0==memcmp (&SSB__DATA (sb, i)->u.data,
				     &SSB__DATA (sb2, i)->u.data, 
				     sb->memb_size - SSP_OVERHEAD));
		is_free: continue;
		}
	    }
	}
    }
  assert (strcmp (wall, WALL)==0);
  return 0;
}
#else
int
main (int argc, char *argv[])
{
  int i = argc > 1 ? atoi (argv[1]) : 1000;
  int size_mask = argc > 2 ? ((1 << atoi(argv[2])) - 1) : ((1 << 12) - 1);
  int cnt = 0;
  void *m[0x100] = { 0 };
  size_t s[0x100] = { 0 };
#define WALL "abcdefghijklmnopqrstuvwxyz"
  char wall[] = WALL;

  if (!Asma_init_module ())
    exit (20);
  atexit (Asma_exit_module);

  Asma_free (Asma_alloc (0));	/* check 0 size allocation. */

  while (i--)
    {
      long n = random ();
      size_t size = (n >> 18) & size_mask;
      int index = 0xff & n;
      fprintf (stderr, "%d\n", ++cnt);

      if (m[index] == 0)
	{
	  void *p = 0;

#if TEST != 4
	  p = Asma_alloc (size);
#else
	  p = Asma_realloc (m[index], size);
#endif
	    
	  if (p)
	    {
	      memset (p, ~0, size);
	      m[index] = p;
	      s[index] = size;
	      Asma_check_all_free_lists();
	      Asma_check_storage (m, m + 0x100);
#if TEST == 3
	      {
		long offs = Asma_locate_address (p);
		assert (offs != -1);
		assert (p == Asma_locate_offset (offs));
	      }
#endif
	    }
	}	  
      else
	{
	  void *p=0;

	  int i=0;

	  Asma_check_storage (m, m + 0x100);
#if TEST != 4
	  Asma_free (m[index]);
	  Asma_check_all_free_lists();
	  m[index] = 0;
	  Asma_check_storage (m, m + 0x100);
#else
	  if (m[index])
	    for (i = 0; i < s[index] && i < size; ++i)
	      (((char**)m)[index])[i] = size+i;
	  p = Asma_realloc (m[index], size);
	  if (p && m[index])
	    for (i = 0; i < s[index] && i < size; ++i)
	      assert (((char*)p)[i] == (char)(size+i));
	  if (p)
	    memset (p, ~0, size);
	  Asma_check_all_free_lists();
	  Asma_check_storage (m, m + 0x100);
	  m[index] = p;
	  s[index] = size;
#endif
	}
    }
#if TEST == 3
  unlink ("/t/sma_out");
  {
    FILE *os = fopen ("/t/sma_out", "ab+");
    int used_old[1000]={0};
{
  int i;
  sln *ii;
  int u=0;

  for (i=MAXLG-1; i >= 0; --i)
    SLH__RFOR (ii, &a_sma.free_blocks[i])
      if (u < 1000)
	used_old[u++] = ((ssb*)ii)->nmb_used;
}

    if (os)
      {
	void *early;

	Asma_save_malloc (os);
	Asma_exit_module ();

	a_sma.exit_flag = false;
	Asma_init_module ();

	early = Asma_alloc (42); /* mem which will marked SSB_NORELOC_F. */
	rewind (os);
	if (!Asma_restore_malloc (os))
	  puts ("restore failed");
	else
	  {
	    puts ("restore succeeded");
	    Asma_check_all_free_lists();
	    {
	      int i;
	      sln *ii;
	      int u=0;

	      for (i=MAXLG-1; i >= 0; --i)
		SLH__RFOR (ii, &a_sma.free_blocks[i])
		  {
		    if (((ssb*)ii)->flags & SSB_NORELOC_F)
		      continue;	/* skip early blocks */
		    assert (used_old[u] == ((ssb*)ii)->nmb_used);
		    ++u;
		  }
	    }
	  }
      }
  }
#endif
  assert (strcmp (wall, WALL)==0);
  return 0;
}
#endif
#endif


/* Section: statistic */
#if ASMA_DB_STAT
unsigned long asma_stat_waste_histo[101];
int asma_stat_waste_histo_check = -1;
struct { size_t upper_bound, lower_bound, count; } asma_stat_count;
volatile void
Asma_stat_waste_histo_check  (volatile size_t needed, 
			      volatile size_t allocated)
{
  if (asma_stat_count.lower_bound <= needed
      && needed < asma_stat_count.upper_bound)
    asma_stat_count.count++;
  return;
}
static void
Asma_stat_waste_histo_update (size_t needed, size_t allocated)
{
  size_t wasted, percent;
  local_precond (needed > allocated);
  wasted = allocated - needed;
  percent = (wasted * 100) / allocated;
  assert (percent <= 100);
  ++(asma_stat_waste_histo[percent]);
  if (percent == asma_stat_waste_histo_check)
    Asma_stat_waste_histo_check (needed, allocated);
}
#endif /* ASMA_DB_STAT */

/* Fill in a vector of stat blocks.  */
/* BLOCK_BUF must contain at least MAX_BLOCKS */
static int
Asma_stat_create (struct Asma_block_stat block_buf[], int max_blocks)
{
  ssb *i;
  int k, l;

  local_precond (block_buf && max_blocks > 0);
  l = 0;

  ASMA__FOR (i, k)
    {
      if (l >= max_blocks)
	break;
      block_buf[l].address = i;
      block_buf[l].siz_member = i->memb_size;
      block_buf[l].nmb_member = i->nmb_memb;
      block_buf[l].nmb_free = i->nmb_memb - i->nmb_used;
      block_buf[l].nmb_access = SSB__NACCESS(i);
      
      ++l; 
    }
  return l;
}

/* Make statistic and pass it to a callback (LISP) function.  */
/* To avoid a static data buffer, we create the statistic on stack and
   pass it to a CALLBACK.  The result of callback will returned. (The
   callback do usual turn the statistic into a Lisp_Object and give it
   back then.)  -bw/13-Sep-97 */
int
Asma_get_stat (int (*callback) (struct Asma_stat *))
{
  ssb *i;
  int k;
  struct Asma_stat stat;
  
  check_precond (callback);
  check_precond (a_sma.initialized);

  stat.size = 0;
  ASMA__FOR (i, k)
    ++stat.size;
  stat.data = alloca (stat.size * sizeof *stat.data);
  Asma_stat_create (stat.data, stat.size);
  /* note: Statistic becomes invalid now.  */
  return (*callback) (&stat);
}


/* Section: debug aids for user code */

/* Find block for given address  */
const ssb *
Asma_db_find_parent (const void *m)
{
  sln *ii;
  int i;

  check_precond (a_sma.initialized);

  if (!m)
    return 0;

  for (i=0; i < MAXLG; ++i)
    SLH__FOR (ii, &a_sma.free_blocks[i])
      if (SSB__RANGE_P((ssb*)ii, m))
	return (ssb*)ii;

  return 0;			/* not found */
}

/* Find element for given block and address */
int
Asma_db_find_child_index (const ssb *block, const void *m)
{
  return -1;			/* not found */
}

const ssp *
Asma_db_find_child_address (const ssb *block, int i)
{  return 0;			/* not found */
}



