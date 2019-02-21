/* amiga_malloc.c - storage segmentation

   Author: Bert Winkelmann <bertw@in-brb.de> (1997)
   Purpose: Allow use of low and high addresses in LISP pointers of a single Emacs
            process. If you have no memory installed above VALBITS (==28) then it
            would be faster to configure without segmentation (try `../configure --help')
   Note: Segmentation is less useful since we have 28 valbits (formerly 26).

 */

#include <stdlib.h>		/* malloc prototypes */
#include <assert.h>
#include <string.h>

#undef LONGBITS
#include "config.h"
#include "lisp.h"
#include "amiga.h"
#include "amiga_smalloc.h"	/* for statistic functions */
#if 0
#undef AMAL_NMB_SEGS
#define AMAL_NMB_SEGS 0
#endif

/* ==== configure === */
#if (AMAL_NMB_SEGS) > 1
/* We use a segment start table */
#define AMAL_MULTI_SEGS
#elif (AMAL_NMB_SEGS) == 0
#define AMAL_NO_SEGMENT
#else /* (AMAL_NMB_SEGS) == 1 */
/* We store the common high bits of all addresses in mem_seg_bits[0] */
#define AMAL_SINGLE_SEGMENT
#endif

/* We use enums here because we need them in gdb(1) (see src/.gdbinit) */
enum 
{
  /* Number of bits used for segment */
  AMAL_NMB_SEGBITS = AMAL_NMB_SEGS,

  /* Number of segment blocks. */
  AMAL_NMB_SEG = 1<<AMAL_NMB_SEGBITS,      

  /* Number of bits used for offset. */       
  AMAL_NMB_OFFSBITS = VALBITS - AMAL_NMB_SEGBITS,
  
  /* Size of segment block. */
  AMAL_SIZ_SEG = (1<<AMAL_NMB_OFFSBITS)-1,

  /* Mask for the segment ID bits.  */
  AMAL_SEGMASK  = ((AMAL_NMB_SEG-1) << AMAL_NMB_OFFSBITS),

  /* Mask for segment offset bits.  */
  AMAL_OFFSMASK = (1<<AMAL_NMB_OFFSBITS)-1,

  /* Init value for segment vector cells. */
  /* Should be an invalid 32bit address far away (in both directions)
     from any sensible addresses. */
  NIL_SEG = 0x70000001,

  /* Increment to change AM_nil_segment. */
  /* Is NIL_SEG unfortunally a valid address on the running machine,
     we increment it with this until we found an unused segment. */
  NIL_SEG_INC =  0x10000000,
};

/* === switches === */

/* Define AMAL_XPNTR_NULL to allow calling XPNTR() with 0.  As a side
   effect we must force memb_seg_bits[0] to be either a nil segment or
   the 0 segment, because the offset 0 in segment 0 must be the
   NULL-pointer.  (I found only one place of XPNTR(0) in the source.
   Maybe better fix it there.)  -bw/2-Sep-97*/
#define AMAL_XPNTR_NULL 1

/* Should XSET() checking for unknown types? (It would call abort if
   it find such one). (1=check;0=assume-pointer) */
#ifndef FULLDEBUG
#define SAVE_XSET 0
#else
#define SAVE_XSET 1
#endif

/* Assertions: check_postcond() should usual disabled. check_precond()
   shoud enabled if the caller is not trustworthy. */
#define NO_PRECOND 0
#define NO_POSTCOND 1
#include "amiga_defs.h"

/* === data === */

#ifdef AMAL_MULTI_SEGS
/* Segment prefix lookup table.  */
unsigned mem_seg_bits[AMAL_NMB_SEG]; 
int mem_seg_bits_used;
unsigned AM_nil_segment = NIL_SEG;
#endif

#ifdef AMAL_SINGLE_SEGMENT
/* Segment prefix lookup table.  */
unsigned mem_seg_bits [1];

/* replaces enum in lisp.h */
int gdb_data_seg_bits;
#endif

/* Encapsulated local data. */
static struct
  {
    bool initialized;           /* invariant */
#ifdef AMAL_SINGLE_SEGMENT
    bool single_seg_initialized;
#endif
  }
a_mall;




/* === debug aids === */

/* === code === */

/* export interface */
extern bool Ama_verify_storage (void *begin, void *end);
extern bool Amal_init_module ();
/* import interface */

/* Test if A is located in storage [BEGIN,END) */
#define  RANGE(a_, begin_, end_) \
((char *)(a_) >= (char *)(begin_) && (char *)(a_) < (char *)(end_))

bool
Amal_init_module ()
{
  int i=0;

  check_precond (a_mall.initialized == false);

#ifdef AMAL_MULTI_SEGS 
  for (i=0; i < AMAL_NMB_SEG; ++i)
    mem_seg_bits [i] = NIL_SEG;

#if (AMAL_XPNTR_NULL)
  /* Allows 0 arg in XPNTR().  */
  mem_seg_bits_used = 1;
  mem_seg_bits [0] = 0;
#endif
#endif /* AMAL_MULTI_SEGS */

  /* Register all further allocated storage. */
  Asma_verify_storage_hook = Ama_verify_storage;

  /* Register the process segments. */
  if (Ama_verify_storage (DATA_FIRST, DATA_LAST)
      && Ama_verify_storage (TEXT_FIRST, TEXT_LAST)
      && Ama_verify_storage (BSS_FIRST, BSS_LAST))
    {
      a_mall.initialized = true;
      return true;
    }
  else
    Aerr_fail ("I can't handle your memory configuration\n"
	"Hint:  Reconfigure with --enable-nsegs=N. N should be 2, 3 or 4\n"
        "       N=1 would be faster, but it may not work either\n"
        "       The default is N=0, which is fastest but works for\n"
        "       addresses fitting in 28bit variables only\n");

  return false;
}

/* consing the storage manager statistics */
Lisp_Object
Amal_stat_callback (struct Asma_stat *stat)
{
  int i;
  Lisp_Object list;		/* RESULT */

  list = Qnil;

  for (i=0; i < stat->size; ++i)
    {
      /* (addr nmemb nfree nacc) */
      register struct Asma_block_stat *ptr = &stat->data[i];
      Lisp_Object block
	= Fcons (make_number (ptr->address),
		 Fcons (make_number (ptr->siz_member),
			Fcons (make_number (ptr->nmb_member),
			       Fcons (make_number (ptr->nmb_free),
				      Fcons (make_number (ptr->nmb_access),
					     Qnil)))));
      /* prepend to list */
      list = Fcons (block, list);
    }

  return list;
}

DEFUN ("amiga-db-malloc-log", Famiga_db_malloc_log, Samiga_db_malloc_log,
       1, 1, "s file: ", 
       "A function to debug Emacs itself.\n\
Open FILE to write the log output of malloc and close an existing log stream.\n\
If FILE is an empty string or nil then do only closing the existing log stream.\n\
The result will be nil if opening the stream has failed or t otherwise")
     (file)
     Lisp_Object file;
{
  if (NILP (file)
      || (STRINGP (file) && XSTRING (file)->data == '\0'))
    {
      Asma_log_stream (0, 0);
      return Qt;
    }

  CHECK_STRING (file, 0); /* What does the 2nd parameter mean? ???-bw/20-May-98 */

  if (STRINGP (file) /* paranoia */
      && Asma_log_stream (XSTRING (file)->data, 0))
    return Qt;

  return Qnil;
}

/* Get stat about array blocks */
DEFUN ("amiga-memory-stat", Famiga_memory_stat, Samiga_memory_stat,
       0, 0, 0, 
       "A function to debug Emacs itself.\n\
It prints some statistics about memory allocation\n\
status: under development\n
  The result is a list of sublists containg the following fields:\n\
  void *address;		/* Used as a kind of ID. */\n\
  size_t siz_member;		/* Size of sub block. */
  int nmb_member;		/* Number of sub blocks.  */\n\
  int nmb_free;			/* Number of unused sub blocks. */\n\
  long nmb_access;		/* Number of accesses or -1 if n/a. */\n\
\n\
Note: The address field may be truncated to fit in a LISP number\n\
\n")
()
{
  return Asma_get_stat (Amal_stat_callback);
}

void
syms_of_amiga_malloc (void)
{
  defsubr (&Samiga_memory_stat);
  defsubr (&Samiga_db_malloc_log);
}

/* === Memory allocation code === */
#ifdef AMAL_MULTI_SEGS
/* Register segment in the segment table. */
/* Alloc a `mem_seg_bits' table entry for ADDR. Result:
   (BOOL)succes. */
static bool
alloc_mem_seg (void *addr)
{
  unsigned new_segment = 0;
  
  if (mem_seg_bits_used >= (1 << AMAL_NMB_SEGBITS))
    return false;
  new_segment =  (unsigned)addr & ~AMAL_OFFSMASK;
  mem_seg_bits[mem_seg_bits_used++] = (unsigned) new_segment;

  if (AM_nil_segment == new_segment)
    {
      unsigned i;
      assert ((NIL_SEG_INC & ~(VALMASK)) != 0);
    loop: AM_nil_segment += NIL_SEG_INC;
      for (i=0; i < mem_seg_bits_used; ++i)
	if (AM_nil_segment == mem_seg_bits[i])
	  goto loop;
      for (i= mem_seg_bits_used; i < AMAL_NMB_SEG; ++i)
	mem_seg_bits[i] = AM_nil_segment;
    }      
  return true;
}
#endif

#ifdef AMAL_MULTI_SEGS
/* Test wether a address is located in a registered segment. */
/* Test wether A's segment part is registered in the segment table.
   `A' has to be an absolute address. */
int
Amal_addr_valid_p (void *a)
{
  unsigned i=0;
  unsigned a_segment = (unsigned)a & ~AMAL_OFFSMASK;

  check_precond (mem_seg_bits_used <= AMAL_NMB_SEG);
  
  for (i=0; i < mem_seg_bits_used; ++i)
    if (mem_seg_bits[i] == a_segment)
      return 1;
  return 0;
}
#endif /* AMAL_MULTI_SEGS */

/* Test wether a storage block may be used for LISP code. */
/* Return false if any address in [BEGIN,END) are not already
registered in the segment table and table is full. */

bool
Ama_verify_storage (void *begin, void *end) 
{
  check_precond ((char *) begin < (char *) end);

#ifdef AMAL_NO_SEGMENT
  return !((int)end & ~VALMASK);
#elif defined (AMAL_SINGLE_SEGMENT)
  if (!a_mall.single_seg_initialized)
    {
#ifndef AMAL_CONST_SEGMENT
      DATA_SEG_BITS = ((unsigned int)begin & ~VALMASK);
#endif
      gdb_data_seg_bits = DATA_SEG_BITS;
      a_mall.single_seg_initialized = true;
    }
  return (RANGE (begin, DATA_SEG_BITS, (DATA_SEG_BITS | ((1<<VALBITS)-1)))
	  && RANGE (end, DATA_SEG_BITS, (DATA_SEG_BITS | ((1<<VALBITS)-1))));
#else /* AMAL_MULTI_SEGS */
  {
    void *i=0;
    for (i=begin; i < end; i = (char*)i + AMAL_SIZ_SEG)
      if (! (Amal_addr_valid_p (i) || alloc_mem_seg (i)))
	return false;
  
    return Amal_addr_valid_p (end) || alloc_mem_seg (end);
  }
#endif /* AMAL_MULTI_SEGS */
}


/* verify storage allocated for interval blocks */

bool
Ama_verify_storage_iv (void *begin, void *end) 
{
  EMACS_INT begin_type, end_type;
  check_precond ((char *) begin < (char *) end);
  
  begin_type = XGCTYPE ((Lisp_Object)begin);
  end_type = XGCTYPE ((Lisp_Object)end);

  return
    begin_type != Lisp_String
    && begin_type != Lisp_Vectorlike
    && end_type != Lisp_String
    && end_type != Lisp_Vectorlike;
}



/* Called from XPNTR() for panic. */
void
Amal_invalid_lisp_ptr (Lisp_Object a)
{
  Aerr_fail ("Panic: invalid Lisp pointer detected: <0x%x>\n", a);
  abort ();
}

#ifdef AMAL_USE_FAST_ADDR_TEST
/* This tests for valid address on *my* machine!  Purpose: This test
   is faster than testing for valid process addresses.
   -bw/13-Sep-97 */
#define AMAL_ADDR_EXIST_P(a_)			\
(RANGE ((a_), 0xDC00000, 0xF000000) 		\
 || RANGE ((a_), 0x400, 0x1FFFFF))
#else /* not AMAL_USE_FAST_ADDR_TEST */
#define AMAL_ADDR_EXIST_P(a_) (AMAL_SEG_ADDR_P (a_))
#endif  /* not AMAL_USE_FAST_ADDR_TEST */

#define AMAL_SEG_ADDR_P(a_)			\
(RANGE ((char*)(a_), TEXT_FIRST, TEXT_LAST)	\
 || RANGE ((char*)(a_), DATA_FIRST, DATA_LAST) 	\
 || RANGE ((char*)(a_), BSS_FIRST, BSS_LAST) 	\
 || RANGE ((char*)(a_), PURE_START, PURE_END)	\
 || Asma_member_p ((void*)(a_)))



/* Some implementations of macros defined in "lisp.h".   */
/*  See m/amiga.h for maros overriding "lisp.h" */

#ifdef AMAL_NO_SEGMENT
/* We can only use memory upto 28 bit addresses.  */

/* test for valid lisp pointer - called from amgia_dump.c::check_cands() */
bool
Amal_lpntr_p (int lptr)
{
  unsigned ptr;  /* RESULT */
  int seg_start;
  int seg_id;
  
  ptr = XUINT (lptr);

  if (ptr & 1)  /* Can LISP-pointers be unaligned? */
    return false;
  
  if (! ((ptr == 0 && (AMAL_XPNTR_NULL)) 
         || AMAL_SEG_ADDR_P(ptr)))
    return false;

  return true;
}

#elif defined (AMAL_SINGLE_SEGMENT)

/* test for valid lisp pointer - called from amgia_dump.c::check_cands() */
bool
Amal_lpntr_p (int lptr)
{
  unsigned int ptr;  /* RESULT */
  int seg_start;
  int seg_id;
  
  ptr = (XUINT (lptr) | DATA_SEG_BITS);

  if (ptr & 1)  /* Can LISP-pointers be unaligned? */
    return false;
  
  if (! ((ptr == 0 && (AMAL_XPNTR_NULL)) 
         || AMAL_SEG_ADDR_P(ptr)))
    return false;

  return true;
}
#elif defined (AMAL_MULTI_SEGS)

/* Make machine pointer from LISP pointer */
/* Implementation of m/amiga.h::XPNTR(). */
unsigned const
#ifdef XPNTR_REGPARM
     __attribute__ ((regparm, const))
#endif
Amal_xpntr (int lptr)
{
  int ptr;  /* RESULT */
  int seg_start;
  int seg_id;

  ptr = lptr & AMAL_OFFSMASK;   /* add segment offset */
  seg_id = (lptr >> AMAL_NMB_OFFSBITS) & ((1<<AMAL_NMB_SEGBITS)-1);
  seg_start = mem_seg_bits [seg_id];  

  if (seg_start & 1)
    Aerr_panic ("Bad segment ID in LISP pointer <0x%x>\n", lptr);
  
  ptr |= seg_start;   /* add segment base */

#ifdef FULLDEBUG
#if 1
  if (! ((ptr == 0 && (AMAL_XPNTR_NULL))
	 || AMAL_ADDR_EXIST_P (ptr)))
    Aerr_panic ("Bad LISP pointer <0x%x>. Target <%p> doesn't exist.\n", lptr, ptr);
#else
  register unsigned ptr = XUINT (lptr);
  if (! ((ptr == 0 && (AMAL_XPNTR_NULL)) 
         || AMAL_SEG_ADDR_P(ptr)))
    Aerr_panic ("Bad LISP pointer <0x%x>. Target <%p> doesn't exist.\n", lptr, ptr);
#endif
#endif

    return ptr;
}

/* test for valid lisp pointer - called from amgia_dump.c::check_cands() */
bool
Amal_lpntr_p (int lptr)
{
  int ptr;  /* RESULT */
  int seg_start;
  int seg_id;

  ptr = lptr & AMAL_OFFSMASK;   /* add segment offset */
  seg_id = (lptr >> AMAL_NMB_OFFSBITS) & ((1<<AMAL_NMB_SEGBITS)-1);
  seg_start = mem_seg_bits [seg_id];  

  if (seg_start & 1)
    return false;
  
  ptr |= seg_start;   /* add segment base */

  if (! ((ptr == 0 && (AMAL_XPNTR_NULL)) 
         || AMAL_SEG_ADDR_P(ptr)))
    return false;

  return true;
}


/* Build Lisp_Object from type bits and pointer val-bits */
/* Implementation of lisp.h::XSETPNTR(). A is the type.  B is the
   pointer handle. */
int const
Amal_xsetpntr (int type, int val) 
{
  int lptr;   /* RESULT  */
  int i;
  int seg;

  lptr = type & ~VALMASK;  /* add type-id bits */
  lptr |= val & AMAL_OFFSMASK; /* add segment offset bits */

  /* machine address to seg|offs.   */
  seg = val & ~AMAL_OFFSMASK;
  for (i=0; i < mem_seg_bits_used; ++i)
    if (seg == mem_seg_bits[i])
    {
      lptr |= i<<AMAL_NMB_OFFSBITS;         /* add seg-id bits */
      check_postcond (val == (int) XPNTR (lptr));
      return lptr;
    }

    Aerr_fail ("Panic: in %s: invalid address (unknown segment) <%p>\n",
	    	__PRETTY_FUNCTION__, (void*) val);
    abort (); /* NOTREACHED */
}

/* Build Lisp_Object from type bits and a appropriate value (int or ptr). */
/* Implementation of lisp.h::XSET().  Wether A is pointer or integer
   will determined by TYPE. */
unsigned const
Amal_xset (unsigned type, unsigned val)
{
  unsigned lobj;  /* RESULT */
  
  lobj = (type << VALBITS);

  switch (type) {
    /* value types */
  case Lisp_Int:
    lobj |= (val & VALMASK);
    break;

  /* pointer types */
#if SAVE_XSET
  case Lisp_Symbol:
  case Lisp_Misc:
  case Lisp_String:
  case Lisp_Vectorlike:
  case Lisp_Cons:
#ifdef LISP_FLOAT_TYPE
  case Lisp_Float:
#endif
#else
  default:
#endif
    check_precond (Amal_addr_valid_p((void*)val));
#ifdef FULLDEBUG
  if (! (RANGE ((char*)val, TEXT_FIRST, TEXT_LAST)
	 || RANGE ((char*)val, DATA_FIRST, DATA_LAST) 
	 || RANGE ((char*)val, BSS_FIRST, BSS_LAST)
	 || RANGE ((char*)val, PURE_START, PURE_END)
	 || Asma_member_p ((void*)val)))
       Aerr_fail ("Panic: in %s::%s: invalid ptr <%p>\n",
		  __FILE__, __PRETTY_FUNCTION__, (void*) val);
#endif
    XSETPNTR (lobj, val);
    break;
#if SAVE_XSET
  default:
    assert(0); 
#endif
  }
  check_postcond (type == (lobj & ~VALMASK) >> VALBITS);
  return lobj;
}
#endif /* AMAL_MULTI_SEG */


/* Replacement for calloc(3). */
void *
calloc (size_t n, size_t size)
{
  char *t;
  long rsize = n * size;

  t = malloc (rsize);
  if (t)
    bzero (t, rsize);
  
  return t;
}


#ifdef AMIGA_DUMP
Admp_tbl (amiga_malloc) = {
  Admp_ndobj (a_mall),
#ifdef AMAL_MULTI_SEGS
  Admp_ndobj (AM_nil_segment),
  Admp_ndobj (mem_seg_bits_used),
#endif
#ifndef AMAL_NO_SEGMENT
  Admp_ndobj (mem_seg_bits),
#endif
#ifdef AMAL_SINGLE_SEGMENT
  Admp_ndobj (gdb_data_seg_bits),
#endif
  0};
#endif

