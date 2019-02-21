/* machine description file for GNU Emacs running on

   Amiga OS 37.xx - 41.xx, ixemul library-47.x, gcc-2.7.x
   
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


/* The following three symbols give information on
 the size of various data types.  */
/* lisp.h doesn't make any concessions for INTBITS = 16 and
   NO_UNION_TYPE (use int as lisp object) defined.  The following is
   assuming that the Manx 32 bit int. math package is faster than
   bitfield implementation, overall.  If you want to use INTBITS = 16,
   you *cannot* define NO_UNION_TYPE.	*/

#include "../amiga_version.h"

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

#define VALBITS 28		/* Number of bits in an int or pointer offset */

#define GCTYPEBITS 3		/* Number of bits in a type. */

/* Override type conversion macros in "lisp.h" */
/* The implementation functions can be found in "amiga_malloc.c". */
#ifndef NOT_C_CODE

/* Number of segments (not number of seg-id bits!)  If your machine
   have no (virtual) memory beyound 28bit (VALBITS) addresses, you can
   gain speed by disabling segmentation.  But you will lose also the
   better dangle-pointer-recognization.

   0 - no segments at all. (use plain XPNTR() in ../lisp.h)
   1 - one fixed segment. (uses DATA_SEG_BITS version of XPNTR() in ../lisp.h)
   2...28 - N segments. (use the XPNTR() in ../amiga_malloc() */
#ifndef AMAL_NMB_SEGS
#ifndef AMIGA_V_XEMACS_20_4
#define AMAL_NMB_SEGS 3
#else
#define AMAL_NMB_SEGS 0
#endif
#endif /* AMAL_NMB_SEGS */

#if (AMAL_NMB_SEGS) == 0
/* Just use the macros in lisp.h */
#define A_XPNTR(a) XUINT (a)
#define A_FAST_XPNTR A_XPNTR
#define XSETPNTR(obj_, cptr_) \
 ((obj_) = ((EMACS_INT)(obj_) & ~VALMASK) + ((EMACS_INT) (cptr_) & VALMASK))
#elif (AMAL_NMB_SEGS) == 1
/* Just use the alternative macro set in lisp.h */
#define A_XPNTR(a) (XUINT (a) | DATA_SEG_BITS)
#define A_FAST_XPNTR A_XPNTR

/* DATA_SEG_BITS may be a #define'd number constant (e.g. #define
   DATA_SEG_BITS 0x60000000) or may be an integer variable which is
   initialized by the segment of the first block we verify
   -bw/08-Apr-99 */
#ifndef AMAL_CONST_SEGMENT
extern unsigned mem_seg_bits[1];
#define DATA_SEG_BITS  (mem_seg_bits[0])
#else
#define DATA_SEG_BITS  (AMAL_CONST_SEGMENT)
#endif

#define XSETPNTR(obj_, cptr_) \
 ((obj_) = ((EMACS_INT)(obj_) & ~VALMASK) + ((EMACS_INT) (cptr_) & VALMASK))

#else /* AMAL_NMB_SEGS > 1 */

#define AMIGA_MULTI_SEGS

/* Number of bits used for segment offset in Lisp pointers. */
#define A_SEGOFFS_BITS (VALBITS - (AMAL_NMB_SEGS))

/* Use special version dealing with multiple segments.  */

/* Take sgement start of an LISP address.  */
#define A_SEGSTART(lptr)							\
({ extern unsigned mem_seg_bits [AMAL_NMB_SEGS];				\
   register unsigned seg_start							\
     = mem_seg_bits [(lptr >> A_SEGOFFS_BITS) & ((1<<AMAL_NMB_SEGS)-1)];	\
   if (seg_start & 0x1)								\
      Amal_invalid_lisp_ptr (lptr);						\
   seg_start; })

/* Take segment offset of an LISP address.  */
#define A_SEGOFFS(lptr) (lptr & (1 << A_SEGOFFS_BITS) - 1)

/* Macro implementation of lisp.h::XPNTR().  */ 
#define A_XPNTR(lptr) (A_SEGSTART (lptr) | A_SEGOFFS (lptr))

#ifndef FULLDEBUG
/* This is like XPNTR() but in a segmented lisp storage implementation
  its more fast (but also bigger).  If you use only one segment or
  even absolute addresses, then its all the same.  Only the debug
  capabilites may differ.  */
#define A_FAST_XPNTR(lptr) A_XPNTR (lptr)
#else /* FULLDEBUG */
#define A_FAST_XPNTR(lptr) (Amal_xpntr(lptr))
#endif /* FULLDEBUG */

#if 0			
/*  Bloats the code. */
#define XPNTR(lptr) A_XPNTR (lptr)

#else
#define XPNTR(lptr) (Amal_xpntr(lptr))

/* Define XPNTR_REGPARM if your compiler knows about that attribute.  */
#ifdef __regargs
#define XPNTR_REGPARM
#endif

unsigned const Amal_xpntr (int a) 
#ifndef XPNTR_REGPARM
__attribute__ ((const));
#else
__attribute__ ((regparm, const));
#endif

#endif /* not 0 */

int const Amal_xsetpntr (int a, int b) __attribute__ ((const));
#define XSETPNTR(a,b) ((a) = Amal_xsetpntr ((a), (unsigned) (b)))
unsigned const Amal_xset (unsigned type, unsigned ptr) __attribute__ ((const));
#define XSET(var, type, ptr) (var) = Amal_xset (type, (unsigned) (ptr))

#endif  /* AMAL_NMB_SEGS > 1 */
#endif /* NOT_C_CODE */



/* Define WORDS_BIG_ENDIAN if lowest-numbered byte in a word
   is the most significant byte.  */
#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

/* #define NO_ARG_ARRAY */

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#define WORD_MACHINE

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#ifndef	m68000
#define m68000
#endif


#ifndef AMIGA_V_XEMACS_20_4
/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */
#define NO_UNION_TYPE
#endif /* not AMIGA_V_XEMACS_20_4 */

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* We don't have /dev/kmem, so LOAD_AVE_TYPE and LOAD_AVE_CVT are
   not defined. */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#ifdef AMIGA_V_XEMACS_20_4
#define CANNOT_DUMP
#endif

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

#define VIRT_ADDR_VARIES

/* I now rely on AMIGA_DUMP to make appropriate patches in the source */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/* #undef C_ALLOCA */
#define HAVE_ALLOCA

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

#ifndef NOT_C_CODE
#include <machine/float.h>
#endif
