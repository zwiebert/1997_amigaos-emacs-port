/* amiga_defs.h - some misc macros and constants used by some amiga modules */

/* This header may included several times (similar to <assert.h>). */

#ifndef AMIGA_DEFS_H_
#define AMIGA_DEFS_H_

/* storage boundaries */
/* note: _FIRTS/_LAST are the boundaries between link libraries and
   Emacs inside of a segment.  _START/_END is used for the bounds of
   the entire hunk.  The boundaries of text, data and bss segments are
   defined in s/amigaos.h. */

extern int first_data, last_data, first_bss, last_bss, *pure, puresize;
extern void first_function(), last_function();

#define DATA_FIRST ((char *) &first_data)
#define DATA_LAST ((char *) &last_data)
#define TEXT_FIRST ((char *) first_function)
#define TEXT_LAST ((char *) last_function)
#define BSS_FIRST ((char *) &first_bss)
#define BSS_LAST ((char *) &last_bss)
#define PURE_START ((char*)pure)
#define PURE_END ((char*) pure + puresize)

#endif /* AMIGA_DEFS_H_ */

/* debugging aids */

/*
You may use the following assert-like macros:

check_precond, local_precond, check_postcond, check_loop_inv.

You can switch on/off the error checking with this defines:

FULLDEBUG, NO_PRECOND (0/1)*, FORCE_PRECOND, NO_POSTCOND (0/1),
FORCE_POSTCOND, NO_LOOP_INV, FORCE_LOOP_INV.

(* 0=on !0=off
*/


/* Check conditions at beginning of functions.  */
/* This should check all args and any other input.  */
#undef check_precond
#if (NO_PRECOND) && !defined (FORCE_PRECOND) && !defined (FULLDEBUG)
#define check_precond(bool_expr) ((void) 0)
#else
 extern int
 Asys_invalid_precond (const char*, unsigned line, const char *fname,
		       const char *bool_expr);
#define check_precond(bool_expr)						\
 ((void)((bool_expr) ? 0 :							\
 Asys_invalid_precond (__FILE__,__LINE__,__PRETTY_FUNCTION__,#bool_expr)))
#endif

/* Check conditions at beginning of static functions.  */
/* Preconditons for static functions are only needed if the module
   itself has errors.  */
#undef local_precond
#ifndef FULLDEBUG
#define local_precond(bool_expr) ((void) 0)
#else
#define local_precond(bool_expr) check_precond(bool_expr)
#endif

/* Check conditions at end of functions.  */
/* This should catch bugs inside the function code.  */
#undef check_postcond
#if (NO_POSTCOND) && !defined (FORCE_POSTCOND) && !defined (FULLDEBUG)
#define check_postcond(bool_expr) ((void) 0)
#else
 extern int
 Asys_invalid_precond (const char*, unsigned line, const char *fname,
		       const char *bool_expr);
#define check_postcond(bool_expr)						\
 ((void)((bool_expr) ? 0 :							\
 Asys_invalid_precond (__FILE__,__LINE__,__PRETTY_FUNCTION__,#bool_expr)))
#endif

/* Check loop invariant (overacting?!)  */
/* Its not good to have too much different debug macros... */
#undef check_loop_inv
#if (NO_LOOP_INV) && !defined (FORCE_LOOP_INV) && !defined (FULLDEBUG)
#define check_loop_inv(bool_expr) ((void) 0)
#else
 extern int
 Asys_invalid_precond (const char*, unsigned line, const char *fname,
		       const char *bool_expr);
#define check_loop_inv(bool_expr)						\
 ((void)((bool_expr) ? 0 :							\
 Asys_invalid_precond (__FILE__,__LINE__,__PRETTY_FUNCTION__,#bool_expr)))
#endif

/* Check for dangling pointers or check the containers invariant.  */
#undef check_ref
#define check_ref(bool_expr)  assert(bool_expr)

/* Toggle power LED.  */
#undef BLINK
#if NDEBUG
#define BLINK
#else
#define BLINK asm ("bchg.b #1,0xbfe001") 
#endif

#undef DB_TRACE
#if NDBEBUG
#define DB_TRACE
#else
extern kprintf (const char *fmt, ...);
#define DB_TRACE (kprintf("%s:%ld: trace %s\n", __FILE__, __LINE__,  __PRETTY_FUNCTION__))
#endif


/* end of file */
