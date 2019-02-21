/* Emacs-19/20 on  Amiga OS 37.xx - 41.xx with ixemul-47, gcc-2.7.2 */

/* This file is part of GNU Emacs.

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

#include "../amiga_version.h"

#ifdef AMIGA_V_XEMACS_20_4
#define CANNOT_DUMP
#endif


/*
 *      Define symbols to identify the version of Unix this is.
 *      Define all the symbols that apply correctly.
 */

#ifndef __amigaos
#define __amigaos
#endif
#ifndef AMIGA
#define AMIGA
#endif

/* SYSTEM_TYPE should indicate the kind of system you are using.  It
sets the Lisp variable system-type.  */
#define SYSTEM_TYPE "amigaos"

#ifdef HAVE_X_WINDOWS
#ifdef AMIGA_HAVE_INTUI
#error "Fatal configure error: Two different window systems (X + Intuition) are enabled."
#undef HAVE_X_WINDOWS
#endif /* AMIGA_HAVE_INTUI */
#endif /* HAVE_X_WINDOWS */

#ifndef AMIGA_HAVE_INTUI
/* The REXX needs some code in "amiga_window.c".  Thus, currently we
   can use AREXX only if configured (not necessary running it) for
   Intuition. -bw/27-May-98 */
#undef AMIGA_REXX
#endif

/* nomultiplejobs should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

/* Do not use interrupt_input = 1 by default, because in 4.3
   we can make noninterrupt input work properly.  */
#undef INTERRUPT_INPUT
/* Have this a meaning for window systems? Note: In ADE-X we go into a
   endless loop in x_make_frame_visible() when interrupt_input is
   enabled.  (Set breakpoint x_sync in gdb to stop the loop)
   -bw/07-May-98 */
/* disable interrupt input for X (see keyboard.c::Fset_input_mode())
   -bw/29-May-98*/
#ifdef HAVE_X_WINDOWS
#define NO_SOCK_SIGIO
#endif

/* Netbsd has POSIX-style pgrp behavior.  */
#undef BSD_PGRPS


/* It's not possible to obtain this at runtime, I think.  (XXX?) */
#undef TIOCGPGRP		/* FIXME-bw: not-in-ixemul-41 */
#undef EMACS_HAVE_TTY_PGRP


/* (If POSIX_SIGNAL defined, or SIGTSP undefined than we will unable
   to return from sys_subshell()!) -bw */
/* #undef SIGTSTP */ /* make suspend-emacs spawn a sub-shell */
/* #define POSIX_SIGNALS */		/* -bw/23-Jul-97 */


/* Define HAVE_PTYS if the system supports pty devices. */

#define HAVE_PTYS

/* First pty name is /dev/ptyp0.  */

#define FIRST_PTY_LETTER 'p'

/* Don't close pty in process.c to make it as controlling terminal.
   It is already a controlling terminal of subprocess, because we did
   ioctl TIOCSCTTY.  */
#define DONT_REOPEN_PTY

#define SETUP_SLAVE_PTY							\
    /* Now change our controlling terminal and make files for input	\
       and output separate. (This is needed by some C: commands)	\
       -bw/16-Sep-97  */						\
    if ((Asys_new_tty (xforkin) < 0)					\
	|| (xforkin = open ("/dev/tty", O_RDWR, 0)) < 0)		\
      {									\
	write (1, "Couldn't use this pty terminal ", 31);		\
	write (1, pty_name, strlen (pty_name));				\
	write (1, "\n", 1);						\
	_exit (1);							\
      }

// FIXME-bw:  Check what is better!
//#define SIGNALS_VIA_CHARACTERS

/* Define this macro if system defines a type `union wait'.  */
#define HAVE_UNION_WAIT

/* XEmacs have more configure options */
#ifndef AMIGA_V_XEMACS_20_4

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */
#define HAVE_SOCKETS

/* We use the Berkeley (and usg5.2.2) interface to nlist.  */
#define NLIST_STRUCT  //FIXME-bw: check

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

#define CLASH_DETECTION

#define SYSV_SYSTEM_DIR 

#define HAVE_TERMIOS
#define NO_TERMIO

#endif /* not AMIGA_V_XEMACS_20_4 */

/*
 *      Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

/* Define this symbol if your system has the functions bcopy, etc. */

#define BSTRING

/* subprocesses should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   This is generally OS dependent, and not supported
   under most USG systems. */

#define subprocesses

/* #define DID_REMOTE */              /* Use 0 length write to send eof */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#undef MAIL_USE_FLOCK

/* We use our own malloc. */
#define SYSTEM_MALLOC           /* But I have replaced the system malloc ... */
#undef REL_ALLOC

#define SYMS_SYSTEM syms_of_amiga()

#if (defined (emacs) || defined (NOT_C_CODE)) && !defined (CANNOT_DUMP)
/* Define this to include various patches that allow the Amiga to dump.
   This *must* be defined on the Amiga! */
#define AMIGA_DUMP
#define AMIGA_DUMP_MK
#endif

#ifdef AMIGA_DUMP
#ifdef emacs
extern int puresize;
#endif
 /* Pure-storage will truncated before dumping.  Therefore we alloc a
    fatly piece. -bw */
#define DEF_PURESIZE (BASE_PURESIZE + 1000000)

#define PURESIZE puresize       /* Puresize is variable ... */
#endif /* AMIGA_DUMP */

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* Here are some symbols for src/Makefile.in.in's benefit */

#define START_FILES amiga_firstfile.o

#define OBJECTS_SYSTEM amiga_smalloc.o

#define UNEXEC amiga_dump.o

/* We define LIBS_DEBUG to override a define to "-lg" */
#define LIBS_DEBUG
/* Link with -lamiga, because it's no longer done by
   "lib/lib-gcc/.../specs" */
#if 0
/* We must keep the order: -lm -lc -lamiga.  Therefore we cannot use
   LIBS_SYSTEN, which is included before LIB_STANDARD */
#define LIBS_SYSTEM -lamiga
#else
#define LIB_STANDARD -lc -lamiga
#endif

/* Normally you might be tempted to use OBJECTS_SYSTEM here, but this would cause them
   to be linked after lastfile.o, and thus not be dumped.  This doesn't work. */

#define AMIGA_OBJ_2
#define AMIGA_OBJ_3

#define AMIGA_OBJ_1  amiga_dump_pl.o amiga_sysdep.o \
 amiga_fileio.o amiga_malloc.o amiga_rexx.o amiga_simplerexx.o
#ifdef AMIGA_HAVE_INTUI
#undef AMIGA_OBJ_2
#define AMIGA_OBJ_2 xfns.o xterm.o amiga_window_select.o
#ifndef AMIGA_V_EMACS_20_2
#undef AMIGA_OBJ_3
#define AMIGA_OBJ_3 xfaces.o
#endif /* >= 20.2 */
#endif /* AMIGA_HAVE_INTUI */

#define AMIGAOS_OBJ AMIGA_OBJ_1 AMIGA_OBJ_2 AMIGA_OBJ_3

#ifdef AMIGA_HAVE_INTUI
#define AMIGA_IWIN_CLIPBOARD
/* disable MULE XXX-bw/21-Apr-98: move this to config.in */
#define AMIGA_NO_MULE
/* Activate experimental code to use inner-window sizes for the X like
   functions. -bw/06-Jun-98 */
#define XAPI_GZZ
#else /* not AMIGA_HAVE_INTUI */
 /* reactivate some original code in X modules disactivated in
    experimental Intuition code */
#define BW
#define NBW
#endif /* not AMIGA_HAVE_INTUI */

#define NAME_SHELL "/bin/sh"

#ifndef AMIGA_V_EMACS_20_2
/* Define USE_TEXT_PROPERTIES to support visual and other properties
   on text.  */
#ifndef USE_TEXT_PROPERTIES
#define USE_TEXT_PROPERTIES
#endif
#endif /* not 20.2 */

#define HAVE_VFORK

/* Use $HOME instead /tmp for emacsserver file.  This prevents
   problems with lack of uid/gid on Ram Disk. (MultiUser) */
#define SERVER_HOME_DIR

#define SEPCHAR ':'

#ifndef AMIGA_V_EMACS_20_2
/* Note: Since version 20 file-locking is done via symbolic
links to a filename consist of user-, hostname and process ID. This
may exceed the 30 char limit for filenames. -bw/18-Dec-97 */

/* filelock.c: Does a valid path name fit in a single filename.  Not
   true on AmigaDOS */
#define AMIGA_FILELOCK_CASE(c) tolower(c)
#endif /* not 20.2 */

/* 30 chars per file name aren't long enough for many things -bw/07-Jan-98 */
#undef HAVE_LONG_FILE_NAMES

/* We don't want special startup code. */
#define ORDINARY_LINK

/* Segment boundaries defined by linker:  */
#ifndef NOT_C_CODE
extern int _sdata, _edata, _stext, _etext, _bss_start, _end;
#endif
#define DATA_START ((char *) &_sdata)
#define DATA_END ((char *) &_edata)
#define TEXT_START ((char *) &_stext)
#define TEXT_END ((char *) &_etext)
#define BSS_START ((char *) &_bss_start)
#define BSS_END ((char *) &_end)

#define WAITTYPE int
#ifndef NOT_C_CODE
/* Avoid both name and type clash for struct timeval (note: we cannot
   use unsigned timeval fields for EMACS_SUB_TIME and probably other
   code.) -bw/20-May-98 */
#define timeval amigaos_timeval
#define TEXT EXEC_TYPES_TEXT
#include <devices/timer.h>	/* avoid <struct timeval> name clash */
#undef REGISTER
#undef TEXT
#undef timeval
#ifndef _SYS_TIME_H_
struct timeval 
{
  union {
    long tvu_sec;
    unsigned long tvu_secs;
  } u1;
  union {
    long tvu_usec;
    unsigned long tvu_micros;
  } u2;
};
#include <sys/time.h>

#undef tv_sec
#undef tv_usec
#define tv_sec u1.tvu_sec
#define tv_usec u2.tvu_usec
#define tv_secs u1.tvu_secs
#define tv_micros u1.tvu_micros

#endif /* not _SYS_TIME_H_ */

/* I use ``nm'' to find names for output of amiga_dump.c::check_cands() */
#if !defined (CANNOT_DUMP) && !defined (intui_lib)
#include <stdio.h>		/* cannot included after "#define static" */
#define static
#endif

/* ???-bw/11-Dec-97 */
/* get this since it won't be included if WAITTYPE is defined */
#include <sys/wait.h>
#endif

/* netbsd uses OXTABS instead of the expected TAB3.  */
#define TAB3 OXTABS
#define TABDLY OXTABS  /* Why this? */

#if 0 /* Problems with ncurses-1.9.9e:: no umlauts; no left arrow key;
seems to make the machine instable -bw/12-Mar-98*/

/* XXX-bw/10-Mar-98: CHECKME  */
/* Tell Emacs that we are a terminfo based system; disable the use
   of local termcap.  (GNU uses ncurses.) */
#ifdef HAVE_LIBNCURSES
#define TERMINFO
#define LIBS_TERMCAP -lncurses
/* XXX-bw/10-Mar-98: Is the tparam() in libncurses-1.9.9e compatible
   (i.e. does it accept a NULL-pointer for BUFFER)? */
#define tparam tparam_
#endif
#endif /* 0 */

#define WRETCODE(w) (_W_INT(w) >> 8)
#define HAVE_WAIT_HEADER
#define DECLARE_GETPWUID_WITH_UID_T

/* This allows to run Emacs with higher priority without inheriting it
   to subprocesses.  Very useful on Amiga -bw/7-Aug-97 FIXME-bw:#exp# */
#define SET_EMACS_PRIORITY 1


/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* Some defaults for environment variables */

#define AMIGA_DEFAULT_HOME "/sys"
#define AMIGA_DEFAULT_TERM "amiga"
#define AMIGA_DEFAULT_TERMCAP \
"AA|amiga|Amiga ANSI:\
:co#89:li#33:am:bs:bw:MT:\
:AL=\E[%dL:DC=\E[%dP:DL=\E[%dM:DO=\E[%dB:IC=\E[%d@:\
:LE=\E[%dD:RI=\E[%dC:SF=\E[%dS:SR=\E[%dT:UP=\E[%dA:\
:ae=\017:al=\E[L:as=\016:bl=\007:bt=\E[Z:cd=\E[J:ce=\E[K:cl=\E[H\E[J:\
:cm=\E[%i%d;%dH:dc=\E[P:dl=\E[M:do=\E[B:ho=\E[H:ic=\E[@:is=\E[20l:\
:k1=\2330~:k2=\2331~:k3=\2332~:k4=\2333~:k5=\2334~:\
:k6=\2335~:k7=\2336~:k8=\2337~:k9=\2338~:k0=\2339~:\
:km:kb=\177:kD=\010:kd=\233B:kl=\233D:kr=\233C:ku=\233A:le=\233D:\
:mb=\E[7;2m:md=\E[1m:me=\E[0m:mh=\E[2m:mk=\E[8m:mr=\E[7m:nd=\E[C:nl=\E[B:\
:rs=\Ec:se=\E[0m:sf=\E[S:so=\E[7m:sr=\E[T:ue=\E[0m:up=\E[A:us=\2334m:\
:ve=\E[\040p:vi=\E[\060\040p:xn:"


/* nodump ranges. used at end of c-files to avoid undumping for this
   data. */

#ifndef NOT_C_CODE
/* macro to help track addresses of nodump data */

/*
   Usage:
   Admp_tbl (some_module_name) = {
    Admp_ndobj(a),		// ignore A
    Admp_lobj(b),		// patch Lisp_Object B
    Admp_lobj_vec(c),		// patch Lisp_Object C[n] 
   0};
*/

struct Admp_obj
{
  int key;
#define ADMP_KEY_TERMINATE 0
#define ADMP_KEY_ND 1
#define ADMP_KEY_LOBJ_VEC 2
#define ADMP_KEY_LOBJ 4
#define ADMP_KEY_CPTR 8
#define ADMP_KEY_LPTR_VEC 16
#define ADMP_KEY_CPTR_VEC 32
  unsigned long arg1, arg2;
};


#define Admp_tbl(module) struct Admp_obj Admp_tbl__##module []
/* compatibility */
#define Admp_ndtbl Admp_tbl

/* Used for Lisp_Object not reached from `staticvec'. */
#define Admp_lobj(obj)						\
{								\
  ADMP_KEY_LOBJ,						\
  (unsigned long)(Lisp_Object *) &(obj),			\
  0								\
}

/* Used for Lisp_Object[] not reached from `staticvec'. */
#define Admp_lobj_vec(obj)			\
{						\
  ADMP_KEY_LOBJ_VEC,				\
  (unsigned long)(Lisp_Object *) &(obj)[0],	\
  (sizeof (obj) / sizeof (Lisp_Object[1]))	\
}

/* Objects which should not dumped (like pointers referring outside
   the process) */
#define Admp_ndobj(obj)					\
{							\
  ADMP_KEY_ND,						\
  (unsigned long) (void*) &(obj),			\
  (unsigned long) ((char*) &(obj) + sizeof (obj))	\
}

#endif /* not NOT_C_CODE */



/* This overrides -O in ../../configure.in NOTE: with egcs we have to
   use-m68020 or better to avoid "wrong displacement" errors */
#ifndef C_OPTIMIZE_SWITCH
#define C_OPTIMIZE_SWITCH -O2 -m68020
#endif

/* Defining AMIGA_TEMACS_STACK_SIZE adds
  ``-ixstack AMIGA_TEMACS_STACK_SIZE temacs''
 to the temacs-rule in /src/Makefile */

#ifdef AMIGA_V_EMACS_20_2
#define AMIGA_TEMACS_STACK_SIZE 350000
#else /* 19.34 */
#define AMIGA_TEMACS_STACK_SIZE 250000
#endif /* 19.34 */

/* read only access to runtime config elements */
#define ACFG_AMIGA_PATHS ({extern amiga_paths; amiga_paths+0; })
#define ACFG_AMIGA_EXPAND_PATH ({extern amiga_expand_path; amiga_expand_path+0; })
#define ACFG_HAVE_JOBCTRL ({extern amiga_working_jobctrl; amiga_working_jobctrl+0; })
#define ACFG_EXP_USG_JOBCTRL \
({extern amiga_exp_use_usg_jobctrl; amiga_exp_use_usg_jobctrl+0; })

#define DEVICE_SEP ':'
#define IS_DEVICE_SEP(a) ((a) == DEVICE_SEP && ACFG_AMIGA_PATHS)

#if 1
/* FIXME-bw: check this -bw/8-Sep-97  */
#define IS_ANY_SEP(a) (IS_DIRECTORY_SEP(a) || IS_DEVICE_SEP(a))
#endif

#define GETPGRP_NO_ARG

#if 0 /* The stack extension code breaks ixemul signals -bw */
#ifndef NO_C_CODE
/* Extend process stack.  The normal (Emacs-19.28) stack usage is
   only 20,000...30,000.  The biggest value I measured was 200,080
   bytes.  This was caused by "endless" recursion stopped by
   grow_specpdl().  FIXME-bw: examine regex.h for functions needing
   stackext. */
 /*  ./eval.c */
//int Feval () __attribute__ ((stackext));
//int Ffuncall () __attribute__ ((stackext));
//void grow_specpdl () __attribute__ ((stackext));
#endif
#endif /* 0 */

#ifndef AMIGA_V_EMACS_20_2
#ifdef AMIGA_HAVE_INTUI
#define AMIGA_I_WINDOWS
#define POLL_FOR_INPUT		/* ???-bw/30-May-98: */
#endif
#endif /* not 20.2 */

/* New for Emacs-20.2.97: disable abort() replacement in ../emacs.c */
#define NO_ABORT
