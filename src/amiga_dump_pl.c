/* Apply function pointers to the external pointers which have to be
   patched/unpatched while dumping/undumping. */
/* This module is used only by amiga_dump.c.  It is separated to
   suppress type checking by the C compiler.  That would cause compile
   time erors, because we treat all external pointers as generic
   pointers of type <void*>. */

#include "config.h"

/* function pointer type applied on single pointer objects */
typedef  void *(*fnp) (void *ptr, void **); /* for C pointers */
typedef int (*fnp3) (int lptr, int **);	/* for LISP pointers */

/* funcion pointer type applied on link node objects */
/* OFFSET has to be the byte offset (offsetoff()) of NODE's link
   field */
typedef  void (*fnp2)(void **node, int offset);


/* Patch/unpatch C pointer objects */
/* CPTR_PATCHER has to be a function pointer to a conversion for
   single pointer objects.  CHAIN_PATCHER have to be a function
   pointer to a conversioin for linked pointer objects.  (Note: We
   cannot use a local CHAIN_PATCHER because the traverse order depends
   wether we do patch or unpatch.) */

void
do_apply_cptr_list (fnp cptr_patcher, fnp2 chain_patcher)
{
  /* PNTR have to be a global scalar C pointer variable. */
#define PTR(pntr) { extern void *pntr; pntr = cptr_patcher ((pntr), 0); }
  /* LIST ist the C data pointer of the first node of linked nodes.
     LINK is the offset relative to LIST to find the C pointer which
     points to the successor node. */
#define CHAIN(list, link)  { extern void *list; chain_patcher(&list, link); }

#ifdef AMIGA_V_EMACS_20_2
  PTR (conversion_buffer);	/* coding.c */
#ifdef HAVE_WINDOW_SYSTEM
  PTR (set_frame_fontset_func); /* fontset.c */
#ifdef AMIGA_V_EMACS_20_3
  PTR (find_ccl_program_func); /* fontset.c */
#endif /* 20.3 */
  PTR (check_window_system_func); /* fontset.c */
  PTR (get_font_info_func);  /* fontset.c */
  PTR (list_fonts_func);  /* fontset.c */
  PTR (load_font_func);  /* fontset.c */
  PTR (query_font_func);  /* fontset.c */
#endif /* HAVE_WINDOW_SYSTEM */
#endif /* AMIGA_V_EMACS_20_2 */

  PTR (current_string_block);
  PTR (first_string_block);
  PTR (message_text);
#ifndef AMIGA_V_EMACS_20_2
#ifdef CLASH_DETECTION
  PTR (lock_dir);
  PTR (superlock_file);
#endif
#endif /* not 20.2 */
  PTR (float_error_fn_name);

  PTR (stack_copy);

  PTR (searchbuf_head);
  PTR (specpdl);
  PTR (read_buffer);
  PTR (all_buffers);
  PTR (current_buffer);

  PTR (selected_frame);
  PTR (last_nonminibuf_frame);

  /* print */
  PTR (print_buffer);

  /* callint */
  PTR (callint_message);

#ifndef HAVE_ALLOCA
  /* regex */
  PTR (regstart);
  PTR (regend);
  PTR (old_regstart);
  PTR (old_regend);
  PTR (best_regstart);
  PTR (best_regend);
  PTR (reg_info);
  PTR (reg_dummy);
  PTR (reg_info_dummy);
#endif

  /* keyboard */
#ifdef MULTI_KBOARD
  PTR (initial_kboard);
  PTR (current_kboard);
  PTR (all_kboards);
#endif

  PTR (kbd_fetch_ptr);
  PTR (kbd_store_ptr);

  CHAIN (cons_block, 0);
#ifndef AMIGA_V_EMACS_20_2
  CHAIN (cons_free_list, 0);
#else /* 20.2 */
  CHAIN (cons_free_list, 4);
#endif /* 20.2 */
  CHAIN (all_vectors, 4);
  CHAIN (symbol_block, 0);
  CHAIN (symbol_free_list, 4);
  CHAIN (marker_block, 0);
  CHAIN (marker_free_list, 4);
#ifdef USE_TEXT_PROPERTIES
  CHAIN (interval_block, 0);
  CHAIN (interval_free_list, 4*sizeof(long));
#endif
  CHAIN (large_string_blocks, 0);

#ifdef LISP_FLOAT_TYPE
  CHAIN (float_block, 0);
#ifndef AMIGA_V_EMACS_20_2
  CHAIN (float_free_list, 0);
#else /* 20.2 */
  CHAIN (float_free_list, 4);
#endif /* 20.2 */
#endif

#undef PTR
#undef CHAIN
}




/* Patch/unpatch LISP pointer objects. */  
void
do_apply_lptr_list (fnp3 lptr_patcher)
{
  /* LPNTR have to be a global scalar LISP pointer variable. */
#define LPTR(lpntr)  { extern int lpntr; lpntr = lptr_patcher ((lpntr), 0); }

  /* Since Emacs-20.2, most of global pointers are staticpro'ed. We
     could move them to Admp_..tbl ore vice versa... (But the latter
     is more general) -bw/16-Dec-97 */
#ifndef AMIGA_V_EMACS_20_2
  LPTR (current_global_map);
  LPTR (global_map);
#endif /* 19.34 */

  LPTR (meta_map);
  LPTR (control_x_map);
  LPTR (selected_window);
#ifdef AMIGA_V_EMACS_20_2
  LPTR (Qecho_area_clear_hook); /* keyboard.c */
  LPTR (executing_macro);	/* keyboard.c */
#else /* 19.34 */
  LPTR (Qvariable_documentation);
  LPTR (Qdirectory_files);
  LPTR (Qfile_name_completion);
  LPTR (Qfile_name_all_completions);
  LPTR (Qfile_attributes);
  LPTR (Qset_visited_file_modtime);
#ifdef HAVE_SOCKETS
  LPTR (stream_process);
#endif

#if 0				/* staticpro?! */
  LPTR (Vother_window_scroll_buffer);
  LPTR (Vtemp_buffer_show_function);
  LPTR (Vpop_up_frame_function);
  LPTR (Vdisplay_buffer_function);
  LPTR (Vspecial_display_buffer_names);
  LPTR (Vspecial_display_regexps);
  LPTR (Vspecial_display_function);
  LPTR (Vsame_window_buffer_names);
  LPTR (Vsame_window_regexps);
#endif 
#endif /* 19.34 */

#undef LPTR
}

