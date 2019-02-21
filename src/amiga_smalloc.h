#ifndef H_EMACS_AMIGA_SMALLOC_H_
#define H_EMACS_AMIGA_SMALLOC_H_

#include <stdlib.h>
#include <stdio.h>		/* needed by "amiga.h" */
#include "amiga.h"

#ifdef emacs
/* In Emacs we provide  malloc(3), free(3), realloc(3).  */
#define Asma_alloc malloc
#define Asma_free free
#define Asma_realloc realloc
#endif

extern  void *Asma_alloc (size_t size);
extern  void *Asma_low_alloc (size_t size);
extern  void *Asma_realloc (void *ptr, size_t size);

extern  void Asma_free (void *ptr);
extern  bool Asma_init_module (void);  /* called by main() or Asma_alloc() */
extern  void Asma_exit_module (void);  /* called by exit(3) */
extern  bool (*Asma_verify_storage_hook) (void *begin, void *end);

extern  long Asma_locate_address (const void *m);
extern  void *Asma_locate_offset (long offs);
struct offs_addr_map *Asma_offset_table (void);
extern  bool Asma_member_p (const void *a);
extern  bool Asma_save_malloc (FILE *os);
extern  bool Asma_restore_malloc (FILE *is);

extern bool Asma_log_stream (const char *file, const char *mode);


struct  offs_addr_map_tbl { unsigned long offs, *addr, size; };
struct offs_addr_map 
{
  size_t nmemb;
  struct  offs_addr_map_tbl kv_tbl[1];
};

/* statistic report */

struct Asma_block_stat
{
  void *address;		/* Used as a kind of ID. */
  size_t siz_member;		/* Size of sub block. */
  int nmb_member;		/* Number of sub blocks.  */
  int nmb_free;			/* Number of unused sub blocks. */
  long nmb_access;		/* Number of accesses or -1 if n/a. */
};

struct Asma_stat
{
  int size;			/* Number of blocks. */
  struct Asma_block_stat *data;	/* Statistic blocks. */
};

extern int Asma_get_stat (int (*callback) (struct Asma_stat *));

#endif
