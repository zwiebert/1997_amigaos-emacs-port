#ifndef H_EMACS_AMIGA_AMIGA_H_
#define H_EMACS_AMIGA_AMIGA_H_

#include <stddef.h>

#define ASYSE_MSG (Asys_errmsgs[Asys_errno])
#define ASYSE_RESOURCE (Asys_err_resouurce)
const char *Asys_err_resource;
#define ASYSE_SET(resource, code) \
({ Asys_errno = code; Asys_err_resource = resource; })
#define ASYSE_SET_MISC(resource, msg) \
({ Asys_errno = ASYSE_MISC; Asys_errmsgs[ASYSE_MISC] = msg; })

extern int Asys_errno;
extern const char *Asys_errmsgs[];
#define ASYSE_MISC 1
#define ASYSE_NO_MEMORY 2
#define ASYSE_NO_SHARED_LIB 3
#define ASYSE_NO_FONT 4
#define ASYSE_NO_WINDOW 5
#define ASYSE_DOS 6

/* Prototypes for functions defined in amiga_dump.c */
void map_out_data(char *fn);
void map_in_data(int load);

/* Prototypes for functions defined in amiga_rexx.c */
int check_arexx(void);
void init_amiga_rexx(void);
void cleanup_amiga_rexx(void);
void syms_of_amiga_rexx(void);

char * Asys_expand_path (char *buf, int len, const char *src);
struct MsgPort *Asys_create_softint (void (*handler) ());
void Asys_destroy_softint (struct MsgPort *mp);

void syms_of_amiga(void);
void amiga_undump_reinit(void);

/* Failure stuff */
/* was in "internal/messages.h" */

void _message(char const *format, ...) __attribute__ ((format (printf, 1, 2)));
void Aerr_fail(char const *format, ...) __attribute__ ((format (printf, 1, 2)));
#ifdef _STDIO_H_
int Aerr_print_backtrace (int start, int depth, FILE *os);
#else
int Aerr_print_backtrace ();
#endif
/* Various special values used to find the beginning & end of the text, data,
   bss and malloc segments. */
extern int amiga_initialized;	/* True once Emacs has been undumped or initialised */

typedef enum
  {
    false, true
  }
bool;

#define PROGNAME ({ extern char *__progname; (const char *)__progname; })
#define Aerr_panic(fmt, args...) \
Aerr_fail ("%s: Panic in %s::%s: " fmt "\n", \
	   PROGNAME, __FILE__, __PRETTY_FUNCTION__,  ##args)

/* amiga_smalloc.c */
extern  void *Asma_alloc (size_t size);
extern  void Asma_free (void *ptr);
extern  bool Asma_init_module (void);
extern  void Asma_exit_module (void);
extern  bool (*Asma_verify_storage_hook) (void *begin, void *end);
extern  long Asma_locate_address (const void *m);
extern  void *Asma_locate_offset (long offs);

#ifdef _STDIO_H_
extern  bool Asma_save_malloc (FILE *os);
extern  bool Asma_restore_malloc (FILE *is);
#else
extern  bool Asma_save_malloc ();
extern  bool Asma_restore_malloc ();
#endif

/* amiga_malloc.c */
extern bool Ama_verify_storage (void *begin, void *end);
extern bool Ama_verify_storage_iv (void *begin, void *end);
extern bool Amal_init_module (void);

#endif
