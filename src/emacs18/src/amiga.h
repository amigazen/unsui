
/* Prototypes for functions defined in amiga_clipboard.c */
void syms_of_amiga_clipboard(void);
void early_clipboard(void);
void init_clipboard(void);
void cleanup_clipboard(void);

/* Prototypes for functions defined in amiga_dump.c */
void map_out_data(char *fn);
void map_in_data(int load);
extern void *far first_fn, *far last_fn;

/* Prototypes for functions defined in amiga_menu.c */
void suspend_menus(void);
int resume_menus(void);
void syms_of_amiga_menu(void);
void init_amiga_menu(void);
void cleanup_amiga_menu(void);

/* Prototypes for functions defined in amiga_processes.c */
/* Simulation of unix processes & signals */
int wait_for_termination(int pid);
int wait_without_blocking(void);
char *amiga_path(void);
void init_amiga_processes(void);
void cleanup_amiga_processes(void);

/* Prototypes for functions defined in amiga_rexx.c */
int check_arexx(int force, int kbd);
void init_amiga_rexx(void);
void cleanup_amiga_rexx(void);
void syms_of_amiga_rexx(void);

/* Prototypes for functions defined in amiga_screen.c */
extern struct Window *emacs_win;
void get_window_size(int *widthp, int *heightp);
void reset_window(void);
void force_window(void);
void add_wbevent(struct WBArg *wbarg);
void check_window(int force);
void setup_intchar(char intchar);

void start_count(int n);
void stop_count(int n);
void suspend_count(int n);
void resume_count(int n);
int disp_counts(void);

void screen_puts(char *str, unsigned int len);
void syms_of_amiga_screen(void);
void init_amiga_screen(void);
void cleanup_amiga_screen(void);

/* Prototypes for functions defined in amiga_serial.c */
void init_amiga_serial(void);
void cleanup_amiga_serial(void);
void check_serial(int force);
void serial_puts(char *str, int len);
unsigned long serial_baud_rate(void);

/* Prototypes for functions defined in amiga_sysdep.c */
extern int selecting;

int set_exclusive_use(int fd);
int sys_suspend(void);
char *get_system_name(void);
char *expand_path(char *path, char *buf, int len);
int syms_of_amiga(void);
void cleanup_amiga(void);
void amiga_undump_reinit(void);
void *early_xmalloc(long size);
void *early_xrealloc(void *old, long size);

/* Failure stuff */
void wbmessage(char *msg);
void fail(char *cause);
void fail_nomem(void);
void _fail_internal(char *file, int line);
#define fail_internal() _fail_internal(__FILE__, __LINE__);
enum exit_method { use_exit, use_xcexit, use_safe };
extern enum exit_method amiga_fail_exit;

#define MALLOC_HUNK_SIZE 92000 /* Default malloc hunk size */
extern long malloc_hunk_size; /* Amount of memory malloc'ed by a to-be-dumped emacs */
extern long malloc_bytes_used;	/* Amount of this hunk actually used */
extern long far pre_alloc;	/* amount of memory to reserve for emacs */
extern int puresize;		/* Size of pure hunk */

/* Various special values used to find the beginning & end of the text, data,
   bss and malloc segments. */
extern int first_data, last_data, first_bss, last_bss;
extern void first_function(), last_function();
extern char *malloc_hunk;
extern int amiga_initialized;	/* True once Emacs has been undumped or initialised */
struct mem_header		/* sizeof() must be multiple of 4 ! */
{
    struct mem_header *next, *prev;
    long size;
    /* Data follows */
};
extern struct mem_header *free_list;


/* Prototypes for functions defined in amiga_term.c */
int amiga_term_init(void);

/* Prototypes for functions defined in amiga_tty.c */
extern struct timeinfo *far odd_timer;
extern unsigned long odd_sig;
int setpgrp_of_tty(int pid);
int init_sigio(void);
int reset_sigio(void);
int request_sigio(void);
int unrequest_sigio(void);
int tabs_safe_p(void);
int get_screen_size(int *widthp, int *heightp);
int init_baud_rate(void);
void check_intuition(void);
#define AMIGASEQ 256 /* When passed to enque, insert the Amiga sequence introducer
		        C-x C-^ */
void enque(unsigned int c, int meta);
int init_sys_modes(void);
int reset_sys_modes(void);
void amiga_consume_input(void);
int discard_tty_input(void);
int emacs_fflush(struct __iobuf *f);
void emacs_putchar(int c);
void emacs_output(char *str, int size);
void emacs_fwrite(char *str, unsigned int nblocks, unsigned int len, FILE *f);
void syms_of_amiga_tty(void);
void init_amiga_tty(void);
void cleanup_amiga_tty(void);
void early_amiga_tty(void);
void amiga_term_open(void);
/* Signal mask used to detect available keyboard input.
   Must be set by amiga_serial or amiga_screen */
extern unsigned long inputsig;

/* Prototypes for functions defined in amiga_unix.c */
void MemCleanup(void);
char *malloc(int size);
int free(void *p);
char *calloc(long n, long size);
char *realloc(char *p, long size);
void emacs_malloc_init(void);
