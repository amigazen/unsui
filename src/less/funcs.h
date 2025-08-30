#ifdef __STDC__
#ifndef __PROTO
#define __PROTO(a) a
#endif
#endif

#ifndef __NOPROTO
#ifndef __PROTO
#define __PROTO(a) a
#endif
#else
#ifndef __PROTO
#define __PROTO(a) ()
#endif
#endif

/* Prototypes for functions defined in ch.c */

extern int ch_seek __PROTO((register POSITION pos));
extern int ch_end_seek __PROTO((void));
extern int ch_beg_seek __PROTO((void));
extern POSITION ch_length __PROTO((void));
extern POSITION ch_tell __PROTO((void));
extern int ch_forw_get __PROTO((void));
extern int ch_back_get __PROTO((void));
extern void ch_init __PROTO((int want_nbufs));

/* Prototypes for functions defined in command.c */

extern void cmd_reset __PROTO((void));
extern void start_mcc __PROTO((int c));
extern void commands __PROTO((void));

/* Prototypes for functions defined in help.c */

extern void help __PROTO((void));

/* Prototypes for functions defined in input.c */

extern POSITION forw_line __PROTO((POSITION curr_pos));
extern POSITION back_line __PROTO((POSITION curr_pos));

/* Prototypes for functions defined in io.c */

extern void ttopen __PROTO((void));
extern void getrowcol __PROTO((void));
extern void ttclose __PROTO((void));
extern int ttgetc __PROTO((void));
extern int chk_sigs __PROTO((void));
extern void ttwrite __PROTO((char *buffer, int length));
extern void ttputs __PROTO((char *s));
#ifdef AMIGA
#define tputs(x,y,z) Tputs(x)
#endif
extern void Tputs __PROTO((char *s));
extern struct Screen *MyFindWB __PROTO(( void ));
extern void MyFreeWB __PROTO(( struct Screen * ));
#ifdef AMIGA
#define sprintf MySprintf /* uses exec.library, to save on code size */
extern __stdargs void MySprintf __PROTO (( char *buffer, char *fmt, ... ));
#else
extern int sprintf __PROTO((char *, char *,...));
#endif

/* Prototypes for functions defined in line.c */

extern void prewind __PROTO((void));
extern int pappend __PROTO((int c));
extern POSITION forw_raw_line __PROTO((POSITION curr_pos));
extern POSITION back_raw_line __PROTO((POSITION curr_pos));

/* Prototypes for functions defined in main.c */

extern void edit __PROTO((register char *filename));
extern void next_file __PROTO((int n));
extern void prev_file __PROTO((int n));
extern int main __PROTO((int argc, char **argv));
extern void strtcpy __PROTO((char *to, char *from, int len));
extern void quit __PROTO((void));

/* Prototypes for functions defined in option.c */

extern void init_option __PROTO((void));
extern void toggle_option __PROTO((char *s));
extern void scan_option __PROTO((char *s));

/* Prototypes for functions defined in os.c */

extern char *glob __PROTO((char *filename));
extern char *bad_file __PROTO((char *filename, char *message, int len));
extern char *errno_message __PROTO((char *filename, char *message, int len));

/* Prototypes for functions defined in output.c */

extern void put_line __PROTO((void));
extern int control_char __PROTO((int c));
extern int carat_char __PROTO((int c));
extern void flush __PROTO((void));
extern void dropout __PROTO((void));
extern void putchr __PROTO((int c));
extern void putstr __PROTO((register char *s));
extern void error __PROTO((char *s));

/* Prototypes for functions defined in position.c */

extern POSITION position __PROTO((int where));
extern void add_forw_pos __PROTO((POSITION pos));
extern void add_back_pos __PROTO((POSITION pos));
extern void pos_clear __PROTO((void));
extern int onscreen __PROTO((POSITION pos));

/* Prototypes for functions defined in prim.c */

extern void forward __PROTO((int n, int only_last));
extern void backward __PROTO((int n, int only_last));
extern void repaint __PROTO((void));
extern void jump_forw __PROTO((void));
extern void jump_back __PROTO((register int n));
extern void jump_percent __PROTO((int percent));
extern void jump_loc __PROTO((POSITION pos));
extern void init_mark __PROTO((void));
extern void setmark __PROTO((int c));
extern void lastmark __PROTO((void));
extern void gomark __PROTO((int c));
extern int get_back_scroll __PROTO((void));
extern void search __PROTO((int direction, char *pattern, register int n));

/* Prototypes for functions defined in prompt.c */

extern char *eq_message __PROTO((void));
extern char *pr_string __PROTO((void));

/* Prototypes for functions defined in screen.c */

extern void raw_mode __PROTO((int on));
extern void set_scroll __PROTO((void));
extern void get_term __PROTO((void));
extern void init __PROTO((void));
extern void deinit __PROTO((void));
extern void home __PROTO((void));
extern void add_line __PROTO((void));
extern void lower_left __PROTO((void));
extern void bell __PROTO((void));
extern void vbell __PROTO((void));
extern void clear __PROTO((void));
extern void clear_eol __PROTO((void));
extern void so_enter __PROTO((void));
extern void so_exit __PROTO((void));
extern void ul_enter __PROTO((void));
extern void ul_exit __PROTO((void));
extern void bo_enter __PROTO((void));
extern void bo_exit __PROTO((void));
extern void it_enter __PROTO((void));
extern void it_exit __PROTO((void));
extern void nv_enter __PROTO((void));
extern void nv_exit __PROTO((void));
extern void backspace __PROTO((void));
extern void putbs __PROTO((void));

/* Prototypes for functions defined in signal.c */

extern void winch __PROTO((void));
extern void init_signals __PROTO((void));
extern void psignals __PROTO((void));

/* Prototypes for functions defined in ttyin.c */

extern void open_getchr __PROTO((void));
extern int getchr __PROTO((void));
