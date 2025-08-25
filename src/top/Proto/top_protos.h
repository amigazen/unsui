/* Prototypes for functions defined in
top.c
 */

void __regargs __chkabort(void);

extern LIBRARY * TimerBase;

extern int max_lines;

extern TOP tlist_a[100];

extern TOP tlist_b[100];

extern TOP * t_cur;

extern TOP * t_prev;

extern TASK * task_list[100];

extern TASK * this_task;

extern TASK * t_ptr;

extern int last_sort_count;

extern int this_sort_count;

extern int sort_flag;

extern int t_offs;

extern ULONG delta;

extern int task_count;

extern int task_count_new;

extern int task_pri;

extern ECLOCKVAL * ecv;

extern int not_found;

extern int eclock;

extern char title[80];

extern BOOL g_switch;

extern int sample_time;

extern ULONG e_ticks;

extern ULONG e_ticks_temp;

extern ULONG e_ticks_then;

extern SCREEN scr;

extern NEWWINDOW nw;

extern LIBRARY * IntuitionBase;

extern WINDOW * win;

extern IOSTDREQ * Con_writeReq;

extern MSGPORT * Con_writePort;

extern IOSTDREQ * Con_readReq;

extern MSGPORT * Con_readPort;

extern BOOL OpenedConsole;

extern INTUIMESSAGE * winmsg;

extern ULONG signals;

extern UBYTE och;

extern BYTE oc_error;

void __asm __interrupt __saveds myswitch(void);

void main(int , char ** );

char * get_state(TOP * , TASK * );

int build_tsk_array(TOP * );

void build_tsk_item(TOP * , TASK * , int );

void copy_task_name(TOP * , char * );

char * get_proc_name(PROCESS * );

BOOL my_qsort(void * , int , int , int (* )());

int my_cmp(void * , void * );

void open_timer(void);

ULONG start_timer(int );

void deleteTimer(TIMEREQUEST * );

void open_console(void);

void con_cleanup(void);

BYTE openConsole(IOSTDREQ * , IOSTDREQ * , WINDOW * );

void closeConsole(IOSTDREQ * );

void conPuts(IOSTDREQ * , UBYTE * );

void conRead(IOSTDREQ * , UBYTE * , int );

int con_win_event(void);

void get_win_size(int * , int * );

void move_cursor(int , int );

void fatal_err(char * );

void hexit(char * );

void usage(void);

