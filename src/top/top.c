
#define VERSION "2.0 "

#define EARLIEST_LIB	37	/* 2.04 and above */
/* *INDENT-OFF* */

/********************************************************************
 *
 * Name: Top
 *
 *
 * Function:
 *
 *	A program, similar to the UNIX 'top', which displays a list of
 *      Tasks/Process's showing their relative CPU-usage as a percentage.
 *
 *
 * Implemention:
 *
 *	It puts a wedge in front of Exec's Switch() and records the 
 *	execution time of the task/process invoked.
 *	
 *	The display is refreshed every 5 seconds, and lists items in
 *	descending order of usage, i.e. usage hogs are at the "top".
 *
 *	Runs on AmigaDos 2.04 and above.
 *
 *
 *
 *****  Written by Gary Duncan
 *
 *      57 Melbourne Hill Rd
 *      Warrandyte		<< bell-birds live here :-)
 *      Vic 3113
 *      Australia
 *
 * E-mail: gduncan@werple.net.au
 *
 *
 *****  Freely distributable for non-commercial purposes.
 *      Please keep this header intact.
 *
 *
 *      Compiles under SAS 6.x
 *
 *      Formatted with 'indent -gnu' ; a big time-saving program.
 *
 *
 *
 * Acknowledgements:-
 *
 *
 *      1) Gunther Nikl for explaining how Xoper does it (Thanks, Gunther)
 *	   ( and to Werner Gunther for writing the original Xoper)
 *
 *	2) Beta-testers; thanks everyone ...
 * 
 *========================================================================

 Timo Karjalainen	tikarjal@raita.oulu.fi 
 Everett M. Greene	mojaveg@ridgecrest.ca.us
 Johan Thelmin		jth@kuai.se
 Andy Savage		andy@epma.demon.co.uk
 Donald J. Maddox	dmaddox@scsn.net
 Innovision Concepts Ltd keith@innov.demon.co.uk
 Laurent Papier		papier@dpt-info.u-strasbg.fr
 Infinite Dreams	rahamans@cs.man.ac.uk 
 Johan Alfredsson	d95duvan@dtek.chalmers.se
 Petri Nordlund		petrin@walrus.megabaud.fi
 Allan Duncan		a.duncan@trl.telstra.com.au
 	(no relation :)
 Rene Mendoza		mendoza@cmc1.coloradomtn.edu
 Michal Rybaski		mryba@mcs.com

 *========================================================================
 *
 *	3) ... and to C= ; I used chunks of their Console handler code
 *                          from the RKRM C-examples.
 *
 *
 ********************************************************************/

/*** Function List


  myswitch	(void)
  main		(int argc, char **argv)
  build_tsk	(void)
  build_tsk_item	(TOP * tsk_ptr, TASK * t_ptr, int t_num, UBYTE flag)
  copy_task_name	(TOP * tsk_ptr, char *ptr)
  get_proc_name	(PROCESS * p_ptr)
  my_qsort	(void *ptr, int no, int sz, int (*func) ())
  my_cmp	(void *p1, void *p2)
  open_timer	(void)
  start_timer	(int secs)
  deleteTimer	(TIMEREQUEST * xt_req)
  open_console	(void)
  con_cleanup	(void)
  openConsole	(IOSTDREQ * writereq, IOSTDREQ * readreq, WINDOW * window)
  closeConsole	(IOSTDREQ * writereq)
  conPuts	(IOSTDREQ * writereq, UBYTE * string)
  conRead	(IOSTDREQ * readreq, UBYTE * whereto, int len)
  con_win_event	(void)
  get_win_size	(int *x, int *y)
  move_cursor	(int x, int y)
  fatal_err	(char *z)
  hexit		(char *ptr)
  usage		(void)

 ***/

/* *INDENT-ON* */
































/**** ****/

#define TASK_NAME_LEN 25
#define TASK_PROC_LEN 10

#define SAMPLE_SECS	5

#include "top.h"

extern __far ULONG (*oldswitch) (void);
extern __far ULONG wrapper (void);
void __regargs __chkabort (void);
void __regargs
__chkabort (void)
{
}

typedef struct
{
  int t_no;
  int t_calls;
  long t_eticks;
  TASK *t_task;
  int t_taskpri;
  BOOL t_isaCLI;
  char t_type;
  char t_name[TASK_NAME_LEN + 1];
  int t_name_len;
  char t_task_proc[TASK_PROC_LEN + 1];
}
TOP;

#include "proto/top_protos.h"

/*
 * string for AmigaDOS Version Command
 */
static char A_vers[] = "$VER: top\t" VERSION __AMIGADATE__;

static char *what_who = "Top (%s) - written by gduncan@werple.net.au";


static char *hdr =
{"\x1B[1\x1B[7;31;40m\
NO  TYPE       NAME                     PRI   CALLS     SECS   USAGE  STATE  \
\x1B[0;31;40m\n"

};


int my_cmp (void *p1, void *p2);



LIBRARY *TimerBase = NULL;

static TIMEREQUEST *t_req;
static MSGPORT *timerport;
static ULONG e_freq;

#define NTASKS 100

int max_lines;

TOP tlist_a[NTASKS];
TOP tlist_b[NTASKS];

TOP *t_cur;
TOP *t_prev;

TASK *task_list[NTASKS];


static ECLOCKVAL ecl_now;
static ECLOCKVAL ecl_last;

static EXECBASE *ExecBase;

TASK *this_task;
TASK *t_ptr;
int last_sort_count;
int this_sort_count;
int sort_flag;
int t_offs;
ULONG delta;
int task_count;
int task_count_new;
int task_pri = -1;
ECLOCKVAL *ecv;
int not_found;
int eclock;
char title[80];
BOOL g_switch = FALSE;
int sample_time = SAMPLE_SECS;

ULONG e_ticks;
ULONG e_ticks_temp;
ULONG e_ticks_then;

//==================================================

SCREEN scr;

NEWWINDOW nw =
{
  0, 0,				/* starting position (left,top) */
  640, 200,			/* width, height */
  (UBYTE) - 1, (UBYTE) - 1,	/* detailpen, blockpen */

  CLOSEWINDOW | NEWSIZE,	/* flags for idcmp */
  WINDOWDEPTH | WINDOWSIZING |
  WINDOWDRAG | WINDOWCLOSE |
  SMART_REFRESH | ACTIVATE | NEWSIZE,	/* window flags */
  NULL,				/* no user gadgets */
  NULL,				/* no user checkmark */
  title,
  NULL,				/* pointer to window screen */
  NULL,				/* pointer to super bitmap */
  640, 50,			/* min width, height */
  640, 500,			/* max width, height */
  WBENCHSCREEN			/* open on workbench screen */
};


LIBRARY *IntuitionBase = NULL;
WINDOW *win = NULL;
IOSTDREQ *Con_writeReq = NULL;	/* I/O request block pointer */
MSGPORT *Con_writePort = NULL;	/* replyport for writes      */
IOSTDREQ *Con_readReq = NULL;	/* I/O request block pointer */
MSGPORT *Con_readPort = NULL;	/* replyport for reads       */
BOOL OpenedConsole = FALSE;


INTUIMESSAGE *winmsg;
ULONG signals;
UBYTE och;
BYTE oc_error;


/*
 ********************************************************-
 */

/*
 * SetFunc'd into Switch(); called by asm stub, wrapper()
 * 
 * - this is the code which accumulates task run time. 
 * 
 */

void __asm __saveds __interrupt
myswitch (void)
{
  BOOL found = FALSE;

  t_offs = 0;
  t_ptr = (TASK *) ExecBase->ThisTask;

  /*
   * search list for this task
   */
  for (t_offs = 0; t_offs < task_count;)
    {
      if (t_cur[t_offs].t_task == t_ptr)
	{
	  found = TRUE;
	  break;
	}

      if (++t_offs == NTASKS)
	{
	  ++not_found;
	  return;		/* too many tasks... */
	}
    }				/*for */
  /*
   * new task, add to list
   */

  if (found == FALSE)
    {
      build_tsk_item (&t_cur[t_offs], t_ptr, t_offs);
      ++task_count;
    }

  /*
   * increment e-ticks used for this task...
   */
  eclock = ReadEClock (&ecl_now);

  delta = ecl_now.ev_lo - ecl_last.ev_lo;

  t_cur[t_offs].t_eticks += delta;
  t_cur[t_offs].t_calls++;

  ecl_last = ecl_now;

}

/*
 ********************************************************-
 */

void
main (int argc, char **argv)
{
  int k;
  ULONG wait_mask;
  ULONG retval;
  LONG m_timer;
  LONG m_window;
  LONG m_console;

  int my_notfound;
  int total_ticks;
  char con_buf[200];

  TOP *tp;
  int time;
  int usage_percent;
  int usage_dot_percent;
  int secs;
  int msecs;
  char *ptr;
  char buf_usage[40];
  char buf_secs[60];


  /*
   * check for -t option
   */
  if (argc > 1)
    {
      BOOL flag = FALSE;

      if (*(ptr = argv[1]) == '-')
	{
	  if (*++ptr == 't')
	    {
	      sample_time = atoi (++ptr);
	      if (sample_time < 1)
		{
		  fprintf (stderr, "Bad time value\n");
		}
	      else
		{
		  flag = TRUE;
		}
	    }
	}
      if (flag == FALSE)
	{
	  usage ();
	  exit (0);
	}
    }

  ExecBase = *(EXECBASE **) 4;

  this_task = (TASK *) ExecBase->ThisTask;

  sprintf (title, what_who, VERSION);

  open_timer ();

  open_console ();

  eclock = ReadEClock (&ecl_last);

  /*
   * create Wait mask (timer, Console Read , and Write)
   */

  m_timer = 1 << timerport->mp_SigBit;
  m_console = 1 << Con_readPort->mp_SigBit;
  m_window = 1 << win->UserPort->mp_SigBit;

  wait_mask = m_timer | m_console | m_window;

  e_ticks_then = start_timer (1);	/* quick start first up */

  t_cur = tlist_a;
  /*
   * build list of active tasks
   */

  task_count = build_tsk_array (t_cur);

  for (;;)
    {

      /*
       * patch exec's Switch() to ours...
       */

      if (g_switch != TRUE)
	{
	  Forbid ();
	  {
	    oldswitch = (ULONG (*)(void)) SetFunction ((LIBRARY *) ExecBase,
						       -54, wrapper);
	    g_switch = TRUE;
	  }
	  Permit ();
	}

      /*
       * soak up time busy waiting for signals at lowest priority...
       */
      for (;;)
	{
	  task_pri = SetTaskPri (this_task, -127);
	  {
	    while ((retval = (this_task->tc_SigRecvd & wait_mask)) == 0);
	  }
	  SetTaskPri (this_task, task_pri);	/* up priority again... */

	  Disable ();
	  {
	    this_task->tc_SigRecvd &= (~retval);
	  }
	  Enable ();

	  if (retval & m_console)
	    {
	      /* future use ? */

	      retval &= ~m_console;
	    }

	  if (retval & m_window)
	    {
	      retval &= ~m_window;

	      if (con_win_event () == TRUE)	/* window activity */
		{
		  hexit ("");
		}
	    }

	  if ((retval & (~wait_mask)) != 0)
	    {
	      sprintf (con_buf, "Invalid signal bit (%08X)\n", retval);
	      hexit (con_buf);	/*invalid */
	    }

	  if (retval & m_timer)
	    {
	      retval &= ~m_timer;
	      GetMsg (timerport);	/* timer */
	      break;		/* get out of look to print report */
	    }
	}

      Disable ();
      {
	my_notfound = not_found;
	not_found = 0;
	total_ticks = 0;

	/*
	 * switch arrays
	 */

	if (t_cur == tlist_a)
	  {
	    t_cur = tlist_b;
	    t_prev = tlist_a;
	  }
	else
	  {
	    t_cur = tlist_a;
	    t_prev = tlist_b;
	  }

	/*
	 * build new list of active tasks
	 */

	task_count_new = build_tsk_array (t_cur);
      }
      Enable ();

      /*
       * restart timer; get actual elapsed Eticks
       */
      e_ticks_temp = start_timer (sample_time);
      e_ticks = e_ticks_temp - e_ticks_then;
      e_ticks_then = e_ticks_temp;


      /*
       * sort task list in descending order of t_eticks
       */

      my_qsort (t_prev, task_count, sizeof (TOP), my_cmp);

      task_count = task_count_new;

      move_cursor (1, 1);	/* cursor to home */

      conPuts (Con_writeReq, hdr);	/* write header */

      /*
       *===============================================================
       *
       * ! now print report, task by task ... 
       *
       *===============================================================
       */

      for (k = 0; k < max_lines; ++k)
	{
	  tp = &t_prev[k];

	  /*
	   *   ignore empty slots (task has gone) 
	   */

	  if (tp->t_task == (TASK *) 0)
	    continue;

	  time = tp->t_eticks;

	  /*
	   * if time = 0 , just print spaces...
	   */

	  if (time == 0)
	    {
	      strcpy (buf_usage, "     ");
	      strcpy (buf_secs, "      ");	/*ss.mmm */
	    }
	  else
	    {
	      secs = time / eclock;
	      msecs = (time % eclock) / 1000;

	      usage_dot_percent = (((time * 100) % e_ticks) * 10) / e_ticks;
	      usage_percent = (time * 100) / e_ticks;

	      /*
	       * inc to avoid printing all zeros (a "white lie" :) ...
	       */
	      if ((secs + msecs) == 0)
		msecs = 1;

	      if ((usage_percent + usage_dot_percent) == 0)
		++usage_dot_percent;

	      sprintf (buf_secs, "%2d.", secs);
	      sprintf (&buf_secs[3], "%03d", msecs);

	      sprintf (buf_usage, "%2d.", usage_percent);
	      sprintf (&buf_usage[3], "%d%%", usage_dot_percent);
	    }

	  /*
	   * build this tasks report line ... 
	   */

	  sprintf (con_buf, "%2d  %s  %-23s %4d    %4d   %s   %s   %s\n",
		   k,
		   tp->t_task_proc,
		   tp->t_name,
		   tp->t_taskpri,
		   tp->t_calls,
		   buf_secs,
		   buf_usage,
		   get_state (t_cur, tp->t_task));

	  /*
	   * ... and write it to the Console
	   */
	  conPuts (Con_writeReq, con_buf);

	  total_ticks += tp->t_eticks;
	  tp->t_eticks = 0;
	  tp->t_calls = 0;
	}

      /*
       * erase to end of display ( ... since a task may have gone)
       */
      conPuts (Con_writeReq, "\x9B\x4A");

    }

  hexit ("");
}
/*
 *********************************************************************
 */

char *
get_state (TOP * t_ptr, TASK * task_ptr)
{
  char *r_val = "<gone>";
  static char buf[10];
  int j = 0;
  TASK *p;

  while (p = t_ptr[j].t_task)
    {
      if (p == task_ptr)
	{
	  switch (p->tc_State)
	    {
	    case 2:
	      r_val = "run   ";
	      break;

	    case 3:
	      r_val = "ready ";
	      break;

	    case 4:
	      r_val = "wait  ";
	      break;

	    default:
	      sprintf (buf, "%6d", p->tc_State);
	      r_val = buf;
	      break;
	    }
	}
      ++j;
    }

  return (r_val);

}

/*
 *********************************************************************
 */

int
build_tsk_array (TOP * t_ptr)
{
  int tsk_no;
  NODE *node;

  memset (t_ptr, sizeof(t_ptr[NTASKS]), 0);

  tsk_no = 0;

  Forbid ();
  {
    build_tsk_item (&t_ptr[tsk_no], this_task, tsk_no);

    ++tsk_no;

    /*
     * scan Wait list
     */

    for (node = ExecBase->TaskWait.lh_Head;
	 node->ln_Succ; node = node->ln_Succ)
      {
	build_tsk_item (&t_ptr[tsk_no], (TASK *) node, tsk_no);

	++tsk_no;
      }

    /*
     * scan Ready list
     */
    for (node = ExecBase->TaskReady.lh_Head;
	 node->ln_Succ; node = node->ln_Succ)
      {
	build_tsk_item (&t_ptr[tsk_no], (TASK *) node, tsk_no);

	++tsk_no;

      }
  }
  Permit ();

  return (tsk_no);
}

/*
 *********************************************************************
 */
void
build_tsk_item (TOP * tsk_ptr, TASK * t_ptr, int t_num)
{
  char cli_name[40];

  char *ptr_name = t_ptr->tc_Node.ln_Name;
  PROCESS *p_ptr = (PROCESS *) t_ptr;

  tsk_ptr->t_no = t_num;
  tsk_ptr->t_calls = 0;
  tsk_ptr->t_eticks = 0;
  tsk_ptr->t_task = t_ptr;
  tsk_ptr->t_taskpri = t_ptr->tc_Node.ln_Pri;
  tsk_ptr->t_type = t_ptr->tc_Node.ln_Type;
  tsk_ptr->t_isaCLI = FALSE;

  ptr_name = t_ptr->tc_Node.ln_Name;
  tsk_ptr->t_name_len = strlen (ptr_name);

  /*
   * construct Task/Process field 
   */

  if (tsk_ptr->t_type == 1)
    {
      /*
       * its a Task...
       */

      strcpy (tsk_ptr->t_task_proc, "Task     ");
    }
  else
    {
      /*
       * its a Process...
       */

      int pr_num = p_ptr->pr_TaskNum;

      /* 
       * check if CLI...
       */
      if (pr_num == 0)
	{
	  sprintf (tsk_ptr->t_task_proc, "Proc     ");
	}
      else
	{
	  char *p_name;

	  tsk_ptr->t_isaCLI = TRUE;
	  sprintf (tsk_ptr->t_task_proc, "Proc[%2d] ", pr_num);

	  /*
	   * CLI; build command name
	   */
	  p_name = get_proc_name (p_ptr);
	  sprintf (cli_name, "[ %s ]", p_name);
	  ptr_name = cli_name;
	  tsk_ptr->t_name_len = strlen (p_name);
	}
    }
  copy_task_name (tsk_ptr, ptr_name);

}

/*
 *********************************************************************
 */
void
copy_task_name (TOP * tsk_ptr, char *ptr)
{
  int j;

  for (j = 0; j < TASK_NAME_LEN; ++j)
    {
      UBYTE cc = *ptr++;

      if (cc == 0xA9)
	cc = 0;

      tsk_ptr->t_name[j] = cc;

      if (cc == '\0')
	break;
    }
  tsk_ptr->t_name[j] = '\0';
}

/*
 *********************************************************************
 */

char *
get_proc_name (PROCESS * p_ptr)
{

  COMMANDLINEINTERFACE *p_cli = BADDR (p_ptr->pr_CLI);
  char *ptr;
  int len;

  /*
   * if it's a  CLI command, use it as task name
   */
  if (p_cli->cli_Module != 0)
    {
      ptr = BADDR (p_cli->cli_CommandName);
      len = *ptr++;
      ptr[len] = '\0';
    }
  else
    {
      ptr = ((TASK *) p_ptr)->tc_Node.ln_Name;
    }

  return (ptr);
}

/*
 *********************************************************************
 */
BOOL
my_qsort (void *ptr, int no, int sz, int (*func) ())
{
  last_sort_count = this_sort_count;
  this_sort_count = 0;

  qsort (ptr, no, sz, func);

  if (last_sort_count == this_sort_count)
    return TRUE;
  else
    return FALSE;
}

/*
 *********************************************************************
 */

int
my_cmp (void *p1, void *p2)
{
  int retval;

  retval = ((TOP *) p2)->t_eticks - ((TOP *) p1)->t_eticks;
  if (retval > 0)
    ++this_sort_count;

  return retval;
}

/*
 *********************************************************************
 */

void
open_timer (void)
{

  if ((timerport = CreatePort ("TOP", 0)) == NULL)
    {
      fatal_err ("Timer Port create failure\n");
    }

  if ((t_req = (TIMEREQUEST *) CreateExtIO ((MSGPORT *) timerport,
					    sizeof (TIMEREQUEST))) == NULL)
    {
      fatal_err ("Couldn't CreateExtIO in open_timer()\n");
    }

  if (OpenDevice (TIMERNAME, UNIT_VBLANK, (IOREQUEST *) t_req, 0L) != 0)
    {
      fatal_err ("Couldn't open VBLANK Timer\n");
    }

  TimerBase = (LIBRARY *) t_req->tr_node.io_Device;
}
/*
 *********************************************************************
 */

ULONG
start_timer (int secs)
{
  ECLOCKVAL ecl;

  /*
   * set up timer request
   */

  t_req->tr_node.io_Message.mn_ReplyPort = timerport;
  t_req->tr_node.io_Command = TR_ADDREQUEST;
  t_req->tr_node.io_Flags = 0;
  t_req->tr_node.io_Error = 0;
  t_req->tr_time.tv_secs = secs;
  t_req->tr_time.tv_micro = 0;

  SendIO ((IOREQUEST *) & t_req->tr_node);

  /*
   * return current Eclock time
   */
  ReadEClock (&ecl);

  return (ecl.ev_lo);
}

/*
 *********************************************************************
 */
void
deleteTimer (TIMEREQUEST * xt_req)
{

  if (CheckIO ((IOREQUEST *) xt_req) == 0)
    {
      AbortIO ((IOREQUEST *) xt_req);
      WaitIO ((IOREQUEST *) xt_req);
    }

  CloseDevice ((IOREQUEST *) xt_req);
  DeleteExtIO ((IOREQUEST *) xt_req);

  if (timerport)
    DeletePort (timerport);
}

/*
 *********************************************************************
 */

void
open_console (void)

{
  int x;

  if (!(IntuitionBase = OpenLibrary ("intuition.library", EARLIEST_LIB)))
    {
      hexit ("Can only run on AmigaDos 2.04 and above\n");
    }

  /*
   * calculate max number of lines 
   */
  if (GetScreenData (&scr, sizeof (scr), WBENCHSCREEN, NULL) == FALSE)
    {
      hexit ("GetScreenData() fail\n");
    }
  else
    {
      if (scr.Height >= 400)
	{
	  max_lines = 32;
	  nw.Height = 400;	/* NTSC default */
	}
      else if (scr.Height >= 200)
	{
	  max_lines = 14;
	  nw.Height = 200;
	}
      else
	{
	  hexit ("Screen too small\n");
	}
    }
  /*
   * Create reply port and io block for writing to console 
   */

  if (!(Con_writePort = CreatePort ("top.console.write", 0)))
    {
      hexit ("Can't create write port\n");
    }

  if (!(Con_writeReq = (IOSTDREQ *)
	CreateExtIO (Con_writePort, (LONG) sizeof (IOSTDREQ))))
    {
      hexit ("Can't create write request\n");
    }

  /*
   * Create reply port and io block for reading from console 
   */

  if (!(Con_readPort = CreatePort ("top.console.read", 0)))
    {
      hexit ("Can't create read port\n");
    }

  if (!(Con_readReq = (IOSTDREQ *)
	CreateExtIO (Con_readPort, (LONG) sizeof (IOSTDREQ))))
    {
      hexit ("Can't create read request\n");
    }

  /*
   *  Open a window 
   */

  if (!(win = OpenWindow (&nw)))
    {
      hexit ("OpenWindow() fail\n");
    }

  /*
   *  Now, attach a console to the window 
   */

  if (oc_error = openConsole (Con_writeReq, Con_readReq, win))
    {
      hexit ("Can't open console.device\n");
    }
  else
    {
      OpenedConsole = TRUE;
    }
  /*
   *  get size of window (cols, rows) 
   */
  get_win_size (&x, &max_lines);
}
/*
 *********************************************************************
 */

void
con_cleanup (void)
{
  if (Con_readReq)
    {
      if (!(CheckIO ((IOREQUEST *) Con_readReq)))
	{
	  AbortIO ((IOREQUEST *) Con_readReq);

	  WaitIO ((IOREQUEST *) Con_readReq);
	}

    }
  if (OpenedConsole)
    closeConsole (Con_writeReq);

  if (win)
    CloseWindow (win);

  if (Con_readReq)
    DeleteExtIO ((IOREQUEST *) Con_readReq);

  if (Con_readPort)
    DeletePort (Con_readPort);

  if (Con_writeReq)
    DeleteExtIO ((IOREQUEST *) Con_writeReq);

  if (Con_writePort)
    DeletePort (Con_writePort);

  if (IntuitionBase)
    CloseLibrary (IntuitionBase);
}

/*
 *********************************************************************
 */

BYTE
openConsole (IOSTDREQ * writereq, IOSTDREQ * readreq, WINDOW * window)
{
  BYTE open_error;

  writereq->io_Data = (APTR) window;
  writereq->io_Length = sizeof (WINDOW);

  open_error = OpenDevice ("console.device", 0, (IOREQUEST *) writereq, 0);

  readreq->io_Device = writereq->io_Device;	/* clone required parts */
  readreq->io_Unit = writereq->io_Unit;

  return (open_error);
}

/*
 *********************************************************************
 */

void
closeConsole (IOSTDREQ * writereq)
{
  CloseDevice ((IOREQUEST *) writereq);
}

/*
 *********************************************************************
 */


/*
 * Output a NULL-terminated string of characters to a console
 */

void
conPuts (IOSTDREQ * writereq, UBYTE * string)
{
  writereq->io_Command = CMD_WRITE;
  writereq->io_Data = (APTR) string;
  writereq->io_Length = -1;	/* means print till terminating null */

  DoIO ((IOREQUEST *) writereq);
}

/*
 *********************************************************************
 */

/*
 * Queue up a read request to console, passing it pointer
 * to a buffer into which it can read the string 
 */

void
conRead (IOSTDREQ * readreq, UBYTE * whereto, int len)
{

  readreq->io_Command = CMD_READ;
  readreq->io_Data = (APTR) whereto;
  readreq->io_Length = len;

  DoIO ((IOREQUEST *) readreq);
}

/*
 *********************************************************************
 */

int
con_win_event (void)
{
  int flag = FALSE;
  int x;
  int class;

  while (winmsg = (INTUIMESSAGE *) GetMsg (win->UserPort))
    {
      class = winmsg->Class;

      switch (class)
	{
	case CLOSEWINDOW:
	  flag = TRUE;
	  break;

	case NEWSIZE:
	  /*
	   * recalculate max lines, chars in resized window 
	   */
	  get_win_size (&x, &max_lines);

	  break;

	default:
	  break;
	}
      ReplyMsg ((MESSAGE *) winmsg);
    }

  return (flag);
}
/*
 *********************************************************************
 */

void
get_win_size (int *x, int *y)
{
  char buf[12];
  /*
   * request new window size
   */
  conPuts (Con_writeReq, "\033[0 \x71");

  /*
   * examine response, and get x,y  (cols, rows)
   */

  conRead (Con_readReq, buf, sizeof (buf));

  *x = atoi (&buf[8]);
  *y = atoi (&buf[5]) - 2;

}
/*
 *********************************************************************
 */

void
move_cursor (int x, int y)
{
  static char buf[10];

  sprintf (buf, "\x9B%d;%d\x48", y, x);

  conPuts (Con_writeReq, buf);
}
/*
 *********************************************************************
 */

void
fatal_err (char *z)
{
  fprintf (stderr, "ERROR: %s\n", z);
  hexit ("");
}

/*
 *********************************************************************
 */

void
hexit (char *ptr)
{
  if (task_pri != -1)
    SetTaskPri (this_task, task_pri);

  if (*ptr != '\0')
    printf ("%s", ptr);

  /*
   * reset Switch() vector
   */
  if (g_switch == TRUE)
    {
      Forbid ();
      {
	(ULONG (*)(void)) SetFunction ((LIBRARY *) ExecBase, -54, oldswitch);
      }
      Permit ();
    }

  deleteTimer (t_req);

  con_cleanup ();

  exit (0);
}
/*
 *********************************************************************
 */

void
usage (void)
{
  fprintf (stderr, "\
top (version %s): Written by gduncan@werple.net.au\n\n\
 	Prints task usage as a relative %% over sample period, which is\n\
	5 secs, or changeable via -t option.\n\
Usage:	top [-tnn]\n\
\n\
	-t  nn = sample time in secs (default = 5)\n",

	   VERSION);

}
