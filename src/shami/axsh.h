/* AXsh.h */
#include <exec/types.h>
#include <exec/lists.h>
#include <exec/nodes.h>
#include <dos.h>
#include <dos/dos.h>


/*
 * Stuff used by AXsh
 */

#define SHELL_NAME	"AXsh"

/* states of cmd_state */

#define STATE_INIT	0	    /* undefined state */
#define STATE_INTERNAL	1	/* internal command of shell */
#define STATE_RESIDENT	2	/* command is from Resident segment list */
#define STATE_COMMAND	3	/* an object file to hunt for */
#define STATE_ERROR	4	    /* couldn't be found... */

typedef long Function(char *);

/*
    Utility routines
*/
char *deblank_ptr    (char *p);     /* strips preceeding blanks from p */
char *process_prompt (char *p);     /* does %S and %N substitution in Prompt */
char *process_alias  (char *p);     /* expand aliases */
void push_stream(BPTR old);			/* remember old input */
BPTR pop_stream(BPTR def);			/* get old input */
int empty_stream(void);				/* return TRUE if the stack is empty */

/*
    Internal Commands (char *p is the parameter pointer)
*/
long cmd_pwd	  (char *p);        /* Print What Directory */
long cmd_sh		  (char *p);        /* 'Shell' out (ie, Execute ()) */
long cmd_quit	  (char *p);        /* EndCli, EndShell, exit, quit */
long cmd_xcd	  (char *p);        /* like 'cd' but internal */
long cmd_xecute   (char *p);        /* like 'execute' but use XShell */
long cmd_xpath	  (char *p);        /* like 'path show' but internal */
long cmd_xrun	  (char *p);        /* like 'run' but use XShell */
long cmd_internal (char *p);        /* shows internal commands, and status */

/* Main loop prototypes begin */

long entry_point(void);
void clean_up_io(void);

long shell_init(void);
long shell_cleanup(void);
long is_not_system_call(struct CommandLineInterface *clip,long fn);
long command_examine(char *fname, BPTR *homedir);
long command_device(char *device, char *fname);
long command_processor(long abs_cmd, char *device, char *fname);
long getcommand(char *line,long size);
long docommand(int level,BPTR console);

/* prototypes end */

#include <string.h>

/*
 * Data received from Randell Jesup in private e-mail
 */

#ifndef END_STREAM_CH
#define END_STREAM_CH  -1L
#endif

#ifndef CMD_SYSTEM

/* from dos/dosextens.i (2.04 includes):                              */
/* structure for the Dos resident list.  Do NOT allocate these, use	  */
/* AddSegment(), and heed the warnings in the autodocs!               */

struct Segment
{
	BPTR seg_Next;
	LONG seg_UC;
	BPTR seg_Seg;
	UBYTE seg_Name[4];	/* actually the first 4 chars of BSTR name */
};

#define CMD_SYSTEM	-1
#define CMD_INTERNAL	-2
#define CMD_DISABLED	-999

#endif

long MyRunCommand(char *,char *,BPTR,BPTR,int residents);

#include "axshlanguage.h"

#define COPYRIGHT 		"Copyright �1991-93 Digital Design\n"
#define UPDATED			DATE

#define CMDLINELEN	512

#define ARGLEN	160			/* maximum argument (and input line) lenght */
#define ARGMAX	128			/* max number of arguments */
#define ARGBUF	1024		/* max total size of arguments */

/* These are not included in 1.3 headers - actually these are not valid in 1.3 */
#ifndef AFF_68030
#define AFF_68030       (1<<2)
#endif
#ifndef AFF_68040
#define AFF_68040       (1<<3)
#endif
#ifndef AFF_68881
#define AFF_68881       (1L<<4)
#endif
#ifndef AFF_68882
#define AFF_68882       (1L<<5)
#endif
#ifndef AFF_FPU40
#define AFF_FPU40       (1L<<6)
#endif

#define DUMB	0
#define VT100	1
#define ANSI	2

#define MODE_NORMAL	0
#define MODE_ESC	1
#define MODE_CSI	2

#define MODE_RAW		1
#define MODE_CONSOLE	0

#define PARSE_ARGC_MASK	0x00000fff	/* max 4096 arguments, should be enough */
#define PARSE_PIPED     0x00001000	/* command is piped to the next one */
#define PARSE_MULTI		0x00002000	/* multiple commands on line, call again */
#define PARSE_ERROR		0x00010000
#define REDIR_INOUT		0x00020000
#define REDIR_APPEND	0x00040000
#define REDIR_PIPE		0x00080000  /* Pipe not read from stdin.. */

long parsecommand(BPTR console,
				  char *linein,
				  char *lineout,
				  char *args[],
				  int argmax,
				  char *buffer,
				  int buffersize,
				  char **inFile,
				  char **outFile,
				  char *pipefile);
/*************************************************************************\
* returns argc | flags                                                    *
* linein will be changed to contain leftover command line ( ; or | )      *
* lineout will contain pre-parsed argument string (for RunCommand())      *
* args[0] will contain pointer to command name                            *
* all valid args[] will point to the buffer after this call               *
* args will be made to point into the buffer somewhere                    *
* buffersize should be more than 5*ARGLEN                                 *
\*************************************************************************/

int  getline(BPTR input,char *prompt, UBYTE *str,int lenght, BOOL echo);
void history_free(void);
int  history_expand(char *line, char *expanded, int maxlen);
extern char **histpoint;	/* history pointer array */
extern int histn;			/* number of history entries */
int  fcomp(char *);		/* file name completion */
void fcomplist(char *, int Columns);		/* file name completion */
int  ccomp(char *);		/* command name completion */
void ccomplist(char *, int Columns);		/* command name completion */
char *lowercase(register char *kasa,register char *fromkasa);

void tinyreadconfig(void);
void cleanexit(int);
void openthings(void);

void readconfig(void);
int  isinhomedir(char *dir);
int  getuser(void);
long checkquota(char *dir);
void get_time(char *);
char cat(char *filename);
void updatelog(char *);
int  realpath(char *real,char *fake);	/* convert symbolic path to physical path */

void prompt(char *def);	/* print the prompt */
void sysinfo(void);		/* print information about the system */
char *printout(ULONG);


int handlelogin(void);
