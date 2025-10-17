#include <exec/types.h>
#include <exec/memory.h>
#include <libraries/dosextens.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <dos.h>
#include <time.h>

#include "/lib/iomodes.h"
#include "/lib/misc.h"

int tolower(int);

/* Full screen editor for AXsh by Pasi 'Albert' Ojala */
/* Needs a vt100 or an ansi terminal */
/* Is configurable (term=vt100/ansi,rows=# of rows,edscroll= scroll lenght */

/*  7-Feb-92	Added ins_str and simple one-line Yank */
/* 15-Feb-92	JrComm and Ncomm don't have full ansi/vt102-emulation, but
				console.device has !?!? */
/* 25-Feb-92	Some bug fixes and optimizations in wrapping */
/* 27-Feb-92	Changes: ^c^c^c will exit, EOF detected, backup-autosave */
/* 28-Feb-92	Display now uses cursor movement instead of CR/LF, wrap in readfile */
/* 29-Feb-92	Numerous fixes, removed a Guru which I had coded yesterday.. */
/* 23-Mar-92	Some bug fixes */
/* 07-Jun-92	Better getline (from rn), checks carrier.. */
/* 26-Nov-92	Some changes */

#define VERSION		"0.99"
#define DATE		"27-Nov-92"
#define VERSTAG		"\0$VER: "

static const char vers[] = VERSTAG"Editor "VERSION" ("DATE")\n";

#define NAMELEN 64
#define SCREENX 80
#define WRAPLEN 10
#define AUTOSAVE 400	/* 400 chars, 5 secs==5 chars */

#define	CONSOLE	1	/* not yet used */
#define	VT100	2
#define	ANSI	3


/*	ESC[2J - Clear screen
	ESC[y;xH - cursor position
	ESC[K - erase to EOL
	ESC[2K - erase whole line
cursor codes:
	ESC[A - up
	ESC[B - down
	ESC[C - right
	ESC[D - left	*/

char Filename[NAMELEN] = ".editor",	/* default filename */
	 killed[SCREENX] = "";			/* killed line */

char **E = NULL;	/* E[MAXLINES+1][SCREENX+1] */
int	maxlines = 1024,
	lines = 0,
	pos = 0,
	cx = 0,
	cy = 0,
	errnum = 0,
	term = 0,
	scroll_lenght,
	rows;

FILE *fopen();

void read_file(char *name);
int insert_file(char *name);
void write_file(char *name);
void cur_pos(int y, int x);
void crsr_up(void);
void crsr_down(void);
void crsr_right(void);
void crsr_left(void);
void kill_line(void);
void bs_char(void);
void del_char(void);
void ins_str(char *str, int line, int pos);
void display(void);
void error(int num, char *a);
void helptext(void);
void cleanexit(void);

void __regargs __chkabort(void);
void __regargs __chkabort(void){}
void chkabort(void);
void chkabort(void) {}

char *get_time(void);
void wrap(char *a, char *b, int start);
int getline(char *hotkeys, char *answer, char *line, int maxlen, UBYTE pre);
int vokaali(char c);
int konsonantti(char c);


void main(int argc, char *argv[])
{
	unsigned char c, apuname[NAMELEN], temp[80];
	int i, j, events=0;

	if(!(E=(char **)AllocMem((maxlines+1)*sizeof(char *), MEMF_CLEAR)))
		cleanexit();
	if(!(*E=(char *)AllocMem((maxlines+1)*(SCREENX+1), MEMF_CLEAR)))
		cleanexit();
	for(i=1;i<maxlines+1;i++)
		E[i]=E[i-1]+SCREENX+1;	/* initialize string pointers */

	setmode(MODE_RAW);
	who(COM_GET, NULL, "term", temp);
	for(i=0;temp[i];i++)
		temp[i]=tolower(temp[i]);
	if(!strcmp(temp, "vt100") || !strcmp(temp, "vt102"))
		term=VT100;
	else
		term=ANSI;

	sprintf(apuname, "24");
	who(COM_GET, NULL, "rows", apuname);
	rows=atoi(apuname);
	if(rows<5)	rows=5;
	if(rows>100) rows=100;

	sprintf(apuname, "%d", rows-5);
	who(COM_GET, NULL, "edscroll", apuname);
	scroll_lenght=atoi(apuname);
	if(scroll_lenght<2 || scroll_lenght>rows-2)
		scroll_lenght=rows-5;	/* default scroll value */

	if(argc>1)
		strcpy(Filename, argv[1]);
	read_file(Filename);
	cy=pos=cx=0;
	if(lines<rows-2)
		cy=lines-1; /* if only one page,  go to EOF */
	display();
	if(errnum)
		error(errnum, Filename); /* Display an error,  if file not found */
	cur_pos(cy, cx);

	while(1)
	{
		if(WaitForChar(Input(), 5000000))
		{
			if(Read(Input(), &c, 1)<=0)			/* EOF */
				cleanexit();
			events++;
		}
		else
		{	if(events)
				events+=5;	/* if changes,  timeout increases counter */
			c=0;
			if(SetSignal(0L, SIGBREAKF_CTRL_C) & SIGBREAKF_CTRL_C)
			{	c=3;
				sprintf(apuname, "%s", Filename);
				write_file(apuname);
				cleanexit();
			}
		}
		if(events>AUTOSAVE)
		{	sprintf(apuname, "%s-", Filename);
			error(12, Filename);
			write_file(apuname);
			error(10, Filename);
			events=0;
			cur_pos(cy, cx);
		}
		else
		if(errnum)
		{
			error(0, Filename); /* Remove error from screen,  if any */
			cur_pos(cy, cx);
		}

		if(!c)	continue;	/* while-loop */

		switch(c)
		{
		case 0x0d: /* CR or NL */
		case 0x0a:
			if(lines<maxlines)
			{
				if(cy<lines-1)	/* Appending to the middle of file */
				{
					lines++;
					for(i=lines-1;i>cy;i--)		strcpy(E[i], E[i-1]);
					strcpy(E[cy+1], E[cy]+cx);
					E[cy++][cx]=0;
					cx=0;
					if(cy<pos+rows-2)
					{
						if(term==VT100)
						{
							printf("\033[K\r\n\033[L%s", E[cy]);
							error(0, Filename); /* remove scrolled unused line */
						}
						else		/* "ansi" */
						{
							printf("\033[K\n\r");
							for(i=cy;i<pos+rows-2;i++)
								if(i<lines)
									printf("%s\033[K\n\r", E[i]);
						}
					}
					else	/* If we're gone off page */
					{	pos+=scroll_lenght;
						display();
					}
				}
				else			/* This time appending to EOF */
				{
					strcpy(E[lines++], E[cy++]+cx);
					E[cy-1][cx]=0;
					cx=0;
					if(cy<pos+rows-2)	printf("\033[K\n\r%s", E[cy]);
					else		/* If we're gone off page */
					{	pos+=scroll_lenght;
						display();
					}
				}
				error(0, Filename); /* update file- and %-display */
			}
			else error(3, Filename);
			cur_pos(cy, cx);
			break;

		case 0x08: /* BS */
			bs_char();
			break;
		case 0x04: /* ^D tai DEL */
		case 0x7f:
			del_char();
			break;
		case 0x1b: /* ESC or <cio> */
		case 0x9b:
			if(c==0x1b)
				Read(Input(), &c, 1); /* remove [ from sequence */
			if(c==0x1b)
			{	helptext();
			}
			else
			if(c==0x76)	/* ESC+v page up */
			{	if(pos>0)
				{	pos-=scroll_lenght;
					cy-=scroll_lenght;
					if(cx>strlen(E[cy])) cx=strlen(E[cy]);
					display();
				}
				else
				{	cy=0;
					if(cx>strlen(E[cy])) cx=strlen(E[cy]);
					cur_pos(cy, cx);
				}
			}
			else
			if(c==0x3c)	/* ESC+< goto the beginning of the file (first page) */
			{	if(pos>0)
				{	pos=cy=0;
					if(cx>strlen(E[cy])) cx=strlen(E[cy]);
					display();
				}
				else
				{	cy=0;
					if(cx>strlen(E[cy])) cx=strlen(E[cy]);
					cur_pos(cy, cx);
				}
			}
			else
			if(c==0x3e)	/* ESC+> goto the EOF (last page) */
			{	if(pos<lines-rows+2)
				{	while(pos<lines-rows+2)	pos+=scroll_lenght;
					cy=lines-1;
					if(cx>strlen(E[cy])) cx=strlen(E[cy]);
					display();
				}
				else
				{	cy=lines-1;
					if(cx>strlen(E[cy])) cx=strlen(E[cy]);
					cur_pos(cy, cx);
				}
			}
			else
			if(c==0x9b || c==0x5b) /* cursor movements: <cio> or ESC[ */
			{
				Read(Input(), &c, 1);
				if(c=='A') crsr_up();		/* cursor up */
				if(c=='B') crsr_down();	/* cursor down */
				if(c=='C') crsr_right();	/* cursor right */
				if(c=='D') crsr_left();	/* cursor left */
			}
			break;
		case 0x05: /* ctrl-e goto the EOL */
			cx=strlen(E[cy]);
			cur_pos(cy, cx);
			break;
		case 0x01: /* ctrl-a goto the SOL */
			cx=0;
			cur_pos(cy, cx);
			break;
		case 0x10:
			crsr_up();		/* ctrl-p = cursor up */
			break;
		case 0x0e:
			crsr_down();	/* ctrl-n = cursor down */
			break;
		case 0x06:
			crsr_right();	/* ctrl-f = cursor right */
			break;
		case 0x02:
			crsr_left();	/* ctrl-b = cursor left */
			break;
		case 0x1a:			/* ctrl-z */
			error(11, Filename);
			write_file(Filename);
			sprintf(apuname, "%s-", Filename);
			DeleteFile(apuname);
			cleanexit();

		case 0x03:			/* ctrl-c */
		case 0x18:			/* ctrl-x */
			if(WaitForChar(Input(), 5000000))
			{	if(!Read(Input(), &c, 1))			/* EOF */
				{	sprintf(apuname, "(%s)", Filename);
					write_file(apuname);
					cleanexit();
				}
			}
			else	c=0;

			if(c==0x03 || c=='c')
			{	if(getline("yY", "Modifications will be lost! Do you really want to quit? ", apuname, 5, 0))
					cleanexit();
				if(apuname[0]=='y' || apuname[0]=='Y')	cleanexit();
				else	error(0, Filename);
			}
			if(c=='q')	/* ctrl-x + q  = save and quit */
			{	write_file(Filename);
				sprintf(apuname, "%s-", Filename);
				DeleteFile(apuname);
				cleanexit();
			}
			if(c=='s')
			{	error(11, Filename);
				write_file(Filename);	/* save the file */
				sprintf(apuname, "%s-", Filename);
				DeleteFile(apuname);
				events=0;
			}
			if(c=='w')
			{	if(getline("", "Filename to save to :", apuname, NAMELEN, 0))
					cleanexit();
				if(strlen(apuname))
				{
					if(!strstr(apuname, ":") && !strstr(apuname, "/"))
					{	strcpy(Filename, apuname);
						error(11, Filename);
						write_file(Filename);
						sprintf(apuname, "%s-", Filename);
						DeleteFile(apuname);
						events=0;
					}
					else
						error(14, apuname);
				}
				else
					error(13, Filename);
			}
			if(c=='i')
			{	if(getline("", "File to insert :", apuname, NAMELEN, 0))
					cleanexit();

				if(!strstr(apuname, ":") && !strstr(apuname, "/"))
				{
					if(insert_file(apuname))
					{	while(pos<cy-rows+2)
							pos+=scroll_lenght;
						display();
					}
				}
				else
					error(14, apuname);
			}
			if(cx>strlen(E[cy])) cx=strlen(E[cy]);
			cur_pos(cy, cx);
			break;
		case 0x16:			/* ctrl-v page down*/
			if(cy+scroll_lenght<lines)
			{	pos+=scroll_lenght;
				cy+=scroll_lenght;
				if(cx>strlen(E[cy])) cx=strlen(E[cy]);
				display();
			}
			else
			{	cy=lines-1;
				if(cx>strlen(E[cy])) cx=strlen(E[cy]);
				cur_pos(cy, cx);
			}
			break;
		case 0x0b:			/* ctrl-k remove line/delete to EOL */
			kill_line();
			break;
		case 0x19:			/* ctrl-y Yank killed text back */
			if(killed[0])
			{	ins_str(killed, cy, cx);
				cur_pos(cy, cx);
			}
			break;
		case 0x0c:			/* ctrl-l refresh screen */
			display();
			break;
		case '\t':			/* TAB is now space */
			c=' ';
		default:
			if(c<0x20 || (c>0x7f && c<0xa0)) break; /* no control-characters allowed (yet?)*/

			if(!E[cy][cx])
			{	E[cy][cx++]=c;
				E[cy][cx]=0; /* to end of line */
				Write(Output(), &c, 1);
			}
			else
			{	for(i=strlen(E[cy])+1;i>cx;i--)		E[cy][i]=E[cy][i-1];
				if(term==VT100)
				{	printf("\033[@%c", c);
				}
				else
				{	printf("\033[K%c%s", c, (char *)E[cy]+cx+1);
					cur_pos(cy, cx);
				}
				E[cy][cx++]=c;
			}
			if(strlen(E[cy])>SCREENX-3)	/* wrap! */
			{	char buf[160];

				wrap(E[cy], buf, SCREENX-3);
				if(lines<maxlines)
				{	for(i=++lines;i>cy;i--)	strcpy(E[i], E[i-1]);
					strcpy(E[cy+1], buf);
					if(cy+1<pos+rows-2)		/* new line fits in page */
					{	if(term==VT100)
						{	if(cx>strlen(E[cy]))	/* wrapped cursor */
							{	cx=cx-strlen(E[cy]);
								++cy;
								if(cy<lines)
								{	printf("\033[%dD\033[K\n\r\033[L\033[K%s", cx, E[cy]);
									error(0, Filename);
								}
								else	/* append to end of file */
								{	printf("\033[%dD\033[K\n\r%s", cx, E[cy]);
								}
								cur_pos(cy, cx);
							}
							else
							{	if(cy+1<lines)
								{	printf("%s\033[K\n\r\033[L\033[K%s", E[cy]+cx, E[cy+1]);
									error(0, Filename);
								}
								else	/* append to end of file */
								{	printf("%s\033[K\n\r%s", E[cy]+cx, E[cy+1]);
								}
								cur_pos(cy, cx);
							}
							break;
						}
						else		/* "ansi" */
						{	if(cx>strlen(E[cy]))	/* wrapped cursor */
							{	j=cx;
								cx=strlen(E[cy]);
								cur_pos(cy, cx);
								printf("\033[K");
								for(i=++cy;i<pos+rows-2;i++)
									if(i<lines)
										printf("\n\r\033[K%s", E[i]);
								cx=j-cx;
								error(0, Filename);
								cur_pos(cy, cx);
							}
							else
							{	cur_pos(cy, cx);
								printf("\033[K%s", E[cy]+cx);
								for(i=cy+1;i<pos+rows-2;i++)
									if(i<lines)
										printf("\n\r\033[K%s", E[i]);
								cur_pos(cy, cx);
							}
						}
					}
					else
					{	pos+=scroll_lenght;
						if(cx>strlen(E[cy]))	/* wrapped cursor */
							cx-=strlen(E[cy++]);
						else
							cx=strlen(E[cy]);	/* wrapped behind cursor */
						display();
					}
				}
				else	/* line buffer is full */
				{	E[cy][SCREENX-3]=0;
					cx=strlen(E[cy]);
					error(3, Filename);
					cur_pos(cy, cx);
				}
			}
		}
	}
}


void crsr_up()
{	if(cy>0)
	{	cy--;
		if(cy<pos)	/* if we're off page */
		{	pos-=scroll_lenght;
			display();
		}
		else
		{	Write(Output(), "\033[A", 3);
		}
		if(cx>strlen(E[cy])) /* if the old line was shorter than the current line */
		{	cx=strlen(E[cy]);
			cur_pos(cy, cx);
		}
	}
}


void crsr_down()
{	if(cy<lines-1) /* cursor down */
	{	cy++;
		if(cy>=pos+rows-2)	/* if we're off page */
		{	pos+=scroll_lenght;
			display();
		}
		else
		{	Write(Output(), "\033[B", 3);
		}
		if(cx>strlen(E[cy])) /* if the old line was shorter than the current line */
		{	cx=strlen(E[cy]);
			cur_pos(cy, cx);
		}
	}
}


void crsr_right()
{	if(cx<strlen(E[cy]))
	{	Write(Output(), "\033[C", 3);
		cx++;
	}
	else	/* if we're off line */
	{	if(cy<lines-1)
		{	cx=0;
			cy++;
			if(cy<pos+rows-2)
			{	cur_pos(cy, cx);
			}
			else	/* if we're off page */
			{	pos+=scroll_lenght;
				display();
			}
		}
	}
}


void crsr_left()
{	if(cx>0)
	{	Write(Output(), "\033[D", 3);
		cx--;
	}
	else	/* if we're off line */
	{	if(cy>0)
		{	cy--;
			cx=strlen(E[cy]);
			if(cy<pos)	/* if we're off page */
			{	pos-=scroll_lenght;
				display();
			}
			else
			{	cur_pos(cy, cx);
			}
		}
	}
}


void kill_line()
{	register int i;

	if(E[cy][cx]==0 && cy<lines-1)
	{	char buf[180];

		strcpy(buf, E[cy]);
		strcat(buf, E[cy+1]);
		wrap(buf, E[cy+1], SCREENX-3);
		strcpy(E[cy], buf);
		if(E[cy+1][0])		/* if there was something left in the next line,  just leave if there */
		{	printf("\033[K%s", E[cy]+cx);
			if(cy+1<pos+rows-2)
			{	printf("\n\r%s\033[K", E[cy+1]);
				cur_pos(cy, cx);
			}
			if(strlen(E[cy])==cx) /* if no change goto next line */
			{	cx=0;
				cy++;
				if(cy<pos+rows-2)
				{	cur_pos(cy, cx);
				}
				else
				{	pos+=scroll_lenght;
					display();
				}
			}
		}
		else				/* an empty line - erase it */
		{	lines--;
			for(i=cy+1;i<lines;i++)		strcpy(E[i], E[i+1]);
			E[lines][0]=0;
			printf("\033[K%s", E[cy]+cx);
			if(term==VT100)
			{	printf("\033[%d;H\033[M\033[%d;H%s\033[K", cy+2-pos, rows-2, E[pos+rows-3]);
			}
			else		/* "ansi" */
			{	cur_pos(cy, 0);
				for(i=cy;i<pos+rows-2;i++)
				{	if(i<=lines)
						printf("%s\033[K\n\r", E[i]);
				}
			}
			error(0, Filename);
			cur_pos(cy, cx);
		}
	}
	else	/* delete to the end of line */
	{	strcpy(killed, E[cy]+cx);
		E[cy][cx]=0;
		printf("\033[K");
	}
}


void del_char()
{	register int i;

	if(E[cy][cx])	/* there is something to delete in current line */
	{	strcpy(E[cy]+cx, E[cy]+cx+1);
		if(term==VT100)
		{	printf("\033[P");
		}
		else
		{	printf("%s ", E[cy]+cx);
			cur_pos(cy, cx);
		}
	}
	else
	if(cy<lines-1)		/* if there is line to be 'borrowed' */
	{	char buf[180];

		strcpy(buf, E[cy]);
		strcat(buf, E[cy+1]);
		wrap(buf, E[cy+1], SCREENX-3);
		strcpy(E[cy], buf);
		if(cx>strlen(buf))			/* wrap just shortened the line */
		{	cx=i=cx-strlen(buf);
			if(term==VT100)
			{	printf("\033[%dD\033[K\n\r%s", i, E[++cy]);
			}
			else
			{	while(i)	/* this could be done with ESC[nDESC[K */
				{	printf("\b");
					i--;
				}
				printf("\033[K\n\r%s\033[K", E[++cy]);
			}
			cur_pos(cy, cx);
			return;
		}
		printf("\033[K%s", E[cy]+cx);	/* update the rest of the line */
		if(E[cy+1][0])		/* if there is something left in the next line,  just leave if there */
		{	if(cy+1<pos+rows-2)
			{	printf("\n\r%s\033[K", E[cy+1]);
				cur_pos(cy, cx);
			}
			if(strlen(E[cy])==cx) /* if no change,  delete the first char from the next line */
			{	strcpy(E[cy+1], E[cy+1]+1);
				if(cy<pos+rows-3)
				{	printf("\n\r%s ", E[cy+1]);
					cur_pos(cy, cx);
				}
				else
				{	pos+=scroll_lenght;
					display();
				}
			}
		}
		else				/* we created an empty line - erase it */
		{	lines--;
			for(i=cy+1;i<lines;i++)		strcpy(E[i], E[i+1]);
			E[lines][0]=0;
			if(term==VT100)
			{	printf("\n\r\033[M");
				if(pos+rows-2<lines)	/* scroll line to display */
					printf("\033[%d;H%s\033[K", rows-2, E[pos+rows-3]);
				else					/* delete extra status line.. */
					printf("\033[%d;H\033[K", rows-2);
			}
			else		/* "ansi" */
			{	for(i=cy+1;i<pos+rows-2;i++)
				{	if(i<=lines)
						printf("\n\r%s\033[K", E[i]);
				}
			}
			error(0, Filename);
			cur_pos(cy, cx);
		}
	}
}


void bs_char()
{	register int i;

	if(cx>0)	/* Remove characters */
	{	if(E[cy][cx]==0)	/* delete from EOL */
		{	E[cy][--cx]=0;
			Write(Output(), "\010 \010", 3);
		}
		else				/* delete in the middle of line */
		{	i=--cx;
			while(E[cy][i]=E[cy][i+1]) i++;
			if(term==VT100)
			{	printf("\033[D\033[P");
			}
			else
			{	printf("\b%s\033[K", E[cy]+cx);
			}
			cur_pos(cy, cx);
		}
	}
	else
	if(cy>0)	 /* if lines can be glued together,  they will.. */
	{	char buf[180];

		strcpy(buf, E[cy-1]);
		cx=strlen(buf);
		strcat(buf, E[cy]);
		wrap(buf, E[cy], SCREENX-3);
		strcpy(E[cy-1], buf);
		if(E[cy][0])
		{	if(cy-->pos)
			{	cur_pos(cy, 0);
				printf("%s\033[K\n\r%s\033[K", E[cy], E[cy+1]);
				error(0, Filename);
				cur_pos(cy, cx);
			}
			else	/* if we've gone off page */
			{	pos-=scroll_lenght;
				display();
			}
		}
		else
		{	lines--;
			for(i=cy--;i<lines;i++)		strcpy(E[i], E[i+1]);
			E[lines][0]=0;
			if(cy>=pos)
			{	if(term==VT100)
				{	if(cy<lines-1)
					{	cur_pos(cy+1, 0);
						printf("\033[M");
						cur_pos(cy, cx);
						printf("%s\033[%d;H\033[K", E[cy]+cx, rows-2);
					}
					else
					{	printf("\033[K");
						cur_pos(cy, cx);
						printf("%s\033[K", E[cy]+cx);
					}
					if(lines>pos+rows-3)
					{	printf("\033[%d;H%s", rows-2, E[pos+rows-3]);
					}
					error(0, Filename);
				}
				else		/* "ansi" */
				{	printf("\033[A");
					for(i=cy;i<pos+rows-2;i++)
					{	if(i<lines)
							printf("%s\033[K\n\r", E[i]);
						if(i==lines)
							printf("\033[K");
					}
				}
				cur_pos(cy, cx);
			}
			else	/* if we're gone off page */
			{	pos-=scroll_lenght;
				display();
			}
		}
	}
}


void ins_str(char *str, int line, int xpos)
{
	char temp[260]="", apu[80]="", new[160]="";
	int i;

	strcpy(apu, E[line]+xpos);	/* save the old line end */
	strcpy(temp, E[line]);
	strcpy(temp+xpos, str);

	wrap(temp, new, SCREENX-3);
	strcat(new, apu);
	wrap(new, apu, SCREENX-3);	/* now we have 3 lines,  temp, new and apu */
	strcpy(E[line], temp);
	if(line>=pos && line<pos+rows-2)
		printf("\033[%d;H%s\033[K", line+1-pos, temp);

	if(new[0] && apu[0])
	{
		if(lines<maxlines)
		{
			lines+=2;
			for(i=lines-1;i>line+1;i--)		strcpy(E[i], E[i-2]);
			strcpy(E[line+1], new);
			strcpy(E[line+2], apu);
		}
		else
			error(4, Filename);
	}
	else
	if(new[0])
	{
		if(lines<maxlines)
		{
			lines++;
			for(i=lines-1;i>line+1;i--)		strcpy(E[i], E[i-1]);
			strcpy(E[line+1], new);
		}
		else
			error(4, Filename);
	}

	if(new[0] || apu[0])
	{
		if(term==VT100)
		{
			if(new[0] && line+1>=pos && line+1<pos+rows-2)
			{
				printf("\033[%d;H\033[L%s\033[K", line+2-pos, new);
			}
			if(apu[0] && line+2>=pos && line+2<pos+rows-2)
			{
				printf("\033[%d;H\033[L%s\033[K", line+3-pos, apu);
			}
			error(0, Filename); /* remove scrolled unused line */
		}
		else		/* "ansi" */
		{
			if(line+1>=pos && line+1<pos+rows-2)
			{
				printf("\033[%d;H", line+2-pos);
				for(i=line+1;i<pos+rows-2;i++)
					if(i<lines)
						printf("%s\033[K\n\r", E[i]);
			}
		}
	}
	error(0, Filename); /* update file- and %-display */
}



void cur_pos(int y,  int x)
{
	printf("\033[%d;%dH", y+1-pos, x+1);
	fflush(stdout);
}


void display()
{	register int i;

	printf("\033[;H\033[2J");
	for(i=pos;i<pos+rows-2;i++)
	{
		if(i<lines)
		{
			cur_pos(i, 0);
			printf("%s", E[i]);
		}
	}
	error(0, Filename);
	cur_pos(cy, cx);
}


void error(int nro, char *a)
{	register int i=strlen(a);

	if(i>30)
	{
		while(*a!='/' && *a!=':' && i>28)
		{
			--i;
			++a;
		}
	}

	errnum=nro;
	if(nro)		printf("\033[%d;H", rows); /* cursor position */
	if(nro==1)	printf("\033[1mWrite Failed,  check protections!");
	if(nro==2)	printf("\033[1mFile %s not found!", a);
	if(nro==3)	printf("\033[1mBuffer full,  no more lines!");
	if(nro==4)	printf("\033[1mInsert aborted,  buffer full!");
	if(nro==5)	printf("\033[1mInsert file %s not found!", a);

	if(nro==10) printf("\033[1m%s Written", a);
	if(nro==11) printf("\033[1mWriting %s", a);
	if(nro==12) printf("\033[1mAutosaving %s", a);
	if(nro==13) printf("\033[1mFile %s not written", a);
	if(nro==14) printf("\033[1mIllegal filename %s", a);
	if(!nro) printf("\033[%d;H\033[7m%-32s ESC+ESC for help  Ctrl-z to save&quit  %3d%%  \r\n", rows-1, a, (cy+1)*100/lines);
	printf("\033[m\033[K");
}


void helptext()
{	char c;

	printf("\033[;H\033[2J");
	printf("\033[1m\tHelp-screen %s\033[m\n\n", vers+7);

	printf("\033[1m\tCursor movements\033[m\n");
	printf("ESC[A or ctrl-p\tUp one line\n");
	printf("ESC[B or ctrl-n\tDown one line\n");
	printf("ESC[C or ctrl-f\tForward one position\n");
	printf("ESC[D or ctrl-b\tBack one position\n\n");

	printf("^v\tDown one page\t\tESC v\tUp one page\n");
	printf("ESC <\tFirst page\t\tESC >\tLast page\n");
	printf("^e\tGoto end of line\t^a\tGoto start of line\n\n");

	printf("^l\tRefresh screen\n");
	printf("^k\tDelete to end of line\t^y\tYank the deleted text back\n");
	printf("^x q\tSave and quit\n");
	printf("^x c\tQuit without saving\n");
	printf("^x s\tSave the file\t\tBS\tDelete the previous char\n");
	printf("^x w\tWrite file\t\tDEL\tDelete the next char\n");
	printf("^x i\tInsert file\n");

	printf("\033[%d;0H\033[1mAny key to return to editor\033[m", rows-2);
	if(WaitForChar(Input(), 60000000))
		Read(Input(), &c, 1);	/* 60secs */
	display();
}


int getline(char *hotkeys, char *q, char *a, int b, UBYTE pre)
{	register int i=0, c, f;

	printf("\033[%d;H\033[2K%s", rows, q);
	if(pre)
	{
		printf(a);
		i=strlen(a);
	}
    while(1)
    {
		if(SetSignal(0, 0) & SIGBREAKF_CTRL_C)
			return -1;

		if(WaitForChar(Input(), 4000000))
			c=Read(Input(), &a[i], 1);
		else
			continue;

		if(!c)	return -1;

		/* we mask out '/' and ':' so that write and insert can't crack the system */
		if(c==':' || c=='/')
			continue;

		if(a[i]==0x0a || a[i]==0x0d)
    	{
			a[i]=0;
    		break;
    	}
    	else
    	{
			if(a[i]==0x7f || a[i]==0x08)
    		{
				if(i>0)
				{
					if(a[i-1]<' ')
					{
						Write(Output(), "\010\010  \010\010", 6);
					}
					else
					{
						Write(Output(), "\010 \010", 3);
					}
					i--;
				}
			}
			else
			{
				if(a[i]==0x03)
					return -1;
				if(i<b-2)
				{
					if(a[i]<0x20)
					{
						putchar('^');
						putchar(a[i]+'@');
						i++;
						fflush(stdout);
					}
					else
					if(a[i]<0x80 || a[i]>0x9f)
					{
						Write(Output(), &a[i++], 1);
					}
				}
			}
		}
		if(i==1)
		{
			if(!hotkeys)	/* getchar-mode */
			{
				a[1]=0;
				break;
			}
			f=0;
			while(hotkeys[f]!=a[0] && hotkeys[f])	f++;
			if(hotkeys[f]==a[0])
			{
				a[1]=0;
				break;
			}
		}
	}
	error(0, Filename);
	cur_pos(cy, cx);
	return 0;
}


void read_file(char *name)
{	FILE *handle;
	register int i;
	char buf[160];

	lines=0;
	if(handle=fopen(name, "rb"))
	{
		while(!feof(handle) && lines<maxlines-2)
		{
			if(fgets(buf, 158, handle)==NULL)
				break;

			i=strlen(buf)-1;
			if(buf[i]=='\n' || buf[i]=='\r')
				buf[i]='\0';

			if(i<SCREENX-3)
			{
				strcpy(E[lines++], buf);
			}
			else
			{
				wrap(buf, E[lines+1], SCREENX-3);
				strcpy(E[lines], buf);
				lines+=2;
			}
		}
		fclose(handle);
		E[lines++][0]=0;
		E[lines+1][0]=0;
	}
	else
	{
		errnum=2;
		E[0][0]=E[1][0]=0;
		lines=1;
	}
}


int insert_file(char *insfile)
{	FILE *handle;
	char buf[16][SCREENX+1]; /* insert-buffer */
	register int c, i=0, j=0;

	if(handle=fopen(insfile, "rb"))
	{
		while(!feof(handle) && lines+j<maxlines-2)
		{
			c=fgetc(handle);
			if(c==0x0a || c==0x0d)
			{
				buf[j++][i]=0;
				i=0;
				if(j>15)
				{
					for(i=lines-1;i>cy;i--)
						strcpy(E[i+j], E[i]);
					for(i=0;i<j;i++)
						strcpy(E[cy+i+1], buf[i]);
					lines+=j;
					cy+=j;
					j=i=0;
				}
			}
			else
			{
				if(c==0x09)
					c=0x20;
				if((c>0x1f && c<0x80) || c>0x9f)
				{
					buf[j][i++]=c;
					if(i>SCREENX-4)
					{
						buf[j++][i]=0;
						i=0;
						if(j>15)
						{
							for(i=lines-1;i>cy;i--)
								strcpy(E[i+j], E[i]);
							for(i=0;i<j;i++)
								strcpy(E[cy+i+1], buf[i]);
							lines+=j;
							cy+=j;
							j=i=0;
						}
					}
				}
			}
		}
		fclose(handle);
		if(i>0)
			buf[j++][i]=0;
		for(i=lines-1;i>cy;i--)
			strcpy(E[i+j], E[i]);
		for(i=0;i<j;i++)
			strcpy(E[cy+i+1], buf[i]);
		lines+=j;
		cy+=j;

		while(cy>=pos+rows-2)	/* if we're off page */
		{
			pos+=scroll_lenght;
		}

		if(lines>=maxlines-2)
		{
			display();
			error(4, Filename);
			cur_pos(cy, cx);
			return 0;
		}
		return -1;
	}
	else
	{
		error(5, insfile);
		cur_pos(cy, cx);
		return 0;
	}
}


void write_file(char *name)
{	FILE *handle;
	register int i;

	if(!E[lines-1][0] && !E[lines-2][0] && lines>2)
		lines--;
	if(handle=fopen(name, "wb"))
	{
		for(i=0;i<lines;i++)
			fprintf(handle, "%s\n", E[i]);
		fclose(handle);
		error(10, name);
	}
	else
		error(1, name);
	cur_pos(cy, cx);
}

int vokaali(char c)
{
	switch(c)
	{	case 'a':
		case 'e':
		case 'i':
		case 'o':
		case 'u':
		case 'y':
		case '�':
		case '�':
		case 'A':
		case 'E':
		case 'I':
		case 'O':
		case 'U':
		case 'Y':
		case '�':
		case '�':
			return -1;
		default:
			return 0;
	}
}


int konsonantti(char c)
{
	switch(c)
	{	case 'r':
		case 't':
		case 'p':
		case 'l':
		case 'k':
		case 'j':
		case 'h':
		case 's':
		case 'v':
		case 'n':
		case 'm':
			return -1;
		default:
			return 0;
	}
}


void wrap(char *a, char *b, int start)
{
	char *wrapchars=")>?!;:, .- ";
	register int i, j, k;

	if(start>strlen(a)-1 || start<2)
	{
		b[0]=0;
		return;
	}
	if(strlen(a)+6>2*SCREENX)		/* too long,  just split it */
	{
		strcpy(b, a+strlen(a)/2);
		a[strlen(a)/2]=0;
		return;
	}
	j=start-WRAPLEN;

	if(j<strlen(a)-SCREENX+3)		j=strlen(a)-SCREENX+3;
	if(start<strlen(a)-SCREENX+3)	start=strlen(a)-SCREENX+3;
	if(j<0)	j=0;

	for(i=start;i>j;i--)
	{
		for(k=strlen(wrapchars)-1;k>=0;k--)
		{
			if(a[i]==wrapchars[k])
			{	strcpy(b, a+i+1);
				a[i+1]=0;
				return;
			}
		}
	}

	if(j<2)	j=2;
	for(i=start;i>j;i--)
	{
		if(vokaali(a[i]) && konsonantti(a[i-1]))
		{	strcpy(b, a+i-1);
			/*a[i-1]='-';a[i]=0;*/
			a[i-1]='\0';
			return;
		}
	}

	strcpy(b, a+start);
	a[start]=0;
}


char *get_time()
{	long joo;

	joo=time(&joo);
	return asctime(localtime(&joo));
}


void cleanexit()
{
	printf("\033[0;%dr\033[%d;1H\n\033[2K", rows+1, rows);
	if(E)
	{
		if(*E)
		{	FreeMem(*E, (maxlines+1)*(SCREENX+1));
			*E=0;
		}
		FreeMem(E, (maxlines+1)*sizeof(char *));
		E=0;
	}
	setmode(MODE_CONSOLE);
	SetSignal(0,  SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_D);
	/* clear signals */
	exit(0);
}
