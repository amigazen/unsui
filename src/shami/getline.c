#include <exec/types.h>
#include <exec/memory.h>
#include <dos/dosextens.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <string.h>
#include <stdlib.h>
#include <dos.h>
#include <dos/var.h>
#include <stdio.h>

int tolower(int);

#include "AXsh.h"

static char *histbuf = NULL, **histpoint = NULL, *histcur;
static int histbufsz, histpointsz, histn=0;


void history_add(char *line)
{
	int i, len = strlen(line)+1;
	char *loc;

	if(!histbuf)
	{
		char temp[40];

		histpointsz = 50;
		if(GetVar("history", temp, sizeof(temp), GVF_LOCAL_ONLY) != -1)
		{
			histpointsz = atoi(temp);
		}
		if(histpointsz < 1)
		{
			histpointsz = 1;
		}
		if(!(histpoint = (char **)AllocMem(histpointsz*sizeof(char *), MEMF_PUBLIC)))
		{
			return;
		}

		histbufsz = 128*histpointsz;
		if(GetVar("historybuf", temp, sizeof(temp), GVF_LOCAL_ONLY) != -1)
		{
			histbufsz = atoi(temp);
		}
		if(histbufsz < 128*histpointsz)
		{
			histbufsz = 128*histpointsz;
		}
		if(!(histbuf = (char *)AllocMem(histbufsz, MEMF_PUBLIC)))
		{
			FreeMem(histpoint, histpointsz*sizeof(char *));
			return;
		}
		histcur = histbuf;
	}

#if 0
	for(i=0;i<histn;i++)
	{
		if(line == histpoint[i])
		{
			break;
		}
	}
#else
	for(i=histn-1;i>=0;i--)
	{
		if(!strcmp(line, histpoint[i]))
		{
			break;
		}
	}
#endif

	if(i<0 || i==histn)	/* String is new.. */
	{
		if(len > histbufsz)
			return;	/* No way we can fit it in! */

		/* Doesn't fit.. make room */
		while(histcur+len > histbuf+histbufsz)
		{
			int j = strlen(histbuf)+1;

PutStr("History: Cleanup/size\n");
			for(i=0;i<histn;i++)
			{
				histpoint[i] -= j;
				if(histpoint[i] < histbuf)
				{
					histpoint[i] = histbuf;
				}
			}
			memmove(histbuf, histbuf+j, histbufsz-j);
			histcur -= j;
		}
		loc = histcur;
		histcur += len;
		strcpy(loc, line);
	}
	else
	{
		loc = histpoint[i];
	}

	if(histn == histpointsz)
	{
		/* Pointer doesn't fit.. make room */
		for(i=1;i<histn;i++)
		{
			histpoint[i-1] = histpoint[i];
		}
		histn--;
	}

	histpoint[histn++] = loc;
}


void history_free()
{
	if(histbuf)
	{
		FreeMem(histpoint, histpointsz*sizeof(char *));
		FreeMem(histbuf, histbufsz);
	}
}


void convert_funcstr(char *funcstr)
{
	char *target = funcstr;
	char c;

	while(*funcstr)
	{
		switch(c = *funcstr++)
		{
		case '\\':
			if(*funcstr)
			{
				switch(c = *funcstr++)
				{
				case 'n':
					*target++ = 0x0a;
					break;
				case 't':
					*target++ = 0x09;
					break;
				case 'a':
					*target++ = 0x07;
					break;
				case 'e':
					*target++ = 0x1b;
					break;
				case 'r':
					*target++ = 0x0d;
					break;
				default:
					*target++ = '\\';
					*target++ = c;
				}
			}
			break;
		case '^':
			if(*funcstr)
			{
				*target++ = (*funcstr++ & 0x1f);
			}
			break;
		default:
			*target++ = c;
		}
	}
	*target = '\0';
}


int getline(BPTR input, char *pr, UBYTE *a, int lenght, BOOL echo)
{
	static int func=-1;
	static UBYTE funcstr[ARGLEN+1], killbuf[ARGLEN+1];
	int autologout=4;
	UBYTE c=0, temp[ARGLEN+2];	/* temporary for expansions etc. */
	int done=0, term=DUMB, hist=histn, oldhist=histn, altc=0, mode=MODE_NORMAL;
	long Signals, count=0, i=0, j, k, error = 0;
	BYTE oldpri = SetTaskPri(FindTask(NULL),2);
	APTR oldWindowPtr = ((struct Process *)FindTask(NULL))->pr_WindowPtr;

	int carrier_checking=0;
	int Rows=-1,Columns=-1;



	((struct Process *)FindTask(NULL))->pr_WindowPtr = (APTR)-1;
	SetMode(input, MODE_RAW);
	SetSignal(0L, SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_D | SIGBREAKF_CTRL_E | SIGBREAKF_CTRL_F);
	if(GetVar("term", temp, sizeof(temp), GVF_LOCAL_ONLY) == -1)
	{
		term = DUMB;
	}
	else
	{
		if(!strnicmp(temp,"vt100",5) || !strnicmp(temp,"vt102",5))
			term = VT100;
		else if(!strnicmp(temp,"ansi",4))
			term = ANSI;
	}
	if(Rows<0 || Columns<0)
	{
		Rows = 24;
		Columns = 80;
		PutStr("\2330 q"); /* Query the window size */
	}

	if(GetVar("columns",temp,sizeof(temp),GVF_LOCAL_ONLY) != -1)
	{
		Columns = atoi(temp);
	}
	if(GetVar("rows",temp,sizeof(temp),GVF_LOCAL_ONLY) != -1)
	{
		Rows = atoi(temp);
	}

	i = 0;
	a[0] = '\0';

	if(!carrier_checking)
	{
		PutStr("\23311{");		/* Report - Window closed */
		PutStr("\23312{");		/* Report - Window size changed */
		Flush(Output());
	}

	while(!done)
	{
		c = '\0';
		j = 1;

		if(func != -1)
		{
			if((SetSignal(0L,0L) & SIGBREAKF_CTRL_C))
			{
				PutStr("^C");
				mode = 0;
				func = -1;
				continue;
			}
			c = funcstr[func++];

			if(!funcstr[func])
				func = -1;
		}
		else
		{
			j = Read(input, &c, 1);
#if 0
			if(WaitForChar(input, 4000000))
			{
				j = Read(input,&c,1);
			}
			else
			{
/*
 * TODO: Handle read mode so that WaitForChar() isn't used on devices that
 *		 doesn't support it. Now this is SLOW with dpipe:, if works at all.
 */
				if(IoErr() ==  ERROR_ACTION_NOT_KNOWN)
				{
					j=Read(input,&c,1);
				}
			}
#endif
		}
		if(!j || j < 0)
		{
			error = -1;
			goto getexit;
		}

		if(mode == MODE_NORMAL && c == 0x0c)
			PutStr("^L");
		Signals = SetSignal(0L,0L);	/* this is more compatible than SysBase->ThisTask->tc_SigRecvd */
		if(Signals & SIGBREAKF_CTRL_C)
		{
			PutStr("^C");
			c = 0x0c;	/* refresh line */
			mode = 0;
			SetSignal(0L, SIGBREAKF_CTRL_C);	/* clear pending break */
		}
		if(0 && (Signals & SIGBREAKF_CTRL_D))
		{
			PutStr("^D");
			error = -1;
			goto getexit;
		}

		if(c == 0x12)	/* ^r - refresh line also */
			c = 0x0c;

		if(c == 0)
		{
			/* NUL (timeout 2s) */
			count++;
			/*
			if(carrier_checking && (count/30)>=autologout)
			{
				error = -1;
				goto getexit;
			}
			*/
			/*
			if(logged_in)
			{
				sprintf(temp,"AXsh:dev/%s",tty);
				if(file=Lock(temp,ACCESS_READ))
				{
					UnLock(file);
					PutStr("\a\n");
					cat(temp);
					DeleteFile(temp);
					c=0x0c;
				}
				if((count&0x0f) == 1)
				{
					if(check_mail(mail,user.Loginname,user.Home))
					{
            			PutStr(printout(MSG_NEW_MAIL_ARRIVED));
						c=0x0c;
					}
				}
			}
			*/
			/*
			if(file=Lock("AXsh:etc/nologin",ACCESS_READ))
			{
				UnLock(file);
				if(!logged_in)
				{
					PutStr(printout(MSG_SHUTDOWN_FORCED));
					error = -1;
					goto getexit;
				}
				printf(printout(MSG_SHUTDOWN_MSG),user.Loginname,thismachine);
				cat("AXsh:etc/nologin");
				if(!(user.UID & UID_SUPERUSER))
				{
					PutStr(printout(MSG_SHUTDOWN_FORCED));
					error = -1;
					goto getexit;
				}
				else
				{
					PutStr(printout(MSG_SHUTDOWN_PLEASE));
				}
				c=0x0c;
			}
			*/
			if(!c)
				continue;
		}

		if(mode == MODE_NORMAL && c == 0x9b)
		{
			mode = MODE_CSI;
			altc = 0;
		}
		else if(mode == MODE_NORMAL)
		{
			switch(c)
			{
			case 0x0c:	/* ^L - refresh*/
				PutStr("\n");
				PutStr(pr);
				if(term != DUMB)
					PutStr("\033[K");
				if(echo)
					PutStr(a);
				if(j = strlen(a)-i)
				{
					if(term == VT100)
					{
						sprintf(temp,"\033[%ldD",j);
						PutStr(temp);
					}
					else
					{
						if(term == ANSI)
						{
							for(;j>0;j--)
								PutStr("\033[D");
						}
						else
						{
							for(;j>0;j--)
								FPutC(Output(),'\b');
						}
					}
				}
				break;
			case 0x0a:	/* LF or CR */
			case 0x0d:
				PutStr(a+i);
				done=1;
				break;
			case 0x1b:
				mode = MODE_ESC;
				break;
			case 0x9b:
				mode = MODE_CSI;
				altc = 0;
				break;
			case 0x10:
				if(hist)
					hist--;
				break;
			case 0x0e:
				if(hist < histn)
					hist++;
				break;
			case 0x06:	/* crsr right */
				if(i < strlen(a))
				{
					if(echo)
					{
						if(term == DUMB)
							FPutC(Output(),a[i]);
						else
							PutStr("\033[C");
					}
					i++;
				}
				break;
			case 0x02:	/* crsr left */
				if(i)
				{
					if(echo)
					{
						if(term == DUMB)
							FPutC(Output(),'\b');
						else
							PutStr("\033[D");
					}
					i--;
				}
				break;
			case 0x01:		/* ^a - goto start of line */
				if(echo)
				{
					if(term == VT100)
					{
						if(i)
						{
							sprintf(temp,"\033[%ldD",i);
							PutStr(temp);
						}
					}
					else
					{
						while(i)
						{
							if(term == DUMB)
								FPutC(Output(),'\b');
							else
								PutStr("\033[D");
							i--;
						}
					}
				}
				i = 0;
				break;
			case 0x05:		/* ^e - goto eol */
				if(echo)
					PutStr(a+i);
				i = strlen(a);
				break;
			case 0x09:		/* TAB (expand command/filename) */
				if(i && echo)
				{
					int x,y, res;

					j = i-1;
					while(j>=0)
					{
						if(a[j]==' ' || a[j]=='>' || a[j]=='\"')
						{
							if(j && a[j-1]!='\\')
								break;
						}
						j--;
					}
					j++;
#if 1
					x = j;
					y = 0;
					while(x<i)
					{
						if(a[x]=='\\')
							x++;
						temp[y++] = a[x++];
					}
					temp[y] = '\0';
#else
					strcpy(temp, a+j);
					temp[i-j] = '\0';
#endif
					oldpri = SetTaskPri(FindTask(NULL), oldpri);

					if(j > 0)
						res = fcomp(temp);
					else
						res = ccomp(temp);

					oldpri = SetTaskPri(FindTask(NULL), oldpri);

					if(strlen(temp)+strlen(a)-i+j+1<lenght)
					{
						k=0;
						while(temp[k])
						{
							switch(temp[k])
							{
							case ' ':
								/* if found exact, skip the added space */
								if(res && !temp[k+1])
									break;
								/* Fall through */
							case '\"':
							case '?':
							case '#':
							case '$':
							case '*':
							case '|':
							case ';':
							case '`':
							case '!':
							case '&':
								memmove(temp+k+1, temp+k, strlen(temp+k)+1);
								temp[k++] = '\\';
								break;
							}
							k++;
						}

						k = strlen(temp)+j;
						if(a[i])
						{
							if(a[i] == ' ' && temp[strlen(temp)-1] == ' ')
							{
								strcat(temp, a+i+1);
							}
							else
							{
								strcat(temp, a+i);
							}
							strcpy(a+j, temp);
						}
						else
						{
							strcpy(a+j, temp);
						}


						if(j<i)
						{
							if(term == VT100)
							{
								sprintf(temp, "\033[%ldD", i-j);
								PutStr(temp);
							}
							else
							{
								int z;

								if(term == DUMB)
								{
									for(z=i;z>j;z--)
										FPutC(Output(), '\b');
								}
								else
								{
									for(z=i;z>j;z--)
										PutStr("\033[D");
								}
							}
						}
						PutStr(a+j);

						i = strlen(a+k);
						if(i)
						{
							if(term == VT100)
							{
								sprintf(temp,"\033[%ldD",i);
								PutStr(temp);
							}
							else
							{
								if(term == DUMB)
								{
									for(j=i;j>0;j--)
										FPutC(Output(),'\b');
								}
								else
								{
									for(j=i;j>0;j--)
										PutStr("\033[D");
								}
							}
						}
						i=k;
						oldhist=hist=histn;
					}
				}
				break;
			case 0x04:		/* ^D (only in IgnoreEof-mode) */
/*
				if(i == 0 && a[0] == '\0' && ignoreeof)
				{
					PutStr("^D");
					error = -1;
					goto getexit;
				}
				else
*/
				if(i && a[i] == '\0' && echo)	/* If on the EOL, show completions */
				{
					j = i-1;
					while(j >= 0 && a[j] != ' ' && a[j] != '>' && a[j] != '\"')
						j--;
					strcpy(temp,a+j+1);
					temp[i-j-1]=0;
					PutStr("\n");

					k=j;
					while(k>=0 && a[k] == ' ')
						k--;	/* See if we have spaces before the command */

					oldpri = SetTaskPri(FindTask(NULL), oldpri);

					if(k>=0)
						fcomplist(temp, Columns);
					else
						ccomplist(temp, Columns);

					oldpri = SetTaskPri(FindTask(NULL), oldpri);

					PutStr(pr);
					if(term != DUMB)
						PutStr("\033[K");
					if(echo)
						PutStr(a);
					break;
				}
				/* Fall through */
			case 0x7f:		/* DEL & ^D */
				if(a[i])
				{
					strcpy(a+i,a+i+1);
					if(echo)
					{
						if(term == VT100)
						{
							PutStr("\033[P");
						}
						else
						{
							if(term == DUMB)
							{
								PutStr(a+i);
								PutStr(" ");
								for(j=strlen(a+i)+1;j>0;j--)
									FPutC(Output(),'\b');
							}
							else
							{
								sprintf(temp,"%s\033[%ldD",a+i,strlen(a+i)+1);
								PutStr(temp);
							}
						}
					}
					oldhist=hist=histn;
				}
				break;
			case 0x08:		/* BS */
				if(i>0)
				{
					i--;
					j=i;
					while(a[j]=a[j+1])
						j++;
					if(echo)
					{
						PutStr("\b");
						PutStr(a+i);
						PutStr(" ");
						if(term == VT100)
						{
							sprintf(temp,"\033[%ldD",strlen(a+i)+1);
							PutStr(temp);
						}
						else
						{
							if(term == ANSI)
							{
								for(j=strlen(a+i)+1;j>0;j--)
									PutStr("\033[D");
							}
							else
							{
								for(j=strlen(a+i)+1;j>0;j--)
									FPutC(Output(),'\b');
							}
						}
					}
					oldhist=hist=histn;
				}
				break;
			case 0x0b:	/* ^K - delete to the end of line */
				if(term != DUMB)
				{
					PutStr("\033[K");
				}
				else
				{
					for(j=strlen(a+i);j>0;j--)
						FPutC(Output(),' ');
					for(j=strlen(a+i);j>0;j--)
						FPutC(Output(),'\b');
				}
				strcpy(killbuf,a+i);
				a[i]='\0';
				oldhist=hist=histn;
				break;
			case 0x15:	/* ^U - clear start of the line */
				if(i)
				{
					if(term == VT100)
					{
						sprintf(temp,"\033[%ldD",i);
						PutStr(temp);
					}
					else
					{
						if(term == DUMB)
						{
							for(j=i;j>0;j--)
								FPutC(Output(),'\b');
						}
						else
						{
							for(j=i;j>0;j--)
								PutStr("\033[D");
						}
					}
					j=strlen(a);	/* remember old length */
					strncpy(killbuf,a,i);
					killbuf[i]='\0';
					strcpy(a,a+i);

					if(term == VT100)
					{
						if(a[0])
						{
							sprintf(temp,"%s\033[K\033[%ldD",(LONG)a,strlen(a));
							PutStr(temp);
						}
						else
							PutStr("\033[K");
					}
					else
					{
						if(term == DUMB)
						{
							PutStr(a);
							for(i=strlen(a);i<j;i++)
								FPutC(Output(),' ');
							for(;i>0;i--)
								FPutC(Output(),'\b');
						}
						else
						{
							PutStr(a);
							PutStr("\033[K");
							for(j=strlen(a);j>0;j--)
								PutStr("\033[D");
						}
					}
					i=0;
					oldhist=hist=histn;
				}
				break;
			case 0x17:	/* ^W - delete previous word */
				if(i)
				{
					j=i;
					if(a[i-1] == ' ')
					{
						FPutC(Output(),'\b');
						j--;
					}
					else
					{
						while(j && a[j-1] != ' ')
						{
							FPutC(Output(),'\b');
							j--;
						}
					}
					strncpy(killbuf,a+j,i-j);
					killbuf[i-j]='\0';
					strcpy(a+j,a+i);

					if(term == VT100)
					{
						if(a[j])
						{
							sprintf(temp,"%s\033[K\033[%ldD",(LONG)a+j,strlen(a+j));
							PutStr(temp);
						}
						else
							PutStr("\033[K");
					}
					else
					{
						if(term == DUMB)
						{
							PutStr(a+j);
							for(c=i-j;c>0;c--)
								FPutC(Output(),' ');
							for(i=strlen(a+j)+i-j;i>0;i--)
								FPutC(Output(),'\b');
							c=0x17;	/* Just be sure it has no side effects */
						}
						else
						{
							PutStr(a+j);
							PutStr("\033[K");
							for(i=strlen(a+j);i>0;i--)
								PutStr("\033[D");
						}
					}
					i=j;
					oldhist=hist=histn;
				}
				break;
			case 0x18:	/* ^X - clear line */
				if(strlen(a))
				{
					if(term == DUMB)
					{
						for(j=strlen(a+i);j>0;j--)
							FPutC(Output(),' ');
						for(j=strlen(a);j>0;j--)
							FPutC(Output(),'\b');
						for(j=strlen(a);j>0;j--)
							FPutC(Output(),' ');
						for(j=strlen(a);j>0;j--)
							FPutC(Output(),'\b');
					}
					else
					{
						if(i)
						{
							if(term == VT100)
							{
								sprintf(temp,"\033[%ldD",i);
								PutStr(temp);
							}
							else
							{
								for(j=i;j>0;j--)
									PutStr("\033[D");
							}
						}
						PutStr("\033[K");
					}
					strcpy(killbuf,a);
					a[0]='\0';
					i=0;
					oldhist=hist=histn;
				}
				break;
			case 0x19:	/* ^Y - yank killed text back */
				strcpy(funcstr,killbuf);
				func=0;
				break;
			case 0x1c:	/* ^\, EOF */
				error = -1;
				goto getexit;

			default:
				if(strlen(a)<lenght-1 && c>0x1f && (c<0x80 || c>0x9f)) /* we don't want control chars */
				{
					for(j=strlen(a) ; j>=i ; j--)
						a[j+1] = a[j];
					a[i] = c;
					if(echo)
					{
						PutStr(a+i);
						if(a[i+1])
						{
							if(term == VT100)
							{
								sprintf(temp,"\033[%ldD",strlen(a+i+1));
								PutStr(temp);
							}
							else
							{
								if(term == DUMB)
								{
									for(j=strlen(a+i+1);j>0;j--)
										PutStr("\b");
								}
								else
								{
									for(j=strlen(a+i+1);j>0;j--)
										PutStr("\033[D");
								}
							}
						}
					}
					i++;
					oldhist = hist = histn;
				}
			}
		}
		else if(mode == MODE_ESC)
		{
			if(c == 0x5b)
			{
				mode=MODE_CSI;	/* if it is [, we are in the sequence mode */
				altc=0;
			}
			else
			{
				mode=MODE_NORMAL;
			}
		}
		else if(mode == MODE_CSI)
		{
			switch(c)
			{
			case 'A':
				temp[altc]=0;
				if(altc == 0)
				{
					if(hist)		/* crsr up */
						hist--;
				}
				else
				{
					if(temp[0] == ' ')
					{	/* shift-left - goto start of line */
						if(echo)
						{
							if(term == VT100)
							{
								if(i)
								{
									sprintf(temp,"\033[%ldD",i);
									PutStr(temp);
								}
							}
							else
							{
								while(i)
								{
									PutStr("\033[D");
									i--;
								}
							}
						}
						i=0;
					}
					else
					{
						PutStr("<csi>");
						PutStr(temp);
						PutStr("A\n");
					}
				}
				mode = MODE_NORMAL;
				break;
			case 'B':
				if(hist < histn)	/* crsr down */
					hist++;
				mode = MODE_NORMAL;
				break;
			case 'C':
				if(i < strlen(a))	/* crsr right */
				{
					PutStr("\033[C");
					i++;
				}
				mode = MODE_NORMAL;
				break;
			case 'D':
				if(i)			/* crsr left */
				{
					PutStr("\033[D");
					i--;
				}
				mode = MODE_NORMAL;
				break;
			case 'Z':	/* shift-TAB */
			case 'T':
				temp[altc] = 0;
				altc = 0;

				/* shift-up */
				if(echo)	/* history completion */
				{
					char	found[ARGLEN], name1[ARGLEN];
					int		len = strlen(a), d = 0;
					BOOL	done = FALSE;

					strcpy(name1, a);
					found[0] = '\0';
					for(j=histn-1; j>=0; j--)
					{
						if(!strnicmp(name1, histpoint[j], len))
						{
							if(histpoint[j][len] == '\0')		/* exact history name */
							{
								strcpy(found, name1);
								done = TRUE;
								break;
							}
							if(found[0])
							{
								d = len;
								while(found[d] == histpoint[j][d])
									d++;
								if(found[d])	/* the histories are not identical */
								{
									found[d] = '\0';
									done  =FALSE;
								}
							}
							else
							{
								strcpy(found, histpoint[j]);
								done = TRUE;
								if(!name1[0])
									break;
							}
						}
					}
					if(!done)
						PutStr("\a");
					if(found[0])
					{
						if(strlen(found)+2 < lenght)
						{
							strcpy(a+len, found+len);
							PutStr(found+i);
						}
						else
							PutStr("\a");
					}
					i = strlen(a);
				}
				mode = MODE_NORMAL;
				break;
			case 'S':
				temp[altc] = '\0';
				altc = 0;

				/* shift-down */
				mode = MODE_NORMAL;
				break;
			case 'r':
				temp[altc] = '\0';
				altc = 0;

				if(!strncmp(temp,"1;1;",4))
				{
					Rows = atoi(temp+4);
					Columns = atoi(temp+6+((temp[6] == ';')?1:0));
					sprintf(temp,"%ld",Rows);
					SetVar("rows",temp,strlen(temp),GVF_LOCAL_ONLY);
					sprintf(temp,"%ld",Columns);
					SetVar("columns",temp,strlen(temp),GVF_LOCAL_ONLY);
				}
				else
				{
					PutStr("Oops! Not exactly what I expected.. (<csi>");
					PutStr(temp);
					PutStr("r)\n");
				}

				mode=MODE_NORMAL;
				break;
			case '@':
				temp[altc] = '\0';
				altc = 0;

				/* shift-right - goto eol */
				if(echo)
					PutStr(a+i);
				i = strlen(a);

				mode = MODE_NORMAL;
				break;
			case '~':
				temp[altc] = '\0';
				altc = 0;
				if(temp[0] == '?')
				{
					strcpy(funcstr,"HELP");
					if(GetVar(funcstr,funcstr,ARGLEN,GVF_LOCAL_ONLY) != -1)
					{
						func = 0;
						convert_funcstr(funcstr);
					}
					/* HELP */
				}
				else
				{
					func = atoi(temp)+1;	/* get the key name */
					sprintf(funcstr,"F%ld", func);
					if(GetVar(funcstr,funcstr,ARGLEN,GVF_LOCAL_ONLY) != -1)
					{
						func = 0;
						convert_funcstr(funcstr);
					}
					else
					{
						func = -1;
					}
				}
				mode = MODE_NORMAL;
				break;
			case '|':
				temp[altc] = '\0';
				altc = 0;
				if(!strncmp(temp,"11",2))	/* close gadget pressed */
				{
					error = -1;
					goto getexit;
				}
				else if(!strncmp(temp,"12",2))	/* size changed */
				{
					PutStr("\2330 q");		/* Query the new size */
				}

				mode = MODE_NORMAL;
				break;
			case '{':
				temp[altc] = '\0';
				altc = 0;
				PutStr("<csi>");
				PutStr(temp);
				PutStr("{\n");

				mode = MODE_NORMAL;
				break;
			default:
				if(altc < ARGLEN)
					temp[altc++] = c;
				break;
			}
		}

		if(hist != oldhist && echo)
		{
			if(hist < histn)
			{
				/* if we have selected an event */
				strcpy(a, histpoint[hist]);
			}
			else
			{
				a[0] = '\0';
			}
			if(i)
			{
				if(term == VT100)
				{
					sprintf(temp,"\033[%ldD",i);	/* print new event and delete the rest of the line */
					PutStr(temp);
				}
				else
				{
					if(term == DUMB)
					{
						for(j=i; j>0; j--)
							PutStr("\b");
					}
					else
					{
						for(j=i; j>0; j--)
							PutStr("\033[D");
					}
				}
			}
			PutStr(a);
			PutStr("\033[K");		/* print new event */
			i = strlen(a);
			oldhist = hist;
		}
		Flush(Output());
	}
	PutStr("\n");
	SetSignal(0L,SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_D | SIGBREAKF_CTRL_E | SIGBREAKF_CTRL_F);
	/* Clear signals */

	if(echo && a[0])
	{
		/* if the line was a new one and non-blank, add event */

		if(hist == histn)
		{
			history_add(a);
		}
		else
		{
			history_add(histpoint[hist]);
		}
	}
	strcat(a,"\n");

getexit:
	Flush(Output());

	if(!carrier_checking)
	{
		/* Do not report - Window closed, Window size changed */
		PutStr("\23311}\23312}");
		Flush(Output());
	}
	SetMode(input, MODE_CONSOLE);
	SetTaskPri(FindTask(NULL), oldpri);
	((struct Process *)FindTask(NULL))->pr_WindowPtr = oldWindowPtr;
	SetSignal(0L, SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_D | SIGBREAKF_CTRL_E | SIGBREAKF_CTRL_F);

	return error;	/* return the right value.. */
}

#if 0
int check_mail(char *mailhome,char *loginname,char *home)
{
	char temp[200];
	BPTR lock,file;
	static struct DateStamp lastmail={-1,-1,-1};
	static long mailsize=0;
	struct FileInfoBlock *fib;
	int error=0;

	if(!(fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL)))
		return error;

	if(lastmail.ds_Days == -1)	/* first time around here get .lastlogin datestamp */
	{
		sprintf(temp,"%s.lastlogin",home);
		if(lock = Lock(temp,ACCESS_READ))
		{
			if(Examine(lock, fib))
			{
				lastmail = fib->fib_Date;	/* remember the date */
			}
			UnLock(lock);
		}
		FreeDosObject(DOS_FIB, fib);
		return error;
	}

	sprintf(temp, "%s%s", mailhome, loginname);	/* any new mail to user? */
	if(lock = Lock(temp, ACCESS_READ))
	{
		if(Examine(lock, fib))
		{
			if(lastmail.ds_Days < fib->fib_Date.ds_Days ||
				((lastmail.ds_Days == fib->fib_Date.ds_Days) && (lastmail.ds_Minute < fib->fib_Date.ds_Minute)) ||
				((lastmail.ds_Days == fib->fib_Date.ds_Days) && (lastmail.ds_Minute == fib->fib_Date.ds_Minute) && lastmail.ds_Tick < fib->fib_Date.ds_Tick))
			{
			    if(fib->fib_Size > mailsize)
			    {
	    			error=-1;

	    			sprintf(temp, "%s.lastlogin", home);	/* touch .lastlogin */
	    			if(file = Open(temp, MODE_READWRITE))
	    			{
	    				Seek(file, OFFSET_END, 0);
	    				Write(file, "", 0);
	    				Close(file);
	    			}
	    		}
			}
			lastmail = fib->fib_Date;	/* remember the date */
    		mailsize = fib->fib_Size;
		}
		UnLock(lock);
	}
	/* else no mail at all */
	FreeDosObject(DOS_FIB, fib);
	return error;
}
#endif

int fcomp(char *keyname)
{
	struct	FileInfoBlock *fib;
	BPTR 	mylock;
	char	*found, *temppi, *name1, *name2, *name3;
	int		len, i=0, q;
	BOOL	done=FALSE, dir=FALSE;
	const	int size=5*ARGLEN+sizeof(struct FileInfoBlock);

	if((fib=(struct FileInfoBlock *)AllocMem(size,MEMF_CLEAR)))
	{
		found = (char *)fib+sizeof(struct FileInfoBlock);
		name3 = (name2=(name1=(temppi=found+ARGLEN)+ARGLEN)+ARGLEN)+ARGLEN;

		found[0] = '\0';
		strcpy(temppi, keyname);
		i = strlen(temppi);
		while(i>=0 && temppi[i] != '/' && temppi[i] != ':')
			i--;
		q = i+1;
		lowercase(name1, temppi+q);
		len = strlen(name1);
		temppi[q] = '\0';

		if(mylock=Lock(temppi,ACCESS_READ))
		{
			Examine(mylock,fib);
			while(ExNext(mylock,fib))
			{
				lowercase(name2, fib->fib_FileName);
				if(!strncmp(name2,name1,len))
				{
					if(found[0])
					{
						i=len;
						while(found[i] == name2[i] && found[i])
							i++;
						if(found[i])	/* filenames aren't identical */
						{
							found[i] = '\0';
							strcpy(name3, fib->fib_FileName);
							name3[i] = '\0';
							done = FALSE;
						}
					}
					else								/* the first match */
					{
						strcpy(name3, fib->fib_FileName);
						strcpy(found, name2);			/* name2 is already in lowercase */
						if(fib->fib_DirEntryType > 0)
							dir = TRUE;
						done = TRUE;
						if(!name1[0])
							break;
					}
				}
			}
			UnLock(mylock);
			if(dir && done)
				strcat(name3, "/");	/* we have a directory */
			if(!dir && done)
				strcat(name3, " ");	/* we have a complete filename */
			if(found[0])
				strcpy(keyname+q, name3);
		}
		FreeMem(fib, size);
	}
	if(!done)
	{
		PutStr("\a");
		return 0;
	}
	return -1;
}


void fcomplist(char *keyname,int Columns)
{
	struct	FileInfoBlock *fib;
	BPTR 	mylock;
	char	*temppi, *name1, *name2;
	int		len, i, q, num=0;
	const	int size=3*ARGLEN+sizeof(struct FileInfoBlock);

	if((fib=(struct FileInfoBlock *)AllocMem(size,MEMF_CLEAR)))
	{
		temppi = (char *)fib + sizeof(struct FileInfoBlock);
		name1 = temppi + ARGLEN;
		name2 = name1 + ARGLEN;

		strcpy(temppi, keyname);
		i = strlen(temppi);
		while(i>=0 && temppi[i] != '/' && temppi[i] != ':')	i--;
		q = i+1;
		lowercase(name1, temppi+q);
		len = strlen(name1);
		temppi[q] = '\0';

		if(mylock=Lock(temppi,ACCESS_READ))
		{
			Examine(mylock, fib);
			while(ExNext(mylock, fib))
			{
				lowercase(name2, fib->fib_FileName);
				if(!strncmp(name2, name1, len))
				{
					num += strlen(fib->fib_FileName)+2;
					if(fib->fib_DirEntryType>0)
					{
						if(num > Columns)
						{
							PutStr("\n");
							PutStr(fib->fib_FileName);
							PutStr("/ ");
							num = strlen(fib->fib_FileName)+2;
						}
						else
						{
							PutStr(fib->fib_FileName);
							PutStr("/ ");
						}
					}
					else
					{
						if(num > Columns)
						{
							PutStr("\n");
							PutStr(fib->fib_FileName);
							PutStr("  ");
							num = strlen(fib->fib_FileName)+2;
						}
						else
						{
							PutStr(fib->fib_FileName);
							PutStr("  ");
						}
					}
				}
			}
			UnLock(mylock);
			if(num)
				PutStr("\n");
		}
		FreeMem(fib, size);
	}
}


char *lowercase(register char *dest,register char *src)
{	register char *ret=dest;

	while(*dest++ = tolower(*src++));
	return ret;
}
