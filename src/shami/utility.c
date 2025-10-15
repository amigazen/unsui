#include <exec/types.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <string.h>
#include <dos.h>
#include <dos/dosextens.h>
#include <dos/dos.h>
#include <dos/dostags.h>
#include <dos/var.h>
#include <stdio.h>

#include "AXsh.h"


/*
	Utility routines
*/

char *deblank_ptr(char *p)
{
	if(p)
		while(*p==' ')
			p++;
	return p;
}

long check_eol(char *p)
{
	/* Short-circuit IS guaranteed in C */
	if(!p || *p=='\0' || *p=='\n' || *p=='\r')
		return 1;
	return 0;
}

long eol_error(char *p)
{
	if(!check_eol(p))
	{
		PutStr("End of command expected\n");
		return RETURN_FAIL;
	}
	return RETURN_OK;
}

char *process_prompt(char *p)
{
	static char buf[256], tmp[256];
	char *q = &buf[0];
	struct Process *myproc = (struct Process *)FindTask(NULL);
	long t, i;

	if(check_eol(p))
		return NULL;

	i = 0;
	*q = '\0';
	while(*p)
	{
		if(*p == '\\')
		{
			p++;
			switch(*p)
			{
			case 'e':
				*q++ = '\033';
				break;
			case 'n':
				*q++ = '\n';
				break;
			case 'r':
				*q++ = '\r';
				break;
			case 't':
				*q++ = '\t';
				break;
			case 'a':
				*q++ = '\a';
				break;
			default:
				*q++ = *p;
			}
			i++;
			p++;
			*q = '\0';
		}
		else if(*p == '^')
		{
			i++;
			p++;
			*q++ = *p++ & 0x1f;
		}
		else if(*p == '%')
		{
			p++;
			switch(*p)
			{
			case 'S':
				if(GetCurrentDirName(tmp, 255-i))
				{
					strcpy(q, tmp);
					while(*q)
					{
						i++;
						q++;
					}
				}
				else
				{
					i += 2;
					*q++ = '%';
					*q++ = *p;
					*q = '\0';
				}
				break;
			case 'D':
				if(GetCurrentDirName(tmp, 255-i))
				{
					t = strlen(tmp)-2;
					while(t>=0 && tmp[t]!='/' && tmp[t]!=':')
						t--;

					strcpy(q, tmp+t+1);
					while(*q)
					{
						i++;
						q++;
					}
				}
				else
				{
					i += 2;
					*q++ = '%';
					*q++ = *p;
					*q = '\0';
				}
				break;
			case 'd':
				if(GetCurrentDirName(tmp, 255-i))
				{
					t = strlen(tmp)-2;
					while(t>=0 && tmp[t]!='/' && tmp[t]!=':')
						t--;

					if(t>0)
						t--;
					while(t>=0 && tmp[t]!='/' && tmp[t]!=':')
						t--;

					strcpy(q, tmp+t+1);
					while(*q)
					{
						i++;
						q++;
					}
				}
				else
				{
					i += 2;
					*q++ = '%';
					*q++ = *p;
					*q = '\0';
				}
				break;
			case 'N':
				/*
					Note: only handles TaskNum up to 99
				*/
				t = myproc->pr_TaskNum;

				if(t>=10)
				{
					i++;
					*q++ = '0' + (char) (t / 10);
					t %= 10;
				}
				i++;
				*q++ = '0' + (char) t;
				*q = '\0';
				break;
			default:
				i++;
				*q++ = '%';
				*q++ = *p;
				*q = '\0';
			}
			p++;
		}
		else
		{
			i++;
			*q++ = *p++;
			*q = '\0';
		}
	}
	return buf;
}

char *process_alias(char *p)
{
	static char buf[CMDLINELEN+1];
	char *q, *r;

	q = deblank_ptr(p);
	if(check_eol(q))
		return NULL;

	r = &buf[0];
	while(*q && *q!=' ' && *q!='\t' && *q!='\n' && *q!='\0')
		*r++ = *q++;
	*r = '\0';

	/* And remember q! */

	if(GetVar(buf, buf, CMDLINELEN-1, LV_ALIAS)==-1)
		return NULL;

	if(strlen(buf) + strlen(q) < CMDLINELEN)
	{
		if(r = strstr(buf, " []"))
		{
			/* handle [] nicely */
			if(strlen(p) + strlen(r+3) < CMDLINELEN)
			{
				if(q[strlen(q)-1] == '\n')
					q[strlen(q)-1] = '\0';
				strcat(q, r+3);
				strcpy(r, q);
				strcpy(p, buf);
				strcat(p, "\n");
				/*PutStr(p);*/
			}
		}
		else
		{
			/* just append to the end... */
			strcat(buf, q);
			strcpy(p,buf);
		}
	}
	return p;
}


static int st_index=0;
static BPTR st_table[20];

void push_stream(BPTR old)
{
	st_table[st_index++]=old;
}

BPTR pop_stream(BPTR def)
{
	if(st_index)
		return st_table[--st_index];
	else
		return def;
}

int empty_stream()
{
	return (st_index==0);
}

