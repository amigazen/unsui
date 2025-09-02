/*
 * pipe.c
 *
 * Copyright (c) 1992, 1994 Ville Saari
 * All rights reserved
 *
 * Created: 30-Oct-92
 * Updated: 27-Dec-94
 */

#define UNAME "PIPE"
#define NAME "pipe"
#define COPYRSIGN "\251"
#define CSI(command) "\233" command

#include "version.h"

static volatile char version[]="$VER: " NAME " " RELEASE "." VERSION " (" VERDATE ")";

#define COPYRIGHT \
	CSI("1m") UNAME CSI("0m") " V" RELEASE "." VERSION "\n"\
	"Copyright " COPYRSIGN " 1992, 1994 Ville Saari\n"\
	"All rights reserved\n"
#define USAGE \
	COPYRIGHT \
	"\n" \
	"Usage: " NAME " <command> [ | <command> ] ...\n"

#include <stdlib.h>
#include <string.h>

#include <proto/exec.h>
#include <exec/memory.h>
#include <exec/execbase.h>

#include <proto/dos.h>
#include <dos/dosextens.h>
#include <dos/dostags.h>
#include <dos/rdargs.h>

#include <stdlib.h>
#include <string.h>

struct handle
	{
	struct FileHandle *fh;
	struct DosPacket *pkt;
	char *ptr;
	int bytes, isoutput;
	struct handle *link;
	enum { UNUSED, INUSE, CLOSED } state;
	};

struct prochandle
	{
	struct Task *task;
	struct handle input, output;
	};

static void rwreturn(struct handle *, long);
static int makeproc(struct prochandle *, char *);
static struct FileHandle *initfh(struct handle *, struct MsgPort *);

struct DosLibrary *DOSBase;

void __asm main(register __a0 char *cmdp)
	{
	int f, copysize, iodisabled=0, commands=0, opencount=0;
	char *p;
	struct Message *msg;
	struct DosPacket *pkt;
	struct MsgPort *port;
	struct handle *handle, *rhandle, *whandle;
	struct prochandle *proctab;

	if(DOSBase=(struct DosLibrary *)OpenLibrary("dos.library", 37))
		{
		p=cmdp;

		while(*p && *p!='\n' && *p!='\r')
			{
			enum { OUT, IN, ESC } state=OUT;

			commands++;

			while(*p && *p!='\n' && *p!='\r' && (state!=OUT || *p!='|'))
				{
				switch(state)
					{
					case OUT:
						if(*p=='"') state=IN;
						break;

					case IN:
						if(*p=='*') state=ESC;
						else if(*p=='"') state=OUT;
						break;

					case ESC:
						state=IN;
					}

				p++;
				}

			if(*p=='|') *p++=0;
			}

		*p=0;

		if(!commands)
			{
			BPTR stderr;

			if(stderr=Open("CONSOLE:", MODE_NEWFILE))
				{
				FPuts(stderr, USAGE);
				Close(stderr);
				}
			}
		else
			{
			if(port=CreateMsgPort())
				{
				if(proctab=AllocMem(commands*sizeof *proctab, MEMF_CLEAR))
					{
					for(f=0; f<commands; f++)
						{
						if(!(proctab[f].input.fh=initfh(&proctab[f].input, port)) ||
							!(proctab[f].output.fh=initfh(&proctab[f].output, port)))
							break;

						proctab[f].output.isoutput=1;

						if(f) proctab[f].input.link=&proctab[f-1].output;
						if(f<commands-1) proctab[f].output.link=&proctab[f+1].input;
						}

					if(f==commands)
						{
						for(f=0; f<commands; f++)
							{
							char *ncmdp=cmdp+strlen(cmdp)+1;

							while(*cmdp==' ' || *cmdp=='\t') cmdp++;

							if(*cmdp=='"')
								{
								char *q;

								p=q=++cmdp;

								for(;;)
									{
									switch(*q)
										{
										case '*':
											if(*(q+1)) q++;
											break;

										case '"':
											q++;

										case 0:
											goto out;
										}

									*p++=*q++;							
									}
								out:

								while(*p++=*q++);
								}

							if(makeproc(proctab+f, cmdp))
								{
								opencount+=2;
								proctab[f].input.state=proctab[f].output.state=INUSE;
								}

							cmdp=ncmdp;
							}

						while(opencount>0)
							{
							unsigned long sigs;

							if(sigs=(Wait(1<<port->mp_SigBit|
								SIGBREAKF_CTRL_C|SIGBREAKF_CTRL_D|SIGBREAKF_CTRL_E|SIGBREAKF_CTRL_F)&
								(SIGBREAKF_CTRL_C|SIGBREAKF_CTRL_D|SIGBREAKF_CTRL_E|SIGBREAKF_CTRL_F)))
									{
									for(f=0; f<commands; f++)
										if(proctab[f].task &&
											proctab[f].input.state==INUSE &&
											proctab[f].output.state==INUSE)
											Signal(proctab[f].task, sigs);

									if(sigs&SIGBREAKF_CTRL_C) iodisabled=1;
									}

							while(msg=GetMsg(port))
								{
								pkt=(struct DosPacket *)msg->mn_Node.ln_Name;

								handle=(struct handle *)pkt->dp_Arg1;

								if(handle->isoutput)
									whandle=handle, rhandle=handle->link;
								else
									rhandle=handle, whandle=handle->link;

								switch(pkt->dp_Type)
									{
									case ACTION_READ:
										if(handle->isoutput)
											{
											ReplyPkt(pkt, 0, ERROR_READ_PROTECTED);
											break;
											}

										if(!handle->link)
											{
											ReplyPkt(pkt,
												Read(Input(), (void *)pkt->dp_Arg2, pkt->dp_Arg3),
												IoErr());
											break;
											}

										goto READ_and_WRITE;

									case ACTION_WRITE:
										if(!handle->isoutput)
											{
											ReplyPkt(pkt, 0, ERROR_WRITE_PROTECTED);
											break;
											}

										if(!handle->link)
											{
											ReplyPkt(pkt,
												Write(Output(), (void *)pkt->dp_Arg2, pkt->dp_Arg3),
												IoErr());
											break;
											}

									READ_and_WRITE:
										if(!pkt->dp_Arg2 || pkt->dp_Arg3<0)
											{
											ReplyPkt(pkt, 0, ERROR_BAD_NUMBER);
											break;
											}

										if(iodisabled)
											{
											ReplyPkt(pkt, 0, ERROR_INVALID_LOCK);
											break;
											}

										if(handle->pkt)
											{
											ReplyPkt(pkt, 0, ERROR_OBJECT_IN_USE);
											break;
											}

										handle->pkt=pkt;
										handle->ptr=(char *)pkt->dp_Arg2;
										handle->bytes=pkt->dp_Arg3;

										if(rhandle->pkt && whandle->pkt)
											{
											copysize=rhandle->bytes<whandle->bytes?
												rhandle->bytes:whandle->bytes;

											CopyMem(whandle->ptr, rhandle->ptr, copysize);

											whandle->ptr+=copysize;
											whandle->bytes-=copysize;
											rhandle->ptr+=copysize;
											rhandle->bytes-=copysize;
											}

										if(rhandle->pkt)
											if(!rhandle->bytes)
												rwreturn(rhandle, 0);
											else if(whandle->state!=INUSE)
												rwreturn(rhandle, 0);

										if(whandle->pkt)
											if(!whandle->bytes)
												rwreturn(whandle, 0);
											else if(rhandle->state!=INUSE)
												rwreturn(whandle, ERROR_INVALID_LOCK);

										break;

									case ACTION_END:
										if(handle->pkt)
											{
											ReplyPkt(pkt, 0, ERROR_OBJECT_IN_USE);
											break;
											}

										if(handle->state!=INUSE)
											{
											ReplyPkt(pkt, 0, ERROR_INVALID_LOCK);
											break;
											}

										opencount--;
										handle->state=CLOSED;

										if(handle->link && handle->link->pkt)
											rwreturn(handle->link, 0);

										ReplyPkt(pkt, 1, 0);
										break;

									default:
										ReplyPkt(pkt, 0, ERROR_ACTION_NOT_KNOWN);
									}
								}
							}
						}

					for(f=0; f<commands; f++)
						{
						if(proctab[f].input.state==UNUSED && proctab[f].input.fh)
							FreeDosObject(DOS_FILEHANDLE, proctab[f].input.fh);

						if(proctab[f].output.state==UNUSED && proctab[f].output.fh)
							FreeDosObject(DOS_FILEHANDLE, proctab[f].output.fh);
						}

					FreeMem(proctab, commands*sizeof *proctab);
					}

				DeleteMsgPort(port);
				}
			}

		CloseLibrary((struct Library *)DOSBase);
		}
	}

static struct FileHandle *initfh(struct handle *h, struct MsgPort *port)
	{
	struct FileHandle *fh;

	if(!(fh=AllocDosObjectTags(DOS_FILEHANDLE, ADO_FH_Mode, MODE_OLDFILE, TAG_END)))
		return 0;

	fh->fh_Type=port;
	fh->fh_Port=0;
	fh->fh_Link=0;
	fh->fh_Arg1=(long)h;

	return fh;
	}

static void rwreturn(struct handle *handle, long error)
	{
	int size;

	if(handle->state==INUSE)
		size=handle->ptr-(char *)handle->pkt->dp_Arg2;
	else
		size=0;

	if(!size && error)
		size=-1;

	ReplyPkt(handle->pkt, size, error);

	handle->pkt=0;
	}

extern char addtaskfunc, mytaskptr, newtaskptrptr, origaddtask;

#define ADDTASKPTR       ((unsigned long (*)())(semaphore+1))
#define ADDTASKSIZE      (&origaddtask-&addtaskfunc)
#define MYTASKPTR        (*(struct Task **)((char *)ADDTASKPTR+(&mytaskptr-&addtaskfunc)))
#define NEWTASKPTRPTR    (*(struct Task ***)((char *)ADDTASKPTR+(&newtaskptrptr-&addtaskfunc)))
#define ORIGADDTASKPTR   (*(unsigned long (**)())((char *)ADDTASKPTR+ADDTASKSIZE))
#define SEMAPHORENAME    "pipe addtask patch"
#define SEMAPHORENAMEPTR ((char *)ADDTASKPTR+ADDTASKSIZE+4)
#define ALLOCSIZE        (sizeof(struct SignalSemaphore)+ADDTASKSIZE+4+sizeof(SEMAPHORENAME))

static int makeproc(struct prochandle *ph, char *cmd)
	{
	struct SignalSemaphore *semaphore;
	int rc=1;

#ifdef DEBUG
	int mysemaphore=0;
#endif

	Forbid();

	if(!(semaphore=FindSemaphore(SEMAPHORENAME)) &&
		(semaphore=AllocMem(ALLOCSIZE, MEMF_CLEAR|MEMF_PUBLIC)))
		{
		memcpy(ADDTASKPTR, &addtaskfunc, ADDTASKSIZE);
		ORIGADDTASKPTR=SetFunction(*(struct Library **)4, -282, ADDTASKPTR);
		memcpy(SEMAPHORENAMEPTR, SEMAPHORENAME, sizeof(SEMAPHORENAME));
		semaphore->ss_Link.ln_Name=SEMAPHORENAMEPTR;
		AddSemaphore(semaphore);

#ifdef DEBUG
		mysemaphore=1;
#endif
		}

	Permit();

	ph->task=0;

	if(!semaphore) return 0;

	ObtainSemaphore(semaphore);

	NEWTASKPTRPTR=&ph->task;
	MYTASKPTR=FindTask(0);
	CacheClearU();

	if(SystemTags(cmd,
		SYS_Input,  MKBADDR(ph->input.fh),
		SYS_Output, MKBADDR(ph->output.fh),
		SYS_Asynch, 1,
		TAG_DONE)<0)
		ph->task=0, rc=0;

	MYTASKPTR=0;
	CacheClearU();

#ifdef DEBUG
	if(mysemaphore)
		{
		RemSemaphore(semaphore);
		SetFunction(*(struct Library **)4, -282, ORIGADDTASKPTR);
		FreeMem(semaphore, ALLOCSIZE);
PutStr("Semrem\n");
		}
	else
#endif

	ReleaseSemaphore(semaphore);

	return rc;
	}
