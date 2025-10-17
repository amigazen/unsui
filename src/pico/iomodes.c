#include "iomodes.h"
#include <exec/types.h>
#include <exec/ports.h>
#include <libraries/dosextens.h>
#include <proto/exec.h>
#include <proto/dos.h>

int setmode(LONG flag)
{
	struct MsgPort *port,*rp;
	struct StandardPacket sp;

	if(!(port=(struct MsgPort *)(((struct FileHandle *)(Output()<<2))->fh_Type))) return 0;
	if(!(rp=CreatePort(0,0))) return 0;
	sp.sp_Pkt.dp_Type=ACTION_SCREEN_MODE;
	sp.sp_Msg.mn_Node.ln_Name=(char *)&sp.sp_Pkt;
	sp.sp_Pkt.dp_Link=&sp.sp_Msg;
	sp.sp_Pkt.dp_Port=rp;
	sp.sp_Pkt.dp_Arg1=flag;

	PutMsg(port,&sp.sp_Msg);
	WaitPort(rp);
	GetMsg(rp);
	DeletePort(rp);
	return -1;
}

long iostatus()
{
	struct MsgPort *port,*rp;
	struct StandardPacket sp;

	if(!(port=(struct MsgPort *)(((struct FileHandle *)(Output()<<2))->fh_Type)))
		return -1L;
	if(!(rp=CreatePort(0,0)))
		return -1L;
	sp.sp_Pkt.dp_Type=ACTION_QUERY;
	sp.sp_Msg.mn_Node.ln_Name=(char *)&sp.sp_Pkt;
	sp.sp_Pkt.dp_Link=&sp.sp_Msg;
	sp.sp_Pkt.dp_Port=rp;

	PutMsg(port,&sp.sp_Msg);
	WaitPort(rp);
	GetMsg(rp);
	DeletePort(rp);
	if(sp.sp_Pkt.dp_Res1==DOS_TRUE)
		return sp.sp_Pkt.dp_Res2;
	else
		return -1L;
}

int carrier_lost()
{	long status=iostatus();

	if(status!=-1 && (status & (1L<<5)))
		return -1;
	else
		return 0;
}
