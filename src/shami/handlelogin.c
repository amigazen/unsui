#include <exec/types.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <dos.h>
#include <dos/var.h>

#include "axsh.h"
#include "users.h"

int handlelogin()
{	struct DPUser *user;
	extern struct Library *DOSBase;
	extern struct Library *AXshBase;

	if((DOSBase=(struct Library *)OpenLibrary("dos.library",37)))
	{
		if((AXshBase=(struct Library *)OpenLibrary("axsh.library",1)))
		{
			AXshSetup();	/* In case we someday put something in there */
			if(user=FindUser(GetProcessUID()&UID_UIDMASK))
			{
				SetVar("USER",user->Loginname,strlen(user->Loginname),GVF_LOCAL_ONLY);
				SetVar("REALNAME",user->Fullname,strlen(user->Fullname),GVF_LOCAL_ONLY);
				SetVar("HOME",user->Home,strlen(user->Home),GVF_LOCAL_ONLY);
				FreeUser(user);
			}
			CloseLibrary(AXshBase);
			return -1;
		}
		CloseLibrary(DOSBase);
	}
	return 0;
}
