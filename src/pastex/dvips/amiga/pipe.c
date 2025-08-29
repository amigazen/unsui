/*
 *   pipe.c
 *
 *   Implements popen and pclose using the Per Bojsen's APIPE: device.
 *   Alternative emulation of popen (read only) is also provided, for people
 *   who haven't the APIPE: device.
 *
 *   Written by Giuseppe Ghibò <ghibo@galileo.polito.it>
 *
 *   last revised: 28 Sep 1994.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <ios1.h>
#include <ctype.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <proto/dos.h>
#include "pipe.h"

static int ExistsDevice(const char *);
static int bstricmp(const char *, BPTR);
static int tempnumber = 0;

FILE *popen(command, type)
const char *command, *type;
{
	static char cmd[512];

	if (ExistsDevice("APIPE")) {
		sprintf(cmd, "APIPE:%s", command);
		return( fopen(cmd, type) );
	}
	else
	{
		FILE *fh;
		char tempname[18];
		struct UFB *myufb;

		sprintf(tempname,"T:dvipstemp.%05d",tempnumber++);

		switch (*type) {
			case 'w':
				return((FILE *) NULL); /* "w" mode is currently not supported with popen as redirection to a file */

			case 'r':
				sprintf(cmd,"%s >%s",command,tempname);
				system(cmd);
				if ((fh = fopen(tempname,type)) != NULL) {
					myufb = chkufb(fileno(fh));
					myufb->ufbflg |= UFB_TEMP;
				}
				return(fh);

			default:
				return((FILE *) NULL);
		}
	}
}

int pclose(stream)
FILE *stream;
{
	return( fclose(stream) );
}

/*
 * ExistsDevice (const char *devname);
 *
 * devname = name of device to search for, without trailing ':'
 *
 * This function returns TRUE if devname exists as a device, else it
 * returns FALSE.
 *
 */
 
static int ExistsDevice(const char *devname)
{
	extern struct DosLibrary *DOSBase;
	struct DosInfo *dinfo=(struct DosInfo *) BADDR(DOSBase->dl_Root->rn_Info);
	struct DeviceList *dlist=(struct DeviceList *) BADDR(dinfo->di_DevInfo);

	while (dlist) {
		if (dlist->dl_Type == DLT_DEVICE) {
			if (!bstricmp(devname,dlist->dl_Name)) return(TRUE);
		}
		dlist = (struct DeviceList *) BADDR (dlist->dl_Next);
	}
	return(FALSE);
}

/*
 * bstricmp (cstring, bstring)
 *
 * This function compares a C NULL-terminated string with a BCPL string.
 * It returns 0 if strings are equal, else returns 1.
 *
 * const char *cstring = pointer to string in C format (null-terminated)
 * BSTR bstring        = pointer to string in BCPL format (first byte = length)
 *
 */

static int bstricmp(const char *cstring, BSTR bstring)
{
	const char *p = (const char *) BADDR(bstring);
	UBYTE len = (UBYTE) *p++;

	while (len--) {
		if (!*cstring)
			return(1); /* strings have different lengths */
		if (toupper(*p) != toupper(*cstring))
			return(1); /* strings have different chars */
		*p++;
		*cstring++;
	}
	if ( *cstring )
		return(1); /* BCPL string is a substring of C string */

	return(0);
}
