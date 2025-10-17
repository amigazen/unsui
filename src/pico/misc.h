char *lowercase(char *kasa,char *fromkasa);

/* make a lowercase string of the fromkasa and store it to kasa */
/* returns 'kasa' */
/* =============================================================== */

int makepath(char *destin,char *partial);

/* add a '/' or ':' to the string partial, if necassary and store the result
   to the string destin, will return a non-zero value if something is wrong */
/* =============================================================== */


unsigned char *crypt(unsigned char *dest,unsigned char *source);

/* scrambles the source string, destination must have space for 8+1 chars.
   The string to be crypted should be at least 8 bytes long. Return value is
   the destination string start address. This is a one-way coding. */
/* =============================================================== */

#include <exec/types.h>
#include <exec/memory.h>
#include <libraries/dosextens.h>

UBYTE who(UBYTE,UBYTE,char *,char *);

/*	Beware! This routine uses CreatePort. So, if you have included arp_proto.h,
	you Must have 'struct Library *ArpBase' BEFORE 'include "lib/misc.h".
	Without arp-library the CreatePort will be linked from amiga.lib */

/* sysghost commands */
#define COM_ADD		'a'		/* add IO user tty */
#define COM_REMOVE	'r'		/* remove IO */
#define COM_UPDATE	'u'		/* update IO what */
#define COM_WRITE	'w'		/* write IO user text */
#define COM_WHO		'l'		/* who IO buffer maxlen */
#define COM_QUIT	'q'		/* quit */
#define COM_SET		's'		/* set IO env value */
#define COM_SETSYS	'y'		/* setsys IO sys value */
#define COM_UNSET	'x'		/* unset IO env */
#define COM_GET		'g'		/* get IO env/sys value */	/* we are using just this one! */
#define COM_GETN	'h'		/* getn IO n value */

/* New for 1.31 */
#define COM_READ	'd'		/* read IO buffer maxlen */
#define COM_KICK	'k'		/* kick IO user */
#define COM_SHUTDOWN 'X'

/* =============================================================== */

int copyfile(char *destination,char *source);

/* copies the file and does not use system copy-command */
/* returns 0 for success */
/* =============================================================== */

