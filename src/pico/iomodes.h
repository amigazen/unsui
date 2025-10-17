#include <exec/types.h>

/* modes for ACTION_SCREEN_MODE */
#define MODE_CONSOLE	0L	/* normal console mode (buffered, crlf translations, echo) */
#define MODE_RAW		1L	/* raw mode, no line editing nor echo */
#define MODE_NOCRLF		2L	/* CR not added to LF, all codes will be sent (0)*/

#define ACTION_QUERY	64	/* our own action */

#define DOS_FALSE		0L
#define DOS_TRUE		-1L


int  setmode(LONG mode);	/* set stdio mode */
long iostatus(void);		/* get status (serial) -1 == does not support*/
int  carrier_lost(void);	/* true value will mean that no carrier is present */

