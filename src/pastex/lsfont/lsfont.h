

/*
 * Amiga-Includes
 */

#include <exec/types.h>
#include <exec/lists.h>
#include <exec/memory.h>
#include <dos/dos.h>
#include <dos/dosasl.h>
#include <dos/exall.h>

#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <clib/utility_protos.h>

#include <pragmas/exec_pragmas.h>
#include <pragmas/dos_pragmas.h>
#include <pragmas/utility_pragmas.h>


#pragma libcall DOSBase ParsePatternNoCase 3C6 32103
#pragma libcall DOSBase ExAllEnd 3DE 5432105



/*
 * ANSI-C Includes
 */

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>




/*
 * Define amiga.lib function.
 */

extern void __stdargs NewList(struct List * list);

