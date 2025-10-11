/*
 * value of $KSH_VERSION
 */

#ifndef lint
static char *RCSid = "$Id: version.c,v 1.9 93/09/29 13:17:51 sjg Exp $";
#endif

#include "stdh.h"
#include <setjmp.h>
#include "sh.h"
#include "patchlevel.h"

char ksh_version [] =
	"KSH_VERSION=@(#)PD KSH v4.9 93/09/29";


