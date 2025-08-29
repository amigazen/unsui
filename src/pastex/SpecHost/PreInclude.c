/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heﬂmann
*/

#include <intuition/intuitionbase.h>

#include <workbench/workbench.h>
#include <workbench/startup.h>

#include <graphics/gfxmacros.h>
#include <graphics/gfxbase.h>
#include <graphics/view.h>

#include <datatypes/pictureclass.h>

#include <exec/execbase.h>
#include <exec/memory.h>

#include <dos/dosextens.h>
#include <dos/dostags.h>
#include <dos/rdargs.h>

#include <libraries/iffparse.h>
#include <libraries/gadtools.h>
#include <libraries/mui.h>
#include <libraries/asl.h>

#include <clib/muimaster_protos.h>
#include <clib/intuition_protos.h>
#include <clib/datatypes_protos.h>
#include <clib/graphics_protos.h>
#include <clib/iffparse_protos.h>
#include <clib/gadtools_protos.h>
#include <clib/utility_protos.h>
#include <clib/exec_protos.h>
#include <clib/icon_protos.h>
#include <clib/alib_protos.h>
#include <clib/asl_protos.h>
#include <clib/dos_protos.h>

#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <dos.h>

#include <pragmas/datatypes_pragmas.h>
#include <pragmas/muimaster_pragmas.h>
#include <pragmas/utility_pragmas.h>
#include <pragmas/exec_pragmas.h>
#include <pragmas/asl_pragmas.h>
