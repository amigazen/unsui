/*** skippage.c		-> move over a page	(hes) 31.05.90 ***/


#include "defines.h"

#include <stdio.h>
#include <ctype.h>

#include "commands.h"
#include "globals.h"
#include "flmt.h"
#include "new_font.h"
#include "dvihand.h"

#ifdef ANSI
#  include <string.h>
#  include <stdlib.h>
#endif

#ifdef AZTEC_C
#  include <functions.h>
#endif

#include "globals.i"
#include "dvihand.i"
#include "liste.i"
#include "new_font.i"
#include "dospecia.i"


extern long PreLoad;


int skippage(DVIFILE *fp, long *cpagep, long *ppagep,
		long *current_page, long *current_page_phy)
{
  short command;
  long i;
  unsigned long len;
  int endoffile = FALSE;

  while (!DVIfeof(fp) && !endoffile && (command=Read1Byte(fp)) != EOP) {
    switch (command) {
	case BOP:
		*cpagep = DVIftell(fp) - 1;

		*current_page = Read4Byte(fp);
		for (i=1; i<=9; i++) {
		  (void)Read4Byte(fp);
		}
		*ppagep = Read4Byte(fp);

		             /* each new page should be marked */
		if ( take_pptr(*current_page, *cpagep, 0L, FALSE)==0L ) {
		  in_list(*current_page,*cpagep, 0L);
		}
		*current_page_phy = get_phy_number(*cpagep);
		break;

	case POST:
		endoffile = TRUE;
		break;

	case XXX1:
	case XXX2:
	case XXX3:
	case XXX4:
		len = NoSignExtend(fp, command-XXX1+1);
		SkipDoSpecial(fp, len);
		break;

    	case SET2:	case SET3:	case SET4:
    	case PUT2:	case PUT3:	case PUT4:
	case FNT2:	case FNT3:	case FNT4:
	case FNT_DEF2:	case FNT_DEF3:	case FNT_DEF4:
	case PRE:
	case POST_POST:
	case 250: case 251: case 252: case 253: case 254: case 255:
		FatalStr(5,"unexpected command (%d) in DVI file!",command);
		break;
	case SET_RULE:
	case PUT_RULE:
		(void)Read4Byte(fp);
	case RIGHT4:
	case W4:
	case X4:
	case DOWN4:
	case Y4:
	case Z4:
		(void)Read4Byte(fp);
		break;
	case RIGHT3:
	case W3:
	case X3:
	case DOWN3:
	case Y3:
	case Z3:
		(void)Read3Byte(fp);
		break;
	case RIGHT2:
	case W2:
	case X2:
	case DOWN2:
	case Y2:
	case Z2:
		(void)Read2Byte(fp);
		break;
	case SET1:
	case PUT1:
	case RIGHT1:
	case W1:
	case X1:
	case DOWN1:
	case Y1:
	case Z1:
	case FNT1:
		(void)Read1Byte(fp);
		break;
	case FNT_DEF1:
		i = NoSignExtend(fp, command-FNT_DEF1+1);
		if (HasBeenRead(i)) {
			SkipFontDef ();
		}
		else {
			ReadFontDef (i, FALSE);		/* only define ! */
		}
		break;
	default:
		break;
    }
  }
  if (DVIfeof(fp)) {
    endoffile = TRUE;
  }
  
  return endoffile;
}
