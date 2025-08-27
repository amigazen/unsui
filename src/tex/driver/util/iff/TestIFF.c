#include <stdio.h>
#include <stdarg.h> 
#include <intuition/intuitionbase.h>
#include "ilbmapp.h"

#include "defines.h"
#include "globals.h"
#include "globals.i"

static UWORD           bw_ctable[2] = { 0x0FFF, 0x0000 };


int main(int argc, char * argv[])
{
  struct ILBMInfo IInfo;
  struct Screen * scr;
  int err;

  memset(&IInfo, 0, sizeof(IInfo));

  if (!IntuitionBase) {
    printf("Uuups...no IntuitionBase!\n");
    exit(5);
  }
  
  IInfo.ParseInfo.iff = AllocIFF();
  if (!IInfo.ParseInfo.iff) {
    printf("Fehler bei AllocIFF()\n");
    exit(5);
  }
  
  scr = IntuitionBase->FirstScreen;
  
  /* err = screensave(&IInfo, scr, NULL, NULL, "ram:screen.out"); */
  
  err = saveilbm(&IInfo, scr->RastPort.BitMap, DEFAULT_MONITOR_ID, 
		scr->Width, scr->Height, 
		scr->Width, scr->Height, 
		bw_ctable, 2, 4, 0xFD, 0,
		NULL, NULL, "ram:bwscreen.out");

  if (err) {
    printf("Fehler bei screensave()!\n");
  }
  else {
    printf("alles ok! Juhuuu!\n");
  }
  
  FreeIFF(IInfo.ParseInfo.iff);
  
  return 0;
}




void __stdargs Warning(char *fmt,...)  	/* issue a warning */
{
  volatile va_list argptr;

  va_start(argptr, fmt);
  printf(fmt, argptr);
  va_end(argptr);
}
