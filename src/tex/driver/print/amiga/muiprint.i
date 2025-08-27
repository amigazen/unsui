/* muiprint.i */

#ifndef __MUIPRINT_I__
#define __MUIPRINT_I__

void MUIfree(void);
void MUIShowMessWin(void);
void MUIMessage(char * str);
void InitMUIWorkWindow(void);
void MUISetWorkCurPage(int pagenum);
void MUISetGauge(int lines);
void MUIFatal(char * str);
void MUImainwin(void);
void MUIEvent(void);

#endif
