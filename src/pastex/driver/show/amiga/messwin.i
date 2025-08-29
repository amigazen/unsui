/* functions of messwin.c */
extern void OpenMessWin		Args((void));
extern void CloseMessWin	Args((void));
extern void WorkMessWin		Args((int mx, int my,
				      int down, int up));
extern void SetUpMessWin	Args((int delta));
extern void ToggleMessWin	Args((WORD MouseX, WORD MouseY));
extern long MessWinMsg		Args((void));
