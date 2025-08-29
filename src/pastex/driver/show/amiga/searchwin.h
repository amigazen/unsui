/* searchwin.h */

extern struct Window * SearchWin;
extern UBYTE sig_searchwin;	// ist in amscreen.c deklariert!

enum SearchActions { SEARCHWIN_ACTION_NONE, SEARCHWIN_ACTION_SEARCH, SEARCHWIN_ACTION_STRING, SEARCHWIN_ACTION_CANCEL, SEARCHWIN_ACTION_CLOSE };
