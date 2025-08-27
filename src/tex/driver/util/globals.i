/* Prototypes for functions defined in globals.c */

extern void *xmalloc		Args((unsigned bytes));
extern void  xfree		Args((void *));
extern void  xfree_all		Args((void));

#if defined(BETACOPYRIGHT)
extern void PrintBetaCopy(void);
#endif

extern char *GetCopy		Args((void));
extern void PrintAuthors	Args((void));
extern void AbortRun		Args((int code));

extern void __stdargs Fatal	Args((int ret, int msg, ... ));
extern void __stdargs Logging	Args((int msg, ... ));
extern void __stdargs Message	Args((int msg, ... ));
extern void __stdargs Warning	Args((int msg, ... ));

extern void __stdargs FatalStr		Args((int ret, char *fmt, ... ));
extern void __stdargs LoggingStr	Args((char *fmt, ... ));
extern void __stdargs MessageStr	Args((char *fmt, ... ));
extern void __stdargs WarningStr	Args((char *fmt, ... ));


#ifdef ATARI
extern void beep		Args((void));
#endif

#ifdef AMIGA
extern void _abort		Args((void));
extern int  CXBRK		Args((void));
extern int  call_rexx		Args((char *str, int *result));
extern int  call_mf		Args((char *fntname,
				      long fontmag,
				      long hres,
				      long vres,
				      long driver_type,
				      char *pkname,
				      char *pkdir));
#endif
