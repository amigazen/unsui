/* Prototypes for functions defined in am_requ.c */

extern void CenterWindow	Args((WORD *LeftEdge,
				      WORD *TopEdge,
				      WORD Width,
				      WORD Height));

extern void  AboutWinUp		Args((void));	/* die Signal number wird global in sig_aboutwin gespeichert */
extern long  AboutWinMsg	Args((void));	/* work with Message */
extern void  AboutWinDown	Args((void));	/* closes AboutWin */

#if !defined(REQ_LIBRARY)
extern void set_0_active	Args((void));
extern void set_1_active	Args((void));
extern void set_col_0		Args((void));
extern void set_col_1		Args((void));
extern void refresh_props	Args((void));
extern void remove_col_request	Args((void));
#endif

extern void show_col_request	Args((void));
extern void FatalMessage	Args((int ret, char *msg));
extern long real_prog_end	Args((void));

/* wird in gadget.c gebraucht */
extern struct PropInfo cprop_r_info;
extern struct PropInfo cprop_g_info;
extern struct PropInfo cprop_b_info;

extern void __stdargs ok(char *format, ...);
extern int  __stdargs MySimpleRequest(int NrBut,
			       char *Lbut, char *Rbut, char *Mbut,
			       char *title, char *format,...);

extern void InitFileReq(void);
extern long LoadFileReq(void);
extern void FreeFileReq(void);

extern int  MyGetString		(char *title, char *result);
extern int  MyGetLong		(char *title, long min, long max, long defval, long *result);
extern void Okay1		(char *title);
extern int  Okay2		(char *title);

extern void ChangeFormatFile	(void);

extern int  start_command	Args((char *title, char *name, char *PubScr));


extern int GetScreenMode(ULONG * DisplayID, struct Window * w);	// ret == TRUE|FALSE


extern int StartSearch(enum SearchOpt opt);
