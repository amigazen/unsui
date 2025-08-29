/* app_win.i */

extern BOOL setup_app_icon	Args((UBYTE *app_icon_sig));
extern void clear_app_icon	Args((void));
extern long work_with_app_icon	Args((void));
extern long CheckAppArgName	Args((char * name, BPTR lock));
extern BOOL save_app_icon_pos	Args((void));
