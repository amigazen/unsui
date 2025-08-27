/* Prototypes for functions defined in am_menu.c */
extern void SetDosMenu			Args((void));
extern void ClearDosMenu		Args((void));
extern void free_menu			Args((void));
extern void init_menu			Args((void));
extern long show_menu			Args((UWORD Code, WORD MouseX, WORD MouseY));

extern struct MenuItem *MyItemAddress	Args((unsigned long menuNumber));

extern void free_os_menu		Args((void));
extern void init_os_menu		Args((void));
extern void set_checked_os_menu		Args((void));
extern long work_with_os_menu		Args((UBYTE MenuNum, UBYTE ItemNum, UBYTE SubNum));
extern void work_with_os_help_menu	Args((UBYTE MenuNum, UBYTE ItemNum, UBYTE SubNum));

