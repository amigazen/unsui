void InitHelpFirst(void);
void FreeHelpLast(void);
long GetHelpNum(void);
void StartHelp(struct Screen * scr, struct Window * win);
void StopHelp(void);
long work_with_help(void);
long work_with_gadgethelp(struct IntuiMessage * msg);
void do_menu_help(long sel, int checked, int enabeled, int is_barlabel);
void HelpKeyAGuide(int conthelp);
