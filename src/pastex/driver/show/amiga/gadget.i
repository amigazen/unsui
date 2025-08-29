/* Prototypes for functions defined in gadget.c */

extern UWORD VFindScrollerTop	Args((void));
extern UWORD HFindScrollerTop	Args((void));
extern void  VSetScrollerValues	Args((void));
extern void  HSetScrollerValues	Args((void));

extern void  init_images		Args((void));
extern void  SetArrowsOn		Args((void));
extern void  SetArrowsOff		Args((void));
extern void  ModifyXPot			Args((void));
extern void  ModifyYPot			Args((void));
extern void  CheckUpDownBorderGad	Args((void));
extern void  init_gad			Args((void));
extern void  free_images			Args((void));
extern long  handle_file_gad		Args((void));
extern long  check_gad			Args((struct Gadget *m_gad));
extern long  down_gad			Args((long gadid, struct Gadget *m_gad,
					      ULONG time_secs, ULONG time_mics));
extern void  follow_pot_gad		Args((long gadid));
/* extern long follow_gad		Args((struct IntuiMessage *msg)); */
extern void  Add_system_gadgets		Args((void));
extern void  Add_scroll_gadgets		Args((int no_refresh));
extern void  Remove_scroll_gadgets	Args((int no_refresh));
extern void  Remove_all_gadgets		Args((void));

extern void DelPgGad			Args((void));
extern void Set_PgIntGad		Args((long pg));
extern void Set_PgPotGadRange		Args((void));
extern void Set_PgGadPage		Args((long phy));
extern void Set_PgGadPageCur		Args((void));


