/* Prototypes for functions defined in amscreen.c */
extern void FreeChipPuffRP		Args((void));
extern void AllocChipPuffRP		Args((void));
extern ULONG get_DisplayID		Args((void));
extern char * GetModeIDName		Args((ULONG, long *));
extern int  get_default_screen_dimension Args((long *width, long *height, short *lace));
extern void ChangeScreenSize		Args((short new_width, short new_height));
extern void CloseOpenScreen		Args((int change_lace, int change_color, int change_size));
extern int  is_newer_dvi_file		Args((void));
extern void ToggleAutoAgain		Args((void));
extern void init_screen_colors		Args((void));
extern void set_screen_colors		Args((void));
extern void set_temp_screen_colors	Args((void));
extern void block_win2                  (void);
extern void unblock_win2                (void);
extern void sleep_pointer		Args((void));
extern void clear_pointer		Args((void));
extern void SetupMargin			Args((void));
extern void MyModifyIDCMP		Args((ULONG add, ULONG sub));
extern void AddTimeRequest		Args((void));
extern void abort_print_page		Args((void));
extern void printing			Args((void));
extern void fill_block			Args((short x,
       					      short y,
       					      short dx,
   					      short dy,
					      short color));
extern void refresh_screen		Args((void));
extern void write_status		Args((void));
extern int  write_screen		Args((char *str));
extern void write_screen_counter	Args((int nr));
extern void window_move			Args((const int dx,
      				              const int dy));
extern void window_show			Args((void));
extern void window_plus_sbar_move	Args((int dx,
	                   		      int dy));
extern void window_set_x		Args((int x));
extern void window_set_y		Args((int y));
extern void ToggleColorDepth		Args((void));
extern void change_resolution		Args((void));
extern void toggle_scrollbar		Args((int no_refresh));
extern void DrawDottedBorder		Args((int toggle, int no_reresh));
extern void display_full_page		Args((void));
extern void show_full_page		Args((int no_refresh));
extern void getdir			Args((char *file,
      					      char *dir));
extern int  is_dir			Args((char *file));
extern void SavePageIFF			Args((char * name));
extern void set_Gadgets_to_fname	Args((void));
extern void set_int_gad			Args((void));
extern void clear_bild			Args((void));
extern int  init_task_name		Args((int argc,
					      char **argv));
extern char *GetCurrentPubScr		Args((void));
extern void Init_ShowDVI		Args((void));
extern void Open_ShowDVI		Args((int isDVIfile));
extern void SetShowDVIVars		Args((char *name));
extern void close_all_bild		Args((void));
extern long *Init_all			Args((int pix_length,
	         			      int map_width));
extern long ShowPage			Args((int same_page));
extern void beep			Args((void));
extern void make_old_active		Args((void));
extern void make_show_active		Args((void));
extern int  can_i_exit			Args((void));

