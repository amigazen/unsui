/* external functions from new_font.c */

extern void release_mem		Args((void));

extern void setup_ctfmw		Args((struct Font *font));
extern void init_fontmt		Args((char show));
extern void resetfont		Args((void));
extern void SetFntNum		Args((long fntnum));
extern int  HasBeenRead		Args((long fntnum));
extern void LoadFont		Args((int  action, 		/* 0: define only	*/
				      long fntnum,		/* 1: load only		*/
				      char *fntname,		/* 2: define & load	*/
				      long chksum,
				      long d,
				      long s,
				      long fontmag5,
				      long fontmag));
extern void Load_really		Args((struct Font *fnt));
extern long alloc_char		Args((unsigned long size));	/* ret = -1 -> no memory */

extern void write_font_def_file Args((void));
extern void get_full_name	Args((char *str,
				      struct Font *fnt));
extern void mk_correct_path	Args((char *path));

