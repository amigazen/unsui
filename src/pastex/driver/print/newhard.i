/* Prototypes for functions defined in newhard.c */

extern void FormFeed	Args((void));
extern void getout	Args((char c));
extern void string_out	Args((char *string, int length, int opt_argument));
extern void Hardcopy	Args((struct bitmap *bmap, long lineno, int first_pass, int last_pass));

extern void initialize_canon_printer(void);	/* called from PrepareHardcopy() */
extern void end_canon_printer(void);		/* called from EndHardcopy() */

extern char *PrnBuffer;
