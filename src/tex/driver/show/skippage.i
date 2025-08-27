/* Prototypes for functions defined in skippage.c */
extern int skippage	Args((DVIFILE *fp,
			      long *cpagep,
			      long *ppagep,
			      long *current_page,
			      long *current_page_phy));
