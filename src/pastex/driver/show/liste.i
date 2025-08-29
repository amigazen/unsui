/* Prototypes for functions defined in liste.c */
extern int  init_liste		Args((void));
extern void free_liste		Args((void));
extern void in_list		Args((long pnr,
                        	      long pptr,
				      long phy_nr));
extern long take_pptr		Args((long pnr_log,
				      long pptr,
				      long phy_nr,
				      short is_phy));
extern long get_page_number	Args((long nr));
extern long get_log_page_number Args((long phy));
extern long get_phy_number	Args((long cpagep));
extern long get_secundary	Args((long phy));
#if 0
extern int take_next		Args((long *pnr,
                        	      long *pptr));
extern int take_prev		Args((long *pnr,
                	              long *pptr));
#endif
extern void take_first		Args((long *pnr,
        	                      long *pptr));
extern void take_last		Args((long *pnr,
	                              long *pptr));
extern void take_less		Args((long apnr,
                        	      long apptr,
                	              long *pnr,
       		                      long *pptr));
extern void take_greater	Args((long apnr,
	        	              long apptr,
	                	      long *pnr,
		                      long *pptr));
extern void calc_phy_nr	Args((long max_pages));
extern void print_list		Args((void));


