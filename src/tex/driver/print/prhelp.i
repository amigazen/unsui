#ifndef DISPLAY

extern struct printer_para *Printer;

#ifdef OLD
extern void ShowPrinters			Args((int longlist));
#endif
extern struct printer_para *SetupPrinterPara	Args((char *name, int draft, int secure));
extern void PrepareHardcopies			Args((void));
extern void EndHardcopies			Args((void));

extern int no_printer_given;	/* no printer explicitedly defined 	*/
extern long
	pp_dots_per_space,	/* as name says				*/
	pp_dots_per_point,	/* as name says				*/
	pp_max_dots_line;	/* depends upon resolution & paper_width*/
extern char *default_printer_name;

#endif
