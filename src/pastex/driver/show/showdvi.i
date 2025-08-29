/* Prototypes for functions defined in showdvi.c */
extern void main	Args((int argc,
	       		      char **argv));
extern void OpenNewDVI	Args((char *filename, int theSame));
extern int page_counter	Args((int i,
           	              int m));
extern char * DecodeArgs(int argc, char *argv[]);


