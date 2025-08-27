/* Prototypes for functions defined in dviprint.c */
extern void main		Args((int argc,
	        	 	      char **argv));

#ifdef AMIGA
extern void OpenARP		Args((void));
extern void CloseARP		Args((void));
#endif
