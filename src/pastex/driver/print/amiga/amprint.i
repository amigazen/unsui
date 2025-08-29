/* Prototypes for functions defined in amprint.c */
extern long bufferz;
extern long bufflen;
extern char *buffer;

#ifdef IO_TEST
extern void prn_string		Args((char *str, int len));
#endif

#if 0
#define prnout(c)	{ buffer[bufferz++] = (char)c;\
			  if (bufferz==bufflen) { make_room(); }}

#ifdef JCH
#define prnout(c)	if (buffer[bufferz++] = (char)c,\
			    bufferz==bufflen) make_room(); else /* ; follows */
#endif
#endif /* 0 */

extern void make_room		Args((void));
extern void prnflush		Args((void));
extern void WaitForPrinterOK	Args((void));

extern void check_background	Args((void));

