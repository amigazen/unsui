/* Prototypes for functions defined in dvihand.c */

#ifndef __DVIHAND_I_
#define __DVIHAND_I_

#ifdef AMIGA
#define MODE_READ	0
#define MODE_WRITE	1
#define MODE_APPEND	2
extern FILE *OpenConfigFile	Args((char *name, int modus));
#endif

extern DVIFILE * OpenDVI(char * name, int toRam);
extern void 	 CloseDVI(DVIFILE * dvifp);
extern void	 TempCloseDVI(DVIFILE * dvifp);
extern void	 TempOpenDVI(DVIFILE ** dvifp);
extern long	 DVIftell(DVIFILE * dvifp);
extern int	 DVIfseekSet(DVIFILE * dvifp, long pos);
extern void	 DVIrewind(DVIFILE * dvifp);
extern void	 DVIseekEnd(DVIFILE * dvifp);
extern int	 DVIfread(char * buf, int bsize, int nr, DVIFILE * dvifp);
extern int	 DVIfeof(DVIFILE * dvifp);



extern long scalewidth		Args((long a, long b));
extern double ActualFactor	Args((long unmodsize));
extern void AllDone		Args((void));
extern long DoConv		Args((long num,
	            		      long den,
        	    		      int convResolution));
/** void DoSpecial		Args((char *str, long n)); **/
extern int  FindPostAmblePtr	Args((long *postambleptr));
extern void GetBytes		Args((DVIFILE *fp,
	             		      char *cp,
        	     		      long n));
extern void GetFontDef		Args((int load));

extern void MoveDown		Args((long p));
extern void MoveOver		Args((long p));
// extern void setmotion	Args((void));

extern long OldNoSignExtend	Args((DVIFILE *fp,
	                  	      int n));
extern long NoSignExtend	Args((DVIFILE *fp,
	                 	      int n));
extern void ReadFontDef	Args((long k,
	                 	      int load));
extern int ReadPostAmble	Args((int load));
extern long SignExtend		Args((DVIFILE *fp,
	                	      int n));
extern long SRead1Byte		Args((DVIFILE *fp));
extern long SRead2Byte		Args((DVIFILE *fp));
extern long SRead3Byte		Args((DVIFILE *fp));
extern long SRead4Byte		Args((DVIFILE *fp));
extern void SkipFontDef		Args((void));
#ifdef SLOW
extern void SetChar		Args((long c,
				      int command));
extern void SetRule		Args((long a,
					  long b,
					  BOOLEAN Set));
#endif




#endif
