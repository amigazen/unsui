/*** crypt.h ***/


/***	Codierfunktion:	b = a + CR_OFFSET - FACT*index(a)		***/
/***									***/
/***	Decodierfunkt.: a = b + FACT*index(b) - CR_OFFSET		***/
/***									***/
/***	Achtung:	Stringlaenge auf (127+CR_OFFSET)/FACT begrenzt!	***/



#ifdef CRYPT
#  define CR_OFFSET	(int)113
#  define FACT		(int)5
#  define ENCODE(strin,strout)	{register int i,j=strlen(strin); \
				strout[j]='\0';for(i=0;i<strlen(strin);i++){ \
		j--;strout[j]=(char)((int)strin[i]+FACT*j-CR_OFFSET);}}
#  define DECODE(strin,strout)	{register int i,j=strlen(strin); \
				strout[j]='\0';for(i=0;i<strlen(strin);i++){ \
		j--;strout[j]=(char)((int)strin[i]-FACT*i+CR_OFFSET);}}
#else
#  define CR_OFFSET	(int)0
#  define FACT		(int)1
#  define ENCODE(strin,strout)	strcpy(strout,strin)
#  define DECODE(strin,strout)	strcpy(strout,strin)
#endif

#define ENCODE2(strin,strout)		{register int i, len=(strin[0]/3)-3; \
					for(i=0;i<len;i++) strout[i]=(strin[i+1]-573)/3;}
#define DECODE2(strin,strout,len)	{register int i; \
					for(i=0; i<len; i++) strout[i]=strin[i]*3+573;}
