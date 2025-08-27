/*	amprint.h  --- defines, globale Var. u. Funkt.Def. */

#ifdef AMIGA

#if 0
#define MIN_STACK_SIZE	10000		/* minimal stack size */
#endif


/* *** Groesse des internen Puffers von prnout und anderes...	*** */
#define BUFFLEN 	10240L
#define BUF_VOLL 	0
#define BUF_LEER 	1
#define BUF_A 		0
#define BUF_B 		1



union printerIO {
	struct IOStdReq		iostd;
	struct IODRPReq		iodrp;
	struct IOPrtCmdReq	iocmd;
	};

/* ***	Globale Variablen	*** */
extern union printerIO *request;
extern struct MsgPort *printerPort;
extern long bufferz;
extern long bufflen;
extern char *bufferA, *bufferB;
extern int  bufferZu_A, bufferZu_B;
extern char *buffer;
extern int  aktBufNr;
extern long error;

extern int  is_printer_started;		/* 0: nein, 1: init ok, 2: print gestartet */

extern int  printer_group;		/* Wieviele Nadeln / 8			*/
extern char *PrnBuffer;			/* Pointer to local buffer		*/

/* printer_group wird in dviprint.c gesetzt (sofort nach DecodeArgs)	*/
/* PrnBuffer wird in InitPrinter allociert und bei ClosePrinter wieder	*/
/* freigegeben.								*/

#endif
