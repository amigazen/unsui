/*	standard input output for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 		*/

#ifndef	_STDIO_H
#define _STDIO_H

#include    <sys/types.h>
#include    <sys/file.h>

/* FILE structure flags */

#define _IOREAD 	0x0001		/* file may be read from */
#define _IOWRT		0x0002		/* file may be written to */
#define _IORW		0x0003		/* file may be read from and write to */
#define _IOLAST		0x0004		/* last io was read:0 or write:1 */

#define _IOLBF		0x0008		/* i/o is line buffered */
#define _IOMYBUF	0x0020		/* i/o is buffered */
#define _IOFBF		_IOMYBUF	/* this is an alias */

#define _IOEOF		0x0040		/* EOF has been reached */
#define _IOERR		0x0080		/* an error has occured */

#define _IOSTRG 	0x0100		/* this is a dummy file from sscanf */
#define _IOSMALLBUF	0x0200		/* i/o has small buffer */
#define _IONBF          _IOSMALLBUF     /* i/o is not buffered */

typedef struct					/* FILE structure for buffered I/O */
	{
	unsigned char	*_ptr;		/* current buffer pointer */
	unsigned char	*_base; 	/* base of file buffer */
	unsigned int	_flag;		/* file status flags */
	int				_cnt;		/* # of bytes in buffer */
	int				_file;		/* file handle */
	int				_bufsiz;	/* buffer size */
	unsigned char   *_name;         /* if not NULL delete file */
	unsigned char	_ch;		/* tiny buffer, for "unbuffered" i/o */
	}
	FILE;

/* standard defines */

#define EOF		-1		/* end of file indicator */
#define BUFSIZ		1024		/* default buffer size */
#define BUFSIZ_2	128		/* low memory default buffer size */
#define EOS		0		/* end-of-string indicator */
#define EXIT_FAILURE	20		/* failure return value for exit() */
#define EXIT_SUCCESS	0		/* success return value for exit() */
#define	FILENAME_MAX	30		/* actually max 30 chars on the amiga */
#define FOPEN_MAX	50		/* this is the max level 1 file number */

#ifndef ERROR
#define ERROR		-1		/* general error condition */
#endif

#define	L_tmpnam	24		/* length of tmpname generated string */
#define TMP_MAX		99999		/* max number of temporary file names */

/* lseek() origins */

#define SEEK_SET	0		/* from beginning of file */
#define SEEK_CUR	1		/* from current location */
#define SEEK_END	2		/* from end of file */

/* stdio functions prototypes */

#ifdef	__cplusplus
extern "C" {
#endif

extern	FILE	*stdin,*stdout,*stderr;
extern	void	_exit();
extern	FILE	*tmpfile(void);
extern	FILE	*fopen(char *,char *),*fdopen(int,char *),*freopen(char *,char *,FILE *);
extern	char	*gets(char *),*fgets(char *,int,FILE *);
extern	int	_filbuf(FILE *),_flsbuf(int,FILE *);
extern	int	getw(FILE *),puts(char *),putw(int,FILE *);
extern	int	rename(char *,char *),setbuffer(FILE *,char *,int);
extern	int	setlinebuf(FILE *),strout(int,char *,int,FILE *,int);
extern	int	ungetc(int,FILE *),fputs(const char *,FILE *);
extern	int	fread(void *,size_t,size_t,FILE *),fseek(FILE *,long,int);
extern	int	fwrite(void *,size_t,size_t,FILE *),fclose(FILE *),fflush(FILE *);
extern	void	perror(const char *),rewind(FILE *),setbuf(FILE *,char *);
extern	long	ftell(FILE *);
extern	char	*tmpnam();

extern	long	scanf(const char *,...),fscanf(FILE *,const char *,...),sscanf(const char *,const char *,...);
extern	int		sprintf(char *,const char *,...),printf(const char *,...),fprintf(FILE *,const char *,...);
extern	int		vfprintf(FILE *,const char *,char *);
extern	int		vsprintf(char *,const char *,char *);

#ifdef	__cplusplus
}
#endif

/* stream macros */

#define clearerr(iop)	((void) ((iop)->_flag &= ~(_IOERR|_IOEOF)))
#define feof(iop)	((iop)->_flag & _IOEOF)
#define ferror(iop)	((iop)->_flag & _IOERR)
#define fileno(iop)	((iop)->_file)
#define fexists		access(f,0x00)
#define exists(f)	access(f,0x00)
#define fungetc		ungetc
#define ungetchar(c)	ungetc((c),stdin)
#define getc(iop)	fgetc(iop)
#define getchar()	fgetc(stdin)
#define	fgetc(iop)	((!((iop)->_cnt) || ((iop)->_flag & _IOLAST)) ? _filbuf(iop) : (((iop)->_cnt)-- , *(((iop)->_ptr)++) & 0xff))
#define putchar(c)	fputc((c),stdout)
#define putc		fputc
#define fputc(c,iop)	((!(iop->_cnt) || !(iop->_flag & _IOLAST)) ? _flsbuf(c,iop) : (*((iop->_ptr)++) = c, (iop->_cnt)-- , 1))
#define fgetpos(iop,p)	((*p=ftell(iop))==-1 ? 1 : 0)
#define	fsetpos(iop,p)	(fseek(iop,SEEK_SET,*p))

#endif	/* _STDIO_H */
