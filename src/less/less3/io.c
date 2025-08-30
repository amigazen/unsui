/*  io.c */

#include <ctype.h>

/*
 * Name:	MicroEMACS
 *		AmigaDOS terminal I/O
 * Version:	31
 * Compiler:	Manx Aztec C
 * Created:	19-Apr-86 ...!ihnp4!seismo!ut-sally!ut-ngp!mic
 */
#include	<libraries/dos.h>
#include	<libraries/dosextens.h>
#undef TRUE
#undef FALSE

#define	NIBUF	128		/* Probably excessive.		*/
#define	NOBUF	512		/* Not too big for 750/730.	*/

struct	FileHandle	*tty;
struct	FileHandle	*Open();
char	obuf[NOBUF];	/* Output buffer		*/
int	nobuf;				/* # of bytes in above		*/
char	ibuf[NIBUF];	/* Input buffer			*/
int	nibuf;				/* # of bytes in above		*/
int	nrow = 0;				/* Terminal size, rows.		*/
int	ncol;				/* Terminal size, columns.	*/


extern char version[];

/*
 * This routine gets called once, to set up the
 * terminal channel.
 */
ttopen()
{
	char WindowName[80];

	if(nrow) return;
	
	nrow = 23;
	ncol = 77;
	nobuf = nibuf = 0;

	strcpy(WindowName,"RAW:0/0/640/200/");
	strcat(WindowName, version);
	tty = Open(WindowName, MODE_NEWFILE);
	if (tty == (struct FileHandle *) 0) {
		printf("Can't open window!\n");
		exit(200);
	}
}

_abort()
{
	ttclose();
	exit(0);   
}


/*
 * This function gets called just
 * before we go back home to the command interpreter.
 * On the Amiga it closes up the virtual terminal window.
 */
ttclose()
{
	if (tty != (struct FileHandle *) 0L) {
		ttflush();
		Close(tty);
	}
	tty = /*(struct FileHandle *)*/ NULL;
}

/*
 * Write a character to the display.
 * On the Amiga, terminal output is buffered, and
 * we just put the characters in the big array,
 * after cheching for overflow.
 */
ttputc(c)
{
	if (nobuf >= NOBUF)
		ttflush();
	obuf[nobuf++] = c;
}

/*
 * This function does the real work of
 * flushing out buffered I/O on the Amiga. All
 * we do is blast out the block with a write call.
 */
ttflush()
{
	if (nobuf > 0) {
		Write(tty, obuf, (long) nobuf);
		nobuf = 0;
	}
}

/*
 * Read a character from the terminal,
 * performing no editing and doing conditional echo 
 */
int do_echo = 1; /* echo flag */

ttgetc()
{
	unsigned char c, ignore;	/* must be unsigned! */

	ttflush();

	Read(tty,&c,1L);
	if (c == '\x9b') {
		Read(tty, &c, 1L);

		/* was it a function key  */
		if (isdigit(c) || c == '?')
			Read(tty, &ignore, 1L);

		/* return the char with top bit set */
		c |= 0x80;
	} else 
		if (do_echo) 
			ttputc(c);

	return ((int) c);
}


/*
 * Write a string to the terminal 
 */
ttputs(s)
char *s;
{
	while(*s) ttputc(*s++);
	ttflush();
}

/* fake termcap output */
tputs(s, ignore_heigth, ignore_func)
char *s;
int ignore_heigth, ignore_func;
{
	if(nrow == 0)
		ttopen();

	flush();  
	while(*s) ttputc(*s++);
	ttflush();
}

