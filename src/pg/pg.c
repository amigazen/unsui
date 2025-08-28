/* pg
   Source for Manx Aztec C
   Freeware, (C) Thomas Radtke 1996
*/

#include <stdio.h>
#include <sgtty.h>

/* we try to determine the 'visible' stringsize */

int strlen_(char *ptr)
{
	int i=0,z=0;
	unsigned char c;

	while ((c=ptr[i++])!='\0') {
		if (c<32) {
			switch(c) {
				case '\t': /* tab */
					z+=7;
					break;
				case 8: /* backspace */
					z-=2;
					break;
				default: /* unknown, could be terribly wrong :( */
					z-=1;
					break;
			}
		}
	}
	return i+z-1;
}

void rawmode(struct FileHandle *fh)
{
	struct sgttyb stty;

	ioctl(fh,TIOCGETP,&stty);
	stty.sg_flags|=RAW;
	ioctl(fh,TIOCSETP,&stty);
}

void conmode(struct FileHandle *fh)
{
	struct sgttyb stty;

	ioctl(fh,TIOCGETP,&stty);
	stty.sg_flags&=~RAW;
	ioctl(fh,TIOCSETP,&stty);
}

int forw(int old_pos,char *buffer)
{
	while (buffer[old_pos++]!=';');
	return old_pos;
}

main(int argc, char **argv)
{
	FILE *fp_read,*fp_write,*std_in;
	char *buffer;
	char c,x[5],y[5];
	int i,idx,pos,row,col,len;

	if (!(buffer=(char *)malloc(1024))) exit(20);
	fp_read=fopen("*","r");
	fp_write=fopen("*","w");
	len=strlen(argv[argc-1]);
	if (!strcmp(&argv[argc-1][len-2],"pg") || !(std_in=fopen(argv[argc-1],"r"))) std_in=stdin;
	rawmode(fileno(fp_write));
	fprintf(fp_write,"%c0 q",0x9b);
	fscanf(fp_read,"%[^r]",buffer);
	getc(fp_read);
	conmode(fileno(fp_write));
	pos=forw(1,buffer);
	pos=forw(pos,buffer);
	sscanf(&buffer[pos],"%[^;]",y);
	pos=forw(pos,buffer);
	sscanf(&buffer[pos],"%[^;]",x);
	row=atoi(y);
	col=atoi(x);
	i=0;
	idx=0;
	for(;;) {
		while (i<row-1) {
			if (!idx) {
				if (!fgets(buffer,1024,std_in)) exit(0);
				i+=(1+strlen_(buffer)/col);
			}
			else idx=0;
			if (i>=row-1) idx=1;
			else printf("%s",buffer);
		}
		i=0;
		printf("[More]");
		fflush(fp_write);
		rawmode(fileno(fp_write));
		if (getc(fp_read)=='q') {
			printf("%c6D      %c6D",0x9b,0x9b);
			conmode(fileno(fp_write));
			while (fgets(buffer,1024,std_in)) printf("%s",buffer);
			exit(0);
		}
		printf("%c6D      %c6D",0x9b,0x9b);
		conmode(fileno(fp_write));
	}
}
