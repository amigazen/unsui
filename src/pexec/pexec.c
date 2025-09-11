#include <libraries/dosextens.h>
#include <stdio.h>
#include <functions.h>

char rpipefile[] = "pipe:Z";
char wpipefile[] = "pipe:W";
char rcommand[] = "run cat >pipe:Z <NIL: s:startup-sequence";
char wcommand[] = "run cat >ram:pipez1 <pipe:W";

main()
{
    char *cp1, *cp2;
    char inbuf[1024];
    char backbuf[1024];
    struct FileHandle *exin, *exout;
    FILE *fp, *fp2;
    int exrtn;

/* READ */

    exin = (struct FileHandle *)Open("NIL:", MODE_OLDFILE);
/*    exout = (struct FileHandle *)Open(rpipefile, MODE_NEWFILE); */
    exrtn = Execute(rcommand, exin, exin);
/*    Close((BPTR)exout); */
    Close((BPTR)exin);
    if (!exrtn) {
	printf ("Execute failed.");
	exit (1);
    }
    fp = fopen(rpipefile, "r");
    while (fgets(inbuf, sizeof(inbuf), fp)) {
	cp2 = &backbuf[strlen(inbuf) - 1];	/* Skip \n\0 */
	*cp2-- = '\0';
	cp1 = inbuf;
	while (*cp1) *cp2-- = *cp1++;
	puts (backbuf);
    }
    fclose (fp);

/* WRITE */

    exin = (struct FileHandle *)Open("NIL:", MODE_OLDFILE);
/*    exout = (struct FileHandle *)Open(wpipefile, MODE_NEWFILE); */
    exrtn = Execute(wcommand, exin, exin);
/*    Close((BPTR)exout); */
    Close((BPTR)exin);
    if (!exrtn) {
	printf ("Execute failed.");
	exit (1);
    }
    fp = fopen("s:startup-sequence", "r");
    fp2 = fopen(wpipefile, "w");
    while (fgets(inbuf, sizeof(inbuf), fp)) {
	cp2 = &backbuf[strlen(inbuf)];	/* Insert \n\0 */
	*cp2-- = '\0';
	*cp2-- = '\n';
	cp1 = inbuf;
	while (*cp1) *cp2-- = *cp1++;
	fputs (backbuf, fp2);
    }
    fclose (fp2);
    fclose (fp);

    exit(0);
}
