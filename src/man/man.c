
/* man.c
 *
 * Man pages on amiga?
 *
 * by Mark Papadakis, markp@palamida.math.uch.gr
 * http://palamida.math.uch.gr/markp
 *
 * -- changes --
 * 10.06.07 : New options, read MANPATH env variable now
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dos/dos.h>
#include <proto/dos.h>
#include <exec/types.h>

#define VERSION "1.2"
const UBYTE Version[] = "$VER: man "VERSION" by MarkP (10.06.97)\n\n";
int main(int argc, char *argv[])
{   
    FILE *envFile = NULL;
    char tbuf[256];
    char ndir[20][128];
    int ndirs = 0;
    char full[256];
    char command[512];
    BPTR tlock = NULL;
    int i;
    int found = 0;
    int search_mode = 0;
    if(argc<2)
    {
    fprintf(stderr,"Syntax : Man [-k] <function>\n");
    exit(1L);
    }
    if(argv[1][0]=='?')
    {
    printf("man version "VERSION" by MarkP(Mark Papadakis).\n");
    printf("Reads man pages. Can also look for keywords using the -k option.\n");
    printf("Read the docs for further info.\n");
    exit(0);
    }

    ndirs = 0;
    envFile = fopen("ENV:MANPATH","r");
    if(!envFile)
    {
    ndirs = 1;
    strcpy(&ndir[1][0],"ManDir:");
    }
    else
    {
    while(fgets(tbuf,254, envFile))
    {
        if(tbuf[0]>32 && tbuf[0]!='#')
        {
        int l;
        l = strlen(tbuf);
        if(tbuf[l-1]=='\n')
            tbuf[l-1]='\0';
        ndirs++;
        l = strlen(tbuf);
        if(tbuf[l-1]!='/' && tbuf[l-1]!=':')
              strcat(tbuf,"/");
        strcpy(&ndir[ndirs][0], tbuf);
         }
     }
     fclose(envFile);
    }


    // search directories
    for(i = 1; i<=ndirs;i++)
    {


    if(!(strcmp(argv[1],"-k")))
    {
    // man -k function
    if(argc<=2)
    {
        printf("Error : syntax man -k what?\nwhat not defined.\n");
        exit(1L);
    }
    sprintf(full,"Search %s %s QUIET", &ndir[i][0], argv[2]);
    system(full);
    search_mode = 1;
    }
    else
    {
    // man function
    sprintf(full,"%s%s",&ndir[i][0],argv[1]);
    tlock = Lock(full,ACCESS_READ);
    if(!tlock)
          strcat(full,"A");
    else
          UnLock(tlock);

       tlock = Lock(full,ACCESS_READ);
       if(tlock)
       {
       UnLock(tlock);
       found = 1;
       sprintf(command,"more %s",full);
       system(command);
       exit(0);
       }
    }

    }

    if(!found && !search_mode)
       printf("'%s' not found.\n",argv[1]);
}
    

    
