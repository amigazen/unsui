/**************************************************************************/
/***		decode.c				06.06.89 (hes)	***/
/**************************************************************************/

#include "defines.h"
#include <stdio.h>

#include "globals.h"

#ifdef ANSI
#  include <string.h>
#  include <stdlib.h>
#endif


#include "decode.i"

#ifdef AMIGA
#  ifdef DISPLAY
#    include "sglobals.i"
#  else
#    include "globals.i"
#  endif
#endif


#include "version.h"	/* load versions */

#include "crypt.h"	/* load crypt Funktions */




FILE *f;

void decode(char *strin, char *strout)
{
  DECODE(strin,strout);
}

void decode2(char *strin, short *strout, int len)
{
  DECODE2(strin,strout,len);
}


void encode(char *strin, char *strout)
{
  ENCODE(strin,strout);
}


void encode2(short *strin, char *strout)
{
  ENCODE2(strin,strout);
}



void print_code(char *str)
{
#ifdef CRYPT
  int i;
  long k=0;
  char *kk;

  kk = (char *)(((char *)(&k))+3);
  fprintf(f,"{");
  for (i=0;i<strlen(str);i++) {
    *kk=str[i];
    fprintf(f," %ld,",k);
  }
  fprintf(f," 0};\n");
#else
  fprintf(f,"\"%s\";\n",str);
#endif
}

void print_code2(short *str, int len)
{
  int i;
  
  fprintf(f,"{ %d, ", (len+3)*3);
  for (i=0; i<len; i++) {
    fprintf(f, " %d,", str[i]);
    if (i % 20 == 0 && i>0) fprintf(f, "\n\t ");
  }
  fprintf(f," %d};\n", 1736);
}


void print_encode(str)
  char *str;
{
  char strout[80];

  encode(str, strout);
  fprintf(f,"  \"%s\"\n",strout);
}

void main(int argc,char **argv)
{
  char strout[200];
  char file_name[80];
#if defined(BETACOPYRIGHT)
  short strout2[100];
#endif

  if (argc != 2) {
    fprintf(stderr,"Usage: %s outfile\n",argv[0]);
    exit (10);
  }
 
  (void)strcpy(file_name,argv[1]);

  fprintf(stdout,"decode: Creating file %s.\n",file_name);

  f = fopen(file_name,"w");
  if (f == NULL) {
    fprintf(stderr,"decode: Can't open file!\n");
    exit (15);
  }

  fprintf(f,"/************************************************************/\n");
  fprintf(f,"/*******************  %s  *********************/\n",file_name);
  fprintf(f,"/************************************************************/\n");
  fprintf(f,"/**** This file is automaticaly generated from 'decode'! ****/\n");
  fprintf(f,"/**** Based on the files 'version.h' and 'crypt.h'.      ****/\n");
  fprintf(f,"/************************************************************/\n\n");


#ifdef CRYPT
  fprintf(f,"#define CRYPT\t\t/* encoding enabled */\n\n");
#else
  fprintf(f,"#undef CRYPT\t\t/* encoding disabeld */\n\n");
#endif


  decode(VERSION_FORMAT_STRING,strout); 
  fprintf(f,"unsigned char VERSION_FORMAT_STRING[] =\n\t");
  print_code(strout);

  fprintf(f,"int COMPILER =\t %d;\n",(int)COMPILER+CR_OFFSET);

  decode(DVIPRINT_VERSION,strout); 
  fprintf(f,"\nunsigned char DVIPRINT_VERSION[] =\n\t");
  print_code(strout);

  decode(SHOWDVI_VERSION,strout);
  fprintf(f,"unsigned char SHOWDVI_VERSION[] =\n\t");
  print_code(strout);


  fprintf(f,"\n#ifdef DISPLAY\n");
  fprintf(f,"#  define VERSION\tSHOWDVI_VERSION\n");
  decode(PROGRAMTITLE_SHOW,strout);
  fprintf(f,"unsigned char PROGRAMTITLE[] =\n\t");
  print_code(strout);

  fprintf(f,"#else\t/* DISPLAY */\n");
  fprintf(f,"#    define VERSION\tDVIPRINT_VERSION\n");
  decode(PROGRAMTITLE_PRINT,strout);
  fprintf(f,"unsigned char PROGRAMTITLE[] =\n\t");
  print_code(strout);
  fprintf(f,"#endif\t/* DISPLAY */\n\n");

  decode(PROGRAMKENNUNG,strout);
  fprintf(f,"unsigned char PROGRAMKENNUNG[] =\n\t");
  print_code(strout);

  decode(AUTHOR_S_FORMAT,strout);
  fprintf(f,"unsigned char AUTHOR_S_FORMAT[] =\n\t");
  print_code(strout);
  decode(AUTHOR_D_FORMAT,strout);
  fprintf(f,"unsigned char AUTHOR_D_FORMAT[] =\n\t");
  print_code(strout);
  decode(AUTHOR_TITLE,strout);
  fprintf(f,"unsigned char AUTHOR_TITLE[] =\n\t");
  print_code(strout);
  decode(AUTHOR1,strout);
  fprintf(f,"unsigned char AUTHOR1[] =\n\t");
  print_code(strout);
  //decode(AUTHOR2,strout);
  //fprintf(f,"unsigned char AUTHOR2[] =\n\t");
  //print_code(strout);
  decode(AUTHOR3,strout);
  fprintf(f,"unsigned char AUTHOR3[] =\n\t");
  print_code(strout);
  fprintf(f,"\n");

#if defined(BETACOPYRIGHT)
  fprintf(f, "#if defined(BETACOPYRIGHT)\n");
  fprintf(f, "unsigned short COPYRIGHT[] =\n\t");
  decode2(COPYRIGHT, strout2, strlen(COPYRIGHT));
  print_code2(strout2, strlen(COPYRIGHT));
  fprintf(f,"\n");
  fprintf(f,"#endif\n");
#endif

  exit (0);
}

