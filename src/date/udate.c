/*-------------------------------------------------------------------------*/
/*      program: date                                                      */
/*      Purpose: display a formatted date/time or set system date/time.    */
/*   Programmer: George Kerber                                             */
/*      Written: 06/27/89                                                  */
/*     Compiler: Lattice 5.04                                              */
/*-------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <dos.h>

#define WRITTEN "06/27/89 - 01/14/90"
#define VERSION "v1.15c"
#define AUTHOR "George Kerber"
#define TRUE 1

/* Function prototypes */
int ampm(int hour);
int dateset(char name[], char tempy[], int length, int daymax[13], 
            char month[13][4], char monthext[13][7], char node[], 
            unsigned char clock[8]);
void defaultdate(int tznflag, char *timezone, char day[7][4], 
                 char dayext[7][4], char month[13][4], 
                 unsigned char clock[8], int year_fix);
char *ext(int day);
void helpscreen(int escape_char, char node[]);
void mistake(char description[], char node[]);
void quitcheck(char xxx[]);
void setdate(unsigned char clock[8], int daymax[13], char month[13][4], 
             char monthext[13][7], char node[]);
int twelve(int hour);
int isdst(unsigned char clock[8]);

/* AmigaDOS function prototypes - these are declared in system headers */
/* extern int stcgfn(char *node, char *argv0); - declared in string.h */
/* extern int getclk(unsigned char clock[8]); - declared in dos.h */
/* extern int chgclk(unsigned char clock[8]); - declared in dos.h */
/* Simple argument parsing function to replace argopt */
static char *parse_args(int argc, char **argv, char *opts, int *next, char *option);
/* toupper and isdigit are declared in ctype.h */
/* access is declared in unistd.h */
/* gets, puts are declared in stdio.h */
/* getenv is declared in stdlib.h */

int main(int argc, char *argv[])
{ 
    /* Variable declarations - all at the beginning of the block */
    int i, j, length, nflag, dflag, next, counter;
    char option, leadspacer[80], *odata, *timezone, node[33], opts[2];
    char format[80];
    int escape_char, tznflag, y_fix;
    unsigned char clock[8];
    int daymax[13];
    char day[7][4];
    char dayext[7][4];
    char month[13][4];
    char monthext[13][7];
    
    /* Initialize variables */
    nflag = 0;
    dflag = 0;
    next = 1;
    counter = -1;
    escape_char = '*';
    tznflag = 0;
    y_fix = 80;
    leadspacer[0] = '\0';
    opts[0] = 'm';
    opts[1] = '\0';
    
    /* Initialize arrays */
    daymax[0] = 0; daymax[1] = 31; daymax[2] = 28; daymax[3] = 31;
    daymax[4] = 30; daymax[5] = 31; daymax[6] = 30; daymax[7] = 31;
    daymax[8] = 31; daymax[9] = 30; daymax[10] = 31; daymax[11] = 30; daymax[12] = 31;
    
    strcpy(day[0], "Sun"); strcpy(day[1], "Mon"); strcpy(day[2], "Tue");
    strcpy(day[3], "Wed"); strcpy(day[4], "Thu"); strcpy(day[5], "Fri"); strcpy(day[6], "Sat");
    
    strcpy(dayext[0], ""); strcpy(dayext[1], ""); strcpy(dayext[2], "s");
    strcpy(dayext[3], "nes"); strcpy(dayext[4], "rs"); strcpy(dayext[5], ""); strcpy(dayext[6], "ur");
    
    strcpy(month[0], ""); strcpy(month[1], "Jan"); strcpy(month[2], "Feb");
    strcpy(month[3], "Mar"); strcpy(month[4], "Apr"); strcpy(month[5], "May");
    strcpy(month[6], "Jun"); strcpy(month[7], "Jul"); strcpy(month[8], "Aug");
    strcpy(month[9], "Sep"); strcpy(month[10], "Oct"); strcpy(month[11], "Nov"); strcpy(month[12], "Dec");
    
    strcpy(monthext[0], ""); strcpy(monthext[1], "uary"); strcpy(monthext[2], "ruary");
    strcpy(monthext[3], "ch"); strcpy(monthext[4], "il"); strcpy(monthext[5], "");
    strcpy(monthext[6], "e"); strcpy(monthext[7], "y"); strcpy(monthext[8], "ust");
    strcpy(monthext[9], "tember"); strcpy(monthext[10], "ober"); strcpy(monthext[11], "ember"); strcpy(monthext[12], "ember");

/*----- Sets escape value ----- temporary use of *timezone ----------------*/

timezone = getenv("ESCAPE");
if(timezone != NULL) escape_char = *timezone;

/*----- Help Screen -------------------------------------------------------*/

stcgfn(node,argv[0]);
if(argc == 2 && argv[1][0] == '?') helpscreen(escape_char,node);

getclk(clock);   /*  gets current system clock settings  */
if(clock[1] > 19) y_fix = -20;

/*------------------------------------------------------*/

/*  Is this a date or time change request ???  */
length = strlen(argv[1]);
if(length > 2 && length < 9 && 
   !dateset(argv[0],argv[1],length,daymax,month,monthext,node,clock)) 
   exit(0);

/*---------------------------------------*/

for( ; (odata = parse_args(argc,argv,opts,&next,&option)) != NULL ; ) {
   switch(toupper(option)) {
      case 'C':  printf("\x0c"); break;
      case 'N':  nflag = 1; break ;
      case 'D':  if(isdst(clock)) {
                    clock[4]++;
                    if(chgclk(clock)) mistake("Can't Correct for DST",node);
                    }
                 getclk(clock);
                 dflag++;
                 break;
      case 'S':  setdate(clock,daymax,month,monthext,node); break ;
      case 'M':  i = atoi(odata);
                 if(!i || i > 69) mistake("Entry must be 1 - 69",node);
                 for( ; i > 0 ; strcat(leadspacer," "), i--) ;
                 break;
       default:  mistake("Invalid Option",node); break;
      }
   }

if(argc > (next + 1)) mistake("Extra option or Unquoted format string",node);

/*---- SET TIMEZONE if set ------------------------------------------------*/
timezone = getenv("TIMEZONE");
i = strlen(timezone);
if(i == 3 || i == 7) {
   if(isdst(clock) && i == 7) {
      timezone[0] = timezone[4]; 
      timezone[1] = timezone[5]; 
      timezone[2] = timezone[6]; 
      }
   timezone[3] = '\0'; tznflag++;
   }
   else {
   timezone[0] = '\0';
   }
/*------------------------------------------------------------------------*/

printf("%s",leadspacer);

/*-----------------------------*/
if(argc == next ) {
   if(!dflag) {
      odata = getenv("DEFAULT");
      if(odata[0] != NULL) {
         strcpy(format,odata);
         }
         else {
         defaultdate(tznflag,timezone,day,dayext,month,clock,y_fix);
         if(!nflag) printf("\n");
         exit(0);
         }
      }           
   }
   else {
   strcpy(format,argv[next]);
   }
/*-----------------------------*/

while(format[++counter] != '\0') {
   if(format[counter] == escape_char ) {
      if(format[++counter] == escape_char ) {
        printf("%c",escape_char);
        continue;
        }
      switch(format[counter]) {
                    /*  abbreviated weekday name  */
         case 'a':  printf("%s",day[clock[0]]); break ;

                    /*  full weekday name  */
         case 'A':  printf("%s%sday",day[clock[0]],dayext[clock[0]]); break ;

         case 'h':  /*  abbreviated month name  */
         case 'b':  printf("%s",month[clock[2]]); break ;

                    /*  full month name  */
         case 'B':  printf("%s%s",month[clock[2]],monthext[clock[2]]); 
                    break;

                    /*  day of month 01 to 31   */
         case 'd':  printf("%02d",clock[3]); break ;

                    /*  prints date as mm/dd/yy  */
         case 'D':  printf("%02d/%02d/%02d",
                            clock[2],
                            clock[3],
                            clock[1] + y_fix);
                    break;

         case 'E':  /* day of month 1 to 31 */
         case 'e':  printf("%2d",clock[3]); break ;

         case 'o':
         case 'k':  /* greeting and date and time  */
         case 'g':  if(format[counter] == 'g' ||
                       format[counter] == 'o') {; 
                    printf("Good "); 
                    if(clock[4] < 12) {
                       printf("Morning");
                       }
                    else if(clock[4] < 17) {
                       printf("Afternoon");
                       }
                    else {
                       printf("Evening");
                       }
                    }

                    if(format[counter] == 'g') printf("!    "); 
                    if(format[counter] == 'k' ||
                       format[counter] == 'g') {
                   printf("Today is %s%sday, %s%s %d%s, %02d at %d:%02d %cM",
                       day[clock[0]],
                       dayext[clock[0]],
                       month[clock[2]],
                       monthext[clock[2]],
                       clock[3],
                       ext(clock[3]),
                       clock[1] + 1980,
                       twelve(clock[4]),
                       clock[5],
                       ampm(clock[4]));
                    if(tznflag) printf(" (%s)",timezone);
                    }
                    break;

                    /*  hour 00 to 23  */
         case 'H':  printf("%02d",clock[4]); break ;

                     /*  hour 01 to 12  */
         case 'i':  printf("%d",twelve(clock[4])); break;

                    /*  hour 01 to 12  */
         case 'I':  printf("%02d",twelve(clock[4])); break;
                    
         case 'J':  /*  julian date and days remaining in year  */
         case 'j':  if((clock[1] + 1980) % 4 == 0) daymax[2] = 29 ;
                    for(i = 1, j = 0 ; i != clock[2] ; j += daymax[i++] );
                    j += clock[3];
                    if(format[counter] == 'j') { printf("%d",j); break; }
                    if(daymax[2] == 28) i = 365; else i = 366;
                    printf("%d",i - j);
                    break;  

                    /*  month of year 01-12  */
         case 'm':  printf("%02d",clock[2]); break ;

         case 'N':  /*  creates newline  */
         case 'n':  printf("\n"); break ;

                    /*  time as hh:mm:ss pp, pp is "am" or "pm" */
         case 'r':  printf("%02d:%02d:%02d ",
                           twelve(clock[4]),
                           clock[5],
                           clock[6]);
                    /*  no break since am/pm is next  */

                    /*  string containing "am" or "pm"  */
         case 'p':  printf("%cM",ampm(clock[4]));
                    break;

                    /* time as hh:mm  */
         case 'R':  printf("%02d:",clock[4]);
                    /*  no break since minute is next  */

         case 'M':  printf("%02d",clock[5]); break ;

                    /*  insert a tab character */
         case 't':  printf("\t"); break ;

                    /*  prints time as hh:mm:ss  */
         case 'T':  printf("%02d:%02d:",clock[4],clock[5]);
                    /*  no break since seconds is next  */

                    /*  seconds 00 - 59  */
         case 'S':  printf("%02d",clock[6]); break ;

                    /*  prints day of week integer, Sunday = 0  */
         case 'w':  printf("%d",clock[0]); break ;

                    /*  prints day of week integer, Sunday = 1  */
         case 'W':  printf("%d",clock[0] + 1); break ;

                    /*  prints two character year  */
         case 'y':  printf("%d",clock[1] + y_fix); break ;
   
         case 'q':  printf("\""); break;

         case 'c':
         case 'C':  printf("%s %02d-%s-%02d %02d:%02d",
                       day[clock[0]],
                       clock[3],
                       month[clock[2]],
                       clock[1] + y_fix,
                       clock[4],
                       clock[5]);
                    if(tznflag) printf(" %s",timezone); 
                    break;

         case 'z':  /*  timezone name  */
         case 'Z':  if(tznflag) printf("%s",timezone); break;
                   
                    /* displays default date string via descriptor  */ 
         case '$':  defaultdate(tznflag,timezone,day,dayext,month,clock,y_fix);
                    break;

         case 'U':  /* UNIX type date  */
         case 'u':  printf("%s %s %2d %02d:%02d:%02d ",
                       day[clock[0]], 
                       month[clock[2]],
                       clock[3],
                       clock[4],
                       clock[5],
                       clock[6]);
                    if(tznflag) printf("%s ",timezone);
                    /*  no break since year is next  */

                    /*  prints year as ccyy  */
         case 'Y':  printf("%4d",clock[1] + 1980); break ;

         case 'X':  /*  prints date extension ex: th, nd, rd  */     
         case 'x':  printf("%s",ext(clock[3])); break;

         case '0':  printf("[30m"); break ;  /* Blue pen color         */
         case '1':  printf("[31m"); break ;  /* White pen color        */
         case '2':  printf("[32m"); break ;  /* Black pen color        */
         case '3':  printf("[33m"); break ;  /* Orange pen color       */
         case '4':  printf("[0m");  break ;  /* Default colors         */
         case '5':  printf("[1m");  break ;  /* Boldface               */
         case '6':  printf("[4m");  break ;  /* Underline              */
         case '7':  printf("[3m");  break ;  /* Italics                */
         case ')':  printf("[40m"); break;   /* color 0 bkgnd          */
         case '!':  printf("[41m"); break;   /* color 1 bkgnd          */
         case '@':  printf("[42m"); break;   /* color 2 bkgnd          */
         case '#':  printf("[43m"); break;   /* color 3 bkgnd    [0m */

          default:  printf("\n\07  ERROR: %s  [33mBad Format Character.[0m \(%c%c\)\n\n",
                       node,format[--counter],
                       format[++counter]);
                    exit(5);
         }
      }
      else {
      putchar(format[counter]);
      }
   continue;
   }
printf("[0m");
if(!nflag) printf("\n");
exit(0);    /*  exit for good date formatting   */
}

/*-------------------------------------------------------------------------*/

/*  AMPM FUNCTION  */

int ampm(int hour)
{
    if(hour < 12) {
        return('A');
    } else {
        return('P');
    }
}

/*-------------------------------------------------------------------------*/

/*  DATESET FUNCTION  !!!! */

int dateset(char name[], char tempy[], int length, int daymax[13], 
            char month[13][4], char monthext[13][7], char node[], 
            unsigned char clock[8])
{
    int h, i, j, k, slash, colon;
    char date[80];
    char *s1, *s2, temp[3][6];
    static char delim[] = "/-:";
    
    /* Initialize variables */
    h = 0;
    k = 0;
    slash = 0;
    colon = 0;

strcpy(date,tempy);                  /* this is necessary for some reason */
for( i = 0 ; i != length ; i++ ) {   /* or else argv[1] gets changed      */
   if(date[i] == '\\') return(1);    /* I have no idea why ????           */  
   else if(date[i] == ':') colon++;
   else if(date[i] == '/' || date[i] == '-') slash++;
   else if(!isdigit(date[i])) return(1);
   }
if((colon && slash) || (!colon && !slash)) return(1);
i = strlen(date);
if(colon && i != 4 && i != 5) return(1);

s1 = date;
while((s2 = strtok(s1,delim)) != NULL) {      /* break out seperate month */
   strcpy(temp[h],s2);                        /* day, year, hour & minute */
   if(h++ > 3) break;
   s1 = NULL;
   }
if(colon > 1 || slash > 2) return(1); 

i =  atoi(temp[0]); j = atoi(temp[1]); 

if(slash) {     /*  if a 'date' was entered  */
   if(h == 2) {
      k = clock[1];  
      }
      else {
      k = atoi(temp[2]);
      if(k < 0 || k > 99) mistake("Invalid Year",node);
      if(k >= 80) clock[1] = k - 80; else clock[1] = k + 20;
      }
   if((clock[1] + 1980) % 4 == 0) daymax[2] = 29; else daymax[2] = 28;
   if(i < 0 || i > 12 || j < 1 || j > daymax[i]) mistake("Invalid Date",node); 
   clock[2] = i; clock[3] = j; 
   chgclk(clock);
   }

else if(colon) {    /*  if a 'time' was entered  */
   if(i < 0 || i > 23 || j < 0 || j > 59) mistake("Invalid Time",node);
   clock[4] = i;
   clock[5] = j;
   clock[6] = clock[7] = 0;
   chgclk(clock);
   }
/*  confirm the changes  */
   printf("\n%sDate: %s%s %d%s, %d     %sTime: %02d:%02d:%02d\n\n",
           slash ? "New ":"",
           month[clock[2]],
           monthext[clock[2]],
           clock[3],
           ext(clock[3]),
           clock[1] + 1980,
           colon ? "New ":"",
           clock[4],
           clock[5],
           clock[6]);

   return(0);   /*  return '0' for success  */
}

/*-------------------------------------------------------------------------*/

/*  DEFAULTDATE FUNCTION  */

void defaultdate(int tznflag, char *timezone, char day[7][4], 
                 char dayext[7][4], char month[13][4], 
                 unsigned char clock[8], int year_fix)
{
    printf("%s%sday %02d-%s-%02d %02d:%02d:%02d",
            day[clock[0]],
            dayext[clock[0]],
            clock[3],
            month[clock[2]],
            clock[1] + year_fix,
            clock[4],
            clock[5],
            clock[6]);
if(tznflag) printf(" %s",timezone);
      
}

/*-------------------------------------------------------------------------*/

/*  EXT FUNCTION  */

char *ext(int day)
{
    switch(day) {
        case  1:
        case 21:
        case 31: return("st"); 
        case  2:
        case 22: return("nd");
        case  3:
        case 23: return("rd");
        default: return("th");
    }
}

/*-------------------------------------------------------------------------*/

/* HELPSCREEN FUNCTION  */

void helpscreen(int escape_char, char node[])
{
int dhelp;
if(!access("c:datehelp",0)) {
   dhelp = 1 ;
   printf("\x0c");
   }
   else {
   dhelp = 0;
   }
printf("\n   [33m[1m%s[0m      %s %10s %25s[0m\n",node,AUTHOR,VERSION,WRITTEN);
printf("   SYNTAX:  %s [-s] [hh:mm] [mm/dd[/yy]] [-d -n -m xx] [\"format string\"]\n",node);
printf("            -s     Prompts user to set date/time.\n");
printf("         hh:mm     Set time.\n");
printf("    mm/dd[/yy]     Set date, year is optional.            Escape = \"[33m[1m%c[0m\"\n",
   escape_char);
printf("            -d     Automatic Daylight Savings Time adjustment.\n");
printf("            -n     Supress NEWLINE.\n");
printf("            -m xx  Begins date string at xx character position.\n\n");
if(dhelp) system("c:datehelp x");
exit(0);
}

/*-------------------------------------------------------------------------*/

/*  ISDST FUNCTION  */

int isdst(unsigned char clock[8])
{
    int year, startyear, sday, eday, leapyear, dstflag;
    
    /* Initialize variables */
    startyear = 1980;
    sday = 6;
    leapyear = 0;
    dstflag = 0;
    year = clock[1];

for(  ; year > 0 ; year--) {
     leapyear = 0;
     startyear++ ;
     if(!(startyear % 4)) leapyear = 1 ;
     sday-- ;
     if(!sday) sday = 7;
     sday -= leapyear;
     if(!sday && leapyear) sday = 7; 
     }                                              /*  Ending for brace */

/* Calculate Daylight Savings Time end date using start date              */

if((sday + 20) < 25)  eday = sday + 27; else eday = sday + 20;

/* Determine if it's daylight savings time NOW                            */

if((clock[2] > 4 && clock[2] < 10) ||
    (clock[2] == 4 && clock[3] >= sday && clock[4] >= 2) ||
    (clock[2] == 10 && clock[3] <= eday && clock[4] < 2) ||
    (clock[2] == 4 && clock[3] > sday) ||
    (clock[2] == 10 && clock[3] < eday)) {
    dstflag = 1; 
    }
return(dstflag);
}

/*-------------------------------------------------------------------------*/

/*  MISTAKE FUNCTION  */

void mistake(char description[], char node[])
{
printf("\n\07[0m  ERROR: %s  --> [33m%s.[0m\n\n",node,description);
exit(5);
}

/*-------------------------------------------------------------------------*/

/*  QUITCHECK FUNCTION  */

void quitcheck(char xxx[])
{
if(xxx[0] == 'q' || xxx[0] == 'Q') {
   puts("[0m\n");
   exit(0) ;
   }
}

/*-------------------------------------------------------------------------*/

/*  SETDATE FUNCTION  */

void setdate(unsigned char clock[8], int daymax[13], char month[13][4], 
             char monthext[13][7], char node[])
{
    char input[30];
    int temp;
    char current[13];
    char accept[30];
    char new[12];
    
    /* Initialize strings */
    strcpy(current, "The current ");
    strcpy(accept, " or <return> to accept [q]: [33m");
    strcpy(new, " enter new ");

while(TRUE) {
      printf("\x0c\n\n\n  Set [33mDATE/TIME[0m Utility.                 by %s\n\n",AUTHOR);
      while(TRUE) {
         printf("\n  [0m%syear is [33m[1m%2d[0m,  %syear%s",
            current,
            clock[1] + 1980,
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if(temp > 2040 || temp < 1980) {
               printf("\n\07            [0mERROR:  [33mInvalid Year (1980 - 2040).[0m\n\n");
               continue ;
               }
               else {
               clock[1] = temp - 1980;
               }
            }
         break ;
         }

      while(TRUE) {
         printf("\n   [0m%smonth is [33m[1m%02d[0m, %smonth%s",
            current,
            clock[2],
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if(temp < 1 || temp > 12 ) {
               printf("\n\07            [0mERROR:  [33mInvalid Month (1 - 12).[0m\07\n\n");
               continue ;
               }
               else {
               clock[2] = temp;
               }
            }
         break ;
         }

      if((clock[1] + 1980) % 4 == 0) daymax[2] = 29 ;
      if(clock[3] > daymax[clock[2]] ) { 
         printf("\n            [0mNOTE:  [33m%sday is invalid for %d.\n\n",
                current,
                clock[1] + 1980);
         }
   
      while(TRUE) {
         printf("\n     [0m%sday is [33m[1m%02d[0m,   %sday%s",
            current,
            clock[3],
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if(temp < 1 || temp > daymax[clock[2]]) {
               printf("\n\07            [0mERROR:  [33mInvalid Day (1 - %d).[0m\n\n",daymax[clock[2]]);
               continue ;
               }
               else {          
               clock[3] = temp;
               }
            }
         if(clock[3] > daymax[clock[2]] ) {   
         printf("\n            [0mERROR:  [33m%sday is invalid for %d.\n\n",
                current,
                clock[1] + 1980);
            continue ;
            }
         break ;
         }

      while(TRUE) {
         printf("\n    [0m%shour is [33m[1m%02d[0m,  %shour%s",
            current,
            clock[4],
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if((temp > 0 && temp < 24) ||
               !strcmp(input,"0") || !strcmp(input,"00")) {
               clock[4] = temp;
               }
               else {
               printf("\n\07            [0mERROR:  [33mInvalid Hour (00 - 23).[0m\n\n");
               continue ;
               }
            }
         break ;
         }

      while(TRUE) {
         printf("\n  [0m%sminute is [33m[1m%02d[0m,%sminute%s",
            current,
            clock[5],
            new,
            accept);
         gets(input);
         quitcheck(input);
         if(strcmp(input,"")) {
            temp = atoi(input);
            if((temp > 0 && temp < 60) ||
               !strcmp(input,"0") || !strcmp(input,"00")) {
               clock[5] = temp;
               }
               else {
               printf("\n\07            [0mERROR:  [33mInvalid Minute (00 - 59).[0m\n\n");
               continue;
               }
            } 
         break ;
         }

      printf("\n\n      [0mThe system clock will be set to: [33m%02d-%s%s-%d %02d:%02d:00[0m\n",
              clock[3],
              month[clock[2]],
              monthext[clock[2]],
              clock[1] + 1980,
              clock[4],
              clock[5]);
      printf("\n\n      Do you want to q[33m\(uit\)[0m, s[33m\(et\)[0m or e[33m\(dit\)[0m the date/time. [q,S,e]: ");
      gets(input);
      quitcheck(input);
   if(input[0] == 'e' || input[0] == 'E') {
        putchar('\n');
        continue ;
        }
      clock[6] = 0;
     if(chgclk(clock)) mistake("Can't set SYSTEM CLOCK",node);
      putchar('\n');
      exit(0);
   }
putchar('\n');
exit(0);
}

/*-------------------------------------------------------------------------*/

/*  TWELVE FUNCTION  */

int twelve(int hour)
{
    if(hour > 13) {
        return(hour - 12);
    } else {
        if(!hour) {
            return(12);
        } else {
            return(hour);
        }
    }
} 

/*-------------------------------------------------------------------------*/

/* Simple argument parsing function to replace AmigaDOS argopt */
static char *parse_args(int argc, char **argv, char *opts, int *next, char *option)
{
    static int optind = 1;
    char *optarg = NULL;
    int i;
    
    if (optind >= argc) {
        return NULL;
    }
    
    if (argv[optind][0] != '-') {
        return NULL;
    }
    
    *option = argv[optind][1];
    *next = optind + 1;
    
    /* Check if option takes an argument */
    for (i = 0; opts[i] != '\0'; i++) {
        if (opts[i] == *option) {
            if (optind + 1 < argc) {
                optarg = argv[optind + 1];
                optind += 2;
            } else {
                optind++;
            }
            return optarg;
        }
    }
    
    optind++;
    return "";
}

/*-------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------*\

10/04/89 v1.10:  Removed the requirement for strings to begin with a !
10/29/89         Re-compiled with variable assignments changed.
11/01/89 v1.11b: Re-compiled with Lattice 5.04
12/30/89 v1.14:  Added daylight savings time functions.
                 Added command line date and time set options.
                 Removed all global variables.
01/11/90 v1.15b: Re-compiled with Lattice 5.04a, fix for getenv
01/14/90 v1.15c: Add env DEFAULT to change the default date string.

\*-------------------------------------------------------------------------*/
