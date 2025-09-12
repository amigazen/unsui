/*-------------------------------------------------------------------------*/
/*      Program:  dst2                                                     */
/*   Programmer:  George Kerber                                            */
/*      Written:  12/22/89                                                 */
/*     Compiler:  Lattice 5.04                                             */
/*-------------------------------------------------------------------------*/

#include <stdio.h>
#include <dos.h>
#define WRITTEN "01/04/90"
#define VERSION "v2.0"


void helpscreen();

main(int argc, char *argv[])

{
int i , startyear = 1980 , sday = 6 , eday , leapyear = 0;
unsigned char clock[8];

if(argc > 1) helpscreen();

/* Get battery clock and calculate Daylight Savings Time start date       */

getclk(clock); 


for( i = clock[1] ; i > 0 ; i--) {
     leapyear = 0;
     startyear++ ;
     if((startyear % 4) == 0) leapyear = 1 ;
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
    clock[4]++;
    chgclk(clock);
    }
}

/*-------------------------------------------------------------------------*/

/*  HELPSCREEN FUNCTION  */

void helpscreen()
{
printf("\n\n  [33m[1mDST2[0m      George Kerber     %s     %s\n\n",VERSION,WRITTEN);
printf("  Add to startup-sequence for automatic Daylight Savings Time Adjustment.\n\n");
printf("  Always keep your BATTERY CLOCK set to [1mSTANDARD[0m time in your area.\n\n");
exit(0);
}
/*-------------------------------------------------------------------------*/
