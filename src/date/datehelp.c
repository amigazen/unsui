/*-------------------------------------------------------------------------*/
/*      program: datehelp                                                  */
/*      Purpose: display helpscreen for date/udate                         */
/*   Programmer: George Kerber                                             */
/*      Written:  07/08/89                                                 */
/*-------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#define WRITTEN "07/08/89 - 12/28/89"
#define VERSION "v1.04"

int main(int argc, char *argv[])
{
    if(argc > 1 && argv[1][0] == 'x') {
        printf(" [33ma[0m abbreviated weekday name        [33mA[0m full weekday name    [33mR[0m time - hh:mm\n");
        printf(" [33mb[0m abbreviated month name          [33mB[0m full month name      [33mH[0m hour - 00 to 23\n");
        printf(" [33md[0m day of month - 01 to 31         [33mD[0m date - mm/dd/yy      [33mZ[0m timezone name*\n");
        printf(" [33mS[0m second - 00 to 59               [33mt[0m tab character        [33mi[0m hour - 1 to 12\n");
        printf(" [33mx[0m day of month ext \\(st,nd,rd,th\\)  [33mn[0m newline character    [33mI[0m hour - 01 to 12\n");
        printf(" [33me[0m day of month -  1 to 31         [33mY[0m four digit year      [33my[0m two digit year\n");
        printf(" [33mm[0m month of year - 01 to 12        [33mM[0m minute - 00 to 59    [33mT[0m time - hh:mm:ss\n");
        printf(" [33mq[0m print a literal quote           [33mj[0m julian day of year   [33mJ[0m days remaining\n");
        printf(" [33mg[0m greeting + date/time (o + k)    [33mo[0m greeting (Good...)   [33mk[0m date/time string\n");
        printf(" [33mu[0m ddd mmm dd hh:mm:ss tzn* yyyy   [33mr[0m time - hh:mm:ss pp   [33mp[0m string, AM or PM\n");
        printf(" [33mw[0m day of week - Sunday = 0        [33mW[0m same as w, Sun = 1   [33m$[0m default date\n\n");
    }
    else {
        printf("\n  NOTE:  [33mdatehelp[0m is called from the date/udate program.\n\n");
    }
    return 0;
}