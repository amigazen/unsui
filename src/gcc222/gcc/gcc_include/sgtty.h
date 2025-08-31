/* sgtty file for ansic.library */
/* copyright (C) Davide Pasetto 1992 */

#ifndef __SGTTY__
#define __SGTTY__

#include <sys/ioctl.h>

#define TIOCGETP  1
#define TIOCSETP  2
#define TIOCGETD  3
#define TIOCSETD  4
#define TIOCGETC  5
#define TIOCSETC  6
#define TIOCSETN  7

/* not implemented yet
#define TIOCEXCL  8
#define TIOCNXCL  9
#define TIOCHPCL  10
#define TIOCFLUSH 11
#define TIOCSTI   12
#define TIOCSBRK  13
#define TIOCSDTR  14
#define TIOCCDTR  15
#define TIOCGPGRP 16
#define TIOCSPGRP 17
*/

#define FIONREAD  18
#define TIOCLBIS  19
#define TIOCLBIC  20
#define TIOCLSET  21
#define TIOCLGET  22
#define TIOCSLTC  23
#define TIOCGLTC  24


struct sgttyb {
  char sg_ispeed;
  char sg_ospeed;
  char sg_erase;
  char sg_kill;
  short sg_flags;
};

#define B0       0
#define B50      1
#define B75      2
#define B110     3
#define B134     4
#define B150     5
#define B200     6
#define B300     7
#define B600     8
#define B1200    9
#define B1800    10
#define B2400    11
#define B4800    12
#define B9600    13
#define EXTA     14
#define EXTB     15

#define ALLDELAY 0177400
#define BSDELAY  0100000
#define BS0      0
#define BS1      0100000
#define VTDELAY  0040000
#define FF0      0
#define FF1      0100000
#define CRDELAY  0030000
#define CR0      0
#define CR1      0010000
#define CR2      0020000
#define CR3      0030000
#define TBDELAY  0006000
#define TAB0     0
#define TAB1     0001000
#define TAB2     0004000
#define XTABS    0006000
#define NLDELAY  0001400
#define NL0      0
#define NL1      0000400
#define NL2      0001000
#define NL3      0001400
#define EVENP    0000200
#define ODDP     0000100
#define RAW      0000040
#define CRMOD    0000020
#define ECHO     0000010
#define LCASE    0000004
#define CBREAK   0000002
#define TANDEM   0000001



extern ioctl(int,int,void *);

#define stty(x,y) ioctl(x,TIOCSETP,y)
#define gtty(x,y) ioctl(x,TIOCGETP,y)

#endif /* __SGTTY__ */
