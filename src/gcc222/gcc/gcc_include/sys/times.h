/*	system times for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef _SYS_TIMES_H
#define _SYS_TIMES_H

/* #include <sys/time.h> */


struct tms {
  unsigned long tms_utime;
  unsigned long tms_stime;
};

#define HZ 50

#endif /* _SYS_TIMES_H */


