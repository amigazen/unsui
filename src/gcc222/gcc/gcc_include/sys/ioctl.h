/* sys/ioctl.h include file for ansic.library */
/* Copyright (C) 1992 - Davide Pasetto */

#ifndef __SYS_IOCTL__
#define __SYS_IOCTL__

struct tchars {
  char t_intrc;
  char t_quit;
  char t_startc;
  char t_stopc;
  char t_eofc;
  char t_brkc;
};

#define LCRTBS   000001
#define LPRTERA  000002
#define LCRTERA  000004
#define LTILDE   000010
#define LMDMBUF  000020
#define LLITOUT  000040
#define LTOSTOP  000100
#define LFLUSHO  000200
#define LNOHANG  000400
#define LETXACK  001000
#define LCRTKIL  002000
#define LINTRUP  004000
#define LCTLECH  010000
#define LPENDIN  020000
#define LDECCTQ  040000


struct lchars {
  char t_suspc;
  char t_dstopc;
  char t_rprntc;
  char t_flushc;
  char t_werasec;
  char t_lnextc;
};

#endif /* __SYS_IOCTL__ */
