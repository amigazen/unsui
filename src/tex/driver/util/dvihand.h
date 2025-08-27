#define PixRound(x,conv)	(long)(((long)(x) + ((long)(conv) / 2)) / (long)(conv))

extern double alpha;                    /* conversion ratio, DVI unit per TFM unit */



#include "dvihand.i"

static long Read1Byte		Args((DVIFILE *fp));
static long Read2Byte		Args((DVIFILE *fp));
static long Read3Byte		Args((DVIFILE *fp));
static long Read4Byte		Args((DVIFILE *fp));



static long __inline Read1Byte(DVIFILE *fp)     /* return n byte quantity from file fd */
{
  unsigned char buf;

  DVIfread(&buf, 1, 1, fp);

  return buf;
}

static long __inline Read2Byte(DVIFILE *fp)
{
  unsigned short x;

  DVIfread((char *)&x,2,1,fp);
  return (long)((unsigned long)x);
}

static long __inline Read3Byte(DVIFILE *fp)
{
  unsigned char x[4];

  DVIfread(&(x[0]),3,1,fp);
  return (long)((*(unsigned long *)(&(x[0]))) >> 8);
}

static long __inline Read4Byte(DVIFILE *fp)
{
  long x;

  DVIfread((char *)&x,1,4,fp);
  return x;
}

