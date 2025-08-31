/*	math functions for ansic.library	*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef	_MATH_H
#define _MATH_H

#include	<sys/errno.h>

/* the functions prototypes */

#ifdef	__cplusplus
extern "C" {
#endif

extern double const fmod(double, double);
extern double modf(double, double *);
extern double frexp(double, int *);
extern double const IEEEDPAbs(double);
extern double const IEEEDPFloor(double);
extern double const IEEEDPCeil(double);
extern double const IEEEDPTan(double);
extern double const IEEEDPAtan(double);
extern double const IEEEDPCos(double);
extern double const IEEEDPACos(double);
extern double const IEEEDPSin(double);
extern double const IEEEDPASin(double);
extern double const IEEEDPExp(double);
extern double const IEEEDPPow(double,double);
extern double const IEEEDPLog(double);
extern double const IEEEDPLog10(double);
extern double const IEEEDPTanh(double);
extern double const IEEEDPCosh(double);
extern double const IEEEDPSinh(double);
extern double const IEEEDPSqrt(double);

#ifdef	__cplusplus
}
#endif


#ifdef	__cplusplus

static const double	PI = 3.141592653589793;
static const double	M_PI = 3.141592653589793;
static const double	TWO_PI = 2*PI;
static const double	PI2	= PI/2.0;
static const double	PI4	= PI/4.0;
static const double	E = 2.718281828459045;
static const double	M_E = 2.718281828459045;
static const double	LOG10 = 2.302585092994046;
static const double	FPTEN = 10.0;
static const double	FPONE = 1.0;
static const double	FPHALF = 0.5;
static const double	FPZERO = 0.0;
static const double     HUGE = 1.0e300;

#else	/* not __cplusplus */

#define PI		((double)3.141592653589793)
#define M_PI		((double)3.141592653589793)
#define TWO_PI		(((double)2)* PI)
#define PI2		(PI/((double)2))
#define PI4		(PI/((double)4))
#define	E		((double)2.718281828459045)
#define	M_E		((double)2.718281828459045)
#define LOG10		((double)2.302585092994046)
#define FPTEN		((double)10.0)
#define	FPONE		((double)1.0)
#define FPHALF		((double)0.5)
#define FPZERO		((double)0.0)
#define HUGE            ((double)1.0e300)

#endif /* __cplusplus */


/* now define c functions in term of library calls */

extern inline const double fabs(double d)
{
  return IEEEDPAbs(d);
}

extern inline const double floor(double d)
{
  return IEEEDPFloor(d);
}

extern inline const double ceil(double d)
{
  return IEEEDPCeil(d);
}

extern inline const double tan(double d)
{
  return IEEEDPTan(d);
}

extern inline const double atan(double d)
{
  return IEEEDPAtan(d);
}

extern inline const double cos(double d)
{
  return IEEEDPCos(d);
}

extern inline const double acos(double d)
{
  return IEEEDPACos(d);
}

extern inline const double sin(double d)
{
  return IEEEDPSin(d);
}

extern inline const double asin(double d)
{
  return IEEEDPASin(d);
}

extern inline const double exp(double d)
{
  return IEEEDPExp(d);
}

extern inline const double pow(double a,double b)
{
  return IEEEDPPow(b,a);
}

extern inline const double log(double a)
{
  return IEEEDPLog(a);
}

extern inline const double log10(double a)
{
  return IEEEDPLog10(a);
}

extern inline const double sqrt(double a)
{
  return IEEEDPSqrt(a);
}

extern inline const double sinh(double a)
{
  return IEEEDPSinh(a);
}

extern inline const double cosh(double a)
{
  return IEEEDPCosh(a);
}

extern inline const double tanh(double a)
{
  return IEEEDPTanh(a);
}

extern inline const int trunc(double x)
{
  return (int)(x);
}

extern inline const int round(double x)
{
  return (int)((x) + 0.5);
}

extern inline const double itof(int i)
{
  return (double)(i);
}

extern inline const double ldexp(double a,int b)
{
  return a*pow(2.0,b);
}

extern inline const int abs(int x)
{
  return ((x)<0?-(x):(x));
}

extern inline const double atan2(const double y, const double x)
{
  if(x > 0) {
    if (y > 0) {
      if (x > y)
	return atan (y/x);
      else
	return (PI2 - atan(x/y));
    }
    else {
      if (x > -y)
	return atan (y / x);
      else
	return - PI2 - atan (x / y);
    }
  }
  else {
    if (y > 0) {
      if (-x > y)
	return PI + atan (y / x);
      else
	return PI2 - atan (x / y);
    }
    else {
      if (-x > -y)
	return - PI + atan (y / x);
      else if (y < 0)
	return - PI2 - atan (x / y);
      else {
	union {
	  unsigned long i[2];
	  double        val;
	} value;
	
	value.i[0] = 0x7fffffff;
	value.i[1] = 0xffffffff;
	return value.val;
      }
    }
  }
}

extern inline const double hypot (const double x, const double y)
{
  return sqrt (x*x + y*y);
}


#endif	/* _MATH_H */
