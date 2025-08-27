/* Use new ANSI Standard Lib functions to speed up arithmetic */

#include <stdlib.h>	/* for `div_t' and `ldiv_t' structs */

#define EXTERN extern
#include "texd.h"

/*
 *   Fuer Amiga und andere:
 *
 * ldiv ist eine Division zweier `long'-Werte
 *

ldiv_t ldiv(long numer, long denom)
{
  ldiv_t res;

  res.quot = numer / denom;
  res.rem = numer - res.quot * denom;
  return res;
}

 *
 * ist fuer Amiga im File `ldiv.asm' bzw. ist in der C-Library.
 *
 */



integer half ( integer x )
{
 	/* define odd(x)  ((x) % 2)   in "extra.c" */
	/* simpler: odd(x)  ((x) & 1)   is ok for x<0 */
  if ( x & 1 )		/* if ( odd ( x ) ) */
    return ( x + 1 ) / 2;
  else
    return x / 2;
}

#if 0
scaled rounddecimals ( smallnumber k )
{ integer a;

  a = 0 ; 
  while ( k > 0 ) {
    decr ( k ) ; 
    a = ( a + dig [ k ] * two ) / 10 ; 
  } 
  return ( a + 1 ) / 2 ; 
}
#endif

void printscaled ( scaled s )
{ printscaled_regmem 
  scaled delta;

  if ( s < 0 ) {
    printchar ( 45 ) ; 
    s = - (integer) s ; 
  }

  printint ( s / unity ) ; 
  printchar ( 46 ) ; 
  s = TEN_MULT( s % unity ) + 5 ; 
  delta = 10 ; 
  do {
    if ( delta > unity ) 
      s = s - 17232 ; 
    printchar ( 48 + ( s / unity ) ) ; 
    s = TEN_MULT( s % unity ) ; 
    delta = TEN_MULT(delta) ; 
  } while ( s > delta ) ; 
}

scaled multandadd ( integer n, scaled x, scaled y, scaled maxanswer )
{ multandadd_regmem 

  if ( n < 0 ) {
    x = - (integer) x;
    n = - (integer) n;
  }
  if ( n == 0 ) 
    return y;

  if ( ( x <= (maxanswer-y)/n ) && ( - (integer) x <= (maxanswer+y)/n ) )
    return ( n * x + y );

  aritherror = true ; 
  return 0;
} 


scaled xovern ( scaled x, integer n )
{ register int negative = false;	/* was "boolean" */
  ldiv_t r;

  if ( n == 0 ) {
    aritherror = true ;
    remainder = x ;
    return 0 ;
  }

  if ( n < 0 ) {
      x = - (integer) x ; 
      n = - (integer) n ; 
      negative = true ; 
  }
  if ( x >= 0 ) {
      r = ldiv(x, n);
      if( negative )
	r.rem = - r.rem;
      remainder = (integer) r.rem;
      return( (integer) r.quot );
  } else {
      x = - (integer) x;
      r = ldiv(x, n);
      if( ! negative )
	r.rem = - r.rem;
      remainder = (integer) r.rem;
      return( - (integer) r.quot );
  }
}


scaled xnoverd ( scaled xs, integer int_n, integer int_d )
{ xnoverd_regmem
  register unsigned long n = int_n;	/* GCC is better with this */
  register unsigned long d = int_d;
  register int positive;	/* was "boolean" */
  nonnegativeinteger t, u, v;
  unsigned long x;
  ldiv_t r;

  if ( xs >= 0 )
    positive = true;
  else {
    xs = - (integer) xs;
    positive = false;
  }

  /* GCC is better with this ... */
  x = (unsigned long) xs;

  /* x is unsigned long,  x%(2^15) is unsigned short, n dto. */
  t = ( x % 32768L ) * n;
  u = ( x / 32768L ) * n + ( t / 32768L );

  r = ldiv(u, d);
  v = ( r.rem ) * 32768L + ( t % 32768L );
  if ( r.quot >= 32768L )
    aritherror = true ; 
  else
    u = 32768L * ( r.quot );	 /* + ( v / d ) */

  r = ldiv(v, d);
  if ( positive ) {
    remainder = r.rem;
    return( (integer) u + r.quot );	/* here is +(v/d) */
  } else {
    remainder = - r.rem;
    return( - ((integer) u + r.quot) );	/* here is +(v/d) */ 
  }
}


#ifdef atarist
/* GCC 1.37.1 for 68000 is (censored) with  moveq #16,d0  asll d0,d1
 * instead of a simple clrw and swap of the register, so ...
 */
#define SWAP(a)  __asm__ volatile("swap %0" : "=d" (a) : "0" (a) )
#define CLRW(a)  __asm__ volatile ("clrw %0" : "=d" (a) : "0" (a) )
#define EXTL(a)  __asm__ volatile ("extl %0" : "=d" (a) : "0" (a) )
#if 0
#define MULU(a,b) ((b) = (a) * (b))
#else
#define MULU(a,b) \
  __asm__ volatile ("mulu %2,%0" : "=d" (b) : "0" (b), "d,i" (a))
#endif
#endif


long_halfword badness ( scaled t, scaled s )
{ integer r;
  scaled t1;

  if ( t == 0 )
    return 0;

  if ( s <= 0 ) 
    return infbad;

  if ( t <= 7230584L ) {
#ifdef atarist
    /* r = (((unsigned short)(t >> 16)) * 297) << 16;  */
    t1 = t;  SWAP(t1);
    /* r = ((unsigned short) t1 ) * (unsigned short)297; */
    MULU(297,t1); r = t1;
    SWAP(r);  CLRW(r);

    /* r += ((unsigned short)(t & 0xffff)) * (unsigned short)297; */
    MULU(297,t); r += t;
    r /= s;
#else
    r = ( t * 297 ) / s ;
#endif
  } else {
    if ( s >= 1663497L ) {
	r = t / ( s / 297 ) ; 
    } else
		/* ==>> (t > 7.230.584  && s < 1.663.497) */
	/* r = t ; */
	return infbad;
  }

  if ( r > 1290 ) 
    return infbad;

#ifndef atarist
  /* we know: r <= 1290 */
  return( ( ((short)r) * ((short)r) * ((short)r) + 131072L ) / 262144L );
#else
  { unsigned long r1 /* = ((unsigned short) r) * ((unsigned short) r) */ ;
    unsigned long res;
    r1 = r; MULU(r, r1);

    /* res = (((unsigned short)(r1 >> 16)) * (unsigned short) r) << 16; */
    res = r1;  SWAP(res);
    /* res = ((unsigned short) res) * ((unsigned short) r); */
    MULU(r, res);
    SWAP(res);  CLRW(res);

    /* res += ((unsigned short)(r1 & 0xffff)) * ((unsigned short) r); */
    MULU(r, r1); res += r1;

# if 0
    return( ( res + 131072L ) / 262144L );
# else
    res += 131072L;  CLRW(res);  SWAP(res);  return( res >> 2 );
# endif
  }
#endif
} 

/* -- end -- */
