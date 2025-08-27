#define EXTERN extern
#include "texd.h"

#ifdef AMIGA
# ifdef BIG
#  define AMIGA_ASM
# endif
#endif


/*
 *  Hier sind alle Routinen der Speicherverwaltung (Part 9), von der die
 *  wichtigsten in Assembler geschrieben sind.
 *
 * Achtung: Wird `max_halfword', sizeof(memoryword), Node-Layout oder die
 *          Groesse einiger globaler Variablen geaendert, so muss auch hier
 *          geaendert werden!
 *
 */


#if defined(__GNUC__) && defined(atarist)

#  ifdef BIG

__asm__("
.text
	.even
.globl _getavail
_getavail:
"
#ifdef REG_A5
"	movel a5,a0 "
#else
"	movel _zzzaa,a0 "
#endif
"
	movel _avail,d0		| p = avail
	jeq   L_growavail

	movel d0,d1
	asll  #3,d1
	movel a0@(d1:l),_avail
	jra   L_getavailend

L_growavail:
	movel _memend,d0	| p = memend
	cmpl  _memmax:l,d0
	jge   L_growdown

	addql #1,d0		| kann nicht groesser als 65534 werden
	movel d0,_memend
	jra   L_beforeend

L_growdown:
	subql #1,_himemmin
	movel _himemmin,d0
	cmpl  _lomemmax:l,d0
	jgt   L_beforeend

	jbsr  _runaway
	movel _memmax,a1
	addqw #1,a1
	movel a1,sp@-
	pea   3:w
	jbsr  _overflow
	| Not reached

L_beforeend:
	movel d0,d1
	asll  #3,d1

L_getavailend:
	clrl  a0@(d1:l)
	addql #1,_dynused
	rts
");

#  else

__asm__("
.text
	.even
.globl _getavail
_getavail:
"
#ifdef REG_A5
"	movel a5,a0 "
#else
"	movel _zzzaa,a0 "
#endif
"
	moveq #0,d0
	movew _avail,d0		| p = avail
	jeq   L_growavail

	movel d0,d1
	asll  #2,d1
	movew a0@(d1:l),_avail
	jra   L_getavailend

L_growavail:
	movew _memend,d0	| p = memend
	cmpl  _memmax:l,d0
	jge   L_growdown

	addqw #1,d0		| kann nicht groesser als 65534 werden
	movew d0,_memend
	jra   L_beforeend

L_growdown:
	subqw #1,_himemmin
	movew _himemmin,d0
	movew _lomemmax,d1
	cmpw  d0,d1
	jcs   L_beforeend

	jbsr  _runaway
	movel _memmax,d1
	addql #1,d1
	movel d1,sp@-
	pea   3:w
	jbsr  _overflow
	| Not reached

L_beforeend:
	movel d0,d1
	asll  #2,d1

L_getavailend:
	clrw  a0@(d1:l)
	addql #1,_dynused
	rts
");

#  endif

#else  /* defined(__GNUC__) && defined(atarist) */

#  if !defined(AMIGA_ASM)

long_halfword STDARGS getavail ( void )
{ getavail_regmem
  register halfword p;

  p = avail;
  if ( p != 0 ) {
    avail = link( p /* avail */ );
    link ( p ) = 0;
  } else {
    if ( memend < memmax ) {
      incr ( memend );
      p = memend;
    } else {
      decr ( himemmin );
      p = himemmin;
      if ( himemmin <= lomemmax ) {
	runaway();
	overflow(3, memmax + 1 - memmin);
      }
    }
    link ( p ) = 0;
  }

#ifdef STAT
  incr ( dynused );
#endif /* STAT */
  return( (long_halfword)p );
}

#  endif

#endif


#if defined(__GNUC__) && defined(atarist)

#  ifdef BIG

__asm__("
.text
	.even
.globl _flushlist
_flushlist:
	movel sp@(4),d0		| Argument (long) p
	jeq   L_endflush

	subl  a1,a1		| count = 0
"
#ifdef REG_A5
"	movel a5,a0 "
#else
"	movel _zzzaa,a0 "
#endif
"
L_loop:
	addqw #1,a1		| count++

	movel d0,d1		| q = r (= p)
	asll  #3,d1
	movel a0@(d1:l),d0	| r = link(q)
	jne   L_loop

	movel _avail,a0@(d1:l)
	movel sp@(4),_avail

	movel a1,d0
	subl  d0,_dynused

L_endflush:
	rts
");

#  else

__asm__("
.text
	.even
.globl _flushlist
_flushlist:
	moveq #0,d0		| [hi-word(d0) = 0  for this function]
	movel d0,a1		| count = 0

	movew sp@(6),d0		| Argument (halfword) p
	jeq   L_endflush
"
#ifdef REG_A5
"	movel a5,a0 "
#else
"	movel _zzzaa,a0 "
#endif
"
L_loop:
	addqw #1,a1

	movel d0,d1	   	| q = r  ( = p )
	asll  #2,d1
	movew a0@(d1:l),d0 	| r = link(q)
	jne   L_loop

	movew _avail,a0@(d1:l)
	movew sp@(6),_avail

	movel a1,d0
	subl  d0,_dynused

L_endflush:
	rts
");

#  endif

#else /* defined(__GNUC__) && defined(atarist) */

#  if !defined(AMIGA_ASM)

void STDARGS flushlist ( long_halfword p )
{ flushlist_regmem

  if ( p != 0 ) {
    register long_halfword q, r;
    register long count = 0L;

    r = p;
    do {
#ifdef STAT
      /* decr ( dynused ); */
      count++;
#endif /* STAT */
      q = r;
      r = link ( q /* r */ );
    } while ( r != 0 );

    link ( q ) = avail;
    avail = p;
#ifdef STAT
    dynused -= count;
#endif /* STAT */
  }
}

#  endif

#endif



	/* take this out of getnode, because it's called only, if the
	 * lower part of mem grows by 1000 words at a time.
	 */
/*static*/ void STDARGS getnode_grow( void )
{ getnode_regmem

  if ( lomemmax + 2 < himemmin )
  if ( lomemmax + 2 <= membot + maxhalfword ) {
    register halfword p, q;
    register integer t;

    if ( himemmin - lomemmax >= 1998 )
      t = lomemmax + 1000;
    else
      t = lomemmax + 1 + ( himemmin - lomemmax ) / 2;
    p = llink ( rover );
    q = lomemmax;
    rlink ( p ) = q;
    llink ( rover ) = q;
    if ( t > membot + maxhalfword )
      t = membot + maxhalfword;
    rlink ( q ) = rover;
    llink ( q ) = p;
    link ( q ) = emptyflag;
    nodesize ( q ) = t - lomemmax;
    lomemmax = t;
    link ( lomemmax ) = 0;
    info ( lomemmax ) = 0;
    rover = q;
    return;
  }
  overflow(3, memmax + 1 - memmin);
}



#if defined(__GNUC__) && defined(atarist)

#  ifdef BIG

__asm__("
.text
	.even
.globl _getnode
_getnode:
	moveml d2/d3/d4,sp@-	| #0x3800

L_restart:
"
#ifdef REG_A5
"	movel a5,a0 "
#else
"	movel _zzzaa,a0 "
#endif
"
	lea   _rover,a1		| a1 = &rover
	movel a1@,d2		| p = rover	/* hi_word = 0 */

L_repeatloop:
	movel d2,d1		| q = p		/* hi_word = 0 */

L_whileloop:
	movel d1,d0
	asll  #3,d0		| Adr(q)

	addl  a0@(4,d0:l),d1	| q += nodesize(q)

	movel d1,d0		| /* hi_word(q) ist noch 0 */
	asll  #3,d0		| Adr(q)

	cmpl  #524287,a0@(d0:l)
	jne   L_exitwhile

	movel a0@(8,d0:l),d3	| t = rlink(q)

	cmpl  a1@,d1
	jne   L_notrover	| if( q = rover )
	movel d3,a1@		|   rover = t;
L_notrover:

	movel d3,d4
	asll  #3,d4		| Adr(t)

	movel a0@(12,d0:l),a0@(12,d4:l)	| llink(t) = llink(q)

	movel a0@(12,d0:l),d4	| d4 = llink(q)
	asll  #3,d4
	movel d3,a0@(8,d4:l)	| rlink(d4) = t

	jra   L_whileloop

L_exitwhile:
	movel sp@(16),d4	| !!! --->>>> hole Argument (long) s
	movel d1,d0
	subl  d4,d0		| r = q - s ,  /* q in d1 */

	movel d2,d3	| d3 = p + 1
	addql #1,d3
	cmpl  d0,d3	| if ( r > p + 1 )
	jge   L_too_short

	|
	| es passt rein:
	|
	movel d2,d3
	asll  #3,d3		| Adr(p)

	movel d0,d1
	subl  d2,d1		| d4 = r - p
	movel d1,a0@(4,d3:l)	| nodesize(p) = r - p

	movel d2,a1@		| rover = p
	|
	| r ist jetzt in d0, s in d4
	jra L_found

L_too_short:
	cmpl  d0,d2		| if ( r == p )
	jne   L_nextwhile

	movel d2,d3
	asll  #3,d3
	cmpl  a0@(8,d3:l),d2	| if ( rlink(p) != p )
	jeq   L_nextwhile

	movel a0@(8,d3:l),d1
	movel d1,a1@		| rover = rlink(p)

	movel a0@(12,d3:l),d2	| t = llink(p)

	asll  #3,d1
	movel d2,a0@(12,d1:l)	| llink(rover) = t

	asll  #3,d2
	movel a1@,a0@(8,d2:l)	| rlink(t) = rover
	|
	| r ist jetzt in d0, s in d4
	jra   L_found

L_nextwhile:
	movel d2,d3
	asll  #3,d3		| Adr(p)

	subl  d2,d1
	movel d1,a0@(4,d3:l)	| nodesize(p) = q - p

	movel a0@(8,d3:l),d2	| p = rlink(p)
	cmpl  a1@,d2
	jne   L_repeatloop

	cmpl  #1073741824,d4
	jne   L_growmem

	movel #524287,d0
	jra   L_exitgetnode

L_growmem:	| wird nur sehr selten durchlaufen, ...
	jbsr  _getnode_grow
	jra   L_restart


L_found:			| r ist in d0:l, s in d4:l
	movel d0,d1
	asll  #3,d1
	clrl  a0@(d1:l)		| link(r) = 0

	addl  d4,_varused

L_exitgetnode:
	moveml sp@+,d2/d3/d4	| #0x3c
	rts
");

#  else

__asm__("
.text
	.even
.globl _getnode
_getnode:
	moveml d2/d3/d4,sp@-	| #0x3800

L_restart:
"
#ifdef REG_A5
"	movel a5,a0 "
#else
"	movel _zzzaa,a0 "
#endif
"
	lea   _rover,a1		| a1 = &rover
/*
| Damit nicht laufend mit 'moveq #0,di ; movew X,di' X auf long gebracht
| werden muss, wurde mit Vorsicht(!) die Hi-Words von d1 und d2 auf 0
| gelassen. (Bei Aenderungen beachten, da sonst nur ??? herauskommt)
*/
	moveq #0,d2
	movew a1@,d2		| p = rover	/* hi_word = 0 */

L_repeatloop:
	movel d2,d1		| q = p		/* hi_word = 0 */

L_whileloop:
	movel d1,d0
	asll  #2,d0		| Adr(q)

	addw  a0@(2,d0:l),d1	| q += nodesize(q)

	movel d1,d0		| /* hi_word(q) ist noch 0 */
	asll  #2,d0		| Adr(q)

	cmpw  #65535,a0@(d0:l)
	jne   L_exitwhile

	moveq #0,d3
	movew a0@(4,d0:l),d3	| t = rlink(q)

	cmpw  a1@,d1
	jne   L_notrover	| if( q = rover )
	movew d3,a1@		|   rover = t;
L_notrover:

	movel d3,d4
	asll  #2,d4		| Adr(t)

	movew a0@(6,d0:l),a0@(6,d4:l)	| llink(t) = llink(q)

	moveq #0,d4
	movew a0@(6,d0:l),d4	| d4 = llink(q)
	asll  #2,d4
	movew d3,a0@(4,d4:l)	| rlink(d4) = t

	jra   L_whileloop

L_exitwhile:
	movel sp@(16),d4	| !!! --->>>> hole Argument (long) s
	movel d1,d0
	subl  d4,d0		| r = q - s ,  /* q in d1 */

	movel d2,d3	| d3 = p + 1
	addql #1,d3
	cmpl  d0,d3	| if ( r > p + 1 )
	jge   L_too_short

	|
	| es passt rein:
	|
	movel d2,d3
	asll  #2,d3		| Adr(p)

	movel d0,d1
	subl  d2,d1		| d4 = r - p
	movew d1,a0@(2,d3:l)	| nodesize(p) = r - p

	movew d2,a1@		| rover = p
	|
	| r ist jetzt in d0, s in d4
	jra   L_found

L_too_short:
	cmpl  d0,d2		| if ( r == p )
	jne   L_nextwhile

	movel d2,d3
	asll  #2,d3
	cmpw  a0@(4,d3:l),d2	| if ( rlink(p) != p )
	jeq   L_nextwhile

	moveq #0,d1
	movew a0@(4,d3:l),d1
	movew d1,a1@		| rover = rlink(p)

	movew a0@(6,d3:l),d2	| t = llink(p)

	asll  #2,d1
	movew d2,a0@(6,d1:l)	| llink(rover) = t

	asll  #2,d2
	movew a1@,a0@(4,d2:l)	| rlink(t) = rover
	|
	| r ist jetzt in d0, s in d4
	jra   L_found

L_nextwhile:
	movel d2,d3
	asll  #2,d3		| Adr(p)

	subl  d2,d1
	movew d1,a0@(2,d3:l)	| nodesize(p) = q - p

	movew a0@(4,d3:l),d2	| p = rlink(p)
	cmpw  a1@,d2
	jne   L_repeatloop

	cmpl  #1073741824,d4
	jne   L_growmem

	movel #65535,d0
	jra   L_exitgetnode

L_growmem:	| wird nur sehr selten durchlaufen, ...
	jbsr  _getnode_grow
	jra   L_restart


L_found:			| r ist in d0:l, s in d4:l
	movel d0,d1
	asll  #2,d1
	clrw  a0@(d1:l)		| link(r) = 0

	addl  d4,_varused

L_exitgetnode:
	moveml sp@+,d2/d3/d4	| #0x3c
	rts
");

#  endif

#else

#  if !defined(AMIGA_ASM)

  long_halfword	STDARGS	/* produces better code in the caller function */
getnode ( integer s )
{ getnode_regmem
  register integer r;

lab20:
  { register halfword p;
    register halfword q;
    register halfword *roverPTR = &rover;
#define rover  (*roverPTR)

  p = rover;
  do {
    q = p;
    while(1) {
      register halfword r;

      q += nodesize( q );

      if( ! isempty( q ) )
	break;

      r = rlink ( q );
      if ( q == rover )
        rover = r;
      llink ( r ) = llink ( q );
      rlink ( llink ( q ) ) = r;
    }
    r = q - s;
    if ( r > toint ( p + 1 ) ) {
      nodesize ( p ) = r - p;
      rover = p;
      goto lab40;
    }
    if ( r == p ) {
      if ( p != rlink ( p ) ) {
	q = llink ( p );
	p /* rover */ = rlink ( p );
	rlink ( q ) = p /* rover */;
	llink ( p /* rover */ ) = q;
	rover = p;
	goto lab40;
      }
    }
    nodesize ( p ) = q - p;
    p = rlink ( p );
  } while ( p != rover );

  if ( s == 1073741824L )
    return(maxhalfword);

#undef rover
  }

  getnode_grow();
  goto lab20;

lab40:
#ifdef STAT
  varused += s;
#endif /* STAT */
  link ( r ) = 0;
  return( (long_halfword)r );
}

#  endif

#endif



#if defined(__GNUC__) && defined(atarist)

#  ifdef BIG

__asm__("
.text
	.even
.globl _freenode
_freenode:
"
#ifdef REG_A5
"	movel a5,a0 "
#else
"	movel _zzzaa,a0 "
#endif
"
	movel sp@(4),d0		| Argument (long) p

	movel d0,d1
	asll  #3,d1
	lea   a0@(d1:l),a1	| Adr. von p

	movel sp@(8),d1		| Argument (long) s

	subl  d1,_varused	| varused -= s

	movel #524287,a1@+	| link(p) = emptyflag
	movel d1,a1@+		| nodesize(p) = s

	movel _rover,d1
	movel d1,a1@+		| rlink(p) = rover

	movel d2,sp@-

	movel d1,d2
	asll  #3,d2
	movel a0@(12,d2:l),d1	| q = llink(rover)
	movel d0,a0@(12,d2:l)	| llink(rover) = p

	movel sp@+,d2

	movel d1,a1@		| llink(p) = q
	asll  #3,d1
	movel d0,a0@(8,d1:l)	| rlink(q) = p
	rts
");

#  else

__asm__("
.text
	.even
.globl _freenode
_freenode:
"
#ifdef REG_A5
"	movel a5,a0 "
#else
"	movel _zzzaa,a0 "
#endif
"
	moveq #0,d0
	movew sp@(6),d0		| Argument (halfword) p

	movel d0,d1
	asll  #2,d1
	lea   a0@(d1:l),a1	| Adr. von p

	moveq #0,d1
	movew sp@(10),d1	| Argument (halfword) s

	subl  d1,_varused	| varused -= s

	movew #65535,a1@+	| link(p) = emptyflag
	movew d1,a1@+		| nodesize(p) = s

	movew _rover,d1
	movew d1,a1@+		| rlink(p) = rover

	movel d2,sp@-

	movel d1,d2
	asll  #2,d2
	movew a0@(6,d2:l),d1	| q = llink(rover)
	movew d0,a0@(6,d2:l)	| llink(rover) = p

	movel sp@+,d2

	movew d1,a1@		| llink(p) = q
	asll  #2,d1
	movew d0,a0@(4,d1:l)	| rlink(q) = p
	rts
");

#  endif

#else /* defined(__GNUC__) && defined(atarist) */

#  if !defined(AMIGA_ASM)

void STDARGS freenode ( long_halfword p, long_halfword s )
{ freenode_regmem

#ifdef STAT
  varused -= s;
#endif /* STAT */
  nodesize ( p ) = s;
  link ( p ) = emptyflag;

  { register halfword q;
    register halfword r_rover = rover;	/* (br) constant */

  q = llink ( r_rover );
  rlink ( p ) = r_rover;
  llink ( p ) = q;
  rlink ( q ) = p;
  llink ( r_rover ) = p;
  }
}

#  endif

#endif

/* -- end -- */
