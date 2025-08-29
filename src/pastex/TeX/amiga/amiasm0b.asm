	SECTION "amiasm0.asm"

	XREF	_zzzaa
	XREF	_avail
	XREF	_memend
	XREF	_memmax
	XREF	_himemmin
	XREF	_lomemmax
	XREF	_runaway		; function
	XREF	_overflow		; function
	XREF	_dynused
	XREF	_rover
	XREF	_varused
	XREF	_getnode_grow		; function


_getavail:

	movea.l	_zzzaa(a4),a0

	move.l	_avail(a4),d0		; p = avail
	beq.b	L_growavail

	move.l	d0,d1
	asl.l	#3,d1
	;move.l	a0_(d1:l),_avail
	move.l	00(a0,d1.l),_avail(a4)
	bra.b	L_getavailend

L_growavail:
	move.l _memend(a4),d0	; p = memend
	cmp.l _memmax(a4),d0
	bge.b L_growdown

	addq.l #1,d0		; kann nicht groesser als 65534 werden
	move.l d0,_memend(a4)
	bra.b L_beforeend

L_growdown:
	subq.l #1,_himemmin(a4)
	move.l _himemmin(a4),d0
	cmp.l _lomemmax(a4),d0
	bgt.b L_beforeend

	jsr _runaway(pc)
	move.l _memmax(a4),a1
	addq.w #1,a1
	move.l a1,-(sp)
	pea (3).w
	jsr _overflow(pc)
	; Not reached

L_beforeend:
	move.l d0,d1
	asl.l #3,d1

L_getavailend:
	;clr.l a0_(d1:l)
	clr.l 00(a0,d1.l)
	addq.l #1,_dynused(a4)
	rts


;halfword getavail ( void )
;{ getavail_regmem
;  register halfword p ;
;
;  p = avail ;
;  if ( p != 0 ) {
;    avail = link ( p /* avail */ ) ;
;    link ( p ) = 0 ;
;  } else {
;    if ( memend < memmax ) {
;      incr ( memend ) ;
;      p = memend ;
;    } else {
;      decr ( himemmin ) ;
;      p = himemmin ;
;      if ( himemmin <= lomemmax ) {
;	runaway () ;
;	overflow ( 298 , memmax + 1 - memmin ) ;
;      }
;    }
;    link ( p ) = 0 ;
;  }
;
;#ifdef STAT
;  incr ( dynused ) ;
;#endif /* STAT */
;  return(p) ;
;}



_flushlist:
	;move.l sp_(4),d0		; Argument (halfword) p
	move.l 4(sp),d0			; Argument (halfword) p
	beq.b L_endflush

	sub.l  a1,a1		; count = 0
	move.l _zzzaa(a4),a0

L_loop:
	addq.w #1,a1		; count++

	move.l d0,d1	   	; q = r  (= p)
	asl.l  #3,d1
	;move.l a0_(d1:l),d0 	; r = link(q)
	move.l 00(a0,d1.l),d0 	; r = link(q)
	bne.b L_loop

	;move.l _avail(a4),a0_(d1:l)
	move.l _avail(a4),00(a0,d1.l)
	;move.l sp_(4),_avail(a4)
	move.l 4(sp),_avail(a4)

	move.l a1,d0
	sub.l  d0,_dynused(a4)

L_endflush:
	rts

;void flushlist ( halfword p )
;{ flushlist_regmem
;
;  if ( p != 0 ) {
;    register unsigned long /* halfword */ q, r;
;    register int count = 0L;
;
;    r = p ;
;    do {
;#ifdef STAT
;      /* decr ( dynused ) ; */
;      count++;
;#endif /* STAT */
;      q = r ;
;      r = link ( q /* r */ ) ;
;    } while ( r != 0 );
;
;    link ( q ) = avail ;
;    avail = p ;
;#ifdef STAT
;    dynused -= count;
;#endif /* STAT */
;  }
;}


_getnode:
	movem.l d2/d3/d4,-(sp)	; #0x3800

L_restart:
	move.l _zzzaa(a4),a0

	lea _rover(a4),a1		; a1 = &rover
	;move.l a1_,d2		; p = rover
	move.l (a1),d2		; p = rover

L_repeatloop:
	move.l d2,d1		; q = p

L_whileloop:
	move.l d1,d0
	asl.l  #3,d0		; Adr(q)

	;add.l a0_(4,d0:l),d1	; q += nodesize(q)
	add.l 4(a0,d0.l),d1	; q += nodesize(q)

	move.l d1,d0		;
	asl.l  #3,d0		; Adr(q)

	;cmp.l #524287,a0_(d0:l)
	cmp.l #524287,00(a0,d0.l)
	bne.b L_exitwhile

	;move.l a0_(8,d0:l),d3	; t = rlink(q)
	move.l 8(a0,d0.l),d3	; t = rlink(q)

	cmp.l (a1),d1
	bne.b L_notrover		; if( q = rover )
	move.l d3,(a1)		;   rover = t;
L_notrover:

	move.l d3,d4
	asl.l  #3,d4		; Adr(t)

	;move.l a0_(12,d0:l),a0_(12,d4:l)	; llink(t) = llink(q)
	move.l 12(a0,d0.l),12(a0,d4.l)	; llink(t) = llink(q)

	;move.l a0_(12,d0:l),d4	; d4 = llink(q)
	move.l 12(a0,d0.l),d4	; d4 = llink(q)
	asl.l #3,d4
	;move.l d3,a0_(8,d4:l)	; rlink(d4) = t
	move.l d3,8(a0,d4.l)	; rlink(d4) = t

	bra.b L_whileloop

L_exitwhile:
	;move.l sp_(16),d4	; !!! --->>>> hole Argument (long) s
	move.l 16(sp),d4	; !!! --->>>> hole Argument (long) s
	move.l d1,d0
	sub.l d4,d0		; r = q - s ,  /* q in d1 */

	move.l d2,d3	; d3 = p + 1
	addq.l #1,d3
	cmp.l d0,d3	; if ( r > p + 1 )
	bge.b L_too_short

	;
	; es passt rein:
	;
	move.l d2,d3
	asl.l #3,d3		; Adr(p)

	move.l d0,d1
	sub.l d2,d1		; d4 = r - p
	;move.l d1,a0_(4,d3:l)	; nodesize(p) = r - p
	move.l d1,4(a0,d3.l)	; nodesize(p) = r - p

	move.l d2,(a1)		; rover = p
	;
	; r ist jetzt in d0, s in d4
	bra.b L_found

L_too_short:
	cmp.l d0,d2		; if ( r == p )
	bne.b L_nextwhile

	move.l d2,d3
	asl.l #3,d3
	;cmp.l a0_(8,d3:l),d2	; if ( rlink(p) != p )
	cmp.l 8(a0,d3.l),d2	; if ( rlink(p) != p )
	beq.b L_nextwhile

	;move.l a0_(8,d3:l),d1
	move.l 8(a0,d3.l),d1
	move.l d1,(a1)		; rover = rlink(p)

	;move.l a0_(12,d3:l),d2	; t = llink(p)
	move.l 12(a0,d3.l),d2	; t = llink(p)

	asl.l #3,d1
	;move.l d2,a0_(12,d1:l)	; llink(rover) = t
	move.l d2,12(a0,d1.l)	; llink(rover) = t

	asl.l #3,d2
	;move.l a1_,a0_(8,d2:l)	; rlink(t) = rover
	move.l (a1),8(a0,d2.l)	; rlink(t) = rover
	;
	; r ist jetzt in d0, s in d4
	bra.b L_found

L_nextwhile:
	move.l d2,d3
	asl.l #3,d3		; Adr(p)

	sub.l d2,d1
	;move.l d1,a0_(4,d3:l)	; nodesize(p) = q - p
	move.l d1,4(a0,d3.l)	; nodesize(p) = q - p

	;move.l a0_(8,d3:l),d2	; p = rlink(p)
	move.l 8(a0,d3.l),d2	; p = rlink(p)
	cmp.l (a1),d2
	bne.w L_repeatloop

	cmp.l #1073741824,d4
	bne.b L_growmem

	move.l #524287,d0
	bra.b L_exitgetnode

L_growmem:	; wird nur sehr selten durchlaufen, ...
	jsr _getnode_grow(pc)
	bra.w L_restart


L_found:			; r ist in d0:l, s in d4:l
	move.l d0,d1
	asl.l  #3,d1
	;clr.l a0_(d1:l)		; link(r) = 0
	clr.l  00(a0,d1.l)		; link(r) = 0

	add.l d4,_varused(a4)

L_exitgetnode:
	movem.l (sp)+,d2/d3/d4	; #0x3c
	rts

;halfword getnode ( integer s )
;{ getnode_regmem
;  register integer r;
;
;lab20:
;  { register halfword p;
;    register halfword q;
;    register halfword *roverPTR = &rover;
;#define rover  (*roverPTR)
;
;  p = rover ;
;  do {
;#if 0
;    q = p + nodesize ( p ) ;
;    while ( isempty ( q ) ) {
;      register halfword t;
;
;      t = rlink ( q ) ;
;      if ( q == rover )
;        rover = t ;
;      llink ( t ) = llink ( q ) ;
;      rlink ( llink ( q ) ) = t ;
;      q += nodesize ( q ) ;
;    }
;#else
;    q = p;
;    while(1) {
;      register halfword r;
;
;      q += nodesize ( q ) ;
;
;      if( ! isempty ( q ) )
;	break;
;
;      r = rlink ( q ) ;
;      if ( q == rover )
;        rover = r ;
;      llink ( r ) = llink ( q ) ;
;      rlink ( llink ( q ) ) = r ;
;    }
;#endif
;    r = q - s ;
;    if ( r > toint ( p + 1 ) ) {
;      nodesize ( p ) = r - p ;
;      rover = p ;
;      goto lab40 ;
;    }
;    if ( r == p ) {
;      if ( p != rlink ( p ) ) {
;#if 0
;	register integer t;
;
;	rover = rlink ( p ) ;
;	t = llink ( p ) ;
;	llink ( rover ) = t ;
;	rlink ( t ) = rover ;
;#else
;	q = llink ( p ) ;
;	p /* rover */ = rlink ( p ) ;
;	rlink ( q ) = p /* rover */ ;
;	llink ( p /* rover */ ) = q ;
;	rover = p;
;#endif
;	goto lab40 ;
;      }
;    }
;    nodesize ( p ) = q - p ;
;    p = rlink ( p ) ;
;  } while ( p != rover );
;
;  if ( s == 1073741824L )
;    return(maxhalfword);
;
;#undef rover
;  }
;
;  getnode_grow();
;  goto lab20;
;
;lab40:
;#ifdef STAT
;  varused += s ;
;#endif /* STAT */
;  link ( r ) = 0 ;
;  return(r);
;}


_freenode:
	move.l _zzzaa(a4),a0

	move.l 4(sp),d0		; Argument (halfword) p

	move.l d0,d1
	asl.l #3,d1
	;lea a0_(d1:l),a1	; Adr. von p
	lea 00(a0,d1.l),a1	; Adr. von p

	move.l 8(sp),d1		; Argument (halfword) s

	sub.l d1,_varused(a4)	; varused -= s

	move.l #524287,(a1)+	; link(p) = emptyflag
	move.l d1,(a1)+		; nodesize(p) = s

	move.l _rover(a4),d1
	move.l d1,(a1)+		; rlink(p) = rover

	move.l d2,-(sp)

	move.l d1,d2
	asl.l #3,d2
	;move.l a0_(12,d2:l),d1	; q = llink(rover)
	move.l 12(a0,d2.l),d1	; q = llink(rover)
	;move.l d0,a0_(12,d2:l)	; llink(rover) = p
	move.l d0,12(a0,d2.l)	; llink(rover) = p

	move.l (sp)+,d2

	move.l d1,(a1)		; llink(p) = q
	asl.l #3,d1
	;move.l d0,a0_(8,d1:l)	; rlink(q) = p
	move.l d0,8(a0,d1.l)	; rlink(q) = p
	rts

;void freenode ( halfword p, halfword s )
;{ freenode_regmem
;
;#ifdef STAT
;  varused -= s ;
;#endif /* STAT */
;  nodesize ( p ) = s ;
;  link ( p ) = emptyflag ;
;
;  { register halfword q ;
;    register halfword r_rover = rover;	/* (br) constant */
;
;  q = llink ( r_rover ) ;
;  rlink ( p ) = r_rover ;
;  llink ( p ) = q ;
;  rlink ( q ) = p ;
;  llink ( r_rover ) = p ;
;  }
;}

	XDEF	_getavail
	XDEF	_flushlist
	XDEF	_getnode
	XDEF	_freenode

	END
