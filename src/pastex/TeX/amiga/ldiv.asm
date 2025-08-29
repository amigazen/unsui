

	SECTION "ldiv.asm"

___udivsi3:
__udivsi3:
	move.l d2,a0		 
	move.l d3,a1

	moveq #0,d0		 
	move.l (8)(a7),d2		 
	beq udiv_err		 
	move.l (4)(a7),d1		 
	beq udiv_done		 
	cmp.l d2,d1		 
	bcs udiv_done
	moveq #31,d0		 
	tst.b (4)(a7)		 
	bne udiv_bit
	subq.l #8,d0
	tst.b (5)(a7)
	bne udiv_bit
	subq.l #8,d0
	tst.b (6)(a7)
	bne udiv_bit
	subq.l #8,d0
udiv_bit:
	btst d0,d1		 
	dbne d0,udiv_bit
	moveq.l #1,d3		 
	 
	 
	 
udiv_sl:
	btst d0,d2		 
	bne udiv_start		 
	add.l d3,d3		 
	add.l d2,d2		 
	bra udiv_sl
udiv_start:
	 
	 
	 
	moveq #0,d0		 
udiv_sr:
	cmp.l d2,d1		 
	bcs udiv_s
	or.l d3,d0		 
	sub.l d2,d1		 
	beq udiv_done		 
udiv_s:
	lsr.l #1,d2		 
	lsr.l #1,d3		 
	bne udiv_sr		 

udiv_done:
	 
	 
	move.l a0,d2		 
	move.l a1,d3
	rts
udiv_err:
	divs d2,d1		 
	bra udiv_done		 
 
 
___divsi3:
__divsi3:
	link a4,#-2		 
	clr.w (-2)(a4)		 

	move.l (12)(a4),d0	 
	bge divs1		 
	neg.l d0			 
	addq.w #1,(-2)(a4)	 
divs1:
	move.l d0,-(sp)		 
	move.l (8)(a4),d0		 
	bge divs2		 
	neg.l d0			 
	subq.w #1,(-2)(a4)	 
divs2:
	move.l d0,-(sp)		 
	jsr __udivsi3(pc)
	addq.l #8,sp		 
	tst.w (-2)(a4)		 
	beq divs3		 
	neg.l d0			 
divs3:
	tst.b (8)(a4)
	bpl divs4
	neg.l d1			 

 
divs4:
	unlk a4			 
	rts			 


;void  __asm ldiv1(register __d0 long int numer, 
;		   register __d1 long int denom,
;		   register __a2 ldiv_t *res);
 
_ldiv1:
	move.l	d1,-(a7)
	move.l	d0,-(a7)
	jsr	__divsi3(pc)
	addq.w	#8,a7
	move.l	d0,(a2)
	move.l	d1,4(a2)
	rts


	XDEF	_ldiv1


	END
