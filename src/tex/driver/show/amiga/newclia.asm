
	include "exec/types.i"
	include "libraries/dosextens.i"
	


G_LOADDEVICE	equ	112
G_FINDDEVICE	equ	124

CALL_GV	macro
	moveq	#\1,d0
	jsr	-28(a6)
	endm


	xdef	_NewConsole


	section "text",code

_NewConsole
	movem.l	d2/a2/a6,-(sp)

	move.l	16(sp),d2
	movea.l	20(sp),a6

	lea	newName(pc),a0
	bsr.s	2$
	tst.l	d0
	bne.s	1$
	lea	conName(pc),a0
	bsr.s	2$

1$	movem.l	(sp)+,d2/a2/a6
	rts

2$	move.l	a0,d1
	lsr.l	#2,d1
	CALL_GV	G_FINDDEVICE
	tst.l	d0
	beq.s	3$

	movea.l	d0,a2
	adda.l	a2,a2
	adda.l	a2,a2

	move.l	d0,d1
	moveq	#-1,d0
	move.l	d0,dvi_Startup(a2)
	CALL_GV	G_LOADDEVICE
	clr.l	dvi_Startup(a2)

3$	rts


	cnop	0,4
newName	dc.b	6,'NEWCON'
	cnop	0,4
conName	dc.b	3,'CON'

	end
