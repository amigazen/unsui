;
;	top_stub.asm
;
;	Gary Duncan; March 1996
;
;	This is simply a wrapper which saves and restores
;	registers around a C-function (my_switch).
;
;


	csect	text,0

	XDEF	_wrapper
	XDEF	_oldswitch

	XREF	_myswitch

_wrapper:
	movem.l	d0-d7/a0-a6,-(sp)

	jsr	_myswitch	

	movem.l	(sp)+,d0-d7/a0-a6	
	move.l	_oldswitch,-(sp)
	rts

_oldswitch:	dc.l	0

	end	
