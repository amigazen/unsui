*
* C initial startup procedure under AmigaDOS for usershell startup
*
* Use the following command line to make cusershell.o
* asm -u -ocusershell.o cusershell.a
*
* SAS/C cres-startup for 6.51, Modified by Pasi Ojala
*
* No parameters for usershell, no stack checking allowed!
* There is no CLI when we are run, unlike normal cres startup!
*

        INCLUDE        "exec/types.i"
        INCLUDE        "exec/alerts.i"
        INCLUDE        "exec/nodes.i"
        INCLUDE        "exec/lists.i"
        INCLUDE        "exec/ports.i"
        INCLUDE        "exec/libraries.i"
        INCLUDE        "exec/tasks.i"
        INCLUDE        "exec/memory.i"
        INCLUDE        "exec/execbase.i"
        INCLUDE        "libraries/dos.i"
        INCLUDE        "libraries/dosextens.i"
        INCLUDE        "workbench/startup.i"
        INCLUDE        "exec/funcdef.i"
        INCLUDE        "exec/exec_lib.i"
        INCLUDE        "libraries/dos_lib.i"

MEMFLAGS        EQU        MEMF_CLEAR+MEMF_PUBLIC
AbsExecBase     EQU        4

;;;
;;;   Stack map.
;;;
      OFFSET  0
           ds.b    4
savereg    ds.b    13*4
stackbtm   ds.b    4



; some usefull macros:

callsys macro
        CALLLIB _LVO\1
        endm

        xref        LinkerDB        * linker defined base value
        xref        _BSSBAS                * linker defined base of BSS
        xref        _BSSLEN                * linker defined length of BSS

        xref        RESLEN
        xref        RESBASE
        xref        NEWDATAL

*       library references

        section text,code

        xref    _main

start:
        movem.l d1-d6/a0-a6,-(a7)       * save registers

        lea     LinkerDB,a4             * load base register
        move.l  AbsExecBase.W,a6

        movem.l       a0-a2,-(a7)

        move.l        #RESLEN,d0
        move.l        #MEMFLAGS,d1
        callsys       AllocMem
        tst.l         d0
        bne.s         ok1
        movem.l       (a7)+,a0-a2
        bra.w         return

ok1:    move.l        d0,a0
        move.l        d0,a2             * a2 now has difference
        move.l        d0,a1
        move.l        #NEWDATAL,d0
        sub.l         #RESBASE,a4

                                        * copy data over
cpy:    move.l        (a4)+,(a0)+
        subq.l        #1,d0
        bne.s         cpy

                                        * a4 now points at number of relocs
        move.l        (a4)+,d0
reloc:  beq.s         nreloc
        move.l        a1,a0
        add.l         (a4)+,a0          * a0 now has add of reloc
        add.l         (a0),a2
        move.l        a2,(a0) 
        move.l        a1,a2             * restore offset
        subq.l        #1,d0
        bra.s         reloc

nreloc: move.l        a1,a4             * set up new base register
        add.l         #RESBASE,a4

        move.l        #RESLEN,realdatasize(a4)
        movem.l       (a7)+,a0-a2

        move.l        a6,SysBase(A4)
        movem.l       a7,_StackPtr(A4)  * Save stack ptr


*-----  clear any pending signals
        moveq         #0,d0
        move.l        #$00003000,d1
        callsys       SetSignal

        move.l        ThisTask(a6),A3

*------ attempt to open DOS library:
        lea           DOSName(PC),A1
        moveq.l       #0,D0
        callsys       OpenLibrary
        move.l        D0,DOSBase(A4)
        bne.s         ok2
        moveq.l       #100,d0
        bra.w         exit2

ok2:
        move.l        pr_CurrentDir(A3),__curdir(A4)


*=============================================
*------ common code --------
*=============================================

main
        jsr        _main(PC)
        move.l     4(SP),d0                  * extract return code

exit2:

*-- Save Return Code
        move.l     _StackPtr(a4),a2
        move.l     d0,-(a2)

        movea.l    a2,a7                     * restore stack ptr

exit4:
        move.l     DOSBase(A4),a1
        callsys    CloseLibrary              * close Dos library

        move.l     realdatasize(a4),d0
        move.l     a4,a1
        sub.l      #RESBASE,a1
        callsys    FreeMem

        move.l     (a7)+,d0

return:
        movem.l    (a7)+,d1-d6/a0-a6
        rts

DOSName dc.b    'dos.library',0


        section __MERGED,BSS

        xdef    NULL,SysBase,DOSBase

        xref    __curdir
        xref    _StackPtr


NULL          ds.b    4
DOSBase       ds.b    4
SysBase       ds.b    4


realdatasize ds.b    4                   * size of memory allocated for data

             END
