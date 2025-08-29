	SECTION "apack.asm"
 
**************************************************************** 
* 
*  Copyright 1988 by CREATIVE FOCUS.  This code is freely 
*  distributable as long as this notice is retained and no 
*  other conditions are imposed upon its redistribution. 
* 
* 
*  APACK.ASM --  
* 
*  A fully compatible replacement for Electronic Arts' PACKER.C 
*  routine.  Converts data according to the IFF ILBM cmpByteRun1 
*  compression protocol: 
* 
*     control bytes: 
* 
*        n =  0.. 127:   followed by n+1 bytes of data; 
*        n = -1..-127:   followed by byte to be repeated -n+1 times; 
*        n =     -128:   don't do no nada. 
* 
*     calling format: 
* 
*        long packrow(from, too, amt) 
*           char **from, /* pointer to source data pointer */ 
*                **too;  /* pointer to destination data pointer */ 
*           long amt;    /* number of bytes to compress */ 
* 
*        return(number of bytes written to destination); 
* 
*     effects: 
* 
*         *from = *from + amt, and *too = *too + return; 
*         return is "smart," that is, not greater than 
*         MaxPackedSize = amt + ((amt+127) >> 7). 
* 
*     By commenting out CHECK (below) you disable checking for runs 
*     exceeding 128 bytes.  That CHECK is not needed if you are sure 
*     the amt to be compressed is always 128 or less. 
* 
*  !!! DISCLAIMER !!!  You use this code entirely at your own 
*  risk.  I don't warrantee its fitness for any purpose.  I 
*  can't even guarantee the accuracy of anything I've said 
*  about it, though I've tried my damndest to get it right. 
*  I may, in fact, be completely out of my tiny little mind :-). 
* 
*  That being said, I can be reached for questions, comments, 
*  or concerns at: 
* 
*        Dr. Gerald Hull 
*        CREATIVE FOCUS 
*        12 White Street 
*        Binghamton, N.Y.  13901 
*        (607) 648-4082 
* 
*        bix:    ghull 
*        PLink:  DRJERRY 
* 
*************************************************************** 
 
      xdef  _Apackrow 
 
PT    equr  a0                -> beginning of replicate run (if any) 
IX    equr  a1                -> end+1 of input line 
IP    equr  a2                -> beginning of literal run (if any) 
IQ    equr  a3                -> end+1 of lit and/or rep run (if any) 
OP    equr  a4                -> end+1 of output line current pos 
FP    equr  a6                frame pointer 
SP    equr  a7                stack pointer 
 
RT    equr  d0                return value 
MX    equr  d1                check for maximum run = MAX 
AM    equr  d2                amount 
CH    equr  d3                character 
 
REGS  reg   AM/CH/IP/IQ/OP 
 
FRM   equ   8                 input line address 
TOO   equ   12                output line address 
AMT   equ   16                length of input line 
 
MAX   equ   128               maximum encodable output run 
* CHECK equ   1                 turns on maximum row checking 
 
 
_Apackrow 
 
 
***************     CASE 0:   GRAB PARAMS & INITIALIZE 
CAS0 
      link     FP,#0 
      movem.l  REGS,-(SP) 
      movea.l  FRM(FP),IP 
      movea.l  (IP),IP        IP = *from 
      movea.l  IP,IQ          IQ = IP 
      movea.l  IQ,IX 
      adda.l   AMT(FP),IX     IX = IP + amt 
      movea.l  TOO(FP),OP 
      movea.l  (OP),OP        OP = *too 
 
 
***************     CASE 1:   LITERAL RUN 
CAS1 
      movea.l  IQ,PT          adjust PT (no replicates yet!) 
      move.b   (IQ)+,CH       grab character 
      cmpa.l   IQ,IX          if input is finished 
      beq.s    CAS5              branch to case 5 
 
      ifd      CHECK 
      move.l   IQ,MX 
      sub.l    IP,MX 
      cmpi     #MAX,MX        if run has reached MAX 
      beq.s    CAS6              branch to case 6 
      endc 
 
      cmp.b    (IQ),CH        if next character != CH 
      bne.s    CAS1              stay in case 1 
 
*                             else fall into case 2 
 
 
***************     CASE 2:   AT LEAST 2 BYTE REPEAT 
CAS2 
      move.b   (IQ)+,CH       grab character 
      cmpa.l   IQ,IX          if input is finished 
      beq.s    CAS7              branch to case 7 
 
      ifd      CHECK 
      move.l   IQ,MX 
      sub.l    IP,MX 
      cmpi     #MAX,MX        if run has reached MAX 
      beq.s    CAS6              branch to case 6 
      endc 
 
      cmp.b    (IQ),CH        if next character != CH 
      bne.s    CAS1              branch to case 1 
 
*                             else fall into case 3 
 
 
***************     CASE 3:   REPLICATE RUN 
CAS3 
      move.b   (IQ)+,CH       grab character 
      cmpa.l   IQ,IX          if input is finished 
      beq.s    CAS7              branch to case 7 
 
      ifd      CHECK 
      move.l   IQ,MX 
      sub.l    PT,MX 
      cmpi     #MAX,MX        if run has reached MAX 
      beq.s    CAS4              branch to case 4 
      endc 
 
      cmp.b    (IQ),CH        if next character = CH 
      beq.s    CAS3              stay in case 3 
 
*                             else fall into case 4 
 
 
***************     CASE 4:   LIT AND/OR REP DUMP & CONTINUE 
CAS4 
      move.l   PT,AM 
      sub.l    IP,AM          AM = PT - IP 
*                             if no literal run 
      beq.s    C41               branch to replicate run 
 
      subq     #1,AM          AM = AM - 1 
      move.b   AM,(OP)+       output literal control byte 
 
C40   move.b   (IP)+,(OP)+    output literal run 
      dbra     AM,C40 
 
C41   move.l   PT,AM 
      sub.l    IQ,AM          AM = PT - IQ (negative result!) 
      addq     #1,AM          AM = AM + 1 
      move.b   AM,(OP)+       output replicate control byte 
      move.b   CH,(OP)+       output repeated character 
      movea.l  IQ,IP          reset IP 
      bra.s    CAS1           branch to case 1 (not done) 
 
 
***************     CASE 5:   LITERAL DUMP & QUIT 
CAS5 
      move.l   IQ,AM 
      sub.l    IP,AM          AM = IQ - IP (positive result > 0) 
      subq     #1,AM          AM = AM - 1 
      move.b   AM,(OP)+       output literal control byte 
 
C50   move.b   (IP)+,(OP)+    output literal run 
      dbra     AM,C50 
 
      bra.s    CAS8           branch to case 8 (done) 
 
 
      ifd      CHECK 
 
***************     CASE 6:   LITERAL DUMP & CONTINUE 
CAS6 
      move.l   IQ,AM 
      sub.l    IP,AM          AM = IQ - IP (positive result > 0) 
      subq     #1,AM          AM = AM - 1 
      move.b   AM,(OP)+       output literal control byte 
 
C60   move.b   (IP)+,(OP)+    output literal run 
      dbra     AM,C60 
 
      bra      CAS1           branch to case 1 (not done) 
 
      endc 
 
 
***************     CASE 7:   LIT AND/OR REP DUMP & FINISH 
CAS7 
      move.l   PT,AM 
      sub.l    IP,AM          AM = PT - IP (positive result > 0) 
*                             if no literal run 
      beq.s    C71               branch to replicate run 
 
      subq     #1,AM          AM = AM - 1 
      move.b   AM,(OP)+       output literal control byte 
 
C70   move.b   (IP)+,(OP)+    output literal run 
      dbra     AM,C70 
 
C71   move.l   PT,AM 
      sub.l    IQ,AM          AM = PT - IQ (negative result) 
      addq     #1,AM          AM = AM + 1 
      move.b   AM,(OP)+       output replicate control byte 
      move.b   CH,(OP)+       output repeated character 
 
*                             fall into case 8 
 
 
***************     CASE 8:   ADJUST PARAMS & RETURN VALUE 
CAS8 
      movea.l  FRM(FP),PT     PT = **from 
      move.l   IQ,(PT)        *from = *from + amt 
      movea.l  TOO(FP),PT     PT = **too 
 
      move.l   OP,RT 
      sub.l    (PT),RT       return = OP - *too  
 
      move.l   OP,(PT)       *too = *too + return 
      movem.l  (SP)+,REGS 
      UNLK     FP 
      rts 
 
      end


