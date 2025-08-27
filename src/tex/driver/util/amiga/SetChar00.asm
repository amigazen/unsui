              SECTION      TEXT,CODE
__code:
@SetChar:
;    1:#ifndef SLOW
;    2:
;    3:#include "defines.h"
;    4:
;    5:#include <stdio.h>
;    6:
;    7:#ifdef AMIGA
;    8:# ifdef DISPLAY
;    9:#  include <intuition/intuition.h>
;   10:   extern struct RastPort 	 myRastPort;
;   11:# endif
;   12:#endif
;   13:
;   14:#ifdef AZTEC_C
;   15:#  include "functions.h"
;   16:#endif
;   17:
;   18:
;   19:#include "globals.h"
;   20:#include "bitmap.h"
;   21:#include "commands.h"
;   22:#include "flmt.h"
;   23:#include "new_font.h"
;   24:
;   25:#include "globals.i"
;   26:#include "fast_cp.i"
;   27:#include "dvihand.i"
;   28:#include "unpack.i"
;   29:#include "new_font.i"		/* fuer Load_really() */
;   30:
;   31:#ifdef DISPLAY
;   32:#  include <clib/graphics_protos.h>
;   33:#endif
;   34:
;   35:
;   36:#define PixRound(x,conv)	(long)(((x) + ((conv) >> 1)) / (conv))
;   37:
;   38:#define BITSPERLONG	32
;   39:#define BITSPERWORD	16
;   40:#define WORD_WIDTH	16
;   41:
;   42:extern long  hconv, vconv;            /* converts DVI units to pixels       */
;   43:extern long  h;                       /* current horizontal position        */
;   44:extern long  hh;                      /* current h on device                */
;   45:extern long  v;                       /* current vertical position          */
;   46:extern long  vv;                      /* current v on device                */
;   47:
;   48:
;   49:
;   50:extern int		DeBug;
;   51:extern struct bitmap	map;
;   52:extern long		upper_limit;
;   53:extern long		lower_limit;
;   54:
;   55:/* extern struct font_entry *cfontptr; */
;   56:
;   57:extern struct Font *cfontptr;
;   58:
;   59:
;   60:
;   61:
;   62:/*-->SetChar*/
;   63:/**********************************************************************/
;   64:/*****************************  SetChar  ******************************/
;   65:/**********************************************************************/
;   66:
;   67:#if !defined(USE_ASM_SETCHAR) || !defined(DISPLAY)
;   68:
;   69:void SetChar(long pc, int command)
;   70:{
              SUB.W          #$1c,A7                  ;9efc 001c 
              MOVEM.L        D2-D7/A2-A3/A5-A6,-(A7)
___SetChar__1:
;   71:  struct Char			*ptr;
;   72:
;   73:  register unsigned long 	reg;
;   74:  unsigned short		*char_adr;
;   75:  unsigned short		*start_adr, *adr;
;   76:  long				nr_sh, length_row;
;   77:  long				c, l;
;   78:
;   79:  long			x, y;        /* upper left corner (pixels)           */
;   80:  long			cp_h, w;     /* height / width of character (pixels) */
;   81:  /* long		*p; */          /* pointer to bitmap */
;   82:  unsigned short	*p;          /* pointer to bitmap */
;   83:  struct Chars		*cd;
;   84:
;   85:  long			words, lmin, lmax;
;   86:
;   87:
;   88:  cd = &(cfontptr->common->ch[pc]);
              MOVE.L         D0,D7                    ;2e00 
              MOVE.L         D7,D2                    ;2407 
              ASL.L          #$2,D2                   ;e582 
              ADD.L          D7,D2                    ;d487 
              ADD.L          D2,D2                    ;d482 
              MOVE.L         _cfontptr(A4),A1          ;226c 0000 
              MOVE.L         $414(A1),A0              ;2069 0414 
              ADD.L          D2,A0                    ;d1c2 
              LEA            $24(A0),A5               ;4be8 0024 
;   89:
;   90:  if (cd->packed_data == -1) {
              MOVE.L         A5,A0                    ;204d 
              MOVE.L         D0,$38(A7)               ;2f40 0038 
              MOVE.L         D1,$3c(A7)               ;2f41 003c 
              MOVE.L         A0,$34(A7)               ;2f48 0034 
              MOVE.L         #$ff,D0                  ;70ff        (hes)
              CMP.L          $2(A5),D0                ;b0ad 0002 
              BNE.B          ___SetChar__9            ;6652 
___SetChar__2:
;   91:    /* character not in font:
;   92:     * add tfm width to h and set hh = PixRound(h)
;   93:     */
;   94:    if (command <= SET4) {    	/* SET command (or PUT command) ? */
              MOVEQ.L        #$7c,D0                  ;707c 
              NOT.B          D0                       ;4600 
              CMP.L          D0,D1                    ;b280 
              BGT.W          ___SetChar__44           ;6e00 021e 
___SetChar__3:
;   95:	if (!cfontptr->ctfmw_valid ) {
              MOVEQ.L        #$0,D1                   ;7200 
              NOT.B          D1                       ;4601 
              AND.L          D1,D7                    ;ce81 
              ASL.L          #$2,D7                   ;e587 
              TST.B          $418(A1)                 ;4a29 0418 
              BNE.B          ___SetChar__5            ;6614 
___SetChar__4:
;   96:	   h += scalewidth(cfontptr->common->fnt_group->tfmw[(int)pc&255],
              MOVE.L         $414(A1),A6              ;2c69 0414 
              MOVE.L         (A6),A0                  ;2056 
;   97:			 	  cfontptr->space_faktor);
              MOVE.L         $0(A0,D7.L),D0           ;2030 7800 
              MOVE.L         $8(A1),D1                ;2229 0008 
              JSR            @scalewidth(PC)          ;4eba 0000 
              BRA.B          ___SetChar__6            ;6006 
___SetChar__5:
;   98:        }
;   99:	else {
;  100:	  h += cfontptr->ctfmw[(int)pc&255];
              ADD.L          D7,A1                    ;d3c7 
              MOVE.L         $14(A1),D0               ;2029 0014 
___SetChar__6:
              ADD.L          D0,_h(A4)                 ;d1ac 0000 
___SetChar__7:
;  101:	}
;  102:	hh = PixRound(h, hconv);
              MOVE.L         _hconv(A4),D1             ;222c 0000 
              ASR.L          #$1,D1                   ;e281 
              MOVE.L         _h(A4),D0                 ;202c 0000 
              ADD.L          D1,D0                    ;d081 
              MOVE.L         _hconv(A4),D1             ;222c 0000 
              JSR            __CXD33(PC)               ;4eba 0000 
              MOVE.L         D0,_hh(A4)                ;2940 0000 
___SetChar__8:
;  103:    }
;  104:    return ;  /* kein Shift */
              BRA.W          ___SetChar__44           ;6000 01d6 
___SetChar__9:
;  105:  }
;  106:
;  107:  if ((ptr = cd->unpacked) == NULL) {
              MOVE.L         $6(A5),A3                ;266d 0006 
              MOVE.L         A3,D0                    ;200b 
              BNE.B          ___SetChar__15           ;6634 
___SetChar__10:
;  108:    if (cfontptr->common->fnt_status == FNT_DEFINED ||
              MOVE.L         _cfontptr(A4),A1          ;226c 0000 
              MOVE.L         $414(A1),A0              ;2069 0414 
              MOVEQ.L        #$3,D0                   ;7003 
              CMP.B          $a26(A0),D0              ;b028 0a26 
              BEQ.B          ___SetChar__12           ;6708 
___SetChar__11:
;  109:        cfontptr->common->fnt_status == FNT_FOUND) {
              MOVEQ.L        #$2,D0                   ;7002 
              CMP.B          $a26(A0),D0              ;b028 0a26 
              BNE.B          ___SetChar__13           ;6606 
___SetChar__12:
;  110:      /* Font noch nicht geladen?? */
;  111:      /* Die Abfrage kommt nur, wenn der Buchstabe nicht eh schon gefunden	*/
;  112:      /* wird, also wenn der Char sicher noch nie ausgepackt wurde!		*/
;  113:
;  114:      Load_really(cfontptr);		/* nun wird er geladen... */
              MOVE.L         A1,A0                    ;2049 
              JSR            @Load_really(PC)         ;4eba 0000 
___SetChar__13:
;  115:    }
;  116:    unpack_char(cfontptr, pc);
              MOVE.L         D7,D0                    ;2007 
              EXT.L          D0                       ;48c0 
              MOVE.L         _cfontptr(A4),A0          ;206c 0000 
              JSR            @unpack_char(PC)         ;4eba 0000 
;  117:    if ((ptr = cd->unpacked) == NULL) {
              MOVE.L         $6(A5),A3                ;266d 0006 
              MOVE.L         A3,D0                    ;200b 
              BEQ.W          ___SetChar__37           ;6700 0124 
___SetChar__14:
___SetChar__15:
;  118:      /* auspacken hat nicht geklappt, aber die Breite des Chars ist bekannt */
;  119:      goto sh_return;
;  120:    }
;  121:  }
;  122:
;  123:#if 0
;  124:  x = (long)(PixRound(h, hconv)-ptr->xOffset) + hoffset;
;  125:  y = (long)(PixRound(v, vconv)-ptr->yOffset) + voffset;
;  126:#endif
;  127:
;  128:#ifdef DISPLAY
;  129:  x = hh - ptr->xOffset + hoffset;
              MOVE.L         _hh(A4),D0                ;202c 0000 
              SUB.L          $4(A3),D0                ;90ab 0004 
              ADD.L          _hoffset(A4),D0           ;d0ac 0000 
;  130:  y = vv - ptr->yOffset + voffset;
              MOVE.L         _vv(A4),D7                ;2e2c 0000 
              SUB.L          $8(A3),D7                ;9eab 0008 
              ADD.L          _voffset(A4),D7           ;deac 0000 
;  131:#else
;  132:  if (landscape) {
;  133:    // wird spaeter noch gedreht, also jetzt etwas seltsame Offsets
;  134:    x = hh - ptr->xOffset + hconvresolution;	// plus 1 inch
;  135:    y = vv - ptr->yOffset + hoffset;
;  136:  }
;  137:  else {
;  138:    x = hh - ptr->xOffset + hoffset;
;  139:    y = vv - ptr->yOffset + voffset;
;  140:  }
;  141:#endif
;  142:  w = (long)ptr->width;					/* char width */
              MOVEQ.L        #$0,D1                   ;7200 
              MOVE.W         (A3),D1                  ;3213 
;  143:  cp_h = (long)ptr->height;				/* char height */
              MOVEQ.L        #$0,D6                   ;7c00 
              MOVE.W         $2(A3),D6                ;3c2b 0002 
;  144:
;  145:  p = (unsigned short *)(ptr+1);
;  146:
;  147:  /* Buchstaben am Rand werden vollstaendig weggelassen */
;  148:  if (x < 0L || x+w >= map.width) {
              MOVE.L         D0,$30(A7)               ;2f40 0030 
              MOVE.L         D1,$2c(A7)               ;2f41 002c 
              TST.L          D0                       ;4a80 
              BMI.W          ___SetChar__37           ;6b00 00f4 
___SetChar__16:
              ADD.L          D0,D1                    ;d280 
              MOVE.L         _map(A4),D3               ;262c 0000 
              CMP.L          D3,D1                    ;b283 
              BGE.W          ___SetChar__37           ;6c00 00e8 
___SetChar__17:
___SetChar__18:
;  149:      goto sh_return;
;  150:  }
;  151:#ifdef BERND
;  152:  if (y >= lower_limit || y-cp_h < upper_limit) {
;  153:  			  ^^^^^^ falsche Richtung!
;  154:      goto sh_return;
;  155:  }
;  156:#else
;  157:  /* Buchstaben die ganz unten oder oben heraus sind werden weggelassen */
;  158:  if (y >= lower_limit || y+cp_h < upper_limit) {
              MOVE.L         _lower_limit(A4),D2       ;242c 0000 
              CMP.L          D2,D7                    ;be82 
              BGE.W          ___SetChar__37           ;6c00 00de 
___SetChar__19:
              MOVE.L         D7,D5                    ;2a07 
              ADD.L          D6,D5                    ;da86 
              MOVE.L         _upper_limit(A4),D3       ;262c 0000 
              CMP.L          D3,D5                    ;ba83 
              BLT.W          ___SetChar__37           ;6d00 00d0 
___SetChar__20:
___SetChar__21:
;  159:      goto sh_return;
;  160:  }
;  161:#endif
;  162:
;  163:  lmin = 0; lmax = cp_h;
              CLR.L          $40(A7)                  ;42af 0040 
              MOVE.L         D6,D4                    ;2806 
;  164:
;  165:  /* Buchstaben die nach unten herausragt wird abgeschnitten */
;  166:  if (y+cp_h >= lower_limit) {
              CMP.L          D2,D5                    ;ba82 
              BLT.B          ___SetChar__23           ;6d04 
___SetChar__22:
;  167:    lmax = lower_limit - y /*- 1*/;	/* -1 wird nicht benoetigt, da <lmax! */
              SUB.L          D7,D2                    ;9487 
              MOVE.L         D2,D4                    ;2802 
___SetChar__23:
;  168:  }
;  169:  /* Buchstaben die nach oben herausragen werden ebenfalls abgeschnitten */
;  170:  if (y < upper_limit) {
              MOVE.L         _upper_limit(A4),D2       ;242c 0000 
              CMP.L          D2,D7                    ;be82 
              BGE.B          ___SetChar__25           ;6c08 
___SetChar__24:
;  171:    lmin = upper_limit - y;
              MOVE.L         D2,D3                    ;2602 
              SUB.L          D7,D3                    ;9687 
              MOVE.L         D3,$40(A7)               ;2f43 0040 
___SetChar__25:
;  172:  }
;  173:
;  174:  length_row = map.width >> 4;		/* Laenge einer Zeile in Woerter */
              MOVE.L         _map(A4),D3               ;262c 0000 
              ASR.L          #$4,D3                   ;e883 
;  175:
;  176:  start_adr = ((unsigned short *)map.pixptr)
;  177:				+ ((y - upper_limit + lmin) * length_row)
;  178:				+ (x >> 4);
              MOVE.L         D7,D1                    ;2207 
              SUB.L          D2,D1                    ;9282 
              ADD.L          $40(A7),D1               ;d2af 0040 
              MOVE.L         D3,D0                    ;2003 
              MOVE.L         D0,$28(A7)               ;2f40 0028 
              JSR            __CXM33(PC)               ;4eba 0000 
              MOVE.L         $30(A7),D1               ;222f 0030 
              ASR.L          #$4,D1                   ;e881 
              ADD.L          D1,D1                    ;d281 
              ADD.L          D0,D0                    ;d080 
              MOVE.L         _map+$8(A4),A2            ;246c 0008 
              ADD.L          D0,A2                    ;d5c0 
              ADD.L          D1,A2                    ;d5c1 
;  179:  /* Adresse des ersten Wortes */
;  180:
;  181:  nr_sh = x & 15;			/* Anzahl benoetigter Shifte */
              MOVEQ.L        #$f,D6                   ;7c0f 
              AND.L          $30(A7),D6               ;ccaf 0030 
;  182:  words = (w + 15) / 16;		/* Anzahl Words des Chars pro Zeile */
              MOVE.L         $2c(A7),D0               ;202f 002c 
              MOVEQ.L        #$f,D1                   ;720f 
              ADD.L          D1,D0                    ;d081 
              MOVEQ.L        #$10,D1                  ;7210 
              JSR            __CXD33(PC)               ;4eba 0000 
;  183:
;  184:  char_adr = (unsigned short *) ((unsigned short *)p + (lmin * words));
              MOVE.L         $40(A7),D1               ;222f 0040 
              MOVE.L         D0,$2c(A7)               ;2f40 002c 
              JSR            __CXM33(PC)               ;4eba 0000 
              ADD.L          D0,D0                    ;d080 
              MOVE.L         A3,A0                    ;204b 
              ADD.L          D0,A0                    ;d1c0 
              LEA            $c(A0),A3                ;47e8 000c 
;  185:
;  186:   //reg = 0L;
;  187:
;  188:   if (words == 1) {				/* width char <= width word */
              MOVE.L         D4,D5                    ;2a04 
              MOVE.L         $40(A7),D4               ;282f 0040 
              SUB.L          D4,D5      ; lmax-lmin um auf 0 abfragen zu koennen
___SetChar__26:
              MOVEQ.L        #$1,D0                   ;7001 
              CMP.L          $2c(A7),D0               ;b0af 002c 
              BNE.B          ___SetChar__31           ;661e 
___SetChar__27:
              MOVE.L         D3,D7                    ;2e03 
              ADD.L          D7,D7                    ; (hes) ich brauch immer das doppelte
;  189:     for (l=lmax; l>lmin; l--) {
              BRA.B          ___SetChar__29           ;6014 
___SetChar__28:
;  190:        reg = ((long)(*char_adr++)) << 16;
;  191:        reg >>= nr_sh;
;  192:        *(unsigned long *)start_adr |= reg;
              MOVEQ.L        #$0,D0                   ;7000 
              MOVE.W         (A3)+,D0                 ;301b 
              SWAP           D0                       ;4840 
;              CLR.W          D0                       ;4240 (hes)
              LSR.L          D6,D0                    ;eca8 
              OR.L           D0,(A2)                  ;8192 
;  193:        start_adr += length_row;
;              MOVE.L         D7,D0                    ;2007 
;              ADD.L          D0,D0                    ;d080 
              ADD.L          D7,A2                    ;d5c0 (hes)
;              SUBQ.L         #$1,D5                   ;5385 (hes)
___SetChar__29:
              DBF            D5,___SetChar__28
;              CMP.L          D4,D5                    ;ba84 
;              BGT.B          ___SetChar__28           ;6ee8 
___SetChar__30:
              BRA.B          ___SetChar__37           ;6030 
___SetChar__31:
;  194:    }						/* end for l		*/
;  195:   }
;  196:   else {
;  197:    adr = start_adr;
              MOVE.L         A2,A5                    ;2a4a 
;  198:    for (l=lmax; l>lmin; l--) {
              MOVE.L         $28(A7),D1               ; (hes) length_row * 2
              ADD.L          D1,D1                    ; (hes) schon vorbereiten
              MOVE.L         $2c(A7),D2               ; (hes) words in ein Register sichern
              BRA.B          ___SetChar__36           ;6028 
___SetChar__32:
;  199:      for (c=words; c>0; c--) {
;              MOVE.L         $2c(A7),D7               ;2e2f 002c 
              MOVE.L         D2,D7               ; (hes) words aus dem Register holen
              BRA.B          ___SetChar__34           ;6012 
___SetChar__33:
;  200:        reg = ((long)(*char_adr++)) << 16;
;  201:        reg >>= nr_sh;
;  202:        *(unsigned long *)(adr++) |= reg;
              MOVE.L         A5,A0                    ;204d 
              ADDQ.L         #$2,A5                   ;548d 
              MOVEQ.L        #$0,D0                   ;7000 
              MOVE.W         (A3)+,D0                 ;301b 
              SWAP           D0                       ;4840 
;              CLR.W          D0                       ;4240 (hes)
              LSR.L          D6,D0                    ;eca8 
              OR.L           D0,(A0)                  ;8190 
;              SUBQ.L         #$1,D7                   ;5387 
___SetChar__34:
              DBF            D7,___SetChar__33
;              TST.L          D7                       ;4a87 
;              BGT.B          ___SetChar__33           ;6eea 
___SetChar__35:
;  203:      }						/* end for c		*/
;  204:      start_adr += length_row;
;              MOVE.L         $28(A7),D1               ;222f 0028 
;              ADD.L          D1,D1                    ;d281 
              ADD.L          D1,A2                    ;d5c1 
;  205:      adr = start_adr;
              MOVE.L         A2,A5                    ;2a4a 
;              SUBQ.L         #$1,D5                   ;5385 
___SetChar__36:
              DBF            D5,___SetChar__32
;              CMP.L          D4,D5                    ;ba84 
;              BGT.B          ___SetChar__32           ;6ed4 
___SetChar__37:
;  206:    }						/* end for l		*/
;  207:  }						/* end else w<16	*/
;  208:
;  209:
;  210:sh_return:
;  211:
;  212:    if (command <= SET4) {	/* SET command, not a PUT command */
              CMPI.L         #$83,$3c(A7)             ;0caf 0000 0083 003c 
              BGT.B          ___SetChar__44           ;6e6c 
___SetChar__38:
;  213:       /* ... but {\tt DVItype} will allow character codes greater 255,
;  214:        * assuming that they all have the same width as the character
;  215:        * whose code is  c mod 256.
;  216:        */
;  217:      if (!cfontptr->ctfmw_valid ) {
              MOVE.L         $38(A7),D7               ;2e2f 0038 
              MOVE.L         _cfontptr(A4),A0          ;206c 0000 
              TST.B          $418(A0)                 ;4a28 0418 
              BNE.B          ___SetChar__40           ;663c 
___SetChar__39:
;  218:        /* Diese Abfrage ist eigentlich ueberfluessig!!		 */
;  219:        /* Wenn das Programm korrekt funktioniert, sollte dieser */
;  220:        /* Fall nie auftreten!					 */
;  221:        Warning("Internal error: 'ctfmw_valid' (%s, #%d, St:%d)",
              MOVE.L         $414(A0),A1              ;2268 0414 
              MOVE.L         (A1),A6                  ;2c51 
              MOVEQ.L        #$0,D0                   ;7000 
              MOVE.B         $a26(A1),D0              ;1029 0a26 
;  222:		cfontptr->common->fnt_group->fnt_name, cfontptr->fnt_number, cfontptr->common->fnt_status);
              MOVE.L         D0,-(A7)                 ;2f00 
              MOVE.L         (A0),-(A7)               ;2f10 
              PEA            $400(A6)                 ;486e 0400 
              PEA            $52(PC)                  ;487a 0052 
              JSR            _WarningStr(PC)              ;4eba 0000 
;  223:	setup_ctfmw(cfontptr);
              MOVE.L         _cfontptr(A4),A0          ;206c 0000 
              JSR            @setup_ctfmw(PC)         ;4eba 0000 
              LEA            $10(A7),A7               ;4fef 0010 
;  224:        h  += cfontptr->ctfmw[(int)pc&255];
              MOVEQ.L        #$0,D1                   ;7200 
              NOT.B          D1                       ;4601 
              AND.L          D1,D7                    ;ce81 
              ASL.L          #$2,D7                   ;e587 
              MOVE.L         _cfontptr(A4),A1          ;226c 0000 
              ADD.L          D7,A1                    ;d3c7 
              MOVE.L         $14(A1),D0               ;2029 0014 
              BRA.B          ___SetChar__41           ;600e 
___SetChar__40:
;  225:	/* h += scalewidth(cfontptr->common->fnt_group->tfmw[(int)pc&255], cfontptr->space_faktor); */
;  226:      }
;  227:      else {
;  228:        h  += cfontptr->ctfmw[(int)pc&255];
              MOVEQ.L        #$0,D1                   ;7200 
              NOT.B          D1                       ;4601 
              AND.L          D7,D1                    ;c287 
              ASL.L          #$2,D1                   ;e581 
              ADD.L          D1,A0                    ;d1c1 
              MOVE.L         $14(A0),D0               ;2028 0014 
___SetChar__41:
              ADD.L          D0,_h(A4)                 ;d1ac 0000 
___SetChar__42:
;  229:      }
;  230:      hh += cd->pixelwidth;
              MOVE.L         $34(A7),A0               ;206f 0034 
              MOVE.W         (A0),D0                  ;3010 
              EXT.L          D0                       ;48c0 
              ADD.L          D0,_hh(A4)                ;d1ac 0000 
;  231:      setmotion(); 	/* Immer wenn was geaendert wird, wird auch setmotion() gemacht! */
              JSR            @setmotion(PC)           ;4eba 0000 
___SetChar__43:
___SetChar__44:
              MOVEM.L        (A7)+,D2-D7/A2-A3/A5-A6
              ADD.W          #$1c,A7                  ;defc 001c 
              RTS                                     ;4e75 
__const:
__strings:
              DC.B           'Internal error: ctfmw_valid  (%s, #%d, St:%d)'
              DC.B           $00
              DC.B           $0


              XREF           _cfontptr
              XREF           @scalewidth
              XREF           _h
              XREF           _hconv
              XREF           __CXD33
              XREF           _hh
              XREF           @Load_really
              XREF           @unpack_char
              XREF           _hoffset
              XREF           _vv
              XREF           _voffset
              XREF           _map
              XREF           _lower_limit
              XREF           _upper_limit
              XREF           __CXM33
              XREF           _WarningStr
              XREF           @setup_ctfmw
              XREF           @setmotion
              XREF           _vconv
              XDEF           @SetChar
              END
