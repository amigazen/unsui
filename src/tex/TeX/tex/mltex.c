#define EXTERN extern
#include "texd.h"

#ifdef MLTEX

#ifndef effective_char

#if 0		/* das Ergebnis sollte etwa so aussehen... */
static void
dummy_function_do_nothing()
{

__asm__("
.text
	.even
.globl _effective_char
_effective_char:
	lea _zeqtb,a0
	movel sp@(4),d0

|	cmpl a0@(charsubdefmax_offset),d0
	movel a0,a1
	addw %0,a1
	cmpl a1@,d0
	jgt L_effc_end

	movel d0,d1	| in_chr ist < 256
	aslw #2,d1
	addw d1,a0
	addw %1,a0
	tstw a0@
	jeq L_effc_end

	moveq #0,d0
	moveb a0@(1),d0
L_effc_end:
	rts "
  :
  : "i" ((intbase + char_sub_def_max_code) * sizeof(MEDmemoryword)),
    "i" (char_sub_base * sizeof(MEDmemoryword))
);

}

/* intbase (6419) + char_sub_def_max_code (55) = 25896
   char_sub_base (513) * sizeof(MEDmemoryword)  = 2052 */

#endif

  integer
effective_char( integer in_chr )
{ effective_char_regmem

  if( is_ML_TeX && in_chr <= char_sub_def_max && char_list_exists(in_chr) )
    return ( (integer) eqtb[char_sub_base + in_chr].qqqq.b1 );
  return in_chr;
}


#endif

/* called from hlist_out: */
/* (br) added this as a new function */

  halfword
substitute_char_list(halfword p, quarterword f, quarterword c)
{ substitute_char_list_regmem
  halfword rem_chs;	/* remaining hlist */
  long_halfword accent_chs, char_chs;
  halfword k1_chs, k2_chs;
  halfword p_chs, q_chs;
  scaled a_chs, h_chs, ha_chs, w_chs, x_chs, delta_chs;
  real s_chs;
  fourquarters i_chs;

  rem_chs = link(p);
  font(p) = nullfont;

  if( !char_list_exists(c) || (integer)c > char_sub_def_max ) {
    charwarning(f, c);
    return rem_chs;		/* Don't forget this ! */
  }

  accent_chs = eqtb[c + char_sub_base].qqqq.b0;
  char_chs   = eqtb[c + char_sub_base].qqqq.b1;

  if( tracinglostchars > 99 ) {
    begindiagnostic();
    c_printnl("Using Character Substitution List for: ");
    print(c);  c_print(" = ");
    print(accent_chs);  printchar(' ');
    print(char_chs);  c_print(" in font ");
    slowprint(fontname(f));
    printchar('!');
    enddiagnostic(false);
  }

  p_chs = newcharacter(f, accent_chs);	/* charnode for accent */
  if( p_chs == (halfword)0 ) {
    /* Warning ist schon in newcharacter ausgegeben worden */
    return rem_chs;
  }

  q_chs = newcharacter(f, char_chs);	/* charnode for basechar */
  if( q_chs == (halfword)0 ) {
    /* Warning ist schon in newcharacter ausgegeben worden */
    /* und nicht vergessen, den erfolgreich allokierten Charnode freizugeben */
    freeavail(p_chs);
    return rem_chs;
  }

  /* Rebuild character from its list: */
  /* link(p) = 0; */		/* nicht unbedingt notwendig */

  x_chs = xheight(f);
  s_chs = slant(f)/((double) 65535.0);

  i_chs = zcharinfo(f, char_chs);
  w_chs = zcharwidth(f, i_chs);
  h_chs = zcharheight(f, heightdepth(i_chs));

  i_chs = zcharinfo(f, accent_chs);
  a_chs = zcharwidth(f, i_chs);
  ha_chs = zcharheight(f, heightdepth(i_chs));

  if( ha_chs > 0 && h_chs != x_chs ) {
	/* Falls Akzent (koennte auch ein Circonflex, ... sein) und
	 * Buchstabe != x_height, muss Akzent geshiftet werden:
	 */
    long_halfword r_chs;

    r_chs = newnullbox();
    width(r_chs)       = a_chs;
    depth(r_chs)       = zchardepth(f, heightdepth(i_chs));
    height(r_chs)      = ha_chs;
    shiftamount(r_chs) = x_chs - h_chs;
    listptr(r_chs)     = p_chs;
    p_chs = r_chs;
  }

  delta_chs = round( (w_chs - a_chs)/((double)2.0) + (h_chs - x_chs) * s_chs );
  k1_chs = newkern(delta_chs);		subtype(k1_chs) = acckern;
  k2_chs = newkern(-a_chs - delta_chs);	subtype(k2_chs) = acckern;

  /* neue Liste aufbauen */
  link(p) = k1_chs;		/* an alte Liste anhaengen, dann hat man
				   jedoch noch altes Zeichen ??? */

  p            = k1_chs;	/* \kern delta_chs	*/
  link(p)      = p_chs;		/* \char accent		*/
  link(p_chs)  = k2_chs;	/* \kern -delta_chs-\w accent */
  link(k2_chs) = q_chs;		/* \char c		*/
  link(q_chs)  = rem_chs;	/* remaining List */

  return p;
}
#endif
