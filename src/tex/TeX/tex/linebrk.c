#define EXTERN extern
#include "texd.h"

void trybreak ( integer pi, smallnumber breaktype )		/* 829. */
{/* 10 30 31 22 60 */ trybreak_regmem 
  register halfword r  ; 
  register halfword prevr  ; 
  register halfword oldl  ; 
  boolean nobreakyet  ; 
  register halfword prevprevr  ; 
/* register halfword s; */
/* register halfword q; */
/* register halfword v; */
/* register integer t; */
/* register internalfontnumber f; */
  register halfword l  ; 
  boolean noderstaysactive  ; 
  register scaled linewidth  ; 
  register schar fitclass  ; 
  register halfword b  ; 
  register integer d  ; 
  boolean artificialdemerits  ; 
/* register halfword savelink; */
/* register scaled shortfall; */

#if 0
  scaled breakwidth[6];
  scaled curactivewidth[6];
#endif

  if ( abs ( pi ) >= infpenalty ) 
    if ( pi > 0 ) 
      goto lab10;
    else
      pi = ejectpenalty;

  nobreakyet = true ; 
  oldl = 0 ;
  prevr = active ; 
#if 0
  { register scaled *caw1 = &curactivewidth[0];
    register scaled *aw  = &activewidth[0];

    *caw1++ = *aw++;    *caw1++ = *aw++;    *caw1++ = *aw++;
    *caw1++ = *aw++;    *caw1++ = *aw++;    *caw1   = *aw;
  }
#else
  { register scaled *aw  = &activewidth[0];

    curactivewidth[0] = *aw++;	curactivewidth[1] = *aw++;
    curactivewidth[2] = *aw++;	curactivewidth[3] = *aw++;
    curactivewidth[4] = *aw++;	curactivewidth[5] = *aw;
  }
#endif

  while ( true ) {
      
lab22:
   { register long_halfword tmp_r;

    tmp_r = link ( prevr ) ; 

    while ( ztype ( tmp_r ) == deltanode ) {
#if 0
      curactivewidth [ 1 ] = curactivewidth [ 1 ] + mem [ r + 1 ] .cint ; 
	...
      curactivewidth [ 6 ] = curactivewidth [ 6 ] + mem [ r + 6 ] .cint ; 
#else
      /* !!!! inner loop !!!! */
      register scaled *caw = &curactivewidth[0];
      register memoryword *memr = &mem[tmp_r];
      memr++;  /* produces better code than: memr = &mem[tmp_r+1] */

      *caw++ += (memr++)->cint;      *caw++ += (memr++)->cint;
      *caw++ += (memr++)->cint;      *caw++ += (memr++)->cint;
      *caw++ += (memr++)->cint;      *caw   += memr->cint;
#endif
      prevprevr = prevr ; 
      prevr = tmp_r ;
      tmp_r = link ( /*prevr*/ tmp_r );		/* goto lab22; */
    }

    r = tmp_r;
    l = zlinenumber ( tmp_r ) ; 
   }

    {
      if ( l > oldl ) {
	if ( ( minimumdemerits < awfulbad )
		&& ( ( oldl != easyline ) || ( r == lastactive ) ) ) {
	  if ( nobreakyet ) {
	    register halfword s;

	    nobreakyet = false;
	    { register scaled *bw = &breakwidth[0];
	      register scaled *bg = &background[0];	/* was 1 */

	      *bw++ = *bg++;  *bw++ = *bg++;  *bw++ = *bg++;
	      *bw++ = *bg++;  *bw++ = *bg++;  *bw   = *bg;
	    }
	    s = curp ; 
	    if ( breaktype > unhyphenated && /*curp*/ s != 0 ) {
	      register halfword v;
	      integer t;

	      v = /*curp*/ s ; 
	      s = postbreak ( /*curp*/ v ) ; 
	      t = replacecount ( /*curp*/ v ) ; 
	      while ( t > 0 ) {
		decr ( t ) ; 
		v = link ( v ) ; 
		if ( ischarnode ( v ) ) {
		  internalfontnumber f;

		  f = font ( v ) ; 
		  breakwidth[0] -= zcharwidth(f, zcharinfo(f, character(v)));
		} else switch ( ztype ( v ) ) {
		case ligaturenode :
		  { internalfontnumber f;

		    f = font ( ligchar ( v ) );
		    breakwidth [ 0 ] -= zcharwidth ( f,
			zcharinfo ( f, character ( ligchar ( v ) ) ) ) ;
		  }
		  break ; 
		case hlistnode : 
		case vlistnode : 
		case rulenode : 
		case kernnode : 
		  breakwidth [ 0 ] -= width ( v ) ;
		  break ;
		default: 
		  confusion("disc1");
		  break ; 
		} 
	      }		/* while ( t > 0 ) */
	      while ( s != 0 ) {
		if ( ischarnode ( s ) ) {
		  internalfontnumber f;

		  f = font ( s ) ; 
		  breakwidth [ 0 ] += zcharwidth ( f,
				zcharinfo ( f, character ( s ) ) ) ;
		} else switch ( ztype ( s ) ) {
		case ligaturenode : 
		  { internalfontnumber f;

		    f = font ( ligchar ( s ) ) ; 
		    breakwidth [ 0 ] += zcharwidth ( f,
			zcharinfo ( f, character ( ligchar ( s ) ) ) ) ;
		  }
		  break ; 
		case hlistnode : 
		case vlistnode : 
		case rulenode : 
#if 1  /* 3.1415 */
		case kernnode : 
#endif
		  breakwidth [ 0 ] += width ( s ) ; 
		  break ; 
#if 0  /* 3.1415 */
		case kernnode : 
		  if ( ( t == 0 ) && ( subtype ( s ) != acckern ) ) 
		    t = -1 ; 
		  else
		    breakwidth [ 0 ] += width ( s ) ;
		  break ;
#endif
		default: 
		  confusion("disc2");
		  break ; 
		} 
		incr ( t ) ; 
		s = link ( s ) ; 
	      }			/* while ( s != 0 ) */
	      breakwidth [ 0 ] += discwidth ;
#if 0  /* 3.1415 */
	      if ( t == 0 )
		s = link ( v ) ; 
#else
	      if ( postbreak(curp) == 0 )
		s = link ( v ) ; 
#endif
	    }
	    while ( s != 0 ) {
	      if ( ischarnode ( s ) ) 
		goto lab30 ; 
	      switch ( ztype ( s ) ) {
	      case gluenode : 
		{ register halfword v;

		  v = glueptr ( s ) ; 
		  breakwidth [ 0 ] -= width ( v ) ;
		  breakwidth [ 1 + stretchorder ( v ) ] -= stretch ( v ) ;
		  breakwidth [ 5 ] -= shrink ( v ) ;
		}
		break ;
	      case penaltynode : 
		break ; 
#if 0  /* 3.1415 */
	      case mathnode : 
	      case kernnode : 
		if ( subtype ( s ) == acckern )
		  goto lab30 ;
		else
		  breakwidth [ 0 ] -= width ( s ) ;
		break ; 
#else
	      case mathnode : 
		  breakwidth [ 0 ] -= width ( s ) ;
		break;
	      case kernnode : 
		if ( subtype ( s ) != explicit )
		  goto lab30 ;
		else
		  breakwidth [ 0 ] -= width ( s ) ;
		break ; 
#endif
	      default: 
		goto lab30 ; 
		break ; 
	      } 
	      s = link ( s ) ; 
	    }
lab30: ;
	  }	/* if ( nobreakyet ) */

	  if ( ztype ( prevr ) == deltanode ) {
#if 0
	    converttobreakwidth ( 1 ); ... converttobreakwidth ( 6 );
#else
	    register scaled *caw = &curactivewidth[0];
	    register scaled *bw  = &breakwidth[0];
	    register memoryword *mem_prevr = &mem[prevr];
	    mem_prevr++;

	    (mem_prevr++)->cint += (*bw++ - *caw++);
	    (mem_prevr++)->cint += (*bw++ - *caw++);
	    (mem_prevr++)->cint += (*bw++ - *caw++);
	    (mem_prevr++)->cint += (*bw++ - *caw++);
	    (mem_prevr++)->cint += (*bw++ - *caw++);
	    (mem_prevr)->cint   += (*bw   - *caw);
#endif
	  } else if ( prevr == active ) {
#if 0
	    storebreakwidth ( 1 ); ... storebreakwidth ( 6 );
#else
	    register scaled *aw = &activewidth[0];
	    register scaled *bw = &breakwidth[0];

	    *aw++ = *bw++;  *aw++ = *bw++;  *aw++ = *bw++;
	    *aw++ = *bw++;  *aw++ = *bw++;  *aw   = *bw;
#endif
	  } else {
	    register long_halfword q;

	    q = getnode ( deltanodesize ) ; 
	    link ( q ) = r ;
	    /* ztype ( q ) = deltanode; subtype ( q ) = 0; */
	    set_type_subtype(q, deltanode, 0);

#if 0
	    newdeltatobreakwidth ( 1 ); ... newdeltatobreakwidth ( 6 );
#else
	    { register scaled *caw = &curactivewidth[0];
	      register scaled *bw  = &breakwidth[0];
	      register memoryword *mem_q = &mem[q];
	      mem_q++;

	      (mem_q++)->cint = *bw++ - *caw++;
	      (mem_q++)->cint = *bw++ - *caw++;
	      (mem_q++)->cint = *bw++ - *caw++;
	      (mem_q++)->cint = *bw++ - *caw++;
	      (mem_q++)->cint = *bw++ - *caw++;
	      (mem_q)->cint   = *bw   - *caw;
	    }
#endif
	    prevprevr = prevr ; 
	    link ( prevr ) = q ; 
	    prevr = q ; 
	  } 

	  if ( abs ( adjdemerits ) >= awfulbad - minimumdemerits ) 
	    minimumdemerits = awfulbad - 1 ; 
	  else
	    minimumdemerits += abs ( adjdemerits ) ;

	  for( fitclass = 0 ; fitclass <= 3 ; fitclass++ ) {
	    if ( minimaldemerits [ fitclass ] <= minimumdemerits ) {
		register long_halfword q;

		q = getnode ( passivenodesize ) ; 
		link ( q ) = passive ; 
		passive = q ; 
		curbreak ( q ) = curp ; 
#ifdef STAT
		incr ( passnumber ) ; 
		serial ( q ) = passnumber ; 
#endif /* STAT */
		prevbreak ( q ) = bestplace [ fitclass ] ;

		q = getnode ( activenodesize ) ; 
		breaknode ( q ) = passive ; 
		zlinenumber ( q ) = bestplline [ fitclass ] + 1 ; 
		fitness ( q ) = fitclass ; 
		ztype ( q ) = breaktype ; 
		totaldemerits ( q ) = minimaldemerits [ fitclass ] ; 
		link ( q ) = r;
		link ( prevr ) = q;
		prevr = q;
#ifdef STAT
		if ( tracingparagraphs > 0 ) {
		  c_printnl("@@");
		  printint ( serial ( passive ) );
		  c_print(": line ");
		  printint ( zlinenumber ( q ) - 1 );
		  printchar ( 46 );
		  printint ( fitclass );
		  if ( breaktype == hyphenated )
		    printchar ( 45 );
		  c_print(" t=");
		  printint ( totaldemerits ( q ) );
		  c_print(" -> @@");
		  if ( prevbreak ( passive ) == 0 )
		    printchar ( 48 );
		  else
		    printint ( serial ( prevbreak ( passive ) ) );
		} 
#endif /* STAT */
	    }
	    minimaldemerits [ fitclass ] = awfulbad ; 
	  }

	  minimumdemerits = awfulbad ; 
	  if ( r != lastactive ) {
	    register long_halfword q;

	    q = getnode ( deltanodesize ) ; 
	    link ( q ) = r ; 
	    /* ztype ( q ) = deltanode; subtype ( q ) = 0; */
	    set_type_subtype(q, deltanode, 0);

#if 0
	    newdeltafrombreakwidth ( 1 ); ... newdeltafrombreakwidth ( 6 );
#else
	    { register scaled *caw = &curactivewidth[0];
	      register scaled *bw  = &breakwidth[0];
	      register memoryword *mem_q = &mem[q];
	      mem_q++;

	      (mem_q++)->cint = *caw++ - *bw++;
	      (mem_q++)->cint = *caw++ - *bw++;
	      (mem_q++)->cint = *caw++ - *bw++;
	      (mem_q++)->cint = *caw++ - *bw++;
	      (mem_q++)->cint = *caw++ - *bw++;
	      (mem_q)->cint   = *caw   - *bw;
	    }
#endif
	    prevprevr = prevr ; 
	    link ( prevr ) = q ; 
	    prevr = q ; 
	  } 
	} 
	if ( r == lastactive ) 
	  goto lab10 ;

	if ( l > easyline ) {
	  linewidth = secondwidth ; 
	  oldl = maxhalfword - 1 ; 
	} else {
	  oldl = l ; 
	  if ( l > lastspecialline ) 
	    linewidth = secondwidth ; 
	  else if ( parshapeptr == 0 ) 
	    linewidth = firstwidth ; 
	  else
	    linewidth = mem [ parshapeptr + 2 * l ] .cint ; 
	}
      }
    }
    {
      artificialdemerits = false ; 

    { register scaled *caw = &curactivewidth[0];
      register scaled shortfall;

      shortfall = linewidth - *caw;	/*curactivewidth[0]*/

      if ( shortfall > 0 ) {
	if ( *(caw+2) != 0 || *(caw+3) != 0 || *(caw+4) != 0 ) {
	  b = 0 ; 
	  fitclass = 2 ; 
	} else {
	  if ( shortfall > 7230584L && *(caw+1) < 1663497L ) {
	    b = infbad ; 
	    fitclass = 0 ; 
	  } else {
	    b = badness ( shortfall, *(caw+1) );
	    if ( b > 12 ) 
	      if ( b > 99 ) 
	        fitclass = 0 ; 
	      else fitclass = 1 ; 
	    else fitclass = 2 ;
	  }
        }
      } else {
	if ( - (integer) shortfall > *(caw+5) ) {
	  b = infbad + 1 ;
	  fitclass = 3 ;
	} else {
	  b = badness ( - (integer) shortfall, *(caw+5) ) ; 
	if ( b > 12 ) 
	  fitclass = 3 ; 
	else
	  fitclass = 2 ;
	}
      }
    }

      if ( ( b > infbad ) || ( pi == ejectpenalty ) ) {
	if ( finalpass && ( minimumdemerits == awfulbad )
	     && ( link ( r ) == lastactive ) && ( prevr == active ) )
	  artificialdemerits = true ; 
	else if ( b > threshold ) 
	  goto lab60 ; 
	noderstaysactive = false ; 
      } else {
	prevr = r ; 
	if ( b > threshold ) 
	  goto lab22 ; 
	noderstaysactive = true ; 
      }
      if ( artificialdemerits ) 
	d = 0 ; 
      else {
	d = linepenalty + b ; 
	if ( abs ( d ) >= 10000 ) 
	  d = 100000000L ; 
	else
	  d = d * d ; 
	if ( pi != 0 ) {
	  if ( pi > 0 ) 
	    d = d + pi * pi ; 
	  else if ( pi > ejectpenalty ) 
	    d = d - pi * pi ; 
	}
	if ( ( breaktype == hyphenated ) && ( ztype ( r ) == hyphenated ) ) {
	  if ( curp != 0 ) 
	    d = d + doublehyphendemerits ; 
	  else
	    d = d + finalhyphendemerits ;
	}
	if ( abs ( toint ( fitclass ) - toint ( fitness ( r ) ) ) > 1 ) 
	  d = d + adjdemerits ; 
      }
#ifdef STAT
      if ( tracingparagraphs > 0 ) {
	if ( printednode != curp ) {
	  c_printnl("");
	  if ( curp == 0 ) 
	    shortdisplay ( link ( printednode ) );
	  else {
	    register halfword savelink;		/* (br) */

	    savelink = link ( curp ) ; 
	    link ( curp ) = 0 ; 
	    c_printnl("");
	    shortdisplay ( link ( printednode ) ) ; 
	    link ( curp ) = savelink ; 
	  } 
	  printednode = curp ; 
	} 
	printnl ( 64 ) ; 
	if ( curp == 0 ) 
	  printesc( STR_PAR );
	else if ( ztype ( curp ) != gluenode ) {
	  if ( ztype ( curp ) == penaltynode ) 
	    printesc( STR_PENALTY );
	  else if ( ztype ( curp ) == discnode ) 
	    printesc( STR_DISCRETIONARY );
	  else if ( ztype ( curp ) == kernnode ) 
	    printesc( STR_KERN );
	  else
	    printesc( STR_MATH );
	} 
	c_print(" via @@");
	if ( breaknode ( r ) == 0 ) 
	  printchar ( 48 ) ; 
	else
	  printint ( serial ( breaknode ( r ) ) );
	c_print(" b=");
	if ( b > infbad ) 
	  printchar ( 42 ) ; 
	else
	  printint ( b ) ; 
	c_print(" p=");
	printint ( pi ) ; 
	c_print(" d=");
	if ( artificialdemerits ) 
	  printchar ( 42 ) ; 
	else
	  printint ( d ) ; 
      } 
#endif /* STAT */
      d += totaldemerits ( r ) ; 
      if ( d <= minimaldemerits [ fitclass ] ) {
	minimaldemerits [ fitclass ] = d ; 
	bestplace [ fitclass ] = breaknode ( r ) ; 
	bestplline [ fitclass ] = l ; 
	if ( d < minimumdemerits ) 
	  minimumdemerits = d ; 
      }
      if ( noderstaysactive ) 
	goto lab22 ; 
lab60:
      link ( prevr ) = link ( r ) ; 
      freenode ( r , activenodesize ) ; 

      if ( prevr == active ) {
	r = link ( /*active*/ prevr ) ; 
	if ( ztype ( r ) == deltanode ) {
#if 0
	  updateactive ( 1 ) ; ... updateactive ( 6 ) ;
	  curavtivewidth[1..6] = activewidth[1..6];
#else
	  { register scaled *caw = &curactivewidth[0];
	    register scaled *aw  = &activewidth[0];
	    register memoryword *memr = &mem[r];
	    memr++;

	    *aw += (memr++)->cint;	*caw++ = *aw++;
	    *aw += (memr++)->cint;	*caw++ = *aw++;
	    *aw += (memr++)->cint;	*caw++ = *aw++;
	    *aw += (memr++)->cint;	*caw++ = *aw++;
	    *aw += (memr++)->cint;	*caw++ = *aw++;
	    *aw += (memr)->cint;	*caw   = *aw;
	  }
#endif
	  link ( /*active*/ prevr ) = link ( r ) ; 
	  freenode ( r , deltanodesize ) ; 
	} 

      } else if ( ztype ( prevr ) == deltanode ) {

	r = link ( prevr ) ; 
	if ( r == lastactive ) {
#if 0
	  downdatewidth ( 1 ); ... downdatewidth ( 6 );
#else
	  register scaled *caw = &curactivewidth[0];
	  register memoryword *mem_prevr = &mem[prevr];
	  mem_prevr++;

	  *caw++ -= (mem_prevr++)->cint;  *caw++ -= (mem_prevr++)->cint;
	  *caw++ -= (mem_prevr++)->cint;  *caw++ -= (mem_prevr++)->cint;
	  *caw++ -= (mem_prevr++)->cint;  *caw   -= (mem_prevr)->cint;
#endif
	  link ( prevprevr ) = /*lastactive*/ r ;
	  freenode ( prevr , deltanodesize ) ; 
	  prevr = prevprevr ;

	} else if ( ztype ( r ) == deltanode ) {
#if 0
	  curactivewidth [ 1 ] = curactivewidth [ 1 ] + mem [ r + 1 ] .cint;
		...
	  curactivewidth [ 6 ] = curactivewidth [ 6 ] + mem [ r + 6 ] .cint;
	  combinetwodeltas ( 1 ) ; ... combinetwodeltas ( 6 );
#else
	  register scaled *caw = &curactivewidth[0];
	  register memoryword *mem_r = &mem[r];
	  register memoryword *mem_prevr = &mem[prevr];
	  mem_r++;
	  mem_prevr++;

	  *caw++ += mem_r->cint;   (mem_prevr++)->cint += (mem_r++)->cint;
	  *caw++ += mem_r->cint;   (mem_prevr++)->cint += (mem_r++)->cint;
	  *caw++ += mem_r->cint;   (mem_prevr++)->cint += (mem_r++)->cint;
	  *caw++ += mem_r->cint;   (mem_prevr++)->cint += (mem_r++)->cint;
	  *caw++ += mem_r->cint;   (mem_prevr++)->cint += (mem_r++)->cint;
	  *caw   += mem_r->cint;    mem_prevr->cint    += mem_r->cint;
#endif
	  link ( prevr ) = link ( r ) ; 
	  freenode ( r , deltanodesize ) ; 
	} 
      } 
    }
  }

lab10: ;
#ifdef STAT
  if ( curp != 0 && curp == printednode && ztype ( curp ) == discnode ) {
    integer t;

    t = replacecount ( curp ) ; 
    while ( t > 0 ) {
      decr ( t ) ; 
      printednode = link ( printednode ) ; 
    } 
  } 
#endif /* STAT */
}

  static void
cleanup_memory_removing_breaknodes(void)
{ linebreak_regmem
  register long_halfword q, r_curp;

  q = link(active);
  while( (halfword)q != lastactive ) {
    r_curp = link(q);
    if ( ztype(q) == deltanode )
      freenode( q, deltanodesize );
    else
      freenode( q, activenodesize );
    q = r_curp;
  }
  q = passive;
  while ( (halfword)q != 0 ) {
    r_curp = link(q);
    freenode( q, passivenodesize );
    q = r_curp;
  }
  /* curp = r_curp; */ /* <<== notwendig ??? */
}


#ifndef OLD_TRYTO

/* 894. Try to hyphenate the following word */

  static void
try_to_hyphenate(halfword curp)
{ linebreak_regmem

  halfword prevs;	/* (br) */
  register halfword s, q;

  prevs = curp;
  s = link ( prevs );
  if ( s == 0 )
    return;

  while ( true ) {
    /* 896. Skip to node ha, ... */
    register unsigned char c;	/* (br) */

    if ( ischarnode ( s ) ) {
      c = character ( s );
      hf = font ( s );
    } else if ( ztype ( s ) == ligaturenode ) 
      if ( ligptr ( s ) == 0 ) 
	goto lab22;
      else {
	q = ligptr ( s );
	c = character ( q );
	hf = font ( q );
      }
    else if ( (ztype(s) == kernnode) && (subtype(s) == normal) )
      goto lab22;
    else if ( ztype ( s ) == whatsitnode ) {
      if ( subtype ( s ) == languagenode ) {
	curlang = whatlang ( s );
	lhyf = whatlhm ( s );
	rhyf = whatrhm ( s );
      }
      goto lab22;
    } else
      return;  /* goto lab31; */

    if ( lccode(c) != 0 )
      if ( ( lccode(c) == c ) || ( uchyph > 0 ) )
	goto lab32;
      else
	return;  /* goto lab31; */

lab22:
    prevs = s;
    s = link ( prevs );
  }

lab32:
  hyfchar = hyphenchar(hf);
  if ( hyfchar < 0 )
    return;  /* goto lab31; */

  if ( hyfchar > 255 )
    return; /* goto lab31; */

  ha = prevs;

  if ( lhyf + rhyf > 63 ) 
    return; /* goto lab31; */

  /* 897. Skip to node hb, putting letters into hu and hc */
  hn = 0;
  {
    /* Damit es ein bisschen schneller geht, lokale Variable */
    register unsigned short l_hyf_bchar;   /* TeX 3.141 */

    while ( true ) {
      if ( ischarnode ( s ) ) {
	register unsigned char c;	/* (br) */

	if ( font ( s ) != hf )
	  goto lab33;

	/* alt: c = character ( s ); */   /* TeX 3.141 */
	/* neu: l_hyf_bchar = character(s);  c = l_hyf_bchar; */
	c = character(s);  l_hyf_bchar = c; /* c muss unsigned sein */

	if ( lccode ( c ) == 0 )
	  goto lab33;
	if ( hn == 63 )
	  goto lab33;
	hb = s;
	incr ( hn );
	hu [ hn ] = c;
	hc [ hn ] = lccode( c );
        l_hyf_bchar = nonchar;   /* TeX 3.141 */
      } else if ( ztype ( s ) == ligaturenode ) {
	/* j nur hier benoetigt, deshalb ... */
	register smallnumber j;

	if ( font ( ligchar ( s ) ) != hf ) 
	  goto lab33 ; 
	j = hn ; 
	q = ligptr ( s ) ; 
	if ( q > 0 )    /* TeX 3.141 */
	  l_hyf_bchar = character(q);
	while ( q > 0 ) {
	  unsigned char c;	/* (br) */

	  c = character( q );
	  if ( lccode( c ) == 0 )
	    goto lab33;
	  if ( j == 63 )
	    goto lab33;
	  incr ( j );
	  hu [ j ] = c;
	  hc [ j ] = lccode ( c );
	  q = link( q );
	}
	hb = s;
	hn = j;
	if ( odd(subtype(s)) ) {  /* TeX 3.141 */
	  l_hyf_bchar = fontbchar(hf);
	} else {
	  l_hyf_bchar = nonchar;
	}
      }	else if ( ( ztype(s) == kernnode ) && ( subtype(s) == normal ) ) {
	hb = s;    /* TeX 3.141 */
      } else {
	goto lab33;
      }
      s = link ( s );
    }

lab33: ;
    hyf_bchar = l_hyf_bchar;    /* TeX 3.141 */
  }

  /* 899. Check that the nodes following hb... */
  if ( hn < lhyf + rhyf ) 
    goto lab31;

  while ( true ) {
    if ( ! ( ischarnode ( s ) ) ) {
      switch ( ztype ( s ) ) {
	case ligaturenode : 
	  break ; 
	case kernnode : 
	  if ( subtype ( s ) != normal ) 
	    goto lab34 ; 
	  break ; 
	case whatsitnode : 
	case gluenode : 
	case penaltynode : 
	case insnode : 
	case adjustnode : 
	case marknode : 
	  goto lab34 ; 
	  break;
	default:
	  return;  /* goto lab31; */
	  break;
      }
    }
    s = link ( s );
  }

lab34: ;
  hyphenate ();

lab31: ;
}
#endif



void linebreak ( integer finalwidowpenalty )		/* 815. */
{/* 30 31 32 33 34 35 22 */ linebreak_regmem 
  boolean autobreaking;
#ifndef OLD_PREVP
  /* register */ halfword prevp;
#else
  boolean prevp;
#endif
  /*register halfword q, r, s;*/ /* , prevs; */ /* nach unten verlagert */
  /* register internalfontnumber f; */  /* nach unten verlagert (mehrmals) */
  /* register smallnumber j; */ /* nach unten verlagert */
  /* unsigned char c; */ /* nach unten verlagert (mehrmals) */
  boolean secondpass;

  packbeginline = curlist .mlfield ; 

  /* 816. Get ready to start line breaking */
  link ( temphead ) = link ( curlist .headfield ) ; 
  if ( ischarnode ( curlist .tailfield ) ) 
    tailappend ( newpenalty ( infpenalty ) ) ; 
  else if ( ztype ( curlist .tailfield ) != gluenode ) 
    tailappend ( newpenalty ( infpenalty ) ) ; 
  else {
    ztype ( curlist .tailfield ) = penaltynode ; 
    deleteglueref ( glueptr ( curlist .tailfield ) ) ; 
    flushnodelist ( leaderptr ( curlist .tailfield ) ) ; 
    mem [ curlist .tailfield + 1 ] .cint = infpenalty ; 
  } 
  link ( curlist .tailfield ) = newparamglue ( parfillskipcode ) ; 
#if 1  /* 3.1415 */
  initcurlang = curlist .pgfield % 0x010000L;
  initlhyf = curlist .pgfield / 0x400000L;
  initrhyf = ( curlist .pgfield / 0x010000L ) % 0x40;
#endif
  popnest () ; 

  /* 827. Get ready ... 816.+ */
  noshrinkerroryet = true ; 
  if ( shrinkorder ( leftskip ) != normal && shrink ( leftskip ) != 0 )  {
    leftskip = finiteshrink ( leftskip ) ; 
  } 
  if ( shrinkorder ( rightskip ) != normal && shrink ( rightskip ) != 0  ) {
    rightskip = finiteshrink ( rightskip ) ; 
  } 

  background [ 1 ] = 0 ; 
  background [ 2 ] = 0 ; 
  background [ 3 ] = 0 ; 
  background [ 4 ] = 0 ; 

  { register halfword q, r;

    q = leftskip;  r = rightskip;
    background [ 0 ] = width ( q ) + width ( r ) ; 
    background [ 1 + stretchorder ( q ) ] = stretch ( q );
    background [ 1 + stretchorder ( r ) ] += stretch ( r );
    background [ 5 ] = shrink ( q ) + shrink ( r );
  }

  /* 834. Get ready ... 816.+ */
  minimumdemerits = awfulbad ; 
  minimaldemerits [ 0 ] = awfulbad ; 
  minimaldemerits [ 1 ] = awfulbad ; 
  minimaldemerits [ 2 ] = awfulbad ; 
  minimaldemerits [ 3 ] = awfulbad ; 

  /* 848. Get ready ... 816.+ */
  if ( parshapeptr == 0 ) 
    if ( hangindent == 0 ) {
      lastspecialline = 0 ; 
      secondwidth = hsize ; 
      secondindent = 0 ; 
    } else {
      lastspecialline = abs ( hangafter ) ; 
      if ( hangafter < 0 ) {
	firstwidth = hsize - abs ( hangindent ) ; 
	if ( hangindent >= 0 ) 
	  firstindent = hangindent ; 
	else firstindent = 0 ; 
	secondwidth = hsize ; 
	secondindent = 0 ; 
      } else {
	firstwidth = hsize ; 
	firstindent = 0 ; 
	secondwidth = hsize - abs ( hangindent ) ; 
	if ( hangindent >= 0 ) 
	  secondindent = hangindent ; 
	else secondindent = 0 ; 
      }
    }
  else {
    lastspecialline = info ( parshapeptr ) - 1 ; 
    secondwidth = mem [ parshapeptr + 2 * ( lastspecialline + 1 ) ] .cint ; 
    secondindent = mem [ parshapeptr + 2 * lastspecialline + 1 ] .cint ; 
  }
  if ( looseness == 0 ) 
    easyline = lastspecialline;
  else
    easyline = maxhalfword;

  /* 863. Find optimal breakpoints */
  threshold = pretolerance;
  if ( threshold >= 0 ) {
#ifdef STAT
    if ( tracingparagraphs > 0 ) {
      begindiagnostic () ; 
      c_printnl("@firstpass");
    } 
#endif /* STAT */
    secondpass = false;
    finalpass = false;
  } else {
    threshold = tolerance;
    secondpass = true;
    finalpass = ( emergencystretch <= 0 );
#ifdef STAT
    if ( tracingparagraphs > 0 )
      begindiagnostic ();
#endif /* STAT */
  }

  while ( true ) {
    if ( threshold > infbad ) 
      threshold = infbad;
    if ( secondpass ) {
#ifdef INITEX
      if ( trienotready )
	inittrie ();
#endif /* INITEX */
#if 0  /* 3.1415 */
      lhyf = curlist .lhmfield;
      rhyf = curlist .rhmfield;
      curlang = 0;
#else
      curlang = initcurlang;
      lhyf = initlhyf;
      rhyf = initrhyf;
#endif
    }

    /* 864. Create an active breakpoint ... */
    { register long_halfword q;

    q = getnode ( activenodesize );
    ztype ( q ) = unhyphenated;
    fitness ( q ) = 2;
    link ( q ) = lastactive;
    breaknode ( q ) = 0;
    zlinenumber ( q ) = curlist .pgfield + 1;
    totaldemerits ( q ) = 0;
    link ( active ) = q;
    }
 
#if 0
    activewidth [ 0 ] = background [ 0 ] ; 
    activewidth [ 1 ] = background [ 1 ] ; 
    activewidth [ 2 ] = background [ 2 ] ; 
    activewidth [ 3 ] = background [ 3 ] ; 
    activewidth [ 4 ] = background [ 4 ] ; 
    activewidth [ 5 ] = background [ 5 ] ;
#else
    { register scaled *act_w = &activewidth[0];
      register scaled *bg = &background[0];

      *act_w++ = *bg++;		/* 1 */
      *act_w++ = *bg++;
      *act_w++ = *bg++;
      *act_w++ = *bg++;
      *act_w++ = *bg++;
      *act_w   = *bg;		/* 6 */
    }
#endif

    passive = 0;
    printednode = temphead;
    passnumber = 0;
    fontinshortdisplay = nullfont;
    autobreaking = true;

    curp = link ( temphead );
#ifndef OLD_PREVP
    prevp = curp;	/* glue at beginning is not a legal breakpoint */
#else
    prevp = false;
#endif

#ifdef ERWEITERUNG
  { int anzahl_zeichen = 0;
#endif

    while ( ( curp != 0 ) && ( link ( active ) != lastactive ) ) {
      if ( ischarnode(curp) ) {
	/* 867. Advance |cur_p| to the node following the present ... */
	/* !! inner loop !! */
	/* Gib' dem Compiler ein paar Tips bzgl. Registern ... */
        register internalfontnumber f, old_f = -1;
	register halfword tmp_curp = curp;
	register scaled act_width = activewidth[0];
        register SMALLmemoryword *old_width, *old_info;

#ifndef OLD_PREVP
	prevp = tmp_curp;
#else
	prevp = true;
#endif

#if 0
	do {
#ifdef ERWEITERUNG
          anzahl_zeichen++;
#endif
	  f = font ( tmp_curp );
	  act_width += zcharwidth(f, zcharinfo(f, character(tmp_curp)) );
	  tmp_curp = link ( tmp_curp );
	} while ( ischarnode( tmp_curp ) );
#else
	do {
#ifdef ERWEITERUNG
          anzahl_zeichen++;
#endif
	  f = font ( tmp_curp );
	  /* Die Breite eines einzigen Wortes (ohne Kernings, etc.) oder
	     Teil davon.  Nur in Ausnahmefaellen werden dabei Character
	     aus mehreren Fonts verwendet, deshalb ... */
	  if( f != old_f ) {
#ifdef FONTPTR
	    old_info = charbase(f);
	    old_width = widthbase(f);
#else
	    old_info = &fontinfo[charbase(f)];
	    old_width = &fontinfo[widthbase(f)];
#endif
	    old_f = f;
	  }
	  act_width += (*(old_width +
			(*(old_info + character(tmp_curp))).qqqq.b0)).cint;
	  tmp_curp = link(tmp_curp);
	} while ( ischarnode(tmp_curp) );
#endif
	curp = tmp_curp;
	activewidth[0] = act_width;
      }

      switch ( ztype ( curp ) ) {
      case hlistnode:
      case vlistnode:
      case rulenode:
	activewidth[0] += width ( curp );
#ifdef ERWEITERUNG
          anzahl_zeichen = 0;
#endif
	break;
      case whatsitnode : 
	if ( subtype ( curp ) == languagenode ) {
	  curlang = whatlang ( curp );
	  lhyf = whatlhm ( curp );
	  rhyf = whatrhm ( curp );
	}
#ifdef ERWEITERUNG
          anzahl_zeichen = 0;
#endif
	break;
      case gluenode : 
	{
	  /* 868. If node cur_p is a legal breakpoint ... */
	  if ( autobreaking ) {
#ifndef OLD_PREVP
	    if ( ischarnode ( prevp ) ) 
	      trybreak ( 0, unhyphenated ) ; 
	    else if ( precedesbreak ( prevp ) ) 
	      trybreak ( 0, unhyphenated ) ; 
#if 1  /* 3.1415 */
	    else if ( ( ztype ( prevp ) == kernnode )
		   && ( subtype( prevp ) != explicit ) )
	      trybreak ( 0, unhyphenated ) ; 
#endif
#else
	    if ( prevp )
	      trybreak ( 0, unhyphenated );
#endif
	  }

	  if ( ( shrinkorder( glueptr(curp) ) != normal )
		&& ( shrink( glueptr(curp) ) != 0 ) ) {
	    glueptr(curp) = finiteshrink ( glueptr(curp) );
	  }

	  { register halfword q;

	  q = glueptr ( curp );
	  activewidth[ 0 ] += width ( q );
	  activewidth[ 1 + stretchorder ( q ) ] += stretch ( q );
	  activewidth[ 5 ] += shrink ( q );
	  }

	  if ( secondpass && autobreaking ) {

#ifndef OLD_TRY_TO
	    try_to_hyphenate(curp);
#else
	    /* 894. Try to hyphenate the following word */
	    halfword prevs;	/* (br) */
	    register halfword s;  /* br */

	    prevs = curp;
	    s = link ( prevs );
	    if ( s != 0 ) {
	      while ( true ) {
		unsigned char c;	/* (br) */

		if ( ischarnode ( s ) ) {
		  c = character ( s );
		  hf = font ( s );
		} else if ( ztype ( s ) == ligaturenode ) 
		  if ( ligptr ( s ) == 0 ) 
		    goto lab22 ; 
		  else {
		    register halfword q;

		    q = ligptr ( s ) ; 
		    c = character ( q ) ; 
		    hf = font ( q ) ; 
		  } 
		else if ( (ztype(s) == kernnode) && (subtype(s) == normal) )
		  goto lab22 ; 
		else if ( ztype ( s ) == whatsitnode ) {
		  if ( subtype ( s ) == languagenode ) {
		    curlang = whatlang ( s ) ; 
		    lhyf = whatlhm ( s ) ; 
		    rhyf = whatrhm ( s ) ; 
		  } 
		  goto lab22 ; 
		}
		else goto lab31 ; 
		if ( lccode ( c ) != 0 ) 
		  if ( ( lccode ( c ) == c ) || ( uchyph > 0 ) ) 
		    goto lab32 ; 
		  else
		    goto lab31 ; 
lab22:		prevs = s ; 
		s = link ( prevs ) ; 
	      }
lab32:	      hyfchar = hyphenchar(hf);
	      if ( hyfchar < 0 )
		goto lab31 ; 
	      if ( hyfchar > 255 ) 
		goto lab31 ; 
	      ha = prevs ; 

	      if ( lhyf + rhyf > 63 ) 
		goto lab31 ; 

	      /* 897. Skip to node hb, putting letters into hu and hc */
	      hn = 0;
	    {
	      /* Damit es ein bisschen schneller geht, lokale Variable */
	      unsigned short l_hyf_bchar;  /* TeX 3.141 */

	      while ( true ) {
		if ( ischarnode ( s ) ) {
		  unsigned char c;	/* (br) */

		  if ( font ( s ) != hf )
		    goto lab33;

		  /* alt: c = character ( s ); */ /* TeX 3.141 */
		  /* neu: l_hyf_bchar = character(s);  c = l_hyf_bchar; */
		  c = character(s); l_hyf_bchar = c; /* c muss unsigned sein */

		  if ( lccode ( c ) == 0 )
		    goto lab33;
		  if ( hn == 63 )
		    goto lab33;
		  hb = s;
		  incr ( hn );
		  hu [ hn ] = c;
		  hc [ hn ] = lccode( c );
		  l_hyf_bchar = nonchar;   /* TeX 3.141 */
		} else if ( ztype ( s ) == ligaturenode ) {
		  /* j nur hier benoetigt, deshalb ... */
		  register smallnumber j;
		  register halfword q;

		  if ( font ( ligchar ( s ) ) != hf ) 
		    goto lab33 ; 

		  j = hn ; 
		  q = ligptr ( s ) ; 
		  if ( q > 0 )    /* TeX 3.141 */
		    l_hyf_bchar = character(q);
		  while ( q > 0 ) {
		    unsigned char c;	/* (br) */

		    c = character( q );
		    if ( lccode( c ) == 0 )
		      goto lab33;
		    if ( j == 63 )
		      goto lab33;
		    incr ( j );
		    hu [ j ] = c;
		    hc [ j ] = lccode ( c );
		    q = link( q );
		  }
		  hb = s;
		  hn = j ; 
		  if ( odd(subtype(s)) ) {  /* TeX 3.141 */
		    l_hyf_bchar = fontbchar(hf);
		  } else {
		    l_hyf_bchar = nonchar;
		  }
		} else if ( ( ztype(s) == kernnode )
			 && ( subtype(s) == normal ) ) {
		  hb = s;  /* TeX 3.141 */
		  l_hyf_bchar = fontbchar(hf);  /* TeX 3.1415 */
		} else {
		  goto lab33;
		}

		s = link ( s );
	      }
lab33: ;
	      hyf_bchar = l_hyf_bchar;
	    }

	      if ( hn < lhyf + rhyf ) 
		goto lab31;

	      while ( true ) {
		if ( ! ( ischarnode ( s ) ) ) 
		switch ( ztype ( s ) ) {
		case ligaturenode : 
		  break ; 
		case kernnode : 
		  if ( subtype ( s ) != normal ) 
		    goto lab34 ; 
		  break ; 
		case whatsitnode : 
		case gluenode : 
		case penaltynode : 
		case insnode : 
		case adjustnode : 
		case marknode : 
		  goto lab34 ; 
		  break;
		default:
		  goto lab31;
		  break;
		}
		s = link ( s );
	      }
lab34: ;
	      hyphenate ();
	    }
lab31: ;

#endif  /* OLD_TRY_TO */

	  }

#ifdef ERWEITERUNG
          anzahl_zeichen = 0;
#endif

	}
	break;

      case ligaturenode : 
	{ register internalfontnumber f;  /* (br) */

	  f = font ( ligchar ( curp ) );
	  activewidth [ 0 ] += zcharwidth ( f,
			zcharinfo ( f, character ( ligchar ( curp ) ) ) ) ;
	}

#ifdef ERWEITERUNG
	{ register halfword q = ligptr(curp);

	  while ( q > 0 ) {
	    anzahl_zeichen++;
	    q = link(q);
	  }
	}
#endif

	break;
      case discnode : 
	{ register halfword s;  /* br */

	  s = prebreak ( curp );
	  discwidth = 0;
	  if ( s == 0 ) {
#ifdef ERWEITERUNG
	    if( anzahl_zeichen > lhyf ) {
#endif
	      trybreak ( exhyphenpenalty, hyphenated );
#ifdef ERWEITERUNG
	      anzahl_zeichen = 0;
	    }
#endif
	  } else {
	    do {
	      if ( ischarnode(s) ) {
		register internalfontnumber f;  /* (br) */

#ifdef ERWEITERUNG
		anzahl_zeichen++;
#endif
		f = font(s);
		discwidth += zcharwidth (f, zcharinfo (f, character(s) ) );
	      } else switch ( ztype(s) ) {
	      case ligaturenode:
		{ register internalfontnumber f;  /* (br) */

		  f = font ( ligchar(s) );
		  discwidth += zcharwidth ( f,
			zcharinfo ( f, character( ligchar(s) ) ) ) ;
		}
#ifdef ERWEITERUNG
		{ register halfword q = ligptr(s);

		  while ( q > 0 ) {
		    anzahl_zeichen++;
		    q = link(q);
		  }
		}
#endif
		break;
	      case hlistnode:
	      case vlistnode:
	      case rulenode:
	      case kernnode:
		discwidth += width(s); 
#ifdef ERWEITERUNG
		anzahl_zeichen = 0;
#endif
		break;
	      default: 
		confusion("disc3"); /* 930 */
		break; 
	      }
	      s = link( s );
	    } while ( s != 0 );
	    activewidth[0] += discwidth;
#ifdef ERWEITERUNG
	    if( anzahl_zeichen > lhyf ) {
#endif
	      trybreak ( hyphenpenalty , hyphenated );
#ifdef ERWEITERUNG
	      anzahl_zeichen = 0;
	    }
#endif
	    activewidth[0] -= discwidth;
	  }

	{ register halfword r;

	  r = replacecount ( curp );
	  s = link ( curp );
	  while ( r > 0 ) {
	    if ( ischarnode ( s ) ) {
	      register internalfontnumber f;  /* (br) */

#ifdef ERWEITERUNG
	      anzahl_zeichen++;
#endif
	      f = font ( s );
	      activewidth[0] += zcharwidth ( f,
					zcharinfo ( f, character(s) ) );
	    } else switch ( ztype ( s ) ) {
	    case ligaturenode : 
	      { register internalfontnumber f;  /* (br) */

		f = font ( ligchar(s) );
		activewidth[0] += zcharwidth ( f,
			zcharinfo ( f, character ( ligchar(s) ) ) );
	      }
#ifdef ERWEITERUNG
	      { register halfword q = ligptr(s);

		while ( q > 0 ) {
		  anzahl_zeichen++;
		  q = link(q);
		}
	      }
#endif
	      break;
	    case hlistnode:
	    case vlistnode:
	    case rulenode:
	    case kernnode:
	      activewidth[0] += width(s);
#ifdef ERWEITERUNG
	      anzahl_zeichen = 0;
#endif
	      break;
	    default:
	      confusion("disc4"); /* 931 */
	      break;
	    }
	    decr ( r );
	    s = link ( s );
	  }
	}

#ifndef OLD_PREVP
	  prevp = curp;
#else
	  prevp = true;
#endif
	  curp = s;
	  goto lab35;
	}
	break;

      case mathnode:
	/* innerhalb einer Formel *nicht* umbrechen */
	autobreaking = ( subtype(curp) == after);

	if ( ! ischarnode ( link(curp) ) && autobreaking )
	  if ( ztype ( link(curp) ) == gluenode )
	    trybreak( 0, unhyphenated );
	activewidth[0] += width(curp);
#ifdef ERWEITERUNG
          anzahl_zeichen = 0;
#endif
	break;

      case kernnode:
#if 1  /* 3.1415 */
	if ( subtype(curp) == explicit ) {
#endif
	if ( ! ischarnode ( link(curp) ) && autobreaking )
	  if ( ztype ( link(curp) ) == gluenode )
	    trybreak( 0, unhyphenated );
#ifdef ERWEITERUNG
          anzahl_zeichen = 0;
#endif
	activewidth[0] += width(curp);
#if 1  /* 3.1415 */
	} else {
#ifdef OLD_PREVP
	prevp = true ;
#endif
	  activewidth[0] += width(curp);
	}
#endif
	break;

      case penaltynode:
	trybreak ( mem[ curp + 1 ].cint, unhyphenated );
#ifdef ERWEITERUNG
          anzahl_zeichen = 0;
#endif
	break;
      case marknode:
      case insnode:
      case adjustnode:
#ifdef ERWEITERUNG
          anzahl_zeichen = 0;
#endif
	break;
      default:
	confusion("paragraph");
	break;
      }
#ifndef OLD_PREVP
      prevp = curp;
#else
      if ( ztype ( curp ) < 9 )
	prevp = true;
      else
	prevp = false;
#endif
      curp = link ( curp );
lab35: ;
    }
#ifdef ERWEITERUNG
  }
#endif

    if ( curp == 0 ) {
      trybreak ( ejectpenalty , hyphenated );
      if ( link ( active ) != lastactive ) {
	{ register halfword r;

	  r = link ( active );
	  fewestdemerits = awfulbad;
	  do {
	    if ( ztype ( r ) != deltanode ) 
	      if ( totaldemerits ( r ) < fewestdemerits ) {
	        fewestdemerits = totaldemerits ( r );
	        bestbet = r;
	      }
	    r = link ( r );
	  } while ( r != lastactive );
	  bestline = zlinenumber( bestbet );
	}

	if ( looseness == 0 )
	  goto lab30;

	{ register halfword r;

	  r = link ( active );
	  actuallooseness = 0;
	  do {
	    if ( ztype ( r ) != deltanode ) {
	      linediff = toint ( zlinenumber ( r ) ) - toint ( bestline );
	      if ( ( (linediff < actuallooseness) && (looseness <= linediff) )
		|| ( (linediff > actuallooseness) && (looseness >= linediff) ) )
	      {
		bestbet = r;
		actuallooseness = linediff;
		fewestdemerits = totaldemerits(r);
	      } else if ( ( linediff == actuallooseness )
		       && ( totaldemerits(r) < fewestdemerits ) )
	      {
		bestbet = r;
		fewestdemerits = totaldemerits(r);
	      }
	    }
	    r = link( r );
	  } while ( r != lastactive );
	  bestline = zlinenumber ( bestbet );
	}

	if ( ( actuallooseness == looseness ) || finalpass )
	  goto lab30;
      }
    }

    cleanup_memory_removing_breaknodes();

    if ( ! secondpass ) {
#ifdef STAT
      if ( tracingparagraphs > 0 ) 
	c_printnl("@secondpass");
#endif /* STAT */
      threshold = tolerance;
      secondpass = true;
      finalpass = ( emergencystretch <= 0 );
    } else {
#ifdef STAT
      if ( tracingparagraphs > 0 ) 
	c_printnl("@emergencypass");
#endif /* STAT */
      background[1] += emergencystretch; 
      finalpass = true;
    }
  }

lab30: ;
#ifdef STAT
  if ( tracingparagraphs > 0 ) {
    enddiagnostic ( true );
    normalizeselector ();
  }
#endif /* STAT */

  postlinebreak ( finalwidowpenalty );

  cleanup_memory_removing_breaknodes();

  packbeginline = 0;
}


/* -- eof -- */
