/* Part 26: Basic scanning subroutines... */

#define EXTERN extern
#include "texd.h"

void scanleftbrace ( void )		/* 403. */
{ scanleftbrace_regmem
  register eightbits r_curcmd;

  r_curcmd = getxnbtoken(1);

  if ( r_curcmd != 1 ) {
    p_print_err( STR_H_MISSING_LEFTBRACE );
    zhelp1( STR_H_A_LEFTBRACE_MANDATORY );
    backerror();
    curtok = leftbracetoken + 123 ; 
    curcmd = 1 ; 
    curchr = 123 ; 
    incr ( alignstate ) ; 
  }
}

void scanoptionalequals ( void )	/* 405. */
{ scanoptionalequals_regmem

  (void) getxnbtoken(0);
  if ( curtok != othertoken + 61 )
    backinput ();
}

boolean scankeyword ( strnumber s )	/* 407. */
{ scankeyword_regmem 
  register long_halfword p ;
  register packedASCIIcode *pp, *endp;

  p = backuphead;
  link ( p ) = 0;

  pp   = &strpool[strstart[s]];		/* k = strstart[s]; */
  endp = &strpool[strstart[s+1]];
  while ( pp < endp ) {			/* (k < strstart[s+1]) */
    register eightbits r_curcmd;

    r_curcmd = getxtoken ();		/* *pp: strpool[k] */
    if ( curcs == 0
	&& ( (packedASCIIcode)curchr == *pp
	  || (packedASCIIcode)curchr == *pp - 32 ) ) {
      storenewtoken ( curtok );
      pp++;
    } else if ( r_curcmd != 10 || (halfword)p != backuphead ) {
      backinput ();
      if ( (halfword)p != backuphead )
	begintokenlist ( link ( backuphead ) , backedup );
      return(false);
    }
  }
  flushlist ( link ( backuphead ) );
  return(true);
}

void muerror ( void )		/* 408. */
{ muerror_regmem 

  print_err ("Incompatible glue units");
  zhelp1( STR_H_IM_GOING_MU_PT );
  error () ; 
} 


integer scaneightbitint ( void )	/* 433. */
{ scaneightbitint_regmem 
  register integer r_curval;

  r_curval = scanint (); 
  if ( r_curval < 0 || r_curval > 255 ) {
    print_err("Bad register code");
    zhelp2( STR_H_REGISTERNUMBER_BETWEEN, STR_H_I_CHANGED_TO_ZERO );
    interror ( curval );
    curval = 0L;
    return 0L;
  }
  return r_curval;
}

integer scancharnum ( void )
{ scancharnum_regmem 
  register integer r_curval;
  
  r_curval = scanint();
  if ( r_curval < 0 || r_curval > 255 ) {
    print_err("Bad character code");
    zhelp2( STR_H_CHARNUMBER_BETWEEN, STR_H_I_CHANGED_TO_ZERO );
    interror ( curval );
    curval = 0L;
    return 0L;
  }
  return r_curval;
}

integer scanfourbitint ( void )
{ scanfourbitint_regmem 
  register integer r_curval;

  r_curval = scanint();
  if ( r_curval < 0 || r_curval > 15 ) {
    print_err("Bad number");
    zhelp2( STR_H_SINCE_I_EXPECT_BETWEEN, STR_H_I_CHANGED_TO_ZERO );
    interror ( curval );
    curval = 0L; 
    return 0L;
  }
  return r_curval;
}

integer scanfifteenbitint ( void )
{ scanfifteenbitint_regmem
  register integer r_curval;

  r_curval = scanint();
  if ( r_curval < 0 || r_curval > 32767 ) {
#if 0  /* TeX 3.141 */
    print_err("Bad math code");
#else
    print_err("Bad mathchar");
#endif
    zhelp2( STR_H_NUMERICMATH_BETWEEN, STR_H_I_CHANGED_TO_ZERO );
    interror ( curval );
    curval = 0L;
    return 0L;
  }
  return r_curval;
}

integer scantwentysevenbitint ( void )
{ scantwentysevenbitint_regmem 
  register integer r_curval;

  r_curval = scanint();
  if ( r_curval < 0 || r_curval > 134217727L ) {
    print_err("Bad delimiter code");
    zhelp2( STR_H_NUMERICDEL_BETWEEN, STR_H_I_CHANGED_TO_ZERO );
    interror ( curval );
    curval = 0L;
    return 0L;
  }
  return r_curval;
}

integer scanfontident ( void )		/* 577. */
{ scanfontident_regmem
  register internalfontnumber f;
  register eightbits r_curcmd;

  r_curcmd = getxnbtoken(0);

  if ( r_curcmd == def_font )
    f = curfont;
  else if ( r_curcmd == set_font )
    f = curchr;
  else if ( r_curcmd == def_family ) {
    register halfword m;
    m = curchr;
    f = equiv( m + scanfourbitint() );
  } else {
    print_err("Missing font identifier");
    zhelp1( STR_H_I_LOOKING_FOR_CS );
    backerror();
    f = nullfont;
  }
  curval = f;
  return (integer)f;
}


integer findfontdimen ( boolean writing )	/* 578. */
{ findfontdimen_regmem
  register internalfontnumber f;
  register integer n;
  register integer r_curval;

  n = scanint ();
  f = scanfontident();
  if ( n <= 0 )
    r_curval = fmemptr;
  else {
    if ( writing && ( n <= spaceshrinkcode )
	&& ( n >= spacecode ) && ( fontglue(f) != 0 ) )
    {
      deleteglueref ( fontglue(f) );
      fontglue(f) = 0;
    }
    if ( n > fontparams(f) ) {
      if ( f < fontptr ) {
	r_curval = fmemptr ; 
      } else {
	do {
	  if ( fmemptr == fontmemsize ) 
	    overflow(11, fontmemsize);
	  fontinfo [ fmemptr ] .cint = 0 ; 
	  incr ( fmemptr ) ; 
	  incr ( fontparams(f) ) ; 
        } while ( n != fontparams(f) );
        r_curval = fmemptr - 1;
      }
    } else {
      r_curval = n + parambase(f);
    }
  }

  curval = r_curval;

  if ( r_curval == fmemptr ) {
    print_err("Font ");
    printesc ( fontidtext ( f ) );
    c_print(" has only ");
    printint ( fontparams(f) );
    c_print(" fontdimen parameters");
    zhelp1( STR_H_TO_INCREASE_FNTP );
    error();
    return curval;
  } else {
    return r_curval;
  }
}


  static void
missing_number_error(void)
{
  print_err("Missing number, treated as zero");
  zhelp1( STR_H_ANUMBER_SHOULD_HERE );
  backerror();
}


  integer
scansomethinginternal ( smallnumber level, boolean negative ) /* 413. */
{ scansomethinginternal_regmem 
  register halfword m;
  register integer r_curval;
  register schar r_curvallevel;

  m = curchr;
  switch ( curcmd ) {
  case def_code:
      r_curval = scancharnum () ; 
      if ( m == mathcodebase ) {
	r_curval = mathcode ( r_curval );
	r_curvallevel = intval;
      } else if ( m < mathcodebase ) {
	r_curval = equiv ( m + r_curval ) ; 
	r_curvallevel = intval ; 
      } else {
	r_curval = eqtb [ m + r_curval ] .cint ; 
	r_curvallevel = intval ; 
      }
      break;
  case toks_register:
  case assign_toks:
  case def_family:
  case set_font:
  case def_font:
    if ( level != tokval ) {
      missing_number_error();
      r_curval = 0 ; 
      r_curvallevel = dimenval ; 
    } else if ( curcmd <= assign_toks ) {
      if ( curcmd < assign_toks ) {
	r_curval = scaneightbitint () ;
	m = toksbase + r_curval ;
      }
      r_curval = equiv(m);
      r_curvallevel = tokval;
    } else {
      backinput ();
      r_curval = scanfontident ();
      r_curval = fontidbase + r_curval ; 
      r_curvallevel = identval ; 
    }
    break;
  case assign_int:
    r_curval = eqtb[m].cint;
    r_curvallevel = intval;
    break ; 
  case assign_dimen:
    r_curval = eqtb [ m ] .cint;
    r_curvallevel = dimenval;
    break;
  case assign_glue:
    r_curval = equiv ( m );
    r_curvallevel = glueval;
    break;
  case assign_mu_glue:
    r_curval = equiv ( m );
    r_curvallevel = muval;
    break;
  case set_aux:
    if ( abs ( curlist.modefield ) != m ) {
      p_print_err( STR_H_IMPROPER );
      printcmdchr( set_aux, m );
      zhelp2( STR_H_YOU_REFER_SPACEFACTOR, STR_H_IM_FORGETTING_ZERO );
      error ();
      r_curval = 0;
      if ( level != tokval ) {
	r_curvallevel = dimenval;
      } else {
	r_curvallevel = intval;
      }
    } else if ( m == vmode ) {
      r_curval = curlist .auxfield .cint;
      r_curvallevel = dimenval;
    } else {
      r_curval = curlist .auxfield .hh .v.LH;
      r_curvallevel = intval;
    }
    break;
  case set_prev_graf: 
    if ( curlist .modefield == 0 ) {
      r_curval = 0;
      r_curvallevel = intval;
    } else {	
      register integer p;

      nest [ nestptr ] = curlist;
      p = nestptr;
      while ( abs ( nest [ p ] .modefield ) != vmode )
	decr ( p );
      r_curval = nest [ p ] .pgfield;
      r_curvallevel = intval;
    }
    break ;
  case set_page_int:
    if ( m == 0 ) 
	r_curval = deadcycles ; 
    else
	r_curval = insertpenalties ; 
    r_curvallevel = intval ; 
    break ; 
  case set_page_dimen:
    if ( pagecontents == 0 && !outputactive )
      if ( m == 0 )
	r_curval = maxdimen;
      else
	r_curval = 0;
    else
      r_curval = pagesofar [ m ];
    r_curvallevel = dimenval;
    break ; 
  case set_shape:
    if ( parshapeptr == 0 ) 
	r_curval = 0;
    else
	r_curval = info ( parshapeptr );
    r_curvallevel = intval ; 
    break ; 
  case set_box_dimen:
    r_curval = scaneightbitint ();
    if ( box ( r_curval ) == 0 )
	r_curval = 0;
    else
	r_curval = mem [ box ( r_curval ) + m ] .cint;
    r_curvallevel = dimenval ; 
    break ; 
  case chargiven:
  case math_given:
    r_curval = curchr;
    r_curvallevel = intval;
    break ; 
  case assign_font_dimen:
    r_curval = findfontdimen ( false );
    fontinfo[fmemptr].cint = 0;
    r_curval = fontinfo [ r_curval ] .cint;
    r_curvallevel = dimenval;
    break;
  case assign_font_int:
    r_curval = scanfontident ();
    if ( m == 0 ) {
	r_curval = hyphenchar(r_curval);
    } else {	  
	r_curval = skewchar(r_curval);
    }
    r_curvallevel = intval ; 
    break ; 
  case register_cmd:
    {
      r_curval = scaneightbitint ();
      switch ( m ) {
      case intval :
	r_curval = count ( r_curval );
	break ; 
      case dimenval : 
	r_curval = dimen ( r_curval ) ;
	break ;
      case glueval : 
	r_curval = skip ( r_curval ) ;
	break ; 
      case muval : 
	r_curval = muskip ( r_curval ) ;
	break ; 
      }
      r_curvallevel = m ; 
    }
    break ; 
  case last_item:
    if ( curchr > glueval ) {
      if ( curchr == inputlinenocode ) 
	r_curval = line ; 
      else
	r_curval = lastbadness ; 
      r_curvallevel = intval ; 
    } else {	
      if ( curchr == glueval )
	r_curval = zeroglue ; 
      else
	r_curval = 0 ; 
      r_curvallevel = curchr;
      if ( ! ischarnode ( curlist.tailfield ) && ( curlist.modefield != 0 ) ) {
	switch ( curchr ) {
	case intval : 
	  if ( ztype ( curlist .tailfield ) == penaltynode )
	    r_curval = mem [ curlist .tailfield + 1 ] .cint ; 
	  break ; 
	case dimenval : 
	  if ( ztype ( curlist .tailfield ) == kernnode ) 
	    r_curval = width ( curlist .tailfield ) ; 
	  break ; 
	case glueval : 
	  if ( ztype ( curlist .tailfield ) == gluenode ) {
	    r_curval = glueptr ( curlist .tailfield ) ; 
	    if ( subtype ( curlist .tailfield ) == muglue ) 
	      r_curvallevel = muval ; 
	  }
	  break;
	}
      } else if ( curlist .modefield == vmode
	       && curlist.tailfield == curlist.headfield ) {
	switch ( curchr ) {
	case intval : 
	  r_curval = lastpenalty ; 
	  break ; 
	case dimenval : 
	  r_curval = lastkern ; 
	  break ; 
	case glueval : 
	  if ( lastglue != maxhalfword ) 
	    r_curval = lastglue ; 
	  break;
	}
      }
    }
    break;
  default: 
    {
      p_print_err( STR_H_YOUCANTUSE );
      printcmdchr( curcmd, curchr );
      print( STR_H_AFTER );
      printesc( STR_THE );
      zhelp1( STR_H_IM_FORGETTING_ZERO );
      error () ; 
      r_curval = 0;
      if ( level != tokval ) {
	r_curvallevel = dimenval;
      } else {
	r_curvallevel = intval;
      }
    }
    break;
  }

  while ( r_curvallevel > level ) {      
    if ( r_curvallevel == glueval )
      r_curval = width ( r_curval ) ; 
    else if ( r_curvallevel == muval ) {
      muerror ();
      /* Possible insertions in error()... reload registers */
      r_curval = curval; r_curvallevel = curvallevel;
    }
    decr ( r_curvallevel );
  }
  if ( negative ) {
    if ( r_curvallevel >= glueval ) {
      curval = r_curval = newspec ( r_curval );
      width ( r_curval ) = - (integer) width ( r_curval );
      stretch ( r_curval ) = - (integer) stretch ( r_curval );
      shrink ( r_curval ) = - (integer) shrink ( r_curval );
    } else {
      r_curval = - (integer) r_curval;
    }
  } else if ( r_curvallevel >= glueval && r_curvallevel <= muval ) {
    addglueref ( r_curval );
  }

  /* Update global values... */
  curvallevel = r_curvallevel;
  curval = r_curval;

  return r_curval;
}


  integer
scanint ( void )		/* 440. */
{ scanint_regmem
  boolean negative;
  register eightbits r_curcmd;

  radix = 0;
  negative = false;
  do {
    r_curcmd = getxnbtoken(0);
    if ( curtok == othertoken + 45 ) {
      negative = ! negative ; 
      curtok = othertoken + 43 ;
    }
  } while ( curtok == othertoken + 43 );

  if ( curtok == alphatoken ) {
    r_curcmd = gettoken();
    if ( curtok < cstokenflag ) {
      curval = curchr ;
      if ( r_curcmd <= 2 ) 
	if ( r_curcmd == 2 ) 
	  incr ( alignstate ) ; 
	else
	  decr ( alignstate );
    } else if ( curtok < cstokenflag + singlebase )
      curval = curtok - cstokenflag - activebase;
    else
      curval = curtok - cstokenflag - singlebase;

    if ( curval > 255 ) {
      print_err("Improper alphabetic constant");
      zhelp1( STR_H_ONECHAR_CS_BELONGS );
      curval = 48;
      backerror ();
    } else {
      if ( (eightbits)getxtoken() != 10 )
        backinput();
    }

  } else if ( r_curcmd >= min_internal && r_curcmd <= max_internal )

    scansomethinginternal ( intval , false ) ; 

  else {
      
    register integer m;
    boolean vacuous;
    boolean OKsofar;

    radix = 10 ; 
    m = 214748364L ; 
    if ( curtok == octaltoken ) {
      radix = 8 ; 
      m = 268435456L ; 
      getxtoken () ; 
    } else if ( curtok == hextoken ) {
      radix = 16 ; 
      m = 134217728L ; 
      getxtoken () ; 
    } 
    OKsofar = true;
    vacuous = true;
    curval = 0;

    while ( true ) {
      register smallnumber d;

      if ( ( curtok < zerotoken + radix ) && ( curtok >= zerotoken )
		&& ( curtok <= zerotoken + 9 ) )
	d = curtok - zerotoken ; 
      else if ( radix == 16 ) 
	if ( ( curtok <= Atoken + 5 ) && ( curtok >= Atoken ) ) 
	  d = curtok - Atoken + 10 ; 
 	else if ( ( curtok <= otherAtoken + 5 ) && ( curtok >= otherAtoken ) ) 
	  d = curtok - otherAtoken + 10;
	else goto lab30;
      else
	goto lab30;
      vacuous = false;
      if ( curval >= m && ( curval > m || d > 7 || ( radix != 10 ) ) ) {
	if ( OKsofar ) {
	  print_err("Number too big");
	  zhelp1( STR_H_ICAN_ONLY_GOUPTO );
	  error();
	  curval = infinity;
	  OKsofar = false;
	}
      } else
	curval = curval * radix + d;
      getxtoken();
    }

lab30:
    if ( vacuous ) {
      missing_number_error();
    } else if ( curcmd != 10 ) 
      backinput () ;
  }
  if ( negative )
    curval = - (integer) curval;

  return curval;
}

static char *Illegal_Unit_of_Measure = "Illegal unit of measure (";

  integer
scandimen ( boolean mu, boolean inf, boolean shortcut )		/* 448. */
{ scandimen_regmem
  boolean negative;
  register integer f;
  register integer r_curval;

  f = 0 ; 
  aritherror = false ; 
  curorder = normal ; 
  negative = false ; 
  if ( ! shortcut ) {
    negative = false ; 
    do {
      (void) getxnbtoken(0);
      if ( curtok == othertoken + 45 ) {
	negative = ! negative ;
	curtok = othertoken + 43 ;
      }
    } while ( curtok == othertoken + 43 );

    if ( curcmd >= min_internal && curcmd <= max_internal ) {
      if ( mu ) {
	r_curval = scansomethinginternal ( muval , false ) ; 
	if ( curvallevel >= glueval ) {
	  register scaled v;

	  v = width ( r_curval ) ; 
	  deleteglueref ( r_curval ) ; 
	  curval = r_curval = v ; 
	}
	if ( curvallevel == muval ) 
	  goto lab89 ; 
	if ( curvallevel != intval ) 
	  muerror ();
      } else {
	r_curval = scansomethinginternal ( dimenval , false ) ; 
	if ( curvallevel == dimenval ) 
	  goto lab89 ; 
      }
    } else {		/* not internal.. */
      backinput ();
      if ( curtok == continentalpointtoken ) 
	curtok = pointtoken ; 
      if ( curtok != pointtoken ) 
	scanint ();
      else {
	radix = 10;
	curval = 0;
      }
      if ( curtok == continentalpointtoken ) 
	curtok = pointtoken ;

      if ( radix == 10 && curtok == pointtoken ) {
	register long_halfword p;
	register smallnumber k; 

	k = 0; p = 0;
	gettoken();

	while ( true ) {
	  getxtoken () ; 
	  if ( ( curtok > zerotoken + 9 ) || ( curtok < zerotoken ) ) 
	    goto lab31 ; 
	  if ( k < 17 ) {
	    register long_halfword q;

	    q = getavail () ; 
	    link ( q ) = p ; 
	    info ( q ) = curtok - zerotoken ; 
	    p = q ; 
	    incr ( k ) ; 
	  }
	}

lab31:
	{ short kk;
#if 0
	for( kk = k ; kk >= 1 ; --kk ) {
	  register long_halfword q;

	  dig [ kk - 1 ] = info ( p ) ; 
	  q = p ; p = link ( p ) ; freeavail ( q ) ; 
	}
	f = rounddecimals ( k ) ; 
#else
	f = 0;
	kk = k;
	while ( kk > 0 ) {
	  register long_halfword q;

	  decr ( kk ) ; 
	  f = ( f + ((schar) info(p)) * two ) / 10; 
	  q = p; p = link(p); freeavail(q);
	}
	f = (f + 1) / 2;
#endif
	}
	if ( curcmd != 10 ) 
	  backinput () ;
      }	/* end... scan decimal fraction */
    }	/* end... not internal quantity */
  }	/* end... !shortcut */

  if ( curval < 0 ) {
    negative = ! negative;
    curval = - (integer) curval;
  }

  if ( inf ) {
    if ( scankeyword( STR_FIL ) ) {	/* "fil" */
      curorder = fil;
      while ( scankeyword( 108 ) ) {	/* "l" */
	if ( curorder == filll ) {
	  print_err(Illegal_Unit_of_Measure);
	  c_print("replaced by filll)");
	  zhelp1( STR_H_I_DDDONT_GO );
	  error ();
	} else
	  incr ( curorder );
      }
      goto lab88;
    }
  }


  /* 455. Scan for units that are internal dimensions.. */
  { integer savecurval;
    halfword r_curcmd;
    scaled v;

  savecurval = curval ; 
  r_curcmd = getxnbtoken(0);
  if ( r_curcmd < min_internal || r_curcmd > max_internal )
    backinput ();
  else {
    if ( mu ) {
      scansomethinginternal ( muval , false );
      if ( curvallevel >= glueval ) {
	v = width ( curval ) ; 
	deleteglueref ( curval );
	curval = v;
      }
      if ( curvallevel != muval ) 
	muerror () ; 
    } else
      scansomethinginternal ( dimenval , false ) ; 
    v = curval;
    goto lab40 ; 
  }

  if ( mu )
    goto lab45;

  if ( scankeyword( STR_EM ) )
    v = quad ( curfont );
  else if ( scankeyword( STR_EX ) )
    v = xheight ( curfont );
  else
    goto lab45;
  {
    if ( (eightbits)getxtoken() != 10 )
      backinput ();
  }
lab40:
  r_curval = multandadd (savecurval, v, xnoverd (v, f, 65536L), 1073741823L);
  goto lab89;
 }


lab45:
  if ( mu ) {
    if ( scankeyword( STR_MU ) ) {	/* "mu" */
      goto lab88;
    } else {
      print_err(Illegal_Unit_of_Measure);
      c_print("mu inserted)");
      zhelp2( STR_H_UNIT_IN_MATHGLUE, STR_H_TO_RECOVER_GRACEF );
      error ();
      goto lab88;
    }
  }

  if ( scankeyword( STR_TRUE ) ) {		/* "true" */
    preparemag ();
    if ( mag != 1000 ) {
      curval = xnoverd ( curval , 1000 , mag );
      f = ( 1000 * f + 65536L * remainder ) / mag;
      curval = curval + ( f / 65536L );
      f = f % 65536L;
    }
  }

  if ( scankeyword( STR_PT ) )		/* "pt" */
    goto lab88;

 { register integer num, denom;

  if ( scankeyword( STR_IN ) ) {
    num = 7227;	denom = 100;
  } else if ( scankeyword( STR_PC ) ) {
    num = 12;	denom = 1;
  } else if ( scankeyword( STR_CM ) ) {
    num = 7227;	denom = 254;
  } else if ( scankeyword( STR_MM ) ) {
    num = 7227;	denom = 2540;
  } else if ( scankeyword( STR_BP ) ) {
    num = 7227;	denom = 7200;
  } else if ( scankeyword( STR_DD ) ) {
    num = 1238;	denom = 1157;
  } else if ( scankeyword( STR_CC ) ) {
    num = 14856; denom = 1157;
  } else if ( scankeyword( STR_SP ) )
    goto lab30;
  else {
    print_err(Illegal_Unit_of_Measure);
    c_print("pt inserted)");
    zhelp2( STR_H_DIMENSIONS_EMEX, STR_H_TO_RECOVER_GRACEF );
    error () ; 
    goto lab32 ; 
  } 
  curval = xnoverd ( curval , num , denom ) ; 
  f = ( num * f + 65536L * remainder ) / denom ; 
  curval = curval + ( f / 65536L ) ; 
  f = f % 65536L ;
lab32: ;
 }

lab88:
  if ( curval >= 16384 ) 
    aritherror = true;
  else
    curval = curval * unity + f;

lab30: ; 
  {
    if ( (eightbits)getxtoken() != 10 )
      backinput();
  } 
  r_curval = curval;

lab89:
  if ( aritherror || ( abs ( r_curval ) >= 1073741824L ) ) {
    print_err("Dimension too large");
    zhelp1( STR_H_ICANT_WORK_BIGGER );
    error();
    r_curval = maxdimen;
    aritherror = false;
  }
  if ( negative )
    r_curval = - (integer) r_curval;

  curval = r_curval;
  return r_curval;
}


  long_halfword
scanglue ( smallnumber level )		/* 461. */
{ scanglue_regmem
  boolean negative;
  register long_halfword q;
  boolean mu;
  register eightbits r_curcmd;
  register integer r_curval;

  negative = false ; 
  do {
    r_curcmd = getxnbtoken(0);
    if ( curtok == othertoken + 45 ) {
      negative = ! negative ; 
      curtok = othertoken + 43 ; 
    }
  } while ( curtok == othertoken + 43 ) ; 

  mu = ( level == muval );
  if ( r_curcmd >= min_internal && r_curcmd <= max_internal ) {
    r_curval = scansomethinginternal ( level , negative );
    if ( curvallevel >= glueval ) {
      if ( curvallevel != level ) {
	muerror ();
	r_curval = curval;
      }
      return r_curval; 
    }
    if ( curvallevel == intval ) 
      r_curval = scandimen ( mu , false , true );
    else if ( level == muval ) {
      muerror ();
      r_curval = curval;
    }
  } else {
    backinput () ; 
    r_curval = scandimen ( mu, false, false );
    if ( negative )
      r_curval = - (integer) r_curval;
  }

  q = newspec ( zeroglue );
  width ( q ) = r_curval;

  if ( scankeyword( STR_PLUS ) ) {		/* "plus" */
    stretch ( q ) = scandimen ( mu , true , false );
    stretchorder ( q ) = curorder;
  }
  if ( scankeyword( STR_MINUS ) ) {		/* "minus" */
    shrink ( q ) = scandimen ( mu , true , false );
    shrinkorder ( q ) = curorder ; 
  }

  curval = q;
  return q;
}

long_halfword scanrulespec ( void )
{ scanrulespec_regmem 
  register long_halfword q;

  q = newrule ();
  if ( curcmd == 35 )
    width ( q ) = defaultrule;
  else {
    height ( q ) = defaultrule;
    depth ( q ) = 0;
  }

lab21:
  if ( scankeyword( STR_WIDTH ) ) {
    width(q) = scandimen ( false , false , false );
    goto lab21;
  }
  if ( scankeyword( STR_HEIGHT ) ) {
    height(q) = scandimen ( false , false , false );
    goto lab21;
  }
  if ( scankeyword( STR_DEPTH ) ) {
    depth(q) = scandimen ( false , false , false );
    goto lab21 ; 
  }

  return(q);
}

/* -- end -- */
