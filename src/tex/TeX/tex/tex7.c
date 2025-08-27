#define EXTERN extern
#include "texd.h"


smallnumber normmin ( integer h )
{ normmin_regmem

  if ( h <= 0 ) 
    return(1);

  if ( h >= 63 )
   return(63);

  return(h);
} 


void newgraf ( boolean indented )
{ newgraf_regmem 

  curlist .pgfield = 0;
  if ( curlist.modefield == vmode || curlist .headfield != curlist .tailfield )
    tailappend ( newparamglue ( parskipcode ) ) ; 
#if 0  /* 3.1415 */
  curlist .lhmfield = normmin ( lefthyphenmin ) ; 
  curlist .rhmfield = normmin ( righthyphenmin ) ; 
#endif
  pushnest ();
  curlist .modefield = hmode;
  curlist .auxfield .hh .v.LH = 1000;
#if 0  /* 3.1415 */
  curlist .auxfield .hh .v.RH = 0;
#else
  if ( language <= 0 || language > 255 )
    curlang = 0;
  else
    curlang = language ; 
  curlist .auxfield .hh .v.RH = curlang;
  curlist .pgfield = ( normmin ( lefthyphenmin ) * 0x40
			+ normmin ( righthyphenmin ) ) * 0x10000L + curlang;
#endif
  if ( indented ) {
    curlist .tailfield = newnullbox () ; 
    link ( curlist .headfield ) = curlist .tailfield ; 
    width ( curlist .tailfield ) = parindent ; 
  }
  if ( everypar != 0 )
    begintokenlist( everypar, everypartext );
  if ( nestptr == 1 )
    buildpage();
}


void indentinhmode (  boolean indented /* void */ )
{ indentinhmode_regmem 
  register long_halfword p;

  if ( indented /* curchr > 0 */ ) {
    p = newnullbox ();
    width ( p ) = parindent;
    if ( abs ( curlist .modefield ) == hmode )
      curlist .auxfield .hh .v.LH = 1000;
    else {
      register long_halfword q;

      q = newnoad ();
      mathtype ( nucleus ( q ) ) = subbox;
      info ( nucleus ( q ) ) = p;
      p = q;
    }
    tailappend ( p );
  }
}


void headforvmode ( void )
{ headforvmode_regmem 

  if ( curlist .modefield < 0 ) {
    if ( curcmd != 36 )
      offsave();
    else {
      p_print_err( STR_H_YOUCANTUSE );
      printesc( STR_HRULE );
      c_print("' here except with leaders");
      zhelp1( STR_H_TOPUTAHORIZONTAL );
      error ();
    }
  } else {
    backinput ();
    curtok = partoken;
    backinput ();
    tokentype = inserted;
  }
}


void endgraf ( void )
{ endgraf_regmem 

  if ( curlist .modefield == hmode ) {
    if ( curlist .headfield == curlist .tailfield ) 
      popnest ();
    else
      linebreak ( widowpenalty );
#ifdef TEXXET
    if( LR_save != 0 ) {
	flushlist(LR_save);
	LR_save = 0;
    }
#endif
    normalparagraph();
    errorcount = 0;
  }
}


void begininsertoradjust ( void )
{ begininsertoradjust_regmem 
  register integer r_curval;

  if ( curcmd == 38 ) 
    curval = r_curval = 255;
  else {
    r_curval = scaneightbitint ();
    if ( r_curval == 255 ) {
      print_err("You can't ");
      printesc( STR_INSERT );
      printint ( 255 ) ; 
      zhelp1( STR_H_IMCHANGINGTOINSERT );
      error () ; 
      curval = r_curval = 0;
    }
  }
  saved ( 0 ) = r_curval;
  incr ( saveptr );
  newsavelevel( insertgroup );
  scanleftbrace();
  normalparagraph();
  pushnest();
  curlist .modefield = -vmode;
  curlist .auxfield .cint = ignoredepth;
}


void makemark ( void )
{
  scantoks( false, true );

 {makemark_regmem 
  register long_halfword p;

  p = getnode ( smallnodesize );
  /* ztype ( p ) = marknode ; subtype ( p ) = 0 ; */
  set_type_subtype(p, marknode, 0);

  markptr ( p ) = defref;
  link ( curlist .tailfield ) = p;
  curlist .tailfield = p;
 }
}


void appendpenalty ( void )
{ appendpenalty_regmem 
  register integer r_curval;

  r_curval = scanint ();
  tailappend ( newpenalty ( r_curval ) );
  if ( curlist .modefield == vmode )
    buildpage ();
}


void deletelast ( void )
{ deletelast_regmem

  if ( curlist.modefield == vmode && curlist.tailfield == curlist.headfield ) {
    if ( ( curchr != gluenode ) || ( lastglue != maxhalfword ) ) {
      youcant();
#ifdef NEW_HELP
      if ( curchr == kernnode )
	zhelp2( STR_H_SORRY_CANTTAKETHINGS, STR_H_TRY_KERNLASTSKIP );
      else if ( curchr != gluenode ) 
	zhelp2( STR_H_SORRY_CANTTAKETHINGS, STR_H_PERHAPSYOUCAN_OUTPUT );
      else
	zhelp2( STR_H_SORRY_CANTTAKETHINGS, STR_H_TRY_VSKIPLASTSKIP );
#else
      zhelp2 ( STR_H_SORRY_CANTTAKETHINGS, STR_H_TRY_VSKIPLASTSKIP );
      if ( curchr == kernnode ) 
        helpline [ 0 ] = ( STR_H_TRY_KERNLASTSKIP );
      else if ( curchr != gluenode ) 
        helpline [ 0 ] = ( STR_H_PERHAPSYOUCAN_OUTPUT );
#endif
      error () ; 
    }
  } else {
    if ( ! ischarnode ( curlist .tailfield ) ) 
    if ( ztype ( curlist .tailfield ) == curchr ) {
      register halfword p, q;

      q = curlist .headfield ; 
      do {
	p = q;
	if ( ! ischarnode ( q ) ) 
	if ( ztype ( q ) == discnode ) {
	  register quarterword m;

	  for( m = replacecount ( q ) ; m >= 1 ; --m ) {
	    p = link ( p );
	  }
	  if ( p == curlist .tailfield ) 
	    return;
	}
	q = link ( p ) ; 
      } while( q != curlist.tailfield );
      link ( p ) = 0;
      flushnodelist ( curlist .tailfield );
      curlist .tailfield = p;
    }
  }
}


void unpackage ( void )
{ unpackage_regmem
  register long_halfword p;
  register schar c;

  c = curchr;
  p = box( scaneightbitint() );
  if ( p == 0 )
    return;

  if ( ( abs ( curlist .modefield ) == mmode )
  || ( ( abs ( curlist .modefield ) == vmode )
    && ( ztype ( p ) != vlistnode ) )
  || ( ( abs ( curlist .modefield ) == hmode )
    && ( ztype ( p ) != hlistnode ) ) )
  {
    print_err("Incompatible list can't be unboxed");
    zhelp1( STR_H_SORRYPANDORA );
    error();
    return;
  }

#if 0
  if ( c == 1 ) 
    link ( curlist .tailfield ) = copynodelist ( listptr ( p ) ) ; 
  else {
    link ( curlist .tailfield ) = listptr ( p ) ; 
    box ( curval ) = 0 ; 
    freenode ( p , boxnodesize ) ; 
  }
#ifdef ERW_LANGUAGE
  if ( abs( curlist .modefield ) == hmode ) {
    register halfword p = 0;
    while ( link ( curlist .tailfield ) != 0 ) {
      curlist .tailfield = link ( curlist .tailfield ) ;
      if (ischarnode(curlist .tailfield))
	;
      else {
	if (ztype(curlist .tailfield) == whatsitnode &&
	    subtype(curlist .tailfield) == languagenode) {
	  p = curlist .tailfield;
	}
      }
    }
    /* Abpruefen, ob language node in Box war... */
    if( p != 0 ) {
      if( whatlang(p) != curlist .auxfield .hh .v.RH ) {
        /* Update `clang' */
	curlist .auxfield .hh .v.RH = whatlang(p);
	/* Insert additional languagenode, if \language != clang */
	fixlanguage();
      }
    }
  } else {
    while ( link ( curlist .tailfield ) != 0 )
      curlist .tailfield = link ( curlist .tailfield ) ;
  }
#else
  while ( link ( curlist .tailfield ) != 0 )
    curlist .tailfield = link ( curlist .tailfield ) ;
#endif


#else

  /* schneller geht die while-Schleife zum Schluss, wenn man `tail' in
   * einer lokalen Variable haelt...
   */
  { register halfword ctf = curlist.tailfield;

    if ( c == 1 ) 
      link(ctf) = copynodelist ( listptr(p) );
    else {
      link(ctf) = listptr(p);
      box ( curval ) = 0;
      freenode ( p, boxnodesize );
    }
#ifdef ERW_LANGUAGE
    if ( abs( curlist .modefield ) == hmode ) {
      if( link(ctf) != 0 ) {
	register halfword p = 0;
	do {
	  ctf = link( ctf );
	  if (ischarnode(ctf))
	    ;
	  else {
	    if (ztype(ctf) == whatsitnode && subtype(ctf) == languagenode) {
	      p = ctf;
	    }
	  }
	} while( link(ctf) != 0 );
	curlist.tailfield = ctf;
	/* Abpruefen, ob... */
	if( p != 0 ) {
	  if( whatlang(p) != curlist .auxfield .hh .v.RH ) {
	    /* Update `clang' */
	    curlist .auxfield .hh .v.RH = whatlang(p);
	    /* Insert additional languagenode, if \language != clang */
	    fixlanguage();
	  }
	}
      }
    } else {
      if( link(ctf) != 0 ) {
	do {
	  ctf = link( ctf );
	} while( link(ctf) != 0 );
	curlist.tailfield = ctf;
      }
    }
#else
    if( link(ctf) != 0 ) {
      do {
	ctf = link( ctf );
      } while( link(ctf) != 0 );
      curlist.tailfield = ctf;
    }
#endif  /* ERW_LANGUAGE */
  }
#endif
}


void appenditaliccorrection ( void )
{ appenditaliccorrection_regmem
  register halfword p;
  register internalfontnumber f;

  if ( curlist .tailfield != curlist .headfield ) {
    if ( ischarnode ( curlist .tailfield ) )
	p = curlist .tailfield ; 
    else if ( ztype ( curlist .tailfield ) == ligaturenode ) 
	p = ligchar ( curlist .tailfield ) ; 
    else
	return;
    f = font ( p );
    tailappend ( newkern ( zcharitalic ( f, zcharinfo(f, character(p)) ) ) );
    subtype ( curlist .tailfield ) = explicit ; 
  }
}


void appenddiscretionary ( void )
{ appenddiscretionary_regmem
  register integer c;

  tailappend ( newdisc () );
  if ( curchr == 1 ) {
    c = hyphenchar(curfont);
    if ( c >= 0 && c < 256 )
      prebreak ( curlist .tailfield ) = newcharacter( curfont, c );
  } else {
    incr ( saveptr );
    saved ( -1 ) = 0;
    newsavelevel ( discgroup );
    scanleftbrace ();
    pushnest();
    curlist .modefield = -hmode;
    curlist .auxfield .hh .v.LH = 1000;
  }
}


void builddiscretionary ( void )
{ builddiscretionary_regmem 
  register halfword p, q;
  register integer n;

  unsave ();

  n = 0;
  q = curlist .headfield;
  p = link ( q );
  while ( p != 0 ) {
    if( !ischarnode(p) && ( ztype(p) > rulenode )
	&& ( ztype(p) != kernnode ) && ( ztype(p) != ligaturenode ) ) {
      print_err("Improper discretionary list");
      zhelp1( STR_H_DISCLISTS_MUST );
      error();
      begindiagnostic();
      c_printnl("The following discretionary sublist has been deleted:");
      showbox ( p );
      enddiagnostic ( true );
      flushnodelist ( p );
      link ( q ) = 0;
      goto lab30;
    }
    q = p;
    p = link ( q );
    incr ( n );
  }

lab30:
  p = link ( curlist .headfield );
  popnest ();
  switch ( saved ( -1 ) ) {
  case 0:
    prebreak ( curlist .tailfield ) = p;
    break;
  case 1:
    postbreak ( curlist .tailfield ) = p;
    break;
  case 2:
    {
      if ( ( n > 0 ) && ( abs ( curlist .modefield ) == mmode ) ) {
	print_err("Illegal math ");
	printesc( STR_DISCRETIONARY );
	zhelp1( STR_H_SORRY_THIRDPARTOFDISC );
	flushnodelist ( p );
	n = 0;
	error();
      } else
	link ( curlist .tailfield ) = p;
      if ( n <= maxquarterword )
	replacecount ( curlist .tailfield ) = n;
      else {
	print_err("Discretionary list is too long");
	zhelp1( STR_H_WOW_INEVERTHOUGHT );
	error();
      }
      if ( n > 0 )
	curlist .tailfield = q;
      decr( saveptr );
      return;
    }
    break ; 
  }

  incr ( saved ( -1 ) );
  newsavelevel ( discgroup );
  scanleftbrace();
  pushnest();
  curlist .modefield = -hmode;
  curlist .auxfield .hh .v.LH = 1000;
}


void makeaccent ( void )
{ makeaccent_regmem
  real s, t;
  register halfword p, q;
  register internalfontnumber f;
  register scaled a, h, x, w, delta;
  fourquarters i;

  { register integer r_curval;

    r_curval = scancharnum ();
    f = curfont;
    p = newcharacter( f, r_curval );
  }

  if ( p != 0 ) {
    x = xheight ( f ) ; 
    s = slant ( f ) / ((double) 65536.0 ) ; 
    a = zcharwidth ( f ,  zcharinfo ( f ,  character ( p ) ) ) ; 

    doassignments();

    f = curfont ; 
    if ( curcmd == 11 || curcmd == 12 || curcmd == 68 ) {
      q = newcharacter ( f , curchr );
    } else if ( curcmd == 16 ) {
      q = newcharacter( f, scancharnum() );
    } else {
      q = 0;
      backinput();
    }

    if ( q != 0 ) {
      t = slant ( f ) / ((double) 65536.0 );
      i = zcharinfo( f, character ( q ) );
      w = zcharwidth( f, i ) ; 
      h = zcharheight( f, heightdepth ( i ) );
      if ( h != x ) {
	p = hpack( p, 0, 1 );
	shiftamount ( p ) = x - h;
      }
      delta = round ( ( w - a ) / ((double) 2.0 ) + h * t - x * s ) ; 
      { register long_halfword r;
	r = newkern ( delta );
	subtype ( r ) = acckern;
	link ( curlist .tailfield ) = r;
	link ( r ) = p;
      }
      curlist .tailfield = newkern ( - (integer) a - delta );
      subtype ( curlist .tailfield ) = acckern;
      link ( p ) = curlist .tailfield;
      p = q;
    }
    link ( curlist .tailfield ) = p;
    curlist .tailfield = p;
    curlist .auxfield .hh .v.LH = 1000;
  }
}


static char err_misplaced[] = "Misplaced "; /* 1107 */

void alignerror ( void )
{ alignerror_regmem 

  if ( abs ( alignstate ) > 2 ) {
    print_err( err_misplaced );
    printcmdchr ( curcmd , curchr ) ; 
    if ( curtok == tabtoken + 38 ) {
      zhelp3( STR_H_ICANTFIGURE_TABMARK, STR_H_HERE_AMPERSAND,
		STR_H_UPABOVE_PREVALIGN );
    } else {
      zhelp3( STR_H_ICANTFIGURE_TABMARK, STR_H_ORCRORSPAN,
		STR_H_UPABOVE_PREVALIGN );
    }
    error();
  } else {
    backinput();
    if ( alignstate < 0 ) {
      p_print_err( STR_H_MISSING_LEFTBRACE );
      incr ( alignstate );
      curtok = leftbracetoken + 123;
    } else {
      print_err("Missing } inserted");
      decr ( alignstate );
      curtok = rightbracetoken + 125;
    }
    zhelp1( STR_H_IVEPUTIN_TOFIX );
    inserror();
  }
}


void noalignerror ( void )
{ noalignerror_regmem 

  print_err( err_misplaced );
  printesc( STR_NOALIGN );
  zhelp2( STR_H_IEXPECT_NOALIGN, STR_H_AN_ALIGNMENT );
  error();
}


void omiterror ( void )
{ omiterror_regmem 

  print_err( err_misplaced );
  printesc( STR_OMIT );
  zhelp2( STR_H_IEXPECT_OMIT, STR_H_AN_ALIGNMENT );
  error();
}


void doendv ( void )
{ doendv_regmem 

  if ( curgroup == aligngroup ) {
    endgraf ();
    if ( fincol () )
      finrow();
  } else {
    offsave();
  }
}


void cserror ( void )
{ cserror_regmem 

  print_err("Extra ");
  printesc( STR_ENDCSNAME );
  zhelp1( STR_H_IMIGNORING_CSNAME );
  error();
}


void pushmath ( groupcode c )
{ pushmath_regmem 

  pushnest();
  curlist .modefield = -mmode;
  curlist .auxfield .cint = 0;
  newsavelevel ( c );
}


  static void
go_into_ordinary_math(void)		/* added (br) */
{ gointoordinarymath_regmem

  pushmath ( mathshiftgroup );
  eqworddefine ( intbase + curfamcode, -1 );
  if ( everymath != 0 )
    begintokenlist ( everymath, everymathtext );
}


void initmath ( void )
{/* 21 40 45 30 */ initmath_regmem 
  register scaled w  ; 
  register scaled l  ; 
  register scaled s  ; 
  register halfword p  ; 
  register halfword q  ; 
  register internalfontnumber f  ; 
  register integer n  ; 
  register scaled v  ; 
  register scaled d  ; 

  gettoken ();
  if ( ( curcmd == 3 ) && ( curlist .modefield > 0 ) ) {
    if ( curlist .headfield == curlist .tailfield ) {
      popnest ();
      w = - (integer) maxdimen;
    } else {
      linebreak ( displaywidowpenalty ) ; 
      v = shiftamount ( justbox ) + 2 * quad ( curfont ) ; 
      w = - (integer) maxdimen ; 
      p = listptr ( justbox ) ; 
      while ( p != 0 ) {
lab21:
	if ( ischarnode ( p ) ) {
	  f = font ( p ) ; 
	  d = zcharwidth ( f ,  zcharinfo ( f ,  character ( p ) ) ) ; 
	  goto lab40 ; 
	} 
	switch ( ztype ( p ) ) {
	case hlistnode : 
	case vlistnode : 
	case rulenode : 
	    d = width ( p ) ; 
	    goto lab40 ; 
	  break ; 
	case ligaturenode : 
	    mem [ ligtrick ] = mem [ ligchar ( p ) ] ; 
	    link ( ligtrick ) = link ( p ) ; 
	    p = ligtrick ; 
	    goto lab21 ; 
	  break ; 
	case kernnode : 
	case mathnode : 
	  d = width ( p ) ; 
	  break ; 
	case gluenode : 
	    q = glueptr ( p ) ; 
	    d = width ( q ) ; 
	    if ( gluesign ( justbox ) == stretching ) {
	      if ( ( glueorder ( justbox ) == stretchorder ( q ) ) && ( 
	      stretch ( q ) != 0 ) ) 
	      v = maxdimen ; 
	    } else if ( gluesign ( justbox ) == shrinking ) {
	      if ( ( glueorder ( justbox ) == shrinkorder ( q ) ) && ( shrink 
	      ( q ) != 0 ) ) 
	      v = maxdimen ; 
	    } 
	    if ( subtype ( p ) >= aleaders ) 
	      goto lab40;
	  break ; 
	case whatsitnode : 
	default: 
	  d = 0 ; 
	  break ; 
	}
	if ( v < maxdimen ) 
	  v = v + d ; 
	goto lab45 ; 

lab40:	if ( v < maxdimen ) {
	  v = v + d ; 
	  w = v ; 
	} else {
	  w = maxdimen ; 
	  goto lab30 ; 
	} 
lab45:
	p = link ( p ) ; 
      }
lab30: ; 
    }

    if ( parshapeptr == 0 ) {
    if ( ( hangindent != 0 )
	&& ( ( hangafter >= 0 && ( curlist .pgfield + 2 > hangafter ) )
	  || ( curlist .pgfield + 1 < - (integer) hangafter ) ) )
      {
	l = hsize - abs ( hangindent ) ; 
	if ( hangindent > 0 ) 
	  s = hangindent ; 
	else
	  s = 0 ; 
      } else {
	l = hsize ; 
	s = 0 ; 
      }
    } else {
      n = info ( parshapeptr ) ; 
      if ( curlist .pgfield + 2 >= n ) 
	p = parshapeptr + 2 * n ; 
      else
	p = parshapeptr + 2 * ( curlist .pgfield + 2 ) ; 
      s = mem [ p - 1 ] .cint ; 
      l = mem [ p ] .cint ; 
    } 
    pushmath ( mathshiftgroup ) ; 
    curlist .modefield = mmode;
    eqworddefine ( intbase + curfamcode , -1 ) ; 
    eqworddefine ( dimenbase + predisplaysizecode , w ) ; 
    eqworddefine ( dimenbase + displaywidthcode , l ) ; 
    eqworddefine ( dimenbase + displayindentcode , s ) ; 
    if ( everydisplay != 0 ) 
      begintokenlist ( everydisplay , everydisplaytext ) ; 
    if ( nestptr == 1 ) 
      buildpage () ; 
  } else {
    backinput () ; 
    go_into_ordinary_math();
  }
}


void starteqno ( void )
{ starteqno_regmem 

  saved ( 0 ) = curchr;
  incr ( saveptr );
  go_into_ordinary_math();
}


void scanmath ( halfword p )
{ scanmath_regmem 
  register integer c;
  register eightbits r_curcmd;

lab20:
  r_curcmd = getxnbtoken(1);

/*lab21:*/
  switch ( r_curcmd ) {
  case 16 :
    curchr = scancharnum ();	/* curchr = curval; */
    curcmd = 68;
#if 0
    r_curcmd = 68;
    goto lab21 ;
    break ;
#else
    /* Fall through */
#endif
  case 11 : 
  case 12 : 
  case 68 : 
      c = mathcode ( curchr ) ; 
      if ( c == 32768L ) {
	{
	  curcs = curchr + activebase ; 
	  curcmd = eqtype ( curcs ) ; 
	  curchr = equiv ( curcs ) ; 
	  xtoken () ; 
	  backinput () ; 
	} 
	goto lab20 ; 
      }
    break ; 
  case 17 : 
    c = scanfifteenbitint ();		/* c = curval; */
    break ; 
  case 69 : 
    c = curchr ; 
    break ; 
  case 15 : 
    c = scantwentysevenbitint() / 4096;	/* c = curval / 4096; */
    break ; 
  default: 
    {
      backinput () ; 
      scanleftbrace () ; 
      saved ( 0 ) = p ; 
      incr ( saveptr ) ; 
      pushmath ( mathgroup ) ; 
      return ; 
    }
    break ; 
  }
  mathtype ( p ) = mathchar ; 
  character ( p ) = c % 256 ; 
  if ( ( c >= varcode ) && ( ( curfam >= 0 ) && ( curfam < 16 ) ) ) 
    fam ( p ) = curfam ; 
  else
    fam ( p ) = ( c / 256 ) % 16 ; 
}


void setmathchar ( integer c )
{ setmathchar_regmem 

  if ( c >= 32768L ) {
    curcs = curchr + activebase ; 
    curcmd = eqtype ( curcs ) ; 
    curchr = equiv ( curcs ) ; 
    xtoken () ; 
    backinput () ; 
  } else {
    register long_halfword p;

    p = newnoad () ; 
    mathtype ( nucleus ( p ) ) = mathchar ; 
    character ( nucleus ( p ) ) = c % 256 ; 
    fam ( nucleus ( p ) ) = ( c / 256 ) % 16 ; 
    if ( c >= varcode ) {
      if ( ( ( curfam >= 0 ) && ( curfam < 16 ) ) ) 
	fam ( nucleus ( p ) ) = curfam ; 
      ztype ( p ) = ordnoad ; 
    } else
      ztype ( p ) = ordnoad + ( c / 4096 ) ; 
    link ( curlist .tailfield ) = p ; 
    curlist .tailfield = p ; 
  }
}


void mathlimitswitch ( void )
{ mathlimitswitch_regmem 

  if ( curlist .headfield != curlist .tailfield ) 
  if ( ztype ( curlist .tailfield ) == opnoad ) {
    subtype ( curlist .tailfield ) = curchr ; 
    return ; 
  }
  print_err("Limit controls must follow a math operator");
  zhelp1( STR_H_IMIGNORING_LIMITS );
  error () ; 
}


void scandelimiter ( halfword p, boolean r )
{ scandelimiter_regmem 
  register integer r_curval;

  if ( r ) 
    r_curval = scantwentysevenbitint ();
  else {
    register eightbits r_curcmd;

    r_curcmd = getxnbtoken(1);
    switch ( r_curcmd ) {
    case 11 : 
    case 12 : 
      r_curval = delcode ( curchr );
      break ; 
    case 15 : 
      r_curval = scantwentysevenbitint ();
      break ;
    default: 
      r_curval = -1;
      break ;
    }
    curval = r_curval;
  }

  if ( r_curval < 0 ) {
    print_err("Missing delimiter (. inserted)");
    zhelp1( STR_H_IWASEXP_BRACE );
    backerror();
    curval = r_curval = 0;
  }
  smallfam ( p ) = ( r_curval / 1048576L ) % 16;
  smallchar ( p ) = ( r_curval / 4096 ) % 256;
  largefam ( p ) = ( r_curval / 256 ) % 16;
  largechar ( p ) = r_curval % 256;
}


void mathradical ( void )
{ mathradical_regmem

  tailappend ( getnode ( radicalnoadsize ) ) ; 
  ztype ( curlist .tailfield ) = radicalnoad ; 
  subtype ( curlist .tailfield ) = normal ; 
  mem [ nucleus ( curlist .tailfield ) ] .hh = emptyfield ; 
  mem [ subscr ( curlist .tailfield ) ] .hh = emptyfield ; 
  mem [ supscr ( curlist .tailfield ) ] .hh = emptyfield ; 
  scandelimiter ( leftdelimiter ( curlist .tailfield ) , true ) ; 
  scanmath ( nucleus ( curlist .tailfield ) ) ; 
}


void mathac ( void )
{ mathac_regmem 

  if ( curcmd == 45 ) {
    print_err("Please use ");
    printesc( STR_MATHACCENT );
    c_print(" for accents in math mode");
    zhelp1( STR_H_IMCHANGING_ACCENT );
    error();
  }
  tailappend ( getnode ( accentnoadsize ) ) ; 
  ztype ( curlist .tailfield ) = accentnoad ; 
  subtype ( curlist .tailfield ) = normal ; 
  mem [ nucleus ( curlist .tailfield ) ] .hh = emptyfield ; 
  mem [ subscr ( curlist .tailfield ) ] .hh = emptyfield ; 
  mem [ supscr ( curlist .tailfield ) ] .hh = emptyfield ; 
  mathtype ( accentchr ( curlist .tailfield ) ) = mathchar ; 
  { register integer r_curval;

    r_curval = scanfifteenbitint ();
    character ( accentchr ( curlist .tailfield ) ) = r_curval % 256;
    if ( r_curval >= varcode && ( curfam >= 0 && curfam < 16 ) )
      fam ( accentchr ( curlist .tailfield ) ) = curfam;
    else
      fam ( accentchr ( curlist .tailfield ) ) = ( r_curval / 256 ) % 16 ;
  }
  scanmath ( nucleus ( curlist .tailfield ) ) ;
}


void appendchoices ( void )
{ appendchoices_regmem 

  tailappend ( newchoice () ) ; 
  incr ( saveptr ) ; 
  saved ( -1 ) = 0 ; 
  pushmath ( mathchoicegroup ) ; 
  scanleftbrace () ; 
}


halfword finmlist ( halfword p )
{ finmlist_regmem
  register halfword q ;

  if ( curlist .auxfield .cint != 0 ) {
    mathtype ( denominator ( curlist .auxfield .cint ) ) = submlist ; 
    info ( denominator( curlist.auxfield.cint ) ) = link( curlist.headfield );
    if ( p == 0 )
      q = curlist .auxfield .cint ; 
    else {
      q = info ( numerator ( curlist .auxfield .cint ) ) ; 
      if ( ztype ( q ) != leftnoad ) 
	confusion("right");
      info ( numerator ( curlist .auxfield .cint ) ) = link ( q ) ; 
      link ( q ) = curlist .auxfield .cint ; 
      link ( curlist .auxfield .cint ) = p ; 
    } 
  } else {
    link ( curlist .tailfield ) = p ; 
    q = link ( curlist .headfield ) ; 
  } 
  popnest () ; 

  return(q);
}


void buildchoices ( void )
{/* 10 */ buildchoices_regmem 
  register halfword p  ; 

  unsave () ; 
  p = finmlist ( 0 ) ; 
  switch ( saved ( -1 ) ) {
  case 0 : 
    displaymlist ( curlist .tailfield ) = p ; 
    break ; 
  case 1 : 
    textmlist ( curlist .tailfield ) = p ; 
    break ; 
  case 2 : 
    scriptmlist ( curlist .tailfield ) = p ; 
    break ; 
  case 3 : 
    {
      scriptscriptmlist ( curlist .tailfield ) = p ; 
      decr ( saveptr ) ; 
      return ; 
    } 
    break ; 
  } 
  incr ( saved ( -1 ) ) ; 
  pushmath ( mathchoicegroup ) ; 
  scanleftbrace () ; 
} 


void subsup ( void )
{ subsup_regmem 
  register smallnumber t  ; 
  register halfword p  ; 

  t = 0 ; 
  p = 0 ; 
  if ( curlist .tailfield != curlist .headfield ) 
  if ( scriptsallowed ( curlist .tailfield ) ) {
    p = supscr ( curlist .tailfield ) + curcmd - 7 ; 
    t = mathtype ( p ) ; 
  } 
  if ( ( p == 0 ) || ( t != 0 ) ) {
    tailappend ( newnoad () ) ; 
    p = supscr ( curlist .tailfield ) + curcmd - 7 ; 
    if ( t != 0 ) {
      if ( curcmd == 7 ) {
	print_err("Double superscript");
	zhelp1( STR_H_ITREAT_XUP1UP2 );
      } else {
	print_err("Double subscript");
	zhelp1( STR_H_ITREAT_XDOWN1DOWN2 );
      } 
      error () ; 
    } 
  }
  scanmath ( p ) ; 
}


void mathfraction ( void )
{ mathfraction_regmem 
  register smallnumber c  ; 

  c = curchr ; 
  if ( curlist .auxfield .cint != 0 ) {
    if ( c >= 3 ) {
      scandelimiter ( garbage , false ) ; 
      scandelimiter ( garbage , false ) ; 
    } 
    if ( c % 3 == 0 ) 
      scandimen( false, false, false );
    print_err("Ambiguous; you need another { and }");
    zhelp1( STR_H_IM_IGNORING_FRACTION );
    error();
  } else {
    curlist .auxfield .cint = getnode ( fractionnoadsize ) ; 
    ztype ( curlist .auxfield .cint ) = fractionnoad ; 
    subtype ( curlist .auxfield .cint ) = normal ; 
    mathtype ( numerator ( curlist .auxfield .cint ) ) = submlist ; 
    info ( numerator ( curlist.auxfield.cint ) ) = link ( curlist.headfield );
    mem [ denominator ( curlist .auxfield .cint ) ] .hh = emptyfield ; 
    mem [ leftdelimiter ( curlist .auxfield .cint ) ] .qqqq = nulldelimiter ; 
    mem [ rightdelimiter ( curlist .auxfield .cint ) ] .qqqq = nulldelimiter ; 
    link ( curlist .headfield ) = 0 ; 
    curlist .tailfield = curlist .headfield ; 
    if ( c >= 3 ) {
      scandelimiter ( leftdelimiter ( curlist .auxfield .cint ) , false ) ; 
      scandelimiter ( rightdelimiter ( curlist .auxfield .cint ) , false ) ; 
    }
    switch ( c % 3 ) {
    case 0 : 
      thickness( curlist.auxfield.cint ) = scandimen( false, false, false );
      break ; 
    case 1 : 
      thickness ( curlist .auxfield .cint ) = defaultcode ; 
      break ; 
    case 2 : 
      thickness ( curlist .auxfield .cint ) = 0 ; 
      break ; 
    } 
  }
}


void mathleftright ( void )
{ mathleftright_regmem 
  register smallnumber t;

  t = curchr ; 
  if ( ( t == rightnoad ) && ( curgroup != mathleftgroup ) ) {
    if ( curgroup == mathshiftgroup ) {
      scandelimiter ( garbage , false ) ; 
      print_err("Extra ");
      printesc( STR_RIGHT );
      zhelp1( STR_H_IM_IGNORING_RIGHT );
      error();
    } else
      offsave ();
  } else {
    register halfword p;

    p = newnoad () ; 
    ztype ( p ) = t ; 
    scandelimiter ( delimiter ( p ) , false ) ; 
    if ( t == leftnoad ) {
      pushmath ( mathleftgroup ) ; 
      link ( curlist .headfield ) = p ; 
      curlist .tailfield = p ; 
    } else {
      p = finmlist ( p ) ; 
      unsave () ; 
      tailappend ( newnoad () ) ; 
      ztype ( curlist .tailfield ) = innernoad ; 
      mathtype ( nucleus ( curlist .tailfield ) ) = submlist ; 
      info ( nucleus ( curlist .tailfield ) ) = p ; 
    }
  }
}


void resumeafterdisplay ( void )
{ resumeafterdisplay_regmem 

  if ( curgroup != mathshiftgroup ) 
    confusion("display");
  unsave () ; 
  curlist .pgfield = curlist .pgfield + 3 ; 
  pushnest () ; 
  curlist .modefield = hmode ;
  curlist .auxfield .hh .v.LH = 1000 ; 
#if 0  /* 3.1415 */
  curlist .auxfield .hh .v.RH = 0 ; 
#else
  if ( language <= 0 || language > 255 )
    curlang = 0;
  else
    curlang = language ; 
  curlist .auxfield .hh .v.RH = curlang;
  curlist .pgfield = ( normmin ( lefthyphenmin ) * 0x40
			+ normmin ( righthyphenmin ) ) * 0x10000L + curlang;
#endif

  if ( (eightbits)getxtoken() != 10 )
    backinput();

  if ( nestptr == 1 ) 
    buildpage();
}


void getrtoken ( void )
{ getrtoken_regmem 

lab20:
  do {
    gettoken();
  } while ( curtok == spacetoken );

  if ( ( curcs == 0 ) || ( curcs > frozencontrolsequence ) ) {
    print_err("Missing control sequence inserted");
    zhelp1( STR_H_PLEASEDONTSAYDEF );
    if ( curcs == 0 )
      backinput();
    curtok = cstokenflag + frozenprotection;
    inserror();
    goto lab20;
  }
}


  long_halfword
trapzeroglue ( long_halfword r_curval )
{ trapzeroglue_regmem 

  if( width(r_curval) == 0 && stretch(r_curval) == 0 && shrink(r_curval) == 0 ) {
    addglueref ( zeroglue );
    deleteglueref ( r_curval );
    curval = r_curval = zeroglue;
  }
  return r_curval;
}


void doregistercommand ( smallnumber a )
{ doregistercommand_regmem 
  register halfword l, q;
  register integer p;
  register integer r_curval;

  q = curcmd ; 
  if ( q != register_cmd ) {
    register eightbits r_curcmd;

    r_curcmd = getxtoken();
    if ( r_curcmd >= assign_int && r_curcmd <= assign_mu_glue ) {
	p = r_curcmd - assign_int;
	l = curchr ; 
	goto lab40 ; 
    } 
    if ( r_curcmd != register_cmd ) {
	p_print_err(  STR_H_YOUCANTUSE );
	printcmdchr ( r_curcmd , curchr );
	print( STR_H_AFTER );
	printcmdchr ( q , 0 );
	zhelp1( STR_H_IMFORGETTING );
	error () ; 
	return;
    }
  }

  p = curchr ; 
  r_curval = scaneightbitint ();
  switch ( p ) {
    case intval : 
      l = r_curval + countbase;
      break ; 
    case dimenval : 
      l = r_curval + scaledbase ; 
      break ; 
    case glueval : 
      l = r_curval + skipbase ; 
      break ; 
    case muval : 
      l = r_curval + muskipbase ; 
      break ; 
  }

lab40: ; 
  if ( q == register_cmd )
    scanoptionalequals () ; 
  else if ( scankeyword ( STR_BY ) )
    ;

  aritherror = false ; 
  if ( q < multiply ) {

  if ( p < glueval ) {
    if ( p == intval ) 
      r_curval = scanint ();
    else
      r_curval = scandimen ( false, false, false );
    if ( q == advance )
      r_curval = r_curval + eqtb [ l ] .cint;
  } else {
    r_curval = scanglue ( p );
    if ( q == advance ) {
      register long_halfword r;

      q = newspec ( r_curval ) ; 
      r = equiv ( l ) ; 
      deleteglueref ( r_curval ) ; 
      width ( q ) += width ( r );
      if ( stretch ( q ) == 0 ) 
	stretchorder ( q ) = normal ; 
      if ( stretchorder ( q ) == stretchorder ( r ) ) 
	stretch ( q ) += stretch ( r );
      else if ( ( stretchorder ( q ) < stretchorder ( r ) )
	     && ( stretch ( r ) != 0 ) ) {
	stretch ( q ) = stretch ( r ) ; 
	stretchorder ( q ) = stretchorder ( r ) ; 
      }
      if ( shrink ( q ) == 0 )
	shrinkorder ( q ) = normal ; 
      if ( shrinkorder ( q ) == shrinkorder ( r ) ) 
	shrink ( q ) += shrink ( r );
      else if ( ( shrinkorder ( q ) < shrinkorder ( r ) )
	     && ( shrink ( r ) != 0 ) ) {
	shrink ( q ) = shrink ( r ) ; 
	shrinkorder ( q ) = shrinkorder ( r ) ; 
      }
      r_curval = q ; 
    }
  }

  } else {

    r_curval = scanint ();
    if ( p < glueval ) {
      if ( q == multiply ) 
	if ( p == intval ) 
	  r_curval = multandadd( eqtb[l].cint, r_curval, 0, 2147483647L );
	else
	  r_curval = multandadd( eqtb[l].cint , r_curval, 0, 1073741823L );
      else
	r_curval = xovern( eqtb[l].cint, r_curval );
    } else {
      register long_halfword r, s;

      s = equiv ( l );
      r = newspec ( s );
      if ( q == multiply ) {
	width(r) = multandadd( width(s), r_curval, 0, 1073741823L ) ; 
	stretch(r) = multandadd( stretch(s), r_curval, 0, 1073741823L );
	shrink(r) = multandadd( shrink(s), r_curval, 0, 1073741823L );
      } else {
	width(r) = xovern( width(s), r_curval );
	stretch(r) = xovern( stretch(s), r_curval );
	shrink(r) = xovern( shrink(s), r_curval );
      }
      r_curval = r;
    }
  }

  if ( aritherror ) {
    print_err("Arithmetic overflow");
    zhelp1( STR_H_ICANTCARRYOUT );
    error ();
    return;
  }

  if ( p < glueval ) {
    if ( ( a >= 4 ) ) 
      geqworddefine( l, r_curval );
    else
      eqworddefine( l, r_curval );
  } else {
    r_curval = trapzeroglue(r_curval);
    if ( a >= 4 )
      geqdefine( l, glueref, r_curval );
    else
      eqdefine( l, glueref, r_curval );
  }
}


void alteraux ( void )
{ alteraux_regmem

  if ( curchr != abs ( curlist .modefield ) )
    reportillegalcase ();
  else {
    register halfword c;

    c = curchr ; 
    scanoptionalequals();
    if ( c == vmode ) {
      curlist.auxfield.cint = scandimen( false, false, false );
    } else {
      register integer r_curval;

      r_curval = scanint ();
      if ( r_curval <= 0 || r_curval > 32767 ) {
	print_err("Bad space factor");
	zhelp1( STR_H_IALLOWONLYVALUES );
	interror ( r_curval );
      } else
	curlist .auxfield .hh .v.LH = r_curval;
    }
  }
}


void alterprevgraf ( void )
{ alterprevgraf_regmem 
  register integer p;
  
  nest [ nestptr ] = curlist ; 
  p = nestptr ; 
  while ( abs ( nest [ p ] .modefield ) != vmode )
    decr ( p );

  scanoptionalequals ();

 { register integer r_curval;

  r_curval = scanint ();
  if ( r_curval < 0 ) {
    print_err("Bad ");
    printesc( STR_PREVGRAF );
    zhelp1( STR_H_IALLOWONLYNONNEG );
    interror ( r_curval ) ; 
  } else {
    nest[p].pgfield = r_curval;
    curlist = nest[nestptr];
  }
 }
}


void alterpagesofar ( void )
{ alterpagesofar_regmem
  register schar c;

  c = curchr;
  scanoptionalequals ();
  pagesofar[c] = scandimen( false, false, false );
}


void alterinteger ( void )
{ alterinteger_regmem
  register schar c;

  c = curchr;
  scanoptionalequals();
  if ( c == 0 ) 
    deadcycles = scanint();
  else
    insertpenalties = scanint();
}


void alterboxdimen ( void )
{ alterboxdimen_regmem 
  register smallnumber c  ; 
  register eightbits b  ; 

  c = curchr ; 
  b = scaneightbitint ();	/* b = curval; */
  scanoptionalequals ();
  { register integer r_curval;

    r_curval = scandimen( false, false, false );
    if ( box ( b ) != 0 )
      mem [ box ( b ) + c ] .cint = r_curval;
  }
}



/* preparemag() & newfont() => filename.c */


void newinteraction ( void )
{ newinteraction_regmem 

  println();
#ifdef ERW_INTERACTION
  geqworddefine ( intbase + interactionmodecode, curchr );
#else
  interaction = curchr;
#endif
  if ( interaction <= batchmode )
    selector = noprint;
  else selector = termonly;
  if ( logopened )
    selector = selector + 2;
}


void doassignments ( void )
{ doassignments_regmem 
  register eightbits r_curcmd;

  while ( true ) {
    r_curcmd = getxnbtoken(1);
    if ( r_curcmd <= max_non_prefixed_command )
      return;
    setboxallowed = false;  /* TeX 3.141 */
    prefixedcommand();
    setboxallowed = true;   /* TeX 3.141 */
  }
}


void openorclosein ( void )
{ openorclosein_regmem 
  register schar c;
  register schar n;

  c = curchr ; 
  n = scanfourbitint ();
  if ( readopen [ n ] != closed ) {
    aclose ( readfile [ n ] );
    readopen [ n ] = closed;
  }
  if ( c != 0 ) {
    scanoptionalequals ();
    scanfilename ();
    if ( curext == 335 )
      curext = STR_DOT_TEX;
    packfilename ( curname , curarea , curext );
    if ( aopenin ( readfile [ n ] , TEXINPUTPATH ) ) 
      readopen [ n ] = justopen;
  }
}


void issuemessage ( void )
{ issuemessage_regmem
  register integer oldsetting;
  schar c;
  register strnumber s;

  c = curchr ; 
  link ( garbage ) = scantoks ( false , true ) ; 

  oldsetting = selector ; 
  selector = newstring ; 
  tokenshow ( defref ) ; 
  selector = oldsetting ; 
  flushlist ( defref ) ; 

  strroom ( 1 );
  s = makestring ();
  if ( c == 0 ) {
    if ( termoffset + length ( s ) > maxprintline - 2 )
      println ();
    else if ( ( termoffset > 0 ) || ( fileoffset > 0 ) )
      printchar( 32 );
#if 0 /* TeX 3.141 */
    print ( s );
#else
    slowprint( s );
#endif
    flush ( stdout );
  } else {
#if 0 /* TeX 3.141 */
    p_print_err ( s );
#else
    p_print_err ( 335 ); /* "" */  slowprint( s );
#endif
    if ( errhelp != 0 )
      useerrhelp = true;
    else if ( longhelpseen )
      zhelp1( STR_H_THATWASANOTHER );
    else {
      if ( interaction < errorstopmode )
	longhelpseen = true;
      zhelp1( STR_H_THISERRORMESSAGE );
    }
    error();
    useerrhelp = false;
  }
  flushstring;
}


void shiftcase ( void )
{ shiftcase_regmem 
  register halfword b;

  b = curchr;

 { register halfword p;

  p = scantoks ( false , false );
  p = link ( defref );
  while ( p != 0 ) {
    register halfword t;

    t = info ( p ) ; 
    if ( t < cstokenflag + singlebase ) {
      register eightbits c;

      c = t % 256;
      if ( equiv ( b + c ) != 0 )
	info ( p ) = t - c + equiv ( b + c );
    }
    p = link ( p );
  }
 }
  begintokenlist ( link ( defref ) , backedup );
  freeavail ( defref );
}


void showwhatever ( void )
{ showwhatever_regmem
  /* register halfword p; */

  switch ( curchr ) {
  case 3:
    begindiagnostic ();
    showactivities ();
    break;
  case 1:
    { register integer r_curval;

      r_curval = scaneightbitint();
      begindiagnostic();
      c_printnl("> \\box");
      printint ( r_curval );
      printchar( 61 );
      if ( box ( r_curval ) == 0 )
	c_print("void");
      else
	showbox( box(r_curval) );
    }
    break;
  case 0:
    {
      gettoken ();
      if ( interaction >= errorstopmode ) 
	wakeupterminal ();
      c_printnl("> ");
      if ( curcs != 0 ) {
	sprintcs ( curcs );
	printchar ( 61 );
      }
      printmeaning ();
      goto lab50;
    }
    break;
  default:
    {
      /* p = */ (void) thetoks ();
      if ( interaction >= errorstopmode ) 
	wakeupterminal ();
      c_printnl("> ");
      tokenshow ( temphead );
      flushlist ( link ( temphead ) );
      goto lab50 ; 
    } 
    break ; 
  } 
  enddiagnostic ( true );
  print_err("OK");

  if ( selector == termandlog ) 
  if ( tracingonline <= 0 ) {
    selector = termonly ; 
    c_print(" (see the transcript file)");
    selector = termandlog;
  }
lab50:
  if ( interaction < errorstopmode ) {
    help0;
    decr ( errorcount );
  } else if ( tracingonline > 0 ) {
    zhelp1( STR_H_THISISNTERR );
  } else {
    zhelp2( STR_H_THISISNTERR, STR_H_ANDTYPEITRACING );
  }
  error();
}

/* -- end -- */
