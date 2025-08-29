#define EXTERN extern
#include "texd.h"


long_halfword strtoks ( poolpointer b )
{ strtoks_regmem 
  register long_halfword p;
  register halfword t;
  register poolpointer k;

  strroom ( 1 ) ; 
  p = temphead ; 
  link ( p ) = 0 ; 
  k = b;
  while ( k < poolptr ) {
    t = strpool[k];
    if ( t == 32 ) 
      t = spacetoken ; 
    else
      t = othertoken + t ; 
    faststorenewtoken ( t ) ; 
    incr ( k );
  }
  poolptr = b;
  return(p);
}


long_halfword thetoks ( void )
{ register integer r_curval;

  getxtoken ();
  r_curval = scansomethinginternal( tokval, false );
 { thetoks_regmem

  if ( curvallevel >= identval ) {
    long_halfword p;

    p = temphead;
    link ( p ) = 0;
    if ( curvallevel == identval )
      storenewtoken ( cstokenflag + r_curval );
    else if ( r_curval != 0 ) {
      register halfword r;

      r = link ( r_curval );
      while ( r != 0 ) {
	faststorenewtoken ( info ( r ) );
	r = link ( r );
      }
    }
    return(p);
  } else {
    poolpointer b;
    integer oldsetting;

    oldsetting = selector ; 
    selector = newstring ; 
    b = poolptr;
    switch ( curvallevel ) {
    case intval : 
      printint ( r_curval );
      break;
    case dimenval : 
	printscaled ( r_curval );
	print( STR_PT );
      break;
    case glueval : 
	printspec( r_curval, STR_PT );
	deleteglueref ( r_curval ) ; 
      break ; 
    case muval : 
	printspec ( r_curval, STR_MU );
	deleteglueref ( r_curval );
      break ; 
    } 
    selector = oldsetting ; 
    return( strtoks ( b ) );
  }
 }
}


void insthetoks ( void )
{ insthetoks_regmem 

  link ( garbage ) = thetoks();
  begintokenlist( link(temphead), inserted );
}


void convtoks ( void )
{ convtoks_regmem
  register integer c;
  integer oldsetting;
  poolpointer b;

  c = curchr ; 
  switch ( c ) {
  case numbercode : 
  case romannumeralcode : 
    scanint ();
    break;
  case stringcode :
  case meaningcode :
    { register smallnumber savescannerstatus;

      savescannerstatus = scannerstatus;
      scannerstatus = normal;
      gettoken();
      scannerstatus = savescannerstatus;
    }
    break;
  case fontnamecode : 
    scanfontident ();
    break;
  case jobnamecode : 
    if ( jobname == 0 )
      openlogfile();
    break;
  }

  oldsetting = selector ; 
  selector = newstring ; 
  b = poolptr ; 
  switch ( c ) {
  case numbercode : 
    printint ( curval ) ; 
    break ; 
  case romannumeralcode : 
    printromanint ( curval ) ; 
    break ; 
  case stringcode : 
    if ( curcs != 0 ) 
      sprintcs ( curcs );
    else
      printchar ( curchr );
    break;
  case meaningcode : 
    printmeaning ();
    break;
  case fontnamecode : 
    { register integer r_curval = curval;

      print ( fontname(r_curval) );
      if ( fontsize(r_curval) != fontdsize(r_curval) ) {
	print( STR_AT_ );
	printscaled ( fontsize(r_curval) );
	print( STR_PT );
      }
    }
    break;
  case jobnamecode:
    print( jobname );
    break;
  }
  selector = oldsetting;
  link ( garbage ) = strtoks ( b );
  begintokenlist( link(temphead), inserted );
}


long_halfword scantoks ( boolean macrodef, boolean xpand )
{ scantoks_regmem
  register halfword t, s;
  register long_halfword p;
  register halfword unbalance, hashbrace;

  if ( macrodef )
    scannerstatus = defining ; 
  else
    scannerstatus = absorbing ; 
  warningindex = curcs ; 
#if 0
  defref = getavail () ; 
  tokenrefcount ( defref ) = 0 ; 

  p = defref;
#else
  p = getavail();
  tokenrefcount( p ) = 0 ; 

  defref = p;
#endif
  hashbrace = 0;
  t = zerotoken;

  if ( macrodef ) {
    while ( true ) {
      gettoken () ; 
      if ( curtok < rightbracelimit )
	goto lab31;
      if ( curcmd == 6 ) {
	s = matchtoken + curchr ; 
	if( (eightbits)gettoken() == 1 ) {
	  hashbrace = curtok;
	  storenewtoken ( curtok );
	  storenewtoken ( endmatchtoken );
	  goto lab30;
	}
	if ( t == zerotoken + 9 ) {
	  print_err("You already have nine parameters");
	  zhelp1( STR_H_IMGOING_TABSIGN );
	  error();
	} else {
	  incr ( t ) ; 
	  if ( curtok != t ) {
	    print_err("Parameters must be numbered consecutively");
	    zhelp1( STR_H_IVEINSERTED_DIGIT );
	    backerror () ; 
	  }
	  curtok = s;
	}
      }
      storenewtoken( curtok );
    }			/* while ( true ) */

lab31:
    storenewtoken ( endmatchtoken );
    if ( curcmd == 2 ) {
      p_print_err( STR_H_MISSING_LEFTBRACE );
      incr ( alignstate );
      zhelp1( STR_H_WHEREWAS_LEFTBRACE );
      error();
      goto lab40;
    }
lab30: ; 

  } else

    scanleftbrace();

  unbalance = 1;
  while ( true ) {
    if ( xpand ) {
      while ( true ) {
	if( (eightbits)getnext() <= maxcommand )
	  goto lab32 ; 
	if ( curcmd != the ) 
	  expand();
	else {
	  register long_halfword q;

	  q = thetoks();
	  if ( link ( temphead ) != 0 ) {
	    link ( p ) = link ( temphead ) ; 
	    p = q;
	  }
	}
      } 	/* while ( true ) */
lab32:
      xtoken();
    } else
      gettoken();

    if ( curtok < rightbracelimit ) {
      if ( curcmd < 2 ) 
	incr ( unbalance ) ; 
      else {
	decr ( unbalance ) ; 
	if ( unbalance == 0 ) 
	  goto lab40 ; 
      }
    } else if ( curcmd == 6 ) 
      if ( macrodef ) {
	s = curtok ; 
	if( (eightbits)( (xpand) ? getxtoken() : gettoken() ) != 6 ) {
	  if ( ( curtok <= zerotoken ) || ( curtok > t ) ) {
	    print_err("Illegal parameter number in definition of ");
	    sprintcs ( warningindex );
	    zhelp1( STR_H_YOUMEANT_TAB );
	    backerror();
	    curtok = s;
	  } else
	    curtok = outparamtoken - 48 + curchr;
	}
      }
    storenewtoken ( curtok ) ; 
  }		/* while (true) */

lab40:
  scannerstatus = normal ; 
  if ( hashbrace != 0 ) 
    storenewtoken ( hashbrace );

  return(p);
}


void readtoks ( integer n, halfword r )
{ readtoks_regmem 
  register halfword p;
  integer s;
  register smallnumber m;

  scannerstatus = defining ; 
  warningindex = r ; 
  defref = getavail () ; 
  tokenrefcount ( defref ) = 0 ; 
  p = defref ; 
  storenewtoken ( endmatchtoken ) ; 
  if ( n < 0 || n > 15 )
    m = 16 ; 
  else
    m = n ; 
  s = alignstate ; 
  alignstate = 1000000L ; 

  do {
    beginfilereading () ; 
    curinput .namefield = m + 1 ; 
    if ( readopen [ m ] == closed ) 
      if ( interaction > nonstopmode ) 
	if ( n < 0 ) 
	  terminput("");
	else {
	  wakeupterminal () ; 
	  println () ; 
	  sprintcs ( r ) ; 
	  terminput("=");
	  n = -1 ; 
	} 
      else
	fatalerror( STR_H_FE_CANNOTREAD );
    else if ( readopen [ m ] == justopen ) 
      if ( inputln ( readfile [ m ] , false ) ) 
	readopen [ m ] = normal ; 
      else {
	aclose ( readfile [ m ] ) ; 
	readopen [ m ] = closed ; 
      }
    else {
      if ( ! inputln ( readfile [ m ] , true ) ) {
	aclose ( readfile [ m ] ) ; 
	readopen [ m ] = closed ; 
	if ( alignstate != 1000000L ) {
	  runaway () ; 
	  print_err("File ended within ");
	  printesc( STR_READ );
	  zhelp1( STR_H_THISREADHASUNBALANCED );
	  alignstate = 1000000L ; 
	  error () ; 
	}
      }
    }
    curinput .limitfield = last ; 
    if ( endlinecharinactive () ) 
      decr ( curinput .limitfield ) ; 
    else
      buffer [ curinput .limitfield ] = endlinechar ; 
    first = curinput .limitfield + 1 ; 
    curinput .locfield = curinput .startfield ; 
    curinput .statefield = 33 ; 
    while ( true ) {
      gettoken();
      if ( curtok == 0 )
	goto lab30;
      /* TeX 3.141 change begin */
      if ( alignstate < 1000000L ) {
	do {
	  gettoken();
	} while( curtok != 0 );
	alignstate = 1000000L;
	goto lab30;
      }
      /* TeX 3.141 change end */
      storenewtoken( curtok );
    }
lab30:
    endfilereading();
  } while ( alignstate != 1000000L );

  curval = defref ; 
  scannerstatus = normal ; 
  alignstate = s;
}


void passtext ( void )
{ passtext_regmem
  register integer l;
  smallnumber savescannerstatus;

  savescannerstatus = scannerstatus ; 
  scannerstatus = skipping ; 
  l = 0 ; 
  skipline = line ; 
  while ( true ) {
    register eightbits r_curcmd;

    r_curcmd = getnext () ; 
    if ( r_curcmd == fiorelse ) {
      if ( l == 0 )
	goto lab30 ; 
      if ( curchr == ficode ) 
	decr ( l ) ; 
    } else if ( r_curcmd == iftest )
      incr ( l ) ; 
  }
lab30:
  scannerstatus = savescannerstatus ; 
}


void changeiflimit ( smallnumber l, halfword p )
{ changeiflimit_regmem 
  register halfword q;

  if ( p == condptr ) 
    iflimit = l ; 
  else {
    q = condptr ; 
    while ( true ) {
      if ( q == 0 ) 
 	confusion("if");
      if ( link ( q ) == p ) {
	ztype ( q ) = l ; 
	return ; 
      } 
      q = link ( q ) ; 
    } 
  }
}


void conditional ( void )
{ conditional_regmem 
  boolean b;
  register integer m, n;
  register halfword p, q;
  halfword savecondptr;
  register smallnumber thisif;

  {
    p = getnode ( ifnodesize ) ; 
    link ( p ) = condptr ; 
    ztype ( p ) = iflimit ; 
    subtype ( p ) = curif ; 
    iflinefield ( p ) = ifline ; 
    condptr = p ; 
    curif = curchr ; 
    iflimit = ifcode ; 
    ifline = line ; 
  }
  savecondptr = condptr ; 
  thisif = curchr ; 

  switch ( thisif ) {
  case ifcharcode : 
  case ifcatcode : 
    { register eightbits r_curcmd;

      getxtokenoractivechar;
      if ( r_curcmd > 13 || curchr > 255 ) {
	m = 0;
	n = 256;
      } else {	  
	m = r_curcmd;
	n = curchr;
      } 
      getxtokenoractivechar;
      if ( r_curcmd > 13 || curchr > 255 ) {
	curcmd = r_curcmd = 0;
	curchr = 256;
      } 
#ifdef ERW_TRACING
	/*  zeige nachdem die beiden Tokens gelesen wurde, die entsprechenden
	 *  Werte im log-file
	 */
      if ( tracingcommands > 2 ) {
	begindiagnostic ();
	printchar( 123 );
#if 0
	if( thisif == ifcharcode ) {
	  print(STR_IF);  printchar(' ');
	  printint(n);  c_print(" = ");  printint(curchr);
	} else {
	  print(STR_IFCAT);  printchar(' ');
	  printcmdchr(m, n);  c_print(" = ");  printcmdchr(r_curcmd, curchr);
	}
#else
	/* Format: {if<cat> <cmdchr1> (<num1>) = <cmdchr2> (<num2>)}
	 */
	print( (thisif == ifcharcode) ? STR_IF : STR_IFCAT );  printchar(' ');
	printcmdchr(m, n);  c_print(" (");
	printint( (thisif == ifcharcode) ? n : m ); c_print(") = ");
	printcmdchr(r_curcmd, curchr); c_print(" (");
	printint( (thisif == ifcharcode) ? curchr : r_curcmd );
	printchar(')');
#endif
	printchar( 125 );
	enddiagnostic ( false );
      }
#endif
      if ( thisif == ifcharcode )
	b = ( n == curchr );
      else
	b = ( m == r_curcmd );
    }
    break;
  case ifintcode:
  case ifdimcode:
    { schar r;

      if ( thisif == ifintcode )
	n = scanint ();
      else
	n = scandimen ( false , false , false );
      (void) getxnbtoken(0);

      if ( ( curtok >= othertoken + 60 ) && ( curtok <= othertoken + 62 ) ) 
	r = curtok - othertoken - 60;
      else {
	print_err("Missing = inserted for ");
	printcmdchr( iftest, thisif );
	zhelp1( STR_H_IWASEXPECTING_DIDNT );
	backerror();
	r = 1 /* 61 */;
      }

      if ( thisif == ifintcode ) 
	m = scanint ();
      else
	m = scandimen ( false , false , false );

      switch ( r ) {
      case 0 /* 60 */:
	b = ( n < m );
	break;
      case 1 /* 61 */: 
	b = ( n == m );
	break;
      case 2 /* 62 */: 
	b = ( n > m );
	break;
      }
    }
    break ; 
  case ifoddcode : 
    b = odd( scanint() );
    break ; 
  case ifvmodecode : 
    b = ( abs ( curlist .modefield ) == vmode );
    break ; 
  case ifhmodecode : 
    b = ( abs ( curlist .modefield ) == hmode );
    break ; 
  case ifmmodecode : 
    b = ( abs ( curlist .modefield ) == mmode );
    break ; 
  case ifinnercode : 
    b = ( curlist .modefield < 0 );
    break;
  case ifvoidcode : 
  case ifhboxcode : 
  case ifvboxcode : 
    {
      p = box( scaneightbitint() );
      if ( thisif == ifvoidcode )
	b = ( p == 0 );
      else if ( p == 0 )
	b = false;
      else if ( thisif == ifhboxcode )
	b = ( ztype ( p ) == hlistnode );
      else
	b = ( ztype ( p ) == vlistnode );
    }
    break ; 
  case ifxcode : 
    { smallnumber savescannerstatus;

      savescannerstatus = scannerstatus;
      scannerstatus = normal;
      p = getnext();
      n = curcs;
      /* p = curcmd; */
      q = curchr;
      getnext();
#ifdef ERW_TRACING
	/*  zeige die beiden Tokens bzw. falls Commands gleich und Makros
	 * wird Teil der Definition im log-file gezeigt.
	 */
	/* Format:
	 *   {ifx <type> <<macroname>:<definition>.>
	 *      = <type> <<macroname>:<definition>.>}
	 */
      if ( tracingcommands > 2 ) {
	begindiagnostic ();
	printchar( 123 );
	print(STR_IFX);  printchar(' ');
	printcmdchr(p, q);
	if( p >= call ) {
	  if ( n != 0 ) {
	    printchar(' ');
	    sprintcs( n );
	  }
	  if( q != 0 ) {
	    printchar(':');
	    showtokenlist( link(q), 0, 32 );  /* maxprintline/2 - 13 */
	    printchar('.');
	  }
	}
	println(); c_print("   = ");
	printcmdchr(curcmd, curchr);
	if( curcmd >= call ) {
	  if( curcs != 0 ) {
	    printchar(' ');
	    sprintcs( curcs );
	  }
	  if( curchr != 0 ) {
	    printchar(':');
	    showtokenlist( link(curchr), 0, 32 );
	    printchar('.');
	  }
	}
	printchar( 125 );
	enddiagnostic ( false );
      }
#endif
      if ( curcmd != p )
	b = false;
      else if ( p /*curcmd*/ < call )	/* es gilt: curcmd == p */
	b = ( curchr == q );
      else {				/* und jetzt: curcmd/p >= call */
	p = link ( curchr );
	q = link ( equiv(n) );
	if ( p == q ) {
	  b = true;
	} else {
#if 0
	  while ( ( p != 0 ) && ( q != 0 ) ) {
	    if ( info ( p ) != info ( q ) )
	      p = 0;
	    else {
	      p = link ( p );
	      q = link ( q );
	    }
	  }
	  b = ( ( p == 0 ) && ( q == 0 ) );
#else
	  b = false;
	  while( 1 ) {
	    if( (p == 0) || (q == 0) ) {
	      if( p == q )  /* <==> ( (p == 0) && (q == 0) ) */
		b = true;
	      break;
	    }
	    if ( info(p) != info(q) ) {
	      break;
	    }
	    p = link(p); q = link(q);
	  }
#endif
	}
      }
      scannerstatus = savescannerstatus;
    }
    break;
  case ifeofcode :
    n = scanfourbitint ();
    b = ( readopen [ /*curval*/ n ] == closed );
    break;
  case iftruecode : 
    b = true ; 
    break ; 
  case iffalsecode : 
    b = false ; 
    break ; 
  case ifcasecode : 
    {
      n = scanint ();
      if ( tracingcommands > 1 ) {
	begindiagnostic () ; 
	c_print("{case ");
	printint ( n ) ; 
	printchar ( 125 ) ; 
	enddiagnostic ( false ) ; 
      } 
      while ( n != 0 ) {
	passtext();
	if ( condptr == savecondptr )
	  if ( curchr == orcode )
	    decr ( n );
	  else
	    goto lab50;
	else if ( curchr == ficode ) {
	  p = condptr;
	  ifline = iflinefield ( p );
	  curif = subtype ( p );
	  iflimit = ztype ( p );
	  condptr = link ( p );
	  freenode( p, ifnodesize );
	}
      }
      changeiflimit( orcode, savecondptr );
      return;
    }
    break;
  }

  if ( tracingcommands > 1 ) {
    begindiagnostic ();
    c_print( b ? "{true}" : "{false}" );
    enddiagnostic ( false );
  }

  if ( b ) {
    changeiflimit ( elsecode, savecondptr );
    return;
  }

  while ( true ) {
    passtext();
    if ( condptr == savecondptr ) {
      if ( curchr != orcode ) 
	goto lab50 ; 
      print_err("Extra ");
      printesc( STR_OR );
      zhelp1( STR_H_IMIGNORING_IF );
      error () ; 
    } else if ( curchr == ficode ) {
      p = condptr ; 
      ifline = iflinefield ( p ) ; 
      curif = subtype ( p ) ; 
      iflimit = ztype ( p ) ; 
      condptr = link ( p ) ; 
      freenode ( p , ifnodesize ) ; 
    }
  }
lab50:
  if ( curchr == ficode ) {
    p = condptr ; 
    ifline = iflinefield ( p ) ; 
    curif = subtype ( p ) ; 
    iflimit = ztype ( p ) ; 
    condptr = link ( p ) ; 
    freenode ( p , ifnodesize ) ; 
  } else
    iflimit = ficode ; 
}

#if 0		/* nach filename.c */

/* =========  Part 29: File Names =========== */

/* Diese Routinen sollte man so abaendern, dass sie vielleicht statt dem
 * String Pool feste oder dynamische C-Arrays benutzen.  TeX hat jetzt die
 * unangenehme Eigenschaft, fuer jeden(!) Filenamen, selbst wenn er schon
 * vorhanden ist, einen neuen String yu verwenden.
 */

/*
 *  Die naechsten drei Routinen werden nur von scanfilename() und
 *  promptfilename() aufgerufen.
 */

  static
void beginname ( void )
{ beginname_regmem 

  areadelimiter = 0;
  extdelimiter = 0;
} 

  static
boolean morename ( ASCIIcode c )
{ morename_regmem

  if ( c == 32 )
    return(false);

  strroom( 1 );
  appendchar( c );
  if ( c == 47 ) {		/* / */
    areadelimiter = curlength;
    extdelimiter = 0;
  } else if ( c == 46 )		/* . */
    extdelimiter = curlength;

  return(true);
}

  static
void endname ( void )
{ endname_regmem

  if ( strptr + 3 > maxstrings )
    overflow(1, maxstrings - initstrptr);

  /* Falls kein Directory angegeben wurde, ist curarea = "",
   * ansonsten wird curarea auf das Directory gesetzt.
   */
  if ( areadelimiter == 0 )
    curarea = 335;
  else {
    curarea = strptr;
    strstart [ strptr + 1 ] = strstart [ strptr ] + areadelimiter;
    incr ( strptr );
  }

  /* Falls keine Extension angegeben wurde, ist curext = "" und curname
   * der aktuelle String.
   * Ansonsten ist curname der String bis zu curext und curext der restliche
   * aktuelle String.
   */
  if ( extdelimiter == 0 ) {
    curext = 335;
    curname = makestring();
  } else {
    curname = strptr;
    strstart[strptr+1] = strstart[strptr] + extdelimiter - areadelimiter - 1;
    incr ( strptr );
    curext = makestring();
  }
}



void packfilename( strnumber n, strnumber a, strnumber e )
{ packfilename_regmem
  register integer k;
  register ASCIIcode c;
  register poolpointer j;

  k = 0;
  {register integer for_end; j = strstart [ a ] ; for_end = strstart [ a + 1 
  ] - 1 ; if ( j <= for_end) do 
    {
      c = strpool [ j ] ; 
      incr ( k ) ; 
      if ( k <= FILENAMESIZE ) 
      nameoffile [ k ] = xchr [ c ] ; 
    } 
  while ( j++ < for_end ) ; } 
  {register integer for_end; j = strstart [ n ] ; for_end = strstart [ n + 1 
  ] - 1 ; if ( j <= for_end) do 
    {
      c = strpool [ j ] ; 
      incr ( k ) ; 
      if ( k <= FILENAMESIZE ) 
      nameoffile [ k ] = xchr [ c ] ; 
    } 
  while ( j++ < for_end ) ; } 
  {register integer for_end; j = strstart [ e ] ; for_end = strstart [ e + 1 
  ] - 1 ; if ( j <= for_end) do 
    {
      c = strpool [ j ] ; 
      incr ( k ) ; 
      if ( k <= FILENAMESIZE ) 
      nameoffile [ k ] = xchr [ c ] ; 
    } 
  while ( j++ < for_end ) ; } 

  if ( k <= FILENAMESIZE ) 
   namelength = k ; 
  else
    namelength = FILENAMESIZE ; 

  {register integer for_end; k = namelength + 1 ; for_end = FILENAMESIZE 
  ; if ( k <= for_end) do 
    nameoffile [ k ] = ' ' ; 
  while ( k++ < for_end ) ; } 
}


void packbufferedname ( smallnumber n, integer a, integer b )
{ packbufferedname_regmem 
  register integer k  ; 
  register ASCIIcode c  ; 
  register integer j  ; 

  if ( n + b - a + 5 > FILENAMESIZE ) 
    b = a + FILENAMESIZE - n - 5 ; 
  k = 0 ; 
  {register integer for_end; j = 1 ; for_end = n ; if ( j <= for_end) do 
    {
      c = xord [ TEXformatdefault [ j ] ] ; 
      incr ( k ) ; 
      if ( k <= FILENAMESIZE ) 
      nameoffile [ k ] = xchr [ c ] ; 
    } 
  while ( j++ < for_end ) ; } 
  {register integer for_end; j = a ; for_end = b ; if ( j <= for_end) do 
    {
      c = buffer [ j ] ; 
      incr ( k ) ; 
      if ( k <= FILENAMESIZE ) 
      nameoffile [ k ] = xchr [ c ] ; 
    } 
  while ( j++ < for_end ) ; } 
  {register integer for_end; j = formatdefaultlength - 3 ; for_end = 
  formatdefaultlength ; if ( j <= for_end) do 
    {
      c = xord [ TEXformatdefault [ j ] ] ; 
      incr ( k ) ; 
      if ( k <= FILENAMESIZE ) 
      nameoffile [ k ] = xchr [ c ] ; 
    } 
  while ( j++ < for_end ) ; } 
  if ( k <= FILENAMESIZE ) 
  namelength = k ; 
  else namelength = FILENAMESIZE ; 
  {register integer for_end; k = namelength + 1 ; for_end = FILENAMESIZE 
  ; if ( k <= for_end) do 
    nameoffile [ k ] = ' ' ; 
  while ( k++ < for_end ) ; } 
}


strnumber makenamestring ( void )
{ makenamestring_regmem 
  register integer k  ; 

  if ( ( poolptr + namelength > poolsize ) || ( strptr == maxstrings )
	|| ( curlength > 0 ) )
    return( (strnumber)63 );

  {register integer for_end; k = 1 ; for_end = namelength ; if ( k <= 
  for_end) do 
    appendchar ( xord [ nameoffile [ k ] ] ) ; 
  while ( k++ < for_end ) ; } 

  return( makestring () );
} 


#if 0
strnumber amakenamestring ( alphafile f )
{ amakenamestring_regmem

  return( makenamestring () );
}


strnumber bmakenamestring ( bytefile f )
{ bmakenamestring_regmem 

  return( makenamestring () );
}


strnumber wmakenamestring ( wordfile f )
{ wmakenamestring_regmem 

  return( makenamestring () );
}
#endif


void scanfilename ( void )
{ scanfilename_regmem
  register eightbits r_curcmd;

  nameinprogress = true;
  beginname();
  r_curcmd = getxnbtoken(0);

  while ( true ) {
    if ( r_curcmd > 12 || (curchr > 255) ) {
      backinput ();
      goto lab30;
    }
    if ( ! morename( curchr ) )
      goto lab30;
    r_curcmd = getxtoken();
  }

lab30:
  endname();
  nameinprogress = false;
}


void packjobname ( strnumber s )
{ packjobname_regmem

  curarea = 335 ;
  curext = s ;
  curname = jobname ;
  packfilename ( curname , curarea , curext );
}


/* Da wir nach der Adresse des naechsten Strings abfragen (ist schneller
 * als `strcmp()', wird er hier einmalig angelegt.
 */
static char input_file_name[] = "input file name";


void promptfilename ( char *s, strnumber e )
{/* 30 */ promptfilename_regmem 
  register integer k  ; 

  if ( interaction == scrollmode ) 
    wakeupterminal () ; 
  if ( s == input_file_name ) {
    print_err("I can't find file `");
  } else {
    print_err("I can't write on file `");
  } 
  printfilename ( curname , curarea , curext ) ; 
  c_print("'.");
  if ( e == STR_DOT_TEX )
    showcontext();
  c_printnl("Please type another ");
  c_print ( s );
  if ( interaction < scrollmode ) 
    fatalerror( STR_H_FE_JOBAB_FILE );
  clearterminal () ; 
  terminput(": ");
  {
    beginname () ; 
    k = first ; 
    while ( ( buffer [ k ] == 32 ) && ( k < last ) )
      incr ( k );
    while ( true ) {
      if ( k == last ) 
	goto lab30 ; 
      if ( ! morename ( buffer [ k ] ) ) 
	goto lab30 ; 
      incr ( k ) ; 
    }
lab30:
    endname () ; 
  } 
  if ( curext == 335 ) 
    curext = e;
  packfilename ( curname , curarea , curext ) ; 
}


void openlogfile ( void )
{ openlogfile_regmem 
  integer oldsetting;
  register integer k;
  register integer l;
  ccharpointer months;

  oldsetting = selector;

  if ( jobname == 0 ) 
    jobname = STR_TEXPUT;
  packjobname( STR_DOT_LOG );  
  while ( ! aopenout ( logfile ) ) {
    selector = termonly ; 
    promptfilename("transcript file name", STR_DOT_LOG );
  }
  logname = amakenamestring ( logfile ) ; 
  selector = logonly ; 
  logopened = true ; 
  {
#if 0
    (void) Fputs( logfile ,  "This is TeX, C Version 3.1t2" ) ; 
#else
    (void) Fputs( logfile, banner );
#endif
#ifdef MLTEX
  /* if user has given ML-TeX switch in IniTeX, output a second line */
  if( is_ML_TeX )
    (void) Fputs(logfile, "\nThis is ML-TeX, C Version 3+");
#endif
    print ( formatident ) ; 
    c_print("  ");
    printint ( zday ) ; 
    printchar ( 32 ) ; 
    months = "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC";
    {register integer for_end; k = 3 * zmonth - 3 ; for_end = 3 * zmonth - 1
    ; if ( k <= for_end) do 
      (void) putc( months [ k ] ,  logfile );
    while ( k++ < for_end ) ; } 
    printchar ( 32 ) ; 
    printint ( zyear ) ; 
    printchar ( 32 ) ; 
    printtwo ( ztime / 60 ) ; 
    printchar ( 58 ) ; 
    printtwo ( ztime % 60 ) ; 
  }
#ifdef INP_PTR
  /*  Da bei INP_PTR curinput auf oberstes Element von inputstack[] zeigt ..
   *  ist nichts zu tun.
   */
#else
  inputstack [ inputptr ] = curinput ; 
#endif
  c_printnl("**");
  l = inputstack [ 0 ] .limitfield ; 
  if ( buffer [ l ] == endlinechar ) 
    decr ( l );
  {register integer for_end; k = 1 ; for_end = l ; if ( k <= for_end) do 
    print ( buffer [ k ] ) ; 
  while ( k++ < for_end ) ; } 
  println () ; 
  selector = oldsetting + 2 ; 
}


void startinput ( void )
{/* 30 */ startinput_regmem 

  scanfilename ();
  if ( curext == 335 ) 
    packfilename ( curname , curarea , STR_DOT_TEX );
  else
    packfilename ( curname , curarea , curext );

  while ( true ) {
    beginfilereading () ; 
    if ( aopenin ( inputfile [ curinput .indexfield ] , TEXINPUTPATH ) ) 
      goto lab30 ; 
    if ( curext == 335 ) {
      packfilename ( curname , curarea , curext ) ; 
      if ( aopenin ( inputfile [ curinput .indexfield ] , TEXINPUTPATH ) ) 
      goto lab30 ; 
    } 
    endfilereading () ; 
    promptfilename( input_file_name, STR_DOT_TEX );
  }
lab30:
  curinput .namefield = amakenamestring ( inputfile[curinput.indexfield] );
  if ( jobname == 0 ) {
    jobname = curname;
    openlogfile();
  }
  if ( termoffset + length ( curinput .namefield ) > maxprintline - 3 )
    println();
  else if ( ( termoffset > 0 ) || ( fileoffset > 0 ) )
    printchar ( 32 );
  printchar ( 40 );
  incr ( openparens );
  print ( curinput .namefield );
  flush ( stdout );
  curinput .statefield = 33;

  line = 1;
  if ( inputln ( inputfile [ curinput .indexfield ] , false ) )
    ;

  curinput.limitfield = last;	/* (br) added */
  if ( pausing > 0 )		/* (br) added */
    firmuptheline () ;

  if ( endlinecharinactive () ) 
    decr ( curinput .limitfield ) ; 
  else
    buffer [ curinput .limitfield ] = endlinechar ; 
  first = curinput .limitfield + 1 ; 
  curinput .locfield = curinput .startfield ; 
}

#endif

/* -- end -- */
