#define EXTERN extern
#include "texd.h"

void charwarning ( internalfontnumber f, eightbits c )
{ charwarning_regmem

  if ( tracinglostchars > 0 ) {
    begindiagnostic();
    c_printnl("Missing character: There is no ");  print( c );
    c_print(" in font ");
#if 0  /* TeX 3.141 */
    print( fontname(f) );
#else
    slowprint( fontname(f) );
#endif
    printchar( 33 );
    enddiagnostic( false );
  }
}

  long_halfword
newcharacter( internalfontnumber f, eightbits c )
{ newcharacter_regmem 

  if ( fontbc(f) <= c && fontec(f) >= effective_char(c) ) {
    if ( charexists ( zcharinfo ( f ,  c ) ) ) {
      register long_halfword p;

      p = getavail();
      font( p ) = f;
      character( p ) = c;
      return(p);
    }
  }
  charwarning( f, c );
  return (long_halfword) 0;
}



void scanspec ( groupcode c, boolean threecodes )
{ scanspec_regmem 
  register integer s;
  register schar speccode;

  if ( threecodes ) 
    s = saved ( 0 );

  if ( scankeyword( STR_TO ) )
    speccode = 0 ; 
  else if ( scankeyword( STR_SPREAD ) )
    speccode = 1 ; 
  else {
    speccode = 1 ; 
    curval = 0 ; 
    goto lab40 ; 
  } 
  scandimen ( false , false , false ) ; 

lab40:
  if ( threecodes ) {
    saved ( 0 ) = s ; 
    incr ( saveptr ) ; 
  } 
  saved ( 0 ) = speccode ; 
  saved ( 1 ) = curval ; 
  saveptr = saveptr + 2 ; 
  newsavelevel ( c ) ; 
  scanleftbrace ();
}


  long_halfword
hpack( halfword p, scaled w, smallnumber m )
{ long_halfword r; 

  r = getnode( boxnodesize );

  { hpack_regmem 
  halfword q;
  register scaled h, d, x;
  glueord o;

#ifdef TEXXET
  halfword LR_Ptr = 0;
  integer LR_problems = 0;
#endif


/* ztype ( r ) = hlistnode ;  subtype ( r ) = 0 ; */
  set_type_subtype( r, hlistnode, 0);

  shiftamount ( r ) = 0;
  q = r + listoffset;
  link ( q ) = p;

  lastbadness = 0;
  h = 0;
  d = 0;
  x = 0;
  totalstretch [ normal ] = 0 ; 
  totalstretch [ fil ] = 0 ; 
  totalstretch [ fill ] = 0 ; 
  totalstretch [ filll ] = 0 ; 
  totalshrink [ normal ] = 0 ; 
  totalshrink [ fil ] = 0 ; 
  totalshrink [ fill ] = 0 ; 
  totalshrink [ filll ] = 0 ; 

  while ( p != 0 ) {
      /* 651. examine node p in the hlist ... */
lab21:
    while ( ischarnode ( p ) ) {
      register internalfontnumber f;
      register eightbits hd;
      fourquarters i;

      f = font ( p ) ; 
      i = zcharinfo ( f ,  character ( p ) ) ; 
      x = x + zcharwidth( f, i );
      hd = heightdepth( i );
      { register scaled s;
      s = zcharheight ( f, hd ) ; 
      if ( s > h ) 
	h = s ; 
      s = zchardepth ( f, hd ) ; 
      if ( s > d ) 
	d = s;
      }
      p = link ( p );
    }
    if ( p != 0 ) {
      switch ( ztype ( p ) ) {
      case hlistnode : 
      case vlistnode : 
      case rulenode : 
      case unsetnode : 
	{ register scaled s;

	  x = x + width ( p ) ; 
	  if ( ztype ( p ) >= rulenode ) 
	    s = 0 ; 
	  else
	    s = shiftamount ( p ) ; 
	  if ( height ( p ) - s > h ) 
	    h = height ( p ) - s ; 
	  if ( depth ( p ) + s > d ) 
	    d = depth ( p ) + s;
	}
	break ; 
      case insnode : 
      case marknode : 
      case adjustnode :
	{ register halfword r_adjusttail = adjusttail;

	  if ( r_adjusttail != 0 ) {
	    while ( link ( q ) != p )
	      q = link ( q ) ;
	    if ( ztype ( p ) == adjustnode ) {
	      link ( r_adjusttail ) = adjustptr ( p ) ; 
	      while ( link ( r_adjusttail ) != 0 )
		r_adjusttail = link ( r_adjusttail );
	      p = link ( p ) ; 
	      freenode ( link ( q ) , smallnodesize ) ; 
	    } else {
	      link ( r_adjusttail ) = p ; 
	      r_adjusttail = p;
	      p = link ( p ) ; 
	    } 
	    link ( q ) = p ; 
	    p = q ;

            adjusttail = r_adjusttail;
	  }
	  break ;
	}
      case whatsitnode :
	break;

      case gluenode:
	{ halfword g;

	  g = glueptr ( p ) ; 
	  x = x + width ( g ) ; 
	  o = stretchorder ( g ) ; 
	  totalstretch [ o ] +=  stretch( g );
	  o = shrinkorder ( g ) ; 
	  totalshrink [ o ] +=  shrink( g );
	  if ( subtype ( p ) >= aleaders ) {
	    g = leaderptr ( p ) ; 
	    if ( height ( g ) > h ) 
	    h = height ( g ) ; 
	    if ( depth ( g ) > d ) 
	    d = depth ( g ) ; 
	  }
	}
	break ; 
#ifdef TEXXET
      case kernnode : 
	x = x + width ( p ) ; 
	break ; 
      case mathnode : 
	x = x + width ( p ) ; 
	/* 1384. Adjust the LR stack for the hpack routine. */
	if( end_LR(p) ) {
	  if( saveinfo(LR_ptr) == subtype(p) ) {
	    pop_LR;
	  } else {
	    incr(LR_problems);
	    while( link(q) != p )
		q = link(q);
	    link(q) = link(p);
	    freenode(p, smallnodesize);
	    p = q;
	  }
	} else {
	  push_LR(p);
	}
	break;
#else
      case kernnode : 
      case mathnode : 
	x = x + width ( p ) ; 
	break ; 
#endif
      case ligaturenode : 
	{
	  mem [ ligtrick ] = mem [ ligchar ( p ) ] ; 
	  link ( ligtrick ) = link ( p ) ; 
	  p = ligtrick ; 
	  goto lab21 ; 
	} 
	break ; 
      default: 
	break ; 
      } 
      p = link ( p ) ; 
    } 
  }

  if ( adjusttail != 0 ) 
    link ( adjusttail ) = 0 ; 
  height ( r ) = h ; 
  depth ( r ) = d ; 
  if ( m == 1 ) 
    w = x + w ; 
  width ( r ) = w ; 
  x = w - x ; 
  if ( x == 0 ) {
    gluesign ( r ) = normal ; 
    glueorder ( r ) = normal ; 
    glueset ( r ) = 0.0 ; 
    goto lab10 ; 
  } else if ( x > 0 ) {
    if ( totalstretch [ filll ] != 0 ) 
      o = filll ; 
    else if ( totalstretch [ fill ] != 0 ) 
      o = fill ; 
    else if ( totalstretch [ fil ] != 0 ) 
      o = fil ; 
    else
      o = normal ; 
    glueorder ( r ) = o ; 
    gluesign ( r ) = stretching ; 
    if ( totalstretch [ o ] != 0 ) 
      glueset ( r ) = x / ((double) totalstretch [ o ] ) ; 
    else {
      gluesign ( r ) = normal ; 
      glueset ( r ) = 0.0 ; 
    } 
    if ( o == normal ) 
    if ( listptr ( r ) != 0 ) {
      lastbadness = badness ( x , totalstretch [ normal ] ) ; 
      if ( lastbadness > hbadness ) {
	println () ; 
	if ( lastbadness > 100 ) 
	  c_printnl("Underfull");
	else
	  c_printnl("Loose");
	c_print(" \\hbox (badness ");
	printint ( lastbadness ) ; 
	goto lab50 ; 
      } 
    } 
    goto lab10 ; 
  } else {
    if ( totalshrink [ filll ] != 0 ) 
      o = filll ; 
    else if ( totalshrink [ fill ] != 0 ) 
      o = fill ; 
    else if ( totalshrink [ fil ] != 0 ) 
      o = fil ; 
    else
      o = normal ; 
    glueorder ( r ) = o ; 
    gluesign ( r ) = shrinking ; 
    if ( totalshrink [ o ] != 0 ) 
      glueset ( r ) = ( - (integer) x ) / ((double) totalshrink [ o ] ) ; 
    else {
      gluesign ( r ) = normal ; 
      glueset ( r ) = 0.0 ; 
    }
    if( (totalshrink[o] < - (integer) x) && (o == normal)
					 && (listptr(r) != 0) ) {
      lastbadness = 1000000L ; 
      glueset ( r ) = 1.0 ; 
      if( (- (integer) x - totalshrink[normal] > hfuzz) || (hbadness < 100) ) {
	if( (overfullrule > 0)
		&& (- (integer)x - totalshrink[normal] > hfuzz) ) {
	  while ( link ( q ) != 0 )
	    q = link( q );
	  link ( q ) = newrule () ; 
	  width ( link ( q ) ) = overfullrule;
	}
	println () ; 
	c_printnl("Overfull \\hbox (");
	printscaled ( - (integer) x - totalshrink [ normal ] ) ; 
	c_print("pt too wide");
	goto lab50 ; 
      }
    } else if ( o == normal ) {
      if ( listptr ( r ) != 0 ) {
	lastbadness = badness ( - (integer) x, totalshrink[normal] );
	if ( lastbadness > hbadness ) {
	  println ();
	  c_printnl("Tight \\hbox (badness ");
	  printint ( lastbadness );
	  goto lab50;
	}
      }
    }
    goto lab10;
  }

lab50:	/* common_ending: */
  if ( outputactive ) 
    c_print(") has occurred while \\output is active");
  else {
    if ( packbeginline != 0 ) {
      if ( packbeginline > 0 ) 
	c_print(") in paragraph at lines ");
      else
	c_print(") in alignment at lines ");
      printint ( abs ( packbeginline ) ) ; 
      c_print("--");
    } else
      c_print(") detected at line ");
    printint ( line ) ; 
  }
  println () ; 
  fontinshortdisplay = nullfont ; 
  shortdisplay ( listptr ( r ) ) ; 
  println () ; 
  begindiagnostic () ; 
  showbox ( r ) ; 
  enddiagnostic ( true ) ; 

lab10:
#ifdef TEXXET
  /* 1385. Check for LR anomalies at the end of hpack. */
  if( LR_ptr != 0 ) {
    while( link(q) != 0 )
      q = link(q);
    do {
      tempptr = q;  q = newmath(0, info(LR_ptr));  link(tempptr) = q;
      LR_problems += 10000;  pop_LR;
    } while( LR_ptr != 0 );
  }
  if( LR_problems > 0 ) {
    println;  c_printnl("\\endL or \\endR problem (");
    printint(LR_problems / 10000); c_print(" missing, ");
    printint(LR_problems % 10000); c_print(" extra");
    LR_problems = 0;
    goto lab50;
  }
#endif

  return(r);
  }
}


long_halfword vpackage ( halfword p, scaled h, smallnumber m, scaled l )
{ vpackage_regmem
  register long_halfword r;
  register scaled w, d, x  ; 
  register halfword g  ; 
  register glueord o  ; 

  lastbadness = 0 ; 
  r = getnode ( boxnodesize ) ; 
  ztype ( r ) = vlistnode ; 
  subtype ( r ) = 0 ; 
  shiftamount ( r ) = 0 ; 
  listptr ( r ) = p ; 
  w = 0 ; 
  d = 0 ; 
  x = 0 ; 
  totalstretch [ normal ] = 0 ; 
  totalshrink [ normal ] = 0 ; 
  totalstretch [ fil ] = 0 ; 
  totalshrink [ fil ] = 0 ; 
  totalstretch [ fill ] = 0 ; 
  totalshrink [ fill ] = 0 ; 
  totalstretch [ filll ] = 0 ; 
  totalshrink [ filll ] = 0 ; 

  while ( p != 0 ) {
    if ( ischarnode ( p ) ) 
      confusion("vpack");
    else switch ( ztype ( p ) ) {
    case hlistnode : 
    case vlistnode : 
    case rulenode : 
    case unsetnode : 
      { register scaled s;

	x = x + d + height ( p ) ; 
	d = depth ( p ) ; 
	if ( ztype ( p ) >= rulenode ) 
	  s = 0 ; 
	else
	  s = shiftamount ( p ) ; 
	if ( width ( p ) + s > w ) 
	  w = width ( p ) + s ; 
      }
      break ; 
    case whatsitnode : 
      break ; 
    case gluenode : 
      {
	x = x + d ; 
	d = 0 ; 
	g = glueptr ( p ) ; 
	x = x + width ( g ) ; 
	o = stretchorder ( g ) ; 
	totalstretch [ o ] = totalstretch [ o ] + stretch ( g ) ; 
	o = shrinkorder ( g ) ; 
	totalshrink [ o ] = totalshrink [ o ] + shrink ( g ) ; 
	if ( subtype ( p ) >= aleaders ) {
	  g = leaderptr ( p ) ; 
	  if ( width ( g ) > w ) 
	  w = width ( g ) ; 
	} 
      } 
      break ; 
    case kernnode : 
      {
	x = x + d + width ( p ) ; 
	d = 0 ; 
      } 
      break ; 
    default: 
      break ; 
    } 
    p = link ( p ) ; 
  } 
  width ( r ) = w ; 
  if ( d > l ) {
    x = x + d - l ; 
    depth ( r ) = l ; 
  } else
    depth ( r ) = d ; 
  if ( m == 1 ) 
  h = x + h ; 
  height ( r ) = h ; 
  x = h - x ; 
  if ( x == 0 ) {
    gluesign ( r ) = normal ; 
    glueorder ( r ) = normal ; 
    glueset ( r ) = 0.0 ; 
    goto lab10 ; 
  } else if ( x > 0 ) {
    if ( totalstretch [ filll ] != 0 ) 
    o = filll ; 
    else if ( totalstretch [ fill ] != 0 ) 
    o = fill ; 
    else if ( totalstretch [ fil ] != 0 ) 
    o = fil ; 
    else o = normal ; 
    glueorder ( r ) = o ; 
    gluesign ( r ) = stretching ; 
    if ( totalstretch [ o ] != 0 ) 
      glueset ( r ) = x / ((double) totalstretch [ o ] ) ; 
    else {
      gluesign ( r ) = normal ; 
      glueset ( r ) = 0.0 ; 
    } 
    if ( o == normal ) 
    if ( listptr ( r ) != 0 ) {
      lastbadness = badness ( x , totalstretch [ normal ] ) ; 
      if ( lastbadness > vbadness ) {
	println () ; 
	if ( lastbadness > 100 ) 
	  c_printnl("Underfull");
	else
	  c_printnl("Loose");
	c_print(" \\vbox (badness ");
	printint ( lastbadness ) ; 
	goto lab50 ; 
      } 
    } 
    goto lab10 ; 
  } else {
    if ( totalshrink [ filll ] != 0 ) 
    o = filll ; 
    else if ( totalshrink [ fill ] != 0 ) 
    o = fill ; 
    else if ( totalshrink [ fil ] != 0 ) 
    o = fil ; 
    else o = normal ; 
    glueorder ( r ) = o ; 
    gluesign ( r ) = shrinking ; 
    if ( totalshrink [ o ] != 0 ) 
      glueset ( r ) = ( - (integer) x ) / ((double) totalshrink [ o ] ) ; 
    else {
      gluesign ( r ) = normal ; 
      glueset ( r ) = 0.0 ; 
    } 
    if ( ( totalshrink [ o ] < - (integer) x ) && ( o == normal ) && ( listptr 
    ( r ) != 0 ) ) 
    {
      lastbadness = 1000000L ; 
      glueset ( r ) = 1.0 ; 
      if ( (-(integer)x - totalshrink[normal] > vfuzz) || (vbadness < 100) ) {
	println();
	c_printnl("Overfull \\vbox (");
	printscaled ( - (integer) x - totalshrink[normal] );
	c_print("pt too high");
	goto lab50;
      }
    }
    else if ( o == normal ) 
    if ( listptr ( r ) != 0 ) {
      lastbadness = badness ( - (integer) x , totalshrink [ normal ] ) ; 
      if ( lastbadness > vbadness ) {
	println () ; 
	c_printnl("Tight \\vbox (badness ");
	printint ( lastbadness ) ; 
	goto lab50 ; 
      } 
    } 
    goto lab10 ; 
  } 

lab50:
  if ( outputactive ) 
    c_print(") has occurred while \\output is active");
  else {
    if ( packbeginline != 0 ) {
      c_print(") in alignment at lines ");
      printint ( abs ( packbeginline ) ) ; 
      c_print("--");
    } else
      c_print(") detected at line ");
    printint ( line ) ; 
    println () ; 
  }
  begindiagnostic () ; 
  showbox ( r ) ; 
  enddiagnostic ( true ) ; 

lab10:
  return(r) ; 
}


void appendtovlist ( halfword b )
{ appendtovlist_regmem
  register scaled d;
  register halfword p;

  if ( curlist .auxfield .cint > ignoredepth ) {
    d = width ( baselineskip ) - curlist .auxfield .cint - height ( b ) ; 
    if ( d < lineskiplimit ) 
      p = newparamglue ( lineskipcode ) ; 
    else {
      p = newskipparam ( baselineskipcode ) ; 
      width ( tempptr ) = d ; 
    } 
    link ( curlist .tailfield ) = p ; 
    curlist .tailfield = p ; 
  } 
  link ( curlist .tailfield ) = b ; 
  curlist .tailfield = b ; 
  curlist .auxfield .cint = depth ( b ) ; 
}


long_halfword newnoad ( void )
{ register long_halfword p ;

  p = getnode ( noadsize ) ; 

  { newnoad_regmem 

/* ztype ( p ) = ordnoad ;  subtype ( p ) = normal ; */
  set_type_subtype(p, ordnoad, normal);

  mem [ nucleus ( p ) ] .hh = emptyfield ;
  mem [ subscr ( p ) ] .hh = emptyfield ;
  mem [ supscr ( p ) ] .hh = emptyfield ;
  }
  return(p) ; 
} 


long_halfword newstyle ( smallnumber s )
{ register long_halfword p  ; 

  p = getnode ( stylenodesize ) ;

  { newstyle_regmem 

/* ztype ( p ) = stylenode ;  subtype ( p ) = s ; */
  set_type_subtype(p, stylenode, s);
  width ( p ) = 0 ; 
  depth ( p ) = 0 ; 
  }
  return(p);
}


long_halfword newchoice ( void )
{ register long_halfword p ;

  p = getnode ( stylenodesize ) ; 

  { newchoice_regmem 

/* ztype ( p ) = choicenode ;  subtype ( p ) = 0 ; */
  set_type_subtype(p, choicenode, 0);
  textmlist ( p ) = 0 ;
  displaymlist ( p ) = 0 ;
  scriptscriptmlist ( p ) = 0 ;
  scriptmlist ( p ) = 0 ;
  }
  return(p);
}


#if 0	/* (br) unnoetig, nur wegen PASCAL eingefuehrt */
void showinfo ( void )
{ showinfo_regmem

  shownodelist ( info ( tempptr ) ) ; 
}
#endif


long_halfword fractionrule ( scaled t )
{ register long_halfword p ;

  p = newrule () ; 
 
  { fractionrule_regmem

  height ( p ) = t ; 
  depth ( p ) = 0 ;
  }
  return(p);
} 


long_halfword overbar ( halfword b, scaled k, scaled t )
{ overbar_regmem 
  register halfword p, q  ; 

  p = newkern ( k ) ; 
  link ( p ) = b ; 
  q = fractionrule ( t ) ; 
  link ( q ) = p ; 
  p = newkern ( t ) ; 
  link ( p ) = q ; 

  return( vpackage ( p , 0 , 1 , maxdimen ) );
} 


long_halfword charbox ( internalfontnumber f, quarterword c )
{ register long_halfword b;

  b = newnullbox ();

 { charbox_regmem
  { fourquarters q;
    register eightbits hd;

    q = zcharinfo (f, c);
    hd = heightdepth ( q ) ; 
    width ( b ) = zcharwidth ( f ,  q ) + zcharitalic ( f ,  q ) ; 
    height ( b ) = zcharheight ( f ,  hd ) ; 
    depth ( b ) = zchardepth ( f ,  hd );
  }
  { register long_halfword p;

    p = getavail ();
    character ( p ) = c;
    font ( p ) = f;
    listptr ( b ) = p;
  }
 }
  return(b);
}


void stackintobox ( halfword b, internalfontnumber f, quarterword c )
{ register halfword p;

  p = charbox( f, c );

  { stackintobox_regmem

  link ( p ) = listptr( b );
  listptr ( b ) = p ; 
  height ( b ) = height ( p ) ;
  }
}


scaled heightplusdepth ( internalfontnumber f, quarterword c )
{ heightplusdepth_regmem 
  fourquarters q  ; 
  register eightbits hd  ; 

  q = zcharinfo ( f ,  c ) ; 
  hd = heightdepth ( q ) ; 
  return( zcharheight ( f ,  hd ) + zchardepth ( f ,  hd ) );
} 


long_halfword vardelimiter ( halfword d, smallnumber s, scaled v )
{ vardelimiter_regmem 
  register long_halfword b  ; 
  register internalfontnumber f, g  ; 
  register quarterword c, x, y  ; 
  register integer n;
  register int m;
  register scaled u  ; 
  register scaled w  ; 
  fourquarters q  ; 
  register eightbits hd  ; 
  fourquarters r  ; 
  register smallnumber z  ; 
  boolean largeattempt  ; 

  f = nullfont ; 
  w = 0 ; 
  largeattempt = false ; 
  z = smallfam ( d ) ; 
  x = smallchar ( d ) ; 

  while ( true ) {
    if ( ( z != 0 ) || ( x != 0 ) ) {
      z = z + s + 16 ; 
      do {
	z = z - 16 ; 
	g = famfnt ( z ) ; 
	if ( g != nullfont ) {
	  y = x ;
	  if ( y >= fontbc(g) && effective_char(y) <= fontec(g) ) {
lab22:
	    q = zcharinfo ( g ,  y ) ; 
	    if ( charexists ( q ) ) {
	      if ( chartag ( q ) == exttag ) {
		f = g ; 
		c = y ; 
		goto lab40 ; 
	      } 
	      hd = heightdepth ( q ) ; 
	      u = zcharheight ( g ,  hd ) + zchardepth ( g ,  hd ) ; 
	      if ( u > w ) {
		f = g ; 
		c = y ; 
		w = u ; 
		if ( u >= v ) 
		goto lab40 ; 
	      } 
	      if ( chartag ( q ) == listtag ) {
		y = rembyte ( q ) ; 
		goto lab22 ; 
	      } 
	    } 
	  } 
	} 
      } while ( ! ( z < 16 ) ) ; 
    }
    if ( largeattempt ) 
      goto lab40 ; 
    largeattempt = true ; 
    z = largefam ( d ) ; 
    x = largechar ( d ) ; 
  } 

lab40:
  if ( f != nullfont ) 
  if ( chartag ( q ) == exttag ) {
    b = newnullbox () ; 
    ztype ( b ) = vlistnode ; 
    r = fontinfo [ extenbase(f) + rembyte ( q ) ] .qqqq ; 
    c = extrep ( r ) ; 
    u = heightplusdepth ( f , c ) ; 
    w = 0 ; 
    q = zcharinfo ( f ,  c ) ; 
    width ( b ) = zcharwidth ( f ,  q ) + zcharitalic ( f ,  q ) ; 
    c = extbot ( r ) ; 
    if ( c != 0 ) 
      w = w + heightplusdepth ( f , c ) ; 
    c = extmid ( r ) ; 
    if ( c != 0 ) 
      w = w + heightplusdepth ( f , c ) ; 
    c = exttop ( r ) ; 
    if ( c != 0 ) 
      w = w + heightplusdepth ( f , c ) ; 
    n = 0 ; 
    if ( u > 0 ) 
    while ( w < v ) {
      w = w + u ; 
      incr ( n ) ; 
      if ( extmid ( r ) != 0 ) 
        w = w + u ; 
    }
    c = extbot ( r ) ; 
    if ( c != 0 ) 
      stackintobox ( b , f , c ) ; 
    c = extrep ( r ) ;
    for( m = n; m > 0; --m )
      stackintobox ( b , f , c );
    c = extmid ( r ) ; 
    if ( c != 0 ) {
      stackintobox ( b , f , c ) ; 
      c = extrep ( r ) ; 
      for( m = n; m > 0; --m )
	stackintobox ( b , f , c );
    }
    c = exttop ( r ) ; 
    if ( c != 0 ) 
      stackintobox ( b , f , c ) ; 
    depth ( b ) = w - height ( b ) ; 
  } else
    b = charbox ( f , c ) ; 
  else {
    b = newnullbox () ; 
    width ( b ) = nulldelimiterspace ; 
  } 
  shiftamount ( b ) = half ( height ( b ) - depth ( b ) ) - axisheight ( s ) ; 

  return(b);
}


long_halfword rebox ( long_halfword b, scaled w )
{ rebox_regmem 
  register halfword p  ; 
  register internalfontnumber f  ; 
  register scaled v  ; 

  if ( ( width ( b ) != w ) && ( listptr ( b ) != 0 ) ) {
    if ( ztype ( b ) == vlistnode ) 
      b = hpack ( b , 0 , 1 ) ; 
    p = listptr ( b ) ; 
    if ( ( ischarnode ( p ) ) && ( link ( p ) == 0 ) ) {
      f = font ( p ) ; 
      v = zcharwidth ( f ,  zcharinfo ( f ,  character ( p ) ) ) ; 
      if ( v != width ( b ) ) 
        link ( p ) = newkern ( width ( b ) - v ) ; 
    } 
    freenode ( b , boxnodesize ) ; 
    b = newglue ( ssglue ) ; 
    link ( b ) = p ; 
    while ( link ( p ) != 0 )
	p = link ( p ) ; 
    link ( p ) = newglue ( ssglue ) ; 
    return( hpack ( b , w , 0 ) );
  } else {
    width ( b ) = w ; 
    return(b);
  }
} 


long_halfword mathglue ( halfword g, scaled m )
{ mathglue_regmem 
  register long_halfword p  ; 
  register integer n  ; 
  register scaled f  ; 

  n = xovern ( m , 65536L ) ; 
  f = remainder ;
  if ( f < 0 ) {  /* TeX 3.141 */
    decr(n); f += 65536L;
  }
  p = getnode ( gluespecsize ) ; 
  width(p) = multandadd ( n, width(g), xnoverd( width(g), f, 65536L ),
				1073741823L );
  stretchorder(p) = stretchorder(g);
  if ( stretchorder(p) == normal )
    stretch(p) = multandadd ( n, stretch(g),
			xnoverd ( stretch(g), f, 65536L ), 1073741823L );
  else
    stretch(p) = stretch(g);
  shrinkorder(p) = shrinkorder(g);
  if ( shrinkorder(p) == normal ) 
    shrink(p) = multandadd ( n, shrink(g), xnoverd ( shrink(g), f, 65536L ),
				1073741823L );
  else
    shrink(p) = shrink(g);

  return(p);
} 


void mathkern ( halfword p, scaled m )
{ mathkern_regmem 
  register integer n  ; 
  register scaled f  ; 

  if ( subtype ( p ) == muglue ) {
    n = xovern ( m , 65536L ) ; 
    f = remainder ;
    if ( f < 0 ) {  /* TeX 3.141 */
      decr(n);  f += 65536L;
    }
    width(p) = multandadd ( n, width(p), xnoverd ( width(p), f, 65536L ),
				1073741823L );
    subtype ( p ) = normal ;
  }
}


void flushmath ( void )
{ flushmath_regmem 

  flushnodelist ( link ( curlist .headfield ) ) ; 
  flushnodelist ( curlist .auxfield .cint ) ; 
  link ( curlist .headfield ) = 0 ; 
  curlist .tailfield = curlist .headfield ; 
  curlist .auxfield .cint = 0 ; 
} 

/* -- end -- */
