#define EXTERN extern
#include "texd.h"

static halfword cleanbox (halfword p, smallnumber style);
static void	fetch (halfword a, smallnumber size);
static void	makeover (halfword q, smallnumber style, smallnumber size);
static void	makeunder (halfword q, smallnumber style, smallnumber size);
static void	makevcenter (halfword q, smallnumber size);
static void	makeradical (halfword q, smallnumber style, smallnumber size);
static void	makemathaccent (halfword q, smallnumber sty, smallnumber sz);
static void	makefraction (halfword q, smallnumber style, smallnumber size);
static scaled	makeop (halfword q, smallnumber style, smallnumber size);
static void	makeord (halfword q, smallnumber cursize);
static void	makescripts (halfword q, scaled delta, smallnumber curstyle,
					smallnumber cursize);
static smallnumber makeleftright (halfword q, smallnumber style,
					scaled maxd, scaled maxh);
static long_halfword
		mlisttohlist (long_halfword curmlist,
			      smallnumber curstyle,
			      int mlistpenalties);


   static halfword
cleanbox (halfword p, smallnumber curstyle)
{ cleanbox_regmem 
  register halfword x;
  register long_halfword q;

  switch ( mathtype ( p ) ) {
  case mathchar : 
    q = newnoad () ; 
    mem [ nucleus ( q ) ] = mem [ p ];
    break;
  case submlist : 
    q = info ( p );
    break ; 

  case subbox : 
    q = info ( p );
    goto lab40 ; 
    break ; 
  default: 
    q = newnullbox();
    goto lab40 ; 
    break ; 
  } 

  q = mlisttohlist (q, curstyle, false);
  /* q = link ( temphead ); */

lab40:
  if ( ischarnode(q) || (q == 0) )
    x = hpack( q, 0, 1 );
  else if( (link(q) == 0) && (ztype(q) <= vlistnode) && (shiftamount(q) == 0) )
    x = q;
  else
    x = hpack( q, 0, 1 );
  q = listptr ( x );
  if ( ischarnode ( q ) ) {
    register halfword r;

    r = link ( q );
    if( (r != 0) && (link(r) == 0)
	&& !ischarnode(r) && (ztype(r) == kernnode) ) {
      freenode( r, smallnodesize );
      link ( q ) = 0;
    }
  }
  return(x);
}


   static void
fetch (halfword a, smallnumber cursize)
{ fetch_regmem 

  curc = character ( a ) ; 
  curf = famfnt ( fam ( a ) + cursize );
  if ( curf == nullfont ) {
    print_err("");
    printsize( cursize );  printchar( 32 );  printint( fam(a) );
    c_print(" is undefined (character ");
    print ( curc );  printchar ( 41 );
    zhelp1( STR_H_SOMEWHERE_IN_MATH );
    error();
    curi = nullcharacter ; 
    mathtype ( a ) = 0 ; 
  } else {
    if ( curc >= fontbc(curf) && curc <= fontec(curf) )
      curi = zcharinfo ( curf ,  curc ) ; 
    else
      curi = nullcharacter;
    if ( ! ( charexists ( curi ) ) ) {
      charwarning ( curf , curc ) ; 
      mathtype ( a ) = 0;
    }
  }
}

   static void
makeover (halfword q, smallnumber curstyle, smallnumber cursize)
{ makeover_regmem 

  info(nucleus(q)) = overbar( cleanbox( nucleus(q), crampedstyle(curstyle) ),
			3 * defaultrulethickness, defaultrulethickness );
  mathtype( nucleus(q) ) = subbox;
}


   static void
makeunder (halfword q, smallnumber curstyle, smallnumber cursize)
{ makeunder_regmem 
  register halfword p, x, y  ; 
  register scaled delta  ; 

  x = cleanbox ( nucleus ( q ) , curstyle ) ; 
  p = newkern ( 3 * defaultrulethickness ) ; 
  link ( x ) = p ; 
  link ( p ) = fractionrule ( defaultrulethickness ) ; 

  y = vpackage ( x , 0 , 1 , maxdimen ) ; 
  delta = height ( y ) + depth ( y ) + defaultrulethickness ; 
  height ( y ) = height ( x ) ; 
  depth ( y ) = delta - height ( y ) ; 

  info ( nucleus ( q ) ) = y ; 
  mathtype ( nucleus ( q ) ) = subbox ; 
}


   static void
makevcenter (halfword q, smallnumber cursize)
{ makevcenter_regmem 
  register halfword v;
  register scaled delta;

  v = info ( nucleus ( q ) ) ; 
  if ( ztype ( v ) != vlistnode ) 
    confusion("vcenter");
  delta = height ( v ) + depth ( v ) ; 
  height ( v ) = axisheight ( cursize ) + half ( delta ) ; 
  depth ( v ) = delta - height ( v ) ; 
}


   static void
makeradical (halfword q, smallnumber curstyle, smallnumber cursize)
{ makeradical_regmem 
  register halfword x, y;
  register scaled delta, clr;

  x = cleanbox ( nucleus ( q ) , crampedstyle ( curstyle ) ) ; 
  if ( curstyle < textstyle ) 
    clr = defaultrulethickness + ( abs ( mathxheight ( cursize ) ) / 4 ) ; 
  else {
    clr = defaultrulethickness ; 
    clr = clr + ( abs ( clr ) / 4 ) ; 
  } 
  y = vardelimiter ( leftdelimiter ( q ) , cursize ,
		height ( x ) + depth ( x ) + clr + defaultrulethickness ) ;
  delta = depth ( y ) - ( height ( x ) + depth ( x ) + clr ) ; 
  if ( delta > 0 )
    clr = clr + half ( delta ) ; 
  shiftamount ( y ) = - (integer) ( height ( x ) + clr ) ; 
  link ( y ) = overbar ( x , clr , height ( y ) ) ; 
  info ( nucleus ( q ) ) = hpack ( y , 0 , 1 ) ; 
  mathtype ( nucleus ( q ) ) = subbox ; 
}


   static void
makemathaccent (halfword q, smallnumber curstyle, smallnumber cursize)
{ makemathaccent_regmem 
  register halfword x, y  ; 
  register quarterword c  ; 
  register internalfontnumber f  ; 
  fourquarters i  ; 
  register scaled s  ; 
  register scaled h  ; 
  register scaled delta  ; 
  register scaled w  ; 

  fetch ( accentchr ( q ), cursize ) ; 
  if ( charexists ( curi ) ) {
    i = curi ; 
    c = curc ; 
    f = curf ; 
    s = 0 ; 
    if ( mathtype ( nucleus ( q ) ) == mathchar ) {
      fetch ( nucleus ( q ), cursize ) ; 
      if ( chartag ( curi ) == ligtag ) {
#ifdef FONTPTR
	register SMALLmemoryword *ligp;

	ligp = zligkernstart ( curf ,  curi ) ;
	curi = ligp->qqqq ;
	if ( skipbyte ( curi ) > stopflag ) {
	  ligp = zligkernrestart ( curf ,  curi ) ; 
	  curi = ligp->qqqq ;
	} 
#else
	register integer a ;

	a = zligkernstart ( curf ,  curi ) ;
	curi = fontinfo [ a ] .qqqq ;
	if ( skipbyte ( curi ) > stopflag ) {
	  a = zligkernrestart ( curf ,  curi ) ; 
	  curi = fontinfo [ a ] .qqqq ;
	} 
#endif
	while ( true ) {
	  if ( nextchar ( curi ) == skewchar(curf) ) {
	    if( (opbyte(curi) >= kernflag) && (skipbyte(curi) <= stopflag) )
	      s = zcharkern( curf, curi );
	    goto lab31;
	  }
	  if ( skipbyte ( curi ) >= stopflag ) 
	    goto lab31 ;
#ifdef FONTPTR
	  ligp = ligp + skipbyte ( curi ) + 1 ; 
	  curi = ligp->qqqq ;
#else
	  a = a + skipbyte ( curi ) + 1 ; 
	  curi = fontinfo [ a ] .qqqq ;
#endif
	} 
      }
    }

lab31:
    x = cleanbox ( nucleus ( q ) , crampedstyle ( curstyle ) ) ; 
    w = width ( x ) ; 
    h = height ( x ) ; 
    while ( true ) {
      if ( chartag ( i ) != listtag ) 
        goto lab30 ; 
      y = rembyte ( i ) ; 
      i = zcharinfo ( f ,  y ) ; 
      if ( ! charexists ( i ) ) 
        goto lab30 ; 
      if ( zcharwidth ( f ,  i ) > w ) 
        goto lab30 ; 
      c = y ; 
    } 

lab30:
    if ( h < xheight ( f ) ) 
      delta = h ; 
    else
      delta = xheight ( f ) ; 
    if( (mathtype(supscr(q)) != 0) || (mathtype(subscr(q)) != 0) )
    if ( mathtype ( nucleus ( q ) ) == mathchar ) {
      flushnodelist ( x ) ; 
      x = newnoad () ; 
      mem [ nucleus ( x ) ] = mem [ nucleus ( q ) ] ; 
      mem [ supscr ( x ) ] = mem [ supscr ( q ) ] ; 
      mem [ subscr ( x ) ] = mem [ subscr ( q ) ] ; 
      mem [ supscr ( q ) ] .hh = emptyfield ; 
      mem [ subscr ( q ) ] .hh = emptyfield ; 
      mathtype ( nucleus ( q ) ) = submlist ; 
      info ( nucleus ( q ) ) = x ; 
      x = cleanbox ( nucleus ( q ) , curstyle ) ; 
      delta = delta + height ( x ) - h ; 
      h = height ( x ) ; 
    }
    y = charbox ( f , c ) ; 
    shiftamount ( y ) = s + half ( w - width ( y ) ) ; 
    width ( y ) = 0 ; 
    { register long_halfword p;

      p = newkern ( - (integer) delta ) ; 
      link ( p ) = x ; 
      link ( y ) = p ;
    }
    y = vpackage ( y , 0 , 1 , maxdimen ) ; 
    width ( y ) = width ( x ) ; 
    if ( height ( y ) < h ) {
      register long_halfword p;

      p = newkern ( h - height ( y ) ) ; 
      link ( p ) = listptr ( y ) ; 
      listptr ( y ) = p ; 
      height ( y ) = h ; 
    } 
    info ( nucleus ( q ) ) = y ; 
    mathtype ( nucleus ( q ) ) = subbox ; 
  }
}


   static void
makefraction (halfword q, smallnumber curstyle, smallnumber cursize)
{ makefraction_regmem 
  register halfword p, v, x, z;
  register scaled delta, shiftup, shiftdown;

  if ( thickness ( q ) == defaultcode ) 
    thickness ( q ) = defaultrulethickness;

  x = cleanbox ( numerator ( q ) , numstyle ( curstyle ) ) ; 
  z = cleanbox ( denominator ( q ) , denomstyle ( curstyle ) ) ; 
  if ( width ( x ) < width ( z ) )
    x = rebox ( x , width ( z ) ); 
  else
    z = rebox ( z , width ( x ) );

  if ( curstyle < textstyle ) {
    shiftup = num1 ( cursize ) ; 
    shiftdown = denom1 ( cursize ) ; 
  } else {
    shiftdown = denom2 ( cursize ) ; 
    if ( thickness ( q ) != 0 ) 
      shiftup = num2 ( cursize ) ; 
    else
      shiftup = num3 ( cursize ) ; 
  }

  if ( thickness ( q ) == 0 ) {
    register scaled clr;

    if ( curstyle < textstyle )
      clr = 7 * defaultrulethickness;
    else
      clr = 3 * defaultrulethickness;
    delta = half( clr - ( (shiftup - depth(x)) - (height(z) - shiftdown) ) );
    if ( delta > 0 ) {
      shiftup = shiftup + delta;
      shiftdown = shiftdown + delta;
    } 
  } else {
    register scaled clr, delta1, delta2;

    if ( curstyle < textstyle ) 
      clr = 3 * thickness ( q ) ; 
    else
      clr = thickness ( q ) ; 
    delta = half ( thickness ( q ) ) ; 
    delta1 = clr - ( (shiftup - depth(x)) - (axisheight(cursize) + delta) );
    delta2 = clr - ( (axisheight(cursize) - delta) - (height(z) - shiftdown) );
    if ( delta1 > 0 )
      shiftup = shiftup + delta1 ; 
    if ( delta2 > 0 ) 
      shiftdown = shiftdown + delta2 ; 
  }

  v = newnullbox ();
  ztype ( v ) = vlistnode;
  height ( v ) = shiftup + height ( x );
  depth ( v ) = depth ( z ) + shiftdown;
  width ( v ) = width ( x );

  if ( thickness ( q ) == 0 ) {
    p = newkern( ( shiftup - depth ( x ) ) - ( height ( z ) - shiftdown ) );
    link ( p ) = z;
  } else {
    register halfword y;

    y = fractionrule( thickness(q) );
    p = newkern( (axisheight(cursize) - delta) - (height(z) - shiftdown) );
    link ( y ) = p ; 
    link ( p ) = z ; 
    p = newkern( (shiftup - depth(x)) - (axisheight(cursize) + delta) );
    link ( p ) = y;
  } 

  link ( x ) = p;
  listptr ( v ) = x;

  if ( curstyle < textstyle ) 
    delta = delim1 ( cursize );
  else
    delta = delim2 ( cursize );

  x = vardelimiter( leftdelimiter(q), cursize, delta );
  link ( x ) = v;

  z = vardelimiter( rightdelimiter(q), cursize, delta );
  link ( v ) = z;

  newhlist(q) = hpack( x, 0, 1 );
}


   static scaled
makeop (halfword q, smallnumber curstyle, smallnumber cursize)
{ makeop_regmem
  register scaled delta;
  register halfword v, x, y, z;

  if ( ( subtype ( q ) == normal ) && ( curstyle < textstyle ) )
    subtype ( q ) = limits;

  if ( mathtype ( nucleus ( q ) ) == mathchar ) {
    fetch ( nucleus ( q ), cursize );
    if ( ( curstyle < textstyle ) && ( chartag ( curi ) == listtag ) ) {
      register quarterword c;
      fourquarters i;

      c = rembyte ( curi ) ; 
      i = zcharinfo ( curf ,  c ) ; 
      if ( charexists ( i ) ) {
	curc = c ; 
	curi = i ; 
	character ( nucleus ( q ) ) = c ; 
      }
    }
    delta = zcharitalic( curf, curi );
    x = cleanbox( nucleus(q), curstyle );
    if( (mathtype(subscr(q)) != 0) && (subtype(q) != limits) )
      width ( x ) = width ( x ) - delta;
    shiftamount(x) = half( height(x) - depth(x) ) - axisheight( cursize );
    mathtype ( nucleus ( q ) ) = subbox;
    info ( nucleus ( q ) ) = x;
  } else
    delta = 0;

  if ( subtype ( q ) == limits ) {
    x = cleanbox ( supscr ( q ) , supstyle ( curstyle ) ) ; 
    y = cleanbox ( nucleus ( q ) , curstyle ) ; 
    z = cleanbox ( subscr ( q ) , substyle ( curstyle ) ) ; 
    v = newnullbox () ; 
    ztype ( v ) = vlistnode ; 
    width ( v ) = width ( y ) ; 
    if ( width ( x ) > width ( v ) ) 
      width ( v ) = width ( x ) ; 
    if ( width ( z ) > width ( v ) ) 
      width ( v ) = width ( z ) ;
    x = rebox ( x , width ( v ) ) ; 
    y = rebox ( y , width ( v ) ) ; 
    z = rebox ( z , width ( v ) ) ; 
    shiftamount ( x ) = half ( delta ) ; 
    shiftamount ( z ) = - (integer) shiftamount ( x ) ; 
    height ( v ) = height ( y ) ; 
    depth ( v ) = depth ( y ) ; 

    if ( mathtype ( supscr ( q ) ) == 0 ) {
      freenode ( x , boxnodesize ) ; 
      listptr ( v ) = y ; 
    } else {
      register scaled shiftup;
      register long_halfword p;

      shiftup = bigopspacing3 - depth ( x ) ; 
      if ( shiftup < bigopspacing1 ) 
        shiftup = bigopspacing1 ; 
      p = newkern ( shiftup ) ; 
      link ( p ) = y ; 
      link ( x ) = p ; 
      p = newkern ( bigopspacing5 ) ; 
      link ( p ) = x;
      listptr ( v ) = p;
      height(v) = height(v) + bigopspacing5 + height(x) + depth(x) + shiftup;
    }

    if ( mathtype ( subscr ( q ) ) == 0 ) 
      freenode ( z, boxnodesize ) ; 
    else {
      register scaled shiftdown;
      register long_halfword p;

      shiftdown = bigopspacing4 - height ( z ) ; 
      if ( shiftdown < bigopspacing2 ) 
	shiftdown = bigopspacing2 ; 
      p = newkern ( shiftdown ) ; 
      link ( y ) = p ; 
      link ( p ) = z ; 
      p = newkern ( bigopspacing5 ) ; 
      link ( z ) = p ; 
      depth(v) = depth(v) + bigopspacing5 + height(z) + depth(z) + shiftdown;
    }

    newhlist ( q ) = v;
  }

  return(delta);
}


   static void
makeord (halfword q, smallnumber cursize)
{ makeord_regmem
  register halfword p;

lab20:
  if ( mathtype ( subscr ( q ) ) == 0 ) 
  if ( mathtype ( supscr ( q ) ) == 0 ) 
  if ( mathtype ( nucleus ( q ) ) == mathchar ) {
    p = link ( q ) ; 
    if ( p != 0 ) 
    if ( ( ztype ( p ) >= ordnoad ) && ( ztype ( p ) <= punctnoad ) ) 
    if ( mathtype ( nucleus ( p ) ) == mathchar ) 
    if ( fam ( nucleus ( p ) ) == fam ( nucleus ( q ) ) ) {
      mathtype ( nucleus ( q ) ) = mathtextchar ; 
      fetch ( nucleus ( q ), cursize );
      if ( chartag ( curi ) == ligtag ) {
#ifdef FONTPTR
	register SMALLmemoryword *ligp;

	curc = character ( nucleus ( p ) ) ; 
	ligp = zligkernstart ( curf ,  curi ) ;
	curi = ligp->qqqq ;
	if ( skipbyte ( curi ) > stopflag ) {
	  ligp = zligkernrestart ( curf ,  curi ) ; 
	  curi = ligp->qqqq ;
	} 
#else
	register integer a ;

	curc = character ( nucleus ( p ) ) ; 
	a = zligkernstart ( curf ,  curi ) ; 
	curi = fontinfo [ a ] .qqqq ; 
	if ( skipbyte ( curi ) > stopflag ) {
	  a = zligkernrestart ( curf ,  curi ) ; 
	  curi = fontinfo [ a ] .qqqq ; 
	} 
#endif
	while ( true ) {
	  if ( nextchar ( curi ) == curc ) 
	  if ( skipbyte ( curi ) <= stopflag ) 
	  if ( opbyte ( curi ) >= kernflag ) {
	    p = newkern ( zcharkern ( curf ,  curi ) ) ; 
	    link ( p ) = link ( q ) ; 
	    link ( q ) = p ; 
	    return ; 
	  } else {
	    {
	      if ( interrupt != 0 ) 
	      pauseforinstructions () ; 
	    } 
	    switch ( opbyte ( curi ) ) 
	    {case 1 : 
	    case 5 : 
	      character ( nucleus ( q ) ) = rembyte ( curi ) ; 
	      break ; 
	    case 2 : 
	    case 6 : 
	      character ( nucleus ( p ) ) = rembyte ( curi ) ; 
	      break ; 
	    case 3 : 
	    case 7 : 
	    case 11 : 
	      { register long_halfword r;

		r = newnoad () ; 
		character ( nucleus ( r ) ) = rembyte ( curi ) ; 
		fam ( nucleus ( r ) ) = fam ( nucleus ( q ) ) ; 
		link ( q ) = r ; 
		link ( r ) = p ; 
		if ( opbyte ( curi ) < 11 ) 
		  mathtype ( nucleus ( r ) ) = mathchar ; 
		else
		  mathtype ( nucleus ( r ) ) = mathtextchar ; 
	      }
	      break ; 
	    default: 
	      {
		link ( q ) = link ( p ) ; 
		character ( nucleus ( q ) ) = rembyte ( curi ) ; 
		mem [ subscr ( q ) ] = mem [ subscr ( p ) ] ; 
		mem [ supscr ( q ) ] = mem [ supscr ( p ) ] ; 
		freenode ( p , noadsize ) ; 
	      } 
	      break ; 
	    } 
	    if ( opbyte ( curi ) > 3 ) 
	      return ; 
	    mathtype ( nucleus ( q ) ) = mathchar ; 
	    goto lab20 ; 
	  }
	  if ( skipbyte ( curi ) >= stopflag ) 
	    return ; 
#ifdef FONTPTR
	  ligp = ligp + skipbyte ( curi ) + 1 ; 
	  curi = ligp->qqqq ; 
#else
	  a = a + skipbyte ( curi ) + 1 ; 
	  curi = fontinfo [ a ] .qqqq ; 
#endif
	}
      }
    }
  }
}


   static void
makescripts (halfword q , scaled delta, smallnumber curstyle,
		smallnumber cursize)
{ makescripts_regmem
  register halfword p, x, y;
  register scaled shiftup, shiftdown, clr;

  p = newhlist ( q ) ; 
  if ( ischarnode ( p ) ) {
    shiftup = 0 ; 
    shiftdown = 0 ; 
  } else {
    register halfword z;
    register smallnumber t;

    z = hpack ( p , 0 , 1 ) ; 
    if ( curstyle < scriptstyle ) 
      t = scriptsize;
    else
      t = scriptscriptsize;
    shiftup = height ( z ) - supdrop ( t ) ; 
    shiftdown = depth ( z ) + subdrop ( t ) ; 
    freenode ( z , boxnodesize ) ; 
  }

  if ( mathtype ( supscr ( q ) ) == 0 ) {
    x = cleanbox ( subscr ( q ) , substyle ( curstyle ) ) ; 
    width ( x ) = width ( x ) + scriptspace ; 
    if ( shiftdown < sub1 ( cursize ) ) 
      shiftdown = sub1 ( cursize ) ; 
    clr = height ( x ) - ( abs ( mathxheight ( cursize ) * 4 ) / 5 ) ; 
    if ( shiftdown < clr ) 
      shiftdown = clr ; 
    shiftamount ( x ) = shiftdown ; 
  } else {
    {
      x = cleanbox ( supscr ( q ) , supstyle ( curstyle ) ) ; 
      width ( x ) = width ( x ) + scriptspace ; 
      if ( odd ( curstyle ) ) 
        clr = sup3 ( cursize ) ; 
      else if ( curstyle < textstyle ) 
        clr = sup1 ( cursize ) ; 
      else clr = sup2 ( cursize ) ; 
      if ( shiftup < clr ) 
        shiftup = clr ; 
      clr = depth ( x ) + ( abs ( mathxheight ( cursize ) ) / 4 ) ; 
      if ( shiftup < clr ) 
        shiftup = clr ; 
    }
    if ( mathtype ( subscr ( q ) ) == 0 ) 
      shiftamount ( x ) = - (integer) shiftup ; 
    else {
      y = cleanbox ( subscr ( q ) , substyle ( curstyle ) ) ; 
      width ( y ) = width ( y ) + scriptspace ; 
      if ( shiftdown < sub2 ( cursize ) ) 
      shiftdown = sub2 ( cursize ) ; 
      clr = 4 * defaultrulethickness - ( ( shiftup - depth ( x ) ) - ( height 
      ( y ) - shiftdown ) ) ; 
      if ( clr > 0 ) {
	shiftdown = shiftdown + clr ; 
	clr = ( abs ( mathxheight ( cursize ) * 4 ) / 5 ) - ( shiftup - depth 
	( x ) ) ; 
	if ( clr > 0 ) {
	  shiftup = shiftup + clr ; 
	  shiftdown = shiftdown - clr ; 
	} 
      }
      shiftamount ( x ) = delta ; 
      p = newkern ( ( shiftup - depth ( x ) ) - ( height ( y ) - shiftdown ) );
      link ( x ) = p ; 
      link ( p ) = y ; 
      x = vpackage ( x , 0 , 1 , maxdimen ) ; 
      shiftamount ( x ) = shiftdown ; 
    } 
  }

  if ( newhlist ( q ) == 0 )
    newhlist ( q ) = x ; 
  else {
    p = newhlist ( q ) ; 
    while ( link ( p ) != 0 )
      p = link ( p );
    link ( p ) = x;
  }
}


   static smallnumber
makeleftright (halfword q, smallnumber style, scaled maxd, scaled maxh)
{ makeleftright_regmem 
  register scaled delta, delta1, delta2 ;
  register smallnumber cursize;

  if ( style < scriptstyle ) 
    cursize = textsize ; 
  else
    cursize = 16 * ( ( style - textstyle ) / 2 ) ; 

  delta2 = maxd + axisheight ( cursize ) ; 
  delta1 = maxh + maxd - delta2 ; 
  if ( delta2 > delta1 ) 
    delta1 = delta2 ; 
  delta = ( delta1 / 500 ) * delimiterfactor ; 
  delta2 = delta1 + delta1 - delimitershortfall ; 
  if ( delta < delta2 ) 
    delta = delta2 ; 
  newhlist ( q ) = vardelimiter ( delimiter ( q ) , cursize , delta ) ; 

  return( ztype ( q ) - ( leftnoad - opennoad ) );
}


   static long_halfword
mlisttohlist (long_halfword mlist, smallnumber curstyle, int penalties)
{ mlisttohlist_regmem 
  smallnumber style  ; 
  register halfword q  ; 
  register halfword r  ; 
  register smallnumber rtype  ; 
  register smallnumber t  ; 
  register halfword p, x, y, z  ; 
  register integer pen  ; 
  register smallnumber s  ; 
  register scaled maxh, maxd  ; 
  register scaled delta  ;

  smallnumber cursize;
  scaled curmu;

  style = curstyle ; 

  q = mlist ; 
  r = 0 ; 
  rtype = opnoad ; 
  maxh = 0 ; 
  maxd = 0 ; 
  {
    if ( curstyle < scriptstyle ) 
      cursize = textsize ; 
    else
      cursize = 16 * ( ( curstyle - textstyle ) / 2 ) ; 
    curmu = xovern ( mathquad ( cursize ) , 18 ) ; 
  }

  while ( q != 0 ) {

lab21:
    delta = 0 ; 
    switch ( ztype ( q ) ) {
    case binnoad : 
      switch ( rtype ) {
      case binnoad : 
      case opnoad : 
      case relnoad : 
      case opennoad : 
      case punctnoad : 
      case leftnoad : 
	{
	  ztype ( q ) = ordnoad ; 
	  goto lab21 ; 
	} 
	break ; 
      default: 
	break ; 
      }
      break ; 
    case relnoad : 
    case closenoad : 
    case punctnoad : 
    case rightnoad : 
      {
	if ( rtype == binnoad )
	  ztype ( r ) = ordnoad ; 
	if ( ztype ( q ) == rightnoad ) 
	  goto lab80 ; 
      } 
      break ; 
    case leftnoad : 
      goto lab80 ; 
      break ; 
    case fractionnoad : 
      {
	makefraction ( q, curstyle, cursize );
	goto lab82 ; 
      } 
      break ; 
    case opnoad : 
      {
	delta = makeop ( q, curstyle, cursize );
	if ( subtype ( q ) == limits ) 
	  goto lab82 ; 
      } 
      break ; 
    case ordnoad : 
      makeord ( q, cursize );
      break ; 
    case opennoad : 
    case innernoad : 
      break ; 
    case radicalnoad : 
      makeradical ( q, curstyle, cursize );
      break ; 
    case overnoad : 
      makeover ( q, curstyle, cursize ) ; 
      break ; 
    case undernoad : 
      makeunder ( q, curstyle, cursize ) ;
      break ; 
    case accentnoad : 
      makemathaccent ( q, curstyle, cursize );
      break ; 
    case vcenternoad : 
      makevcenter ( q, cursize ) ; 
      break ; 
    case stylenode : 
      {
	curstyle = subtype ( q ) ; 
	{
	  if ( curstyle < scriptstyle ) 
	  cursize = textsize ; 
	  else cursize = 16 * ( ( curstyle - textstyle ) / 2 ) ; 
	  curmu = xovern ( mathquad ( cursize ) , 18 ) ; 
	} 
	goto lab81 ; 
      } 
      break ; 
    case choicenode : 
      {
	switch ( curstyle / 2 ) 
	{case 0 : 
	  {
	    p = displaymlist ( q ) ; 
	    displaymlist ( q ) = 0 ; 
	  } 
	  break ; 
	case 1 : 
	  {
	    p = textmlist ( q ) ; 
	    textmlist ( q ) = 0 ; 
	  } 
	  break ; 
	case 2 : 
	  {
	    p = scriptmlist ( q ) ; 
	    scriptmlist ( q ) = 0 ; 
	  } 
	  break ; 
	case 3 : 
	  {
	    p = scriptscriptmlist ( q ) ; 
	    scriptscriptmlist ( q ) = 0 ; 
	  } 
	  break ; 
	} 
	flushnodelist ( displaymlist ( q ) ) ; 
	flushnodelist ( textmlist ( q ) ) ; 
	flushnodelist ( scriptmlist ( q ) ) ; 
	flushnodelist ( scriptscriptmlist ( q ) ) ; 
	ztype ( q ) = stylenode ; 
	subtype ( q ) = curstyle ; 
	width ( q ) = 0 ; 
	depth ( q ) = 0 ; 
	if ( p != 0 ) {
	  z = link ( q ) ; 
	  link ( q ) = p ; 
	  while ( link ( p ) != 0 ) p = link ( p ) ; 
	  link ( p ) = z ; 
	} 
	goto lab81 ; 
      } 
      break ; 
    case insnode : 
    case marknode : 
    case adjustnode : 
    case whatsitnode : 
    case penaltynode : 
    case discnode : 
      goto lab81 ; 
      break ; 
    case rulenode : 
      {
	if ( height ( q ) > maxh ) 
	maxh = height ( q ) ; 
	if ( depth ( q ) > maxd ) 
	maxd = depth ( q ) ; 
	goto lab81 ; 
      } 
      break ; 
    case gluenode : 
      {
	if ( subtype ( q ) == muglue ) {
	  x = glueptr ( q ) ; 
	  y = mathglue ( x , curmu ) ; 
	  deleteglueref ( x ) ; 
	  glueptr ( q ) = y ; 
	  subtype ( q ) = normal ; 
	} else if ( cursize != textsize && ( subtype(q) == condmathglue ) ) {
	  p = link ( q ) ; 
	  if ( p != 0 ) 
	  if ( ( ztype ( p ) == gluenode ) || ( ztype ( p ) == kernnode ) ) {
	    link ( q ) = link ( p ) ; 
	    link ( p ) = 0 ; 
	    flushnodelist ( p ) ; 
	  } 
	} 
	goto lab81 ; 
      } 
      break ; 
    case kernnode : 
      {
	mathkern ( q , curmu ) ; 
	goto lab81 ; 
      } 
      break ; 
    default: 
      confusion("mlist1");
      break ; 
    }

    switch ( mathtype ( nucleus ( q ) ) ) {
    case mathchar : 
    case mathtextchar : 
      {
	fetch ( nucleus ( q ), cursize ) ; 
	if ( charexists ( curi ) ) {
	  delta = zcharitalic ( curf ,  curi ) ; 
	  p = newcharacter ( curf , curc ) ; 
	  if ( mathtype(nucleus(q)) == mathtextchar && space(curf) != 0 )
	    delta = 0 ; 
	  if ( ( mathtype ( subscr ( q ) ) == 0 ) && ( delta != 0 ) ) {
	    link ( p ) = newkern ( delta ) ; 
	    delta = 0 ; 
	  } 
	} 
	else p = 0 ; 
      } 
      break ; 
    case 0 : 
      p = 0 ; 
      break ; 
    case subbox : 
      p = info ( nucleus ( q ) ) ; 
      break ; 
    case submlist : 
      {
#if 0
	mlisttohlist ( info(nucleus(q)), curstyle, false);

	p = hpack ( link ( temphead ) , 0 , 1 ) ; 
#else
	p = hpack( mlisttohlist(info(nucleus(q)), curstyle, false), 0, 1 );
#endif
      } 
      break ; 
    default: 
      confusion("mlist2");
      break ; 
    } 

    newhlist ( q ) = p ; 
    if ( ( mathtype ( subscr(q) ) == 0 ) && ( mathtype ( supscr(q) ) == 0 ) )
      goto lab82 ; 
    makescripts ( q , delta, curstyle, cursize );

lab82:
    z = hpack ( newhlist ( q ) , 0 , 1 ) ; 
    if ( height ( z ) > maxh ) 
      maxh = height ( z ) ; 
    if ( depth ( z ) > maxd ) 
      maxd = depth ( z ) ; 
    freenode ( z , boxnodesize ) ; 
lab80:
    r = q ; 
    rtype = ztype ( r );
lab81:
    q = link ( q );
  }

  if ( rtype == binnoad ) 
    ztype ( r ) = ordnoad ; 

  p = temphead;
  link(p) = 0;
  q = mlist;

  rtype = 0;
  curstyle = style;
  {
    if ( curstyle < scriptstyle ) 
      cursize = textsize ; 
    else
      cursize = 16 * ( ( curstyle - textstyle ) / 2 ) ; 
    curmu = xovern ( mathquad ( cursize ) , 18 ) ; 
  }

  while ( q != 0 ) {
    t = ordnoad ; 
    s = noadsize ; 
    pen = infpenalty ; 
    switch ( ztype ( q ) ) {
    case opnoad : 
    case opennoad : 
    case closenoad : 
    case punctnoad : 
    case innernoad : 
      t = ztype ( q ) ; 
      break ; 
    case binnoad : 
      {
	t = binnoad ; 
	pen = binoppenalty ; 
      } 
      break ; 
    case relnoad : 
      {
	t = relnoad ; 
	pen = relpenalty ; 
      } 
      break ; 
    case ordnoad : 
    case vcenternoad : 
    case overnoad : 
    case undernoad : 
      break ; 
    case radicalnoad : 
      s = radicalnoadsize ; 
      break ; 
    case accentnoad : 
      s = accentnoadsize ; 
      break ; 
    case fractionnoad : 
      {
	t = innernoad ; 
	s = fractionnoadsize ; 
      } 
      break ; 
    case leftnoad : 
    case rightnoad : 
      t = makeleftright ( q , style , maxd , maxh ) ; 
      break ; 
    case stylenode : 
      {
	curstyle = subtype ( q ) ; 
	s = stylenodesize ; 
	{
	  if ( curstyle < scriptstyle ) 
	  cursize = textsize ; 
	  else cursize = 16 * ( ( curstyle - textstyle ) / 2 ) ; 
	  curmu = xovern ( mathquad ( cursize ) , 18 ) ; 
	} 
	goto lab83 ; 
      } 
      break ; 
    case whatsitnode : 
    case penaltynode : 
    case rulenode : 
    case discnode : 
    case adjustnode : 
    case insnode : 
    case marknode : 
    case gluenode : 
    case kernnode : 
      {
	link ( p ) = q ; 
	p = q ; 
	q = link ( q ) ; 
	link ( p ) = 0 ; 
	goto lab30 ; 
      } 
      break ; 
    default: 
      confusion("mlist3");
      break ; 
    } 

    if ( rtype > 0 ) {
	static char math_spacing[] = {
		0, 2, 3, 4, 0, 0, 0, 1,
		2, 2, 9, 4, 0, 0, 0, 1,
		3, 3, 9, 9, 3, 9, 9, 3,
		4, 4, 9, 0, 4, 0, 0, 4,
		0, 0, 9, 0, 0, 0, 0, 0,
		0, 2, 3, 4, 0, 0, 0, 1,
		1, 1, 9, 1, 1, 1, 1, 1,
		1, 2, 3, 4, 1, 0, 1, 1 };

      switch ( math_spacing[ rtype * 8 + t - (9 * ordnoad) ] ) {
      case 0 :
	x = 0 ; 
	break ; 
      case 1 :
	if ( curstyle < scriptstyle ) 
	  x = thinmuskipcode ; 
	else
	  x = 0 ; 
	break ; 
      case 2 : 
	x = thinmuskipcode ; 
	break ; 
      case 3 : 
	if ( curstyle < scriptstyle ) 
	  x = medmuskipcode ; 
	else x = 0 ; 
	break ; 
      case 4 : 
	if ( curstyle < scriptstyle ) 
	  x = thickmuskipcode ; 
	else x = 0 ; 
	break ;
      default:
	confusion("mlist4");
	break ; 
      }

      if ( x != 0 ) {
	y = mathglue ( gluepar ( x ) , curmu ) ; 
	z = newglue ( y ) ; 
	gluerefcount ( y ) = 0 ; 
	link ( p ) = z ; 
	p = z ; 
	subtype ( z ) = x + 1 ; 
      }
    }

    if ( newhlist ( q ) != 0 ) {
      link ( p ) = newhlist ( q ) ; 
      do {
	  p = link ( p ) ; 
      } while ( ! ( link ( p ) == 0 ) ) ; 
    }
    if ( penalties ) 
    if ( link ( q ) != 0 ) 
    if ( pen < infpenalty ) {
      rtype = ztype ( link ( q ) ) ; 
      if ( rtype != penaltynode ) 
      if ( rtype != relnoad ) {
	z = newpenalty ( pen ) ; 
	link ( p ) = z ; 
	p = z ; 
      }
    }
    rtype = t ; 
lab83: r = q ; 
    q = link ( q ) ; 
    freenode ( r , s ) ; 
lab30: ;
  }
  return( link(temphead) );
}


  static int
check_font_params(void)
{ aftermath_regmem

  if ( ( fontparams(famfnt ( 2 + textsize )) < totalmathsyparams )
    || ( fontparams(famfnt ( 2 + scriptsize )) < totalmathsyparams )
    || ( fontparams(famfnt ( 2 + scriptscriptsize )) < totalmathsyparams ) )
  {
    print_err("Math formula deleted: Insufficient symbol fonts");
    zhelp1( STR_H_SORRY_TEXTFONT2 );
    error();
    flushmath(); 
    return(true);
  } else if ( ( fontparams(famfnt( 3 + textsize )) < totalmathexparams )
	|| ( fontparams(famfnt( 3 + scriptsize )) < totalmathexparams )
	|| ( fontparams(famfnt( 3 + scriptscriptsize )) < totalmathexparams ) )
  {
    print_err("Math formula deleted: Insufficient extension fonts");
    zhelp1( STR_H_SORRY_TEXTFONT3 );
    error();
    flushmath();
    return(true);
  } 

  return(false);
}


void check_that_dollar_follows ( void )		/* added (br) */
{ register eightbits r_curcmd;

  r_curcmd = getxtoken();
  if ( r_curcmd != 3 ) {
    print_err("Display math should end with $$");
    zhelp1( STR_H_THEDOLLARTHATI );
    backerror();
  }
}


void aftermath ( void )
{ aftermath_regmem 
  boolean l  ; 
  boolean danger  ; 
  register integer m  ; 
  register halfword p  ; 
  register halfword a  ; 
  register halfword b  ; 
  register scaled w  ; 
  register scaled z  ; 
  register scaled e  ; 
  register scaled q  ; 
  register scaled d  ; 
  register scaled s  ; 
  register smallnumber g1, g2  ; 
  register halfword r  ; 
  register halfword t  ; 


  danger = check_font_params();

  m = curlist .modefield ; 
  l = false ; 
  p = finmlist ( 0 ) ; 

  if ( curlist .modefield == - (integer) m ) {
    check_that_dollar_follows();

#if 0
    mlisttohlist ( p, textstyle, false );

    a = hpack ( link ( temphead ) , 0 , 1 ) ; 
#else
    a = hpack( mlisttohlist(p, textstyle, false), 0, 1 );
#endif
    unsave () ; 
    decr ( saveptr ) ; 
    if ( saved ( 0 ) == 1 ) 
      l = true ;

    danger = check_font_params();
 
    m = curlist .modefield ; 
    p = finmlist ( 0 ) ; 
  } else
    a = 0 ; 

  if ( m < 0 ) {
    /* Finish math in text mode 1196. */
    tailappend ( newmath ( mathsurround , before ) ) ;

#if 0
    mlisttohlist(p, textstyle, (curlist.modefield > 0));
    link ( curlist .tailfield ) = link ( temphead ) ; 
#else
    link(curlist.tailfield) =
		mlisttohlist(p, textstyle, (curlist.modefield > 0));
#endif

    while ( link ( curlist .tailfield ) != 0 )
	curlist .tailfield = link ( curlist .tailfield );
    tailappend( newmath( mathsurround, after ) );
    curlist .auxfield .hh .v.LH = 1000 ; 
    unsave ();
  } else {
    if ( a == 0 ) {
      check_that_dollar_follows();
    }

    p = mlisttohlist(p, displaystyle, false);
    /* p = link ( temphead ); */

    adjusttail = adjusthead;
    b = hpack( p, 0, 1 );
    p = listptr( b );
    t = adjusttail;
    adjusttail = 0;
    w = width ( b );
    z = displaywidth;
    s = displayindent;
    if ( ( a == 0 ) || danger ) {
      e = 0 ; 
      q = 0 ; 
    } else {
      e = width ( a ) ; 
      q = e + mathquad ( textsize ) ; 
    } 
    if ( w + q > z ) {
      if ( ( e != 0 ) && ( ( w - totalshrink [ normal ] + q <= z )
			|| ( totalshrink [ fil ] != 0 )
			|| ( totalshrink [ fill ] != 0 )
			|| ( totalshrink [ filll ] != 0 ) ) )
      {
	freenode ( b , boxnodesize ) ; 
	b = hpack ( p , z - q , 0 ) ; 
      } else {
	e = 0 ; 
	if ( w > z ) {
	  freenode ( b , boxnodesize ) ; 
	  b = hpack ( p , z , 0 ) ; 
	}
      }
      w = width ( b ) ; 
    }
    d = half ( z - w ) ; 
    if ( ( e > 0 ) && ( d < 2 * e ) ) {
      d = half ( z - w - e ) ; 
      if( ( p != 0 ) && ( ! ischarnode(p) ) && ( ztype(p) == gluenode ) )
	d = 0;
    }
    tailappend ( newpenalty ( predisplaypenalty ) ) ; 
    if ( ( d + s <= predisplaysize ) || l ) {
      g1 = abovedisplayskipcode ; 
      g2 = belowdisplayskipcode ; 
    } else {
      g1 = abovedisplayshortskipcode ; 
      g2 = belowdisplayshortskipcode ; 
    }
    if ( l && ( e == 0 ) ) {
      shiftamount ( a ) = s;
      appendtovlist ( a );
      tailappend ( newpenalty ( infpenalty ) );
    } else
      tailappend ( newparamglue ( g1 ) );
    if ( e != 0 ) {
      r = newkern ( z - w - e - d ) ; 
      if ( l ) {
	link ( a ) = r ; 
	link ( r ) = b ; 
	b = a ; 
	d = 0 ; 
      } else {	  
	link ( b ) = r ; 
	link ( r ) = a ; 
      } 
      b = hpack ( b , 0 , 1 ) ; 
    }
    shiftamount ( b ) = s + d ; 
    appendtovlist ( b ) ; 
    if ( ( a != 0 ) && ( e == 0 ) && ! l ) {
      tailappend ( newpenalty ( infpenalty ) ) ; 
      shiftamount ( a ) = s + z - width ( a ) ; 
      appendtovlist ( a ) ; 
      g2 = 0 ; 
    } 
    if ( t != adjusthead ) {
      link ( curlist .tailfield ) = link ( adjusthead ) ; 
      curlist .tailfield = t ; 
    } 
    tailappend ( newpenalty ( postdisplaypenalty ) ) ; 
    if ( g2 > 0 ) 
      tailappend ( newparamglue ( g2 ) );
    resumeafterdisplay ();
  }
}

/* -- end -- */
