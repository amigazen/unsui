#define EXTERN extern
#include "texd.h"


void postlinebreak ( integer finalwidowpenalty )
{ postlinebreak_regmem 
  register halfword q, r;
  boolean discbreak;
  boolean postdiscbreak;
  register halfword curline;
#ifdef TEXXET
  halfword LR_ptr = LR_save;
#endif

  /* 878. Reverse the link ... */
  q = breaknode ( bestbet ) ; 
  curp = 0 ;
  do {
    r = q ; 
    q = prevbreak ( q ) ; 
    prevbreak ( r ) = curp ; 
    curp = r ; 
  } while ( q != 0 );


  curline = curlist .pgfield + 1 ; 

  do {
    /* Justify the line ending... 880. */

#ifdef TEXXET
    /* 1381. Insert LR nodes at the beginning... */
    q = link(temphead);
    if( LR_ptr != 0 ) {
      tempptr = LR_ptr;  r = q;
      do {
        register long_halfword s;

	s = newmath(0, begin_LR_type(tempptr));
	link(s) = r;  r = s;
	tempptr = link(tempptr);
      } while( tempptr != 0 );
    }
    while( q != curbreak(curp) ) {
      if( ! ischarnode(q) ) {
	/* 1382. Adjust the LR stack.... */
	if( ztype(q) == mathnode ) {
	  if( end_LR(q) ) {
	    if( LR_ptr != 0 )
	      if( info(LR_ptr) == subtype(q) )
		pop_LR;
	  } else {
	    push_LR(q);
	  }
	}
      }
      q = link(q);
    }
#endif

    /* Modify the end of the line ... 881. */
    q = curbreak ( curp ) ; 
    discbreak = false ; 
    postdiscbreak = false ; 

    if ( q != 0 ) 
    if ( ztype ( q ) == gluenode ) {
      deleteglueref ( glueptr ( q ) ) ; 
      glueptr ( q ) = rightskip ; 
      subtype ( q ) = rightskipcode + 1 ; 
      addglueref ( rightskip ) ; 
      goto lab30 ; 
    } else {
      if ( ztype ( q ) == discnode ) {
	register quarterword t;

	t = replacecount( q );
	if ( t == 0 )
	  r = link ( q ) ; 
	else {
	  r = q ; 
	  while ( t > 1 ) {
	    r = link ( r ) ; 
	    decr ( t ) ; 
	  } 
          { register halfword s;

	  s = link ( r ) ; 
#if 0					/* TeX 3.1->3.14 Update */
	  if ( ! ischarnode ( s ) ) 
	  if ( prevbreak ( curp ) != 0 ) 
	  if ( curbreak ( prevbreak ( curp ) ) == s ) 
	    s = r ;
#endif
	  r = link ( s ) ; 
	  link ( s ) = 0 ; 
          }
	  flushnodelist ( link ( q ) ) ; 
	  replacecount ( q ) = 0 ; 
	}
	if ( postbreak ( q ) != 0 ) {
          register halfword s;

	  s = postbreak ( q ) ; 
	  while ( link ( s ) != 0 )
	    s = link ( s ) ; 
	  link ( s ) = r ; 
	  r = postbreak ( q ) ; 
	  postbreak ( q ) = 0 ; 
	  postdiscbreak = true ; 
	} 
	if ( prebreak ( q ) != 0 ) {
          register halfword s;

	  s = prebreak ( q ) ; 
	  link ( q ) = s ; 
	  while ( link ( s ) != 0 )
	    s = link ( s ) ; 
	  prebreak ( q ) = 0 ; 
	  q = s ; 
	} 
	link ( q ) = r ; 
	discbreak = true ; 
      } else if ( ( ztype(q) == mathnode ) || ( ztype(q) == kernnode ) ) {
	width ( q ) = 0 ;
#ifdef TEXXET
	if( ztype(q) == mathnode ) {
	  if( end_LR(q) ) {
	    if( LR_ptr != 0 )
	      if( info(LR_ptr) == subtype(q) )
		pop_LR;
	  } else {
	    push_LR(q);
	  }
	}
#endif
      }
    }
    else {
      q = temphead ; 
      while ( link ( q ) != 0 )
	q = link ( q ) ; 
    } 
    r = newparamglue ( rightskipcode ) ; 
    link ( r ) = link ( q ) ; 
    link ( q ) = r ; 
    q = r ; 
lab30:		/* done: */

#ifdef TEXXET
    /* 1383. Insert LR nodes ... */
    if( LR_ptr != 0 ) {
      register halfword s;

      s = temphead;  r = link(s);
      while( r != q ) {
	s = r;  r = link(s);
      }
      r = LR_ptr;
      while( r != 0 ) {
	tempptr = newmath(0, info(r));
	link(s) = tempptr;
	s = tempptr;
	r = link(r);
      }
      link(s) = q;
    }
#endif

    /* Put the \leftskip glue ... 887. */
    r = link ( q ) ; 
    link ( q ) = 0 ; 
    q = link ( temphead ) ; 
    link ( temphead ) = r ; 
    if ( leftskip != zeroglue ) {
      r = newparamglue ( leftskipcode ) ; 
      link ( r ) = q ; 
      q = r ; 
    }

    /* Call the packaging subroutine... 889. */
    { register scaled curindent, curwidth;

      if ( curline > lastspecialline ) {
	curwidth = secondwidth ; 
	curindent = secondindent ; 
      } else if ( parshapeptr == 0 ) {
	curwidth = firstwidth ; 
	curindent = firstindent ; 
      } else {
	curwidth = mem [ parshapeptr + 2 * curline ] .cint ; 
	curindent = mem [ parshapeptr + 2 * curline - 1 ] .cint ; 
      }
      adjusttail = adjusthead ; 
      justbox = hpack ( q , curwidth , 0 ) ; 
      shiftamount ( justbox ) = curindent ;
    }

    /* Append the new box to the current vertical list ... 888. */
    appendtovlist ( justbox ) ; 
    if ( adjusthead != adjusttail ) {
      link ( curlist .tailfield ) = link ( adjusthead ) ; 
      curlist .tailfield = adjusttail ; 
    } 
    adjusttail = 0 ;

    /* Append a penalty node.. 890. */
    if ( curline + 1 != bestline ) {
      register integer pen;

      pen = interlinepenalty ; 
      if ( curline == curlist .pgfield + 1 ) 
	pen = pen + clubpenalty ; 
      if ( curline + 2 == bestline ) 
	pen = pen + finalwidowpenalty ; 
      if ( discbreak ) 
	pen = pen + brokenpenalty ; 
      if ( pen != 0 ) {
	r = newpenalty ( pen ) ; 
	link ( curlist .tailfield ) = r ; 
	curlist .tailfield = r ; 
      } 
    } 


    incr ( curline ) ; 
    curp = prevbreak ( curp ) ; 
    if ( curp != 0 ) 
    if ( ! postdiscbreak ) {
      /* 879. Prune unwanted ... */
      r = temphead ; 
      while ( true ) {
	q = link ( r ) ; 
	if ( q == curbreak ( curp ) ) 
	  goto lab31 ; 
	if ( ischarnode ( q ) ) 
	  goto lab31 ; 
	if ( nondiscardable ( q ) ) 
	  goto lab31 ; 
#if 0  /* 3.1415 */
	if ( subtype ( q ) == acckern ) 
	if ( ztype ( q ) == kernnode ) 
	  goto lab31 ; 
#else
	if ( ztype ( q ) == kernnode )
	  if ( subtype ( q ) != explicit )
	    goto lab31 ; 
#endif
	r = q;
#ifdef TEXXET
	if( ztype(q) == mathnode ) {
	  if( end_LR(q) ) {
	    if( LR_ptr != 0 )
	      if( info(LR_ptr) == subtype(q) )
		pop_LR;
	  } else {
	    push_LR(q);
	  }
	}
#endif
      } 
lab31:
      if ( r != temphead ) {
	link ( r ) = 0 ; 
	flushnodelist ( link ( temphead ) ) ; 
	link ( temphead ) = q ; 
      }
    }
  } while ( curp != 0 );

  if ( ( curline != bestline ) || ( link ( temphead ) != 0 ) ) 
    confusion("line breaking");
  curlist .pgfield = bestline - 1 ;

#ifdef TEXXET
  LR_save = LR_ptr;
#endif
}


  smallnumber
reconstitute ( smallnumber j, smallnumber n, halfword bchar, halfword hchar )
{ reconstitute_regmem 
  register halfword p  ; 
  register halfword t  ; 
  fourquarters q  ; 
  register halfword currh  ; 
  register halfword testchar  ; 
  register scaled w  ; 
#ifdef FONTPTR
  SMALLmemoryword *ligp;	/* (br) ptr in lig/kern program */
#else
  fontindex k  ; 
#endif
  register halfword ligstack, curl, curr, curq;

  hyphenpassed = 0 ; 
  t = holdhead ; 
  w = 0 ; 
  link ( holdhead ) = 0 ; 
  curl = hu [ j ] ; 
  curq = t ; 
  if ( j == 0 ) {
    ligaturepresent = initlig ; 
    p = initlist ; 
    if ( ligaturepresent ) 
      lfthit = initlft ; 
    while ( p > 0 ) {
      {
	link ( t ) = getavail () ; 
	t = link ( t ) ; 
	font ( t ) = hf ; 
	character ( t ) = character ( p ) ; 
      } 
      p = link ( p ) ; 
    } 
  } else if ( curl < nonchar ) {
    link ( t ) = getavail () ; 
    t = link ( t ) ; 
    font ( t ) = hf ; 
    character ( t ) = curl ; 
  } 
  ligstack = 0 ; 
  {
    if ( j < n )
      curr = hu [ j + 1 ] ; 
    else curr = bchar ; 
    if ( odd ( hyf [ j ] ) ) 
      currh = hchar ; 
    else currh = nonchar ; 
  }

lab22:
  if ( curl == nonchar ) {
#ifdef FONTPTR
    if( bcharlabel(hf) == nonaddress )
	goto lab30;
    ligp = &fontinfo[bcharlabel(hf)];
    q = ligp->qqqq;
#else
    k = bcharlabel(hf); 
    if ( k == nonaddress ) 
      goto lab30 ; 
    else
      q = fontinfo [ k ] .qqqq ; 
#endif
  } else {
    q = zcharinfo ( hf ,  curl ) ; 
    if ( chartag ( q ) != ligtag ) 
      goto lab30 ; 
#ifdef FONTPTR
    ligp = zligkernstart ( hf ,  q ) ; 
    q = ligp->qqqq ; 
    if ( skipbyte ( q ) > stopflag ) {
      ligp = zligkernrestart ( hf ,  q ) ; 
      q = ligp->qqqq ; 
    }
#else
    k = zligkernstart ( hf ,  q ) ; 
    q = fontinfo [ k ] .qqqq ; 
    if ( skipbyte ( q ) > stopflag ) {
      k = zligkernrestart ( hf ,  q ) ; 
      q = fontinfo [ k ] .qqqq ; 
    }
#endif
  }
  if ( currh < nonchar ) 
    testchar = currh ; 
  else
    testchar = curr ;

  while ( true ) {
    if ( nextchar ( q ) == testchar ) 
    if ( skipbyte ( q ) <= stopflag ) 
    if ( currh < nonchar ) {
      hyphenpassed = j ; 
      hchar = nonchar ; 
      currh = nonchar ; 
      goto lab22 ; 
    } else {
      if ( hchar < nonchar ) 
      if ( odd ( hyf [ j ] ) ) {
	hyphenpassed = j ; 
	hchar = nonchar ; 
      } 
      if ( opbyte ( q ) < kernflag ) {
	if ( curl == nonchar ) 
	lfthit = true ; 
	if ( j == n ) 
	if ( ligstack == 0 ) 
	rthit = true ; 
	{
	  if ( interrupt != 0 ) 
	  pauseforinstructions () ; 
	} 
	switch ( opbyte ( q ) ) 
	{case 1 : 
	case 5 : 
	  {
	    curl = rembyte ( q ) ; 
	    ligaturepresent = true ; 
	  } 
	  break ; 
	case 2 : 
	case 6 : 
	  {
	    curr = rembyte ( q ) ; 
	    if ( ligstack > 0 ) 
	      character ( ligstack ) = curr ; 
	    else {
	      ligstack = newligitem ( curr ) ; 
	      if ( j == n ) 
		bchar = nonchar ; 
	      else {
		p = getavail () ; 
		ligptr ( ligstack ) = p ; 
		character ( p ) = hu [ j + 1 ] ; 
		font ( p ) = hf ; 
	      } 
	    } 
	  } 
	  break ; 
	case 3 : 
	  {
	    curr = rembyte ( q ) ; 
	    p = ligstack ; 
	    ligstack = newligitem ( curr ) ; 
	    link ( ligstack ) = p ; 
	  } 
	  break ; 
	case 7 : 
	case 11 : 
	  {
	    if ( ligaturepresent ) {
	      p = newligature ( hf , curl , link ( curq ) ) ; 
	      if ( lfthit ) {
		subtype ( p ) = 2 ; 
		lfthit = false ; 
	      } 
	      if ( false ) 
	      if ( ligstack == 0 ) {
		incr ( subtype ( p ) ) ; 
		rthit = false ; 
	      } 
	      link ( curq ) = p ; 
	      t = p ; 
	      ligaturepresent = false ; 
	    } 
	    curq = t ; 
	    curl = rembyte ( q ) ; 
	    ligaturepresent = true ; 
	  } 
	  break ; 
	default: 
	  {
	    curl = rembyte ( q ) ; 
	    ligaturepresent = true ; 
	    if ( ligstack > 0 ) {
	      if ( ligptr ( ligstack ) > 0 ) {
		link ( t ) = ligptr ( ligstack ) ; 
		t = link ( t ) ; 
		incr ( j ) ; 
	      } 
	      p = ligstack ; 
	      ligstack = link ( p ) ; 
	      freenode ( p , smallnodesize ) ; 
	      if ( ligstack == 0 ) {
		if ( j < n ) 
		curr = hu [ j + 1 ] ; 
		else curr = bchar ; 
		if ( odd ( hyf [ j ] ) ) 
		currh = hchar ; 
		else currh = nonchar ; 
	      } 
	      else curr = character ( ligstack ) ; 
	    } 
	    else if ( j == n ) 
	      goto lab30 ; 
	    else {
	      {
		link ( t ) = getavail () ; 
		t = link ( t ) ; 
		font ( t ) = hf ; 
		character ( t ) = curr ; 
	      } 
	      incr ( j ) ; 
	      {
		if ( j < n ) 
		curr = hu [ j + 1 ] ; 
		else curr = bchar ; 
		if ( odd ( hyf [ j ] ) ) 
		currh = hchar ; 
		else currh = nonchar ; 
	      } 
	    } 
	  } 
	  break ; 
	} 
	if ( opbyte ( q ) > 4 ) 
	if ( opbyte ( q ) != 7 ) 
	goto lab30 ; 
	goto lab22 ; 
      } 
      w = zcharkern ( hf ,  q ) ; 
      goto lab30 ; 
    } 
    if ( skipbyte ( q ) >= stopflag ) 
    if ( currh == nonchar ) 
      goto lab30 ; 
    else {
      currh = nonchar ; 
      goto lab22 ; 
    }
#ifdef FONTPTR
    ligp = ligp + skipbyte ( q ) + 1 ; 
    q = ligp->qqqq ; 
#else
    k = k + skipbyte ( q ) + 1 ; 
    q = fontinfo [ k ] .qqqq ;
#endif
  }

lab30:
  if ( ligaturepresent ) {
    p = newligature ( hf , curl , link ( curq ) ) ; 
    if ( lfthit ) {
      subtype ( p ) = 2 ; 
      lfthit = false ; 
    } 
    if ( rthit ) 
    if ( ligstack == 0 ) {
      incr ( subtype ( p ) ) ; 
      rthit = false ; 
    } 
    link ( curq ) = p ; 
    t = p ; 
    ligaturepresent = false ; 
  } 
  if ( w != 0 ) {
    link ( t ) = newkern ( w ) ; 
    t = link ( t ) ; 
    w = 0 ; 
  } 
  if ( ligstack > 0 ) {
    curq = t ; 
    curl = character ( ligstack ) ; 
    ligaturepresent = true ; 
    {
      if ( ligptr ( ligstack ) > 0 ) {
	link ( t ) = ligptr ( ligstack ) ; 
	t = link ( t ) ; 
	incr ( j ) ; 
      } 
      p = ligstack ; 
      ligstack = link ( p ) ; 
      freenode ( p , smallnodesize ) ; 
      if ( ligstack == 0 ) {
	if ( j < n ) 
	curr = hu [ j + 1 ] ; 
	else curr = bchar ; 
	if ( odd ( hyf [ j ] ) ) 
	currh = hchar ; 
	else currh = nonchar ; 
      } 
      else curr = character ( ligstack ) ; 
    } 
    goto lab22 ; 
  }
  return(j);
}


void hyphenate ( void )
{/* 50 30 40 41 42 45 10 */ hyphenate_regmem 
  register schar i, j, l  ; 
  register halfword q, r, s  ; 
  register halfword bchar;
  register halfword majortail, minortail  ; 
  register ASCIIcode c  ; 
  register schar cloc  ; 
  register integer rcount  ; 
  register halfword hyfnode  ; 
  register hyphpointer h  ; 

  for( j = hn ; j >= 0 ; --j ) {
    hyf [ j ] = 0 ; 
  }
  h = hc [ 1 ] ; 
  incr ( hn ) ; 
  hc [ hn ] = curlang ; 
  {register integer for_end; j = 2 ; for_end = hn ; if ( j <= for_end) do 
    h = ( h + h + hc [ j ] ) % hyphsize ; 
  while ( j++ < for_end ) ; } 

  while ( true ) {
    register strnumber k;

    k = hyphword(h) ; 
    if ( k == 0 ) 
      goto lab45 ; 
    if ( length ( k ) < hn ) 
      goto lab45 ; 
    if ( length ( k ) == hn ) {
      register poolpointer u;

      j = 1 ; 
      u = strstart [ k ] ; 
      do {
	if ( strpool [ u ] < hc [ j ] )
	  goto lab45 ; 
	if ( strpool [ u ] > hc [ j ] )
	  goto lab30 ; 
	incr ( j ); incr ( u );
      } while ( j <= hn );
      s = hyphlist(h); 
      while ( s != 0 ) {
	hyf [ info ( s ) ] = 1 ; 
	s = link ( s ) ; 
      } 
      decr ( hn );
      goto lab40;
    } 
lab30:
    if ( h > 0 ) 
      decr ( h ) ; 
    else h = hyphsize ; 
  }
lab45: decr ( hn ) ; 
  if ( trietrc [ curlang + 1 ] != curlang ) 
    return ;

  hc [ 0 ] = 0 ; 
  hc [ hn + 1 ] = 0 ; 
  hc [ hn + 2 ] = 256 ; 

  { register SMALLhalfword r_opstart = opstart[curlang];

  {register integer for_end; j = 0 ; for_end = hn - rhyf + 1 ; if ( j <= 
  for_end) do 
    {
      register triepointer z;

      z = trietrl [ curlang + 1 ] + hc [ j ] ; 
      l = j ; 
      while ( hc [ l ] == trietrc [ z ] ) {
	if ( trietro [ z ] != mintrieop ) {
	  register integer v;

	  v = trietro [ z ] ;
	  do {
	    v = v + r_opstart;		/* opstart [ curlang ] ; */
	    i = l - hyfdistance(v);
	    if ( hyfnum(v) > hyf [ i ] ) 
	      hyf [ i ] = hyfnum(v);
	    v = hyfnext(v);
	  } while ( v != mintrieop );
	}
	incr ( l ) ; 
	z = trietrl [ z ] + hc [ l ] ; 
      }
    } 
  while ( j++ < for_end ) ; }
  }

lab40: {
      register integer for_end; j = 0 ; for_end = lhyf - 1 ; if ( j <= 
  for_end) do 
    hyf [ j ] = 0 ; 
  while ( j++ < for_end ) ; } 
  {register integer for_end; j = 0 ; for_end = rhyf - 1 ; if ( j <= for_end) 
  do 
    hyf [ hn - j ] = 0 ; 
  while ( j++ < for_end ) ; } 
  {register integer for_end; j = lhyf ; for_end = hn - rhyf ; if ( j <= 
  for_end) do 
    if ( odd ( hyf [ j ] ) ) 
    goto lab41 ; 
  while ( j++ < for_end ) ; } 
  return ; 

lab41: ;
  /* 903.  Replace nodes ha..hb by a sequence of nodes ... */
  q = link ( hb );  link ( hb ) = 0;  r = link ( ha );  link ( ha ) = 0;
  bchar = hyf_bchar;  /* bchar = nonchar; */  /* TeX 3.141 */
#if 0  /* TeX 3.141 */
  if ( ! ischarnode ( hb ) ) 
    if ( ztype ( hb ) == ligaturenode ) 
      if ( odd ( subtype ( hb ) ) ) 
	bchar = fontbchar(hf); 
#endif
  if ( ischarnode ( ha ) ) 
    if ( font ( ha ) != hf ) 
      goto lab42 ; 
    else {
      initlist = ha ; 
      initlig = false ; 
      hu [ 0 ] = character ( ha ) ; 
    } 
  else if ( ztype ( ha ) == ligaturenode ) 
  if ( font ( ligchar ( ha ) ) != hf ) 
    goto lab42 ; 
  else {
    initlist = ligptr ( ha ) ; 
    initlig = true ; 
    initlft = ( subtype ( ha ) > 1 ) ; 
    hu [ 0 ] = character ( ligchar ( ha ) ) ; 
    if ( initlist == 0 ) 
    if ( initlft ) {
      hu [ 0 ] = 256 ; 
      initlig = false ; 
    }
    freenode ( ha , smallnodesize ) ; 
  }
  else {
    if ( ! ischarnode ( r ) ) 
    if ( ztype ( r ) == ligaturenode ) 
    if ( subtype ( r ) > 1 ) 
      goto lab42 ; 
    j = 1 ; 
    s = ha ; 
    initlist = 0 ; 
    goto lab50 ; 
  } 
  s = curp ; 
  while ( link ( s ) != ha )
    s = link ( s ) ; 
  j = 0 ; 
  goto lab50 ; 

lab42:
  s = ha ; 
  j = 0 ; 
  hu [ 0 ] = 256 ; 
  initlig = false ; 
  initlist = 0 ; 

lab50:
  flushnodelist ( r ) ; 
  do {
    l = j ; 
    j = reconstitute ( j , hn , bchar , hyfchar ) + 1 ; 
    if ( hyphenpassed == 0 ) {
      link ( s ) = link ( holdhead ) ; 
      while ( link ( s ) > 0 )
	s = link ( s ) ; 
      if ( odd ( hyf [ j - 1 ] ) ) {
	l = j ; 
	hyphenpassed = j - 1 ; 
	link ( holdhead ) = 0 ; 
      } 
    } 
    if ( hyphenpassed > 0 ) 
    do {
      r = getnode ( smallnodesize ) ; 
      link ( r ) = link ( holdhead ) ; 
      ztype ( r ) = discnode ; 
      majortail = r ; 
      rcount = 0 ; 
      while ( link ( majortail ) > 0 ) {
	majortail = link ( majortail ) ; 
	incr ( rcount ) ; 
      } 
      i = hyphenpassed ; 
      hyf [ i ] = 0 ; 
      minortail = 0 ; 
      prebreak ( r ) = 0 ; 
      hyfnode = newcharacter ( hf , hyfchar ) ; 
      if ( hyfnode != 0 ) {
	incr ( i ) ; 
	c = hu [ i ] ; 
	hu [ i ] = hyfchar ; 
	freeavail ( hyfnode ) ; 
      } 
      while ( l <= i ) {
	l = reconstitute ( l , i , fontbchar(hf), nonchar ) + 1 ; 
	if ( link ( holdhead ) > 0 ) {
	  if ( minortail == 0 ) 
	  prebreak ( r ) = link ( holdhead ) ; 
	  else link ( minortail ) = link ( holdhead ) ; 
	  minortail = link ( holdhead ) ; 
	  while ( link ( minortail ) > 0 ) minortail = link ( minortail ) ; 
	} 
      } 
      if ( hyfnode != 0 ) {
	hu [ i ] = c ; 
	l = i ; 
	decr ( i ) ; 
      } 
      minortail = 0 ; 
      postbreak ( r ) = 0 ; 
      cloc = 0 ; 
      if ( bcharlabel(hf) < nonaddress ) {
	decr ( l ) ; 
	c = hu [ l ] ; 
	cloc = l ; 
	hu [ l ] = 256 ; 
      } 
      while ( l < j ) {
	do {
	  l = reconstitute ( l , hn , bchar , nonchar ) + 1 ; 
	  if ( cloc > 0 ) {
	    hu [ cloc ] = c ; 
	    cloc = 0 ; 
	  } 
	  if ( link ( holdhead ) > 0 ) {
	    if ( minortail == 0 ) 
	    postbreak ( r ) = link ( holdhead ) ; 
	    else link ( minortail ) = link ( holdhead ) ; 
	    minortail = link ( holdhead ) ; 
	    while ( link ( minortail ) > 0 ) minortail = link ( minortail ) ; 
	  } 
	} while ( ! ( l >= j ) ) ; 
	while ( l > j ) {	    
	  j = reconstitute ( j , hn , bchar , nonchar ) + 1 ; 
	  link ( majortail ) = link ( holdhead ) ; 
	  while ( link ( majortail ) > 0 ) {
	    majortail = link ( majortail ) ; 
	    incr ( rcount ) ; 
	  } 
	} 
      }
      if ( rcount > 127 ) {
	link ( s ) = link ( r ) ; 
	link ( r ) = 0 ; 
	flushnodelist ( r ) ; 
      } else {
	link ( s ) = r ; 
	replacecount ( r ) = rcount ; 
      } 
      s = majortail ; 
      hyphenpassed = j - 1 ; 
      link ( holdhead ) = 0 ; 
    } while ( ! ( ! odd ( hyf [ j - 1 ] ) ) ) ; 
  } while ( ! ( j > hn ) ) ; 
  link ( s ) = q ; 
  flushlist ( initlist ) ; 
}


void newhyphexceptions ( void )
{/* 21 10 40 45 */ newhyphexceptions_regmem 
  register short n  ; 
  register short j  ; 
  register hyphpointer h  ; 
  register strnumber k  ; 
  register halfword p  ; 
  /* register halfword q; */
  register strnumber s;
  /* register poolpointer u, v; */

  scanleftbrace () ; 
  if ( language <= 0 || language > 255 )
    curlang = 0 ; 
  else
    curlang = language ; 
  n = 0 ; 
  p = 0 ; 

  while ( true ) {
    getxtoken () ; 
lab21:
    switch ( curcmd ) {
    case 16 : 
	curchr = scancharnum ();	/* curchr = curval; */
	curcmd = 68 ; 
	/* goto lab21 ; */
	/* FALL THROUGH */
    case 11 : 
    case 12 : 
    case 68 : 
      if ( curchr == 45 ) {
	if ( n < 63 ) {
	  register long_halfword q;

	  q = getavail ();
	  link ( q ) = p;
	  info ( q ) = n;
	  p = q;
	}
      } else {
	if ( lccode ( curchr ) == 0 ) {
	  print_err("Not a letter");
	  zhelp1( STR_H_LETTERSINHYPH );
	  error () ; 
	} else if ( n < 63 ) {
	  incr ( n ) ; 
	  hc [ n ] = lccode ( curchr ) ; 
	} 
      } 
      break ; 
    case 10 : 
    case 2 : 
      if ( n > 1 ) {
	incr ( n ) ; 
	hc [ n ] = curlang ; 
	strroom ( n ) ; 
	h = 0 ; 
	for( j = 1 ; j <=n ; j++ ) {
	  h = ( h + h + hc [ j ] ) % hyphsize ; 
	  appendchar ( hc [ j ] ) ; 
	}
	s = makestring () ; 

	if ( hyphcount == hyphsize ) 
	  overflow(12, hyphsize);
	incr ( hyphcount ) ; 

	while ( hyphword(h) != 0 ) {
	  k = hyphword(h); 
	  if ( length ( k ) < length ( s ) ) 
	    goto lab40 ; 
	  if ( length ( k ) > length ( s ) ) 
	    goto lab45 ; 
#if 0
	  { register poolpointer u, v;
	    u = strstart [ k ]; v = strstart [ s ];
	    do {
	      if ( strpool [ u ] < strpool [ v ] ) 
	        goto lab40 ; 
	      if ( strpool [ u ] > strpool [ v ] ) 
	        goto lab45 ; 
	      incr ( u ); incr ( v );
	    } while ( u != strstart [ k + 1 ] );
	  }
#else
	  { register ASCIIcode *u, *v, *ep;
	    u = &strpool[strstart [ k ]]; ep = &strpool[strstart[k+1]];
	    v = &strpool[strstart [ s ]];
	    do {
	      if ( *u < *v ) 
	        goto lab40 ; 
	      if ( *u > *v ) 
	        goto lab45 ; 
	      incr(u); incr(v);
	    } while ( u != ep );
	  }
#endif
lab40:
	  { register halfword q;

	    q = hyphlist(h); hyphlist(h) = p; p = q;
	  }
	  { register strnumber t;

	    t = hyphword(h); hyphword(h) = s; s = t;
	  }
lab45:
	  if ( h > 0 )
	    decr ( h );
	  else
	    h = hyphsize ; 
	}
	hyphword(h) = s ; 
	hyphlist(h) = p ; 
      }
      if ( curcmd == 2 ) 
	return;
      n = 0;
      p = 0;
      break ; 
    default: 
      {
	p_print_err( STR_H_IMPROPER );
	printesc( STR_HYPHENATION );
	c_print(" will be flushed");
	zhelp1( STR_H_HYPH_EXC_ONLY );
	error () ; 
      } 
      break ; 
    }
  }
}

 
halfword prunepagetop ( halfword p )
{ prunepagetop_regmem 
  register halfword prevp  ; 
  register halfword q  ; 

  prevp = temphead ; 
  link ( temphead ) = p ; 

 while ( p != 0 )
  switch ( ztype ( p ) ) {
  case hlistnode : 
  case vlistnode : 
  case rulenode : 
    {
      q = newskipparam ( splittopskipcode ) ; 
      link ( prevp ) = q ; 
      link ( q ) = p ; 
      if ( width ( tempptr ) > height ( p ) ) 
	width ( tempptr ) = width ( tempptr ) - height ( p ) ; 
      else
	width ( tempptr ) = 0 ; 
      p = 0 ; 
    } 
    break ; 
  case whatsitnode : 
  case marknode : 
  case insnode : 
    {
      prevp = p ; 
      p = link ( prevp ) ; 
    } 
    break ; 
  case gluenode : 
  case kernnode : 
  case penaltynode : 
    {
      q = p ; 
      p = link ( q ) ; 
      link ( q ) = 0 ; 
      link ( prevp ) = p ; 
      flushnodelist ( q ) ; 
    } 
    break ; 
  default: 
    confusion("pruning");
    break ; 
  }
  return( link ( temphead ) );
} 


halfword vertbreak ( halfword p, scaled h, scaled d )
{/* 30 45 90 */ vertbreak_regmem 
  register halfword prevp  ; 
  register halfword q, r  ; 
  register integer pi  ; 
  register integer b  ; 
  register integer leastcost  ; 
  register halfword bestplace  ; 
  register scaled prevdp  ; 
  register smallnumber t  ; 

  prevp = p ; 
  leastcost = awfulbad ; 
  activewidth [ 0 ] = 0 ; 
  activewidth [ 1 ] = 0 ; 
  activewidth [ 2 ] = 0 ; 
  activewidth [ 3 ] = 0 ; 
  activewidth [ 4 ] = 0 ; 
  activewidth [ 5 ] = 0 ; 
  prevdp = 0 ; 

  while ( true ) {
    if ( p == 0 ) 
      pi = ejectpenalty ; 
    else switch ( ztype ( p ) ) 
    {case hlistnode : 
    case vlistnode : 
    case rulenode : 
      {
	activewidth [ 0 ] += prevdp + height ( p ) ; 
	prevdp = depth ( p ) ; 
	goto lab45 ; 
      } 
      break ; 
    case whatsitnode : 
      goto lab45 ; 
      break ; 
    case gluenode : 
      if ( precedesbreak ( prevp ) ) 
	pi = 0 ;
      else
	goto lab90 ; 
      break ; 
    case kernnode : 
      {
	if ( link ( p ) == 0 ) 
	  t = penaltynode ; 
	else
	  t = ztype ( link ( p ) ) ; 
	if ( t == gluenode ) 
	  pi = 0 ; 
	else
	  goto lab90 ; 
      } 
      break ; 
    case penaltynode : 
      pi = mem [ p + 1 ] .cint ; 
      break ; 
    case marknode : 
    case insnode : 
      goto lab45 ; 
      break ; 
    default: 
      confusion("vertbreak");
      break ; 
    } 
    if ( pi < infpenalty ) {
      if ( activewidth [ 0 ] < h ) 
      if ( ( activewidth [ 2 ] != 0 ) || ( activewidth [ 3 ] != 0 ) || ( 
      activewidth [ 4 ] != 0 ) ) 
      b = 0 ; 
      else b = badness ( h - activewidth [ 0 ] , activewidth [ 1 ] ) ; 
      else if ( activewidth [ 0 ] - h > activewidth [ 5 ] ) 
      b = awfulbad ; 
      else b = badness ( activewidth [ 0 ] - h , activewidth [ 5 ] ) ; 
      if ( b < awfulbad ) 
      if ( pi <= ejectpenalty ) 
      b = pi ; 
      else if ( b < infbad ) 
      b = b + pi ; 
      else b = 100000L ; 
      if ( b <= leastcost ) {
	bestplace = p ; 
	leastcost = b ; 
	bestheightplusdepth = activewidth [ 0 ] + prevdp ;
      } 
      if ( ( b == awfulbad ) || ( pi <= ejectpenalty ) ) 
      goto lab30 ; 
    } 
    if ( ( ztype ( p ) < gluenode ) || ( ztype ( p ) > kernnode ) ) 
    goto lab45 ; 

lab90:
    if ( ztype ( p ) == kernnode ) 
      q = p ; 
    else {
      q = glueptr ( p ) ; 
      activewidth [ 1 + stretchorder ( q ) ] += stretch ( q );
      activewidth [ 5 ] += shrink ( q ) ; 
      if ( ( shrinkorder ( q ) != normal ) && ( shrink ( q ) != 0 ) ) {
	print_err("Infinite glue shrinkage found in box being split");
	zhelp3( STR_H_THEBOX_VSPLIT, STR_H_SHRINKABLEGLUE,
		STR_H_SINCETHE_OFFENSIVE);
	error();
	r = newspec ( q ) ; 
	shrinkorder ( r ) = normal ; 
	deleteglueref ( q ) ; 
	glueptr ( p ) = r ; 
	q = r ; 
      } 
    }
    activewidth [ 0 ] += prevdp + width ( q ) ; 
    prevdp = 0 ;

lab45:
    if ( prevdp > d ) {
      activewidth [ 0 ] += prevdp - d ; 
      prevdp = d;
    }
    prevp = p ; 
    p = link ( prevp ) ; 
  }

lab30:
  return( bestplace );
}


long_halfword vsplit ( eightbits n, scaled h )
{ vsplit_regmem 
  register halfword v  ; 
  register halfword p  ; 
  register halfword q  ; 

  v = box ( n ) ; 
  if ( splitfirstmark != 0 ) {
    deletetokenref ( splitfirstmark ) ; 
    splitfirstmark = 0 ; 
    deletetokenref ( splitbotmark ) ; 
    splitbotmark = 0 ; 
  } 
  if ( v == 0 ) {
    return( 0L ) ; 
  } 
  if ( ztype ( v ) != vlistnode ) {
    print_err("");
    printesc( STR_VSPLIT );
    c_print(" needs a ");
    printesc( STR_VBOX );
    zhelp1( STR_H_THEBOX_SPLITHBOX );
    error();
    return( 0L );
  }

  q = vertbreak ( listptr(v), h, splitmaxdepth );
  p = listptr ( v ) ; 
  if ( p == q ) 
    listptr ( v ) = 0 ; 
  else while ( true ) {
    if ( ztype ( p ) == marknode ) 
    if ( splitfirstmark == 0 ) {
      splitfirstmark = markptr ( p ) ; 
      splitbotmark = splitfirstmark ; 
      tokenrefcount ( splitfirstmark ) = tokenrefcount ( splitfirstmark ) + 2;
    } else {
      deletetokenref ( splitbotmark ) ; 
      splitbotmark = markptr ( p ) ; 
      addtokenref ( splitbotmark ) ; 
    } 
    if ( link ( p ) == q ) {
      link ( p ) = 0 ; 
      goto lab30 ; 
    } 
    p = link ( p ) ; 
  }

lab30:
  q = prunepagetop ( q ) ; 
  p = listptr ( v ) ; 
  freenode ( v , boxnodesize ) ; 
  if ( q == 0 ) 
    box ( n ) = 0 ; 
  else
    box ( n ) = vpackage ( q , 0 , 1 , maxdimen ) ; 

  return( vpackage ( p , h , 0 , splitmaxdepth ) );
}


void printtotals ( void )
{ printtotals_regmem

  printscaled ( pagesofar [ 1 ] ) ; 
  if ( pagesofar [ 2 ] != 0 ) {
    print( STR_PLUS_ );
    printscaled ( pagesofar [ 2 ] ) ; 
    c_print("");
  } 
  if ( pagesofar [ 3 ] != 0 ) {
    print( STR_PLUS_ );
    printscaled ( pagesofar [ 3 ] ) ; 
    print( STR_FIL );
  } 
  if ( pagesofar [ 4 ] != 0 ) {
    print( STR_PLUS_ );
    printscaled ( pagesofar [ 4 ] ) ; 
    c_print("fill");
  } 
  if ( pagesofar [ 5 ] != 0 ) {
    print( STR_PLUS_ );
    printscaled ( pagesofar [ 5 ] ) ; 
    c_print("filll");
  }
  if ( pagesofar [ 6 ] != 0 ) {
    print( STR_MINUS_ );
    printscaled ( pagesofar [ 6 ] ) ; 
  } 
}


void freezepagespecs ( smallnumber s )
{ freezepagespecs_regmem

  pagecontents = s ; 
  pagemaxdepth = maxdepth ; 
  pagesofar [ 0 ] = vsize ; 
  pagesofar [ 1 ] = 0 ; 
  pagesofar [ 2 ] = 0 ; 
  pagesofar [ 3 ] = 0 ; 
  pagesofar [ 4 ] = 0 ; 
  pagesofar [ 5 ] = 0 ; 
  pagesofar [ 6 ] = 0 ; 
  pagesofar [ 7 ] = 0 ; 
  leastpagecost = awfulbad ; 
#ifdef STAT
  if ( tracingpages > 0 ) {
    begindiagnostic();
    c_printnl("%% goal height=");
    printscaled( pagesofar[0] );
    c_print(", max depth=");
    printscaled( pagemaxdepth );
    enddiagnostic( false );
  }
#endif /* STAT */
}


void boxerror ( eightbits n )
{ boxerror_regmem

  error(); 
  begindiagnostic();
  c_printnl("The following box has been deleted:");
  showbox ( box ( n ) );
  enddiagnostic ( true );
  flushnodelist ( box ( n ) );
  box ( n ) = 0;
}


void ensurevbox ( eightbits n )
{ ensurevbox_regmem
  register halfword p  ; 

  p = box ( n ) ; 
  if ( p != 0 ) 
  if ( ztype ( p ) == hlistnode ) {
    print_err("Insertions can only be added to a vbox");
    zhelp2( STR_H_TUTTUT_INSERT, STR_H_PROCEED_DISCRADCONT);
    boxerror ( n ) ; 
  }
}


void fireup ( halfword c )
{/* 10 */ fireup_regmem
  register halfword p, q, r, s  ; 
  register halfword prevp  ; 
  unsigned char n  ; 
  boolean wait  ; 
  /*register*/ integer savevbadness  ; 
  /*register*/ scaled savevfuzz  ; 
  /*register*/ halfword savesplittopskip  ; 

  if ( ztype ( bestpagebreak ) == penaltynode ) {
    geqworddefine ( intbase + outputpenaltycode, mem[bestpagebreak+1].cint );
    mem [ bestpagebreak + 1 ] .cint = infpenalty ; 
  } else
    geqworddefine ( intbase + outputpenaltycode , infpenalty ) ; 

  if ( botmark != 0 ) {
    if ( topmark != 0 ) 
      deletetokenref ( topmark ) ; 
    topmark = botmark ; 
    addtokenref ( topmark ) ; 
    deletetokenref ( firstmark ) ; 
    firstmark = 0 ; 
  } 

  if ( c == bestpagebreak ) 
    bestpagebreak = 0 ; 

  if ( box ( 255 ) != 0 ) {
    print_err("");
    printesc( STR_BOX );
    c_print("255 is not void");
    zhelp2( STR_H_YOUSHOULDNTUSEBOX, STR_H_PROCEED_DISCRADCONT );
    boxerror( 255 );
  } 
  insertpenalties = 0 ; 
  savesplittopskip = splittopskip ; 
  if ( holdinginserts <= 0 ) {
    r = link ( pageinshead ) ; 
    while ( r != pageinshead ) {
      if ( bestinsptr ( r ) != 0 ) {
	n = subtype ( r ) ; 
	ensurevbox ( n ) ; 
	if ( box ( n ) == 0 )
	  box ( n ) = newnullbox () ; 
	p = box ( n ) + listoffset ; 
	while ( link ( p ) != 0 )
	  p = link ( p ) ; 
	lastinsptr ( r ) = p ; 
      } 
      r = link ( r ) ; 
    } 
  } 
  q = holdhead ; 
  link ( q ) = 0 ; 
  prevp = pagehead ; 
  p = link ( prevp ) ; 
  while ( p != bestpagebreak ) {
    if ( ztype ( p ) == insnode ) {
      if ( holdinginserts <= 0 ) {
	r = link ( pageinshead ) ; 
	while ( subtype ( r ) != subtype ( p ) )
	  r = link ( r ) ; 
	if ( bestinsptr ( r ) == 0 ) 
	  wait = true ; 
	else {
	  wait = false ; 
	  s = lastinsptr ( r ) ; 
	  link ( s ) = insptr ( p ) ; 
	  if ( bestinsptr ( r ) == p ) {
	    if ( ztype ( r ) == splitup ) 
	    if ( ( brokenins ( r ) == p ) && ( brokenptr ( r ) != 0 ) ) {
	      while ( link ( s ) != brokenptr ( r ) )
		s = link ( s ) ; 
	      link ( s ) = 0 ; 
	      splittopskip = splittopptr ( p ) ; 
	      insptr ( p ) = prunepagetop ( brokenptr ( r ) ) ; 
	      if ( insptr ( p ) != 0 ) {
		tempptr = vpackage ( insptr ( p ) , 0 , 1 , maxdimen ) ; 
		height ( p ) = height ( tempptr ) + depth ( tempptr ) ; 
		freenode ( tempptr , boxnodesize ) ; 
		wait = true ; 
	      }
	    }
	    bestinsptr ( r ) = 0 ; 
	    n = subtype ( r ) ; 
	    tempptr = listptr ( box ( n ) ) ; 
	    freenode ( box ( n ) , boxnodesize ) ; 
	    box ( n ) = vpackage ( tempptr , 0 , 1 , maxdimen ) ; 
	  } else {
	    while ( link ( s ) != 0 )
	      s = link ( s ) ; 
	    lastinsptr ( r ) = s ; 
	  } 
	} 
	link ( prevp ) = link ( p ) ; 
	link ( p ) = 0 ; 
	if ( wait ) {
	  link ( q ) = p ; 
	  q = p ; 
	  incr ( insertpenalties ) ; 
	} else {
	  deleteglueref ( splittopptr ( p ) ) ; 
	  freenode ( p , insnodesize ) ; 
	} 
	p = prevp ; 
      } 
    } else if ( ztype ( p ) == marknode ) {
      if ( firstmark == 0 ) {
	firstmark = markptr ( p ) ; 
	addtokenref ( firstmark ) ; 
      }
      if ( botmark != 0 ) 
	deletetokenref ( botmark ) ; 
      botmark = markptr ( p ) ; 
      addtokenref ( botmark ) ; 
    }
    prevp = p ; 
    p = link ( prevp ) ; 
  }
  splittopskip = savesplittopskip ; 
  if ( p != 0 ) {
    if ( link ( contribhead ) == 0 ) 
    if ( nestptr == 0 ) 
      curlist .tailfield = pagetail ; 
    else nest [ 0 ] .tailfield = pagetail ; 
    link ( pagetail ) = link ( contribhead ) ; 
    link ( contribhead ) = p ; 
    link ( prevp ) = 0 ; 
  } 
  savevbadness = vbadness ; 
  vbadness = infbad ; 
  savevfuzz = vfuzz ; 
  vfuzz = maxdimen ; 
  box ( 255 ) = vpackage ( link ( pagehead ) , bestsize , 0 , pagemaxdepth ) ; 
  vbadness = savevbadness ; 
  vfuzz = savevfuzz ; 
  if ( lastglue != maxhalfword ) 
    deleteglueref ( lastglue ) ; 
  pagecontents = 0 ; 
  pagetail = pagehead ; 
  link ( pagehead ) = 0 ; 
  lastglue = maxhalfword ; 
  lastpenalty = 0 ; 
  lastkern = 0 ; 
  pagesofar [ 7 ] = 0 ; 
  pagemaxdepth = 0 ; 
  if ( q != holdhead ) {
    link ( pagehead ) = link ( holdhead ) ; 
    pagetail = q ; 
  }
  r = link ( pageinshead ) ; 
  while ( r != pageinshead ) {
    q = link ( r ) ; 
    freenode ( r , pageinsnodesize ) ; 
    r = q ; 
  } 
  link ( pageinshead ) = pageinshead ; 
  if ( ( topmark != 0 ) && ( firstmark == 0 ) ) {
    firstmark = topmark ; 
    addtokenref ( topmark ) ; 
  } 
  if ( outputroutine != 0 ) 
  if ( deadcycles >= maxdeadcycles ) {
    print_err("Output loop---");
    printint ( deadcycles ) ; 
    c_print(" consecutive dead cycles");
    zhelp1( STR_H_IVE_OUTPUT_AWRY );
    error();
  } else {
    outputactive = true ; 
    incr ( deadcycles ) ; 
    pushnest () ; 
    curlist .modefield = -vmode ;
    curlist .auxfield .cint = ignoredepth ; 
    curlist .mlfield = - (integer) line ; 
    begintokenlist ( outputroutine , outputtext ) ; 
    newsavelevel ( outputgroup ) ; 
    normalparagraph () ; 
    scanleftbrace () ; 
    return ; 
  } 

  if ( link ( pagehead ) != 0 ) {
    if ( link ( contribhead ) == 0 ) 
      if ( nestptr == 0 ) 
	curlist .tailfield = pagetail ; 
      else
	nest [ 0 ] .tailfield = pagetail ; 
    else
      link ( pagetail ) = link ( contribhead ) ; 
    link ( contribhead ) = link ( pagehead ) ; 
    link ( pagehead ) = 0 ; 
    pagetail = pagehead ; 
  }
  shipout ( box ( 255 ) ) ; 
  box ( 255 ) = 0 ; 
}


void buildpage ( void )
{/* 10 30 31 22 80 90 */ buildpage_regmem 
  register halfword p  ; 
  register halfword q, r  ; 
  register integer b, c  ; 
  register integer pi  ; 
  unsigned char n  ; 
  register scaled delta, h, w  ; 

  if ( ( link ( contribhead ) == 0 ) || outputactive ) 
    return ; 

  do {
lab22:
    p = link ( contribhead ) ; 
    if ( lastglue != maxhalfword ) 
      deleteglueref ( lastglue ) ; 
    lastpenalty = 0 ; 
    lastkern = 0 ; 
    if ( ztype ( p ) == gluenode ) {
      lastglue = glueptr ( p ) ; 
      addglueref ( lastglue ) ; 
    } else {
      lastglue = maxhalfword ; 
      if ( ztype ( p ) == penaltynode ) 
	lastpenalty = mem [ p + 1 ] .cint ; 
      else if ( ztype ( p ) == kernnode ) 
	lastkern = width ( p ) ; 
    }
    switch ( ztype ( p ) ) 
    {case hlistnode : 
    case vlistnode : 
    case rulenode : 
      if ( pagecontents < boxthere ) {
	if ( pagecontents == 0 ) 
	  freezepagespecs ( boxthere ) ; 
	else
	  pagecontents = boxthere ; 
	q = newskipparam ( topskipcode ) ; 
	if ( width ( tempptr ) > height ( p ) ) 
	  width ( tempptr ) = width ( tempptr ) - height ( p ) ; 
	else
	  width ( tempptr ) = 0 ; 
	link ( q ) = p ; 
	link ( contribhead ) = q ; 
	goto lab22 ; 
      } else {
	pagesofar [ 1 ] = pagesofar [ 1 ] + pagesofar [ 7 ] + height ( p ) ; 
	pagesofar [ 7 ] = depth ( p ) ; 
	goto lab80 ; 
      }
      break ; 
    case whatsitnode : 
      goto lab80 ; 
      break ; 
    case gluenode : 
      if ( pagecontents < boxthere ) 
	goto lab31 ; 
      else if ( precedesbreak ( pagetail ) ) 
	pi = 0 ; 
      else
	goto lab90 ; 
      break ; 
    case kernnode : 
      if ( pagecontents < boxthere ) 
	goto lab31 ; 
      else if ( link ( p ) == 0 ) 
	return ; 
      else if ( ztype ( link ( p ) ) == gluenode ) 
	pi = 0 ; 
      else
	goto lab90 ; 
      break ; 
    case penaltynode : 
      if ( pagecontents < boxthere ) 
	goto lab31 ; 
      else
	pi = mem [ p + 1 ] .cint ; 
      break ; 
    case marknode : 
      goto lab80 ; 
      break ; 
    case insnode : 
      {
	if ( pagecontents == 0 ) 
	  freezepagespecs ( insertsonly ) ; 
	n = subtype ( p ) ; 
	r = pageinshead ; 
	while ( n >= subtype ( link ( r ) ) )
	  r = link ( r ) ; 
	n = n ; 
	if ( subtype ( r ) != n ) {
	  q = getnode ( pageinsnodesize ) ; 
	  link ( q ) = link ( r ) ; 
	  link ( r ) = q ; 
	  r = q ; 
	  subtype ( r ) = n ; 
	  ztype ( r ) = inserting ; 
	  ensurevbox ( n ) ; 
	  if ( box ( n ) == 0 ) 
	    height ( r ) = 0 ; 
	  else
	    height ( r ) = height ( box ( n ) ) + depth ( box ( n ) ) ; 
	  bestinsptr ( r ) = 0 ; 
	  q = skip ( n ) ; 
	  if ( count ( n ) == 1000 ) 
	    h = height ( r ) ; 
	  else
	    h = xovern ( height ( r ) , 1000 ) * count ( n ) ; 
	  pagesofar [ 0 ] = pagesofar [ 0 ] - h - width ( q ) ; 
	  pagesofar [ 2 + stretchorder ( q ) ] += stretch ( q ) ; 
	  pagesofar [ 6 ] += shrink ( q ) ; 
	  if ( ( shrinkorder ( q ) != normal ) && ( shrink ( q ) != 0 ) ) {
	    print_err("Infinite glue shrinkage inserted from ");
	    printesc( STR_SKIP );
	    printint ( n ) ; 
	    zhelp2( STR_H_THECORRECTIONGLUE, STR_H_SINCETHE_OFFENSIVE );
	    error();
	  } 
	} 
	if ( ztype ( r ) == splitup ) 
	  insertpenalties = insertpenalties + floatcost ( p ) ; 
	else {
	  lastinsptr ( r ) = p ; 
	  delta = pagesofar [ 0 ] - pagesofar [ 1 ] - pagesofar [ 7 ] + 
	  pagesofar [ 6 ] ; 
	  if ( count ( n ) == 1000 ) 
	    h = height ( p ) ; 
	  else
	    h = xovern ( height ( p ) , 1000 ) * count ( n ) ; 
	  if ( ( ( h <= 0 ) || ( h <= delta ) ) && ( height ( p ) + height ( r 
	  ) <= dimen ( n ) ) ) 
	  {
	    pagesofar [ 0 ] -= h ; 
	    height ( r ) += height ( p ) ; 
	  } else {
	    if ( count ( n ) <= 0 ) 
	      w = maxdimen ; 
	    else {
	      w = pagesofar [ 0 ] - pagesofar [ 1 ] - pagesofar [ 7 ] ; 
	      if ( count ( n ) != 1000 ) 
		w = xovern ( w , count ( n ) ) * 1000 ; 
	    } 
	    if ( w > dimen ( n ) - height ( r ) ) 
	      w = dimen ( n ) - height ( r ) ; 
	    q = vertbreak ( insptr ( p ) , w , depth ( p ) ) ; 
	    height ( r ) += bestheightplusdepth ; 
#ifdef STAT
	    if ( tracingpages > 0 ) {
	      begindiagnostic () ; 
	      c_printnl("% split");
	      printint ( n ) ; 
	      c_print(" to ");
	      printscaled ( w ) ; 
	      printchar ( 44 ) ; 
	      printscaled ( bestheightplusdepth ) ; 
	      c_print(" p=");
	      if ( q == 0 ) 
		printint ( ejectpenalty ) ; 
	      else if ( ztype ( q ) == penaltynode ) 
		printint ( mem [ q + 1 ] .cint ) ; 
	      else
		printchar ( 48 ) ; 
	      enddiagnostic ( false ) ; 
	    } 
#endif /* STAT */
	    if ( count ( n ) != 1000 ) 
	      bestheightplusdepth = xovern(bestheightplusdepth, 1000)*count(n);
	    pagesofar [ 0 ] -= bestheightplusdepth;
	    ztype ( r ) = splitup ; 
	    brokenptr ( r ) = q ; 
	    brokenins ( r ) = p ; 
	    if ( q == 0 )
	      insertpenalties += ejectpenalty ; 
	    else if ( ztype ( q ) == penaltynode ) 
	      insertpenalties += mem [ q + 1 ] .cint ; 
	  }
	}
	goto lab80 ; 
      } 
      break ; 
    default: 
      confusion("page");
      break ; 
    } 
    if ( pi < infpenalty ) {
      if ( pagesofar [ 1 ] < pagesofar [ 0 ] ) 
      if ( ( pagesofar [ 3 ] != 0 ) || ( pagesofar [ 4 ] != 0 ) || ( pagesofar 
      [ 5 ] != 0 ) ) 
      b = 0 ; 
      else b = badness ( pagesofar [ 0 ] - pagesofar [ 1 ] , pagesofar [ 2 ] );
      else if ( pagesofar [ 1 ] - pagesofar [ 0 ] > pagesofar [ 6 ] ) 
      b = awfulbad ; 
      else b = badness ( pagesofar [ 1 ] - pagesofar [ 0 ] , pagesofar [ 6 ] );
      if ( b < awfulbad ) 
      if ( pi <= ejectpenalty ) 
      c = pi ; 
      else if ( b < infbad ) 
      c = b + pi + insertpenalties ; 
      else c = 100000L ; 
      else c = b ; 
      if ( insertpenalties >= 10000 ) 
      c = awfulbad ; 
#ifdef STAT
      if ( tracingpages > 0 ) {
	begindiagnostic () ; 
	printnl ( 37 ) ; 
	c_print(" t=");
	printtotals () ; 
	c_print(" g=");
	printscaled ( pagesofar [ 0 ] ) ; 
	c_print(" b=");
	if ( b == awfulbad ) printchar ( 42 ) ; else printint ( b ) ; 
	c_print(" p=");
	printint ( pi ) ; 
	c_print(" c=");
	if ( c == awfulbad ) 
	  printchar ( 42 ) ; 
	else
	  printint ( c ) ; 
	if ( c <= leastpagecost ) 
	  printchar ( 35 ) ; 
	enddiagnostic ( false ) ; 
      } 
#endif /* STAT */
      if ( c <= leastpagecost ) {
	bestpagebreak = p ; 
	bestsize = pagesofar [ 0 ] ; 
	leastpagecost = c ; 
	r = link ( pageinshead ) ; 
	while ( r != pageinshead ) {
	  bestinsptr ( r ) = lastinsptr ( r ) ; 
	  r = link ( r ) ; 
	} 
      } 
      if ( ( c == awfulbad ) || ( pi <= ejectpenalty ) ) {
	fireup ( p ) ; 
	if ( outputactive ) 
	  return ; 
	goto lab30 ; 
      }
    } 
    if ( ( ztype ( p ) < gluenode ) || ( ztype ( p ) > kernnode ) ) 
      goto lab80 ; 
lab90:
    if ( ztype ( p ) == kernnode ) 
      q = p ; 
    else {
      q = glueptr ( p ) ; 
      pagesofar [ 2 + stretchorder ( q ) ] += stretch ( q ) ; 
      pagesofar [ 6 ] += shrink ( q ) ; 
      if ( ( shrinkorder ( q ) != normal ) && ( shrink ( q ) != 0 ) ) {
	print_err("Infinite glue shrinkage found on current page");
	zhelp3( STR_H_THEPAGE_INFINITELY, STR_H_SHRINKABLEGLUE,
		STR_H_SINCETHE_OFFENSIVE );
	error();
	r = newspec ( q ) ; 
	shrinkorder ( r ) = normal ; 
	deleteglueref ( q ) ; 
	glueptr ( p ) = r ; 
	q = r ; 
      } 
    } 
    pagesofar [ 1 ] = pagesofar [ 1 ] + pagesofar [ 7 ] + width ( q ) ; 
    pagesofar [ 7 ] = 0 ; 
lab80:
    if ( pagesofar [ 7 ] > pagemaxdepth ) {
      pagesofar [ 1 ] = pagesofar [ 1 ] + pagesofar [ 7 ] - pagemaxdepth ; 
      pagesofar [ 7 ] = pagemaxdepth ; 
    }
    link ( pagetail ) = p ; 
    pagetail = p ; 
    link ( contribhead ) = link ( p ) ; 
    link ( p ) = 0 ; 
    goto lab30 ; 

lab31:
    link ( contribhead ) = link ( p ) ; 
    link ( p ) = 0 ; 
    flushnodelist ( p ) ; 
lab30: ;
  } while ( ! ( link ( contribhead ) == 0 ) ) ;

  if ( nestptr == 0 ) 
    curlist .tailfield = contribhead ; 
  else nest [ 0 ] .tailfield = contribhead ; 
}


void insertdollarsign ( void )
{ insertdollarsign_regmem

  backinput () ; 
  curtok = mathshifttoken + 36 ; 
  print_err("Missing $ inserted");
  zhelp1( STR_H_IVE_BEGINMATH );
  inserror () ; 
} 


void youcant ( void)
{ youcant_regmem

  p_print_err( STR_H_YOUCANTUSE );
  printcmdchr( curcmd , curchr );
  c_print("' in ");
  printmode( curlist .modefield );
} 


void reportillegalcase ( void )
{ reportillegalcase_regmem

  youcant();
  zhelp1( STR_H_SORRY_NOTPROGRAMMED );
  error();
}


boolean privileged ( void )
{ privileged_regmem

  if ( curlist .modefield > 0 ) 
    return(true);
  else {
    reportillegalcase () ; 
    return(false);
  }
}


boolean itsallover ( void )
{/* 10 */ itsallover_regmem

  if ( privileged () ) {
    if ( ( pagehead == pagetail ) && ( curlist .headfield == curlist 
    .tailfield ) && ( deadcycles == 0 ) ) 
    {
      return(true);
    } 
    backinput () ; 
    tailappend ( newnullbox () ) ; 
    width ( curlist .tailfield ) = hsize ; 
    tailappend ( newglue ( fillglue ) ) ; 
    tailappend ( newpenalty ( -1073741824L ) ) ; 
    buildpage () ; 
  }
  return(false) ; 
}


void appendglue ( void )
{ appendglue_regmem
  register smallnumber s;
  register long_halfword r_curval;

  s = curchr ; 
  switch ( s ) {
  case 0 : 
    r_curval = filglue ; 
    break ; 
  case 1 : 
    r_curval = fillglue ; 
    break ; 
  case 2 : 
    r_curval = ssglue ; 
    break ; 
  case 3 : 
    r_curval = filnegglue ; 
    break ; 
  case 4 : 
    r_curval = scanglue ( glueval );
    break ; 
  case 5 : 
    r_curval = scanglue ( muval );
    break ; 
  }
  tailappend ( newglue ( r_curval ) );
  if ( s >= 4 ) {
    decr ( gluerefcount ( r_curval ) );
    if ( s > 4 )
      subtype ( curlist .tailfield ) = muglue;
  }
}


void appendkern ( void )
{ register quarterword s;
  register integer r_curval;

  s = curchr ; 
  r_curval = scandimen ( s == muglue, false, false );
  { appendkern_regmem
    tailappend ( newkern ( r_curval ) );
    subtype ( curlist .tailfield ) = s;
  }
}


void offsave ( void )
{ offsave_regmem
  register halfword p ;

  if ( curgroup == bottomlevel ) {
    print_err("Extra ");
    printcmdchr ( curcmd , curchr ) ; 
    zhelp1( STR_H_THINGS_MIXEDUP );
    error () ; 
  } else {
    backinput () ; 
    p = getavail () ; 
    link ( temphead ) = p ; 
    print_err("Missing ");

    switch ( curgroup ) {
    case semisimplegroup : 
      {
	info ( p ) = cstokenflag + frozenendgroup ; 
	printesc( STR_ENDGROUP );
      } 
      break ; 
    case mathshiftgroup : 
      {
	info ( p ) = mathshifttoken + 36 ; 
	printchar ( 36 ) ; 
      } 
      break ; 
    case mathleftgroup : 
      {
	info ( p ) = cstokenflag + frozenright ; 
	link ( p ) = getavail () ; 
	p = link ( p ) ; 
	info ( p ) = othertoken + 46 ; 
	c_printesc("right.");
      } 
      break ; 
    default: 
      {
	info ( p ) = rightbracetoken + 125 ; 
	printchar ( 125 ) ; 
      } 
      break ; 
    } 
    c_print(" inserted");
    begintokenlist ( link ( temphead ) , inserted ) ; 
    zhelp1( STR_H_IVE_INSERTED_SOMETHING );
    error () ; 
  }
}


void extrarightbrace ( void )
{ extrarightbrace_regmem

  print_err("Extra }, or forgotten ");

  switch ( curgroup ) {
  case semisimplegroup : 
    printesc( STR_ENDGROUP );
    break ; 
  case mathshiftgroup : 
    printchar ( 36 ) ; 
    break ; 
  case mathleftgroup : 
    printesc( STR_RIGHT );
    break ; 
  }
  zhelp1( STR_H_IVE_GROUPCLOSING );
  error();
  incr ( alignstate );
}


void normalparagraph ( void )
{ normalparagraph_regmem

  if ( looseness != 0 ) 
    eqworddefine ( intbase + loosenesscode , 0 ) ; 
  if ( hangindent != 0 ) 
    eqworddefine ( dimenbase + hangindentcode , 0 ) ; 
  if ( hangafter != 1 ) 
    eqworddefine ( intbase + hangaftercode , 1 ) ; 
  if ( parshapeptr != 0 ) 
    eqdefine ( parshapeloc , shaperef , 0 ) ; 
}

/* -- end -- */
