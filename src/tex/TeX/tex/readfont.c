#define EXTERN extern
#include "texd.h"

   static void
start_font_error_message(halfword u, strnumber nom, strnumber aire, scaled s)
{
    print_err("Font ");
    sprintcs ( u );
    printchar ( 61 );
    printfilename ( nom , aire , 335 );
    if ( s >= 0 ) {
      print( STR_AT_ );
      printscaled ( s );
      print( STR_PT );
    } else if ( s != -1000 ) {
      c_print(" scaled ");
      printint ( - (integer) s );
    }
}


internalfontnumber readfontinfo ( halfword u, strnumber nom, strnumber aire,
				  scaled s )
{ readfontinfo_regmem 
  fontindex k  ; 
  boolean fileopened  ; 
  register halfword lf, lh, bc, ec, nw, nh, nd, ni, nl, nk, ne, np  ; 
  register internalfontnumber f  ; 
  register internalfontnumber g  ; 
  register eightbits a, b, c, d  ; 
  fourquarters qw  ; 
  register scaled sw  ; 
  register integer bchlabel  ; 
  register short bchar  ; 
  register scaled z  ; 
  register integer alpha  ; 
  register schar beta  ; 

  bytefile tfmfile;
  unsigned short tfmheader[12];
  halfword filelen;


  g = nullfont;
  fileopened = false;
  packfilename( nom, aire, STR_DOT_TFM );
  if ( ! bopenin ( tfmfile ) )
    goto lab11;

  fileopened = true;

  /* delete the getc for TFM-Files in open_input () !! */


  /* 565. Read the TFM size fields */

  if( fread((char *)tfmheader, 2, 12, tfmfile) != 12 )
    goto lab11;

  if( (lf = tfmheader[0]) > 0x7fff || (lh = tfmheader[1]) > 0x7fff
   || (bc = tfmheader[2]) > 0x7fff || (ec = tfmheader[3]) > 0x7fff )
	goto lab11;

  if ( ( bc > ec + 1 ) || ( ec > 255 ) )
    goto lab11;

  if ( bc > 255 ) {		/* bc = 256 && ec = 255 */
      bc = 1;  ec = 0;
  }

  if( (nw = tfmheader[4]) > 0x7fff || (nh = tfmheader[5]) > 0x7fff
	|| (nd = tfmheader[6]) > 0x7fff  || (ni = tfmheader[7]) > 0x7fff
	|| (nl = tfmheader[8]) > 0x7fff  || (nk = tfmheader[9]) > 0x7fff
	|| (ne = tfmheader[10]) > 0x7fff || (np = tfmheader[11]) > 0x7fff )
     goto lab11;

  if( lf != 6 + lh + (ec-bc+1) + nw + nh + nd + ni + nl + nk + ne + np )
    goto lab11;


  /* 566. Use size fields to allocate font information */

  lf = lf - 6 - lh;	/* |lf| words should be loaded into |font_info| */
  filelen = lf;		/* Read |filelen| * 4 Bytes from TFM-File */
  if ( np < 7 )
    lf = lf + 7 - np;

  if ( ( fontptr == fontmax ) || ( fmemptr + lf > fontmemsize ) ) {
    start_font_error_message( u, nom, aire, s);
    c_print(" not loaded: Not enough room left");
    zhelp1( STR_H_IMAFRAID_FONT );
    error();
    goto lab30;
  }

  f = fontptr + 1;

#ifdef FONTPTR
#  define F_PTR_OFFSET	(&fontinfo[0])
#  define F_PTR_SIZE	sizeof(fontinfo[0])
  charbase(f)   = &fontinfo[fmemptr - bc];
#else
#  define F_PTR_OFFSET	0
#  define F_PTR_SIZE	1
  charbase(f)   = fmemptr - bc;
#endif
  widthbase(f)  = charbase(f) + ec + 1;
  heightbase(f) = widthbase(f) + nw;
  depthbase(f)  = heightbase(f) + nh;
  italicbase(f) = depthbase(f) + nd;
  ligkernbase(f) = italicbase(f) + ni /*** - F_PTR_OFFSET ***/ ;
  kernbase(f)    = ligkernbase(f) + nl - kernbaseoffset;

  /* These are really indizedes in fontinfo[] */
  extenbase(f)   = (kernbase(f) - F_PTR_OFFSET) + kernbaseoffset + nk;
  parambase(f)   = extenbase(f) + ne;

  /* 568. Read the TFM header */

  if ( lh < 2 ) 
    goto lab11;

  qw.b0 = getc ( tfmfile );  qw.b1 = getc ( tfmfile );
  qw.b2 = getc ( tfmfile );  qw.b3 = getc ( tfmfile );
  fontcheck(f) = qw;

  z = getc ( tfmfile );
  if ( z > 127 )
    goto lab11;
  z = z * 256 + getc ( tfmfile );
  z = z * 256 + getc ( tfmfile );
  z = ( z * 16 ) + ( getc(tfmfile) / 16 );

  if ( z < unity ) 
    goto lab11;

  while ( lh > 2 ) {
      (void) getc ( tfmfile );  (void) getc ( tfmfile );
      (void) getc ( tfmfile );  (void) getc ( tfmfile );
      decr ( lh );
  }

  fontdsize(f) = z;
  if ( s != -1000 )
    if ( s >= 0 )
      z = s;
    else
      z = xnoverd ( z , - (integer) s , 1000 );
  fontsize(f) = z;


  /* (br) Read the whole thing by one fread() */
  /* if (eof()) at end of function could be deleted */
  if(fread( (char *)&fontinfo[fmemptr], sizeof(fontinfo[0]), filelen, tfmfile)
	!= filelen)
    goto lab11;

	
  /* 569. Read character data */

  for( k = fmemptr ; k <= (widthbase(f) - F_PTR_OFFSET) - 1 ; k++ ) {

    a = fontinfo[k].qqqq.b0;  b = fontinfo[k].qqqq.b1;
    c = fontinfo[k].qqqq.b2;  d = fontinfo[k].qqqq.b3;

    if ( ( a >= nw ) || ( b/16 >= nh ) || ( b%16 >= nd ) || ( c/4 >= ni ) )
      goto lab11;

    switch ( c % 4 ) {
      case ligtag:
	if ( d >= nl ) 
	  goto lab11;
	break;
      case exttag:
	if ( d >= ne )
	  goto lab11;
	break;
      case listtag:   /* 570. Check for charlist cycle */
	if ( ( d < bc ) || ( d > ec ) )
	  goto lab11;
	while ( d < k + bc - fmemptr ) {
	  qw = CharInfo( f, d );
	  if ( chartag(qw) != listtag )
	    goto lab45;
	  d = rembyte(qw);
	}
	if ( d == k + bc - fmemptr )
	  goto lab11;
lab45:
	break;
      default: 
	break;
    }
  }


  /* 571. Read box dimensions */

  alpha = 16;
  while ( z >= 8388608L ) {
    z = z / 2;
    alpha = alpha + alpha;
  }
  beta = 256 / alpha;
  alpha = alpha * z;

  for( k = (widthbase(f) - F_PTR_OFFSET) ;
	k <= (ligkernbase(f) - F_PTR_OFFSET ) - 1 ; k++ ) {

    a = fontinfo[k].qqqq.b0;  b = fontinfo[k].qqqq.b1;
    c = fontinfo[k].qqqq.b2;  d = fontinfo[k].qqqq.b3;

    sw = ( ( ( ( (d*z) / 256 ) + (c*z) ) / 256 ) + ( b * z ) ) / beta;
    if ( a == 0 )
      fontinfo [ k ] .cint = sw;
    else if ( a == 255 ) 
      fontinfo [ k ] .cint = sw - alpha;
    else
      goto lab11;
  }

  if ( fontinfo [ widthbase(f) - F_PTR_OFFSET ] .cint != 0 ) 
    goto lab11 ; 
  if ( fontinfo [ heightbase(f) - F_PTR_OFFSET ] .cint != 0 ) 
    goto lab11 ; 
  if ( fontinfo [ depthbase(f) - F_PTR_OFFSET ] .cint != 0 ) 
    goto lab11 ; 
  if ( fontinfo [ italicbase(f) - F_PTR_OFFSET ] .cint != 0 ) 
    goto lab11 ; 


  /* 573. Read ligature/kern program */

  bchlabel = 32767;
  bchar = 256;
  if ( nl > 0 ) {
    for( k = (ligkernbase(f) - F_PTR_OFFSET) ;
	 k <= (kernbase(f) - F_PTR_OFFSET) + kernbaseoffset - 1 ; k++ ) {

      a = fontinfo[k].qqqq.b0;  b = fontinfo[k].qqqq.b1;
      c = fontinfo[k].qqqq.b2;  d = fontinfo[k].qqqq.b3;

      if ( a > 128 ) {
	if ( 256 * c + d >= nl )
	  goto lab11;
	if ( a == 255 )
	  if ( k == (ligkernbase(f) - F_PTR_OFFSET) )
	    bchar = b;
      } else {
	if ( b != bchar ) {
	  if ( ( b < bc ) || ( b > ec ) )
	    goto lab11;
	  qw = zcharinfo ( f ,  b );
	  if ( ! charexists ( qw ) )
	    goto lab11;
	}
	if ( c < 128 ) {
	  if ( ( d < bc ) || ( d > ec ) )
	    goto lab11 ; 
	  qw = zcharinfo ( f ,  d ) ; 
	  if ( ! charexists ( qw ) ) 
	    goto lab11;
	} else if ( 256 * ( c - 128 ) + d >= nk )
	  goto lab11;

	if ( a < 128 ) 
	  if ( k - (ligkernbase(f) - F_PTR_OFFSET) + a + 1 >= nl ) 
	    goto lab11;
      }
    }
    if ( a == 255 )
      bchlabel = 256 * c + d;
  } /* if ... */

  for( k = (kernbase(f) - F_PTR_OFFSET) + kernbaseoffset ;
	k <= extenbase(f) - 1 ; k++ ) {

    a = fontinfo[k].qqqq.b0;  b = fontinfo[k].qqqq.b1;
    c = fontinfo[k].qqqq.b2;  d = fontinfo[k].qqqq.b3;

    sw = ( ( ( ( (d*z) / 256 ) + (c*z) ) / 256 ) + ( b * z ) ) / beta;
    if ( a == 0 ) 
      fontinfo [ k ] .cint = sw ; 
    else if ( a == 255 ) 
      fontinfo [ k ] .cint = sw - alpha ; 
    else
      goto lab11;
  }


  /* 574. Read extensible character recipes */

  for( k = extenbase(f) ; k <= parambase(f) - 1 ; k++ ) {

    a = fontinfo[k].qqqq.b0;  b = fontinfo[k].qqqq.b1;
    c = fontinfo[k].qqqq.b2;  d = fontinfo[k].qqqq.b3;

    if ( a != 0 ) {
	{
	  if ( ( a < bc ) || ( a > ec ) ) 
	  goto lab11 ; 
	} 
	qw = zcharinfo ( f ,  a ) ; 
	if ( ! charexists ( qw ) ) 
	goto lab11 ; 
      } 
      if ( b != 0 ) {
	{
	  if ( ( b < bc ) || ( b > ec ) ) 
	  goto lab11 ; 
	} 
	qw = zcharinfo ( f ,  b ) ; 
	if ( ! charexists ( qw ) ) 
	goto lab11 ; 
      } 
      if ( c != 0 ) {
	{
	  if ( ( c < bc ) || ( c > ec ) ) 
	  goto lab11 ; 
	} 
	qw = zcharinfo ( f ,  c ) ; 
	if ( ! charexists ( qw ) ) 
	goto lab11 ; 
      }
      {
	{
	  if ( ( d < bc ) || ( d > ec ) ) 
	  goto lab11 ; 
	} 
	qw = zcharinfo ( f ,  d ) ; 
	if ( ! charexists ( qw ) ) 
	goto lab11 ; 
      } 
  }


  /* 575. Read font parameter */

  for( k = 1 ; k <= np ; k++ ) {

    a = fontinfo[parambase(f)+k-1].qqqq.b0;
    b = fontinfo[parambase(f)+k-1].qqqq.b1;
    c = fontinfo[parambase(f)+k-1].qqqq.b2;
    d = fontinfo[parambase(f)+k-1].qqqq.b3;
#if 0
    qw = fontinfo [ parambase(f) + k - 1].qqqq;
    a = qw.b0;  b = qw.b1;  c = qw.b2;  d = qw.b3;
#endif

    if ( k == 1 ) {
      sw = a;
      if ( sw > 127 ) 
	sw = sw - 256;
      sw = sw * 256 + b;
      sw = sw * 256 + c;
      fontinfo [ parambase(f) ].cint = ( sw * 16 ) + ( d / 16 );
    } else {
      sw = ( ( ( ( (d*z) / 256 ) + (c*z) ) / 256 ) + ( b * z ) ) / beta;
      if ( a == 0 ) 
	fontinfo [ parambase(f) + k - 1 ] .cint = sw;
      else if ( a == 255 ) 
	fontinfo [ parambase(f) + k - 1 ] .cint = sw - alpha;
      else goto lab11 ; 
    }
  }


#if 0		/* (br) not necessary, because of the fread()-call */
  if ( feof ( tfmfile ) )
    goto lab11;
#endif

  for( k = np + 1 ; k <= 7 ; k++ ) {
    fontinfo [ parambase(f) + k - 1 ] .cint = 0 ; 
  }


  /* 576. Make final adjustments and goto done */

  if ( np >= 7 )
    fontparams(f) = np;
  else
    fontparams(f) = 7;

  hyphenchar(f) = defaulthyphenchar;
  skewchar(f) = defaultskewchar;

  if ( bchlabel < nl )
    bcharlabel(f) = bchlabel + (ligkernbase(f) - F_PTR_OFFSET);
  else
    bcharlabel(f) = nonaddress;

  fontbchar(f) = bchar;
  fontfalsebchar(f) = bchar;
  if( ( bchar <= ec ) && ( bchar >= bc ) ) {
    qw = zcharinfo( f, bchar );
    if ( charexists(qw) )
      fontfalsebchar(f) = nonchar;
  }

  fontname(f) = nom;
  fontarea(f) = aire;
  fontbc(f)   = bc;
  fontec(f)   = ec;
  fontglue(f) = 0;

  decr ( parambase(f) );
  fmemptr = fmemptr + lf;
  fontptr = f;
  g = f;
  goto lab30;

lab11:
  start_font_error_message( u, nom, aire, s);

  c_print(" not loadable: ");
  if ( fileopened )
    c_print("Bad metric (TFM) file");
  else
    c_print("Metric (TFM) file not found");
  zhelp1( STR_H_IWASNT_READSIZE );
  error();

lab30:
  if ( fileopened )
    bclose( tfmfile );

  return(g);
}

/* -- end -- */
