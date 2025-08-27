#define EXTERN extern
#include "texd.h"

#define synch_h \
  if( curh != dvih ) { movement(curh-dvih, right1); dvih = curh; }

#define synch_v \
  if( curv != dviv ) { movement(curv-dviv, down1); dviv = curv; }


/* static quarterword c, f; */
static scaled ruleht, ruledp, rulewd;
static scaled dvih, dviv;
static scaled curh, curv;
static internalfontnumber dvif;
static integer curs = -1;

static halfword downptr = 0, rightptr = 0;

static void movement ( scaled w, eightbits o );
static void prunemovements ( integer l );
static void specialout ( halfword p );
static void writeout ( halfword p );
static void hlistout ( halfword thisbox );
static void vlistout ( halfword thisbox );


void dviswap ( void )
{ dviswap_regmem 

  if ( dvilimit == dvibufsize ) {
    writedvi ( 0 , halfbuf - 1 ) ; 
    dvilimit = halfbuf ; 
    dvioffset = dvioffset + dvibufsize ; 
    dviptr = 0 ; 
  } else {
    writedvi ( halfbuf , dvibufsize - 1 ) ; 
    dvilimit = dvibufsize ; 
  } 
  dvigone = dvigone + halfbuf ; 
}

void dvifour ( integer x ) 
{ dvifour_regmem 

#if defined(atarist) || defined(AMIGA)
  /* 68000 rechnet im 2er-Komplement, deshalb nicht notwendig */
  union { char cc[4]; long ll; } xx;
  xx.ll = x;
  dviout ( xx.cc[0] );   dviout ( xx.cc[1] );
  dviout ( xx.cc[2] );   dviout ( xx.cc[3] );
#else
  if ( x >= 0 ) 
    dviout ( x / 16777216L ) ; 
  else {
    x = x + 1073741824L ; 
    x = x + 1073741824L ; 
    dviout ( ( x / 16777216L ) + 128 ) ; 
  } 
  x = x % 16777216L ; 
  dviout ( x / 65536L ) ; 
  x = x % 65536L ; 
  dviout ( x / 256 ) ; 
  dviout ( x % 256 ) ;
#endif
} 

void dvipop ( integer l )
{ dvipop_regmem 

  if ( ( l == dvioffset + dviptr ) && ( dviptr > 0 ) ) 
    decr ( dviptr ) ; 
  else dviout ( pop ) ; 
}

void dvifontdef ( internalfontnumber f )
{ dvifontdef_regmem 
  register poolpointer k  ; 

  dviout ( fntdef1 ) ; 
  dviout ( f - 1 ) ; 
  dviout ( fontcheck(f).b0 ) ; 
  dviout ( fontcheck(f).b1 ) ; 
  dviout ( fontcheck(f).b2 ) ; 
  dviout ( fontcheck(f).b3 ) ; 
  dvifour ( fontsize(f) ) ; 
  dvifour ( fontdsize(f) ) ; 
  dviout ( length ( fontarea(f) ) ) ; 
  dviout ( length ( fontname(f) ) ) ; 
  {register integer for_end; k = strstart [ fontarea(f) ] ; for_end = 
  strstart [ fontarea(f) + 1 ] - 1 ; if ( k <= for_end) do 
    dviout ( strpool [ k ] ) ; 
  while ( k++ < for_end ) ; } 
  {register integer for_end; k = strstart [ fontname(f) ] ; for_end = 
  strstart [ fontname(f) + 1 ] - 1 ; if ( k <= for_end) do 
    dviout ( strpool [ k ] ) ; 
  while ( k++ < for_end ) ; } 
} 

  static
void movement ( scaled w, eightbits o )		/* 607. */
{ movement_regmem 
  register halfword q;
  register smallnumber mstate  ; 
  register halfword p;
  register integer k  ; 

  q = getnode ( movementnodesize );

  width ( q ) = w ; 
  location ( q ) = dvioffset + dviptr ; 
  if ( o == down1 ) {
    link ( q ) = downptr ; 
    downptr = q ; 
  } else {
    link ( q ) = rightptr ; 
    rightptr = q ; 
  } 
  p = link ( q ) ; 
  mstate = 0 ; 
  while ( p != 0 ) {
    if ( width ( p ) == w ) 
    switch ( mstate + info ( p ) ) 
    {case 3 : 
    case 4 : 
    case 15 : 
    case 16 : 
      if ( location ( p ) < dvigone ) 
      goto lab45 ; 
      else {
	k = location ( p ) - dvioffset ; 
	if ( k < 0 ) 
	k = k + dvibufsize ; 
	dvibuf [ k ] = dvibuf [ k ] + y1 - down1 ; 
	info ( p ) = 1 ; 
	goto lab40 ; 
      } 
      break ; 
    case 5 : 
    case 9 : 
    case 11 : 
      if ( location ( p ) < dvigone ) 
      goto lab45 ; 
      else {
	k = location ( p ) - dvioffset ; 
	if ( k < 0 ) 
	k = k + dvibufsize ; 
	dvibuf [ k ] = dvibuf [ k ] + z1 - down1 ; 
	info ( p ) = 2 ; 
	goto lab40 ; 
      } 
      break ; 
    case 1 : 
    case 2 : 
    case 8 : 
    case 13 : 
      goto lab40 ; 
      break ; 
    default: 
      break ; 
    } 
    else switch ( mstate + info ( p ) ) 
    {case 1 : 
      mstate = 6 ; 
      break ; 
    case 2 : 
      mstate = 12 ; 
      break ; 
    case 8 : 
    case 13 : 
      goto lab45 ; 
      break ; 
    default: 
      break ; 
    } 
    p = link ( p ) ; 
  } 

lab45:
  info ( q ) = 3 ; 

  if ( abs ( w ) >= 8388608L ) {
    dviout ( o + 3 ) ; dvifour ( w ) ; 
    return ; 
  }
  if ( abs ( w ) >= 32768L ) {
    dviout ( o + 2 ) ; 
    if ( w < 0 ) 
      w = w + 16777216L ; 
    dviout ( w / 65536L ) ; 
    w = w % 65536L ; 
  } else if ( abs ( w ) >= 128 ) {
    dviout ( o + 1 ) ; 
    if ( w < 0 ) 
      w = w + 65536L ; 
  } else {
    dviout ( o ) ; 
    if ( w < 0 ) 
      w = w + 256 ; 
    goto lab1 ; 
  }

  dviout ( w / 256 ) ;
lab1:
  dviout ( w % 256 ) ; 
  return ; 

lab40:
  info ( q ) = info ( p ) ; 
  if ( info ( q ) == 1 ) {
    dviout ( o + y0 - down1 ) ; 
    while ( link ( q ) != p ) {
      q = link ( q ) ; 
      switch ( info ( q ) ) 
      {case 3 : 
	info ( q ) = 5 ; 
	break ; 
      case 4 : 
	info ( q ) = 6 ; 
	break ; 
      default: 
	break ; 
      } 
    } 
  } else {
    dviout ( o + z0 - down1 ) ; 
    while ( link ( q ) != p ) {
      q = link ( q ) ; 
      switch ( info ( q ) ) 
      {case 3 : 
	info ( q ) = 4 ; 
	break ; 
      case 5 : 
	info ( q ) = 6 ; 
	break ; 
      default: 
	break ; 
      } 
    } 
  }
}

#if 0
  static
void prunemovements ( integer l )	/* 615. */
{/* 30 10 */ prunemovements_regmem 
  register halfword p;
  register halfword drp;	/* br */

  drp = downptr;
  while ( drp != 0 ) {
    if ( location ( drp ) < l ) 
      goto lab30;
    p = drp;
    drp = link ( p ) ; 
    freenode ( p , movementnodesize ) ; 
  }
lab30:
  downptr = drp;
  drp = rightptr;
  while ( drp != 0 ) {
    if ( location ( drp ) < l ) {
      rightptr = drp;
      break;	/* while */
    }
    p = drp;
    drp = link ( p );
    freenode ( p , movementnodesize );
  }
}
#else
  static
void prunemovements ( integer l )	/* 615. */
{ prunemovements_regmem 
  register halfword p;

  while ( downptr != 0 ) {
    if ( location ( downptr ) < l ) 
      goto lab30;
    p = downptr;
    downptr = link ( p ) ; 
    freenode ( p , movementnodesize ) ; 
  }
lab30:
  while ( rightptr != 0 ) {
    if ( location ( rightptr ) < l ) {
	return;
    }
    p = rightptr;
    rightptr = link ( p );
    freenode ( p , movementnodesize );
  }
}
#endif

  static
void specialout ( halfword p )
{ specialout_regmem 
  register integer oldsetting  ; 
  register poolpointer k  ; 

  synch_h;  synch_v;

  oldsetting = selector ; 
  selector = newstring ; 
  showtokenlist ( link ( writetokens ( p ) ) , 0 , poolsize - poolptr ) ; 
  selector = oldsetting ; 
  strroom ( 1 ) ; 
  if ( curlength < 256 ) {
    dviout ( xxx1 ) ; 
    dviout ( curlength ) ; 
  } else {
    dviout ( xxx4 ) ; 
    dvifour ( curlength ) ; 
  } 
  {register integer for_end; k = strstart [ strptr ] ; for_end = poolptr - 1 
  ; if ( k <= for_end) do 
    dviout ( strpool [ k ] ) ; 
  while ( k++ < for_end ) ; } 
  poolptr = strstart [ strptr ] ; 
}

  static
void writeout ( halfword p )
{ writeout_regmem 
  register integer oldsetting  ; 
  register integer oldmode  ; 
  register smallnumber j  ; 
  register halfword q, r  ; 

  q = getavail () ; 
  info ( q ) = rightbracetoken + 125 ; 
  r = getavail () ; 
  link ( q ) = r ; 
  info ( r ) = cstokenflag + endwrite ; 
  begintokenlist ( q , inserted ) ; 
  begintokenlist ( writetokens ( p ) , writetext ) ; 
  q = getavail () ; 
  info ( q ) = leftbracetoken + 123 ; 
  begintokenlist ( q , inserted ) ; 
  oldmode = curlist .modefield ; 
  curlist .modefield = 0 ; 
  curcs = writeloc ; 
  q = scantoks ( false , true ) ; 
  gettoken () ; 
  if ( curtok != cstokenflag + endwrite ) {
    print_err("Unbalanced write command");
    zhelp2( STR_H_ONTHISPAGE_WRITE, STR_H_ICANT_HANDLE_THAT );
    error();
    do {
	gettoken () ; 
    } while ( ! ( curtok == cstokenflag + endwrite ) ) ; 
  }
  curlist .modefield = oldmode ; 
  endtokenlist () ; 
  oldsetting = selector ; 
  j = writestream ( p ) ; 
  if ( writeopen [ j ] ) 
    selector = j ; 
  else {
    if ( ( j == 17 ) && ( selector == termandlog ) ) 
      selector = logonly ; 
    c_printnl("");
  }
  tokenshow ( defref ) ; 
  println () ; 
  flushlist ( defref ) ; 
  selector = oldsetting ; 
} 

void outwhat ( halfword p )
{ outwhat_regmem 
  register smallnumber j  ; 

  switch ( subtype ( p ) ) 
  {case opennode : 
  case writenode : 
  case closenode : 
    if ( ! doingleaders ) {
      j = writestream ( p ) ; 
      if ( subtype ( p ) == writenode ) 
	writeout ( p ) ; 
      else {
	if ( writeopen [ j ] ) 
	  aclose ( writefile [ j ] ) ; 
	if ( subtype ( p ) == closenode ) 
	  writeopen [ j ] = false ; 
	else if ( j < 16 ) {
	  curname = openname ( p ) ; 
	  curarea = openarea ( p ) ; 
	  curext = openext ( p ) ; 
	  if ( curext == 335 ) 
	    curext = STR_DOT_TEX;
	  packfilename ( curname , curarea , curext ) ; 
	  while ( ! aopenout ( writefile [ j ] ) )
	    promptfilename("output file name", STR_DOT_TEX);
	  writeopen [ j ] = true;
	} 
      } 
    } 
    break ; 
  case specialnode : 
    specialout ( p ) ; 
    break ; 
  case languagenode : 
    break ; 
  default: 
    confusion("ext4");
    break ; 
  } 
} 


#ifdef TEXXET
  static
long_halfword reverse ( halfword this_box, halfword t )
{ reverse_regmem

  register halfword l, p, q;
  register char g_order;
  register char g_sign;
  register halfword m, n;

  g_order = glueorder(this_box);  g_sign = gluesign(this_box);
  l = t;
  p = tempptr;
  m = minhalfword;  n = minhalfword;

  while( p != 0 ) {
    /* 1393. Move node p to the new list ... */
reswitch:
    if( ischarnode(p) ) {
      do {
	f = font(p);  c = character(p);
	curh += zcharwidth(f, zcharinfo(f, c));
	q = link(p);  link(p) = l;  l = p;  p = q;
      } while( ischarnode(p) );
    } else {
      /* 1394. Move the non-char_node p to the new list. */
      q = link(p);
      rulewd = width(p);
      switch( ztype(p) ) {
	case hlistnode:  case vlistnode:
	case rulenode:  case kernnode:
	  break;
	case gluenode:
	  g = glueptr(p); rulewd = width(g);
	  if( g_sign != normal ) {
	    if( g_sign == stretching ) {
	      if( stretchorder(g) == g_order )
		rulewd += round(float(glueset(this_box)) * stretch(g));
	    } else {
	      if( shrinkorder(g) == g_order )
		rulewd -= round(float(glueset(this_box)) * shrink(g));
	    }
	  }
	  tempptr = leaderptr(p);
	  if( tempptr == 0 ) {
	    deleteglueref(g);  ztype(p) = kernnode;  width(p) = rulewd;
	  } else if( ztype(tempptr) == rulenode ) {
	    deleteglueref(g);
	    freenode(p, smallnodesize);
	    p = tempptr;  width(p) = rulewd;
	  }
	  break;

	case ligaturenode:
	  flushnodelist(ligptr(p));
	  tempptr = p;
	  p = getavail();  mem[p] = mem[ligchar(tempptr)];
	  link(p) = q;
	  freenode(tempptr, smallnodesize);
	  goto reswitch;

	case mathnode:
	  if( end_LR(p) ) {
	    if( n > minhalfword ) {
	      decr(n);  decr(subtype(p));  /* change after into before */
	    } else {
	      if( info(LR_ptr) != subtype(p) )
		confusion("LR");
	      pop_LR;
	      if( m == minhalfword ) {
		/* 1398. Finish ... */
		if( t == 0 )
		  confusion("LR");
		freenode(p, smallnodesize);
		link(t) = q;
		width(t) = rulewd;
		edge_dist(t) = - curh - rulewd;
		goto done;
	      }
	      decr(m);
	      ztype(p) = kernnode;
	    }
	  } else if( n > minhalfword || LR_dir(p) != cur_dir ) {
	    incr(n);  incr(subtype(p));  /* change before into after */ 
	  } else {
	    push_LR(p);  incr(m);
	    ztype(p) = kernnode;
	  }
	  break;

	case R_node:  edge_node:
	  confusion("LR");
	  break;
	default:
	  goto next_p;
	  break;
      }
      curh += rulewd;
next_p:
      if( (ztype(p) == kernnode) && ((rulewd == 0) || (l == 0)) )
	freenode(p, smallnodesize);
      else {
        link(p) = l;  l = p;
      }
      p = q;
    }
  }

  if( t != 0 || m != minhalfword || n != minhalfword ) {
    confusion("LR");
  }
done:
  return(l);
}
#endif


  static
void hlistout ( halfword thisbox )
{/* 21 13 14 15 */ hlistout_regmem 

  register eightbits *hlist_dvibuf = dvibuf;	/* (br) added this one ... */
#define dvibuf hlist_dvibuf

  register scaled baseline  ; 
  register scaled leftedge  ; 
/* register scaled saveh, savev  ; */
/* register halfword thisbox  ; */
  register glueord gorder  ; 
  register integer gsign  ; 
  register halfword p  ; 
  register integer saveloc  ; 
  register halfword leaderbox  ; 
  register scaled leaderwd  ; 
/* register scaled lx  ; */
/* boolean outerdoingleaders  ; */
  register scaled edge  ; 
#ifdef TEXXET
  register halfword prev_p;
#endif

/*  thisbox = tempptr ; */
  gorder = glueorder ( thisbox ) ; 
  gsign = gluesign ( thisbox ) ; 
  p = listptr ( thisbox ) ; 
  incr ( curs ) ; 
  if ( curs > 0 ) 
    dviout ( push ) ; 
  if ( curs > maxpush ) 
    maxpush = curs ; 
  saveloc = dvioffset + dviptr ; 
  baseline = curv ;
  leftedge = curh ;
#ifdef TEXXET
  prev_p = thisbox + listoffset;

  if( cur_dir == right_to_left ) {
    if( ztype(thisbox) == hlistnode ) {
      /* 1390. Reverse the complete hlist and change this node into a R_node */
      scaled saveh;

      saveh = curh;
        tempptr = p;  /* Uebergebe `reverse' `p' */
        p = newkern(0);  link(prev_p) = p;

        curh = 0;  link(p) = reverse(thisbox, 0);  width(p) = -curh;
      curh = saveh;
      ztype(thisbox) = R_code;
    }
  }
#endif

  while ( p != 0 )
	/* 620. Output node p ... */
lab21:
    if ( ischarnode ( p ) ) {
      synch_h;  synch_v;

      { /* 620. Output node |p| ...  !! inner loop !! */
	register quarterword tmp_f, tmp_c;
	register scaled tmp_curh = curh;

      do {
	tmp_f = font ( p ) ; 
        tmp_c = character ( p ) ; 
        if ( tmp_f != dvif ) {
	  if ( ! fontused(tmp_f) ) {
	    dvifontdef ( tmp_f ) ; 
	    fontused(tmp_f) = true ; 
	  }
	  if ( tmp_f <= 64 ) 
	    dviout ( tmp_f - 1 + fntnum0 ) ; 
	  else {
	    dviout ( fnt1 ) ; 
	    dviout ( tmp_f - 1 ) ; 
	  }
	  dvif = tmp_f;
        }
#ifdef MLTEX
	if( tmp_c > fontec(tmp_f) ) {
	  p = substitute_char_list(p, tmp_f, tmp_c);
	} else {
#endif
	if ( tmp_c >= 128 )
	  dviout ( set1 );
	dviout ( tmp_c );
	tmp_curh += zcharwidth(tmp_f, zcharinfo(tmp_f, tmp_c));
	p = link ( p );
#ifdef MLTEX
        }
#endif
      } while ( ischarnode ( p ) );

      curh = tmp_curh;
      }
      dvih = curh ; 

    } else {
      /* 622. Output the non-charnode p ... */

    switch ( ztype ( p ) ) {
    case hlistnode : 
    case vlistnode : 
#ifdef TEXXET
    case R_node:
#endif
      if ( listptr ( p ) == 0 ) 
	curh += width ( p ) ;
      else {
        scaled saveh, savev;

	saveh = dvih ; 
	savev = dviv ; 
	curv = baseline + shiftamount ( p ) ; 
	/* tempptr = p ; */
#ifdef TEXXET
	edge = curh + width(p);
	if( cur_dir == right_to_left )
	  curh = edge;
#else
	edge = curh ; 
#endif
	if ( ztype ( p ) == vlistnode ) 
	  vlistout ( p ) ; 
	else
	  hlistout ( p ) ; 
	dvih = saveh ; 
	dviv = savev ;
#ifdef TEXXET
	curh = edge;
#else
	curh = edge + width ( p ) ; 
#endif
	curv = baseline ; 
      }
      break ; 
    case rulenode : 
      {
	ruleht = height ( p ) ; 
	ruledp = depth ( p ) ; 
	rulewd = width ( p ) ; 
	goto lab14 ; 
      } 
      break ; 
    case whatsitnode : 
      outwhat ( p ) ;
      break ; 
    case gluenode : 
      { { halfword g;
	g = glueptr ( p ) ; 
	rulewd = width ( g ) ; 
	if ( gsign != normal ) {
	  if ( gsign == stretching ) {
	    if ( stretchorder ( g ) == gorder )
	      rulewd += round ( glueset ( thisbox ) * stretch ( g ) ) ;
	  } else {
	    if ( shrinkorder ( g ) == gorder ) 
	      rulewd -= round ( glueset ( thisbox ) * shrink ( g ) ) ;
	  }
	}
	}
	if ( subtype ( p ) >= aleaders ) {
	  /* 626. ... */
	  leaderbox = leaderptr ( p ) ;
	  if ( ztype ( leaderbox ) == rulenode ) {
	    ruleht = height ( leaderbox ) ;
	    ruledp = depth ( leaderbox ) ;
	    goto lab14 ; 
	  }
	  leaderwd = width ( leaderbox ) ; 
	  if ( ( leaderwd > 0 ) && ( rulewd > 0 ) ) {
	    scaled lx;
	    scaled saveh, savev;
	    boolean outerdoingleaders;

	    rulewd += 10 ;
#ifdef TEXXET
	    if( cur_dir == right_to_left )
		curh -= 10;
#endif
	    edge = curh + rulewd ;
	    lx = 0 ; 
	    if ( subtype ( p ) == aleaders ) {
	      saveh = curh ;
	      curh = leftedge + leaderwd * ( ( curh - leftedge ) / leaderwd ) ;
	      if ( curh < saveh ) 
		curh += leaderwd ;
	    } else {
	      integer lq, lr;

	      lq = rulewd / leaderwd ; 
	      lr = rulewd % leaderwd ; 
	      if ( subtype ( p ) == cleaders ) 
		curh += ( lr / 2 ) ;
	      else {
		lx = ( 2 * lr + lq + 1 ) / ( 2 * lq + 2 ) ; 
		curh += ( ( lr - ( lq - 1 ) * lx ) / 2 ) ;
	      }
	    }
	    while ( curh + leaderwd <= edge ) {
	      curv = baseline + shiftamount ( leaderbox ) ; 

	      synch_v;  savev = dviv ; 
	      synch_h;  saveh = dvih ; 

	      /* tempptr = leaderbox ; */
#ifdef TEXXET
	      if( cur_dir == right_to_left )
		curh += leaderwd;
#endif
	      outerdoingleaders = doingleaders ; 
	      doingleaders = true ; 
	      if ( ztype ( leaderbox ) == vlistnode ) 
		vlistout ( leaderbox ) ; 
	      else
		hlistout ( leaderbox ) ; 
	      doingleaders = outerdoingleaders ; 
	      dviv = savev ; 
	      dvih = saveh ; 
#if 0  /* 3.1415 */
	      curv = savev ; 
#else
	      curv = baseline ; 
#endif
	      curh = saveh + leaderwd + lx ; 
	    }
#ifdef TEXXET
	    if( cur_dir == right_to_left )
		curh = edge;
	    else
		curh = edge - 10;
#else
	    curh = edge - 10 ; 
#endif
	    goto lab15 ; 
	  }
	}
	goto lab13 ; 
      }
      break ; 
#ifdef TEXXET
    case kernnode : 
      curh += width ( p ) ;
      break ; 
    case mathnode : 
      /* 1387. Adjust the LR stack ... */
      if( end_LR(p) ) {
	if( info(LR_ptr) != subtype(p) )
	  confusion("LR");
	pop_LR;
	ztype(p) = kernnode;
      } else {
	push_LR(p);
	ztype(p) = kernnode;
	if( LR_dir(p) != cur_dir ) {
	  /* 1391. Reverse the hlist segment ... */
	  scaled saveh;

	  saveh = curh;
	  tempptr = link(p);  rulewd = width(p);
	  freenode(p, smallnodesize);
	  cur_dir = reflected;
	  p = new_edge(cur_dir, rulewd);
	  link(prev_p) = p;
	  curh = curh - leftedge + rulewd;
	  link(p) = reverse(thisbox, new_edge(reflected, 0));
	  edge_dist(p) = curh;
	  cur_dir = reflected;
	  curh = saveh;

	  goto lab21 /*reswitch*/;
	}
	ztype(p) = kernnode;
      }
      curh += width ( p ) ;
      break ; 
#else
    case kernnode : 
    case mathnode : 
      curh += width ( p ) ;
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
    goto lab15 ;

lab14:
    if ( isrunning ( ruleht ) )
      ruleht = height ( thisbox ) ;
    if ( isrunning ( ruledp ) )
      ruledp = depth ( thisbox ) ;
    ruleht += ruledp ;
    if ( ( ruleht > 0 ) && ( rulewd > 0 ) ) {
      synch_h;  curv = baseline + ruledp ;  synch_v;

      dviout ( setrule ) ;
      dvifour ( ruleht ) ;
      dvifour ( rulewd ) ;
      curv = baseline ;
      dvih += rulewd ;
    }
lab13:
    curh += rulewd ;
lab15:
#ifdef TEXXET
    prev_p = p;
#endif
    p = link(p);
  }
  prunemovements ( saveloc ) ;
  if ( curs > 0 )
    dvipop ( saveloc ) ;
  decr ( curs ) ;
#undef dvibuf
}


  static
void vlistout ( halfword thisbox )
{/* 13 14 15 */ vlistout_regmem 
  register scaled leftedge  ; 
  register scaled topedge  ; 
/* register scaled saveh, savev  ; */
/* register halfword thisbox  ; */
  register glueord gorder  ; 
  register integer gsign  ; 
  register halfword p  ; 
  register integer saveloc  ; 
  register halfword leaderbox  ; 
  register scaled leaderht  ; 
/* register scaled lx  ; */
/* boolean outerdoingleaders  ; */
  register scaled edge  ;

  /* thisbox = tempptr ; */
  gorder = glueorder ( thisbox ) ; 
  gsign = gluesign ( thisbox ) ; 
  p = listptr ( thisbox ) ; 
  incr ( curs ) ; 
  if ( curs > 0 ) 
    dviout ( push ) ; 
  if ( curs > maxpush ) 
    maxpush = curs ; 
  saveloc = dvioffset + dviptr ; 
  leftedge = curh ; 
  curv = curv - height ( thisbox ) ; 
  topedge = curv ; 
  while ( p != 0 ) {
    if ( ischarnode ( p ) ) 
      confusion("vlistout");
    else {
      switch ( ztype ( p ) ) 
      {case hlistnode : 
      case vlistnode : 
#ifdef TEXXET
      case R_node:
#endif
	if ( listptr ( p ) == 0 ) 
	  curv = curv + height ( p ) + depth ( p ) ; 
	else {
	  scaled saveh, savev;

	  curv = curv + height ( p ) ; 

	  synch_v;

	  saveh = dvih ; 
	  savev = dviv ;
#ifdef TEXXET
	  if( cur_dir == right_to_left )
	    curh = leftedge - shiftamount(p);
	  else
	    curh = leftedge + shiftamount(p);
#else
	  curh = leftedge + shiftamount(p);
#endif
	  /* tempptr = p ; */
	  if ( ztype ( p ) == vlistnode ) 
	    vlistout ( p ) ; 
	  else hlistout ( p ) ; 
	  dvih = saveh ; 
	  dviv = savev ; 
	  curv = savev + depth ( p ) ; 
	  curh = leftedge ; 
	}
	break ; 
      case rulenode :
	{
	  ruleht = height ( p ) ; 
	  /* ruledp = depth ( p ) ; */
	  ruleht += depth ( p );
	  rulewd = width ( p ) ; 
	  goto lab14 ; 
	} 
	break ; 
      case whatsitnode : 
	outwhat ( p ) ; 
	break ; 
      case gluenode : 
	{ { halfword g;

	  g = glueptr ( p ) ; 
	  ruleht = width ( g ) ; 
	  if ( gsign != normal ) {
	    if ( gsign == stretching ) {
	      if ( stretchorder ( g ) == gorder ) 
	      ruleht = ruleht + round ( glueset ( thisbox ) * stretch ( g ) );
	    } else {
	      if ( shrinkorder ( g ) == gorder ) 
	      ruleht = ruleht - round ( glueset ( thisbox ) * shrink ( g ) ) ; 
	    } 
	  }
	  }
	  if ( subtype ( p ) >= aleaders ) {
	    leaderbox = leaderptr ( p ) ; 
	    if ( ztype ( leaderbox ) == rulenode ) {
	      rulewd = width ( leaderbox ) ; 
	      /* ruledp = 0 ; */
	      goto lab14 ; 
	    } 
	    leaderht = height ( leaderbox ) + depth ( leaderbox ) ; 
	    if ( ( leaderht > 0 ) && ( ruleht > 0 ) ) {
	      scaled lx;
	      scaled saveh, savev;
	      boolean outerdoingleaders;

	      ruleht = ruleht + 10 ; 
	      edge = curv + ruleht ; 
	      lx = 0 ; 
	      if ( subtype ( p ) == aleaders ) {
		savev = curv ; 
		curv = topedge + leaderht * ( ( curv - topedge ) / leaderht );
		if ( curv < savev ) 
		curv = curv + leaderht ; 
	      } else {
		integer lq, lr;

		lq = ruleht / leaderht ; 
		lr = ruleht % leaderht ; 
		if ( subtype ( p ) == cleaders ) 
		  curv = curv + ( lr / 2 ) ; 
		else {
		  lx = ( 2 * lr + lq + 1 ) / ( 2 * lq + 2 ) ; 
		  curv = curv + ( ( lr - ( lq - 1 ) * lx ) / 2 ) ; 
		} 
	      } 
	      while ( curv + leaderht <= edge ) {
		curh = leftedge + shiftamount ( leaderbox ) ; 
		synch_h;  saveh = dvih ;
		curv = curv + height ( leaderbox ) ; 
		synch_v;  savev = dviv ; 
		/* tempptr = leaderbox ; */
		outerdoingleaders = doingleaders ; 
		doingleaders = true ; 
		if ( ztype ( leaderbox ) == vlistnode ) 
		  vlistout ( leaderbox ) ; 
		else hlistout ( leaderbox ) ; 
		doingleaders = outerdoingleaders ; 
		dviv = savev ; 
		dvih = saveh ; 
#if 0  /* 3.1415 */
		curh = saveh ; 
#else
		curh = leftedge ; 
#endif
		curv = savev - height ( leaderbox ) + leaderht + lx ; 
	      }
	      curv = edge - 10 ; 
	      goto lab15 ; 
	    }
	  } 
	  goto lab13 ; 
	} 
	break ; 
      case kernnode : 
	curv = curv + width ( p ) ; 
	break ; 
      default: 
	break ; 
      } 
      goto lab15 ; 

lab14:
      if ( isrunning ( rulewd ) ) 
	rulewd = width ( thisbox ) ; 
      /* ruleht = ruleht + ruledp ; */
      curv = curv + ruleht ; 
      if ( ( ruleht > 0 ) && ( rulewd > 0 ) ) {
#ifdef TEXXET
	if( cur_dir == right_to_left )
	  curh -= rulewd;
#endif

	synch_h;  synch_v;

	dviout ( putrule ) ; 
	dvifour ( ruleht ) ; 
	dvifour ( rulewd ) ; 
#ifdef TEXXET
	curh = leftedge;
#endif
      } 
      goto lab15 ;
lab13:
      curv = curv + ruleht ; 
    }
lab15:
    p = link ( p ) ; 
  } 
  prunemovements ( saveloc ) ; 
  if ( curs > 0 ) 
    dvipop ( saveloc ) ; 
  decr ( curs ) ; 
}


void shipout ( halfword p )
{/* 30 */ shipout_regmem 
  register integer pageloc  ; 
  register schar j, k  ; 
  register poolpointer s  ; 
  register integer oldsetting  ; 

  if ( tracingoutput > 0 ) {
    c_printnl("");
    println();
    c_print("Completed box being shipped out");
  } 
  if ( termoffset > maxprintline - 9 ) 
    println () ; 
  else if ( ( termoffset > 0 ) || ( fileoffset > 0 ) ) 
    printchar ( 32 ) ; 
  printchar ( 91 ) ; 
  j = 9 ; 
  while ( ( count ( j ) == 0 ) && ( j > 0 ) ) decr ( j ) ; 
  {register integer for_end; k = 0 ; for_end = j ; if ( k <= for_end) do 
    {
      printint ( count ( k ) ) ; 
      if ( k < j ) 
      printchar ( 46 ) ; 
    } 
  while ( k++ < for_end ) ; } 
  flush ( stdout ) ; 
  if ( tracingoutput > 0 ) {
    printchar ( 93 ) ; 
#ifdef MLTEX			/* no char_sub's defined print diag now */
    if( char_sub_def_max == 0 ) {	/* otherwise wait after shipbox */
#endif
      begindiagnostic () ; 
      showbox ( p ) ; 
      enddiagnostic ( true ) ;
#ifdef MLTEX
    }
#endif
  }

#ifdef TEXXET
  cur_dir = left_to_right; /* L-text at outer level */
  LR_ptr = getavail();  info(LR_ptr) = before; /* this will never match */
#endif

  if ( ( height ( p ) > maxdimen ) || ( depth ( p ) > maxdimen )
    || ( height ( p ) + depth ( p ) + voffset > maxdimen )
    || ( width ( p ) + hoffset > maxdimen ) )
  {
    print_err("Huge page cannot be shipped out");
    zhelp1( STR_H_THE_PAGE_JUST );
    error();
    if ( tracingoutput <= 0 ) {
      begindiagnostic () ; 
      c_printnl("The following box has been deleted:");
      showbox ( p ) ; 
      enddiagnostic ( true ) ; 
    } 
    goto lab30 ; 
  } 
  if ( height ( p ) + depth ( p ) + voffset > maxv ) 
    maxv = height ( p ) + depth ( p ) + voffset ; 
  if ( width ( p ) + hoffset > maxh ) 
    maxh = width ( p ) + hoffset ; 
  dvih = 0 ; 
  dviv = 0 ; 
  curh = hoffset ; 
  dvif = nullfont ; 
  if ( outputfilename == 0 ) {
    if ( jobname == 0 ) 
      openlogfile () ; 
    packjobname ( STR_DOT_DVI );
    while ( ! bopenout ( dvifile ) )
      promptfilename("file name for output", STR_DOT_DVI);
    outputfilename = bmakenamestring ( dvifile ) ; 
  } 
  if ( totalpages == 0 ) {
    dviout ( pre ) ; 
    dviout ( idbyte ) ; 
    dvifour ( 25400000L ) ; 
    dvifour ( 473628672L ) ; 
    preparemag();
    dvifour ( mag );

    oldsetting = selector;
    selector = newstring;
#if 1
    /* Fuer TripTest ist es ohne die Aenderung etwas einfacher, die dvi-files
     * zu vergleichen.
     */
    c_print(" TeX output ");
#else

#ifdef atarist
    c_print(" br-TeX output ");
#else
# ifdef AMIGA
    c_print(" Pas-TeX output ");
# else
    c_print(" TeX output ");
# endif
#endif

#endif
    printint ( zyear );
    printchar ( 46 ) ; 
    printtwo ( zmonth ) ; 
    printchar ( 46 ) ; 
    printtwo ( zday ) ; 
    printchar ( 58 ) ; 
    printtwo ( ztime / 60 ) ; 
    printtwo ( ztime % 60 ) ; 
    selector = oldsetting ; 

    dviout ( curlength ) ; 
    {register integer for_end; s = strstart [ strptr ] ; for_end = poolptr - 
    1 ; if ( s <= for_end) do 
      dviout ( strpool [ s ] ) ; 
    while ( s++ < for_end ) ; } 
    poolptr = strstart [ strptr ] ; 
  } 
  pageloc = dvioffset + dviptr ; 
  dviout ( bop ) ; 
  {register integer for_end; k = 0 ; for_end = 9 ; if ( k <= for_end) do 
    dvifour ( count ( k ) ) ; 
  while ( k++ < for_end ) ; } 
  dvifour ( lastbop ) ; 
  lastbop = pageloc ; 
  curv = height ( p ) + voffset ; 
  /* tempptr = p ; */
  if ( ztype ( p ) == vlistnode ) 
    vlistout ( p ) ; 
  else
    hlistout ( p ) ; 
  dviout ( eop ) ; 
  incr ( totalpages ) ; 
  curs = -1 ; 

lab30:
#ifdef TEXXET
  if( info(LR_ptr) != before )
    confusion("LR");

  freeavail(LR_ptr);  LR_ptr = 0;
#endif

#ifdef MLTEX		/* char_sub's not effected until ship out */
  if( tracingoutput > 0 && char_sub_def_max > 0 ) {
    begindiagnostic();
    showbox( p );
    enddiagnostic( true ); 
  }
#endif

  if ( tracingoutput <= 0 ) 
    printchar( 93 );
  deadcycles = 0;
  flush ( stdout );

  /* 639. */
#ifdef STAT
  if ( tracingstats > 1 ) {
    c_printnl("Memory usage before: ");
    printint( varused );  printchar( 38 );
    printint( dynused );  printchar( 59 );
  }
#endif /* STAT */
  flushnodelist ( p );

#ifdef STAT
  if ( tracingstats > 1 ) {
    c_print(" after: ");
    printint( varused );
    printchar( 38 );
    printint( dynused );
    c_print("; still untouched: ");
    printint( himemmin - lomemmax - 1 );
    println();
  }
#endif /* STAT */
}


void closefilesandterminate ( void )
{ closefilesandterminate_regmem 
  register integer k  ; 

  for( k = 0 ; k <= 15 ; k++ ) {
    if ( writeopen [ k ] )
      aclose ( writefile [ k ] ) ;
  }
#ifdef STAT
  if ( tracingstats > 0 ) 
  if ( logopened ) {
    (void) fprintf(logfile, "\n\
Here is how much of TeX's memory you used:\n\
 %ld string%s out of %ld\n\
 %ld string characters out of %ld\n\
 %ld words of memory out of %ld\n",
	(long)strptr - initstrptr,
	(strptr != initstrptr + 1) ? "s" : "",
	(long)maxstrings - initstrptr,

	(long)poolptr - initpoolptr,
	(long)poolsize - initpoolptr,

	(long)lomemmax - memmin + memend - himemmin + 2,
	(long)memend + 1 - memmin );

    (void) fprintf(logfile, "\
 %ld multiletter control sequences out of %ld\n\
 %ld words of font info for %ld font%s, out of %ld for %ld\n",
	(long)cscount,
	(long)hashsize,

	(long)fmemptr,
	(long)fontptr - 0,
	( fontptr != 1 ) ? "s" : "",
	(long)fontmemsize,
	(long)fontmax - 0 );

    (void) fprintf(logfile, "\
 %ld hyphenation exception%s out of %ld\n\
 %ldi,%ldn,%ldp,%ldb,%lds stack positions out of %ldi,%ldn,%ldp,%ldb,%lds\n",
	(long)hyphcount,
	( hyphcount != 1 ) ? "s" : "",
	(long)hyphsize,

	(long)maxinstack, (long)maxneststack, (long)maxparamstack,
	(long)maxbufstack + 1, (long)maxsavestack + 6,
	(long)stacksize, (long)nestsize, (long)paramsize,
	(long)bufsize, (long)savesize );
  }

#endif /* STAT */
  wakeupterminal () ; 
  while ( curs > -1 ) {
    if ( curs > 0 ) 
      dviout ( pop ) ; 
    else {
      dviout ( eop ) ; 
      incr ( totalpages ) ; 
    } 
    decr ( curs ) ; 
  } 
  if ( totalpages == 0 ) 
    c_printnl("No pages of output.");
  else {
    dviout ( post ) ; 
    dvifour ( lastbop ) ; 
    lastbop = dvioffset + dviptr - 5 ; 
    dvifour ( 25400000L ) ; 
    dvifour ( 473628672L ) ; 
    preparemag () ; 
    dvifour ( mag ) ; 
    dvifour ( maxv ) ; 
    dvifour ( maxh ) ; 
    dviout ( maxpush / 256 ) ; 
    dviout ( maxpush % 256 ) ;
    dviout ( (totalpages / 256) % 256 ) ;	/* TeX 3.14 2.update */
    dviout ( totalpages % 256 ) ; 
    while ( fontptr > 0 ) {
      if ( fontused(fontptr) ) 
	dvifontdef ( fontptr ) ; 
      decr ( fontptr ) ; 
    }
    dviout ( postpost ) ; 
    dvifour ( lastbop ) ; 
    dviout ( idbyte ) ; 
    k = 4 + ( ( dvibufsize - dviptr ) % 4 ) ; 
    while ( k > 0 ) {
      dviout ( 223 ) ; 
      decr ( k ) ; 
    }
    if ( dvilimit == halfbuf ) 
      writedvi ( halfbuf , dvibufsize - 1 ) ; 
    if ( dviptr > 0 ) 
      writedvi ( 0 , dviptr - 1 ) ; 
    c_printnl("Output written on ");
    slowprint ( outputfilename );  /* TeX 3.141 */
    c_print(" (");
    printint ( totalpages );
    c_print(" page");
    if ( totalpages != 1 )
      printchar ( 115 );
    c_print(", ");
    printint ( dvioffset + dviptr ) ; 
    c_print(" bytes).");
    bclose ( dvifile ) ; 
  }
  if ( logopened ) {
    (void) putc('\n',  logfile );
    aclose ( logfile ) ; 
    selector = selector - 2 ; 
    if ( selector == termonly ) {
      c_printnl("Transcript written on ");
      slowprint ( logname ); /* TeX 3.141 */
      printchar ( 46 );
    }
  }
  println();
  if ( ( editnamestart != 0 ) && ( interaction > batchmode ) )
    calledit ( strpool , editnamestart , editnamelength , editline );
}

/* -- end -- */
