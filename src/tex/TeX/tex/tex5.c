#define EXTERN extern
#include "texd.h"

#if 0			/* nach math.c */

void zmakefraction ( q ) 
scaled zmakeop ( q ) 
void zmakeord ( q ) 
void zmakescripts ( q , delta ) 
smallnumber zmakeleftright ( q , style , maxd , maxh ) 
void mlisttohlist ( ) 

#endif

void pushalignment ( void )
{ register halfword p  ; 

  p = getnode ( alignstacknodesize ) ; 

  { pushalignment_regmem

  link ( p ) = alignptr ; 
  info ( p ) = curalign ; 
  llink ( p ) = preamble ; 
  rlink ( p ) = curspan ; 
  mem [ p + 2 ] .cint = curloop ; 
  mem [ p + 3 ] .cint = alignstate ; 
  info ( p + 4 ) = curhead ; 
  link ( p + 4 ) = curtail ;
  }
  alignptr = p ; 
  curhead = getavail () ;
}

void popalignment ( void )
{ popalignment_regmem 
  register halfword p  ; 

  freeavail ( curhead ) ; 
  p = alignptr ; 
  curtail = link ( p + 4 ) ; 
  curhead = info ( p + 4 ) ; 
  alignstate = mem [ p + 3 ] .cint ; 
  curloop = mem [ p + 2 ] .cint ; 
  curspan = rlink ( p ) ; 
  preamble = llink ( p ) ; 
  curalign = info ( p ) ; 
  alignptr = link ( p ) ; 
  freenode ( p , alignstacknodesize ) ; 
}

void getpreambletoken ( void )
{ getpreambletoken_regmem 
  register eightbits r_curcmd;

lab20:
  r_curcmd = gettoken () ; 
  while ( (r_curcmd == 4) && (curchr == spancode) ) {
    r_curcmd = gettoken ();
    if ( r_curcmd > maxcommand ) {
      expand ();
      r_curcmd = gettoken ();
    }
  }

  if ( r_curcmd == 9 )
    fatalerror( STR_H_FE_INTERWOVEN );

  if ( (r_curcmd == assign_glue) && (curchr == gluebase + tabskipcode) ) {
    scanoptionalequals ();
    { register long_halfword r_curval;

      r_curval = scanglue( glueval );
      if ( globaldefs > 0 )
	geqdefine( gluebase + tabskipcode, glueref, r_curval );
      else
	eqdefine( gluebase + tabskipcode, glueref, r_curval );
    }
    goto lab20;
  }
}

void initalign ( void )
{/* 30 31 32 22 */ initalign_regmem 
  register halfword savecsptr  ; 
  register halfword p  ; 

  savecsptr = curcs ; 
  pushalignment () ; 
  alignstate = -1000000L ; 
  if ( ( curlist .modefield == mmode )
  && ( ( curlist .tailfield != curlist .headfield )
    || ( curlist .auxfield .cint != 0 ) ) )
  {
    p_print_err( STR_H_IMPROPER );
    printesc ( 516 ) ; 
    c_print(" inside $$'s");
    zhelp2( STR_H_DISPLAYSCANUSE, STR_H_SOIVE_FORMULAS );
    error () ; 
    flushmath () ; 
  } 
  pushnest () ; 
  if ( curlist .modefield == mmode ) {
    curlist .modefield = -vmode;
    curlist .auxfield .cint = nest [ nestptr - 2 ] .auxfield .cint ; 
  } else if ( curlist .modefield > 0 ) 
    curlist .modefield = - curlist .modefield ; 
  scanspec ( aligngroup , false ) ; 
  preamble = 0 ; 
  curalign = alignhead ; 
  curloop = 0 ; 
  scannerstatus = aligning ; 
  warningindex = savecsptr ; 
  alignstate = -1000000L ; 
  while ( true ) {
    link ( curalign ) = newparamglue ( tabskipcode ) ; 
    curalign = link ( curalign ) ; 
    if ( curcmd == 5 ) 
      goto lab30 ; 
    p = holdhead ; 
    link ( p ) = 0 ; 
    while ( true ) {
      getpreambletoken () ; 
      if ( curcmd == 6 ) 
	goto lab31 ; 
      if ( ( curcmd <= 5 ) && ( curcmd >= 4 ) && ( alignstate == -1000000L ) ) 
      if ( ( p == holdhead ) && ( curloop == 0 ) && ( curcmd == 4 ) ) 
	curloop = curalign ; 
      else {
	print_err("Missing # inserted in alignment preamble");
	zhelp2( STR_H_THERESHOULD_TAB, STR_H_NONESOIVEPUT );
	backerror () ; 
	goto lab31 ; 
      } else if ( ( curcmd != 10 ) || ( p != holdhead ) ) {
	link ( p ) = getavail () ; 
	p = link ( p ) ; 
	info ( p ) = curtok ; 
      }
    }
lab31: ; 
    link ( curalign ) = newnullbox () ; 
    curalign = link ( curalign ) ; 
    info ( curalign ) = endspan ; 
    width ( curalign ) = nullflag ; 
    upart ( curalign ) = link ( holdhead ) ; 
    p = holdhead ; 
    link ( p ) = 0 ; 
    while ( true ) {
lab22:
      getpreambletoken () ; 
      if ( ( curcmd <= 5 ) && ( curcmd >= 4 ) && ( alignstate == -1000000L ) ) 
        goto lab32 ; 
      if ( curcmd == 6 ) {
	print_err("Only one # is allowed per tab");
	zhelp2( STR_H_THERESHOULD_TAB, STR_H_MORETHANONE_SOIM );
	error () ; 
	goto lab22 ; 
      }
      link ( p ) = getavail () ; 
      p = link ( p ) ; 
      info ( p ) = curtok ; 
    }
lab32:
    link ( p ) = getavail () ; 
    p = link ( p ) ; 
    info ( p ) = endtemplatetoken ; 
    vpart ( curalign ) = link ( holdhead ) ; 
  }
lab30:
  scannerstatus = normal ; 
  newsavelevel ( aligngroup ) ; 
  if ( everycr != 0 ) 
    begintokenlist ( everycr , everycrtext ) ; 
  alignpeek () ; 
}

void initspan ( halfword p )
{ initspan_regmem 

  pushnest () ; 
  if ( curlist .modefield == -hmode )
    curlist .auxfield .hh .v.LH = 1000 ; 
  else {
    curlist .auxfield .cint = ignoredepth ; 
    normalparagraph () ; 
  } 
  curspan = p ; 
} 

void initrow ( void )
{ initrow_regmem 

  pushnest () ; 
  curlist.modefield = - ((hmode + vmode) + curlist.modefield);
  if ( curlist .modefield == -hmode )
    curlist .auxfield .hh .v.LH = 0 ; 
  else
    curlist .auxfield .cint = 0 ; 
  tailappend ( newglue ( glueptr ( preamble ) ) ) ; 
  subtype ( curlist .tailfield ) = tabskipcode + 1 ; 
  curalign = link ( preamble ) ; 
  curtail = curhead ; 
  initspan ( curalign ) ; 
}

void initcol ( void )
{ initcol_regmem 

  extrainfo ( curalign ) = curcmd ; 
  if ( curcmd == 63 ) 
    alignstate = 0 ; 
  else {
    backinput () ; 
    begintokenlist ( upart ( curalign ) , utemplate ) ; 
  } 
}

boolean fincol ( void )
{/* 10 */ fincol_regmem 
  register halfword p  ; 
  register halfword q, r  ; 
  register halfword s  ; 
  register halfword u  ; 
  register scaled w  ; 
  register glueord o  ; 
  register halfword n  ; 

  if ( curalign == 0 ) 
    confusion("endv"); /* 902 */
  q = link ( curalign ) ; 
  if ( q == 0 )
    confusion("endv"); /* 902 */
  if ( alignstate < 500000L ) 
    fatalerror( STR_H_FE_INTERWOVEN );
  p = link ( q ) ; 
  if ( ( p == 0 ) && ( extrainfo ( curalign ) < crcode ) ) 
  if ( curloop != 0 ) {
    link ( q ) = newnullbox () ; 
    p = link ( q ) ; 
    info ( p ) = endspan ; 
    width ( p ) = nullflag ; 
    curloop = link ( curloop ) ; 
    q = holdhead ; 
    r = upart ( curloop ) ; 
    while ( r != 0 ) {
      link ( q ) = getavail () ; 
      q = link ( q ) ; 
      info ( q ) = info ( r ) ; 
      r = link ( r ) ; 
    } 
    link ( q ) = 0 ; 
    upart ( p ) = link ( holdhead ) ; 
    q = holdhead ; 
    r = vpart ( curloop ) ; 
    while ( r != 0 ) {
      link ( q ) = getavail () ; 
      q = link ( q ) ; 
      info ( q ) = info ( r ) ; 
      r = link ( r ) ; 
    } 
    link ( q ) = 0 ; 
    vpart ( p ) = link ( holdhead ) ; 
    curloop = link ( curloop ) ; 
    link ( p ) = newglue ( glueptr ( curloop ) ) ; 
  } else {
    print_err("Extra alignment tab has been changed to ");
    printesc( STR_CR );
    zhelp1( STR_H_YOUHAVE_SPANORTAB );
    extrainfo ( curalign ) = crcode ; 
    error () ; 
  }
  if ( extrainfo ( curalign ) != spancode ) {
    unsave () ; 
    newsavelevel ( aligngroup ) ; 
    {
      if ( curlist .modefield == -hmode ) {
	adjusttail = curtail ; 
	u = hpack ( link ( curlist .headfield ) , 0 , 1 ) ; 
	w = width ( u ) ; 
	curtail = adjusttail ; 
	adjusttail = 0 ; 
      } else {
	u = vpackage ( link ( curlist .headfield ) , 0 , 1 , 0 ) ; 
	w = height ( u ) ; 
      } 
      n = 0 ; 
      if ( curspan != curalign ) {
	q = curspan ; 
	do {
	  incr ( n ) ; 
	  q = link ( link ( q ) ) ; 
	} while ( ! ( q == curalign ) ) ; 
	if ( n > maxquarterword ) 
	  confusion("256 spans"); /* 907 */
	q = curspan ; 
	while ( link ( info ( q ) ) < n ) q = info ( q ) ; 
	if ( link ( info ( q ) ) > n ) {
	  s = getnode ( spannodesize ) ; 
	  info ( s ) = info ( q ) ; 
	  link ( s ) = n ; 
	  info ( q ) = s ; 
	  width ( s ) = w ; 
	}
	else if ( width ( info ( q ) ) < w ) 
	width ( info ( q ) ) = w ; 
      }
      else if ( w > width ( curalign ) ) 
      width ( curalign ) = w ; 
      ztype ( u ) = unsetnode ; 
      spancount ( u ) = n ; 
      if ( totalstretch [ filll ] != 0 ) 
	o = filll ; 
      else if ( totalstretch [ fill ] != 0 ) 
	o = fill ; 
      else if ( totalstretch [ fil ] != 0 ) 
	o = fil ; 
      else
	o = normal ; 
      glueorder ( u ) = o ; 
      gluestretch ( u ) = totalstretch [ o ] ; 
      if ( totalshrink [ filll ] != 0 ) 
	o = filll ; 
      else if ( totalshrink [ fill ] != 0 ) 
	o = fill ; 
      else if ( totalshrink [ fil ] != 0 ) 
	o = fil ; 
      else
	o = normal ; 
      gluesign ( u ) = o ; 
      glueshrink ( u ) = totalshrink [ o ] ; 
      popnest () ; 
      link ( curlist .tailfield ) = u ; 
      curlist .tailfield = u ; 
    }
    tailappend ( newglue ( glueptr ( link ( curalign ) ) ) ) ; 
    subtype ( curlist .tailfield ) = tabskipcode + 1 ; 
    if ( extrainfo ( curalign ) >= crcode ) {
      return(true) ; 
    }
    initspan ( p ) ; 
  }
  alignstate = 1000000L ;
#if 0
  do {
    getxtoken () ; 
  } while ( ! ( curcmd != 10 ) ) ; 
#else
  getxnbtoken(0);
#endif
  curalign = p ; 
  initcol () ; 

  return(false);
}

void finrow ( void )
{ finrow_regmem 
  register halfword p  ; 

  if ( curlist .modefield == -hmode ) {
    p = hpack ( link ( curlist .headfield ) , 0 , 1 ) ; 
    popnest () ; 
    appendtovlist ( p ) ; 
    if ( curhead != curtail ) {
      link ( curlist .tailfield ) = link ( curhead ) ; 
      curlist .tailfield = curtail ; 
    } 
  } else {
    p = vpackage ( link ( curlist .headfield ) , 0 , 1 , maxdimen ) ; 
    popnest () ; 
    link ( curlist .tailfield ) = p ; 
    curlist .tailfield = p ; 
    curlist .auxfield .hh .v.LH = 1000 ; 
  } 
  ztype ( p ) = unsetnode ; 
  gluestretch ( p ) = 0 ; 
  if ( everycr != 0 )
    begintokenlist ( everycr , everycrtext ) ; 
  alignpeek () ; 
}

void finalign ( void )
{ finalign_regmem 
  register halfword p, q, r, s, u, v  ; 
  register scaled t, w  ; 
  register scaled o  ; 
  register halfword n  ; 
  register scaled rulesave  ; 
  memoryword auxsave  ; 

  if ( curgroup != aligngroup ) 
    confusion("align1"); /* 908 */
  unsave () ; 
  if ( curgroup != aligngroup ) 
    confusion("align2"); /* 909 */
  unsave () ; 
  if ( nest [ nestptr - 1 ] .modefield == mmode ) 
    o = displayindent ; 
  else o = 0 ; 
  q = link ( preamble ) ; 
  do {
    flushlist ( upart ( q ) ) ; 
    flushlist ( vpart ( q ) ) ; 
    p = link ( link ( q ) ) ; 
    if ( width ( q ) == nullflag ) {
      width ( q ) = 0 ; 
      r = link ( q ) ; 
      s = glueptr ( r ) ; 
      if ( s != zeroglue ) {
	addglueref ( zeroglue ) ; 
	deleteglueref ( s ) ; 
	glueptr ( r ) = zeroglue ; 
      } 
    }
    if ( info ( q ) != endspan ) {
      t = width ( q ) + width ( glueptr ( link ( q ) ) ) ; 
      r = info ( q ) ; 
      s = endspan ; 
      info ( s ) = p ; 
      n = 1 ; 
      do {
	width ( r ) = width ( r ) - t ; 
	u = info ( r ) ; 
	while ( link ( r ) > n ) {
	  s = info ( s ) ; 
	  n = link ( info ( s ) ) + 1 ; 
	} 
	if ( link ( r ) < n ) {
	  info ( r ) = info ( s ) ; 
	  info ( s ) = r ; 
	  decr ( link ( r ) ) ; 
	  s = r ; 
	} else {
	  if ( width ( r ) > width ( info ( s ) ) ) 
	    width ( info ( s ) ) = width ( r ) ; 
	  freenode ( r , spannodesize ) ; 
	}
	r = u ; 
      } while ( ! ( r == endspan ) ) ; 
    }
    ztype ( q ) = unsetnode ; 
    spancount ( q ) = 0 ; 
    height ( q ) = 0 ; 
    depth ( q ) = 0 ; 
    glueorder ( q ) = normal ; 
    gluesign ( q ) = normal ; 
    gluestretch ( q ) = 0 ; 
    glueshrink ( q ) = 0 ; 
    q = p ; 
  } while ( ! ( q == 0 ) ) ; 
  saveptr = saveptr - 2 ; 
  packbeginline = - (integer) curlist .mlfield ; 
  if ( curlist .modefield == -vmode ) {
    rulesave = overfullrule ; 
    overfullrule = 0 ; 
    p = hpack ( preamble , saved ( 1 ) , saved ( 0 ) ) ; 
    overfullrule = rulesave ; 
  } else {
    q = link ( preamble ) ; 
    do {
      height ( q ) = width ( q ) ; 
      width ( q ) = 0 ; 
      q = link ( link ( q ) ) ; 
    } while ( ! ( q == 0 ) ) ; 
    p = vpackage ( preamble , saved ( 1 ) , saved ( 0 ) , maxdimen ) ; 
    q = link ( preamble ) ; 
    do {
      width ( q ) = height ( q ) ; 
      height ( q ) = 0 ; 
      q = link ( link ( q ) ) ; 
    } while ( ! ( q == 0 ) ) ; 
  }
  packbeginline = 0 ; 
  q = link ( curlist .headfield ) ; 
  s = curlist .headfield ; 
  while ( q != 0 ) {
    if ( ! ischarnode ( q ) ) 
    if ( ztype ( q ) == unsetnode ) {
      if ( curlist .modefield == -vmode ) {
	ztype ( q ) = hlistnode ; 
	width ( q ) = width ( p ) ; 
      } else {
	ztype ( q ) = vlistnode ; 
	height ( q ) = height ( p ) ; 
      } 
      glueorder ( q ) = glueorder ( p ) ; 
      gluesign ( q ) = gluesign ( p ) ; 
      glueset ( q ) = glueset ( p ) ; 
      shiftamount ( q ) = o ; 
      r = link ( listptr ( q ) ) ; 
      s = link ( listptr ( p ) ) ; 
      do {
	n = spancount ( r ) ; 
	t = width ( s ) ; 
	w = t ; 
	u = holdhead ; 
	while ( n > 0 ) {
	  decr ( n ) ; 
	  s = link ( s ) ; 
	  v = glueptr ( s ) ; 
	  link ( u ) = newglue ( v ) ; 
	  u = link ( u ) ; 
	  subtype ( u ) = tabskipcode + 1 ; 
	  t = t + width ( v ) ; 
	  if ( gluesign ( p ) == stretching ) {
	    if ( stretchorder ( v ) == glueorder ( p ) ) 
	    t = t + round ( glueset ( p ) * stretch ( v ) ) ; 
	  } else if ( gluesign ( p ) == shrinking ) {
	    if ( shrinkorder ( v ) == glueorder ( p ) ) 
	    t = t - round ( glueset ( p ) * shrink ( v ) ) ; 
	  }
	  s = link ( s ) ; 
	  link ( u ) = newnullbox () ; 
	  u = link ( u ) ; 
	  t = t + width ( s ) ; 
	  if ( curlist .modefield == -vmode ) 
	    width ( u ) = width ( s ) ; 
	  else {
	    ztype ( u ) = vlistnode ; 
	    height ( u ) = width ( s ) ; 
	  } 
	}
	if ( curlist .modefield == -vmode ) {
	  height ( r ) = height ( q ) ; 
	  depth ( r ) = depth ( q ) ; 
	  if ( t == width ( r ) ) {
	    gluesign ( r ) = normal ; 
	    glueorder ( r ) = normal ; 
	    glueset ( r ) = 0.0 ; 
	  } else if ( t > width ( r ) ) {
	    gluesign ( r ) = stretching ; 
	    if ( gluestretch ( r ) == 0 ) 
	    glueset ( r ) = 0.0 ; 
	    else glueset ( r ) = ( t - width ( r ) ) / ((double) gluestretch ( 
	    r ) ) ; 
	  } else {
	    glueorder ( r ) = gluesign ( r ) ; 
	    gluesign ( r ) = shrinking ; 
	    if ( glueshrink ( r ) == 0 ) 
	    glueset ( r ) = 0.0 ; 
	    else if ( ( glueorder ( r ) == normal ) && ( width ( r ) - t > 
	    glueshrink ( r ) ) ) 
	    glueset ( r ) = 1.0 ; 
	    else glueset ( r ) = ( width ( r ) - t ) / ((double) glueshrink ( 
	    r ) ) ; 
	  } 
	  width ( r ) = w ; 
	  ztype ( r ) = hlistnode ; 
	} else {
	  width ( r ) = width ( q ) ; 
	  if ( t == height ( r ) ) {
	    gluesign ( r ) = normal ; 
	    glueorder ( r ) = normal ; 
	    glueset ( r ) = 0.0 ; 
	  } else if ( t > height ( r ) ) {
	    gluesign ( r ) = stretching ; 
	    if ( gluestretch ( r ) == 0 ) 
	    glueset ( r ) = 0.0 ; 
	    else glueset ( r ) = ( t - height ( r ) ) / ((double) gluestretch 
	    ( r ) ) ; 
	  } else {
	    glueorder ( r ) = gluesign ( r ) ; 
	    gluesign ( r ) = shrinking ; 
	    if ( glueshrink ( r ) == 0 ) 
	    glueset ( r ) = 0.0 ; 
	    else if ( ( glueorder ( r ) == normal ) && ( height ( r ) - t > 
	    glueshrink ( r ) ) ) 
	    glueset ( r ) = 1.0 ; 
	    else glueset ( r ) = ( height ( r ) - t ) / ((double) glueshrink ( 
	    r ) ) ; 
	  } 
	  height ( r ) = w ; 
	  ztype ( r ) = vlistnode ; 
	}
	shiftamount ( r ) = 0 ; 
	if ( u != holdhead ) {
	  link ( u ) = link ( r ) ; 
	  link ( r ) = link ( holdhead ) ; 
	  r = u ; 
	} 
	r = link ( link ( r ) ) ; 
	s = link ( link ( s ) ) ; 
      } while ( ! ( r == 0 ) ) ; 
    } else if ( ztype ( q ) == rulenode ) {
      if ( isrunning ( width ( q ) ) ) 
      width ( q ) = width ( p ) ; 
      if ( isrunning ( height ( q ) ) ) 
      height ( q ) = height ( p ) ; 
      if ( isrunning ( depth ( q ) ) ) 
      depth ( q ) = depth ( p ) ; 
      if ( o != 0 ) {
	r = link ( q ) ; 
	link ( q ) = 0 ; 
	q = hpack ( q , 0 , 1 ) ; 
	shiftamount ( q ) = o ; 
	link ( q ) = r ; 
	link ( s ) = q ; 
      } 
    }
    s = q ; 
    q = link ( q ) ; 
  }
  flushnodelist ( p ) ; 
  popalignment () ; 
  auxsave = curlist .auxfield ; 
  p = link ( curlist .headfield ) ; 
  q = curlist .tailfield ; 
  popnest () ; 
  if ( curlist .modefield == mmode ) {
    doassignments () ; 
    if ( curcmd != 3 ) {
      print_err("Missing $$ inserted");
      zhelp1( STR_H_DISPLAYSCANUSE );
      backerror () ; 
    } else {
      check_that_dollar_follows();
    }
    popnest () ; 
    tailappend ( newpenalty ( predisplaypenalty ) ) ; 
    tailappend ( newparamglue ( abovedisplayskipcode ) ) ; 
    link ( curlist .tailfield ) = p ; 
    if ( p != 0 ) 
      curlist .tailfield = q ; 
    tailappend ( newpenalty ( postdisplaypenalty ) ) ; 
    tailappend ( newparamglue ( belowdisplayskipcode ) ) ; 
    curlist .auxfield .cint = auxsave .cint ; 
    resumeafterdisplay () ; 

  } else {

    curlist .auxfield = auxsave ; 
    link ( curlist .tailfield ) = p ; 
    if ( p != 0 ) 
      curlist .tailfield = q;
    if ( curlist .modefield == vmode ) 
      buildpage ();
  }
}

void alignpeek ( void )
{ alignpeek_regmem 
  register eightbits r_curcmd;

lab20:
  alignstate = 1000000L ;
#if 0
  do {
    r_curcmd = getxtoken () ; 
  } while ( r_curcmd == 10 );
#else
  r_curcmd = getxnbtoken(0);
#endif
  if ( r_curcmd == 34 ) {
    scanleftbrace () ; 
    newsavelevel ( noaligngroup ) ; 
    if ( curlist .modefield == -vmode ) 
      normalparagraph () ; 
  } else if ( r_curcmd == 2 ) 
    finalign () ; 
  else if ( ( r_curcmd == 5 ) && ( curchr == crcrcode ) ) 
    goto lab20 ; 
  else {
    initrow () ; 
    initcol () ; 
  } 
}

long_halfword finiteshrink ( halfword p )
{ finiteshrink_regmem 

  if ( noshrinkerroryet ) {
    noshrinkerroryet = false ; 
    print_err("Infinite glue shrinkage found in a paragraph");
    zhelp2( STR_H_THEPARAGRAPHJUST, STR_H_SINCETHE_OFFENSIVE );
    error();
  }
  { register long_halfword q;

  q = newspec ( p ) ; 
  shrinkorder ( q ) = normal ; 
  deleteglueref ( p ) ; 

  return(q) ;
  }
}

#if 0		/* in linebrk.c */

void ztrybreak ( pi , breaktype ) 

#endif
