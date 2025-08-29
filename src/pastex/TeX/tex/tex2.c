#define EXTERN extern
#include "texd.h"

/* preparemag() nach tex7.c */

void tokenshow ( halfword p )
{ tokenshow_regmem 

  if ( p != 0 ) 
    showtokenlist ( link ( p ) , 0 , 10000000L ) ; 
}

void printmeaning ( void )
{ printmeaning_regmem 

  printcmdchr ( curcmd , curchr ) ; 
  if ( curcmd >= call ) {
    printchar ( 58 ) ; 
    println () ; 
    tokenshow ( curchr ) ; 
  } else if ( curcmd == topbotmark ) {
    printchar ( 58 ) ; 
    println () ; 
    tokenshow ( curmark [ curchr ] ) ; 
  } 
}


void showcurcmdchr ( void )
{ showcurcmdchr_regmem 

  begindiagnostic () ; 
  printnl ( 123 ) ; 
  if ( curlist .modefield != shownmode ) {
    printmode ( curlist .modefield ) ; 
    c_print(": "); /* 564 */
    shownmode = curlist .modefield ; 
  }
  printcmdchr ( curcmd , curchr ) ; 
  printchar ( 125 ) ; 
  enddiagnostic ( false ) ; 
}


void showcontext ( void )
{/* 30 */ showcontext_regmem 
  register integer oldsetting  ; 
  register integer nn  ; 
  boolean bottomline  ; 
  register integer i  ; 
  register integer j  ; 
  register integer l  ; 
  register integer m  ; 
  register integer n  ; 
  register integer p  ; 
  register integer q  ; 

  baseptr = inputptr ; 
#ifndef INP_PTR
  inputstack [ baseptr ] = curinput ; 
#endif
  nn = -1 ; 
  bottomline = false ; 
  while ( true ) {
#ifdef INP_PTR
    curinput_ptr = &inputstack [ baseptr ] ; 
#else
    curinput = inputstack [ baseptr ] ;
#endif
    if ( ( curinput .statefield != tokenlist ) ) 
      if ( ( curinput .namefield > 17 ) || ( baseptr == 0 ) ) 
	bottomline = true ; 
    if ( baseptr == inputptr || bottomline || nn < errorcontextlines ) {
      if ( ( baseptr == inputptr ) || ( curinput .statefield != tokenlist )
	   || ( tokentype != backedup ) || ( curinput .locfield != 0 ) ) {
	tally = 0 ; 
	oldsetting = selector ; 
	if ( curinput .statefield != tokenlist ) {
	  if ( curinput .namefield <= 17 )
	    if ( ( curinput .namefield == 0 ) ) 
	      if ( baseptr == 0 ) 
		c_printnl("<*>");
	      else
		c_printnl("<insert> ");
	    else {
	      c_printnl("<read ");
	      if ( curinput .namefield == 17 ) 
		printchar ( 42 ) ; 
	      else
		printint ( curinput .namefield - 1 ) ; 
	      printchar ( 62 ) ; 
	    }
	  else {
	    c_printnl("l.");
	    printint ( line ) ; 
	  } 
	  printchar ( 32 ) ; 
	  {
	    l = tally ; 
	    tally = 0 ; 
	    selector = pseudo ; 
	    trickcount = 1000000L ; 
	  } 
	  if ( buffer [ curinput .limitfield ] == endlinechar ) 
	    j = curinput .limitfield ; 
	  else
	    j = curinput .limitfield + 1 ; 
	  if ( j > 0 ) 
	    for ( i = curinput .startfield ; i <= j - 1 ; i++ ) {
	      if ( i == curinput .locfield ) {
		firstcount = tally ; 
		trickcount = tally + 1 + errorline - halferrorline ; 
		if ( trickcount < errorline ) 
		trickcount = errorline ; 
	      }
	      print ( buffer [ i ] ) ; 
	    }
	} else {
	  switch ( tokentype ) {
	  case parameter : 
	    c_printnl("<argument> ");
	    break ; 
	  case utemplate : 
	  case vtemplate : 
	    c_printnl("<template> ");
	    break ; 
	  case backedup : 
	    if ( curinput .locfield == 0 ) 
	      c_printnl("<recently read> ");
	    else
	      c_printnl("<to be read again> ");
	    break ; 
	  case inserted : 
	    c_printnl("<inserted text> ");
	    break ; 
	  case macro : 
	    println () ; 
	    printcs ( curinput .namefield ) ; 
	    break ; 
	  case outputtext : 
	    c_printnl("<output> ");
	    break ; 
	  case everypartext : 
	    c_printnl("<everypar> ");
	    break ; 
	  case everymathtext : 
	    c_printnl("<everymath> ");
	    break ; 
	  case everydisplaytext : 
	    c_printnl("<everydisplay> ");
	    break ; 
	  case everyhboxtext : 
	    c_printnl("<everyhbox> ");
	    break ; 
	  case everyvboxtext : 
	    c_printnl("<everyvbox> ");
	    break ; 
	  case everyjobtext : 
	    c_printnl("<everyjob> ");
	    break ; 
	  case everycrtext : 
	    c_printnl("<everycr> ");
	    break ; 
	  case marktext : 
	    c_printnl("<mark> ");
	    break ; 
	  case writetext : 
	    c_printnl("<write> ");
	    break ; 
	  default: 
	    printnl ( 63 );
	    break ; 
	  }
	  {
	    l = tally ; 
	    tally = 0 ; 
	    selector = pseudo ; 
	    trickcount = 1000000L ; 
	  }
	  if ( tokentype < macro ) 
	    showtokenlist (curinput .startfield, curinput .locfield, 100000L);
	  else
	    showtokenlist ( link ( curinput .startfield ),
				curinput .locfield, 100000L );
	}
	selector = oldsetting ; 
	if ( trickcount == 1000000L ) {
	  firstcount = tally ; 
	  trickcount = tally + 1 + errorline - halferrorline ; 
	  if ( trickcount < errorline ) 
	    trickcount = errorline ;
	}
	m = ( (tally < trickcount) ? tally : trickcount ) - firstcount;
	if ( l + firstcount <= halferrorline ) {
	  p = 0 ; 
	  n = l + firstcount ; 
	} else {
	  print ( 275 ) ; 
	  p = l + firstcount - halferrorline + 3 ; 
	  n = halferrorline ; 
	} 
	for( q = p ; q <= firstcount - 1 ; q++ ) {
	  printchar ( trickbuf [ q % errorline ] ) ;
	}
	println () ; 
	for( q = n ; q >= 1 ; --q ) {
	  printchar ( 32 ) ;
	}
	if ( m + n <= errorline ) 
	  p = firstcount + m ; 
	else
	  p = firstcount + ( errorline - n - 3 ) ; 
	for( q = firstcount ; q <= p - 1 ; q++ ) {
	  printchar ( trickbuf [ q % errorline ] ) ;
	}
	if ( m + n > errorline ) 
	  print ( 275 ) ;
	incr ( nn ) ; 
      }
    } else if ( nn == errorcontextlines ) {
      printnl ( 275 ) ; 
      incr ( nn ) ; 
    }
    if ( bottomline ) 
      goto lab30 ; 
    decr ( baseptr ) ; 
  }
lab30:
#ifdef INP_PTR
  curinput_ptr = &inputstack [ inputptr ] ; 
#else
  curinput = inputstack [ inputptr ] ; 
#endif
}


void begintokenlist ( halfword p, quarterword t )
{ begintokenlist_regmem

  {
    if ( inputptr > maxinstack ) {
      maxinstack = inputptr ; 
      if ( inputptr == stacksize )		/* hier evtl. noch aendern */
        overflow(8, stacksize);
    }
#ifdef INP_PTR
    incr ( inputptr ) ;
    curinput_ptr++;
#else
    inputstack [ inputptr ] = curinput ; 
    incr ( inputptr ) ; 
#endif
  } 

  {
#ifndef INP_PTR
    register instaterecord *curinputPTR = &curinput;
#   define curinput (*curinputPTR)
#endif

  curinput .statefield = tokenlist ; 
  tokentype = t ; 
  curinput .startfield = p ; 
  if ( t >= macro ) {
    addtokenref ( p ) ; 
    if ( t == macro ) 
      paramstart = paramptr ; 
    else {
      curinput .locfield = link ( p ) ; 
      if ( tracingmacros > 1 ) {
	begindiagnostic () ; 
	c_printnl("");
	switch ( t ) {
	case marktext : 
	  printesc ( 348 ) ; 
	  break ; 
	case writetext : 
	  printesc ( STR_WRITE );
	  break ; 
	default: 
	  printcmdchr( assign_toks, t - outputtext + outputroutineloc );
	  break ; 
	}
	c_print("->");
	tokenshow ( p ) ; 
	enddiagnostic ( false ) ; 
      } 
    }
  } else
    curinput .locfield = p ;

#ifndef INP_PTR
# undef curinput
#endif
  }
}


void endtokenlist ( void )
{ endtokenlist_regmem 

#ifndef INP_PTR
  register instaterecord *curinputPTR = &curinput;
# define curinput (*curinputPTR)
#endif

  if ( tokentype >= backedup ) {
    if ( tokentype <= inserted ) 
      flushlist ( curinput.startfield ) ; 
    else {
      deletetokenref ( curinput.startfield ) ; 
      if ( tokentype == macro ) {
#if 0 /* def PARAM_PTR */
	long anzahl = paramptr - paramstart;
	halfword *psp = &paramstack[paramptr];

	while ( anzahl-- > 0 ) {
	  flushlist( *psp );
	  psp--;
	}
	paramptr = paramstart;
#else
	while ( paramptr > paramstart ) {
	  decr ( paramptr ) ; 
	  flushlist ( paramstack [ paramptr ] ) ; 
	} 
#endif
      }
    }
  } else if ( tokentype == utemplate ) 
    if ( alignstate > 500000L ) 
      alignstate = 0 ; 
    else fatalerror( STR_H_FE_INTERWOVEN );

  {
    decr ( inputptr ) ; 
#ifdef INP_PTR
    --curinput_ptr;
#else
    curinput = inputstack [ inputptr ] ; 
#endif
  } 
  {
    if ( interrupt != 0 ) 
    pauseforinstructions () ; 
  }
#ifndef INP_PTR
# undef curinput
#endif
}


void backinput ( void )
{
#ifndef INP_PTR
  register instaterecord *curinputPTR = &curinput;
# define curinput (*curinputPTR)
#endif

  while ( curinput .statefield == tokenlist && curinput .locfield == 0 )
    endtokenlist();

  { register long_halfword p;

  p = getavail();

  { backinput_regmem 
    register halfword r_curtok = curtok;

  info ( p ) = r_curtok ; 
  if ( r_curtok < rightbracelimit )
    if ( r_curtok < leftbracelimit )
      decr ( alignstate ) ;
    else
      incr ( alignstate ) ;
  }

  { register integer r_inputptr = inputptr;

    if ( r_inputptr > maxinstack ) {
      maxinstack = r_inputptr ; 
      if ( r_inputptr == stacksize ) 
        overflow(8, stacksize); 
    }
#ifdef INP_PTR
    incr ( r_inputptr ) ;
    curinput_ptr++;
#else
    inputstack [ r_inputptr ] = curinput ; 
    incr ( r_inputptr ) ;
#endif

    inputptr = r_inputptr;
  }
  curinput .statefield = tokenlist ; 
  tokentype = backedup ; 
  curinput .startfield = p ; 
  curinput .locfield = p ; 
  }
#ifndef INP_PTR
# undef curinput
#endif
}


void backerror ( void )
{ backerror_regmem 

  OKtointerrupt = false ; 
  backinput () ; 
  OKtointerrupt = true ; 
  error () ; 
} 


void inserror ( void )
{ inserror_regmem

  OKtointerrupt = false ; 
  backinput () ; 
  tokentype = inserted ; 
  OKtointerrupt = true ; 
  error () ; 
} 


void beginfilereading ( void )
{ beginfilereading_regmem 

  if ( inopen == maxinopen ) 
    overflow(9, maxinopen);
  if ( first == bufsize ) 
    overflow(1, bufsize);
  incr ( inopen ) ; 
  {
    if ( inputptr > maxinstack ) {
      maxinstack = inputptr ; 
      if ( inputptr == stacksize ) 
	overflow(8, stacksize);
    } 
#ifdef INP_PTR
    incr ( inputptr ) ; 
    curinput_ptr++;
    curinput.locfield = (*(curinput_ptr-1)).locfield;
    curinput.limitfield = (*(curinput_ptr-1)).limitfield;
#else
    inputstack [ inputptr ] = curinput ; 
    incr ( inputptr ) ; 
#endif
  } 
  curinput .indexfield = inopen ; 
  linestack [ curinput .indexfield ] = line ; 
  curinput .startfield = first ; 
  curinput .statefield = 1 ; 
  curinput .namefield = 0 ; 
}


void endfilereading ( void )
{ endfilereading_regmem 

  first = curinput .startfield ; 
  line = linestack [ curinput .indexfield ] ; 
  if ( curinput .namefield > 17 ) 
    aclose ( inputfile [ curinput .indexfield ] ) ; 
  {
    decr ( inputptr ) ; 
#ifdef INP_PTR
    --curinput_ptr;
#else
    curinput = inputstack [ inputptr ] ; 
#endif
  } 
  decr ( inopen ) ; 
}


void clearforerrorprompt ( void )
{ clearforerrorprompt_regmem 

  while ( ( curinput .statefield != tokenlist )
	&& ( curinput .namefield == 0 )
	&& ( inputptr > 0 )
	&& ( curinput .locfield > curinput .limitfield ) )
    endfilereading () ;
  println () ; 
  clearterminal () ; 
} 


void checkoutervalidity ( void )
{ checkoutervalidity_regmem 

  if ( scannerstatus != normal ) {
    deletionsallowed = false ; 
    if ( curcs != 0 ) {
      if ( ( curinput .statefield == tokenlist )
	    || ( curinput .namefield < 1 )
	    || ( curinput .namefield > 17 ) ) {
	register long_halfword p;

	p = getavail();
	info ( p ) = cstokenflag + curcs;
	begintokenlist( p, backedup );
      }
      curcmd = 10 ; 
      curchr = 32 ; 
    }
    if ( scannerstatus > skipping ) {
      runaway () ; 
      if ( curcs == 0 ) {
	print_err("File ended");
      } else {
	curcs = 0 ; 
	print_err("Forbidden control sequence found");
      }
      c_print(" while scanning ");

     { register halfword p;

      p = getavail () ; 
      switch ( scannerstatus ) {
      case defining : 
	{
	  c_print("definition");
	  info ( p ) = rightbracetoken + 125 ; 
	} 
	break ; 
      case matching : 
	{
	  c_print("use");
	  info ( p ) = partoken ; 
	  longstate = outercall ; 
	} 
	break ; 
      case aligning : 
	{ register halfword q;

	  c_print("preamble");
	  info ( p ) = rightbracetoken + 125 ; 
	  q = p ; 
	  p = getavail () ; 
	  link ( p ) = q ; 
	  info ( p ) = cstokenflag + frozencr ; 
	  alignstate = -1000000L ; 
	}
	break ; 
      case absorbing :
	{
	  c_print("text");
	  info ( p ) = rightbracetoken + 125;
	}
	break;
      }
      begintokenlist( p, inserted);
     }
      c_print(" of ");
      sprintcs( warningindex );
      zhelp1( STR_H_ISUSPECT_FORGOTTEN );
      error();
    } else {
      print_err("Incomplete ");
      printcmdchr( iftest, curif );
      c_print("; all text was ignored after line ");
      printint ( skipline );
      if ( curcs != 0 ) {
	zhelp2( STR_H_AFORBIDDEN_SKIPPED, STR_H_THISKIND_HAPPENS_IF );
	curcs = 0;
      } else {
	zhelp2( STR_H_THEFILEENDEDWHILE, STR_H_THISKIND_HAPPENS_IF );
      }
      curtok = cstokenflag + frozenfi;
      inserror();
    }
    deletionsallowed = true;
  }
}


void firmuptheline ( void )
{ firmuptheline_regmem 
  register integer k  ;

#if 0			/* (br) deleted */
  curinput .limitfield = last ; 
  if ( pausing > 0 )
#endif
  if ( interaction > nonstopmode ) {
    wakeupterminal () ; 
    println () ; 
    if ( curinput .startfield < curinput .limitfield ) 
    {register integer for_end; k = curinput .startfield ; for_end = curinput 
    .limitfield - 1 ; if ( k <= for_end) do 
      print ( buffer [ k ] ) ; 
    while ( k++ < for_end ) ; } 
    first = curinput .limitfield;
    terminput("=>");
    if ( last > first ) {
      {register integer for_end; k = first ; for_end = last - 1 ; if ( k <= 
      for_end) do 
	buffer [ k + curinput .startfield - first ] = buffer [ k ] ; 
      while ( k++ < for_end ) ; } 
      curinput .limitfield = curinput .startfield + last - first ; 
    }
  }
}



void macrocall ( void )
{ macrocall_regmem 
  register halfword r;
  halfword p;
  register halfword s;
  halfword rbraceptr; 
  register smallnumber n;
  register halfword unbalance;
  register halfword m;
  halfword refcount;
  smallnumber savescannerstatus;
  halfword savewarningindex;
  ASCIIcode matchchr;

  savescannerstatus = scannerstatus ; 
  savewarningindex = warningindex ; 
  warningindex = curcs ; 
  refcount = curchr;
  r = link ( refcount ) ; 
  n = 0 ; 
  if ( tracingmacros > 0 ) {
    begindiagnostic () ; 
    println () ; 
    printcs ( warningindex ) ; 
    tokenshow ( refcount ) ; 
    enddiagnostic ( false ) ; 
  } 
  if ( info ( r ) != endmatchtoken ) {
    scannerstatus = matching ; 
    unbalance = 0 ; 
    longstate = eqtype ( curcs ) ; 
    if ( longstate >= outercall ) 
      longstate = longstate - 2 ; 
    do {
      link ( temphead ) = 0 ; 
      if ( ( info ( r ) > matchtoken + 255 ) || ( info ( r ) < matchtoken ) ) 
        s = 0 ; 
      else {
	matchchr = info ( r ) - matchtoken ; 
	s = link ( r ) ; 
	r = s ; 
	p = temphead ; 
	m = 0 ; 
      }
lab22: gettoken () ;
      { register halfword r_curtok = curtok;

	if ( r_curtok == info ( r ) ) {
	  r = link ( r ) ; 
	  if ( ( info(r) >= matchtoken ) && ( info(r) <= endmatchtoken ) ) {
	    if ( r_curtok < leftbracelimit ) 
	      decr ( alignstate ) ; 
	    goto lab40 ; 
	  } else
	    goto lab22 ; 
	}
	if ( s != r ) 
	if ( s == 0 ) {
	  print_err("Use of ");
	  sprintcs ( warningindex ) ; 
	  c_print(" doesn't match its definition");
	  zhelp1( STR_H_IFYOUSAY_DEFA );
	  error();
	  goto lab10 ; 
	} else {
	  register halfword t;

	  t = s ; 
	  do {
	    register halfword u, v;

	    storenewtoken( info ( t ) );
	    incr ( m ) ; 
	    u = link ( t ) ; 
	    v = s;
	    while ( true ) {
	      if ( u == r ) 
	      if ( r_curtok != info ( v ) ) 
	        goto lab30 ; 
	      else {
	        r = link ( v ) ; 
	        goto lab22 ; 
	      }
	      if ( info ( u ) != info ( v ) ) 
	        goto lab30 ; 
	      u = link ( u ) ; 
	      v = link ( v ) ; 
	    }
lab30:      t = link ( t ) ; 
	  } while ( ! ( t == r ) ) ; 
	  r = s ; 
	}

	if ( r_curtok == partoken ) 
	if ( longstate != longcall ) {
	  if ( longstate == call ) {
	    runaway () ; 
	    print_err("Paragraph ended before ");
	    sprintcs ( warningindex ) ; 
	    c_print(" was complete");
	    zhelp1( STR_H_ISUSPECT_CAUSING );
	    backerror () ; 
	  }
	  pstack [ n ] = link ( temphead ) ; 
	  alignstate = alignstate - unbalance ; 
	  for( m = 0 ; m <= n ; m++ ) {
	    flushlist ( pstack [ m ] ) ;
	  }
	  goto lab10 ; 
	}

	if ( r_curtok < rightbracelimit ) 

	if ( r_curtok < leftbracelimit ) {
	  /* register halfword r_curtok = curtok; */

	  unbalance = 1 ; 
	  while ( true ) {
	    faststorenewtoken ( r_curtok );
	    gettoken () ;
	    r_curtok = curtok;
	    if ( r_curtok == partoken ) 
	    if ( longstate != longcall ) {
	      if ( longstate == call ) {
		runaway () ; 
		print_err("Paragraph ended before ");
		sprintcs ( warningindex ) ; 
		c_print(" was complete");
		zhelp1( STR_H_ISUSPECT_CAUSING );
		backerror () ; 
	      }
	      pstack [ n ] = link ( temphead ) ; 
	      alignstate = alignstate - unbalance ; 
	      for( m = 0 ; m <= n ; m++ ) {
		flushlist ( pstack [ m ] ) ;
	      }
	      goto lab10 ; 
	    }
	    if ( r_curtok < rightbracelimit ) 
	    if ( r_curtok < leftbracelimit ) 
	      incr ( unbalance ) ; 
	    else {
	      decr ( unbalance ) ; 
	      if ( unbalance == 0 ) 
		goto lab31 ; 
	    }
	  }
lab31:	  rbraceptr = p ;
	  storenewtoken ( r_curtok );

	} else {
	  
	  backinput () ; 
	  print_err("Argument of ");
	  sprintcs ( warningindex ) ; 
	  c_print(" has an extra }");
	  zhelp1( STR_H_IVERUNACROSS );
	  incr ( alignstate ) ; 
	  longstate = call ; 
	  curtok = partoken ; 
	  inserror () ; 
        }

        else {		/* (curtok >= rightbracelimit) */

	  if ( r_curtok == spacetoken ) 
	  if ( info ( r ) <= endmatchtoken &&  info ( r ) >= matchtoken )
	    goto lab22 ; 

	  storenewtoken ( r_curtok );
	}
      }

      incr ( m ) ; 
      if ( info ( r ) > endmatchtoken ) 
        goto lab22 ; 
      if ( info ( r ) < matchtoken ) 
        goto lab22 ;
lab40:
      if ( s != 0 ) {
	if ( (m == 1) && ( info(p) < rightbracelimit ) && ( p != temphead ) ) {
	  link ( rbraceptr ) = 0 ; 
	  freeavail ( p ) ; 
	  p = link ( temphead ) ; 
	  pstack [ n ] = link ( p ) ; 
	  freeavail ( p ) ; 
	} else
	  pstack [ n ] = link ( temphead ) ; 
	incr ( n ) ; 
	if ( tracingmacros > 0 ) {
	  begindiagnostic () ; 
	  printnl ( matchchr ) ; 
	  printint ( n ) ; 
	  c_print("<-");
	  showtokenlist ( pstack [ n - 1 ] , 0 , 1000 ) ; 
	  enddiagnostic ( false ) ; 
	}
      }
    } while ( ! ( info ( r ) == endmatchtoken ) ) ; 
  }

  while ( ( curinput.statefield == tokenlist ) && ( curinput.locfield == 0 ) )
    endtokenlist ();

  begintokenlist ( refcount , macro ) ; 
  curinput .namefield = warningindex ; 
  curinput .locfield = link ( r ) ; 
  if ( n > 0 ) {
    if ( paramptr + n > maxparamstack ) {
      maxparamstack = paramptr + n ; 
      if ( maxparamstack > paramsize ) 
      overflow(10, paramsize);
    }
    for( m = 0 ; m <= n - 1 ; m++ ) {
      paramstack [ paramptr + m ] = pstack [ m ] ;
    }
    paramptr += n ;
  } 
lab10:
  scannerstatus = savescannerstatus ; 
  warningindex = savewarningindex ; 
}


void insertrelax ( void )
{ insertrelax_regmem 

  curtok = cstokenflag + curcs ; 
  backinput () ; 
  curtok = cstokenflag + frozenrelax ; 
  backinput () ; 
  tokentype = inserted ; 
}


void expand ( void )
{ expand_regmem 
  integer cvbackup  ; 
  smallnumber cvlbackup, radixbackup, cobackup  ; 
  halfword backupbackup  ; 

  cvbackup = curval;
  cvlbackup = curvallevel;
  radixbackup = radix ; 
  cobackup = curorder ; 
  backupbackup = link ( backuphead ) ; 
  if ( curcmd < call ) {
    if ( tracingcommands > 1 ) 
      showcurcmdchr () ; 
    switch ( curcmd ) {
    case topbotmark : 
      if ( curmark [ curchr ] != 0 ) 
	begintokenlist ( curmark [ curchr ] , marktext ) ; 
      break ; 
    case expandafter : 
      { register halfword t;

	gettoken () ; 
	t = curtok ; 
	if( (halfword)gettoken() > maxcommand )
	  expand();
	else
	  backinput();
	curtok = t ; 
	backinput();
      } 
      break ; 
    case noexpand : 
      { register halfword t;
	register smallnumber savescannerstatus;

	savescannerstatus = scannerstatus ; 
	scannerstatus = normal ; 
	gettoken () ; 
	scannerstatus = savescannerstatus ; 
	t = curtok ; 
	backinput () ; 
	if ( t >= cstokenflag ) {
	  register halfword p;

	  p = getavail () ; 
	  info ( p ) = cstokenflag + frozendontexpand ; 
	  link ( p ) = curinput .locfield ; 
	  curinput .startfield = p ; 
	  curinput .locfield = p ; 
	}
      }
      break ; 
    case csname : 
      { register halfword p, r;
	register integer j;

	r = getavail () ; 
	p = r ; 
	do {
	  getxtoken () ; 
	  if ( curcs == 0 )
	    storenewtoken ( curtok );
	} while( curcs == 0 );

	if ( curcmd != 67 ) {
	  print_err("Missing ");
	  printesc( STR_ENDCSNAME );
	  c_print(" inserted");
	  zhelp1( STR_H_THECSMARKED );
	  backerror();
	}
	j = first ; 
	p = link ( r ) ; 
	while ( p != 0 ) {
	  if ( j >= maxbufstack ) {
	    maxbufstack = j + 1 ; 
	    if ( maxbufstack == bufsize ) 
	      overflow(1, bufsize);
	  } 
	  buffer [ j ] = info ( p ) % 256 ; 
	  incr ( j ) ; 
	  p = link ( p ) ; 
	} 
	if ( j > first + 1 ) {
	  nonewcontrolsequence = false ; 
	  curcs = idlookup ( first , j - first ) ; 
	  nonewcontrolsequence = true ; 
	} else if ( j == first ) 
	  curcs = nullcs ; 
	else
	  curcs = singlebase + buffer [ first ] ; 
	flushlist ( r ) ; 
	if ( eqtype ( curcs ) == undefinedcs ) {
	  eqdefine ( curcs , 0 , 256 ) ; 
	} 
	curtok = curcs + cstokenflag ; 
	backinput () ; 
      } 
      break ; 
    case convert : 
      convtoks () ; 
      break ; 
    case the : 
      insthetoks () ; 
      break ; 
    case iftest : 
      conditional () ; 
      break ; 
    case fiorelse : 
      if ( curchr > iflimit ) 
      if ( iflimit == ifcode ) 
	insertrelax () ; 
      else {
	print_err("Extra ");
	printcmdchr ( fiorelse , curchr ) ; 
	zhelp1( STR_H_IMIGNORING_IF );
	error () ; 
      } else {
	while ( curchr != ficode )
	  passtext () ;

	{ register halfword p;

	  p = condptr ; 
	  ifline = iflinefield ( p ) ; 
	  curif = subtype ( p ) ; 
	  iflimit = ztype ( p ) ; 
	  condptr = link ( p ) ; 
	  freenode ( p , ifnodesize ) ; 
	}
      }
      break ; 
    case input : 
      if ( curchr > 0 ) 
	forceeof = true ; 
      else if ( nameinprogress ) 
	insertrelax () ; 
      else startinput () ; 
      break ; 
    default: 
      {
	print_err("Undefined control sequence");
	zhelp1( STR_H_THECSATTHEEND );
	error();
      }
      break;
    }
  } else if ( curcmd < endtemplate ) 
    macrocall () ; 
  else {
    curtok = cstokenflag + frozenendv ; 
    backinput () ; 
  } 
  curval = cvbackup;
  curvallevel = cvlbackup;
  radix = radixbackup ; 
  curorder = cobackup ; 
  link ( backuphead ) = backupbackup ; 
}


/* -- end -- */
