#define EXTERN extern
#include "texd.h"


void showbox ( halfword p )
{ showbox_regmem

  depththreshold = showboxdepth;
  breadthmax = showboxbreadth;
  if ( breadthmax <= 0 )
    breadthmax = 5;
  if ( poolptr + depththreshold >= poolsize )
    depththreshold = poolsize - poolptr - 1;
  shownodelist( p );
  println();
} 


void deletetokenref ( halfword p )
{ deletetokenref_regmem

  if ( tokenrefcount ( p ) == 0 )
    flushlist( p );
  else
    decr( tokenrefcount(p) );
}


void deleteglueref ( halfword p )
{ deleteglueref_regmem

  fastdeleteglueref( p );
}


void flushnodelist ( halfword p )
{ flushnodelist_regmem
  register halfword q;

  while ( p != 0 ) {
    q = link ( p );
    if ( ischarnode ( p ) ) {
      register halfword tmp_avail = avail;

      do {
	/* freeavail ( p ) ; */
	link(p) = tmp_avail;  tmp_avail = p;
#ifdef STAT
	decr(dynused);
#endif
	p = q;
        if( p == 0 ) {
	  avail = tmp_avail;
	  return;
	}
        q = link( p );
      } while( ischarnode(p) );

      avail = tmp_avail;
      /* continue; */ /* we know: p != null && !ischarnode(p) */
    }

    switch ( ztype ( p ) ) {
      case hlistnode :
      case vlistnode :
      case unsetnode :
	flushnodelist ( listptr ( p ) );
	freenode( p, boxnodesize );
	goto lab30;
	break;
      case rulenode : 
	freenode( p, rulenodesize );
	goto lab30;
	break;
      case insnode :
	{
	  flushnodelist ( insptr ( p ) ) ; 
	  deleteglueref ( splittopptr ( p ) ) ; 
	  freenode ( p , insnodesize ) ; 
	  goto lab30 ; 
	} 
	break ; 
      case whatsitnode : 
	{
	  switch ( subtype ( p ) ) {
	  case opennode : 
	    freenode ( p , opennodesize ) ; 
	    break ; 
	  case writenode : 
	  case specialnode : 
	    {
	      deletetokenref ( writetokens ( p ) ) ; 
	      freenode ( p , writenodesize ) ; 
	      goto lab30 ; 
	    } 
	    break ; 
	  case closenode : 
	  case languagenode : 
	    freenode ( p, smallnodesize );
	    break;
	  default: 
	    confusion("ext3");
	    break;
	  }
	  goto lab30;
	}
	break;
      case gluenode :
	{
	  fastdeleteglueref ( glueptr ( p ) ) ; 
	  if ( leaderptr ( p ) != 0 ) 
	    flushnodelist ( leaderptr ( p ) ) ; 
	} 
	break ; 
      case kernnode : 
      case mathnode : 
      case penaltynode : 
	break ; 
      case ligaturenode : 
	flushnodelist ( ligptr ( p ) ) ; 
	break ; 
      case marknode : 
	deletetokenref ( markptr ( p ) ) ; 
	break ; 
      case discnode : 
	flushnodelist ( prebreak ( p ) ) ; 
	flushnodelist ( postbreak ( p ) ) ; 
	break ; 
      case adjustnode : 
	flushnodelist ( adjustptr ( p ) ) ; 
	break ; 
      case stylenode : 
	{
	  freenode ( p , stylenodesize ) ; 
	  goto lab30 ; 
	} 
	break ; 
      case choicenode : 
	{
	  flushnodelist ( displaymlist ( p ) ) ; 
	  flushnodelist ( textmlist ( p ) ) ; 
	  flushnodelist ( scriptmlist ( p ) ) ; 
	  flushnodelist ( scriptscriptmlist ( p ) ) ; 
	  freenode ( p , stylenodesize ) ; 
	  goto lab30 ; 
	} 
	break ; 
      case ordnoad : 
      case opnoad : 
      case binnoad : 
      case relnoad : 
      case opennoad : 
      case closenoad : 
      case punctnoad : 
      case innernoad : 
      case radicalnoad : 
      case overnoad : 
      case undernoad : 
      case vcenternoad : 
      case accentnoad : 
	if ( mathtype ( nucleus ( p ) ) >= subbox ) 
	  flushnodelist ( info ( nucleus ( p ) ) ) ; 
	if ( mathtype ( supscr ( p ) ) >= subbox ) 
	  flushnodelist ( info ( supscr ( p ) ) ) ; 
	if ( mathtype ( subscr ( p ) ) >= subbox ) 
	  flushnodelist ( info ( subscr ( p ) ) ) ; 
	if ( ztype ( p ) == radicalnoad ) 
	  freenode ( p , radicalnoadsize ) ; 
	else if ( ztype ( p ) == accentnoad ) 
	  freenode ( p , accentnoadsize ) ; 
	else
	  freenode ( p , noadsize );
	goto lab30;
	break;
      case leftnoad : 
      case rightnoad : 
	{
	  freenode ( p , noadsize ) ; 
	  goto lab30 ; 
	} 
	break ; 
      case fractionnoad : 
	{
	  flushnodelist ( info ( numerator ( p ) ) ) ; 
	  flushnodelist ( info ( denominator ( p ) ) ) ; 
	  freenode ( p , fractionnoadsize ) ; 
	  goto lab30 ; 
	} 
	break ; 
      default: 
	confusion("flushing"); /* 350 */
	break ; 
    }
    freenode( p, smallnodesize );
lab30:
    p = q;
  }
}


halfword copynodelist ( halfword p )
{ copynodelist_regmem 
  register halfword q ; 
  register long_halfword r ; 
  /*register*/ long_halfword h; /* head of copied list, not used in loop! */

  h = q = getavail ();
  /* q = h; */
  while ( p != 0 ) {
    if ( ischarnode ( p ) ) {
      r = getavail () ;
      /* we know:  words = 1 for this simple and often occuring case */
      mem[r] = mem[p];
    } else {
      register schar words = 1;
      /* half of remaining cases are gluenodes with length smallnodesize,
         but only one word must be copied (see 202. flushnodelist) */

      switch ( ztype ( p ) ) {
      case hlistnode : 
      case vlistnode : 
      case unsetnode : 
	r = getnode ( boxnodesize ) ; 
	mem [ r + 6 ] = mem [ p + 6 ] ; 
	mem [ r + 5 ] = mem [ p + 5 ] ; 
	listptr ( r ) = copynodelist ( listptr ( p ) ) ; 
	words = 5 ; 
	break ; 
      case rulenode : 
	r = getnode ( rulenodesize ) ; 
	words = rulenodesize ; 
	break ; 
      case insnode : 
	r = getnode ( insnodesize ) ; 
	mem [ r + 4 ] = mem [ p + 4 ] ; 
	addglueref ( splittopptr ( p ) ) ; 
	insptr ( r ) = copynodelist ( insptr ( p ) ) ; 
	words = insnodesize - 1 ; 
	break ; 
      case whatsitnode : 
	switch ( subtype ( p ) ) {
	case opennode : 
		r = getnode ( opennodesize ) ; 
		words = opennodesize ; 
		break ; 
	case writenode : 
	case specialnode : 
		r = getnode ( writenodesize ) ; 
		addtokenref ( writetokens ( p ) ) ; 
		words = writenodesize ; 
		break ; 
	case closenode : 
	case languagenode : 
		r = getnode ( smallnodesize ) ; 
		words = smallnodesize ; 
		break ; 
	default: 
		confusion("ext2");
		break ; 
	}
	break ; 
      case gluenode : 
	r = getnode ( smallnodesize ) ; 
	addglueref ( glueptr ( p ) ) ; 
	glueptr ( r ) = glueptr ( p ) ; 
	leaderptr ( r ) = copynodelist ( leaderptr ( p ) ) ; 
	break ; 
      case kernnode : 
      case mathnode : 
      case penaltynode : 
	r = getnode ( smallnodesize ) ; 
	words = smallnodesize ; 
	break ; 
      case ligaturenode : 
	r = getnode ( smallnodesize ) ; 
	mem [ ligchar ( r ) ] = mem [ ligchar ( p ) ] ; 
	ligptr ( r ) = copynodelist ( ligptr ( p ) ) ; 
	break ; 
      case discnode : 
	r = getnode ( smallnodesize ) ; 
	prebreak ( r ) = copynodelist ( prebreak ( p ) ) ; 
	postbreak ( r ) = copynodelist ( postbreak ( p ) ) ; 
	break ; 
      case marknode : 
	r = getnode ( smallnodesize ) ; 
	addtokenref ( markptr ( p ) ) ; 
	words = smallnodesize ; 
	break ; 
      case adjustnode : 
	r = getnode ( smallnodesize ) ; 
	adjustptr ( r ) = copynodelist ( adjustptr ( p ) ) ; 
	break ; 
      default:
	confusion("copying"); /* 351 */ /* --> jump_out() --> exit() */
	break ; 
      }
      while ( words > 0 ) {
	decr ( words ) ; 
	mem [ r + words ] = mem [ p + words ] ; 
      }
    }
    link ( q ) = r;
    q = r;
    p = link ( p );
  }
  link( q ) = 0;
  q = link( h );
  freeavail( h );

  return(q);
}


void printmode ( integer m )
{ printmode_regmem

#ifdef OLD_MODE
  if ( m > 0 )
  switch ( m / (maxcommand + 1) ) {
  case 0 : 
    c_print("vertical");
    break ; 
  case 1 : 
    c_print("horizontal");
    break ;
  case 2 :
    c_print("display math");
    break ; 
  } else if ( m == 0 ) 
    c_print("no");
  else switch ( (-(integer)m) / (maxcommand + 1) ) {
  case 0 :
    c_print("internal vertical");
    break ;
  case 1 : 
    c_print("restricted horizontal");
    break ; 
  case 2 : 
    c_print("math");
    break ; 
  }
#else
  char *s;

  if ( m > 0 ) {
    switch ( m ) {
    case vmode:  s = "vertical"; break;
    case hmode:  s = "horizontal"; break;
    case mmode:  s = "display math"; break;
    }
  } else if ( m == 0 ) {
    s = "no";
  } else {
    switch ( -m ) {
    case vmode:  s = "internal vertical"; break;
    case hmode:  s = "restricted horizontal"; break;
    case mmode:  s = "math"; break;
    }
  }
  c_print(s);
#endif
  c_print(" mode");
}


void pushnest ( void )
{ pushnest_regmem 

  if ( nestptr > maxneststack ) {
    maxneststack = nestptr;
    if ( nestptr == nestsize )
      overflow(4, nestsize);
  } 
  nest[nestptr] = curlist;
  incr( nestptr );
  curlist .headfield = getavail();
  curlist .tailfield = curlist .headfield;
  curlist .pgfield = 0;
  curlist .mlfield = line;
#ifdef TEXXET
  LR_save = 0;
#endif
}


void popnest ( void )
{ popnest_regmem

  freeavail( curlist .headfield );
  decr ( nestptr );
  curlist = nest[nestptr];
}


void showactivities ( void )
{ showactivities_regmem 
  register integer p;
  register short m;
  memoryword a;
  register halfword q, r;
  register integer t;

  nest [ nestptr ] = curlist;
  c_printnl("");
  println();
  for( p = nestptr ; p >= 0 ; --p ) {
    m = nest [ p ] .modefield ; 
    a = nest [ p ] .auxfield ; 
    c_printnl("### ");
    printmode ( m ) ; 
    c_print(" entered at line ");
    printint ( abs ( nest [ p ] .mlfield ) ) ; 
    if ( m == hmode )
#if 0  /* 3.1415 */
      if ( ( nest [ p ] .lhmfield != 2 ) || ( nest [ p ] .rhmfield != 3 ) ) {
	c_print(" (hyphenmin ");
	printint ( nest [ p ] .lhmfield ) ; 
	printchar ( 44 ) ; 
	printint ( nest [ p ] .rhmfield ) ; 
	printchar ( 41 ) ; 
      } 
#else
      if ( nest [ p ] .pgfield != 0x830000L ) {
	c_print(" (language");
	printint ( ( nest [ p ] .pgfield % 0x010000L ) ) ;
	c_print(":hyphenmin");
	printint ( ( nest [ p ] .pgfield / 0x400000L ) ) ;
	printchar ( 44 ) ; 
	printint ( ( ( nest [ p ] .pgfield / 0x010000L ) % 0x40 ) ) ;
	printchar ( 41 ) ; 
      }
#endif
    if ( nest [ p ] .mlfield < 0 ) 
      c_print(" (\\output routine)");
    if ( p == 0 ) {
	if ( pagehead != pagetail ) {
	  c_printnl("### current page:");
	  if ( outputactive ) 
	  c_print(" (held over for next output)");
	  showbox ( link ( pagehead ) ) ; 
	  if ( pagecontents > 0 ) {
	    c_printnl("total height ");
	    printtotals () ; 
	    c_printnl(" goal height ");
	    printscaled ( pagesofar [ 0 ] ) ; 
	    r = link ( pageinshead ) ; 
	    while ( r != pageinshead ) {
	      println () ; 
	      printesc( STR_INSERT );
	      t = subtype ( r ) ; 
	      printint ( t ) ; 
	      c_print(" adds ");
	      t = xovern ( height ( r ) , 1000 ) * count ( t ) ; 
	      printscaled ( t ) ; 
	      if ( ztype ( r ) == splitup ) {
		q = pagehead ; 
		t = 0 ; 
		do {
		  q = link ( q ) ; 
		  if ( ( ztype ( q ) == insnode ) && ( subtype ( q ) == 
		  subtype ( r ) ) ) 
		  incr ( t ) ; 
		} while ( ! ( q == brokenins ( r ) ) ) ; 
		c_print(", #");
		printint ( t ) ; 
		c_print(" might split");
	      } 
	      r = link ( r ) ; 
	    } 
	  } 
	} 
	if ( link ( contribhead ) != 0 ) 
	  c_printnl("### recent contributions:");
    }
    showbox ( link ( nest [ p ] .headfield ) ) ; 
#ifdef OLD_MODE
    switch ( abs ( m ) / (maxcommand + 1) )
#else
    switch ( abs ( m ) )
#endif
      {
#ifdef OLD_MODE
      case 0 :
#else
      case vmode:
#endif
	{
	  c_printnl("prevdepth ");
	  if ( a .cint <= ignoredepth ) 
	    c_print("ignored");
	  else
	    printscaled ( a .cint ) ; 
	  if ( nest [ p ] .pgfield != 0 ) {
	    c_print(", prevgraf ");
	    printint ( nest [ p ] .pgfield ) ; 
	    c_print(" line");
	    if ( nest [ p ] .pgfield != 1 ) 
	      printchar ( 115 ) ; 
	  }
	}
	break ;
#ifdef OLD_MODE
      case 1 :
#else
      case hmode:
#endif
	{
	  c_printnl("spacefactor ");
	  printint ( a.hh.v.LH ) ; 
	  if ( m > 0 && a.hh.v.RH > 0 ) {
	    c_print(", current language ");
	    printint ( a.hh.v.RH );
	  }
	}
	break ; 
#ifdef OLD_MODE
      case 2:
#else
      case mmode:
#endif
	if ( a .cint != 0 ) {
	  c_print("this will be denominator of:");
	  showbox ( a .cint ) ; 
	} 
	break ; 
      }
  }
}


void printparam ( integer n )
{ printparam_regmem 

  switch ( n ) {
  case pretolerancecode : 
    printesc ( 416 ) ; 
    break ; 
  case tolerancecode : 
    printesc ( 417 ) ; 
    break ; 
  case linepenaltycode : 
    printesc ( 418 ) ; 
    break ; 
  case hyphenpenaltycode : 
    printesc ( 419 ) ; 
    break ; 
  case exhyphenpenaltycode : 
    printesc ( 420 ) ; 
    break ; 
  case clubpenaltycode : 
    printesc ( 421 ) ; 
    break ; 
  case widowpenaltycode : 
    printesc ( 422 ) ; 
    break ; 
  case displaywidowpenaltycode : 
    printesc ( 423 ) ; 
    break ; 
  case brokenpenaltycode : 
    printesc ( 424 ) ; 
    break ; 
  case binoppenaltycode : 
    printesc ( 425 ) ; 
    break ; 
  case relpenaltycode : 
    printesc ( 426 ) ; 
    break ; 
  case predisplaypenaltycode : 
    printesc ( 427 ) ; 
    break ; 
  case postdisplaypenaltycode : 
    printesc ( 428 ) ; 
    break ; 
  case interlinepenaltycode : 
    printesc ( 429 ) ; 
    break ; 
  case doublehyphendemeritscode : 
    printesc ( 430 ) ; 
    break ; 
  case finalhyphendemeritscode : 
    printesc ( 431 ) ; 
    break ; 
  case adjdemeritscode : 
    printesc ( 432 ) ; 
    break ; 
  case magcode : 
    printesc ( 433 ) ; 
    break ; 
  case delimiterfactorcode : 
    printesc ( 434 ) ; 
    break ; 
  case loosenesscode : 
    printesc ( 435 ) ; 
    break ; 
  case timecode : 
    printesc ( 436 ) ; 
    break ; 
  case daycode : 
    printesc ( 437 ) ; 
    break ; 
  case monthcode : 
    printesc ( 438 ) ; 
    break ; 
  case yearcode : 
    printesc ( 439 ) ; 
    break ; 
  case showboxbreadthcode : 
    printesc ( 440 ) ; 
    break ; 
  case showboxdepthcode : 
    printesc ( 441 ) ; 
    break ; 
  case hbadnesscode : 
    printesc ( 442 ) ; 
    break ; 
  case vbadnesscode : 
    printesc ( 443 ) ; 
    break ; 
  case pausingcode : 
    printesc ( 444 ) ; 
    break ; 
  case tracingonlinecode : 
    printesc ( 445 ) ; 
    break ; 
  case tracingmacroscode : 
    printesc ( 446 ) ; 
    break ; 
  case tracingstatscode : 
    printesc ( 447 ) ; 
    break ; 
  case tracingparagraphscode : 
    printesc ( 448 ) ; 
    break ; 
  case tracingpagescode : 
    printesc ( 449 ) ; 
    break ; 
  case tracingoutputcode : 
    printesc ( 450 ) ; 
    break ; 
  case tracinglostcharscode : 
    printesc ( 451 ) ; 
    break ; 
  case tracingcommandscode : 
    printesc ( 452 ) ; 
    break ; 
  case tracingrestorescode : 
    printesc ( 453 ) ; 
    break ; 
  case uchyphcode : 
    printesc ( 454 ) ; 
    break ; 
  case outputpenaltycode : 
    printesc ( 455 ) ; 
    break ; 
  case maxdeadcyclescode : 
    printesc ( 456 ) ; 
    break ; 
  case hangaftercode : 
    printesc ( 457 ) ; 
    break ; 
  case floatingpenaltycode : 
    printesc ( 458 ) ; 
    break ; 
  case globaldefscode : 
    printesc ( 459 ) ; 
    break ; 
  case curfamcode : 
    printesc ( 460 ) ; 
    break ; 
  case escapecharcode : 
    printesc ( 461 ) ; 
    break ; 
  case defaulthyphencharcode : 
    printesc ( 462 ) ; 
    break ; 
  case defaultskewcharcode : 
    printesc ( 463 ) ; 
    break ; 
  case endlinecharcode : 
    printesc ( 464 ) ; 
    break ; 
  case newlinecharcode : 
    printesc ( 465 ) ; 
    break ; 
  case languagecode : 
    printesc ( 466 ) ; 
    break ; 
  case lefthyphenmincode : 
    printesc ( 467 ) ; 
    break ; 
  case righthyphenmincode : 
    printesc ( 468 ) ; 
    break ; 
  case holdinginsertscode : 
    printesc ( 469 ) ; 
    break ; 
  case errorcontextlinescode : 
    printesc ( 470 ) ; 
    break ;
#ifdef MLTEX
  case char_sub_def_max_code :
    printesc( STR_CHARSUBDEFMAX );
    break;
  case tracing_char_sub_def_code :
    printesc( STR_TRACING_CHARSUBDEF );
    break;
#endif
#ifdef ERW_INTERACTION
  case interactionmodecode :
    printesc( STR_INTERACTION_MODE );
    break;
#endif
  default: 
    c_print("[unknown integer parameter!]");
    break ; 
  }
} 


void begindiagnostic ( void )
{ begindiagnostic_regmem

  oldsetting = selector;
  if ( ( tracingonline <= 0 ) && ( selector == termandlog ) ) {
    decr ( selector );
    if ( history == spotless )
      history = warningissued;
  }
}


void enddiagnostic ( boolean blankline )
{ enddiagnostic_regmem

  c_printnl("");
  if ( blankline )
    println();
  selector = oldsetting;
}


void printlengthparam ( integer n )
{ printlengthparam_regmem 

  switch ( n ) {
  case parindentcode : 
    printesc ( 474 ) ; 
    break ; 
  case mathsurroundcode : 
    printesc ( 475 ) ; 
    break ; 
  case lineskiplimitcode : 
    printesc ( 476 ) ; 
    break ; 
  case hsizecode : 
    printesc ( 477 ) ; 
    break ; 
  case vsizecode : 
    printesc ( 478 ) ; 
    break ; 
  case maxdepthcode : 
    printesc ( 479 ) ; 
    break ; 
  case splitmaxdepthcode : 
    printesc ( 480 ) ; 
    break ; 
  case boxmaxdepthcode : 
    printesc ( 481 ) ; 
    break ; 
  case hfuzzcode : 
    printesc ( 482 ) ; 
    break ; 
  case vfuzzcode : 
    printesc ( 483 ) ; 
    break ; 
  case delimitershortfallcode : 
    printesc ( 484 ) ; 
    break ; 
  case nulldelimiterspacecode : 
    printesc ( 485 ) ; 
    break ; 
  case scriptspacecode : 
    printesc ( 486 ) ; 
    break ; 
  case predisplaysizecode : 
    printesc ( 487 ) ; 
    break ; 
  case displaywidthcode : 
    printesc ( 488 ) ; 
    break ; 
  case displayindentcode : 
    printesc ( 489 ) ; 
    break ; 
  case overfullrulecode : 
    printesc ( 490 ) ; 
    break ; 
  case hangindentcode : 
    printesc ( 491 ) ; 
    break ; 
  case hoffsetcode : 
    printesc ( 492 ) ; 
    break ; 
  case voffsetcode : 
    printesc ( 493 ) ; 
    break ; 
  case emergencystretchcode : 
    printesc ( 494 ) ; 
    break ; 
  default: 
    c_print("[unknown dimen parameter!]");
    break ; 
  } 
} 


void printcmdchr ( quarterword cmd, halfword chrcode )
{ printcmdchr_regmem 

  switch ( cmd ) {
  case left_brace : 
      c_print("begin-group character ");
      print ( chrcode ) ; 
    break ; 
  case right_brace : 
      c_print("end-group character ");
      print ( chrcode ) ; 
    break ; 
  case math_shift : 
      c_print("math shift character ");
      print ( chrcode ) ; 
    break ; 
  case mac_param : 
      c_print("macro parameter character ");
      print ( chrcode ) ; 
    break ; 
  case sup_mark : 
      c_print("superscript character ");
      print ( chrcode ) ; 
    break ; 
  case sub_mark : 
      c_print("subscript character ");
      print ( chrcode ) ; 
    break ; 
  case endv : 
    c_print("end of alignment template");
    break ; 
  case spacer : 
      c_print("blank space ");
      print ( chrcode ) ; 
    break ; 
  case letter : 
      c_print("the letter ");
      print ( chrcode ) ; 
    break ; 
  case otherchar : 
      c_print("the character ");
      print ( chrcode ) ; 
    break ; 

  case assign_glue : 
  case assign_mu_glue : 
    if ( chrcode < skipbase ) 
      printskipparam ( chrcode - gluebase ) ; 
    else if ( chrcode < muskipbase ) {
      printesc( STR_SKIP );
      printint ( chrcode - skipbase ) ; 
    } else {
      printesc ( 392 ) ; 
      printint ( chrcode - muskipbase ) ; 
    } 
    break ; 
  case assign_toks : 
    if ( chrcode >= toksbase ) {
      printesc ( 403 ) ; 
      printint ( chrcode - toksbase ) ; 
    } else switch ( chrcode ) {
    case outputroutineloc : 
      printesc ( 394 ) ; 
      break ; 
    case everyparloc : 
      printesc ( 395 ) ; 
      break ; 
    case everymathloc : 
      printesc ( 396 ) ; 
      break ; 
    case everydisplayloc : 
      printesc ( 397 ) ; 
      break ; 
    case everyhboxloc : 
      printesc ( 398 ) ; 
      break ; 
    case everyvboxloc : 
      printesc ( 399 ) ; 
      break ; 
    case everyjobloc : 
      printesc ( 400 ) ; 
      break ; 
    case everycrloc : 
      printesc ( 401 ) ; 
      break ; 
    default: 
      printesc ( 402 ) ; 
      break ; 
    } 
    break ; 
  case assign_int : 
    if ( chrcode < countbase ) 
      printparam ( chrcode - intbase ) ; 
    else {
      printesc ( 472 ) ; 
      printint ( chrcode - countbase ) ; 
    } 
    break ; 
  case assign_dimen : 
    if ( chrcode < scaledbase ) 
      printlengthparam ( chrcode - dimenbase ) ; 
    else {
      printesc ( 496 ) ; 
      printint ( chrcode - scaledbase ) ; 
    } 
    break ; 
  case accent : 
    printesc ( 504 ) ; 
    break ; 
  case advance : 
    printesc ( 505 ) ; 
    break ; 
  case afterassignment : 
    printesc ( 506 ) ; 
    break ; 
  case aftergroup : 
    printesc ( 507 ) ; 
    break ; 
  case assign_font_dimen : 
    printesc ( 515 ) ; 
    break ; 
  case begin_group : 
    printesc ( 508 ) ; 
    break ; 
  case break_penalty : 
    printesc ( 527 ) ; 
    break ; 
  case charnum : 
    printesc ( 509 ) ; 
    break ; 
  case csname : 
    printesc ( 500 ) ; 
    break ; 
  case def_font : 
    printesc ( 514 ) ; 
    break ; 
  case delim_num : 
    printesc ( 510 ) ; 
    break ; 
  case divide : 
    printesc ( 511 ) ; 
    break ; 
  case end_cs_name : 
    printesc ( STR_ENDCSNAME );
    break ; 
  case end_group : 
    printesc( STR_ENDGROUP );
    break ; 
  case exspace : 
    printesc ( 32 ) ; 
    break ; 
  case expandafter : 
    printesc ( 513 ) ; 
    break ; 
  case halign : 
    printesc ( 516 ) ; 
    break ; 
  case hrule : 
    printesc ( 517 ) ; 
    break ; 
  case ignorespaces : 
    printesc ( 518 ) ; 
    break ; 
  case insert : 
    printesc( STR_INSERT );
    break ; 
  case ital_corr : 
    printesc ( 47 ) ; 
    break ; 
  case mark : 
    printesc ( 348 ) ; 
    break ; 
  case math_accent : 
    printesc ( 519 ) ; 
    break ; 
  case math_char_num : 
    printesc ( 520 ) ; 
    break ; 
  case math_choice : 
    printesc ( 521 ) ; 
    break ; 
  case multiply : 
    printesc ( 522 ) ; 
    break ; 
  case no_align : 
    printesc ( 523 ) ; 
    break ; 
  case noboundary : 
    printesc ( 524 ) ; 
    break ; 
  case noexpand : 
    printesc ( 525 ) ; 
    break ; 
  case non_script : 
    printesc ( 332 ) ; 
    break ; 
  case omit : 
    printesc ( 526 ) ; 
    break ; 
  case radical : 
    printesc ( 529 ) ; 
    break ; 
  case read_to_cs : 
    printesc ( STR_READ );
    break ; 
  case relax : 
    printesc ( 531 ) ; 
    break ; 
  case set_box : 
    printesc ( 532 ) ; 
    break ; 
  case set_prev_graf : 
    printesc ( 528 ) ; 
    break ; 
  case set_shape : 
    printesc ( 404 ) ; 
    break ; 
  case the : 
    printesc ( 533 ) ; 
    break ; 
  case toks_register : 
    printesc ( 403 ) ; 
    break ; 
  case vadjust : 
    printesc ( 349 ) ; 
    break ; 
  case valign : 
#ifdef TEXXET
    if( chrcode != 0 ) {
      switch( chrcode ) {
	case begin_L_code:	print_esc(STR_BEGINL); break;
	case end_L_code:	print_esc(STR_ENDL); break;
	case begin_R_code:	print_esc(STR_BEGINR); break;
	default:		print_esc(STR_ENDR); break;
      }
    } else
#endif
      printesc ( 534 ) ; 
    break ; 
  case vcenter : 
    printesc ( 535 ) ; 
    break ; 
  case vrule : 
    printesc ( 536 ) ; 
    break ; 
#ifdef MLTEX
  case char_sub_def :
    printesc(STR_CHARSUBDEF);
    break;
#endif
  case par_end : 
    printesc( STR_PAR );
    break ; 
  case input : 
    printesc( (chrcode == 0) ? 625 : 626 );
    break ; 
  case topbotmark : 
    switch ( chrcode ) {
    case firstmarkcode : 
      printesc ( 628 ) ; 
      break ; 
    case botmarkcode : 
      printesc ( 629 ) ; 
      break ; 
    case splitfirstmarkcode : 
      printesc ( 630 ) ; 
      break ; 
    case splitbotmarkcode : 
      printesc ( 631 ) ; 
      break ; 
      default: 
      printesc ( 627 ) ; 
      break ; 
    } 
    break ; 
  case register_cmd : 
    if ( chrcode == intval ) 
      printesc ( 472 ) ; 
    else if ( chrcode == dimenval ) 
      printesc ( 496 ) ; 
    else if ( chrcode == glueval ) 
      printesc( STR_SKIP );
    else
      printesc ( 392 ) ; 
    break ; 
  case set_aux:
    printesc( (chrcode == vmode) ? STR_PREVDEPTH : STR_SPACEFACTOR );
    break;
  case set_page_int : 
    printesc( (chrcode == 0) ? STR_DEADCYCLES : STR_INSERTPENALTIES );
    break;
  case set_box_dimen : 
    if ( chrcode == widthoffset ) 
      printesc ( STR_WD ) ; 
    else if ( chrcode == heightoffset ) 
      printesc ( STR_HT ) ; 
    else
      printesc ( STR_DP ) ; 
    break ; 
  case last_item : 
    switch ( chrcode ) {
    case intval : 
      printesc ( STR_LASTPENALTY ) ; 
      break ; 
    case dimenval : 
      printesc ( STR_LASTKERN ) ; 
      break ; 
    case glueval : 
      printesc ( STR_LASTSKIP ) ; 
      break ; 
    case inputlinenocode : 
      printesc ( STR_INPUTLINENO ) ; 
      break ; 
    default: 
      printesc ( STR_BADNESS ) ; 
      break ; 
    } 
    break ; 
  case convert : 
    switch ( chrcode ) {
    case numbercode : 
      printesc ( STR_NUMBER );
      break ; 
    case romannumeralcode : 
      printesc ( STR_ROMANNUMERAL );
      break ; 
    case stringcode : 
      printesc ( STR_STRING );
      break ; 
    case meaningcode : 
      printesc ( STR_MEANING );
      break ; 
    case fontnamecode : 
      printesc ( STR_FONTNAME );
      break ; 
    default: 
      printesc ( STR_JOBNAME );
      break;
    }
    break;
  case iftest : 
    switch ( chrcode ) {
    case ifcatcode : 
      printesc ( STR_IFCAT ) ; 
      break ; 
    case ifintcode : 
      printesc ( STR_IFNUM ) ; 
      break ; 
    case ifdimcode : 
      printesc ( STR_IFDIM ) ; 
      break ; 
    case ifoddcode : 
      printesc ( STR_IFODD ) ; 
      break ; 
    case ifvmodecode : 
      printesc ( STR_IFVMODE ) ; 
      break ; 
    case ifhmodecode : 
      printesc ( STR_IFHMODE ) ; 
      break ; 
    case ifmmodecode : 
      printesc ( STR_IFMMODE ) ; 
      break ; 
    case ifinnercode : 
      printesc ( STR_IFINNER ) ; 
      break ; 
    case ifvoidcode : 
      printesc ( STR_IFVOID ) ; 
      break ; 
    case ifhboxcode : 
      printesc ( STR_IFHBOX ) ; 
      break ; 
    case ifvboxcode : 
      printesc ( STR_IFVBOX ) ; 
      break ; 
    case ifxcode : 
      printesc ( STR_IFX ) ; 
      break ; 
    case ifeofcode : 
      printesc ( STR_IFEOF ) ; 
      break ; 
    case iftruecode : 
      printesc ( STR_IFTRUE ) ; 
      break ; 
    case iffalsecode : 
      printesc ( STR_IFFALSE ) ; 
      break ; 
    case ifcasecode : 
      printesc ( STR_IFCASE ) ; 
      break ; 
    default: 
      printesc( STR_IF );
      break ; 
    } 
    break ; 
  case fiorelse : 
    if ( chrcode == ficode ) 
      printesc ( STR_FI ) ; 
    else if ( chrcode == orcode ) 
      printesc ( STR_OR ) ; 
    else printesc ( STR_ELSE );
    break ; 
  case tab_mark : 
    if ( chrcode == spancode ) 
      printesc( STR_SPAN );
    else {
      c_print("alignment tab character ");
      print ( chrcode ) ; 
    } 
    break ; 
  case car_ret : 
    printesc( (chrcode == crcode) ? STR_CR : STR_CRCR );
    break;
  case set_page_dimen : 
    switch ( chrcode ) {
    case 0 : 
      printesc( STR_PAGEGOAL );
      break ; 
    case 1 : 
      printesc( STR_PAGETOTAL );
      break ; 
    case 2 : 
      printesc( STR_PAGESTRETCH );
      break ; 
    case 3 : 
      printesc( STR_PAGEFILSTRETCH );
      break ; 
    case 4 : 
      printesc( STR_PAGEFILLSTRETCH );
      break ; 
    case 5 : 
      printesc( STR_PAGEFILLLSTRETCH );
      break ; 
    case 6 : 
      printesc( STR_PAGESHRINK );
      break ; 
    default: 
      printesc( STR_PAGEDEPTH );
      break ; 
    } 
    break ; 
  case stop : 
    printesc( (chrcode == 1) ? STR_DUMP : STR_END );
    break ; 
  case hskip : 
    switch ( chrcode ) {
    case 4 : 
      printesc( STR_HSKIP );
      break ; 
    case 0 : 
      printesc( STR_HFIL );
      break ; 
    case 1 : 
      printesc( STR_HFILL );
      break ; 
    case 2 : 
      printesc( STR_HSS );
      break ; 
    default: 
      printesc( STR_HFILNEG );
      break;
    }
    break ; 
  case vskip : 
    switch ( chrcode ) {
    case 4 : 
      printesc( STR_VSKIP );
      break ; 
    case 0 : 
      printesc( STR_VFIL ) ; 
      break ; 
    case 1 : 
      printesc( STR_VFILL ) ; 
      break ; 
    case 2 : 
      printesc( STR_VSS ) ; 
      break ; 
    default: 
      printesc( STR_VFILNEG ) ; 
      break ; 
    } 
    break ; 
  case mskip : 
    printesc ( 333 ) ; 
    break ; 
  case kern : 
    printesc ( 337 ) ; 
    break ; 
  case mkern : 
    printesc ( 339 ) ; 
    break ; 
  case hmove : 
    printesc( (chrcode == 1) ? STR_MOVELEFT : STR_MOVERIGHT );
    break ; 
  case vmove : 
    printesc( (chrcode == 1) ? STR_RAISE : STR_LOWER );
    break ; 
  case make_box : 
    switch ( chrcode ) {
    case 0 : 
      printesc( STR_BOX );
      break ; 
    case 1 : 
      printesc( STR_COPY );
      break ; 
    case 2 : 
      printesc( STR_LASTBOX );
      break ; 
    case 3 : 
      printesc( STR_VSPLIT );
      break ; 
#ifdef OLD_MODE
    case 4 :
      printesc( STR_VTOP );
      break ; 
    case 5 : 
      printesc( STR_VBOX );
      break ; 
    default: 
      printesc( STR_HBOX );
      break ; 
#else
    default:
      if( chrcode == 4 )
	printesc( STR_VTOP );
      else if( chrcode == (4 + vmode) )
	printesc( STR_VBOX );
      else
	printesc( STR_HBOX ); 
      break;
#endif
    }
    break;
  case leader_ship :
    if ( chrcode == aleaders ) 
      printesc( STR_LEADERS );
    else if ( chrcode == cleaders ) 
      printesc( STR_CLEADERS );
    else if ( chrcode == xleaders ) 
      printesc( STR_XLEADERS );
    else
      printesc( STR_SHIPOUT );
    break ; 
  case start_par : 
    printesc( (chrcode == 0) ? STR_NOINDENT : STR_INDENT );
    break;
  case remove_item : 
    if ( chrcode == gluenode ) 
      printesc( STR_UNSKIP );
    else if ( chrcode == kernnode ) 
      printesc( STR_UNKERN );
    else
      printesc( STR_UNPENALTY );
    break;
  case un_hbox : 
    printesc( (chrcode == 1) ? STR_UNHCOPY : STR_UNHBOX );
    break;
  case un_vbox : 
    printesc( (chrcode == 1) ? STR_UNVCOPY : STR_UNVBOX );
    break;
  case discretionary : 
    printesc( (chrcode == 1) ? 45 : 346 );
    break ; 
  case eq_no : 
    printesc( (chrcode == 1) ? STR_LEQNO : STR_EQNO );
    break ; 
  case math_comp : 
    switch ( chrcode ) {
    case ordnoad : 
      printesc ( STR_MATHORD ) ; 
      break ; 
    case opnoad : 
      printesc ( STR_MATHOP ) ; 
      break ; 
    case binnoad : 
      printesc ( STR_MATHBIN ) ; 
      break ; 
    case relnoad : 
      printesc ( STR_MATHREL ) ; 
      break ; 
    case opennoad : 
      printesc ( STR_MATHOPEN ) ; 
      break ; 
    case closenoad : 
      printesc ( STR_MATHCLOSE ) ; 
      break ; 
    case punctnoad : 
      printesc ( STR_MATHPUNCT ) ; 
      break ; 
    case innernoad : 
      printesc ( STR_MATHINNER ) ; 
      break ; 
    case undernoad : 
      printesc ( STR_UNDERLINE ) ; 
      break ; 
    default: 
      printesc ( STR_OVERLINE );
      break ; 
    } 
    break ; 
  case limit_switch : 
    if ( chrcode == limits ) 
      printesc ( STR_LIMITS ) ; 
    else if ( chrcode == nolimits ) 
      printesc ( STR_NOLIMITS ) ; 
    else
      printesc( STR_DISPLAYLIMITS );
    break ; 
  case math_style :
    printstyle ( chrcode ) ; 
    break ; 
  case above :
    { strnumber s;

      switch ( chrcode ) {
	case 1:  s = STR_OVER;  break;
	case 2:  s = STR_ATOP;  break;
	case 3:  s = STR_ABOVEWITHDELIMS; break;
	case 4:  s = STR_OVERWITHDELIMS;  break;
	case 5:  s = STR_ATOPWITHDELIMS;  break;
	default: s = STR_ABOVE; break;
      }
      printesc(s);
    }
    break;
  case left_right : 
    printesc( (chrcode == leftnoad) ? STR_LEFT : STR_RIGHT );
    break ; 
  case prefix : 
    if ( chrcode == 1 ) 
      printesc ( STR_LONG );
    else if ( chrcode == 2 ) 
      printesc ( STR_OUTER );
    else
      printesc ( STR_GLOBAL );
    break ; 
  case def : 
    if ( chrcode == 0 ) 
      printesc ( STR_DEF );
    else if ( chrcode == 1 ) 
      printesc ( STR_GDEF );
    else if ( chrcode == 2 ) 
      printesc ( STR_EDEF );
    else
      printesc ( STR_XDEF );
    break ; 
  case let : 
    printesc( (chrcode != normal) ? STR_FUTURELET : STR_LET );
    break ; 
  case shorthand_def : 
    switch ( chrcode ) {
    case 0 : 
      printesc ( STR_CHARDEF );
      break ; 
    case 1 : 
      printesc ( STR_MATHCHARDEF );
      break ; 
    case 2 : 
      printesc ( STR_COUNTDEF );
      break ; 
    case 3 : 
      printesc ( STR_DIMENDEF );
      break ; 
    case 4 : 
      printesc ( STR_SKIPDEF );
      break ; 
    case 5 : 
      printesc ( STR_MUSKIPDEF );
      break ; 
    default: 
      printesc ( STR_TOKSDEF );
      break ; 
    } 
    break ; 
  case chargiven : 
    printesc ( 509 ) ; 
    printhex ( chrcode ) ; 
    break ; 
  case math_given :
    printesc ( 520 ) ; 
    printhex ( chrcode ) ; 
    break ; 
  case def_code : 
    if ( chrcode == catcodebase ) 
      printesc ( 411 ) ; 
    else if ( chrcode == mathcodebase ) 
      printesc ( 415 ) ; 
    else if ( chrcode == lccodebase ) 
      printesc ( 412 ) ; 
    else if ( chrcode == uccodebase ) 
      printesc ( 413 ) ; 
    else if ( chrcode == sfcodebase ) 
      printesc ( 414 ) ; 
    else printesc ( 473 ) ; 
    break ; 
  case def_family : 
    printsize ( chrcode - mathfontbase );
    break ; 
  case hyph_data : 
    printesc( (chrcode == 1) ? STR_PATTERNS : STR_HYPHENATION );
    break ; 
  case assign_font_int : 
    printesc( (chrcode == 0) ? STR_HYPHENCHAR : STR_SKEWCHAR );
    break ; 
  case set_font :
    {
      c_print("select font ");
#if 0  /* TeX 3.141 */
      print ( fontname(chrcode) );
#else
      slowprint ( fontname(chrcode) );
#endif
      if ( fontsize(chrcode) != fontdsize(chrcode) ) {
	print( STR_AT_ );
	printscaled ( fontsize(chrcode) );
	print( STR_PT );
      } 
    } 
    break ; 
  case set_interaction : 
    switch ( chrcode ) {
    case batchmode : 
      printesc ( 272 ) ; 
      break ; 
    case nonstopmode : 
      printesc ( 273 ) ; 
      break ; 
    case scrollmode : 
      printesc ( 274 ) ; 
      break ; 
    default: 
      printesc ( STR_ERRORSTOPMODE );
      break ; 
    } 
    break ; 
  case instream : 
    printesc( (chrcode == 0) ? STR_CLOSEIN : STR_OPENIN );
    break ; 
  case message : 
    printesc( (chrcode == 0) ? STR_MESSAGE : STR_ERRMESSAGE );
    break ; 
  case caseshift : 
    printesc( (chrcode == lccodebase) ? STR_LOWERCASE : STR_UPPERCASE );
    break ; 
  case xray : 
    switch ( chrcode ) {
    case 1 : 
      printesc ( STR_SHOWBOX );
      break;
    case 2 : 
      printesc ( STR_SHOWTHE );
      break;
    case 3 : 
      printesc ( STR_SHOWLISTS );
      break;
    default: 
      printesc ( STR_SHOW );
      break;
    }
    break ; 
  case undefinedcs : 
    c_print("undefined");
    break ; 
  case call : 
    c_print("macro");
    break ; 
  case longcall : 
    c_printesc("long macro");
    break ; 
  case longoutercall : 
    printesc ( STR_LONG );
    /* Fall through */
  case outercall : 
    c_printesc("outer macro");
    break ;
  case endtemplate : 
    c_printesc("outer endtemplate");
    break ;
  case extension: 
    switch ( chrcode ) {
    case opennode : 
      printesc ( STR_OPENOUT );
      break ; 
    case writenode : 
      printesc ( STR_WRITE );
      break ; 
    case closenode : 
      printesc ( STR_CLOSEOUT );
      break ; 
    case specialnode : 
      printesc ( STR_SPECIAL );
      break ; 
    case 4 : 
      printesc ( STR_IMMEDIATE );
      break ; 
    case 5 : 
      printesc( STR_SETLANGUAGE );
      break ; 
    default: 
      c_print("[unknown extension!]");
      break ; 
    } 
    break ; 
  default:
    c_print("[unknown command code!]");
    break;
  } 
} 


#ifdef STAT
void showeqtb ( halfword n )
{ showeqtb_regmem 

  if ( n < activebase ) 
    printchar ( 63 ) ; 
  else if ( n < gluebase ) {
    sprintcs ( n ) ; 
    printchar ( 61 ) ; 
    printcmdchr ( eqtype ( n ) , equiv ( n ) ) ; 
    if ( eqtype ( n ) >= call ) {
      printchar ( 58 ) ; 
      showtokenlist ( link ( equiv ( n ) ) , 0 , 32 ) ; 
    } 
  } else if ( n < localbase ) {
    if ( n < skipbase ) {
      printskipparam ( n - gluebase ) ; 
      printchar ( 61 ) ; 
      if ( n < gluebase + thinmuskipcode ) 
	printspec( equiv(n), STR_PT );
      else
	printspec( equiv(n), STR_MU );
    } else if ( n < muskipbase ) {
      printesc( STR_SKIP );
      printint ( n - skipbase ) ; 
      printchar ( 61 ) ; 
      printspec ( equiv ( n ) , STR_PT ) ; 
    } else {
      printesc ( 392 ) ; 
      printint ( n - muskipbase ) ; 
      printchar ( 61 ) ; 
      printspec ( equiv ( n ), STR_MU );
    }
  } else if ( n < intbase ) 

  if ( n == parshapeloc ) {
    printesc ( 404 ) ; 
    printchar ( 61 ) ; 
    if ( parshapeptr == 0 ) 
      printchar ( 48 ) ; 
    else printint ( info ( parshapeptr ) ) ; 
  } else if ( n < toksbase ) {
    printcmdchr( assign_toks, n );
    printchar ( 61 ) ; 
    if ( equiv ( n ) != 0 )
      showtokenlist ( link ( equiv ( n ) ) , 0 , 32 ) ; 
  } else if ( n < boxbase ) {
    printesc ( 403 ) ; 
    printint ( n - toksbase ) ; 
    printchar ( 61 ) ; 
    if ( equiv ( n ) != 0 )
      showtokenlist ( link ( equiv ( n ) ) , 0 , 32 ) ; 
  } else if ( n < curfontloc ) {
    printesc( STR_BOX );
    printint ( n - boxbase ) ; 
    printchar ( 61 ) ;
    if ( equiv ( n ) == 0 ) 
      c_print("void");
    else {
      depththreshold = 0 ; 
      breadthmax = 1 ; 
      shownodelist ( equiv ( n ) ) ; 
    } 
  } else if ( n < catcodebase ) {
    if ( n == curfontloc )
      c_print("current font");
    else if ( n < mathfontbase + 16 ) {
      printesc ( 408 ) ; 
      printint ( n - mathfontbase ) ; 
    } else if ( n < mathfontbase + 32 ) {
      printesc ( 409 ) ; 
      printint ( n - mathfontbase - 16 ) ; 
    } else {
      printesc ( 410 ) ; 
      printint ( n - mathfontbase - 32 ) ; 
    } 
    printchar ( 61 ) ; 
    printesc ( hash [ fontidbase + equiv ( n ) ] .v.RH ) ; 
  } else if ( n < mathcodebase ) {
    if ( n < lccodebase ) {
      printesc ( 411 ) ; 
      printint ( n - catcodebase ) ; 
    } else if ( n < uccodebase ) {
      printesc ( 412 ) ; 
      printint ( n - lccodebase ) ; 
    } else if ( n < sfcodebase ) {
      printesc ( 413 ) ; 
      printint ( n - uccodebase ) ; 
    } else {
      printesc ( 414 ) ; 
      printint ( n - sfcodebase ) ; 
    } 
    printchar ( 61 ) ; 
    printint ( equiv ( n ) ) ; 
  } else {
    printesc ( 415 ) ; 
    printint ( n - mathcodebase ) ; 
    printchar ( 61 ) ; 
    printint ( equiv ( n ) ) ; 
  }
 
  else if ( n < dimenbase ) {
    if ( n < countbase ) 
      printparam ( n - intbase ) ; 
    else if ( n < delcodebase ) {
      printesc ( 472 ) ; 
      printint ( n - countbase ) ; 
    } else {
      printesc ( 473 ) ; 
      printint ( n - delcodebase ) ; 
    } 
    printchar ( 61 ) ; 
    printint ( eqtb [ n ] .cint ) ; 
  } else if ( n <= eqtbsize ) {
    if ( n < scaledbase )
      printlengthparam ( n - dimenbase ) ; 
    else {
      printesc ( 496 ) ; 
      printint ( n - scaledbase ) ; 
    } 
    printchar ( 61 ) ; 
    printscaled ( eqtb [ n ] .cint ) ; 
    print ( STR_PT );
  } else
    printchar ( 63 );
}
#endif /* STAT */


halfword idlookup ( integer j, integer l )
{ idlookup_regmem
  register unsigned short h;	/* ==> hashprime < (65536/2-256) */
  register integer d; 
  register halfword p;
  register long k;

  ASCIIcode *cp = &buffer[j];

  /* 261. */
  h = *cp++;	/* h = buffer [ j ] ; */
  for( k = 2 ; k <= l ; k++ ) {
      h = h + h + *cp++;	/* buffer[k] */
      while ( h >= hashprime )
	h -= hashprime;
  }
  /* 261. end */

  p = h + hashbase;

  while ( true ) {
    if( ( ztext(p) > 0 )
	&&  ( length( ztext(p) ) == l )
	&& !strncmp( (char *) &strpool[strstart[ztext(p)]],
		     (char *) &buffer[j],
		     l ) )
      return(p);

    if ( next ( p ) == 0 ) {
      if ( nonewcontrolsequence )
	return( undefinedcontrolsequence );
      else {
	/* 260. */
	if ( ztext ( p ) > 0 ) {
	  do {
	    if ( hashisfull )
	    overflow(5, hashsize);
	    decr ( hashused );
	  } while ( ztext ( hashused ) != 0 );
	  next ( p ) = hashused;
	  p = hashused;
	}
	strroom ( l );
	d = curlength;
	while ( poolptr > strstart [ strptr ] ) {
	  decr ( poolptr );
	  strpool [ poolptr + l ] = strpool [ poolptr ];
	}
	for( k = j ; k <= j + l - 1 ; k++ ) {
	  appendchar ( buffer [ k ] );
	}
	ztext ( p ) = makestring();
	poolptr += d;
#ifdef STAT
	incr ( cscount );
#endif /* STAT */
      }
      return(p);
    }
    p = next ( p );
  }
}


void newsavelevel ( groupcode c )
{ newsavelevel_regmem

  if ( saveptr > maxsavestack ) {
    maxsavestack = saveptr;
    if ( maxsavestack > savesize - 6 )
    overflow(6, savesize);
  }
  savetype ( saveptr ) = levelboundary;
  savelevel ( saveptr ) = curgroup;
  saveindex ( saveptr ) = curboundary;
  if ( curlevel == maxquarterword )
    overflow(7, maxquarterword - 0);
  curboundary = saveptr;
  incr ( curlevel );
  incr ( saveptr );
  curgroup = c;
}


static	/* (hes) */
void eqdestroy ( MEDmemoryword w )
{ eqdestroy_regmem

  switch ( eqtypefield ( w ) ) {
  case call :
  case longcall :
  case outercall :
  case longoutercall :
    deletetokenref ( equivfield ( w ) );
    break;
  case glueref :
    deleteglueref ( equivfield ( w ) );
    break;
  case shaperef :
    { register halfword q;

      q = equivfield( w );
      if ( q != 0 )
	freenode( q, info(q) + info(q) + 1 ); 
    }
    break;
  case boxref :
    flushnodelist ( equivfield ( w ) );
    break;
  default:
    break;
  }
}


void eqsave ( halfword p, quarterword l )
{ eqsave_regmem

  if ( saveptr > maxsavestack ) {
    maxsavestack = saveptr ; 
    if ( maxsavestack > savesize - 6 ) 
      overflow(6, savesize);
  } 
  if ( l == levelzero ) 
    savetype ( saveptr ) = restorezero ; 
  else {
    savestack [ saveptr ] = eqtb [ p ] ; 
    incr ( saveptr ) ; 
    savetype ( saveptr ) = restoreoldvalue ; 
  } 
  savelevel ( saveptr ) = l ; 
  saveindex ( saveptr ) = p ; 
  incr ( saveptr ) ; 
}


void eqdefine ( halfword p, quarterword t, halfword e )
{ eqdefine_regmem

  if ( eqlevel ( p ) == curlevel ) 
    eqdestroy ( eqtb [ p ] ) ; 
  else if ( curlevel > levelone ) 
    eqsave ( p , eqlevel ( p ) ) ; 
  eqlevel ( p ) = curlevel ; 
  eqtype ( p ) = t ; 
  equiv ( p ) = e ; 
}


void eqworddefine ( halfword p, integer w )
{ eqworddefine_regmem 

  if ( xeqlevel [ p ] != curlevel ) {
    eqsave( p, xeqlevel[p] );
    xeqlevel[p] = curlevel;
  }
  eqtb [ p ] .cint = w;
}


void geqdefine ( halfword p, quarterword t, halfword e )
{ geqdefine_regmem

  eqdestroy ( eqtb [ p ] );
  eqlevel ( p ) = levelone;
  eqtype ( p ) = t;
  equiv ( p ) = e;
}


void geqworddefine ( halfword p, integer w )
{ geqworddefine_regmem

  eqtb [ p ] .cint = w;
  xeqlevel [ p ] = levelone;
} 


void saveforafter ( halfword t )
{ saveforafter_regmem

  if ( curlevel > levelone ) {
    if ( saveptr > maxsavestack ) {
      maxsavestack = saveptr ; 
      if ( maxsavestack > savesize - 6 ) 
	overflow(6, savesize);
    }
    savetype ( saveptr ) = inserttoken;
    savelevel ( saveptr ) = levelzero;
    saveindex ( saveptr ) = t;
    incr ( saveptr );
  }
}


#ifdef STAT
void restoretrace ( halfword p, char *s )
{ restoretrace_regmem

  begindiagnostic () ; 
  printchar ( 123 ) ; 
  c_print(s);
  printchar ( 32 ) ; 
  showeqtb ( p ) ; 
  printchar ( 125 ) ; 
  enddiagnostic ( false ) ; 
} 
#endif /* STAT */


void unsave ( void )
{/* 30 */ unsave_regmem 
  register halfword p  ; 
  register quarterword l  ; 

  if ( curlevel > levelone ) {
    decr ( curlevel ) ; 
    while ( true ) {
      decr ( saveptr ) ; 
      if ( savetype ( saveptr ) == levelboundary ) 
	goto lab30 ; 
      p = saveindex ( saveptr ) ; 
      if ( savetype ( saveptr ) == inserttoken ) {
	register halfword t;

	t = curtok ; 
	curtok = p ; 
	backinput () ; 
	curtok = t ; 
      } else {
	if ( savetype ( saveptr ) == restoreoldvalue ) {
	  l = savelevel ( saveptr ) ; 
	  decr ( saveptr ) ; 
	} else
	  savestack [ saveptr ] = eqtb [ undefinedcontrolsequence ] ; 
	if ( p < intbase ) 
	if ( eqlevel ( p ) == levelone ) {
	  eqdestroy ( savestack [ saveptr ] ) ; 
#ifdef STAT
	  if ( tracingrestores > 0 ) 
	    restoretrace(p, "retaining");
#endif /* STAT */
	} else {
	  eqdestroy ( eqtb [ p ] ) ; 
	  eqtb [ p ] = savestack [ saveptr ] ; 
#ifdef STAT
	  if ( tracingrestores > 0 ) 
	    restoretrace(p, "restoring");
#endif /* STAT */
	}
	else if ( xeqlevel [ p ] != levelone ) {
	  eqtb [ p ] = savestack [ saveptr ] ; 
	  xeqlevel [ p ] = l ; 
#ifdef STAT
	  if ( tracingrestores > 0 ) 
	    restoretrace(p, "restoring");
#endif /* STAT */
	}
	else {
#ifdef STAT
	  if ( tracingrestores > 0 ) 
	    restoretrace(p, "retaining");
#endif /* STAT */
	}
      }
    }
lab30:
    curgroup = savelevel ( saveptr ) ; 
    curboundary = saveindex ( saveptr ) ; 
  } else
    confusion("curlevel");
}

/* -- end -- */
