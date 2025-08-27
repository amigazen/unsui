#define EXTERN extern
#include "texd.h"

#ifdef AMIGA
extern int do_rexx;	/* defined in arexx.c, used in error() */
#endif


void println ( void )
{ println_regmem

  switch ( selector ) {
  case termandlog:
	(void) putc('\n', stdout);
	termoffset = 0;
	/* fall throught */
  case logonly:
	(void) putc('\n', logfile);
	fileoffset = 0;
	break;
  case termonly:
	(void) putc('\n', stdout);
	termoffset = 0;
	break;
  case noprint:
  case pseudo:
  case newstring:
	break;
  default:
	(void) putc('\n', writefile[selector]);
	break;
  }
}

  
#ifdef ERW_CODEPAGE
  static
void print_representable_char ( ASCIIcode s )
#else
void printchar ( ASCIIcode s )
#endif
{ printchar_regmem

#ifndef ERW_CODEPAGE
  if ( s == newlinechar && selector < pseudo ) {
    println ();
    return;
  }
#endif

  switch ( selector ) {
  case termandlog :
	(void) putc( Xchr ( s ) ,  stdout );
	incr ( termoffset ) ;
	if ( termoffset == maxprintline ) {
	   (void) putc('\n',  stdout );
	   termoffset = 0;
	}
	/* fall throught */
  case logonly :
	(void) putc( Xchr ( s ) ,  logfile );
	incr ( fileoffset ) ;
	if ( fileoffset == maxprintline ) {
	   (void) putc('\n',  logfile );
	   fileoffset = 0 ;
	}
	break;
  case termonly :
	(void) putc( Xchr ( s ) ,  stdout );
	incr ( termoffset ) ;
	if ( termoffset == maxprintline ) {
	   (void) putc('\n',  stdout );
	   termoffset = 0;
	}
	break ;
  case noprint :
	break ;
  case pseudo :
	if ( tally < trickcount )
	  trickbuf [ tally % errorline ] = s ;
	break ;
  case newstring :
	if ( poolptr < poolsize )
	   appendchar ( s ) ;
	break ;
  default:
	(void) putc( Xchr ( s ) ,  writefile [ selector ] );
	break ;
  }
  incr ( tally ) ;
}


#ifdef ERW_CODEPAGE
void printchar ( ASCIIcode s)
{ printchar_regmem

  if ( s == newlinechar && selector < pseudo ) {
    println ();
    return;
  }

  if (printable[s] || (selector > pseudo) ) {
    print_representable_char(s);
    return;
  }

  print_representable_char('^');
  print_representable_char('^');
  if ( s < 64 ) {
    print_representable_char(s + 64);
  } else if ( s < 128 ) {
    print_representable_char(s - 64);
  } else {
    register short l;

    l = s / 16;
    print_representable_char( l + ( ( l < 10 ) ? '0' : ('a'-10) ) );
    l = s % 16;
    print_representable_char( l + ( ( l < 10 ) ? '0' : ('a'-10) ) );
  }
}
#endif


void print ( integer s )
{ print_regmem
  register packedASCIIcode *cur, *ende;
#ifndef ERW_CODEPAGE
  register integer nl;
#endif

  if ( s >= strptr ) {
    c_print("???");
    return;
  } else if ( s < 256 ) {
    if ( s < 0 ) {
      c_print("???");
      return;
    } else {
#ifdef ERW_CODEPAGE
      printchar(s);
#else
      if ( selector > pseudo ) {
	printchar( s );
	return;
      }
      if ( s == newlinechar ) {
	if ( selector < pseudo ) {
	  println ();
	  return;
	}
      }
      nl = newlinechar;  newlinechar = (integer) -1;
      cur = &strpool[strstart[s]];  ende = &strpool[strstart[s+1]];
      while ( cur < ende ) {
	printchar ( *cur );
	incr ( cur );
      }
      newlinechar = nl;
#endif
      return;
    }
  }

  cur = &strpool[strstart[s]];  ende = &strpool[strstart[s+1]];
  while ( cur < ende ) {
    printchar ( *cur );
    incr ( cur );
  }
}


/* wie `print'
 * nur dass direkt ein null-terminierender String uebergeben wird.
 * (Und fuer diesen gelte "s >= 256", so dass die Abfrage zu
 * Beginn weggelassen werden kann)
 */
void c_print ( char *s )
{ print_regmem

  while( *s != '\0' )
    printchar ( *s++ );
}


#ifndef ERW_CODEPAGE
void slowprint ( integer s )
{ slowprint_regmem
  register packedASCIIcode *cur, *ende;

  if( (s >= strptr) || (s < 256) ) {
    print(s);
  } else {
    cur = &strpool[strstart[s]];  ende = &strpool[strstart[s+1]];
    while ( cur < ende ) {
      print ( *cur );
      incr ( cur );
    }
  }
}
#endif


void c_printnl ( char *s )
{ printnl_regmem

  if ( ( termoffset > 0 && odd(selector) )
    || ( fileoffset > 0 && selector >= logonly ) )
	println ();
  c_print ( s );
}

void printnl ( strnumber s )
{ printnl_regmem

  if ( ( termoffset > 0 && odd(selector) )
    || ( fileoffset > 0 && selector >= logonly ) )
	println ();
  print ( s );
}

	/* Note that c_printesc() should only be used on internal strings! */
	/* Because we don't call c_slowprint() !! ;-) */
void c_printesc ( char *s )
{ printesc_regmem
  register integer c;

  c = escapechar;
  if ( c >= 0 && c < 256 )
    print ( c );
  c_print(s);
}

void printesc ( strnumber s )
{ printesc_regmem
  register integer c;

  c = escapechar;
  if ( c >= 0 && c < 256 )
    print ( c );
  slowprint ( s );  /* TeX 3.14 2.update:  print ( s ); */
}

void printthedigs ( eightbits k )
{ printthedigs_regmem

  while ( k > 0 ) {
    decr ( k ) ;
    if ( dig [ k ] < 10 )
	printchar ( 48 + dig[k] );
    else
	printchar ( 55 + dig[k] );
  }
}

void printint ( integer n )
{ printint_regmem
  register schar k  ;
  register integer m  ;
  ldiv_t result;

  k = 0 ;
  if ( n < 0 ) {
    printchar ( 45 ) ;
    if ( n > -100000000L )
	n = - (integer) n ;
    else {
	m = -1 - n ;
        result = ldiv(m, 10);
	n = result.quot;	/* n = m / 10 ; */
	m = result.rem + 1;	/* m = ( m % 10 ) + 1 ; */
	k = 1;
	if ( m < 10 )
	   dig [ 0 ] = m ;
	else {
	   dig [ 0 ] = 0 ;
	   incr ( n ) ;
	}
    }
  }

  do {
    result = ldiv(n, 10);
    dig[k] = result.rem;	/* dig [ k ] = n % 10 ; */
    n = result.quot;		/* n = n / 10 ; */
    incr ( k );
  } while ( n != 0 );

  printthedigs ( k );
}

void printcs ( integer p )
{ printcs_regmem

  if ( p < hashbase )
    if ( p >= singlebase )
	if ( p == nullcs ) {
	   printesc ( 500 );
	   printesc( STR_ENDCSNAME );
	} else {
	   printesc ( p - singlebase );
	   if ( catcode ( p - singlebase ) == 11 )
	      printchar ( 32 );
	}
    else if ( p < activebase )
	c_printesc("IMPOSSIBLE.");
    else
 	print ( p - activebase );
  else if ( p >= undefinedcontrolsequence )
    c_printesc("IMPOSSIBLE.");
  else if ( ( ztext ( p ) >= strptr ) )
    c_printesc("NONEXISTENT.");
  else {
    printesc ( ztext(p) );  /* TeX 3.14 2.update */
    printchar ( 32 );
  }
}

void sprintcs ( halfword p )
{ sprintcs_regmem

  if ( p < hashbase )
    if ( p < singlebase )
	print ( p - activebase );
    else if ( p < nullcs )
	printesc ( p - singlebase );
    else {
	printesc( 500 );
	printesc( STR_ENDCSNAME );
    }
  else {
    printesc ( ztext(p) );	/* TeX 3.14 2.update */
  }
}


void printfilename ( integer n, integer a, integer e )
{ printfilename_regmem

  slowprint ( a );  slowprint ( n );  slowprint ( e ); /* TeX 3.141 */
}


void printsize ( integer s )
{ printsize_regmem

  if ( s == 0 )
    printesc ( 408 );
  else if ( s == scriptsize )
    printesc ( 409 );
  else
    printesc ( 410 );
}


void printwritewhatsit ( strnumber s, halfword p )
{ printwritewhatsit_regmem

  printesc ( s );
  if ( writestream(p) < 16 )
    printint ( writestream(p) );
  else if ( writestream(p) == 16 )
    printchar ( 42 );
  else
    printchar ( 45 );
}


VOLATILE void jumpout ( void )
{ jumpout_regmem

  closefilesandterminate ();
  flush ( stdout );
  readyalready = 0;
  if ( ( history != spotless ) && ( history != warningissued ) )
    uexit ( 1 );
  else
    uexit ( 0 );
}


void print_err(char *s)
{ printerr_regmem

  if ( interaction >= errorstopmode )
    wakeupterminal ();
  c_printnl("! ");
  c_print(s);
}


void p_print_err( strnumber s )
{ printerr_regmem

  if ( interaction >= errorstopmode )
    wakeupterminal ();
  c_printnl("! ");
  print(s);
}


#ifdef STRINGBUG_FIX
void slow_print_err( strnumber s )
{ printerr_regmem

  if ( interaction >= errorstopmode )
    wakeupterminal();
  c_printnl("! ");
  slowprint(s);
}
#endif


/* wie `print'
 * nur dass direkt ein null-terminierender String uebergeben wird.
 * Ausserdem koennen auch '\n' Zeichen im String vorhanden sein.
 */
static void c_err_print ( char *s )
{ c_print_regmem

  while( *s != '\0' ) {
    if( *s == '\n' ) {
      println ();
    } else {
      printchar ( *s );
    }
    s++;
  }
}


void error ( void )
{ error_regmem
  register ASCIIcode c;
  register integer s1, s2, s3, s4;

  if ( history < errormessageissued )
    history = errormessageissued;
  printchar( 46 );
  showcontext ();

  if ( interaction >= errorstopmode ) {
    while ( true ) {
lab22:
	clearforerrorprompt ();
#ifdef AMIGA
	if (do_rexx == 2) {
	  /* gegen Endlosschleife, wenn 'e' nicht verfuegbar ist */
	  do_rexx = 3;
	  /* es wird ein 'e' probiert und dann ein 'x' gemacht */
	  last = first;
	  buffer[last++] = 69;		/* e : simulate typed 'E' command */
	} else {
	  if (do_rexx == 3) {
	    last = first;
	    buffer[last++] = 88;	/* x : simulate typed 'X' command */
	  } else {
	    terminput("? ");
	  }
	}
#else
	terminput("? ");
#endif
	if ( last == first )
	  return;
	c = buffer[first];
	if ( c >= 97 )
	  c -= 32;

	switch ( c ) {
	case 48:  case 49:  case 50:  case 51:  case 52:
	case 53:  case 54:  case 55:  case 56:  case 57:
	  if ( deletionsallowed ) {
	    s1 = curtok;
	    s2 = curcmd;
	    s3 = curchr;
	    s4 = alignstate;
	    alignstate = 1000000L;
	    OKtointerrupt = false;
	    if ( ( last > first + 1 )
		&& ( buffer [ first + 1 ] >= 48 )
		&& ( buffer [ first + 1 ] <= 57 ) )
	      c = TEN_MULT(c) + buffer [ first + 1 ] - 48 * 11;
	    else
	      c = c - 48;
	    while ( c > 0 ) {
	      gettoken ();  
	      decr ( c );
	    }
	    curtok = s1;
	    curcmd = s2;
	    curchr = s3;
	    alignstate = s4;
	    OKtointerrupt = true;

	    zhelp1( STR_H_IHAVEJUSTDELETED );
	    showcontext();
	    goto lab22;
	  }
	  break;
#ifdef DEBUG
	case 68:	/* D */
	  debughelp();
	  goto lab22; 
#endif /* DEBUG */
	case 69:	/* E */
	  if ( baseptr > 0 ) {
	    editnamestart = strstart [ inputstack[baseptr].namefield ];
	    editnamelength = strstart [ inputstack[baseptr].namefield + 1 ]
			   - strstart [ inputstack[baseptr].namefield ];
	    editline = line;
	    jumpout ();
	  }
	  break;
	case 72:	/* H */
	  if ( useerrhelp ) {
	    giveerrhelp();
	    useerrhelp = false;
	  } else {
#ifdef NEW_HELP
	    if ( *helpptr == 0 )
	      zhelp2( STR_H_SORRY_DONTKNOWHELP, STR_H_MAYBE_ASK_HUMAN );

	    do {
	      if( (int) *helpptr < (int)256 ) {
		c_err_print ( help_messages[(int) *helpptr++] );
	      } else {
	        print ( (int) *helpptr++ );
	      }
	      println ();
	    } while ( *helpptr != 0 );
#else
	    if ( helpptr == 0 )
	      zhelp2( STR_H_SORRY_DONTKNOWHELP, STR_H_MAYBE_ASK_HUMAN );
	    do {
	      decr ( helpptr );
	      if( helpline[helpptr] < (int)256 ) {
		c_err_print ( help_messages[helpline[helpptr]] );
	      } else {
		print ( helpline[helpptr] );
	      }
	      println ();
	    } while ( helpptr != 0 );
#endif
	  }
	  zhelp3( STR_H_SORRY_GAVEHELP, STR_H_MAYBE_ASK_HUMAN,
		  STR_H_ANERRORMIGHT );
	  goto lab22;
	case 73:	/* I */
	  beginfilereading ();
	  if ( last > first + 1 ) {
	    curinput .locfield = first + 1;
	    buffer [ first ] = 32;
	  } else {
	    terminput("insert>");
	    curinput .locfield = first;
	  }
	  first = last;
	  curinput.limitfield = last - 1;
	  return;

	case 81:	/* Q, R, S */
	case 82:
	case 83:
	  errorcount = 0;
#ifdef ERW_INTERACTION
	  geqworddefine(intbase + interactionmodecode, batchmode + c - 81);
#else
	  interaction = batchmode + c - 81;
#endif
	  c_print("OK, entering ");
	  switch ( c ) {
	  case 81:
	    printesc ( 272 );
	    decr ( selector );
	    break;
	  case 82:
	    printesc ( 273 );
	    break;
	  case 83:
	    printesc ( 274 );
	    break;
	  }
	  print ( 275 );
	  println ();
	  flush ( stdout );
	  return;
 	case 88:	/* X */
#ifdef ERW_INTERACTION
	  geqworddefine(intbase + interactionmodecode, scrollmode);
#else
	  interaction = scrollmode;
#endif
	  jumpout();
	  break;
	default:
	  break;
	}

	c_print("Type <return> to proceed, S to scroll future error messages,");
	c_printnl("R to run without stopping, Q to run quietly,");
	c_printnl("I to insert something, ");
	if ( baseptr > 0 )
	  c_print("E to edit your file,");
	if ( deletionsallowed )
	  c_printnl("1 or ... or 9 to ignore the next 1 to 9 tokens of input,");
	c_printnl("H for help, X to quit.");
    }
  }
  incr ( errorcount );
  if ( errorcount == 100 ) {
    c_printnl("(That makes 100 errors; please try again.)");
    history = fatalerrorstop;
    jumpout ();
  }
  if ( interaction > batchmode )
    decr ( selector );
  if ( useerrhelp ) {
    println ();
    giveerrhelp ();
  } else {
#ifdef NEW_HELP
    while ( *helpptr > 0 ) {
      c_printnl("");
      if( (int) *helpptr < (int)256 ) {
	c_err_print ( help_messages[(int) *helpptr++] );
      } else {
	print( (int) *helpptr++ );
      }
    }
#else
    while ( helpptr > 0 ) {
      decr ( helpptr );
      c_printnl("");
      if( helpline[helpptr] < (int)256 ) {
	c_err_print ( help_messages[helpline[helpptr]] );
      } else {
	print ( helpline[helpptr] );
      }
    }
#endif
  }
  println ();
  if ( interaction > batchmode )
    incr ( selector );
  println ();
}


VOLATILE void fatalerror ( strnumber s )
{ fatalerror_regmem

  normalizeselector();

  print_err("Emergency stop");
#ifdef NEW_HELP
  { static short tmp[] = { 0, 0 };
    tmp[0] = s;		/* zhelp1(s); */
    helpptr = tmp;
  }
#else
  zhelp1( s );
#endif
  succumb();
}


#if 0		/* verschoben nach `overflow.c' */
VOLATILE void overflow ( strnumber s, integer n )
#endif


VOLATILE void confusion ( char *s )
{ confusion_regmem

  normalizeselector ();
  if ( history < errormessageissued ) {
    print_err("This can't happen (");
    c_print ( s );
    printchar ( 41 );
    zhelp1( STR_H_IMBROKEN_FIX );
  } else {
    print_err("I can't go on meeting you like this");
    zhelp1( STR_H_ONEOFYOURFAUXPAS );
  }
  succumb ();
}


boolean initterminal ( void )
{ initterminal_regmem

  topenin ();
  if ( last > first ) {
    curinput .locfield = first;
    while ( curinput.locfield < last && buffer[curinput.locfield] == ' ' )
	incr ( curinput .locfield );
    if ( curinput .locfield < last )
      return(true);
  }
  while ( true ) {
    wakeupterminal ();
    (void) Fputs( stdout, "**" );
    flush ( stdout );
    if ( ! inputln ( stdin, true ) ) {
      (void) Fputs( stdout, "\n! End of file on the terminal... why?" );
      return(false);
    }
    curinput .locfield = first;
    while ( curinput.locfield < last && buffer[curinput.locfield] == 32 )
	incr ( curinput .locfield );
    if ( curinput .locfield < last )
      return(true);
    (void) printf("Please type the name of your input file.\n");
  }
}


strnumber makestring ( void )
{ makestring_regmem

  if ( strptr == maxstrings )
    overflow(2, maxstrings - initstrptr); 
  incr ( strptr );
  strstart [ strptr ] = poolptr;
  return(strptr - 1);
}


#if 0      /* (br) both replaced by strncmp() */
boolean zstreqbuf ( strnumber s, integer k );
boolean zstreqstr ( strnumber s, strnumber t );
#endif


void printtwo ( integer n )
{ printtwo_regmem
  ldiv_t result;

  /* n = abs ( n ) % 100 ; */
  if( n < 0 )  n = -n;
  while( n > 100 )
    n -= 100;
  result = ldiv(n, 10);
  printchar ( 48 + result.quot );	/* ( n / 10 ) */
  printchar ( 48 + result.rem );	/* ( n % 10 ) */
}

void printhex ( integer n )
{ printhex_regmem
  register schar k;

  k = 0;
  printchar ( 34 );
  do {
    dig [ k ] = n % 16;
    n = n / 16;
    incr ( k );
  } while ( ! ( n == 0 ) );
  printthedigs ( k );
}

#if 0
void printromanint ( integer n )
{ printromanint_regmem
  register poolpointer j, k;
  register nonnegativeinteger u, v;

  j = strstart [ 260 ];
  v = 1000;
  while ( true ) {
    while ( n >= v ) {
      printchar( strpool[j] );
      n -= v;
    }
    if ( n <= 0 )
      return;
    k = j + 2;
    u = v / ( strpool [ k - 1 ] - 48 );
    if ( strpool [ k - 1 ] == 50 ) {
      k += 2;
      u /= ( strpool [ k - 1 ] - 48 );
    }
    if ( n + u >= v ) {
      printchar ( strpool [ k ] );
      n += u;
    } else {
      j += 2;
      v /= ( strpool [ j - 1 ] - 48 );
    }
  }
}

#else

void printromanint ( integer n )
{ printromanint_regmem
  register unsigned char *k, *j;
  register nonnegativeinteger u, v;
  unsigned char roman[] =
	{ 'm', 2, 'd', 5, 'c', 2, 'l', 5, 'x', 2, 'v', 5, 'i', 0 };

  /* j = "m2d5c2l5x2v5i"; */
  j = roman;

  v = 1000;
  while ( true ) {
    while ( n >= v ) {
      printchar( *j );
      n -= v;
    }
    if ( n <= 0 )
      return;

    k = j + 1;
    u = v / *k;
    if( *k == 2 ) {
	k += 2;
	u /= *k;
    }
    k++;

    if ( n + u >= v ) {
      printchar( *k );
      n += u;
    } else {
      j++;
      v /= *j;
      j++;
    }
  }
}
#endif


void printcurrentstring ( void )
{ printcurrentstring_regmem
  register poolpointer j;

  j = strstart [ strptr ];
  while ( j < poolptr ) {
    printchar ( strpool [ j ] );
    incr ( j );
  }
}


void terminput ( char *s )
{ terminput_regmem
  register integer k;

  wakeupterminal(); 
  c_print( s );
  flush ( stdout );
  if ( ! inputln ( stdin , true ) )
    fatalerror( STR_H_FE_ENDOFFILE );
  termoffset = 0;
  decr ( selector );
  if ( last != first )
    for( k = first ; k <= last - 1 ; k++ ) {
	print ( buffer [ k ] );
    }
  println ();
  incr ( selector );
}


void interror ( integer n )
{ interror_regmem

  c_print(" (");
  printint ( n );
  printchar( 41 );
  error () ;
}

void normalizeselector ( void )
{ normalizeselector_regmem

  selector = (logopened) ? termandlog : termonly;
  if ( jobname == 0 )
    openlogfile ();
  if ( interaction <= batchmode )
    decr ( selector );
}

VOLATILE void succumb ( void )
{ succumb_regmem

  if ( interaction >= errorstopmode ) {
#ifdef ERW_INTERACTION
    geqworddefine(intbase + interactionmodecode, scrollmode);
#else
    interaction = scrollmode;
#endif
  }
  if ( logopened )
    error ();
#ifdef DEBUG
  if ( interaction > batchmode )
    debughelp ();
#endif /* DEBUG */
  history = fatalerrorstop;
  jumpout ();
}

void pauseforinstructions ( void )
{ pauseforinstructions_regmem

  if ( OKtointerrupt ) {
#ifdef ERW_INTERACTION
    geqworddefine(intbase + interactionmodecode, errorstopmode);
#else
    interaction = errorstopmode;
#endif
    if ( ( selector == logonly ) || ( selector == noprint ) )
      incr ( selector );

    print_err("Interruption");
    zhelp1( STR_H_YOURANG );
    deletionsallowed = false;
    error();
    deletionsallowed = true ;
    interrupt = 0;
  }
}



#ifdef DEBUG
void zprintword ( memoryword w )
{ printword_regmem

  printint ( w .cint ) ;
  printchar ( 32 ) ;
  printscaled ( w .cint ) ;
  printchar ( 32 ) ;
  printscaled ( round ( unity * w .gr ) ) ;
  println () ;
  printint ( w .hh .v.LH ) ;
  printchar ( 61 ) ;
  printint ( w .hh.b0 ) ;
  printchar ( 58 ) ;
  printint ( w .hh.b1 ) ;
  printchar ( 59 ) ;
  printint ( w .hh .v.RH ) ;
  printchar ( 32 ) ;
  printint ( w .qqqq .b0 ) ;
  printchar ( 58 ) ;
  printint ( w .qqqq .b1 ) ;
  printchar ( 58 ) ;
  printint ( w .qqqq .b2 ) ;
  printchar ( 58 ) ;
  printint ( w .qqqq .b3 ) ;
}
#endif /* DEBUG */


void showtokenlist ( integer p, integer q, integer l )
{ showtokenlist_regmem
  register integer m, c;
  register ASCIIcode matchchr;
  register ASCIIcode n;

  matchchr = 35;
  n = 48;
  tally = 0;
  while ( ( p != 0 ) && ( tally < l ) ) {
    if ( p == q ) {
      firstcount = tally;
      trickcount = tally + 1 + errorline - halferrorline;
      if ( trickcount < errorline )
	trickcount = errorline;
    }
    if ( ( p < himemmin ) || ( p > memend ) ) {
      c_printesc("CLOBBERED.");
      return;
    }
    if ( info ( p ) >= cstokenflag )
      printcs ( info ( p ) - cstokenflag );
    else {
      m = info ( p ) / 256;
      c = info ( p ) % 256;
      if ( info ( p ) < 0 )
	c_printesc("BAD.");
      else switch ( m ) {
      case left_brace:	case right_brace:	case math_shift:
      case tab_mark:	case sup_mark:		case sub_mark:
      case spacer:	case letter:		case otherchar:
	print ( c );
	break;
      case mac_param:
	print ( c );  print ( c );
	break;
      case out_param:
	print ( matchchr );
	if ( c <= 9 )
	  printchar ( c + 48 );
	else {
	  printchar ( 33 );
	  return;
	}
	break;
      case begin_match:
	matchchr = c;
	print ( c );
	incr ( n );
	printchar ( n );
	if ( n > 57 )
	  return;
	break;
      case end_match:
	c_print("->");
	break;
      default:
	c_printesc("BAD.");
	break;
      }
    }
    p = link ( p );
  }
  if ( p != 0 )
    c_printesc("ETC.");
}


void STDARGS runaway ( void )
{ runaway_regmem
  register halfword p;

  if ( scannerstatus > skipping ) {
    c_printnl("Runaway ");
    switch ( scannerstatus ) {
    case defining:
	c_print("definition");
	p = defref;
	break;
    case matching:
	c_print("argument");
	p = temphead;
	break;
    case aligning:
	c_print("preamble");
	p = holdhead;
	break;
    case absorbing:
	c_print("text");
	p = defref;
	break;
    }
    printchar ( 63 );
    println ();
    showtokenlist ( link(p), 0, errorline - 10 );
  }
}


/* memory management routines -> getmem.c */


long_halfword newnullbox ( void )
{ register long_halfword p;

  p = getnode ( boxnodesize );

  { newnullbox_regmem

/* ztype ( p ) = hlistnode ;  subtype ( p ) = 0 ; */
  set_type_subtype(p, hlistnode, 0);

  width ( p ) = 0;
  depth ( p ) = 0;
  height ( p ) = 0;
  shiftamount ( p ) = 0;
  listptr ( p ) = 0;

/* gluesign ( p ) = normal ;  glueorder ( p ) = normal ; */
  set_gluesign_order(p, normal, normal);

  glueset ( p ) = 0.0;

  }
  return(p);
}

long_halfword newrule ( void )
{ register long_halfword p;

  p = getnode ( rulenodesize );

  { newrule_regmem

/* ztype ( p ) = rulenode ;  subtype ( p ) = 0 ; */
  set_type_subtype(p, rulenode, 0);

  width ( p ) = nullflag;
  depth ( p ) = nullflag;
  height ( p ) = nullflag;
  }

  return(p);
}

long_halfword newligature ( quarterword f, quarterword c, halfword q )
{ register long_halfword p;

  p = getnode ( smallnodesize );

  { newligature_regmem

/* ztype ( p ) = ligaturenode ;  subtype ( p ) = 0 ; */
  set_type_subtype(p, ligaturenode, 0);

  font ( ligchar(p) ) = f;  character ( ligchar(p) ) = c;
  /* ist besser als: */
  /* set_font_character(ligchar(p), f, c); */

  ligptr ( p ) = q;
  }
  return(p);
}

long_halfword newligitem ( quarterword c )
{ register long_halfword p;

  p = getnode ( smallnodesize );

  { newligitem_regmem

  character ( p ) = c;
  ligptr ( p ) = 0;
  }
  return(p);
}

long_halfword newdisc ( void )
{ register long_halfword p;

  p = getnode ( smallnodesize );

  { newdisc_regmem

/* ztype ( p ) = discnode ;  replacecount ( p ) = 0 ; */
  set_type_replacecount(p, discnode, 0);
  prebreak ( p ) = 0;
  postbreak ( p ) = 0;
  }
  return(p);
}

long_halfword newmath ( scaled w, smallnumber s )
{ register long_halfword p;

  p = getnode ( smallnodesize );

  { newmath_regmem

/* ztype ( p ) = mathnode ;  subtype ( p ) = s ; */
  set_type_subtype(p, mathnode, s);

  width ( p ) = w;
  }
  return(p);
}

long_halfword newspec ( halfword p )
{ register long_halfword q;

  q = getnode ( gluespecsize );

  { newspec_regmem

  mem [ q ] = mem [ p ];
  gluerefcount ( q ) = 0;
  width ( q ) = width ( p );
  stretch ( q ) = stretch ( p );
  shrink ( q ) = shrink ( p );
  }
  return(q);
}

long_halfword newparamglue ( smallnumber n )
{ register long_halfword p;

  p = getnode ( smallnodesize );

  { newparamglue_regmem
    register halfword q;

/* ztype ( p ) = gluenode ;  subtype ( p ) = n + 1 ; */
  set_type_subtype(p, gluenode, n+1);

  leaderptr ( p ) = 0;
  q = gluepar ( n );
  glueptr ( p ) = q;
  incr ( gluerefcount(q) );
  }
  return(p);
}

long_halfword newglue ( halfword q )
{ register long_halfword p;

  p = getnode ( smallnodesize );

  { newglue_regmem

/* ztype ( p ) = gluenode ;  subtype ( p ) = normal ; */
  set_type_subtype(p, gluenode, normal);

  leaderptr ( p ) = 0;
  glueptr ( p ) = q;
  incr ( gluerefcount(q) );
  }
  return(p);
}

long_halfword newskipparam ( smallnumber n )
{ newskipparam_regmem

  tempptr = newspec ( gluepar ( n ) );

  { register long_halfword p;

  p = newglue ( tempptr );
  gluerefcount ( tempptr ) = 0;
  subtype ( p ) = n + 1;

  return(p);
  }
}

long_halfword newkern ( scaled w )
{ register long_halfword p;

  p = getnode ( smallnodesize );

  { newkern_regmem

/* ztype ( p ) = kernnode ;  subtype ( p ) = normal ; */
  set_type_subtype(p, kernnode, normal);

  width ( p ) = w;
  }
  return(p);
}

long_halfword newpenalty ( integer m )
{ register long_halfword p;

  p = getnode ( smallnodesize );

  { newpenalty_regmem

/* ztype ( p ) = penaltynode ;  subtype ( p ) = 0 ; */
  set_type_subtype(p, penaltynode, 0);

  mem [ p + 1 ] .cint = m;
  }
  return(p);
}


#ifdef TEXXET
long_halfword new_edge( smallnumber s, scaled w )
{ register long_halfword p;

  p = getnode(edge_node_size);

  { new_edge_regmem

  ztype(p) = edge_node; subtype(p) = s;
  width(p) = w;
  }
  return(p);
}
#endif


#ifdef DEBUG
void zcheckmem ( boolean printlocs )
{/* 31 32 */ checkmem_regmem
  register halfword p, q;
  boolean clobbered;

  {register integer for_end; p = memmin ; for_end = lomemmax ; if ( p <=
  for_end) do
    freearr [ p ] = false ;
  while ( p++ < for_end ) ; }

  {register integer for_end; p = himemmin ; for_end = memend ; if ( p <=
  for_end) do
    freearr [ p ] = false ;
  while ( p++ < for_end ) ; }

  p = avail ;
  q = 0 ;
  clobbered = false ;
  while ( p != 0 ) {
    if ( ( p > memend ) || ( p < himemmin ) )
	clobbered = true ;
    else if ( freearr [ p ] )
	clobbered = true ;
    if ( clobbered ) {
      c_printnl("VAIL list clobbered at ");
      printint ( q ) ;
      goto lab31 ;
    }
    freearr [ p ] = true ;
    q = p ;
    p = link ( q ) ;
  }
lab31:
  p = rover ;
  q = 0 ;
  clobbered = false ;
  do {
    if ( ( p >= lomemmax ) || ( p < memmin ) )
      clobbered = true ;
    else if ( ( rlink ( p ) >= lomemmax ) || ( rlink ( p ) < memmin ) )
      clobbered = true ;
    else if ( ! ( isempty ( p ) ) || ( nodesize ( p ) < 2 ) || ( p + nodesize
    ( p ) > lomemmax ) || ( llink ( rlink ( p ) ) != p ) )
      clobbered = true ;
    if ( clobbered ) {
      c_printnl("Double-AVAIL list clobbered at ");
      printint ( q ) ;
      goto lab32 ;
    }
    {register integer for_end; q = p ; for_end = p + nodesize ( p ) - 1
    ; if ( q <= for_end) do
      {
	if ( freearr [ q ] ) {
	  c_printnl("Doubly free location at ");
	  printint ( q ) ;
	  goto lab32 ;
	}
	freearr [ q ] = true ;
      }
    while ( q++ < for_end ) ; }
    q = p ;
    p = rlink ( p ) ;
  } while ( ! ( p == rover ) ) ;
lab32:
  p = memmin ;
  while ( p <= lomemmax ) {
    if ( isempty ( p ) ) {
      c_printnl("Bad flag at ");
      printint ( p ) ;
    }
    while ( ( p <= lomemmax ) && ! freearr [ p ] ) incr ( p ) ;
    while ( ( p <= lomemmax ) && freearr [ p ] ) incr ( p ) ;
  }
  if ( printlocs ) {
    c_printnl("New busy locs:");
    {register integer for_end; p = memmin ; for_end = lomemmax ; if ( p <=
    for_end) do
      if ( ! freearr [ p ] && ( ( p > waslomax ) || wasfree [ p ] ) ) {
	printchar ( 32 ) ;
	printint ( p ) ;
      }
    while ( p++ < for_end ) ; }
    {register integer for_end; p = himemmin ; for_end = memend ; if ( p <=
    for_end) do
      if ( ! freearr [ p ] && ( ( p < washimin ) || ( p > wasmemend ) ||
      wasfree [ p ] ) )
      {
	printchar ( 32 ) ;
	printint ( p ) ;
      }
    while ( p++ < for_end ) ; }
  }
  {register integer for_end; p = memmin ; for_end = lomemmax ; if ( p <=
  for_end) do
    wasfree [ p ] = freearr [ p ] ;
  while ( p++ < for_end ) ; }
  {register integer for_end; p = himemmin ; for_end = memend ; if ( p <=
  for_end) do
    wasfree [ p ] = freearr [ p ] ;
  while ( p++ < for_end ) ; }
  wasmemend = memend ;
  waslomax = lomemmax ;
  washimin = himemmin ;
}
#endif /* DEBUG */

#ifdef DEBUG
void searchmem ( halfword p )
halfword p ;
{searchmem_regmem
  register integer q  ;

  {register integer for_end; q = memmin ; for_end = lomemmax ; if ( q <=
  for_end) do
    {
      if ( link ( q ) == p ) {
	c_printnl("LINK(");
	printint ( q ) ;
	printchar ( 41 ) ;
      }
      if ( info ( q ) == p ) {
	c_printnl("INFO(");
	printint ( q ) ;
	printchar ( 41 ) ;
      }
    }
  while ( q++ < for_end ) ; }
  {register integer for_end; q = himemmin ; for_end = memend ; if ( q <=
  for_end) do
    {
      if ( link ( q ) == p ) {
	c_printnl("LINK(");
	printint ( q ) ;
	printchar ( 41 ) ;
      }
      if ( info ( q ) == p ) {
	c_printnl("INFO(");
	printint ( q ) ;
	printchar ( 41 ) ;
      }
    }
  while ( q++ < for_end ) ; }
  {register integer for_end; q = activebase ; for_end = boxbase + 255 ; if (
  q <= for_end) do
    {
      if ( equiv ( q ) == p ) {
	c_printnl("EQUIV(");
	printint ( q ) ;
	printchar ( 41 ) ;
      }
    }
  while ( q++ < for_end ) ; }
  if ( saveptr > 0 )
  {register integer for_end; q = 0 ; for_end = saveptr - 1 ; if ( q <=
  for_end) do
    {
      if ( equivfield ( savestack [ q ] ) == p ) {
	c_printnl("SAVE(");
	printint ( q ) ;
	printchar ( 41 ) ;
      }
    }
  while ( q++ < for_end ) ; }
  {register integer for_end; q = 0 ; for_end = hyphsize ; if ( q <= for_end)
  do
    {
      if ( hyphlist(q) == p ) {
	c_printnl("HYPH(");
	printint ( q ) ;
	printchar ( 41 ) ;
      }
    }
  while ( q++ < for_end ) ; }
}
#endif /* DEBUG */

void shortdisplay ( integer p )
{ shortdisplay_regmem

  while ( p > memmin ) {
    if ( ischarnode ( p ) ) {
      if ( p <= memend ) {
	if ( font ( p ) != fontinshortdisplay ) {
	  if ( ( font(p) > fontmax ) )
	  printchar ( 42 );
	  else printesc ( fontidtext ( font(p) ) );
	  printchar ( 32 );
	  fontinshortdisplay = font ( p );
	}
	print ( character(p) );
      }
    }
    else switch ( ztype ( p ) )
    {case hlistnode:
    case vlistnode:
    case insnode:
    case whatsitnode:
    case marknode:
    case adjustnode:
    case unsetnode:
      c_print("[]");
      break;
    case rulenode:
      printchar ( 124 );
      break;
    case gluenode:
      if ( glueptr ( p ) != zeroglue )
      printchar ( 32 );
      break;
    case mathnode:
#ifdef TEXXET
      if( subtype(p) > after )
	c_print("[]");
      else
#endif
        printchar ( 36 );  /* "$" */
      break;
    case ligaturenode:
      shortdisplay ( ligptr(p) );
      break;
    case discnode:
      shortdisplay ( prebreak(p) );
      shortdisplay ( postbreak(p) );
      { register integer n;

	n = replacecount ( p );
	while ( n > 0 ) {
	  if ( link ( p ) != 0 )
	    p = link ( p );
	  decr ( n );
	}
      }
      break;
    default:
      break;
    }
    p = link ( p );
  }
}

void printfontandchar ( integer p )
{ printfontandchar_regmem

  if ( p > memend )
    c_printesc("CLOBBERED.");
  else {
    if ( ( font ( p ) > fontmax ) )
      printchar ( 42 );
    else
      printesc ( fontidtext ( font ( p ) ) );
    printchar ( 32 );
    print ( character ( p ) );
  }
}

void printmark ( integer p )
{ printmark_regmem

  printchar ( 123 );
  if ( ( p < himemmin ) || ( p > memend ) )
    c_printesc("CLOBBERED.");
  else
    showtokenlist ( link(p), 0, maxprintline - 10 );
  printchar ( 125 );
}

void printruledimen ( scaled d )
{ printruledimen_regmem

  if ( isrunning ( d ) )
    printchar ( 42 );
  else
    printscaled ( d );
}

void printglue ( scaled d, integer order, strnumber s )
{ printglue_regmem

  printscaled ( d );
  if ( ( order < normal ) || ( order > filll ) )
    c_print("foul");
  else if ( order > normal ) {
    print( STR_FIL );
    while ( order > fil ) {
      printchar ( 108 );
      decr ( order );
    }
  } else if ( s != 0 )
    print ( s );
}

void printspec ( integer p, strnumber s )
{ printspec_regmem

  if ( ( p < memmin ) || ( p >= lomemmax ) )
    printchar ( 42 );
  else {
    printscaled ( width ( p ) );
    if ( s != 0 )
      print ( s );
    if ( stretch ( p ) != 0 ) {
      print( STR_PLUS_ );
      printglue ( stretch ( p ), stretchorder ( p ), s );
    }
    if ( shrink ( p ) != 0 ) {
      print( STR_MINUS_ );
      printglue ( shrink ( p ), shrinkorder ( p ), s );
    }
  }
}

void printfamandchar ( halfword p )
{ printfamandchar_regmem

  printesc ( 460 );
  printint ( fam ( p ) );
  printchar ( 32 );
  print ( character ( p ) );
}

void printdelimiter ( halfword p )
{ printdelimiter_regmem
  register integer a;

  a = smallfam ( p ) * 256 + smallchar ( p );
  a = a * 4096 + largefam ( p ) * 256 + largechar ( p );
  if ( a < 0 )
    printint ( a );
  else
    printhex ( a );
}

void printsubsidiarydata ( halfword p, ASCIIcode c )
{ printsubsidiarydata_regmem

  if ( curlength >= depththreshold ) {
    if ( mathtype ( p ) != 0 )
    c_print(" []");
  } else {
    appendchar ( c ) ;
    /* tempptr = p ; */
    switch ( mathtype ( p ) ) {
    case mathchar:
	println ();
	printcurrentstring ();
	printfamandchar ( p );
	break;
    case subbox:
      /* showinfo () ; */   shownodelist( info (p) );
      break;
    case submlist:
      if ( info ( p ) == 0 ) {
	println ();
	printcurrentstring ();
	c_print("{}");
      } else {
	/* showinfo () ; */   shownodelist( info (p) );
      }
      break;
    default:
      break;
    }
    flushchar;
  }
}

void printstyle ( integer c )
{ printstyle_regmem

  switch ( c / 2 ) {
  case 0:
    printesc( STR_DISPLAYSTYLE ); 
    break;
  case 1:
    printesc( STR_TEXTSTYLE );
    break;
  case 2:
    printesc( STR_SCRIPTSTYLE );
    break;
  case 3:
    printesc( STR_SCRIPTSCRIPTSTYLE );
    break;
  default:
    c_print("Unknown style!");
    break;
  }
}

void printskipparam ( integer n )
{ printskipparam_regmem

  switch ( n ) {
  case lineskipcode :
    printesc ( 372 ) ;
    break ;
  case baselineskipcode :
    printesc ( 373 ) ;
    break ;
  case parskipcode :
    printesc ( 374 ) ;
    break ;
  case abovedisplayskipcode :
    printesc ( 375 ) ;
    break ;
  case belowdisplayskipcode :
    printesc ( 376 ) ;
    break ;
  case abovedisplayshortskipcode :
    printesc ( 377 ) ;
    break ;
  case belowdisplayshortskipcode :
    printesc ( 378 ) ;
    break ;
  case leftskipcode :
    printesc ( 379 ) ;
    break ;
  case rightskipcode :
    printesc ( 380 ) ;
    break ;
  case topskipcode :
    printesc ( 381 ) ;
    break ;
  case splittopskipcode :
    printesc ( 382 ) ;
    break ;
  case tabskipcode :
    printesc ( 383 ) ;
    break ;
  case spaceskipcode :
    printesc ( 384 ) ;
    break ;
  case xspaceskipcode :
    printesc ( 385 ) ;
    break ;
  case parfillskipcode :
    printesc ( 386 ) ;
    break ;
  case thinmuskipcode :
    printesc ( 387 ) ;
    break ;
  case medmuskipcode :
    printesc ( 388 ) ;
    break ;
  case thickmuskipcode :
    printesc ( 389 ) ;
    break ;
  default:
    print ( 390 ) ;
    break ;
  }
}

void shownodelist ( integer p )
{ shownodelist_regmem
  register integer n  ;
  real g  ;

  if ( curlength > depththreshold ) {
    if ( p > 0 )
    c_print(" []");
    return ;
  }
  n = 0 ;
  while ( p > memmin ) {
    println () ;
    printcurrentstring () ;
    if ( p > memend ) {
      c_print("Bad link, display aborted.");
      return ;
    }
    incr ( n ) ;
    if ( n > breadthmax ) {
      c_print("etc.");
      return ;
    }
    if ( ischarnode ( p ) )
      printfontandchar ( p ) ;
    else switch ( ztype ( p ) ) {
    case hlistnode :
    case vlistnode :
    case unsetnode :
      {
	if ( ztype ( p ) == hlistnode )
	  printesc ( 104 ) ;
	else if ( ztype ( p ) == vlistnode )
	  printesc ( 118 ) ;
	else
	  c_printesc("unset");
	c_print("box(");
	printscaled ( height ( p ) ) ;
	printchar ( 43 ) ;
	printscaled ( depth ( p ) ) ;
	c_print(")x");
	printscaled ( width ( p ) ) ;
	if ( ztype ( p ) == unsetnode ) {
	  if ( spancount ( p ) != 0 ) {
	    c_print(" (");
	    printint ( spancount ( p ) + 1 ) ;
	    c_print(" columns)");
	  }
	  if ( gluestretch ( p ) != 0 ) {
	    c_print(", stretch ");
	    printglue ( gluestretch ( p ) , glueorder ( p ) , 0 ) ;
	  }
	  if ( glueshrink ( p ) != 0 ) {
	    c_print(", shrink ");
	    printglue ( glueshrink ( p ) , gluesign ( p ) , 0 ) ;
	  }
	} else {
	  g = glueset ( p ) ;
	  if ( ( g != 0.0 ) && ( gluesign ( p ) != normal ) ) {
	    c_print(", glue set ");
	    if ( gluesign ( p ) == shrinking )
	      c_print("- ");
	    if ( fabs ( g ) > 20000.0 ) {
	      if ( g > 0.0 )
		printchar ( 62 ) ;
	      else
		c_print("< -");
	      printglue ( 20000 * unity , glueorder ( p ) , 0 ) ;
	    } else
	      printglue ( round ( unity * g ) , glueorder ( p ) , 0 ) ;
	  }
	  if ( shiftamount ( p ) != 0 ) {
	    c_print(", shifted ");
	    printscaled ( shiftamount ( p ) ) ;
	  }
	}
	{
	  appendchar ( 46 ) ;
	  shownodelist ( listptr ( p ) ) ;
	  flushchar ;
	}
      }
      break ;
    case rulenode :
      {
	c_printesc("rule(");
	printruledimen ( height ( p ) ) ;
	printchar ( 43 ) ;
	printruledimen ( depth ( p ) ) ;
	c_print(")x");
	printruledimen ( width ( p ) ) ;
      }
      break ;
    case insnode :
      {
	printesc( STR_INSERT );
	printint ( subtype ( p ) ) ;
	c_print(", natural size ");
	printscaled ( height ( p ) ) ;
	c_print("; split(");
	printspec ( splittopptr ( p ) , 0 ) ;
	printchar ( 44 ) ;
	printscaled ( depth ( p ) ) ;
	c_print("); float cost ");
	printint ( floatcost ( p ) ) ;
	{
	  appendchar ( 46 ) ;
	  shownodelist ( insptr ( p ) ) ;
	  flushchar ;
	}
      }
      break ;
    case whatsitnode :
      switch ( subtype ( p ) ) {
      case opennode :
	printwritewhatsit ( STR_OPENOUT , p ) ;
	printchar ( 61 ) ;
	printfilename ( openname ( p ) , openarea ( p ) , openext ( p ) ) ;
	break ;
      case writenode :
	printwritewhatsit ( STR_WRITE, p ) ;
	printmark ( writetokens ( p ) ) ;
	break ;
      case closenode :
	printwritewhatsit ( STR_CLOSEOUT , p ) ;
	break ;
      case specialnode :
	printesc ( STR_SPECIAL ) ;
	printmark ( writetokens ( p ) ) ;
	break ;
      case languagenode :
	printesc ( STR_SETLANGUAGE );
	printint ( whatlang ( p ) ) ;
	c_print(" (hyphenmin ");
	printint ( whatlhm ( p ) ) ;
	printchar ( 44 ) ;
	printint ( whatrhm ( p ) ) ;
	printchar ( 41 ) ;
	break ;
      default:
	c_print("whatsit?");
	break ;
      }
      break ;
    case gluenode :
      if ( subtype ( p ) >= aleaders ) {
	c_printesc(""); 
	if ( subtype ( p ) == cleaders )
	  printchar ( 99 ) ;
	else if ( subtype ( p ) == xleaders )
	  printchar ( 120 ) ;
	c_print("leaders ");
	printspec ( glueptr ( p ) , 0 ) ;
	{
	  appendchar ( 46 ) ;
	  shownodelist ( leaderptr ( p ) ) ;
	  flushchar ;
	}
      } else {
	printesc ( 331 ) ;
	if ( subtype ( p ) != normal ) {
	  printchar ( 40 ) ;
	  if ( subtype ( p ) < condmathglue )
	    printskipparam ( subtype ( p ) - 1 ) ;
	  else if ( subtype ( p ) == condmathglue )
	    printesc ( 332 ) ;
	  else
	    printesc ( 333 ) ;
	  printchar ( 41 ) ;
	}
	if ( subtype ( p ) != condmathglue ) {
	  printchar ( 32 ) ;
	  if ( subtype ( p ) < condmathglue )
	    printspec ( glueptr ( p ) , 0 ) ;
	  else
	    printspec( glueptr ( p ), STR_MU ); 
	}
      }
      break ;
    case kernnode :
      if ( subtype ( p ) != muglue ) {
	printesc ( 337 ) ;
	if ( subtype ( p ) != normal )
	  printchar ( 32 ) ;
	printscaled ( width ( p ) ) ;
	if ( subtype ( p ) == acckern )
	  c_print(" (for accent)");
      } else {
	printesc ( 339 ) ;
	printscaled ( width ( p ) ) ;
	print( STR_MU );
      }
      break ;
    case mathnode :
#ifdef TEXXET
      if( subtype(p) > after ) {
	c_printesc( end_LR(p) ? "end" : "begin" );
	printchar( (subtype(p) <= end_L_code) ? 'L' : 'R' );
      } else
#endif
      {
	printesc ( 340 ) ;  /* "math" */
	if ( subtype ( p ) == before )
	  c_print("on");
	else
	  c_print("off");
	if ( width ( p ) != 0 ) {
	  c_print(", surrounded ");
	  printscaled ( width ( p ) ) ;
	}
      }
      break ;
    case ligaturenode :
      {
	printfontandchar ( ligchar ( p ) ) ;
	c_print(" (ligature ");
	if ( subtype ( p ) > 1 )
	  printchar ( 124 ) ;
	fontinshortdisplay = font ( ligchar ( p ) ) ;
	shortdisplay ( ligptr ( p ) ) ;
	if ( odd ( subtype ( p ) ) )
	  printchar ( 124 ) ;
	printchar ( 41 ) ;
      }
      break ;
    case penaltynode :
      {
	printesc ( 345 ) ;
	printint ( mem [ p + 1 ] .cint ) ;
      }
      break ;
    case discnode :
      {
	printesc ( 346 ) ;
	if ( replacecount ( p ) > 0 ) {
	  c_print(" replacing ");
	  printint ( replacecount ( p ) ) ;
	}
	{
	  appendchar ( 46 ) ;
	  shownodelist ( prebreak ( p ) ) ;
	  flushchar ;
	}
	appendchar ( 124 ) ;
	shownodelist ( postbreak ( p ) ) ;
	flushchar ;
      }
      break ;
    case marknode :
      {
	printesc ( 348 ) ;
	printmark ( markptr ( p ) ) ;
      }
      break ;
    case adjustnode :
      {
	printesc ( 349 ) ;
	{
	  appendchar ( 46 ) ;
	  shownodelist ( adjustptr ( p ) ) ;
	  flushchar ;
	}
      }
      break ;
    case stylenode :
      printstyle ( subtype ( p ) ) ;
      break ;
    case choicenode :
      {
	printesc ( 521 ) ;
	appendchar ( 68 ) ;
	shownodelist ( displaymlist ( p ) ) ;
	flushchar ;
	appendchar ( 84 ) ;
	shownodelist ( textmlist ( p ) ) ;
	flushchar ;
	appendchar ( 83 ) ;
	shownodelist ( scriptmlist ( p ) ) ;
	flushchar ;
	appendchar ( 115 ) ;
	shownodelist ( scriptscriptmlist ( p ) ) ;
	flushchar ;
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
    case leftnoad :
    case rightnoad :
      {
	switch ( ztype ( p ) ) {
	case ordnoad :
	  printesc ( STR_MATHORD );
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
	case overnoad :
	  printesc ( STR_OVERLINE ) ;
	  break ;
	case undernoad :
	  printesc ( STR_UNDERLINE ) ;
	  break ;
	case vcenternoad :
	  printesc ( 535 ) ;
	  break ;
	case radicalnoad :
	  {
	    printesc ( 529 ) ;
	    printdelimiter ( leftdelimiter ( p ) ) ;
	  }
	  break ;
	case accentnoad :
	  {
	    printesc ( 504 ) ;
	    printfamandchar ( accentchr ( p ) ) ;
	  }
	  break ;
	case leftnoad :
	  {
	    printesc ( STR_LEFT ) ;
	    printdelimiter ( nucleus ( p ) ) ;
	  }
	  break ;
	case rightnoad :
	  {
	    printesc( STR_RIGHT ); 
	    printdelimiter ( nucleus ( p ) ) ;
	  }
	  break ;
	}
	if ( subtype ( p ) != normal )
	if ( subtype ( p ) == limits )
	  printesc ( STR_LIMITS ) ;
	else
	  printesc ( STR_NOLIMITS ) ;
	if ( ztype ( p ) < leftnoad )
	  printsubsidiarydata ( nucleus ( p ) , 46 ) ;
	printsubsidiarydata ( supscr ( p ) , 94 ) ;
	printsubsidiarydata ( subscr ( p ) , 95 ) ;
      }
      break ;
    case fractionnoad :
      {
	c_printesc("fraction, thickness ");
	if ( thickness ( p ) == defaultcode )
	  c_print("= default");
	else
	  printscaled ( thickness ( p ) ) ;
	if ( ( smallfam ( leftdelimiter ( p ) ) != 0 )
	  || ( smallchar ( leftdelimiter ( p ) ) != 0 )
	  || ( largefam ( leftdelimiter ( p ) ) != 0 )
	  || ( largechar ( leftdelimiter ( p ) ) != 0 ) )
	{
	  c_print(", left-delimiter ");
	  printdelimiter ( leftdelimiter ( p ) ) ;
	}
	if ( ( smallfam ( rightdelimiter ( p ) ) != 0 )
	  || ( smallchar ( rightdelimiter ( p ) ) != 0 )
	  || ( largefam ( rightdelimiter ( p ) ) != 0 )
	  || ( largechar ( rightdelimiter ( p ) ) != 0 ) )
	{
	  c_print(", right-delimiter ");
	  printdelimiter ( rightdelimiter ( p ) ) ;
	}
	printsubsidiarydata ( numerator ( p ) , 92 ) ;
	printsubsidiarydata ( denominator ( p ) , 47 ) ;
      }
      break ;
    default:
      c_print("Unknown node type!");
      break ;
    }
    p = link ( p ) ;
  }
}

/* -- end -- */
