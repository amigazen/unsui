#define EXTERN extern
#include "texd.h"

void newwhatsit ( smallnumber s, smallnumber w )
{  register long_halfword p;

  p = getnode ( w );
  { newwhatsit_regmem

    /* ztype ( p ) = whatsitnode; subtype ( p ) = s ; */
    set_type_subtype(p, whatsitnode, s);
    link ( curlist.tailfield ) = p;
    curlist.tailfield = p;
  }
}

void newwritewhatsit ( smallnumber w )
{ newwritewhatsit_regmem

  newwhatsit ( curchr , w );
 {register integer r_curval;

  if ( w != writenodesize ) 
    r_curval = scanfourbitint ();
  else {
    r_curval = scanint ();
    if ( r_curval < 0 )
      r_curval = 17;
    else if ( r_curval > 15 )
      r_curval = 16;
  }
  writestream ( curlist .tailfield ) = r_curval;
 }
}


void doextension ( void )
{ doextension_regmem 
  register halfword p;

  switch ( curchr ) {
  case opennode : 
    {
      newwritewhatsit ( opennodesize ) ; 
      scanoptionalequals () ; 
      scanfilename () ; 
      openname ( curlist .tailfield ) = curname ; 
      openarea ( curlist .tailfield ) = curarea ; 
      openext ( curlist .tailfield ) = curext ; 
    } 
    break ; 
  case writenode : 
    { register integer k;

      k = curcs ; 
      newwritewhatsit ( writenodesize ) ; 
      curcs = k ; 
      p = scantoks ( false , false ) ; 
      writetokens ( curlist .tailfield ) = defref ; 
    } 
    break ; 
  case closenode : 
    {
      newwritewhatsit ( writenodesize ) ; 
      writetokens ( curlist .tailfield ) = 0 ; 
    } 
    break ; 
  case specialnode : 
    {
      newwhatsit ( specialnode , writenodesize ) ; 
      writestream ( curlist .tailfield ) = 0 ; 
      p = scantoks ( false , true ) ; 
      writetokens ( curlist .tailfield ) = defref ; 
    } 
    break ; 
  case 4 : 
    {
      getxtoken () ; 
      if ( ( curcmd == 59 ) && ( curchr <= closenode ) ) {
	p = curlist .tailfield ; 
	doextension () ; 
	outwhat ( curlist .tailfield ) ; 
	flushnodelist ( curlist .tailfield ) ; 
	curlist .tailfield = p ; 
	link ( p ) = 0 ; 
      } else
	backinput () ; 
    } 
    break ; 
  case 5 : 
    if ( abs ( curlist .modefield ) != hmode ) 
      reportillegalcase () ; 
    else {
      newwhatsit ( languagenode , smallnodesize ) ; 
      {register integer r_curval;

      r_curval = scanint ();
      if ( r_curval <= 0 )
	curlist .auxfield .hh .v.RH = 0 ; 
      else if ( r_curval > 255 ) 
	curlist .auxfield .hh .v.RH = 0 ; 
      else
	curlist .auxfield .hh .v.RH = r_curval;
      }
      whatlang ( curlist .tailfield ) = curlist.auxfield.hh.v.RH;
      whatlhm ( curlist .tailfield ) = normmin ( lefthyphenmin );
      whatrhm ( curlist .tailfield ) = normmin ( righthyphenmin );
    }
    break ; 
  default: 
    confusion("ext1");
    break;
  }
}


void fixlanguage ( void )
{ fixlanguage_regmem 
  register /*ASCIIcode*/ integer l;

  l = language;
  if ( l <= 0 || l > 255 )
    l = 0;

  if ( (ASCIIcode)l != curlist .auxfield .hh .v.RH ) {
    newwhatsit ( languagenode , smallnodesize ) ; 
    whatlang ( curlist .tailfield ) = l ; 
    curlist .auxfield .hh .v.RH = l ; 
    whatlhm ( curlist .tailfield ) = normmin ( lefthyphenmin ) ; 
    whatrhm ( curlist .tailfield ) = normmin ( righthyphenmin ) ; 
  }
}


void handlerightbrace ( void )
{ handlerightbrace_regmem 
  register halfword p, q  ; 
  register scaled d  ; 
  register integer f  ; 

  switch ( curgroup ) {
  case simplegroup : 
    unsave () ; 
    break ; 
  case bottomlevel : 
    {
      print_err("Too many }'s");
      zhelp1( STR_H_YOUVE_CLOSED_MORE );
      error();
    } 
    break ; 
  case semisimplegroup : 
  case mathshiftgroup : 
  case mathleftgroup : 
    extrarightbrace () ; 
    break ; 
  case adjustedhboxgroup : 
    adjusttail = adjusthead ; 
    /* Fall through */
  case hboxgroup : 
    package ( 0 ) ; 
    break ;
  case vboxgroup : 
    {
      endgraf () ; 
      package ( 0 ) ; 
    } 
    break ; 
  case vtopgroup : 
    {
      endgraf () ; 
      package ( 4 ) ; 
    } 
    break ; 
  case insertgroup : 
    {
      endgraf () ; 
      q = splittopskip ; 
      addglueref ( q ) ; 
      d = splitmaxdepth ; 
      f = floatingpenalty ; 
      unsave () ; 
      decr ( saveptr ) ; 
      p = vpackage ( link ( curlist .headfield ) , 0 , 1 , maxdimen ) ; 
      popnest () ; 
      if ( saved ( 0 ) < 255 ) {
	tailappend ( getnode ( insnodesize ) ) ; 
	ztype ( curlist .tailfield ) = insnode ; 
	subtype ( curlist .tailfield ) = saved ( 0 ) ; 
	height ( curlist .tailfield ) = height ( p ) + depth ( p ) ; 
	insptr ( curlist .tailfield ) = listptr ( p ) ; 
	splittopptr ( curlist .tailfield ) = q ; 
	depth ( curlist .tailfield ) = d ; 
	floatcost ( curlist .tailfield ) = f ; 
      } else {
	tailappend ( getnode ( smallnodesize ) ) ; 
	ztype ( curlist .tailfield ) = adjustnode ; 
	subtype ( curlist .tailfield ) = 0 ; 
	adjustptr ( curlist .tailfield ) = listptr ( p ) ; 
	deleteglueref ( q ) ; 
      } 
      freenode ( p , boxnodesize ) ; 
      if ( nestptr == 0 ) 
	buildpage () ; 
    } 
    break ; 
  case outputgroup : 
    {
      if ( curinput .locfield != 0
	|| ( tokentype != outputtext && tokentype != backedup ) )
      {
	print_err("Unbalanced output routine");
	zhelp2( STR_H_YOUR_SNEAKY_OUTPUT, STR_H_ICANT_HANDLE_THAT );
	error () ; 
	do {
	  gettoken ();
	} while ( ! ( curinput .locfield == 0 ) ) ; 
      }
      endtokenlist () ; 
      endgraf () ; 
      unsave () ; 
      outputactive = false ; 
      insertpenalties = 0 ; 
      if ( box ( 255 ) != 0 ) {
	print_err("Output routine didn't use all of ");
	printesc( STR_BOX );
	printint( 255 );
	zhelp1( STR_H_YOUR_OUTPUT_COMMANDS );
	boxerror( 255 );
      } 
      if ( curlist .tailfield != curlist .headfield ) {
	link ( pagetail ) = link ( curlist .headfield ) ; 
	pagetail = curlist .tailfield ; 
      } 
      if ( link ( pagehead ) != 0 ) {
	if ( link ( contribhead ) == 0 ) 
	  nest [ 0 ] .tailfield = pagetail ; 
	link ( pagetail ) = link ( contribhead ) ; 
	link ( contribhead ) = link ( pagehead ) ; 
	link ( pagehead ) = 0 ; 
	pagetail = pagehead ; 
      }
      popnest () ; 
      buildpage () ; 
    } 
    break ; 
  case discgroup : 
    builddiscretionary () ; 
    break ; 
  case aligngroup : 
    {
      backinput () ; 
      curtok = cstokenflag + frozencr ; 
      print_err("Missing ");
      printesc( STR_CR );
      c_print(" inserted");
      zhelp1( STR_H_IM_GUESSING_ENDALIGN );
      inserror () ; 
    } 
    break ; 
  case noaligngroup : 
    {
      endgraf () ; 
      unsave () ; 
      alignpeek () ; 
    } 
    break ; 
  case vcentergroup : 
    {
      endgraf () ; 
      unsave () ; 
      saveptr = saveptr - 2 ; 
      p = vpackage ( link ( curlist .headfield ) , saved ( 1 ) , saved ( 0 ) , 
			maxdimen ) ; 
      popnest () ; 
      tailappend ( newnoad () ) ; 
      ztype ( curlist .tailfield ) = vcenternoad ; 
      mathtype ( nucleus ( curlist .tailfield ) ) = subbox ; 
      info ( nucleus ( curlist .tailfield ) ) = p ; 
    } 
    break ; 
  case mathchoicegroup : 
    buildchoices () ; 
    break ; 
  case mathgroup : 
    {
      unsave () ; 
      decr ( saveptr ) ; 
      mathtype ( saved ( 0 ) ) = submlist ; 
      p = finmlist ( 0 ) ; 
      info ( saved ( 0 ) ) = p ; 
      if ( p != 0 ) 
      if ( link ( p ) == 0 ) 
      if ( ztype ( p ) == ordnoad ) {
	if ( mathtype ( subscr ( p ) ) == 0 ) 
	if ( mathtype ( supscr ( p ) ) == 0 ) {
	  mem [ saved ( 0 ) ] .hh = mem [ nucleus ( p ) ] .hh ; 
	  freenode ( p , noadsize ) ; 
	} 
      }
      else if ( ztype ( p ) == accentnoad ) 
      if ( saved ( 0 ) == nucleus ( curlist .tailfield ) ) 
      if ( ztype ( curlist .tailfield ) == ordnoad ) {
	q = curlist .headfield ; 
	while ( link ( q ) != curlist .tailfield ) q = link ( q ) ; 
	link ( q ) = p ; 
	freenode ( curlist .tailfield , noadsize ) ; 
	curlist .tailfield = p ; 
      } 
    } 
    break ; 
  default: 
    confusion("rightbrace");
    break ; 
  } 
}


void prefixedcommand ( void )
{ prefixedcommand_regmem 
  register smallnumber a  ; 
  register internalfontnumber f  ; 
  register halfword j  ; 
  fontindex k  ; 
  register halfword p, q  ; 
  register integer n  ; 
  boolean e  ; 
  register /* eightbits */ long_halfword r_curcmd = curcmd;

  a = 0 ;
  while ( (eightbits)r_curcmd == prefix ) {
    if ( ! odd ( a / curchr ) ) 
      a = a + curchr ;
    r_curcmd = getxnbtoken(1);
    if ( (eightbits)r_curcmd <= max_non_prefixed_command ) {
      print_err("You can't use a prefix with `");
      printcmdchr ( r_curcmd , curchr ) ; 
      printchar ( 39 ) ; 
      zhelp1( STR_H_ILLPRETENDGLOBAL );
      backerror () ; 
      return ; 
    }
  }

  if ( (eightbits)r_curcmd != def && (a % 4) != 0 ) {
    p_print_err( STR_H_YOUCANTUSE );
    printesc ( STR_LONG );
    c_print("' or `");
    printesc ( STR_OUTER );
    c_print("' with `");
    printcmdchr( r_curcmd, curchr ); 
    printchar( 39 );
    zhelp1( STR_H_ILLPRETENDLONG );
    error();

    r_curcmd = curcmd;
  }

  if ( globaldefs != 0 ) 
  if ( globaldefs < 0 ) {
    if ( ( a >= 4 ) ) 
    a = a - 4 ; 
  } else {
    if ( ! ( a >= 4 ) ) 
    a = a + 4 ; 
  }

  switch ( r_curcmd ) {
  case set_font :
    if ( ( a >= 4 ) ) 
	geqdefine ( curfontloc , data , curchr ) ; 
    else
	eqdefine ( curfontloc , data , curchr ) ; 
    break; 

#ifdef MLTEX
  case char_sub_def:
    {   integer chs_accent, chs_char, chs_replace;
	MEDmemoryword chs_value;

	if( !is_ML_TeX ) {
	  fprintf(stderr, "\nInternal error: \\charsubdef in normal TeX!\n");
	  exit(1);
	}
	chs_replace = scancharnum();
	scanoptionalequals();
	chs_accent = scancharnum();
	chs_char = scancharnum();
	chs_value.qqqq.b0 = chs_accent;
	chs_value.qqqq.b1 = chs_char;

	/* Note: In original ML-TeX the following test is ommitted, instead
	   the warning message is printed when using the replacement.
	   I think it is better to inform the user where (s)he makes the error.
	 */
	if( chs_accent == chs_replace || char_list_exists(chs_accent)
		|| chs_char == chs_replace || char_list_exists(chs_char) ) {
	  begindiagnostic();
	  c_printnl("Character Substitution list rejected: ");
	  print(chs_replace); c_print(" = ");
	  print(chs_accent); printchar(' ');
	  print(chs_char); printchar('!');
	  enddiagnostic(false);
	} else {

	if( chs_replace > char_sub_def_max ) {
	  if( a >= 4 )
	    geqworddefine(intbase + char_sub_def_max_code, chs_replace);
	  else
	    eqworddefine(intbase + char_sub_def_max_code, chs_replace);
	}
	if( a >= 4 )
	  geqdefine(char_sub_base + chs_replace,
		char_sub_def, chs_value.hh.v.RH);
	else
	  eqdefine(char_sub_base + chs_replace,
		char_sub_def, chs_value.hh.v.RH);

	if( tracing_char_sub_def > 0 ) {
	  begindiagnostic();
	  c_printnl("New char_sub_def: ");
	  print(chs_replace); c_print(" = ");
	  print(chs_accent); printchar(' ');
	  print(chs_char);
	  enddiagnostic(false);
	}

	}
    }
    break;
#endif

  case def :
    {
      if ( odd ( curchr ) && ! ( a >= 4 ) && ( globaldefs >= 0 ) )
	a = a + 4 ; 
      e = ( curchr >= 2 ) ; 
      getrtoken () ; 
      p = curcs ; 
      q = scantoks ( true , e ) ; 
      if ( ( a >= 4 ) ) 
	geqdefine ( p , call + ( a % 4 ) , defref ) ; 
      else
	eqdefine ( p , call + ( a % 4 ) , defref ) ; 
    } 
    break ; 
  case let : 
    {
      n = curchr ; 
      getrtoken () ; 
      p = curcs ; 
      if ( n == normal ) {
	do {
	  /* nothing */
	} while ( (eightbits)gettoken() == spacer );
	if ( curtok == othertoken + 61 ) {
	  if ( (eightbits)gettoken() == spacer )
	    gettoken();
	}
      } else {
	gettoken () ; 
	q = curtok ; 
	gettoken () ; 
	backinput () ; 
	curtok = q ; 
	backinput () ; 
      } 
      if ( curcmd >= call ) 
	addtokenref ( curchr ) ; 
      if ( ( a >= 4 ) ) 
	geqdefine ( p , curcmd , curchr ) ; 
      else
	eqdefine ( p , curcmd , curchr ) ; 
    } 
    break ; 
  case shorthand_def :
    {
      n = curchr ; 
      getrtoken () ; 
      p = curcs ; 
      if ( ( a >= 4 ) ) 
 	geqdefine ( p , 0 , 256 ) ; 
      else
	eqdefine ( p , 0 , 256 ) ; 
      scanoptionalequals();
      switch ( n ) {
      case 0 : 
	{ register integer r_curval;
	  r_curval = scancharnum ();
	  if ( ( a >= 4 ) ) 
	    geqdefine ( p, chargiven, r_curval );
	  else
	    eqdefine ( p, chargiven, r_curval );
	}
	break ; 
      case 1 : 
	{ register integer r_curval;
	  r_curval = scanfifteenbitint ();
	  if ( ( a >= 4 ) ) 
	    geqdefine ( p, math_given, r_curval ) ; 
	  else
	    eqdefine ( p, math_given, r_curval ) ; 
	} 
	break ; 
      default: 
	{ register integer r_curval;
	  r_curval = scaneightbitint ();
	  switch ( n ) {
	  case 2 : 
	    if ( ( a >= 4 ) ) 
	      geqdefine ( p, assign_int, countbase + r_curval ) ; 
	    else
	      eqdefine ( p, assign_int, countbase + r_curval ) ; 
	    break ; 
	  case 3 : 
	    if ( ( a >= 4 ) ) 
	      geqdefine ( p, assign_dimen, scaledbase + r_curval ) ; 
	    else
	      eqdefine ( p, assign_dimen, scaledbase + r_curval ) ; 
	    break ; 
	  case 4 : 
	    if ( ( a >= 4 ) ) 
	    geqdefine ( p, assign_glue, skipbase + r_curval ) ; 
	    else eqdefine ( p, assign_glue, skipbase + r_curval ) ; 
	    break ; 
	  case 5 : 
	    if ( ( a >= 4 ) ) 
	    geqdefine ( p, assign_mu_glue, muskipbase + r_curval ) ; 
	    else eqdefine ( p, assign_mu_glue, muskipbase + r_curval ) ; 
	    break ; 
	  case 6 : 
	    if ( ( a >= 4 ) ) 
	    geqdefine ( p, assign_toks, toksbase + r_curval ) ; 
	    else eqdefine ( p, assign_toks, toksbase + r_curval ) ; 
	    break ;
	  }
	}
	break;
      }
    }
    break ; 
  case read_to_cs :
    {
      n = scanint ();
      if ( ! scankeyword( STR_TO ) ) {
	print_err("Missing `to' inserted");
	zhelp1( STR_H_YOUSHOULDREAD );
	error();
      } 
      getrtoken();
      p = curcs;
      readtoks( n, p );
      if ( ( a >= 4 ) )
	geqdefine ( p , call , curval );
      else
	eqdefine ( p , call , curval );
    }
    break;
  case toks_register :
  case assign_toks :
    {
      q = curcs ; 
      if ( (eightbits)r_curcmd == toks_register ) {
	p = toksbase + scaneightbitint();
      } else
	p = curchr;
      scanoptionalequals ();
      r_curcmd = getxnbtoken (1);
      if ( (eightbits)r_curcmd != left_brace ) {
	if ( (eightbits)r_curcmd == toks_register ) {
	  curchr = toksbase + scaneightbitint();
	  curcmd = r_curcmd = assign_toks;
	}
	if ( (eightbits)r_curcmd == assign_toks ) {
	  q = equiv ( curchr ) ; 
	  if ( q == 0 ) 
	  if ( ( a >= 4 ) ) 
	  geqdefine ( p , undefinedcs , 0 ) ; 
	  else eqdefine ( p , undefinedcs , 0 ) ; 
	  else {
	    addtokenref ( q ) ; 
	    if ( ( a >= 4 ) ) 
	    geqdefine ( p , call , q ) ; 
	    else eqdefine ( p , call , q ) ; 
	  } 
	  goto lab30 ; 
	}
      } 
      backinput () ; 
      curcs = q ; 
      q = scantoks ( false , false ) ; 
      if ( link ( defref ) == 0 ) {
	if ( ( a >= 4 ) ) 
	geqdefine ( p , undefinedcs , 0 ) ; 
	else eqdefine ( p , undefinedcs , 0 ) ; 
	freeavail ( defref ) ; 
      } else {
	if ( p == outputroutineloc ) {
	  link ( q ) = getavail () ; 
	  q = link ( q ) ; 
	  info ( q ) = rightbracetoken + 125 ; 
	  q = getavail () ; 
	  info ( q ) = leftbracetoken + 123 ; 
	  link ( q ) = link ( defref ) ; 
	  link ( defref ) = q ; 
	} 
	if ( ( a >= 4 ) ) 
	geqdefine ( p , call , defref ) ; 
	else eqdefine ( p , call , defref ) ; 
      } 
    } 
    break ; 
  case assign_int :
    {
      p = curchr ; 
      scanoptionalequals () ; 
      { register integer r_curval;
	r_curval = scanint (); 
	if ( a >= 4 )
	  geqworddefine ( p , r_curval );
	else
	  eqworddefine ( p , r_curval ); 
      }
    }
    break ; 
  case assign_dimen : 
    {
      p = curchr ; 
      scanoptionalequals ();
      { register integer r_curval;
	r_curval = scandimen ( false , false , false );
	if ( ( a >= 4 ) )
	  geqworddefine ( p , r_curval );
	else
	  eqworddefine ( p , r_curval );
      }
    }
    break ; 
  case assign_glue : 
  case assign_mu_glue :
    {
      p = curchr ; 
      n = r_curcmd ; 
      scanoptionalequals () ;
      { register integer r_curval;
	if ( (eightbits)n == assign_mu_glue )
	  r_curval = scanglue ( muval );
	else
	  r_curval = scanglue ( glueval );
	r_curval = trapzeroglue(r_curval);
	if ( a >= 4 )
	  geqdefine ( p , glueref , r_curval );
	else
	  eqdefine ( p , glueref , r_curval );
      }
    }
    break ; 
  case def_code :
    {
      if ( curchr == catcodebase )
      n = 15 ;
      else if ( curchr == mathcodebase )
      n = 32768L ;
      else if ( curchr == sfcodebase )
      n = 32767 ;
      else if ( curchr == delcodebase )
      n = 16777215L ;
      else n = 255 ;
      p = curchr ; 
      p += scancharnum();	/* p = p + curval; */
      scanoptionalequals () ;
     {register integer r_curval;
      r_curval = scanint ();
      if ( ( r_curval < 0 && p < delcodebase ) || r_curval > n ) {
	print_err("Invalid code (");
	printint ( r_curval );
	if ( p < delcodebase ) 
	  c_print("), should be in the range 0..");
	else
	  c_print("), should be at most ");
	printint ( n ) ; 
	zhelp1( STR_H_IMGOINGTOUSENULL );
	error () ; 
	curval = r_curval = 0;
      }
      if ( p < mathcodebase ) 
	if ( ( a >= 4 ) ) 
	geqdefine ( p , data , r_curval ) ; 
	else eqdefine ( p , data , r_curval ) ; 
      else if ( p < delcodebase ) 
	if ( ( a >= 4 ) ) 
	geqdefine ( p , data , r_curval ) ; 
	else eqdefine ( p , data , r_curval ) ; 
      else {
	if ( ( a >= 4 ) )
	geqworddefine ( p , r_curval ) ; 
	else eqworddefine ( p , r_curval ) ; 
      }
     }
    }
    break ; 
  case def_family :
    {
      p = curchr ; 
      p += scanfourbitint ();	/* p = p + curval; */
      scanoptionalequals ();
      { register integer r_curval;
	r_curval = scanfontident ();
	if ( ( a >= 4 ) )
	  geqdefine ( p , data , r_curval );
	else
	  eqdefine ( p , data , r_curval ); 
      }
    }
    break ; 
  case register_cmd : 
  case advance : 
  case multiply : 
  case divide : 
    doregistercommand ( a );
    break;
  case set_box : 
    {
      n = scaneightbitint ();
      if ( a >= 4 )
	n += 256;
      scanoptionalequals ();
      if ( setboxallowed ) {  /* TeX 3.141 */
	scanbox ( 1073741824L + n );
      } else {
	p_print_err(STR_H_IMPROPER);  printesc(532);
	zhelp1( STR_H_SORRY_SETBOXNOTALLOW );
	error();
      }
    }
    break ; 
  case set_aux :
    alteraux();
    break;
  case set_prev_graf :
    alterprevgraf();
    break ; 
  case set_page_dimen :
    alterpagesofar();
    break ; 
  case set_page_int :
    alterinteger();
    break ; 
  case set_box_dimen :
    alterboxdimen();
    break ; 
  case set_shape :
    {
      scanoptionalequals () ; 
      n = scanint ();
      if ( n <= 0 ) 
        p = 0 ; 
      else {
	p = getnode ( 2 * n + 1 ) ; 
	info ( p ) = n ; 
	{register integer for_end; j = 1 ; for_end = n ; if ( j <= for_end) 
	do {
	    mem[ p + 2 * j - 1 ].cint = scandimen( false, false, false );
	    mem[ p + 2 * j ].cint = scandimen( false, false, false );
	} while ( j++ < for_end ) ; }
      }
      if ( ( a >= 4 ) ) 
	geqdefine ( parshapeloc , shaperef , p ) ; 
      else
	eqdefine ( parshapeloc , shaperef , p ) ; 
    }
    break ; 
  case hyph_data :
    if ( curchr == 1 ) {
      if( call_new_patterns() )		/* virtex returns != 0 */
	return;
    } else {
      newhyphexceptions () ; 
    } 
    goto lab30 ; 
    break ; 
  case assign_font_dimen :
    {
      k = findfontdimen ( true );
      scanoptionalequals () ; 
      fontinfo [ k ] .cint = scandimen ( false , false , false );
    } 
    break ; 
  case assign_font_int :
    {
      n = curchr ; 
      f = scanfontident ();
      scanoptionalequals () ; 
      if ( n == 0 ) 
	hyphenchar(f) = scanint ();
      else
	skewchar(f) = scanint();
    }
    break ; 
  case def_font :
    newfont( a );
    break ; 
  case set_interaction :
    newinteraction();
    break ; 
  default: 
    confusion("prefix");
    break ; 
  }

lab30:
  if ( aftertoken != 0 ) {
    curtok = aftertoken;
    backinput();
    aftertoken = 0;
  }
}


#if 0

void maincontrol ( ) 

#endif

void giveerrhelp ( void )
{ giveerrhelp_regmem

  tokenshow ( errhelp );
}


boolean openfmtfile ( void )
{/* 40 10 */ openfmtfile_regmem 
  register integer j  ; 

  j = curinput .locfield ; 
  if ( buffer [ curinput .locfield ] == 38 ) {
    incr ( curinput .locfield ) ; 
    j = curinput .locfield ; 
    buffer [ last ] = 32 ;
    while ( buffer [ j ] != 32 )
      incr( j );
    packbufferedname ( 0, curinput .locfield, j - 1 );
    if ( wopenin ( fmtfile ) ) 
      goto lab40;
    wakeupterminal ();
    (void) printf("Sorry, I can't find that format; will try the default.\n");
    flush ( stdout );
  } 
  packbufferedname ( formatdefaultlength - 4 , 1 , 0 ) ; 
  if ( ! wopenin ( fmtfile ) ) {
    wakeupterminal ();
    (void) printf("I can't find the default format file!\n");
    return(false);
  }

lab40:
  curinput .locfield = j;
  return(true);
}


#if 0		/* nach shipout.c */
void closefilesandterminate ( void )
#endif

#ifdef DEBUG
void debughelp ( void )
{/* 888 10 */ debughelp_regmem 
  register integer k, l, m, n  ; 

  while ( true ) {
    wakeupterminal () ; 
    c_printnl("debug # (-1 to exit):");
    flush ( stdout ) ; 
    read ( stdin , m ) ; 
    if ( m < 0 ) 
      return ; 
    else if ( m == 0 ) 
      dumpcore () ; 
    else {
      read ( stdin , n ) ; 
      switch ( m ) {
      case 1 : 
	printword ( mem [ n ] ) ; 
	break ; 
      case 2 : 
	printint ( info ( n ) ) ; 
	break ; 
      case 3 : 
	printint ( link ( n ) ) ; 
	break ; 
      case 4 : 
	printword ( eqtb [ n ] ) ; 
	break ; 
      case 5 : 
	printword ( fontinfo [ n ] ) ; 
	break ; 
      case 6 : 
	printword ( savestack [ n ] ) ; 
	break ; 
      case 7 : 
	showbox ( n ) ; 
	break ; 
      case 8 : 
	{
	  breadthmax = 10000 ; 
	  depththreshold = poolsize - poolptr - 10 ; 
	  shownodelist ( n ) ; 
	} 
	break ; 
      case 9 : 
	showtokenlist ( n , 0 , 1000 ) ; 
	break ; 
      case 10 : 
#if 0  /* TeX 3.141 */
	print ( n );
#else
	slowprint ( n );
#endif
	break ; 
      case 11 : 
	checkmem ( n > 0 ) ; 
	break ; 
      case 12 : 
	searchmem ( n ) ; 
	break ; 
      case 13 : 
	{
	  read ( stdin , l ) ; 
	  printcmdchr ( n , l ) ; 
	} 
	break ; 
      case 14 : 
	{register integer for_end; k = 0 ; for_end = n ; if ( k <= for_end) 
	do 
	  print ( buffer [ k ] ) ; 
	while ( k++ < for_end ) ; } 
	break ; 
      case 15 : 
	{
	  fontinshortdisplay = nullfont ; 
	  shortdisplay ( n ) ; 
	} 
	break ; 
      case 16 : 
	panicking = ! panicking ; 
	break ; 
      default: 
	print ( 63 ) ; 
	break ; 
      } 
    } 
  } 
} 
#endif /* DEBUG */
