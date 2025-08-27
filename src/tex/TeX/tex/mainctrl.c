#define EXTERN extern
#include "texd.h"


static void appspace ( void )
{ appspace_regmem 
  register halfword q;

  register liststaterecord * curlistp = &curlist;
#define curlist (*curlistp)

  if ( ( curlist .auxfield .hh .v.LH >= 2000 ) && ( xspaceskip != zeroglue ) ) 
    q = newparamglue ( xspaceskipcode );
  else {
    register halfword mainp;
    register internalfontnumber mainf = curfont;	/* (br) */

    if ( spaceskip != zeroglue ) 
      mainp = spaceskip;
    else {
      mainp = fontglue(mainf); 
      if ( mainp == 0 ) {
	register SMALLmemoryword *gluep;	/* halfword maink; */

	mainp = newspec ( zeroglue ) ;
	gluep = &fontinfo[ parambase(mainf) + spacecode ];	/* maink=...*/
	width ( mainp )   = gluep++->cint;	/* fontinfo[maink].cint; */
	stretch ( mainp ) = gluep++->cint;	/* fontinfo[maink+1].cint; */
	shrink ( mainp )  = gluep->cint;	/* fontinfo[maink+2].cint; */
	fontglue(mainf) = mainp;
      }
    }
    mainp = newspec ( mainp ) ; 
    if ( curlist .auxfield .hh .v.LH >= 2000 ) 
      width ( mainp ) += extraspace ( mainf ) ;
    stretch(mainp) = xnoverd(stretch(mainp), curlist.auxfield.hh.v.LH, 1000);
    shrink(mainp)  = xnoverd(shrink(mainp), 1000, curlist.auxfield.hh.v.LH);
    q = newglue ( mainp );
    gluerefcount ( mainp ) = 0;
  }
  link ( curlist .tailfield ) = q ;
  curlist .tailfield = q ;

#undef curlist
}


void maincontrol ( void )
{  maincontrol_regmem 

  /* Try to force the Compiler to free all registers for this
   * function.  (This function is only leaved at the end of the job.)
   */
  /* register */ integer t;
  register internalfontnumber mainf;
  /* register */ fourquarters maini, mainj;
  register fontindex maink;
  /* register halfword mainp; */
#ifdef BIG
  register integer mains;
#else
  register signed short mains;
#endif
  register SMALLmemoryword *ligp;	/* (br) ptr in lig program */
  register long_halfword ligstack;
  register halfword curl, curr, curq;
  register /* eightbits */ long_halfword r_curcmd;

  register liststaterecord * curlistp = &curlist;
#define curlist (*curlistp)

  boolean insdisc = false;
halfword bchar = nonchar;
halfword falsebchar = nonchar;
boolean cancelboundary = false;

  if ( everyjob != 0 )
    begintokenlist ( everyjob , everyjobtext );

lab60:		/* big_switch */
  r_curcmd = getxtoken ();

lab21:		/* reswitch */
  if ( interrupt != 0 && OKtointerrupt ) {
    backinput ();
    if ( interrupt != 0 )
      pauseforinstructions ();
    goto lab60;
  }

#ifdef DEBUG
  if ( panicking )
    checkmem ( false );
#endif /* DEBUG */

  if ( tracingcommands > 0 )
    showcurcmdchr ();

  mains = curlist.modefield;
  if( mains < 0 ) mains = -mains;

#if 0
  switch ( mains + r_curcmd )
#else
  switch ( r_curcmd )
#endif
  {						/* switch begin */
  case charnum :
    if( mains == hmode ) {
#if 0
      curchr = scancharnum ();  /* curchr = curval; */
#else
      r_curcmd = scancharnum ();
      curchr = r_curcmd;
#endif
      goto lab70;
    } else if( mains == vmode ) {
      backinput();  newgraf(true);
    } else {
      curchr = scancharnum ();  /* curchr = curval; */
      setmathchar ( mathcode ( curchr ) ) ; 
    }
    break;

  case letter :
  case otherchar :
  case chargiven :
    if( mains == hmode ) {
      goto lab701 /*lab70*/;
    } else if( mains == vmode ) {
      backinput () ; newgraf ( true ) ; 
    } else {
      setmathchar ( mathcode ( curchr ) ) ; 
    }
    break;

  case noboundary :
    if( mains == hmode ) {
      r_curcmd = getxtoken ();
      if ( (eightbits)r_curcmd == letter || (eightbits)r_curcmd == otherchar
	|| (eightbits)r_curcmd == chargiven || (eightbits)r_curcmd == charnum )
        cancelboundary = true;
      goto lab21;
    } else if( mains == vmode ) {
      backinput () ; newgraf ( true ) ; 
    } else {
      /* nothing */
    }
    break;

  case spacer:
    if( mains == hmode ) {
      if ( curlist .auxfield .hh .v.LH != 1000 ) {	/* space_factor */
	appspace ();
	break;
      } else {
	goto lab120;
      }
    } else {
      /* nothing */
    }
    break;

  case exspace :
    if( mains == vmode ) {
      backinput () ;  newgraf ( true ) ; 
    } else {
      long_halfword tempptr;
lab120:

      if ( spaceskip == zeroglue ) {
	register internalfontnumber tmp_cf = curfont;
	register long_halfword mainp;

	mainp = fontglue(tmp_cf); 
	if ( mainp == 0 ) {
	  register SMALLmemoryword *gluep;

	  mainp = newspec ( zeroglue ) ; 
	  gluep = &fontinfo[ parambase(tmp_cf) + spacecode ];	/* maink=... */
	  width ( mainp )   = gluep++->cint;	/* fontinfo[maink].cint; */
	  stretch ( mainp ) = gluep++->cint;	/* fontinfo[maink+1].cint; */
	  shrink ( mainp )  = gluep->cint;	/* fontinfo[maink+2].cint; */
	  fontglue(tmp_cf) = mainp;
	}
	tempptr = newglue ( mainp );
      } else
	tempptr = newparamglue ( spaceskipcode ) ; 
      link ( curlist .tailfield ) = tempptr ; 
      curlist .tailfield = tempptr ; 
      goto lab60;
    }
    break;

	/* cases of main_control that are not part of the inner loop */
  case relax :
    break;

  case ignorespaces :
    r_curcmd = getxnbtoken(0);
    goto lab21;
    break;

  case stop :
    if( mains == hmode ) {
      headforvmode () ;
    } else if( mains == vmode ) {
      if ( itsallover () )
	return ;
    } else {
      insertdollarsign () ;
    }
    break;

  case vmove :
    if( mains == vmode ) {
      reportillegalcase () ;
    } else {
      /* register integer t; */
      register integer r_curval;

      t = curchr;
      r_curval = scandimen ( false , false , false ); 
      if ( t == 0 )
        scanbox ( r_curval );
      else
	scanbox ( - (integer) r_curval );
    }
    break;

  case hmove :
    if( mains != vmode ) {
      reportillegalcase () ;
    } else {
      /* register integer t; */
      register integer r_curval;

      t = curchr;
      r_curval = scandimen ( false , false , false );
      if ( t == 0 )
        scanbox ( r_curval );
      else
	scanbox ( - (integer) r_curval );
    }
    break;

  case last_item :
    reportillegalcase () ; 
    break ; 

  case vadjust :
    if( mains == vmode ) {
      reportillegalcase () ;
    } else {
      begininsertoradjust () ;
    }
    break;

  case ital_corr :
    if( mains == hmode ) {
      appenditaliccorrection () ;
    } else if( mains == vmode ) {
      reportillegalcase () ;
    } else {
      tailappend ( newkern ( 0 ) ) ;
    }
    break;

  case eq_no :
    if( mains != mmode ) {
      reportillegalcase () ;
    } else {
      if ( privileged () )
	if ( curgroup == mathshiftgroup )
	  starteqno () ;
	else
	  offsave () ;
      break;
    }
    break;

  case mac_param :
    reportillegalcase () ;
    break ;

  case sup_mark :
  case sub_mark :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      subsup () ;
    }
    break;

  case math_char_num :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      setmathchar( scanfifteenbitint () );
      /*setmathchar ( curval ) ;*/
    }
    break;

  case math_given :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      setmathchar ( curchr ) ;
    }
    break;

  case math_comp :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      tailappend ( newnoad () ) ;
      ztype ( curlist .tailfield ) = curchr ;
      scanmath ( nucleus ( curlist .tailfield ) ) ;
    }
    break;

  case delim_num :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      setmathchar( scantwentysevenbitint() / 4096 );
      /*setmathchar ( curval / 4096 );*/
    }
    break;

  case left_right :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      mathleftright () ;
    }
    break;

  case above :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      mathfraction () ;
    }
    break;

  case radical :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      mathradical () ;
    }
    break;

  case math_style :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      tailappend ( newstyle ( curchr ) ) ;
    }
    break;

  case math_choice :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      appendchoices () ;
    }
    break;

  case vcenter :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      scanspec ( vcentergroup , false ) ;
      normalparagraph () ;
      pushnest () ;
      curlist .modefield = -vmode ;
      curlist .auxfield .cint = ignoredepth ;
      if ( everyvbox != 0 )
	begintokenlist ( everyvbox , everyvboxtext ) ;
    }
    break;

  case non_script :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      tailappend ( newglue ( zeroglue ) ) ;
      subtype ( curlist .tailfield ) = condmathglue ;
    }
    break;

  case mkern :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      appendkern () ;
    }
    break;

  case limit_switch :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      mathlimitswitch () ;
    }
    break;

  case mskip :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      appendglue () ;
    }
    break;

  case math_accent :
    if( mains != mmode ) {
      insertdollarsign () ;
    } else {
      mathac () ;
    }
    break;

  case endv :
    if( mains != mmode ) {
      doendv () ;
    } else {
      insertdollarsign () ;
    }
    break;

  case par_end :
    if( mains == hmode ) {
      if ( alignstate < 0 )
	offsave () ;
      endgraf () ;
      if ( curlist .modefield == vmode )
	buildpage () ;
    } else if( mains == vmode ) {
      normalparagraph () ;
      if ( curlist .modefield > 0 )
	buildpage () ;
    } else {
      insertdollarsign () ;
    }
    break;

  case vskip :
    if( mains == hmode ) {
      headforvmode () ;
    } else if( mains == vmode ) {
      appendglue () ;
    } else {
      insertdollarsign () ;
    }
    break;

  case un_vbox :
    if( mains == hmode ) {
      headforvmode () ;
    } else if( mains == vmode ) {
      unpackage () ;
    } else {
      insertdollarsign () ;
    }
    break;

  case valign :
    if( mains == hmode ) {
#ifdef TEXXET
      if( curchr > 0 )
	tailappend(newmath(0, curchr));
      else
	initalign () ;
#else
      initalign () ;
#endif
    } else if( mains == vmode ) {
      backinput () ;
      newgraf ( true ) ;
    } else {
      insertdollarsign () ;
    }
    break;

  case hrule :
    if( mains == hmode ) {
      headforvmode () ;
    } else if( mains == vmode ) {
      tailappend ( scanrulespec () );
      if ( abs ( curlist .modefield ) == vmode )
	curlist .auxfield .cint = ignoredepth ; 
      else if ( abs ( curlist .modefield ) == hmode )
	curlist .auxfield .hh .v.LH = 1000 ; 
    } else {
      insertdollarsign () ;
    }
    break;

  case vrule :
    if( mains == vmode ) {
      backinput () ; 
      newgraf ( true ) ; 
    } else {
      tailappend ( scanrulespec () );
      if ( abs ( curlist .modefield ) == vmode ) 
	curlist .auxfield .cint = ignoredepth ; 
      else if ( abs ( curlist .modefield ) == hmode ) 
	curlist .auxfield .hh .v.LH = 1000 ; 
    }
    break;

  case hskip :
    if( mains == vmode ) {
      backinput () ;
      newgraf ( true ) ;
    } else {
      appendglue () ;
    }
    break;

  case kern :
    appendkern () ;
    break;

  case left_brace :
    if( mains != mmode ) {
      newsavelevel ( simplegroup ) ;
    } else {
      tailappend ( newnoad () ) ;
      backinput () ;
      scanmath ( nucleus ( curlist .tailfield ) );
    }
    break;

  case begin_group :
    newsavelevel ( semisimplegroup ) ; 
    break ; 

  case end_group :
    if ( curgroup == semisimplegroup )
      unsave () ; 
    else
      offsave () ;
    break ;

  case right_brace :
    handlerightbrace () ; 
    break ; 

  case leader_ship :
    scanbox ( 1073742337L - aleaders + curchr );
    break ;

  case make_box :
    beginbox ( 0 ) ; 
    break ;

  case start_par :
    if( mains == vmode ) {
      newgraf( curchr > 0 );
    } else {
      indentinhmode( curchr > 0 );
    }
    break;

  case math_shift :
    if( mains == hmode ) {
      initmath () ;
    } else if( mains == vmode ) {
      backinput () ; 
      newgraf ( true ) ; 
    } else {
      if ( curgroup == mathshiftgroup ) 
	aftermath () ; 
      else
	offsave () ; 
    }
    break;

  case un_hbox :
    if( mains == vmode ) {
      backinput () ; 
      newgraf ( true ) ; 
    } else {
      unpackage () ;
    }
    break;

  case accent :
    if( mains == hmode ) {
      makeaccent () ;
    } else if( mains == vmode ) {
      backinput () ; 
      newgraf ( true ) ; 
    } else {
      mathac () ;
    }
    break;

  case discretionary :
    if( mains == vmode ) {
      backinput () ; 
      newgraf ( true ) ; 
    } else {
      appenddiscretionary () ;
    }
    break;

  case halign :
    if( mains == vmode ) {
      initalign () ;
    } else if( mains == hmode ) {
      headforvmode () ;
    } else {
      if ( privileged () ) 
	if ( curgroup == mathshiftgroup ) 
	  initalign () ; 
	else
	  offsave () ; 
    }
    break;

  case insert :
    begininsertoradjust () ; 
    break ; 

  case mark :
    makemark () ; 
    break ; 

  case break_penalty :
    appendpenalty () ; 
    break ; 

  case remove_item :
    deletelast () ; 
    break ; 

  case tab_mark :
  case car_ret :
    alignerror () ; 
    break ; 

  case no_align :
    noalignerror () ; 
    break ; 

  case omit :
    omiterror () ; 
    break ;

  case end_cs_name :
    cserror () ; 
    break ; 

  case toks_register :
  case assign_toks :
  case assign_int :
  case assign_dimen :
  case assign_glue :
  case assign_mu_glue :
  case assign_font_dimen :
  case assign_font_int :
  case set_aux :
  case set_prev_graf :
  case set_page_dimen :
  case set_page_int :
  case set_box_dimen :
  case set_shape :
  case def_code :
  case def_family :
  case set_font :
  case def_font :
  case register_cmd :
  case advance :
  case multiply :
  case divide :
  case prefix :
  case let :
  case shorthand_def :
  case read_to_cs :
  case def :
  case set_box :
  case hyph_data :
#ifdef MLTEX
  case char_sub_def :
#endif
  case set_interaction :
    prefixedcommand () ; 
    break ; 

  case afterassignment :
    gettoken () ; 
    aftertoken = curtok ; 
    break ; 

  case aftergroup :
    gettoken () ; 
    saveforafter ( curtok ) ; 
    break ; 

  case instream :
    openorclosein () ; 
    break ; 

  case message :
    issuemessage () ; 
    break ; 

  case caseshift :
    shiftcase () ; 
    break ; 

  case xray :
    showwhatever () ; 
    break ;

  case extension :
    doextension () ; 
    break ; 

  default:
    fprintf(stderr, "Internal error: can't happen (mainctrl) !\n");
  }	/* switch 1 */
  goto lab60;		/* goto big_switch ---^ */


/* main_loop:  when hmode + letter/other_char/char_given/char_num */
lab701:
  r_curcmd = curchr;  /* added */
lab70:
#ifdef ERW_SHIFTCASE
  if( uccode(0) > 0 && uccode(0) < 3 ) {
    if( r_curcmd /*curchr*/ > 0 ) {  /* nicht \char0, da \uccode falsch ! */
      if( uccode(0) == 1 ) {	/* lowercase */
	if( lccode(r_curcmd) > 0 ) {
	  r_curcmd = lccode(r_curcmd);
	  curchr = r_curcmd;
	}
      } else {			/* uppercase */
	if( uccode(r_curcmd) > 0 ) {
	  r_curcmd = uccode(r_curcmd);
	  curchr = r_curcmd;
        }
      }
    }
  }
#endif
  /* adjust space_factor */
  mains = sfcode ( r_curcmd /*curchr*/ );
  if ( mains == 1000 )
    curlist .auxfield .hh .v.LH = 1000;
  else if ( mains <= 1000 ) {  /* was:  if ( mains < 1000 ) */
    if ( mains > 0 )
      curlist .auxfield .hh .v.LH = mains;
  } else if ( curlist .auxfield .hh .v.LH < 1000 )
    curlist .auxfield .hh .v.LH = 1000;
  else
    curlist .auxfield .hh .v.LH = mains;

#ifdef ERW_CONSTITUTE

  if (
#ifndef ERW_LANGUAGE
       curlist .modefield > 0 &&
#endif
                                 language != curlist.auxfield.hh.v.RH )
      fixlanguage () ; 
  fastgetavail (ligstack);
  character(ligstack) = curchr;
  font(ligstack) = curfont;
  tailappend(ligstack);
  goto lab60;

#else

  mainf = curfont;
  bchar = fontbchar(mainf);
  falsebchar = fontfalsebchar(mainf);
  if (
#ifndef ERW_LANGUAGE
       curlist .modefield > 0 &&
#endif
                                 language != curlist.auxfield.hh.v.RH )
      fixlanguage () ; 
  fastgetavail ( ligstack ) ; 
  curl = curchr ; 
  font ( ligstack ) = mainf ; 
  character ( ligstack ) = curl ; 
  curq = curlist .tailfield ; 
  if ( cancelboundary ) {
    cancelboundary = false ;
#if 0	/* not needed with the following jump */
    maink = nonaddress; 
#endif
    goto lab92;		/* (br) added this ... see below */
  } else
    maink = bcharlabel(mainf);
  if ( maink == nonaddress ) 
    goto lab92;		/* goto main_loop_move_2 */

  curr = curl ;
  curl = nonchar;
  ligp = &fontinfo[ maink ];	/* (br) init ligp/maink for bcharlabel[] */
  goto lab111;		/* goto main_lig_loop_1 */

/* main_loop_wrapup: wird nur von "unten" angesprungen */
lab80:
  if ( curl < nonchar ) {
    if ( character ( curlist .tailfield ) == hyphenchar(mainf) ) 
      if ( link ( curq ) > 0 ) 
        insdisc = true ; 
    if ( ligaturepresent ) {
      register long_halfword mainp;

      mainp = newligature ( mainf , curl , link ( curq ) ) ; 
      if ( lfthit ) {
	subtype ( mainp ) = 2 ; 
	lfthit = false ; 
      }
      if ( rthit ) 
        if ( ligstack == 0 ) {
	  incr ( subtype ( mainp ) );
	  rthit = false ; 
        }
      link ( curq ) = mainp ; 
      curlist .tailfield = mainp ; 
      ligaturepresent = false ; 
    }
    if ( insdisc ) {
      insdisc = false ; 
      if ( curlist .modefield > 0 ) 
	tailappend ( newdisc () ) ; 
    }
  }

/* main_loop_move: */
lab90:
  if ( ligstack == 0 ) {
    r_curcmd = curcmd;
    goto lab21;
  }
  curq = curlist .tailfield;
  curl = character ( ligstack );   /* curr; TeX 3.141 */

lab91:		/* main_loop_move_1 */
  if ( ! ischarnode ( ligstack ) ) {
    /* statt goto lab95; wurde Teil hierher verschoben */
    register long_halfword mainp;

    mainp = ligptr ( ligstack );
    if ( mainp > 0 )
      tailappend ( mainp ) ;
    {long_halfword tempptr;
     tempptr = ligstack ;
     ligstack = link ( tempptr ) ;
     freenode ( tempptr , smallnodesize ) ;
    }
    maini = zcharinfo ( mainf ,  curl ) ;
    ligaturepresent = true ;
    if ( ligstack == 0 )
      if ( mainp > 0 )
        goto lab100 ;
      else
        curr = bchar ;
    else
      curr = character ( ligstack ) ;
    goto lab110 ;
  }

lab92:		/* main_loop_move_2 */
  if ( curchr < fontbc(mainf) || effective_char(curchr) > fontec(mainf) ) {
    charwarning ( mainf , curchr );
    freeavail ( ligstack );
    goto lab60;		/* goto big_switch */
  }
  maini = zcharinfo ( mainf ,  curl );

  if ( ! charexists ( maini ) ) {
    charwarning ( mainf , curchr ) ; 
    freeavail ( ligstack ) ; 
    goto lab60 ; 
  } 
  tailappend ( ligstack ) ; 
lab100:
  r_curcmd = getnext () ; 
  if ( (eightbits)r_curcmd == letter || (eightbits)r_curcmd == otherchar
	|| (eightbits)r_curcmd == chargiven )
      goto lab101;

  r_curcmd = xtoken () ;
  if ( (eightbits)r_curcmd == letter || (eightbits)r_curcmd == otherchar
	|| (eightbits)r_curcmd == chargiven )
      goto lab101;
  if ( (eightbits)r_curcmd == charnum ) {
      curchr = scancharnum ();	/* curchr = curval; */
      goto lab101 ; 
  }
  if ( (eightbits)r_curcmd == noboundary )
      bchar = nonchar;

  curr = bchar ; 
  ligstack = 0 ; 
  goto lab110 ;

lab101:		/* wird nur ab lab100 bis hier angesprungen */
#ifdef ERW_SHIFTCASE
  if( uccode(0) > 0 && uccode(0) < 3 ) {
    if( curchr > 0 ) {  /* nicht \char0, da \uccode falsch ! */
      if( uccode(0) == 1 ) {	/* lowercase */
	if( lccode(curchr) > 0 )
	  curchr = lccode(curchr);
      } else {			/* uppercase */
	if( uccode(curchr) > 0 )
	  curchr = uccode(curchr);
      }
    }
  }
#endif

  /* adjust_space_factor */
  mains = sfcode ( curchr ) ; 
  if ( mains == 1000 ) 
    curlist .auxfield .hh .v.LH = 1000 ; 
  else if ( mains <= 1000 ) {
    if ( mains > 0 ) 
      curlist .auxfield .hh .v.LH = mains ; 
  } else if ( curlist .auxfield .hh .v.LH < 1000 ) 
    curlist .auxfield .hh .v.LH = 1000 ; 
  else
    curlist .auxfield .hh .v.LH = mains ; 

  fastgetavail ( ligstack ) ; 
  font ( ligstack ) = mainf ; 
  curr = curchr ; 
  character ( ligstack ) = curr ; 
  if ( curr == falsebchar ) 
    curr = nonchar ;

lab110:
#if 1
  if ( mychartag ( maini ) != (ligtag << 8) )	/* (br) */
#else
  if ( chartag ( maini ) != ligtag )
#endif
    goto lab80;

  /* maink = zligkernstart ( mainf ,  maini ) ; */
  /* mainj = fontinfo [ maink ] .qqqq ; */
#ifdef FONTPTR
  ligp = zligkernstart ( mainf, maini );
#else
  ligp = &fontinfo[ zligkernstart ( mainf, maini ) ];
#endif
  mainj = ligp->qqqq;

  if ( skipbyte ( mainj ) <= stopflag )
    goto lab112 ; 

  /* maink = zligkernrestart ( mainf ,  mainj ) ; */
#ifdef FONTPTR
  ligp = zligkernrestart ( mainf ,  mainj );
#else
  ligp = &fontinfo[ zligkernrestart ( mainf ,  mainj ) ];
#endif

lab111:
  mainj = ligp->qqqq;	/* mainj = fontinfo [ maink ] .qqqq ; */

lab112:
  if ( nextchar ( mainj ) == curr ) {
   if ( skipbyte ( mainj ) <= stopflag ) {
    if ( opbyte ( mainj ) >= kernflag ) {
      if ( curl < nonchar ) {
	if ( character ( curlist .tailfield ) == hyphenchar(mainf) ) 
	if ( link ( curq ) > 0 ) 
	insdisc = true ; 
	if ( ligaturepresent ) {
	  register long_halfword mainp;

	  mainp = newligature ( mainf , curl , link ( curq ) ) ; 
	  if ( lfthit ) {
	    subtype ( mainp ) = 2 ; 
	    lfthit = false ; 
	  } 
	  if ( rthit ) 
	  if ( ligstack == 0 ) {
	    incr ( subtype ( mainp ) ) ; 
	    rthit = false ; 
	  } 
	  link ( curq ) = mainp ; 
	  curlist .tailfield = mainp ; 
	  ligaturepresent = false ; 
	}
	if ( insdisc ) {
	  insdisc = false ; 
	  if ( curlist .modefield > 0 ) 
	  tailappend ( newdisc () ) ; 
	} 
      } 
      tailappend ( newkern ( zcharkern ( mainf ,  mainj ) ) ) ; 
      goto lab90 ; 
    } 
    if ( curl == nonchar ) 
      lfthit = true ; 
    else if ( ligstack == 0 ) 
      rthit = true ; 
    {
      if ( interrupt != 0 ) 
      pauseforinstructions () ; 
    } 
    switch ( opbyte ( mainj ) ) {
    case 1 : 
    case 5 : 
      {
	curl = rembyte ( mainj ) ; 
	maini = zcharinfo ( mainf ,  curl ) ; 
	ligaturepresent = true ; 
      } 
      break ; 
    case 2 : 
    case 6 : 
      {
	curr = rembyte ( mainj ) ; 
	if ( ligstack == 0 ) {
	  ligstack = newligitem ( curr ) ; 
	  bchar = nonchar ; 
	} else if ( ischarnode ( ligstack ) ) {
	  register halfword mainp;

	  mainp = ligstack ; 
	  ligstack = newligitem ( curr ) ; 
	  ligptr ( ligstack ) = mainp ; 
	} else
	  character ( ligstack ) = curr ; 
      } 
      break ; 
    case 3 : 
      { register halfword mainp;

	curr = rembyte ( mainj ) ; 
	mainp = ligstack ; 
	ligstack = newligitem ( curr ) ; 
	link ( ligstack ) = mainp ; 
      } 
      break ; 
    case 7 : 
    case 11 : 
      {
	if ( curl < nonchar ) {
	  if ( character ( curlist .tailfield ) == hyphenchar(mainf) ) 
	    if ( link ( curq ) > 0 ) 
	      insdisc = true ; 
	  if ( ligaturepresent ) {
	    register long_halfword mainp;

	    mainp = newligature ( mainf , curl , link ( curq ) ) ; 
	    if ( lfthit ) {
	      subtype ( mainp ) = 2 ; 
	      lfthit = false ; 
	    } 
	    if ( false )
	    if ( ligstack == 0 ) {
	      incr ( subtype ( mainp ) ) ; 
	      rthit = false ; 
	    } 
	    link ( curq ) = mainp ; 
	    curlist .tailfield = mainp ; 
	    ligaturepresent = false ; 
	  }
	  if ( insdisc ) {
	    insdisc = false ; 
	    if ( curlist .modefield > 0 ) 
	    tailappend ( newdisc () ) ; 
	  } 
	} 
	curq = curlist .tailfield ; 
	curl = rembyte ( mainj ) ; 
	maini = zcharinfo ( mainf ,  curl ) ; 
	ligaturepresent = true ; 
      } 
      break ;
    case 0 : /* (br) added to avoid "subql #1" */
    default:
      {
	curl = rembyte ( mainj ) ;
	ligaturepresent = true ;
	if ( ligstack == 0 )
	  goto lab80 ;
	else
	  goto lab91 ;
      }
      break ;
    } 
    if ( opbyte ( mainj ) > 4 && opbyte ( mainj ) != 7 )
      goto lab80 ;
    if ( curl < nonchar )
      goto lab110 ;

    ligp = &fontinfo[ bcharlabel(mainf) ];   /* maink = bcharlabel[mainf]; */
    goto lab111 ;
   }
  }	/* if (nextchar(mainj) == curr) */

  if ( skipbyte ( mainj ) == 0 ) {
    incr (ligp);	/* incr ( maink ); */
  } else {
    if ( skipbyte ( mainj ) >= stopflag )
      goto lab80;
    /* maink = maink + skipbyte ( mainj ) + 1 ; */
    ligp = ligp + skipbyte(mainj) + 1;
  }
  goto lab111 ;

#endif  /* ERW_CONSTITUTE */
}

/* -- end -- */
