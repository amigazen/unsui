#define EXTERN extern
#include "texd.h"

#undef DOSTAT


long_halfword gettoken ( )	/* eightbits */
{ gettoken_regmem
  register long_halfword r_curcmd;

  nonewcontrolsequence = false ; 
  r_curcmd = getnext ();
  nonewcontrolsequence = true ; 

  if ( curcs == 0 ) {
#if defined(AMIGA) && defined(BIG) && defined(LATTICE)
    curtok = ( r_curcmd * 256 ) + curchr;
#else

#ifdef BIG
    register unsigned short *cp = (unsigned short *)&curtok;

    *cp++ = 0;			/* typeof(curtok) = halfword */

    { register unsigned char *ccp = (unsigned char *)cp;

      *ccp++ = r_curcmd;
      *ccp   = curchr;
    }
#else
    register unsigned char *cp = (unsigned char *)&curtok;

    *cp++ = r_curcmd;
    *cp   = curchr;
#endif

#endif
  } else
    curtok = (halfword)cstokenflag + curcs;

  return(r_curcmd);
}


long_halfword getxtoken ( ) 	/* eightbits */
{ getxtoken_regmem
  register long_halfword r_curcmd;

lab20:
  r_curcmd = getnext ();
  if ( (eightbits)r_curcmd <= maxcommand )
     goto lab30;
  if ( (eightbits)r_curcmd >= call ) 
     if ( (eightbits)r_curcmd < endtemplate ) 
	macrocall ();
     else {
	curcs = frozenendv ; 
	curcmd = r_curcmd = endv;
	goto lab30 ; 
     }
  else
     expand ();
  goto lab20;

lab30:
  if ( curcs == 0 ) {
#if defined(AMIGA) && defined(BIG) && defined(LATTICE)
    curtok = ( r_curcmd * 256 ) + curchr;
#else

#ifdef BIG
    register short *cp = (short *)&curtok;

    *cp++ = 0;			/* typeof(curtok) = halfword */
    { register char *ccp = (char *)cp;
      *ccp++ = r_curcmd;
      *ccp   = curchr;
    }
#else
    register char *cp = (char *)&curtok;
    *cp++ = r_curcmd;
    *cp   = curchr;
#endif

#endif
  } else
    curtok = (halfword)cstokenflag + curcs ;

  return(r_curcmd);
}


	/* get next nonblank (nonrelax) token */
	/* = 404./406. repeat getxtoken() until (curcmd != spacer...) */
long_halfword getxnbtoken ( boolean nonrelax ) 	/* eightbits */
{ getxtoken_regmem
  register long_halfword r_curcmd;

lab20:
  r_curcmd = getnext ();
  if ( (eightbits)r_curcmd <= maxcommand ) {
     if( (eightbits)r_curcmd == spacer )
	goto lab20;
     if( nonrelax && (eightbits)r_curcmd == relax )
	goto lab20;
     goto lab30 ;
  }
  if ( (eightbits)r_curcmd >= call ) 
     if ( (eightbits)r_curcmd < endtemplate ) 
	macrocall ();
     else {
	curcs = frozenendv ; 
	curcmd = r_curcmd = endv;
	goto lab30; 	/* curcmd ist: != spacer && != relax */
     }
  else
     expand ();
  goto lab20;

lab30:
  if ( curcs == 0 ) {
#if defined(AMIGA) && defined(BIG) && defined(LATTICE)
    curtok = ( r_curcmd * 256 ) + curchr;
#else
#ifdef BIG
    register short *cp = (short *)&curtok;
    *cp++ = 0;			/* typeof(curtok) = halfword */
    { register char *ccp = (char *)cp;
      *ccp++ = r_curcmd; *ccp   = curchr;
    }
#else
    register char *cp = (char *)&curtok;
    *cp++ = r_curcmd; *cp   = curchr;
#endif
#endif
  } else
    curtok = (halfword)cstokenflag + curcs ;

  return(r_curcmd);
}


long_halfword xtoken ()
{ xtoken_regmem
  register long_halfword r_curcmd = curcmd;

  while ( (eightbits)r_curcmd > maxcommand ) {
    expand ();
    r_curcmd = getnext () ;
  }

  if ( curcs == 0 ) {
#if 0
    curtok = ( r_curcmd * 256 ) + curchr ; 
#else

#ifdef BIG
    register short *cp = (short *)&curtok;

    *cp++ = 0;			/* typeof(curtok) = halfword */
    { register char *ccp = (char *)cp;
      *ccp++ = r_curcmd;
      *ccp   = curchr;
    }
#else
    register char *cp = (char *)&curtok;
    *cp++ = r_curcmd;
    *cp   = curchr;
#endif

#endif
  } else
    curtok = (halfword)cstokenflag + curcs ; 

  return(r_curcmd);
}

#ifdef DOSTAT
static int calls = 0;
static int tokenlistno = 0;
static int externalfile = 0;
static int aligntest = 0;
#endif


long_halfword getnext ( )
{ getnext_regmem

  register instaterecord *curinpp;

  /* register integer k; */
  /* register halfword t; */
  /* register schar cat; */
  register ASCIIcode c, cc;
  register schar d  ;

  register long_halfword tmp_curchr;
  register eightbits r_curcmd;

lab20:
#ifdef INP_PTR
  curinpp = curinput_ptr;
# undef curinput
# define curinput	(*curinpp)
#else
  curinpp = &curinput;
# define curinput	(*curinpp)
#endif

#ifdef DOSTAT
calls++;
#endif

  curcs = 0 ;
  if ( curinput .statefield != tokenlist ) {
    /* 343. Input from external file, ... */
#ifdef DOSTAT
externalfile++;
#endif
lab25:
    if ( curinput.limitfield >= curinput.locfield ) {
      tmp_curchr = buffer[curinput.locfield];
      incr ( curinput .locfield ) ;
lab21:
      r_curcmd = catcode ( tmp_curchr ) ;
      switch ( r_curcmd ) {
      case 9 :		/* 345. Cases where characters are ignored. */
	goto lab25 ;
	break ;
      case 0 :		/* escape */
	{ register schar cat;

	  if ( curinput .locfield > curinput .limitfield )
	    curcs = nullcs ;
	  else {
	    register ASCIIcode *cp, *endp;
	    endp = &buffer[curinput.limitfield];

lab26:
	    cp = &buffer[curinput.locfield];
	    tmp_curchr = *cp++;

	    cat = catcode ( tmp_curchr ) ;
	    if ( cat == 11 )
	      curinput .statefield = 17 ;
	    else if ( cat == 10 )
	      curinput .statefield = 17 ;
	    else curinput .statefield = 1 ;

	    if ( cat == 11 && cp <= endp ) {
	      do {
		tmp_curchr = *cp++;
		cat = catcode ( tmp_curchr ) ;
	      } while ( ! ( cat != 11 || cp > endp ) ) ;

	      if ( *cp == (halfword) tmp_curchr )
		if ( cat == 7 )
		if ( cp < endp ) {
		  c = *(cp + 1);
		  if ( c < 128 ) {
		    d = 2 ;
		    if ( ( c >= 48 && c <= 57 ) || ( c >= 97 && c <= 102 ) )
		      if ( cp + 2 <= endp ) {
			cc = *(cp + 2 );
			if ( ( cc >= 48 && cc <= 57 )
				|| ( cc >= 97 && cc <= 102 ) )
			  incr ( d ) ;
		      }
		  { halfword tmp_curchr;

		    if ( d > 2 ) {
		      tmp_curchr = c - ( (c <= 57) ? 48 : 87);
		      tmp_curchr = 16*tmp_curchr + cc - ((cc <= 57) ? 48 : 87);
		      *(cp - 1) = tmp_curchr;
		    } else 
		      *(cp - 1) = c + ( (c < 64) ? +64 : -64 );
		  }
		    curinput.limitfield -= d;  endp -= d;
		    first -= d;
		    while ( cp <= endp ) {
		      *cp = *(cp + d);
		      cp++;
		    }
		    goto lab26;
		  }
		}

	      if ( cat != 11 )
		--cp;

	      { integer k;
		k = cp - buffer;

		if ( k > curinput .locfield + 1 ) {
		  curcs = idlookup(curinput.locfield, k-curinput.locfield);
		  curinput.locfield = k ;
		  goto lab40 ;
		}
	      }

	    } else {

	      if ( *cp == (halfword) tmp_curchr )
		if ( cat == 7 )
		if ( cp < endp ) {
		  c = *(cp + 1);
		  if ( c < 128 ) {
		    d = 2 ;
		    if ( ( c >= 48 && c <= 57 ) || ( c >= 97 && c <= 102 ) )
		      if ( cp + 2 <= endp ) {
			cc = *(cp + 2 );
			if ( ( cc >= 48 && cc <= 57 )
				|| ( cc >= 97 && cc <= 102 ) )
			  incr ( d ) ;
		      }
		  { halfword tmp_curchr;
		    if ( d > 2 ) {
		      tmp_curchr = c - ( (c <= 57) ? 48 : 87);
		      tmp_curchr = 16*tmp_curchr + cc - ((cc <= 57) ? 48 : 87);
		      *(cp - 1) = tmp_curchr;
		    } else 
		      *(cp - 1) = c + ( (c < 64) ? +64 : -64 );
		  }
		    curinput .limitfield -= d;  endp -= d;
		    first -= d;
		    while ( cp <= endp ) {
		      *cp = *(cp + d);
		      cp++;
		    }
		    goto lab26 ;
		}
	      }
	    }

	    curcs = singlebase + buffer [ curinput .locfield ];
	    incr ( curinput .locfield ) ;
	  }

lab40:	  curcmd = r_curcmd = eqtype ( curcs );
	  curchr = equiv ( curcs );
	  if ( r_curcmd >= outercall ) {
	    checkoutervalidity () ;
	    r_curcmd = curcmd;
	  }
	}
	break ;
      case 13 :		/* active_char */
	{
	  curinput .statefield = 1 ;
	  curcs = tmp_curchr + activebase ;
	  curcmd = r_curcmd = eqtype ( curcs ) ;
	  curchr = equiv ( curcs ) ;
	  if ( r_curcmd >= outercall ) {
	    checkoutervalidity () ;
	    r_curcmd = curcmd;
	  }
	}
	break ;
      case 7 :		/* sup_mark */
	{
	  if( (halfword)tmp_curchr == buffer [ curinput .locfield ] ) {
	    if( curinput .locfield < curinput .limitfield ) {
	      c = buffer [ curinput .locfield + 1 ] ;
	      if ( c < 128 ) {
		curinput .locfield = curinput .locfield + 2 ;
		if ( (c >= 48 && c <= 57) || (c >= 97 && c <= 102) ) {
		  if ( curinput .locfield <= curinput .limitfield ) {
		    cc = buffer [ curinput .locfield ] ;
		    if ( (cc >= 48 && cc <= 57 ) || (cc >= 97 && cc <= 102) ) {
		      incr ( curinput .locfield ) ;
		      tmp_curchr = 16 * ( c - ( (c <= 57) ? 48 : 87 ) );
		      tmp_curchr = tmp_curchr + cc - ( (cc <= 57) ? 48 : 87 );
		      curchr = tmp_curchr;
		      goto lab21 ;
		    }
		  }
		}
		tmp_curchr = c + ( (c < 64) ? +64 : -64 );
		curchr = tmp_curchr;
		goto lab21 ;
	      }
	    }
	  }
	  curinput .statefield = 1 ;
	  curchr = tmp_curchr;
	  curcmd = r_curcmd;
	}
	break ;
      case 15 :		/* invalid_char */
	{
	  curcmd = r_curcmd;
	  curchr = tmp_curchr;

	  print_err("Text line contains an invalid character");
	  zhelp1( STR_H_AFUNNY_SYMBOL );
	  deletionsallowed = false ;
	  error () ;
	  deletionsallowed = true ;
	  goto lab20 ;
	}
	break ;
      case 10 :		/* spacer */
	if( curinput.statefield == 1 ) {
	  curinput.statefield = 17;
	  curchr = 32;
	  curcmd = r_curcmd;
	  return r_curcmd;
	}
	goto lab25;
	break;
      case 5 :		/* carret + midline */
	curinput .locfield = curinput .limitfield + 1 ;
	if( curinput.statefield == 1 ) {
	  curcmd = 10 ;
	  curchr = 32 ;
	  return 10;
	} else if( curinput.statefield == 17 ) {  /* carret + skipblanks */
	  goto getnewline; /* goto lab25; */
	} else {		/* carret + newline */
	  curcs = parloc ;
	  curcmd = r_curcmd = eqtype ( curcs ) ;
	  curchr = equiv ( curcs ) ;
	  if ( r_curcmd >= outercall ) {
	    checkoutervalidity () ;
	    r_curcmd = curcmd;
	  }
	}
	break ;
      case 14 :		/* comment */
	curinput .locfield = curinput .limitfield + 1 ;
	goto getnewline; /* goto lab25; */
	break ;

      case 4 :		/* tabmark + (skipblanks,newline) */
	curinput .statefield = 1 ;
	curchr = tmp_curchr;
	curcmd = r_curcmd;
	if( alignstate != 0 ) {		/* siehe unten [342.] */
	  return r_curcmd;
	}
	break;

      case 1 :		/* leftbrace */
	incr ( alignstate ) ;
	curinput .statefield = 1 ;
	curchr = tmp_curchr;
	curcmd = r_curcmd;
	return r_curcmd;
	break ;

      case 2 :		/* rightbrace */
	decr ( alignstate ) ;
	/* Fall through */

      case 3 :
      case 6 :
      case 8 :
      case 11 :
      case 12 :
	curinput .statefield = 1 ;
	curchr = tmp_curchr;
	curcmd = r_curcmd;
	return r_curcmd;
	break ;

      default:
	curchr = tmp_curchr;
	curcmd = r_curcmd;
	break ;
      }

    } else {

getnewline:

#ifdef DOSTAT
printf("\n STAT(get_next): calls: %d  external: %d  token: %d  aligntest: %d\n",
calls, externalfile, tokenlistno, aligntest);
#endif

      curinput .statefield = 33 ;
      if ( curinput .namefield > 17 ) {
	incr ( line ) ;
	first = curinput .startfield ;
	if ( ! forceeof ) {
	  if ( inputln ( inputfile [ curinput .indexfield ] , true ) ) {
	    curinput.limitfield = last;	/* (br) added */
	    if ( pausing > 0 )		/* (br) added */
	      firmuptheline () ;
	  } else
	    forceeof = true ;
	}
	if ( forceeof ) {
	  printchar ( 41 ) ;
	  decr ( openparens ) ;
	  flush ( stdout ) ;
	  forceeof = false ;
	  endfilereading () ;
	  checkoutervalidity () ;
	  goto lab20 ;
	}
	if ( endlinecharinactive () )
	  decr ( curinput .limitfield ) ;
	else
	  buffer [ curinput .limitfield ] = endlinechar ;
	first = curinput .limitfield + 1 ;
	curinput .locfield = curinput .startfield ;
      } else {
	if ( ! ( curinput .namefield == 0 ) ) {
	  curcmd = 0 ;
	  curchr = 0 ;
	  return 0;	/* curcmd */
	}
	if ( inputptr > 0 ) {
	  endfilereading () ;
	  goto lab20 ;
	}
	if ( selector < logonly )
	  openlogfile () ;
	if ( interaction > nonstopmode ) {
	  if ( endlinecharinactive () )
	    incr ( curinput .limitfield ) ;
	  if ( curinput .limitfield == curinput .startfield )
	    c_printnl("(Please type a command or say `\\end')");
	  println () ;
	  first = curinput .startfield ;
	  terminput("*");
	  curinput .limitfield = last ;
	  if ( endlinecharinactive () )
	    decr ( curinput .limitfield ) ;
	  else
	    buffer [ curinput .limitfield ] = endlinechar ;
	  first = curinput .limitfield + 1 ;
	  curinput .locfield = curinput .startfield ;
	} else
	  fatalerror( STR_H_FE_JOBAB_NO );
      }
      if ( interrupt != 0 )
	pauseforinstructions () ;
      goto lab25 ;
    }

  } else if ( curinput .locfield == 0 ) {  /* 357. Input from token list */

    endtokenlist () ;
    goto lab20 ;

  } else {

#ifndef REG_A5
    register memoryword *mem = zmem;
#endif
    register halfword *t;

#ifdef DOSTAT
tokenlistno++;
#endif
    t = &(info ( curinput.locfield )) ;
    curinput.locfield = link ( curinput.locfield ) ;
    if ( *t >= cstokenflag ) {
      curcs = *t - cstokenflag ;
      curcmd = r_curcmd = eqtype ( curcs ) ;
      curchr = equiv ( curcs ) ;
      if ( r_curcmd >= outercall )
	if ( r_curcmd == dontexpand ) {
	  curcs = info ( curinput .locfield ) - cstokenflag ;
	  curinput .locfield = 0 ;
	  curcmd = r_curcmd = eqtype ( curcs ) ;
	  curchr = equiv ( curcs ) ;
	  if ( r_curcmd > maxcommand ) {
	    curcmd = r_curcmd = 0 ;
	    curchr = noexpandflag ;
	  }
	} else {
	  checkoutervalidity () ;
	  r_curcmd = curcmd;
	}
    } else {
#if 1
      register unsigned char *cp = (unsigned char *)t + sizeof(halfword)-2;
      curcmd = r_curcmd = *cp++;
      curchr = *cp;
#else
      curcmd = r_curcmd = t / 256 ;
      curchr = t % 256 ;
#endif
      switch ( r_curcmd ) {
      case 0 :
	return r_curcmd;
      case 1 :
	incr ( alignstate ) ;
	return r_curcmd;
      case 2 :
	decr ( alignstate ) ;
	return r_curcmd;
      case 4 :
	if ( alignstate != 0 )	/* siehe unten [342.] */
	  return r_curcmd;
	break;
      case 5 :
	begintokenlist ( paramstack [paramstart + curchr - 1] , parameter ) ;
	goto lab20 ;
	break ;
      default:
	return r_curcmd;
	break ;
      }
    }
  }


#ifdef DOSTAT
aligntest++;
#endif

  /* 342. If an alignment entry ... [curcmd == 4 || curcmd == 5] */

  if ( r_curcmd <= 5 )
    if ( r_curcmd >= 4 )
      if ( alignstate == 0 ) {

#ifndef REG_A5
        register memoryword *mem = zmem;  
#endif

	if ( scannerstatus == aligning )
	   fatalerror( STR_H_FE_INTERWOVEN );
	curcmd = extrainfo ( curalign ) ;
	extrainfo ( curalign ) = curchr ;
	if ( curcmd == omit )
	   begintokenlist ( omittemplate , vtemplate ) ;
	else
	   begintokenlist ( vpart ( curalign ) , vtemplate ) ;
	alignstate = 1000000L ;
	goto lab20 ;
      }

  return r_curcmd;
}

#undef curinput
