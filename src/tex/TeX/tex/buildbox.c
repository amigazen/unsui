#define EXTERN extern
#include "texd.h"

   static void
boxend( integer boxcontext, long_halfword curbox )
{ boxend_regmem

  if ( boxcontext < 1073741824L ) {
    if ( curbox != 0 ) {
      shiftamount ( curbox ) = boxcontext; 
      if ( abs ( curlist.modefield ) == vmode ) {
	appendtovlist( curbox );
	if ( adjusttail != 0 ) {
	  if ( adjusthead != adjusttail ) {
	    link ( curlist .tailfield ) = link ( adjusthead );
	    curlist .tailfield = adjusttail;
	  }
	  adjusttail = 0;
	}
	if ( curlist .modefield > 0 )
	  buildpage ();
      } else {
	if ( abs ( curlist .modefield ) == hmode ) 
	  curlist .auxfield .hh .v.LH = 1000 ; 
	else {
	  register long_halfword p;

	  p = newnoad();
	  mathtype ( nucleus ( p ) ) = subbox;
	  info ( nucleus ( p ) ) = curbox;
	  curbox = p;
	}
	link ( curlist .tailfield ) = curbox;
	curlist .tailfield = curbox;
      }
    }
  } else if ( boxcontext < 1073742336L ) {
    if ( boxcontext < 1073742080L ) 
      eqdefine( boxbase - 1073741824L + boxcontext, boxref, curbox );
    else
      geqdefine( boxbase - 1073742080L + boxcontext, boxref, curbox );
  } else if ( curbox != 0 ) {
    if ( boxcontext > 1073742336L ) {
      register eightbits r_curcmd;

      r_curcmd = getxnbtoken(1);
      if ( ( r_curcmd == hskip && abs( curlist .modefield ) != vmode )
	   || ( r_curcmd == vskip && abs( curlist .modefield ) == vmode )
	   || ( r_curcmd == mskip && abs( curlist .modefield ) == mmode ) ) {
	appendglue();
	subtype( curlist .tailfield ) = boxcontext - (1073742337L - aleaders);
	leaderptr( curlist .tailfield ) = curbox;
      } else {
	print_err("Leaders not followed by proper glue");
	zhelp1( STR_H_YOU_SHOULD_LEADERS );
	backerror();
	flushnodelist( curbox );
      }
    } else {
      shipout( curbox );
    }
  }
}


void beginbox ( integer boxcontext )
{ beginbox_regmem
  register halfword curbox;	/* (br) made local */

  switch ( curchr ) {
  case 0 : 
   { register integer r_curval;

     r_curval = scaneightbitint ();
     curbox = box ( r_curval );
     box ( r_curval ) = 0;
     break ;
   }
  case 1 :
    curbox = copynodelist( box( scaneightbitint() ) );
    break;
  case 2 : 
    curbox = 0;
    if ( abs(curlist.modefield) == mmode ) {
	youcant();
	zhelp1( STR_H_SORRY_LASTBOX_VOID );
	error();
    } else if ( ( curlist .modefield == vmode )
		&& ( curlist .headfield == curlist .tailfield ) ) {
	youcant();
	zhelp2( STR_H_SORRY_CANTTAKETHINGS, STR_H_THIS_LASTBOX_VOID );
	error();
    } else {
	if ( ! ischarnode ( curlist .tailfield ) ) 
	if ( ( ztype ( curlist .tailfield ) == hlistnode )
	  || ( ztype ( curlist .tailfield ) == vlistnode ) ) 
	{
	  register halfword p, q ;

	  q = curlist .headfield ; 
	  do {
	    p = q ;
	    if ( ! ischarnode ( q ) ) 
	    if ( ztype ( q ) == discnode ) {
	      register quarterword m;

	      for( m = replacecount ( q ) ; m >= 1 ; --m ) {
		p = link ( p ) ; 
	      }
	      if ( p == curlist .tailfield ) 
	        goto lab30 ; 
	    }
	    q = link ( p ) ; 
	  } while ( q != curlist .tailfield );
	  curbox = curlist .tailfield ; 
	  shiftamount ( curbox ) = 0 ; 
	  curlist .tailfield = p ; 
	  link ( p ) = 0 ; 
lab30: ; 
	}
    }
    break ; 
  case 3 : 
    { register /*eightbits*/ integer n;

      n = scaneightbitint ();	/* n = curval ; */
      if ( ! scankeyword ( STR_TO ) ) {
	print_err("Missing `to' inserted");
	zhelp1( STR_H_IM_WORKING_VSPLIT );
	error();
      }
      curbox = vsplit( n, scandimen( false, false, false ) );
    }
    break;
  default: 
    { register /*halfword*/ short k;

      k = curchr - 4;	/* curchr in [0,vmode,hmode] */
      saved(0) = boxcontext;
      if ( k == hmode ) {
        if ( boxcontext < 1073741824L && ( abs(curlist.modefield) == vmode ) )
	  scanspec( adjustedhboxgroup, true );
        else
	  scanspec( hboxgroup, true );
#ifdef ERW_LANGUAGE
        curlist .lhmfield = normmin(lefthyphenmin);
        curlist .rhmfield = normmin(righthyphenmin);
#endif
      } else {
	if ( k == vmode ) 
	  scanspec( vboxgroup, true );
	else {
	  scanspec( vtopgroup, true );
	  k = vmode;
	}
	normalparagraph();
      }
      pushnest();
      curlist.modefield = - k;
      if ( k == vmode ) {
	curlist .auxfield .cint = ignoredepth;
	if ( everyvbox != 0 )
	  begintokenlist( everyvbox, everyvboxtext );
      } else {
	curlist .auxfield .hh .v.LH = 1000;
#ifdef ERW_LANGUAGE
        curlist .auxfield .hh .v.RH = 0;  /* clang */
#endif
	if ( everyhbox != 0 )
	  begintokenlist( everyhbox, everyhboxtext );
      }
      return;
    }
    break;
  }
  boxend( boxcontext, curbox ) ;  /* (br) */
}


void scanbox ( integer boxcontext )
{ scanbox_regmem
  register eightbits r_curcmd;

  r_curcmd = getxnbtoken(1);
  if ( r_curcmd == make_box )
    beginbox ( boxcontext );
  else if ( boxcontext >= 1073742337L
		&& (r_curcmd == hrule || r_curcmd == vrule) ) {
    /* curbox = scanrulespec () ; */ /* (br) */
    boxend ( boxcontext, scanrulespec() );
  } else {
    print_err("A <box> was supposed to be here");
    zhelp1( STR_H_IWAS_EXPECT_HVBOX );
    backerror();
  }
}


void package ( smallnumber c )
{ package_regmem
  scaled d;

  d = boxmaxdepth;
  unsave();
  saveptr -= 3;

  { register long_halfword curbox;	/* (br) made local */

    if( curlist.modefield == -hmode )
      curbox = hpack( link(curlist.headfield ), saved(2), saved(1) );
    else {
      curbox = vpackage( link(curlist.headfield), saved(2), saved(1), d );
      if ( c == 4 ) {
	register scaled h;
	register halfword p;

	p = listptr ( curbox ) ; 
	if ( p != 0 && ztype(p) <= rulenode ) 
	  h = height(p);
	else
	  h = 0;
	depth(curbox) = depth(curbox) - h + height(curbox);
	height(curbox) = h;
      }
    }
    popnest();
    boxend( saved(0), curbox ); /* (br) */
  }
}

/* -- end -- */
