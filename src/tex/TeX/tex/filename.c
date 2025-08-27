/* =========  Part 29: File Names =========== */

#define EXTERN extern
#include "texd.h"


/* Diese Routinen sollte man so abaendern, dass sie vielleicht statt dem
 * String Pool feste oder dynamische C-Arrays benutzen.  TeX hat jetzt die
 * unangenehme Eigenschaft, fuer jeden(!) Filenamen, selbst wenn er schon
 * vorhanden ist, einen neuen String zu verwenden.
 */


/* Als erste einfache Abhilfe werden die letzten FNCACHELEN Stringnummern
 * fuer Files in `fncache[]' eingetragen... und bevor ein neuer String fuer
 * einen Filenamen erzeugt wird, wird dort nachgesehen.
 */
#ifdef FILENAMECACHE

static strnumber fn_cache[FNCACHELEN];
static char  fn_cache_sc[FNCACHELEN];  /* Second Chance Zaehler */
static short fn_cache_rover = 0;       /* letzte Position */


#if 0

/* Die Funktion `flushstring' kann nur den zuletzt erzeugten String
 * entfernen.  `deletestring(s)' entfernt den beliebigen String Nummer `s'.
 * Dabei sollte `s' nicht allzuweit vom Ende des Pools entfernt sein, da es
 * sonst etwas langsam vor sich geht.
 * Ausserdem darf keiner der Strings > s schon irgendwo abgespeichert
 * worden sein, d.h. die Stringnummern duerfen nicht im Cache sein oder
 * in einer der Variablen curarea, curname, curext....
 */

  static void
delete_string(strnumber s)
{ long len;

c_printnl("Deleting: "); print(s); println();

  if( s >= strptr ) {
    printf("!! Internal Error (Filename Cache [%d]) !!\n", 1);
    return;
  }

  len = length(s);

c_printnl("Str1: "); printf(
"poolptr: %d  str: %d  str-1: %d  str+1 %d  len: %d strptr: %d",
poolptr, strstart[strptr], strstart[strptr-1], strstart[strptr+1],
len, strptr);
println();
c_printnl("Str1: "); printf(
"s: %d  s: %d  s-1: %d  s+1 %d", s, strstart[s], strstart[s-1], strstart[s+1]);
println();

  /* Kopiere den restlichen Stringpool "herunter" */
  /* memcpy( dst, src, size ); */
  memcpy( &strpool[strstart[s]], &strpool[strstart[s+1]],
          poolptr - strstart[s+1] );

  /* poolptr und strptr erniedrigen... */
  poolptr -= len;
  --strptr;

  /* Passe strstart[] oberhalb des Strings an (nicht strstart[s]!) */
  s++;
  while( s <= strptr ) {
    strstart[s] = strstart[s+1] - len;
    s++;
  }

c_printnl("Str2: "); printf(
"poolptr: %d  str: %d  str-1: %d  str+1 %d  len: %d strptr: %d",
poolptr, strstart[strptr], strstart[strptr-1], strstart[strptr+1],
len, strptr);
println();
c_printnl("Str2: "); printf(
"s: %d  s: %d  s-1: %d  s+1 %d", s, strstart[s], strstart[s-1], strstart[s+1]);
println();

  if( poolptr != strstart[strptr] )
    printf("!! Internal Error (Filename Cache [%d]) !!\n", 2);
}
#endif


/* Suchen im Filename-Cache, liefert neue Stringnummer zurueck, alter String
 * wird geloescht (Man huete sich davor lookup_fncache() mit einer String-
 * nummer aufzurufen, die im Cache schon ist und spaeter noch benoetigt wird!)
 */

  static int
lookup_fncache(strnumber search)
{ short cur_ptr = fn_cache_rover;

  /* Verhindere, dass die festen Strings gesucht und evtl. geloescht werden
   * ( makenamestring() liefert z.B. auch 63 zurueck).
   * Und da ist es gleich besser, nur den zuletzt mit makestring() erzeugten
   * String (und den gerade in endname() erzeugten) zuzulassen.
   */
  if( search < (strptr - 1) )
    return( 0 );	/* not found */

  while( 1 ) {

    /* Stringnummer im Cache ?? */
    if( fn_cache[cur_ptr] != 0 ) {   /* gueltiger Eintrag?? */

      /* Der Vergleich
       *     fn_cache[cur_ptr] < strptr
       * ist notwendig, da ab TeX 3.1415 in \S 1260 Code enthalten ist,
       * der den neuen gelesenen Fontnamen von schon vorhandenen Fonts
       * loescht und den alten verwendet.  Dabei koennte dieser Name
       * schon hier eingetragen sein!!
       * Dieser Test alleine bringt's jedoch nicht, deshalb wird vorher
       * schon explizit gegen alle Fontnamen abgeprueft!
       */
      if( fn_cache[cur_ptr] >= strptr ) {  /* wirklich: gueltiger Eintrag?? */
	fn_cache[cur_ptr] = 0;  /* nein: Eintrag loeschen */
      } else {
	if( str_eq_str(search, fn_cache[cur_ptr]) ) {
	  fn_cache_rover = cur_ptr;
	  /* Second Chance Zaehler auf `belegt' setzen */
          fn_cache_sc[cur_ptr] = 0;
	  /* und Stringnummer aus Cache zurueckgeben */
	  return( fn_cache[cur_ptr] );
	}
      }
    }

    /* Zeiger erhoehen (und am Cache-Ende wieder auf den Anfang setzen) */
    cur_ptr++;  cur_ptr %= FNCACHELEN;

    /* schon einmal im Kreis herum und immer noch nicht gefunden...? */
    if( cur_ptr == fn_cache_rover )
	break;
  }

  /* Nicht gefunden, jetzt mit ``Second Chance'' verdraengen. */

  while( 1 ) {
    /* Zeiger erhoehen (und am Cache-Ende wieder auf den Anfang setzen) */
    cur_ptr++;  cur_ptr %= FNCACHELEN;

    /* keine Second Chance mehr ? */
    if( fn_cache_sc[cur_ptr] >= 2 )
	break;

    /* Second Chance Zaehler, von `belegt' (0) auf `gealtert' (1)
     * bzw. von `gealtert' auf `frei' (2) setzen.
     */
    fn_cache_sc[cur_ptr] += 1;
  }

  fn_cache_rover = cur_ptr;

  /* String eintragen und als `belegt' markieren */
  fn_cache[cur_ptr] = search;
  fn_cache_sc[cur_ptr] = 0;

  return( 0 );	/* not found */
}


/* Ersetze einen moeglichen Eintrag des Strings `old' im Cache durch
 * den String `new'.
 * Dies ist immer dann notwendig, wenn ein String durch einen anderen
 * (mit gleichem Inhalt!) ersetzt wird und der alte mit `flushstring()'
 * geloescht wird.
 * Momentan nur in \S 1260 new_font() benoetigt...
 * und dort tritt dies nur auf, wenn ein Fontname gescannt wird, der
 * nicht mehr im Cache ist und deshalb unter der Nummer eingetragen
 * wird.  Anschliessend ersetzt der Original-TeX-Code diesen String
 * durch den in `fontname(f)' gefundenen und dies muss auch im Cache
 * nachvollzogen werden!
 */

  static void
update_fncache(strnumber old, strnumber new)
{ short cur_ptr;
  char errorflag = 0;

  /* fn_cache[] enthaelt 0 als Anzeichen fuer einen leeren Eintrag.
   * `old' sollte nie 0 sein, da die Bedingung in der Schleife
   * eigentlich
   *    if( fn_cache[cur_ptr] != 0 && fn_cache[cur_ptr] == old )
   * lautet.  Stringnummer 0 steht im Grunde fuer "@" und koennte
   * vorkommen, wird aber von `lookup_fncache()' nie zurueckgeliefert
   * und auch nicht in den Cache eingetragen (wieso wohl?!).
   * Vorsichtshalber trotzdem abpruefen...
   */
  if ( old == 0 ) {
    printf("\n!! Internal Error (Filename Cache Update [%d]) !!\n", 1);
    return;
  }

  for( cur_ptr = FNCACHELEN - 1 ; cur_ptr >= 0 ; cur_ptr-- ) {
    if( fn_cache[cur_ptr] == old ) {
      fn_cache[cur_ptr] = new;
      /* Der zu ersetzende Eintrag sollte nur einmal drin sein!! */
      if ( errorflag )
        printf("\n!! Internal Error (Filename Cache Update [%d]) !!\n", 2);
      errorflag = 1;
    }
  }
}

#endif   /* FILENAMECACHE */



/*
 *  Die naechsten drei Routinen werden nur von scanfilename() und
 *  promptfilename() aufgerufen.
 */

  static
void beginname ( void )
{ beginname_regmem 

  areadelimiter = 0;
  extdelimiter = 0;
} 


  static
boolean morename ( ASCIIcode c )
{ morename_regmem

  if ( c == 32 )
    return(false);

  strroom( 1 );
  appendchar( c );
  if ( c == 47 ) {		/* / */
    areadelimiter = curlength;
    extdelimiter = 0;
  } else if ( c == 46 )		/* . */
    extdelimiter = curlength;

  return(true);
}


  static
void endname ( void )
{ endname_regmem
  strnumber tempstr;
  long i;

  if ( strptr + 3 > maxstrings )
    overflow(1, maxstrings - initstrptr);

  /* Falls kein Directory angegeben wurde, ist curarea = "",
   * ansonsten wird curarea auf das Directory gesetzt.
   */
  if ( areadelimiter == 0 )
    curarea = 335;
  else {
    curarea = strptr;
    strstart[strptr + 1] = strstart[strptr] + areadelimiter;
    incr ( strptr );
#ifdef FILENAMECACHE
    if( (tempstr = lookup_fncache(curarea)) != 0 ) {
	curarea = tempstr;
	decr(strptr);	/* nicht flushstring, da sonst poolptr falsch */

	/* Name + Ext des Filenamens herunterkopieren */
	for( i = strstart[strptr + 1] ; i < poolptr ; i++ )
	  strpool[i - areadelimiter] = strpool[i];
	/* Poolpointer korrigieren */
	poolptr -= areadelimiter;
    }
#endif
  }

  /* Falls keine Extension angegeben wurde, ist curext = "" und curname
   * der aktuelle String.
   * Ansonsten ist curname der String bis zu curext und curext der restliche
   * aktuelle String.
   */
  if ( extdelimiter == 0 ) {
    curext = 335;
#ifdef FILENAMECACHE
    if( curlength == 0 ) {
      curname = 335;
    } else {
      curname = makestring();
      if( (tempstr = lookup_fncache(curname)) != 0 ) {
	curname = tempstr;
	flushstring;	/* poolptr wird automatisch richtig gesetzt */
      }
    }
#else
    curname = makestring();
#endif
  } else {
    curname = strptr;
    strstart[strptr+1] = strstart[strptr] + extdelimiter - areadelimiter - 1;
    incr ( strptr );
#ifdef FILENAMECACHE
    if( (tempstr = lookup_fncache(curname)) != 0 ) {
	curname = tempstr;
	decr(strptr);	/* nicht flushstring, da sonst poolptr falsch */

	/* Extension des Filenamens herunterkopieren */
	for( i = strstart[strptr+1] ; i < poolptr; i++ )
	  strpool[i - extdelimiter + areadelimiter + 1] = strpool[i];
	poolptr = poolptr - extdelimiter + areadelimiter + 1;
    }
#endif

    curext = makestring();
#ifdef FILENAMECACHE
    /* Zuerst mal auf ".tex" abpruefen.. */
    if( str_eq_str(STR_DOT_TEX, curext) ) {
	curext = STR_DOT_TEX;
	flushstring;
    } else if( (tempstr = lookup_fncache(curext)) != 0 ) {
	curext = tempstr;
	flushstring;
    }
#endif
  }
}



void packfilename( strnumber n, strnumber a, strnumber e )
{ packfilename_regmem
  register integer k;
  register ASCIIcode c;
  register poolpointer j;

  k = 0;
  { register integer for_end;
    j = strstart[a];  for_end = strstart[a + 1] - 1;
    if ( j <= for_end) do {
      c = strpool[j];
      incr ( k );
      if ( k <= FILENAMESIZE-1 )
	nameoffile [ k ] = xchr [ c ];
    } while ( j++ < for_end ); 
  }
  { register integer for_end;
    j = strstart [ n ] ; for_end = strstart [ n + 1 ] - 1 ;
    if ( j <= for_end) do {
      c = strpool [ j ];
      incr ( k ) ; 
      if ( k <= FILENAMESIZE-1 )
	nameoffile [ k ] = xchr [ c ] ; 
    } while ( j++ < for_end ) ;
  }
  { register integer for_end;
    j = strstart [ e ] ; for_end = strstart [ e + 1 ] - 1 ;
    if ( j <= for_end) do {
      c = strpool [ j ];
      incr ( k );
      if ( k <= FILENAMESIZE-1 ) 
	nameoffile [ k ] = xchr [ c ];
    } while ( j++ < for_end );
  }

  if ( k <= FILENAMESIZE-1 )
    namelength = k;
  else
    namelength = FILENAMESIZE-1;

  { register integer for_end;
    k = namelength + 1 ; for_end = FILENAMESIZE;
    if ( k <= for_end) do 
      nameoffile [ k ] = ' ';
    while ( k++ < for_end );
  }
}


void packbufferedname ( smallnumber n, integer a, integer b )
{ packbufferedname_regmem 
  register integer k;
  register ASCIIcode c;
  register integer j;

  if ( n + b - a + 5 > FILENAMESIZE-1 ) 
    b = a + (FILENAMESIZE-1) - n - 5;
  k = 0;
  { register integer for_end;
    j = 1; for_end = n;
    if ( j <= for_end) do {
      c = xord [ TEXformatdefault[j] ];
      incr ( k );
      if ( k <= FILENAMESIZE-1 )
	nameoffile [ k ] = xchr [ c ];
    } while ( j++ < for_end );
  }
  { register integer for_end;
    j = a; for_end = b;
    if ( j <= for_end) do {
      c = buffer [ j ];
      incr ( k );
      if ( k <= FILENAMESIZE-1 )
	nameoffile [ k ] = xchr [ c ];
    } while ( j++ < for_end );
  }
  { register integer for_end;
    j = formatdefaultlength - 3; for_end = formatdefaultlength;
    if ( j <= for_end) do {
      c = xord [ TEXformatdefault[j] ];
      incr ( k );
      if ( k <= FILENAMESIZE-1 )
	nameoffile [ k ] = xchr [ c ];
    } while ( j++ < for_end );
  }
  if ( k <= FILENAMESIZE-1 )
    namelength = k;
  else
    namelength = FILENAMESIZE-1;

  { register integer for_end;
    k = namelength + 1; for_end = FILENAMESIZE;
    if ( k <= for_end) do 
      nameoffile [ k ] = ' ';
    while ( k++ < for_end );
  }
}


strnumber makenamestring ( void )
{ makenamestring_regmem
  register integer k;

  if ( ( poolptr + namelength > poolsize ) || ( strptr == maxstrings )
	|| ( curlength > 0 ) )
    return( (strnumber)63 );   /* "?" */

  { register integer for_end; k = 1 ; for_end = namelength;
    if ( k <= for_end) do 
      appendchar ( xord [ nameoffile [ k ] ] ) ; 
    while ( k++ < for_end );
  }

  return( makestring () );
}


void scanfilename ( void )
{ scanfilename_regmem
  register eightbits r_curcmd;

  nameinprogress = true;
  beginname();
  r_curcmd = getxnbtoken(0);

  while ( true ) {
    if ( r_curcmd > 12 || (curchr > 255) ) {
      backinput ();
      goto lab30;
    }
    if ( ! morename( curchr ) )
      goto lab30;
    r_curcmd = getxtoken();
  }

lab30:
  endname();
  nameinprogress = false;
}


void packjobname ( strnumber s )
{ packjobname_regmem

  curarea = 335 ;
  curext = s ;
  curname = jobname ;
  packfilename ( curname , curarea , curext );
}


/* Da wir nach der Adresse des naechsten Strings abfragen (ist schneller
 * als `strcmp()', wird er hier einmalig angelegt.
 */
static char input_file_name[] = "input file name";


void promptfilename ( char *s, strnumber e )
{/* 30 */ promptfilename_regmem 
  register integer k  ; 

  if ( interaction == scrollmode ) 
    wakeupterminal () ; 
  if ( s == input_file_name ) {
    print_err("I can't find file `");
  } else {
    print_err("I can't write on file `");
  } 
  printfilename ( curname , curarea , curext ) ; 
  c_print("'.");
  if ( e == STR_DOT_TEX )
    showcontext();
  c_printnl("Please type another ");
  c_print ( s );
  if ( interaction < scrollmode ) 
    fatalerror( STR_H_FE_JOBAB_FILE );
  clearterminal () ; 
  terminput(": ");
  {
    beginname () ; 
    k = first ; 
    while ( ( buffer [ k ] == 32 ) && ( k < last ) )
      incr ( k );
    while ( true ) {
      if ( k == last ) 
	goto lab30 ; 
      if ( ! morename ( buffer [ k ] ) ) 
	goto lab30 ; 
      incr ( k ) ; 
    }
lab30:
    endname () ; 
  } 
  if ( curext == 335 ) 
    curext = e;
  packfilename ( curname , curarea , curext ) ; 
}


void openlogfile ( void )
{ openlogfile_regmem 
  integer oldsetting;
  register integer k;
  register integer l;
  ccharpointer months;

  oldsetting = selector;

  if ( jobname == 0 ) 
    jobname = STR_TEXPUT;
  packjobname( STR_DOT_LOG );  
  while ( ! aopenout ( logfile ) ) {
    selector = termonly ; 
    promptfilename("transcript file name", STR_DOT_LOG );
  }
  logname = amakenamestring ( logfile ) ; 
  selector = logonly ; 
  logopened = true ; 
  {
#if 0
    (void) Fputs( logfile ,  "This is TeX, C Version 3.1t2" ) ; 
#else
    (void) Fputs( logfile, banner );
#endif
#ifdef MLTEX
  /* if user has given ML-TeX switch in IniTeX, output a second line */
  if( is_ML_TeX )
    (void) Fputs(logfile, "\nThis is ML-TeX, Version 3");
#endif
#ifdef TEXXET
  if( is_TeX_XeT )
    (void) Fputs(logfile, "\nThis is TeX--XeT (1.0)");
#endif
    slowprint ( formatident );  /* TeX 3.141 */
    c_print("  ");
    printint ( zday ) ; 
    printchar ( 32 ) ; 
    months = "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC";
    {register integer for_end; k = 3 * zmonth - 3 ; for_end = 3 * zmonth - 1
    ; if ( k <= for_end) do 
      (void) putc( months [ k ] ,  logfile );
    while ( k++ < for_end ) ; } 
    printchar ( 32 ) ; 
    printint ( zyear ) ; 
    printchar ( 32 ) ; 
    printtwo ( ztime / 60 ) ; 
    printchar ( 58 ) ; 
    printtwo ( ztime % 60 ) ; 
  }
#ifdef INP_PTR
  /*  Da bei INP_PTR curinput auf oberstes Element von inputstack[] zeigt ..
   *  ist nichts zu tun.
   */
#else
  inputstack [ inputptr ] = curinput; 
#endif
  c_printnl("**");
  l = inputstack [ 0 ] .limitfield ; 
  if ( buffer [ l ] == endlinechar ) 
    decr ( l );
  {register integer for_end; k = 1 ; for_end = l ; if ( k <= for_end) do 
    print ( buffer [ k ] ) ; 
  while ( k++ < for_end ) ; } 
  println () ; 
  selector = oldsetting + 2 ; 
}


void startinput ( void )
{/* 30 */ startinput_regmem 

  scanfilename ();
  if ( curext == 335 ) 
    packfilename ( curname , curarea , STR_DOT_TEX );
  else
    packfilename ( curname , curarea , curext );

  while ( true ) {
    beginfilereading () ; 
    if ( aopenin ( inputfile [ curinput.indexfield ] , TEXINPUTPATH ) ) 
      goto lab30 ; 
    if ( curext == 335 ) {
      packfilename ( curname , curarea , curext ) ; 
      if ( aopenin ( inputfile [ curinput.indexfield ] , TEXINPUTPATH ) ) 
      goto lab30 ; 
    } 
    endfilereading () ; 
    promptfilename( input_file_name, STR_DOT_TEX );
  }
lab30:
  curinput.namefield = amakenamestring ( inputfile[curinput.indexfield] );
#ifdef FILENAMECACHE
  { strnumber tempstr;
    if( (tempstr = lookup_fncache( curinput.namefield )) != 0 ) {
	curinput.namefield = tempstr;
	flushstring;
    }
  }
#endif
  if ( jobname == 0 ) {
    jobname = curname;
    openlogfile();
  }
  if ( termoffset + length ( curinput.namefield ) > maxprintline - 3 )
    println();
  else if ( ( termoffset > 0 ) || ( fileoffset > 0 ) )
    printchar ( 32 );
  printchar ( 40 );
  incr ( openparens );
#if 0  /* TeX 3.141 */
  print ( curinput.namefield );
#else
  slowprint ( curinput.namefield );
#endif
  flush ( stdout );
  curinput.statefield = 33;

  line = 1;
  if ( inputln ( inputfile[ curinput.indexfield ], false ) )
    ;

  curinput.limitfield = last;	/* (br) added */
  if ( pausing > 0 )		/* (br) added */
    firmuptheline () ;

  if ( endlinecharinactive () ) 
    decr ( curinput.limitfield );
  else
    buffer [ curinput.limitfield ] = endlinechar;
  first = curinput.limitfield + 1;
  curinput.locfield = curinput.startfield;
}


/* aus tex7.c: */

static char err_illegal_mag[] =
	"Illegal magnification has been changed to 1000"; /* 548 */

void preparemag ( void )
{ preparemag_regmem 

  static long magset = 0L;

  if ( ( magset > 0 ) && ( mag != magset ) ) {
    print_err("Incompatible magnification (");
    printint ( mag );
    c_print(");");
    c_printnl(" the previous value will be retained");
    zhelp1( STR_H_ICAN_MAGRATIO );
    interror( magset );
    geqworddefine ( intbase + magcode , magset );
  } 
  if ( ( mag <= 0 ) || ( mag > 32768L ) ) {
    print_err(err_illegal_mag);
    zhelp1( STR_H_THEMAGRATIO );
    interror ( mag ) ; 
    geqworddefine ( intbase + magcode , 1000 ) ; 
  } 
  magset = mag;
}


void newfont ( smallnumber a )
{ newfont_regmem
  register scaled s;
  register internalfontnumber f;
  register halfword u;
  strnumber t;
  strnumber flushablestring;

  if ( jobname == 0 )
    openlogfile();

  getrtoken();
  u = curcs;
  if ( u >= hashbase )
    t = ztext ( u );
  else if ( u >= singlebase )
    if ( u == nullcs )
      t = STR_FONT_;
    else
      t = u - singlebase;
  else {
    register integer oldsetting;

    oldsetting = selector;
    selector = newstring;
    print( STR_FONT_ );
    print( u - activebase );
    selector = oldsetting;
    strroom ( 1 );
    t = makestring();
  }

  if ( ( a >= 4 ) )
    geqdefine( u, set_font, nullfont );
  else
    eqdefine( u, set_font, nullfont );

  scanoptionalequals ();
  scanfilename ();
  nameinprogress = true;

  if ( scankeyword ( STR_AT ) ) {
    s = scandimen ( false , false , false );
    if ( s <= 0 || s >= 134217728L ) {
      print_err("Improper `at' size (");
      printscaled ( s ) ; 
      c_print("pt), replaced by 10pt");
      zhelp1( STR_H_ICANONLYHANDLE );
      error();
      s = 10 * unity;
    } 
  } else if ( scankeyword ( STR_SCALED ) ) {
    s = scanint ();	/* s = - (integer) curval; */
    if ( s <= 0 || s > 32768L ) {
      print_err(err_illegal_mag);
      zhelp1( STR_H_THEMAGRATIO );
      interror ( s );
      s = -1000;
    } else
      s = -s;
  } else
    s = -1000;

  nameinprogress = false;
  flushablestring = strptr - 1;  /* TeX 3.141 */
  {register integer for_end; f = 1 ; for_end = fontptr ; if ( f <= for_end) 
  do 
#if 0		/* (br) changed */
    if ( streqstr(fontname(f), curname) && streqstr(fontarea(f), curarea) )
#else
    if( str_eq_str(curname, fontname(f)) && str_eq_str(curarea, fontarea(f)) )
#endif
    {
      if ( curname == flushablestring	/* TeX 3.141 */
        && curname != fontname(f) ) {   /* notwendig mit lookup_fncache()!! */
#ifdef FILENAMECACHE
	update_fncache(curname, fontname(f));
#endif
	flushstring;
	curname = fontname(f);
      }
      if ( s > 0 ) {
	if ( s == fontsize(f) ) 
	goto lab50 ; 
      } else if ( fontsize(f) == xnoverd( fontdsize(f), - (integer) s, 1000 ) )
	goto lab50;
    }
  while ( f++ < for_end ) ; } 

  f = readfontinfo ( u , curname , curarea , s ) ;

lab50:
  equiv ( u ) = f ; 
  eqtb [ fontidbase + f ] = eqtb [ u ] ; 
  fontidtext ( f ) = t ; 
}


/* -- end -- */
