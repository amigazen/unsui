#define EXTERN extern
#include "texd.h"


void initialize ( void )
{ initialize_regmem 
  register integer i ;
  register integer k ;
  register hyphpointer z ;

#ifndef TEXCONFIG		/* see init.c */

#if !defined(atarist) && !defined(AMIGA)
  xchr [ 32 ] = ' ' ; 
  xchr [ 33 ] = '!' ; 
  xchr [ 34 ] = '"' ; 
  xchr [ 35 ] = '#' ; 
  xchr [ 36 ] = '$' ; 
  xchr [ 37 ] = '%' ; 
  xchr [ 38 ] = '&' ; 
  xchr [ 39 ] = '\'' ; 
  xchr [ 40 ] = '(' ; 
  xchr [ 41 ] = ')' ; 
  xchr [ 42 ] = '*' ; 
  xchr [ 43 ] = '+' ; 
  xchr [ 44 ] = ',' ; 
  xchr [ 45 ] = '-' ; 
  xchr [ 46 ] = '.' ; 
  xchr [ 47 ] = '/' ; 
  xchr [ 48 ] = '0' ; 
  xchr [ 49 ] = '1' ; 
  xchr [ 50 ] = '2' ; 
  xchr [ 51 ] = '3' ; 
  xchr [ 52 ] = '4' ; 
  xchr [ 53 ] = '5' ; 
  xchr [ 54 ] = '6' ; 
  xchr [ 55 ] = '7' ; 
  xchr [ 56 ] = '8' ; 
  xchr [ 57 ] = '9' ; 
  xchr [ 58 ] = ':' ; 
  xchr [ 59 ] = ';' ; 
  xchr [ 60 ] = '<' ; 
  xchr [ 61 ] = '=' ; 
  xchr [ 62 ] = '>' ; 
  xchr [ 63 ] = '?' ; 
  xchr [ 64 ] = '@' ; 
  xchr [ 65 ] = 'A' ; 
  xchr [ 66 ] = 'B' ; 
  xchr [ 67 ] = 'C' ; 
  xchr [ 68 ] = 'D' ; 
  xchr [ 69 ] = 'E' ; 
  xchr [ 70 ] = 'F' ; 
  xchr [ 71 ] = 'G' ; 
  xchr [ 72 ] = 'H' ; 
  xchr [ 73 ] = 'I' ; 
  xchr [ 74 ] = 'J' ; 
  xchr [ 75 ] = 'K' ; 
  xchr [ 76 ] = 'L' ; 
  xchr [ 77 ] = 'M' ; 
  xchr [ 78 ] = 'N' ; 
  xchr [ 79 ] = 'O' ; 
  xchr [ 80 ] = 'P' ; 
  xchr [ 81 ] = 'Q' ; 
  xchr [ 82 ] = 'R' ; 
  xchr [ 83 ] = 'S' ; 
  xchr [ 84 ] = 'T' ; 
  xchr [ 85 ] = 'U' ; 
  xchr [ 86 ] = 'V' ; 
  xchr [ 87 ] = 'W' ; 
  xchr [ 88 ] = 'X' ; 
  xchr [ 89 ] = 'Y' ; 
  xchr [ 90 ] = 'Z' ; 
  xchr [ 91 ] = '[' ; 
  xchr [ 92 ] = '\\' ; 
  xchr [ 93 ] = ']' ; 
  xchr [ 94 ] = '^' ; 
  xchr [ 95 ] = '_' ; 
  xchr [ 96 ] = '`' ; 
  xchr [ 97 ] = 'a' ; 
  xchr [ 98 ] = 'b' ; 
  xchr [ 99 ] = 'c' ; 
  xchr [ 100 ] = 'd' ; 
  xchr [ 101 ] = 'e' ; 
  xchr [ 102 ] = 'f' ; 
  xchr [ 103 ] = 'g' ; 
  xchr [ 104 ] = 'h' ; 
  xchr [ 105 ] = 'i' ; 
  xchr [ 106 ] = 'j' ; 
  xchr [ 107 ] = 'k' ; 
  xchr [ 108 ] = 'l' ; 
  xchr [ 109 ] = 'm' ; 
  xchr [ 110 ] = 'n' ; 
  xchr [ 111 ] = 'o' ; 
  xchr [ 112 ] = 'p' ; 
  xchr [ 113 ] = 'q' ; 
  xchr [ 114 ] = 'r' ; 
  xchr [ 115 ] = 's' ; 
  xchr [ 116 ] = 't' ; 
  xchr [ 117 ] = 'u' ; 
  xchr [ 118 ] = 'v' ; 
  xchr [ 119 ] = 'w' ; 
  xchr [ 120 ] = 'x' ; 
  xchr [ 121 ] = 'y' ; 
  xchr [ 122 ] = 'z' ; 
  xchr [ 123 ] = '{' ; 
  xchr [ 124 ] = '|' ; 
  xchr [ 125 ] = '}' ; 
  xchr [ 126 ] = '~' ; 
  {register integer for_end; i = 0 ; for_end = 31 ; if ( i <= for_end) do 
    xchr [ i ] = chr ( i ) ; 
  while ( i++ < for_end ) ; } 
  {register integer for_end; i = 127 ; for_end = 255 ; if ( i <= for_end) do 
    xchr [ i ] = chr ( i ) ; 
  while ( i++ < for_end ) ; } 
  {register integer for_end; i = 0 ; for_end = 255 ; if ( i <= for_end) do 
    xord [ chr ( i ) ] = 127 ; 
  while ( i++ < for_end ) ; } 
  {register integer for_end; i = 128 ; for_end = 255 ; if ( i <= for_end) do 
    xord [ xchr [ i ] ] = i ; 
  while ( i++ < for_end ) ; } 
  {register integer for_end; i = 0 ; for_end = 126 ; if ( i <= for_end) do 
    xord [ xchr [ i ] ] = i ; 
  while ( i++ < for_end ) ; } 
#else
  /* ST and AMIGA use an ASCII encoding, so ... (save space) */
  /* There should be a CodePage support here, which uses xchr[] & xord[] */
  for( i = 0; i < 255; i++ ) {
    xchr[i] = chr(i);
    /* ... and the rest is just this: (only for this xchr[] !!) */
    xord[i] = i;	/* xord[xchr[i]] = i;  xord[127] = 127; */
  }
#endif

#endif	/* TEXCONFIG */

  interaction = errorstopmode ;
  deletionsallowed = true ;
  setboxallowed = true ;
  errorcount = 0 ;
#if 0
  helpptr = 0 ; 
#else
  help0 ;
#endif
  useerrhelp = false ; 
  interrupt = 0 ; 
  OKtointerrupt = true ; 

#ifdef DEBUG
  wasmemend = memmin ; 
  waslomax = memmin ; 
  washimin = memmax ; 
  panicking = false ; 
#endif /* DEBUG */
  nestptr = 0 ; 
  maxneststack = 0 ; 
  curlist .modefield = vmode ;
  curlist .headfield = contribhead ; 
  curlist .tailfield = contribhead ; 
  curlist .auxfield .cint = ignoredepth ; 
  curlist .mlfield = 0 ; 
#if 0  /* 3.1415 */
  curlist .lhmfield = 0 ; 
  curlist .rhmfield = 0 ; 
#endif
#ifdef TEXXET
  LR_save = 0 ;
#endif
  curlist .pgfield = 0 ; 
  shownmode = 0 ; 
  pagecontents = 0 ; 
  pagetail = pagehead ; 
  link ( pagehead ) = 0 ; 
  lastglue = maxhalfword ; 
  lastpenalty = 0 ; 
  lastkern = 0 ; 
  pagesofar [ 7 ] = 0 ; 
  pagemaxdepth = 0 ;
  for( k = intbase ; k <= eqtbsize ; k++ ) {
    xeqlevel [ k ] = levelone ; 
  }
  nonewcontrolsequence = true ; 
  next ( hashbase ) = 0 ; 
  ztext ( hashbase ) = 0 ; 
  for( k = hashbase + 1 ; k <= undefinedcontrolsequence - 1 ; k++ ) {
    hash [ k ] = hash [ hashbase ] ; 
  }
  saveptr = 0 ; 
  curlevel = levelone ; 
  curgroup = bottomlevel ; 
  curboundary = 0 ; 
  maxsavestack = 0 ; 
  /* magset = 0 ; */ /* local nach preparemag() */
  topmark = 0 ; 
  firstmark = 0 ; 
  botmark = 0 ; 
  splitfirstmark = 0 ; 
  splitbotmark = 0 ; 
  curval = 0 ; 
  curvallevel = intval ; 
  radix = 0 ; 
  curorder = 0 ; 
  for( k = 16 ; k >= 0 ; --k )
    readopen [ k ] = closed;
  condptr = 0 ; 
  iflimit = normal ; 
  curif = 0 ; 
  ifline = 0 ; 
  for( k = fontmax ; k >= 0 ; --k )
    fontused(k) = false;
  nullcharacter .b0 = 0 ; 
  nullcharacter .b1 = 0 ; 
  nullcharacter .b2 = 0 ; 
  nullcharacter .b3 = 0 ; 
  totalpages = 0 ; 
  maxv = 0 ; 
  maxh = 0 ; 
  maxpush = 0 ; 
  lastbop = -1 ; 
  doingleaders = false ; 
  deadcycles = 0 ; 
/* curs = -1 ; */
  halfbuf = dvibufsize / 2 ; 
  dvilimit = dvibufsize ; 
  dviptr = 0 ; 
  dvioffset = 0 ; 
  dvigone = 0 ; 
/* downptr = 0 ; */
/* rightptr = 0 ; */
  adjusttail = 0 ; 
  lastbadness = 0 ; 
  packbeginline = 0 ; 
  emptyfield .v.RH = 0 ; 
  emptyfield .v.LH = 0 ; 
  nulldelimiter .b0 = 0 ; 
  nulldelimiter .b1 = 0 ; 
  nulldelimiter .b2 = 0 ; 
  nulldelimiter .b3 = 0 ; 
  alignptr = 0 ; 
  curalign = 0 ; 
  curspan = 0 ; 
  curloop = 0 ; 
  curhead = 0 ; 
  curtail = 0 ; 
  {register integer for_end; z = 0 ; for_end = hyphsize ; if ( z <= for_end) 
  do 
    {
      hyphword(z) = 0 ; 
      hyphlist(z) = 0 ; 
    } 
  while ( z++ < for_end ) ; } 
  hyphcount = 0 ; 
  outputactive = false ; 
  insertpenalties = 0 ; 
  ligaturepresent = false ; 
  /* cancelboundary = false ; */ /* (br) local in main_control() */
  lfthit = false ; 
  rthit = false ;
  /* insdisc = false ; */ /* (br) local in main_control() */
  aftertoken = 0 ; 
  longhelpseen = false ; 
  formatident = 0 ; 
  for( k = 17 ; k >= 0 ; --k )
    writeopen [ k ] = false;
  editnamestart = 0 ; 
#ifdef INITEX
  {register integer for_end; k = membot + 1 ; for_end = lomemstatmax ; if ( k 
  <= for_end) do 
    mem [ k ] .cint = 0 ; 
  while ( k++ < for_end ) ; } 
  k = membot ; 
  while ( k <= lomemstatmax ) {
    gluerefcount ( k ) = 1 ; 
    stretchorder ( k ) = normal ; 
    shrinkorder ( k ) = normal ; 
    k = k + gluespecsize ; 
  } 
  stretch ( filglue ) = unity ; 
  stretchorder ( filglue ) = fil ; 
  stretch ( fillglue ) = unity ; 
  stretchorder ( fillglue ) = fill ; 
  stretch ( ssglue ) = unity ; 
  stretchorder ( ssglue ) = fil ; 
  shrink ( ssglue ) = unity ; 
  shrinkorder ( ssglue ) = fil ; 
  stretch ( filnegglue ) = - (integer) unity ; 
  stretchorder ( filnegglue ) = fil ; 
  rover = lomemstatmax + 1 ; 
  link ( rover ) = emptyflag ; 
  nodesize ( rover ) = 1000 ; 
  llink ( rover ) = rover ; 
  rlink ( rover ) = rover ; 
  lomemmax = rover + 1000 ; 
  link ( lomemmax ) = 0 ; 
  info ( lomemmax ) = 0 ; 
  {register integer for_end; k = himemstatmin ; for_end = memtop ; if ( k <= 
  for_end) do 
    mem [ k ] = mem [ lomemmax ] ; 
  while ( k++ < for_end ) ; } 
  info ( omittemplate ) = endtemplatetoken ; 
  link ( endspan ) = maxquarterword + 1 ; 
  info ( endspan ) = 0 ; 
  ztype ( lastactive ) = hyphenated ; 
  zlinenumber ( lastactive ) = maxhalfword ; 
  subtype ( lastactive ) = 0 ; 
  subtype ( pageinshead ) = 255 ; 
  ztype ( pageinshead ) = splitup ; 
  link ( pageinshead ) = pageinshead ; 
  ztype ( pagehead ) = gluenode ; 
  subtype ( pagehead ) = normal ; 
  avail = 0 ; 
  memend = memtop ; 
  himemmin = himemstatmin ; 
  varused = lomemstatmax + 1 - membot ; 
  dynused = himemstatusage ; 
  eqtype ( undefinedcontrolsequence ) = undefinedcs ; 
  equiv ( undefinedcontrolsequence ) = 0 ; 
  eqlevel ( undefinedcontrolsequence ) = levelzero ; 
  for( k = activebase ; k <= undefinedcontrolsequence - 1 ; k++ ) {
    eqtb [ k ] = eqtb [ undefinedcontrolsequence ] ; 
  }
  equiv ( gluebase ) = zeroglue ; 
  eqlevel ( gluebase ) = levelone ; 
  eqtype ( gluebase ) = glueref ; 
  for( k = gluebase + 1 ; k <= localbase - 1 ; k++ ) {
    eqtb [ k ] = eqtb [ gluebase ] ; 
  }
  gluerefcount ( zeroglue ) = gluerefcount ( zeroglue ) + localbase - gluebase;
  parshapeptr = 0 ; 
  eqtype ( parshapeloc ) = shaperef ; 
  eqlevel ( parshapeloc ) = levelone ; 
  for( k = outputroutineloc ; k <= toksbase + 255 ; k++ ) {
    eqtb [ k ] = eqtb [ undefinedcontrolsequence ] ; 
  }
  box ( 0 ) = 0 ; 
  eqtype ( boxbase ) = boxref ; 
  eqlevel ( boxbase ) = levelone ; 
  for( k = boxbase + 1 ; k <= boxbase + 255 ; k++ ) {
    eqtb [ k ] = eqtb [ boxbase ] ; 
  }
  curfont = nullfont ; 
  eqtype ( curfontloc ) = data ; 
  eqlevel ( curfontloc ) = levelone ; 
  {register integer for_end; k = mathfontbase ; for_end = mathfontbase + 47 
  ; if ( k <= for_end) do 
    eqtb [ k ] = eqtb [ curfontloc ] ; 
  while ( k++ < for_end ) ; } 
  equiv ( catcodebase ) = 0 ; 
  eqtype ( catcodebase ) = data ; 
  eqlevel ( catcodebase ) = levelone ; 
  {register integer for_end; k = catcodebase + 1 ; for_end = intbase - 1 
  ; if ( k <= for_end) do 
    eqtb [ k ] = eqtb [ catcodebase ] ; 
  while ( k++ < for_end ) ; } 
  for( k = 255 ; k >= 0 ; --k ) {
      catcode ( k ) = 12 ; 
      mathcode ( k ) = k ; 
      sfcode ( k ) = 1000 ; 
  }
  catcode ( 13 ) = 5 ; 
  catcode ( 32 ) = 10 ; 
  catcode ( 92 ) = 0 ; 
  catcode ( 37 ) = 14 ; 
  catcode ( 127 ) = 15 ; 
  catcode ( 0 ) = 9 ; 
  {register integer for_end; k = 48 ; for_end = 57 ; if ( k <= for_end) do 
    mathcode ( k ) = k + varcode ; 
  while ( k++ < for_end ) ; } 
  {register integer for_end; k = 65 ; for_end = 90 ; if ( k <= for_end) do 
    {
      catcode ( k ) = 11 ; 
      catcode ( k + 32 ) = 11 ; 
      mathcode ( k ) = k + varcode + 256 ; 
      mathcode ( k + 32 ) = k + 32 + varcode + 256 ; 
      lccode ( k ) = k + 32 ; 
      lccode ( k + 32 ) = k + 32 ; 
      uccode ( k ) = k ; 
      uccode ( k + 32 ) = k ; 
      sfcode ( k ) = 999 ; 
    } 
  while ( k++ < for_end ) ; } 
  {register integer for_end; k = intbase ; for_end = delcodebase - 1 ; if ( k 
  <= for_end) do 
    eqtb [ k ] .cint = 0 ; 
  while ( k++ < for_end ) ; } 
  mag = 1000 ; 
  tolerance = 10000 ; 
  hangafter = 1 ; 
  maxdeadcycles = 25 ;
#ifdef MLTEX
  char_sub_def_max = 0;
  tracing_char_sub_def = 0;
#endif
  escapechar = 92 ; 
  endlinechar = 13 ; 
#ifdef ERW_INTERACTION
  interaction = errorstopmode;
#endif
  for( k = 255 ; k >= 0 ; --k ) {
    delcode ( k ) = -1 ;
  }

  delcode ( 46 ) = 0 ; 
  {register integer for_end; k = dimenbase ; for_end = eqtbsize ; if ( k <= 
  for_end) do 
    eqtb [ k ] .cint = 0 ; 
  while ( k++ < for_end ) ; } 
  hashused = frozencontrolsequence ; 
  cscount = 0 ; 
  eqtype ( frozendontexpand ) = dontexpand ; 
  ztext ( frozendontexpand ) = 498 ; 
  fontptr = nullfont ; 
  fmemptr = 7 ; 
  fontname(nullfont) = STR_NULLFONT;
  fontarea(nullfont) = 335 ; 
  hyphenchar(nullfont) = 45 ; 
  skewchar(nullfont) = -1 ; 
  bcharlabel(nullfont) = nonaddress ; 
  fontbchar(nullfont) = nonchar ; 
  fontfalsebchar(nullfont) = nonchar;
  fontbc(nullfont) = 1 ; 
  fontec(nullfont) = 0 ; 
  fontsize(nullfont) = 0 ; 
  fontdsize(nullfont) = 0 ;
#ifdef FONTPTR
  charbase(nullfont) = &fontinfo[0] ; 
  widthbase(nullfont) = &fontinfo[0] ; 
  heightbase(nullfont) = &fontinfo[0] ; 
  depthbase(nullfont) = &fontinfo[0] ; 
  italicbase(nullfont) = &fontinfo[0] ; 
  ligkernbase(nullfont) = &fontinfo[0] ; 
  kernbase(nullfont) = &fontinfo[0] ; 
#else
  charbase(nullfont) = 0 ; 
  widthbase(nullfont) = 0 ; 
  heightbase(nullfont) = 0 ; 
  depthbase(nullfont) = 0 ; 
  italicbase(nullfont) = 0 ; 
  ligkernbase(nullfont) = 0 ; 
  kernbase(nullfont) = 0 ; 
#endif
  extenbase(nullfont) = 0 ; 
  fontglue(nullfont) = 0 ; 
  fontparams(nullfont) = 7 ; 
  parambase(nullfont) = -1 ; 
  for( k = 6 ; k >= 0 ; --k ) {
    fontinfo [ k ].cint = 0 ;
  }

  {register integer for_end; k = - (integer) trieopsize ; for_end = 
  trieopsize ; if ( k <= for_end) do 
    trieophash [ k ] = 0 ; 
  while ( k++ < for_end ) ; } 
  for( k = 255 ; k >= 0 ; --k ) {
    trieused [ k ] = mintrieop ;
  }
  maxopused = mintrieop ; 
  trieopptr = 0;
  trienotready = true;
  triel[0] = 0;
  triec[0] = 0;
  trieptr = 0;

  ztext ( frozenprotection ) = STR_INACCESSIBLE;
  formatident = STR_INITEX ;
  ztext ( endwrite ) = STR_ENDWRITE; 
  eqlevel ( endwrite ) = levelone ; 
  eqtype ( endwrite ) = outercall ; 
  equiv ( endwrite ) = 0 ; 
#endif /* INITEX */
}


#ifdef INITEX
#if defined(MLTEX) || defined(TEXXET) || defined(ERW_INTERACTION)
static void insert_c_string(char *s, strnumber n)
{
  if( poolptr + strlen(s) + stringvacancies > poolsize ) {
    wakeupterminal ();
    (void) printf("! You have to increase POOLSIZE.\n");
    exit(1);
  }

  while( *s != '\0' ) {
    appendchar( *s++ );   /* oder xord[*s], falls Umlaute, ... erlaubt */
  }

  if( n != makestring() ) {
    wakeupterminal ();
    (void) printf("! tex.pool doesn't match; wrong tex.pool file.\n");
    (void) printf("! number of strings in pool file is wrong.\n");
    exit(1);
  }
}
#endif


boolean getstringsstarted ( void )
{ getstringsstarted_regmem
  register signed short k, l;
  register short m, n;		/* ASCIIcode */
  register strnumber g;
  register integer a;
  alphafile poolfile;

  poolptr = 0;
  strptr = 0;
  strstart[0] = 0;

  /* 48. Make the first 256 strings */
  for( k = 0 ; k <= 255 ; k++ ) {
#ifdef ERW_CODEPAGE
    appendchar ( k );
    g = makestring ();
#else
#ifdef NONASCII
    if ( printable[k] )
#else
    if ( k < 32 || k > 126 )
#endif
    {
	appendchar ( 94 );  appendchar ( 94 );
	if ( k < 64 )
	  appendchar ( k + 64 );
	else if ( k < 128 )
	  appendchar ( k - 64 );
	else {
	  l = k / 16;  appendchar ( l + ( (l < 10) ? 48 : 87 ) );
	  l = k % 16;  appendchar ( l + ( (l < 10) ? 48 : 87 ) );
	}
    } else
	appendchar ( k );
    g = makestring ();
#endif
  }


  /* 51. Read the other strings from TEX.POOL file ... */
  vstrcpy ( nameoffile + 1 , poolname );
  nameoffile [ 0 ] = ' ';
  nameoffile [ strlen ( poolname ) + 1 ] = ' ';
  namelength = strlen(poolname);  /* added br */
  if ( aopenin(poolfile, TEXPOOLPATH) == false ) {
    wakeupterminal ();
    (void) printf("! I can't read tex.pool.\n");
    return(false);
  }

  while(true) {
    /* 52. Read one string, but return |false| ... */
    if( (m = getc(poolfile)) == EOF  ||  (n = getc(poolfile)) == EOF ) {
      wakeupterminal ();
      (void) printf("! tex.pool has no check sum.\n");
      aclose ( poolfile );
      return(false);
    }

    if ( m == '*' ) {	/* 53. Check the pool check sum */
      a = 0;  k = 9;
      do {
	if ( xord[n] < 48 || xord[n] > 57 ) {
	  wakeupterminal ();
	  (void) printf("! tex.pool check sum doesn't have nine digits.\n"); 
	  aclose ( poolfile );
	  return(false);
	}
	a = 10 * a + xord[n] - 48;
      } while( --k > 0  &&  (n = getc(poolfile)) != EOF );

      if ( a != 127541212L ) {
	wakeupterminal ();
	(void) printf("! tex.pool doesn't match; wrong tex.pool file.\n");
	aclose ( poolfile );
	return(false);
      }
      break;	/*  leave loop ---v  */

    } else {
      if ( xord[m] < 48 || xord[m] > 57 || xord[n] < 48 || xord[n] > 57 ) {
	wakeupterminal ();
	(void) printf("! tex.pool line doesn't begin with two digits.\n");
	aclose ( poolfile );
	return(false);
      }
      l = xord[m] * 10 + xord[n] - 48 * 11;	/* ... - "0" * 10 - "0" */
      if ( poolptr + l + stringvacancies > poolsize ) {
	wakeupterminal ();
	(void) printf("! You have to increase POOLSIZE.\n");
	aclose ( poolfile );
	return(false);
      }
      while( --l >= 0 ) {
	if( (m = getc(poolfile)) == EOF  ||  m == '\n' )    /* eoln() */
	  m = ' ';
	appendchar ( xord[m] );
      }
      while( (m = getc(poolfile)) != EOF  &&  m != '\n' )
	;			/* == readln ( poolfile ); */
      g = makestring();
    }
  }

  aclose ( poolfile );

#ifdef MLTEX
  /* Insert the new strings at the end by hand,
   * so we can use the old tex.pool file
   */
  insert_c_string("charsubdefmax", STR_CHARSUBDEFMAX);
  insert_c_string("tracingcharsubdef", STR_TRACING_CHARSUBDEF);
  insert_c_string("charsubdef", STR_CHARSUBDEF);
#endif

#ifdef ERW_INTERACTION
  insert_c_string("interactionmode", STR_INTERACTION_MODE);
#endif

#ifdef TEXXET
  insert_c_string("beginL", STR_BEGINL);
  insert_c_string("beginR", STR_BEGINR);
  insert_c_string("endL", STR_ENDL);
  insert_c_string("endR", STR_ENDR);
#endif

  return(true);
}
#endif /* INITEX */

#ifdef INITEX
void sortavail ( void )
{ sortavail_regmem 
  register halfword p, q, r ;
  halfword oldrover;

  /* p = */ (void) getnode ( 1073741824L );
  p = rlink ( rover );
  rlink ( rover ) = maxhalfword;
  oldrover = rover;
  while ( p != oldrover )
    if ( p < rover ) {
      q = p ; 
      p = rlink ( q ) ; 
      rlink ( q ) = rover ; 
      rover = q ; 
    } else {
      q = rover ; 
      while ( rlink ( q ) < p )
	q = rlink ( q ) ; 
      r = rlink ( p ) ; 
      rlink ( p ) = rlink ( q ) ; 
      rlink ( q ) = p ; 
      p = r ; 
    }
  p = rover ; 
  while ( rlink ( p ) != maxhalfword ) {
    llink ( rlink ( p ) ) = p ; 
    p = rlink ( p ) ; 
  } 
  rlink ( p ) = rover ; 
  llink ( rover ) = p ; 
} 
#endif /* INITEX */

#if 0

#ifdef INITEX
void zprimitive ( strnumber s, quarterword c, halfword o );
#endif /* INITEX */

#endif

#ifdef INITEX
trieopcode newtrieop ( smallnumber d, smallnumber n, trieopcode v )
{ newtrieop_regmem 
  register integer h ;
  trieopcode u ;
  register integer l ;

  h = abs ( toint ( n ) + 313 * toint ( d ) + 361 * toint ( v ) + 1009 * toint 
	( curlang ) ) % ( trieopsize + trieopsize ) - trieopsize ; 
  while ( true ) {
    l = trieophash [ h ];
    if ( l == 0 ) {
      if ( trieopptr == trieopsize ) 
        overflow(13, trieopsize);
      u = trieused [ curlang ] ; 
      if ( u == maxtrieop ) 
        overflow(14, maxtrieop - mintrieop);
      incr ( trieopptr ) ; 
      incr ( u ) ; 
      trieused [ curlang ] = u ; 
      if ( u > maxopused ) 
        maxopused = u ; 
      hyfdistance(trieopptr) = d;
      hyfnum(trieopptr) = n ; 
      hyfnext(trieopptr) = v ; 
      trieoplang [ trieopptr ] = curlang ; 
      trieophash [ h ] = trieopptr ; 
      trieopval [ trieopptr ] = u ; 
      return(u) ; 
    }
    if ( ( hyfdistance(l) == d ) && ( hyfnum(l) == n )
	&& ( hyfnext(l) == v ) && ( trieoplang [ l ] == curlang ) ) {
      return(trieopval [ l ]);
    }
    if ( h > - (integer) trieopsize ) 
      decr ( h ) ; 
    else h = trieopsize ; 
  }
}

triepointer trienode ( triepointer p )
{ trienode_regmem 
  register triepointer h ;
  register triepointer q ; 

#ifdef ORIGTRIE
  h = abs ( toint ( triec [ p ] ) + 1009 * toint ( trieo [ p ] ) + 2718 * 
	toint ( triel [ p ] ) + 3142 * toint ( trier [ p ] ) ) % triesize ; 
#else
  h = abs ( toint ( triec[p] ) + 1009 * toint ( ztrie[p].trieo ) +
	2718 * toint ( triel[p] ) + 3142 * toint ( trier[p] ) ) % triesize;
#endif
  while ( true ) {
    q = triehash [ h ] ; 
    if ( q == 0 ) {
      triehash [ h ] = p ; 
      return(p) ; 
    } 
    if ( ( triec [ q ] == triec [ p ] ) && (
#ifdef ORIGTRIE
					    trieo [ q ] == trieo [ p ]
#else
					ztrie[q].trieo == ztrie[p].trieo
#endif
									)
	&& ( triel [ q ] == triel [ p ] ) && ( trier [ q ] == trier [ p ] ) ) {
      return(q) ; 
    }
    if ( h > 0 )
      decr ( h ) ; 
    else
      h = triesize ; 
  }
}

triepointer compresstrie ( triepointer p )
{ compresstrie_regmem 

  if ( p == 0 ) 
    return( (triepointer)0L );
  else {
    triel [ p ] = compresstrie ( triel [ p ] ) ; 
    trier [ p ] = compresstrie ( trier [ p ] ) ; 
    return( trienode ( p ) );
  }
}

void firstfit ( triepointer p )
{ firstfit_regmem
  register triepointer q;
  register triepointer h;

 {triepointer z;
  ASCIIcode c;

  c = triec [ p ] ; 
  z = triemin [ c ] ; 
  while ( true ) {
    h = z - c ; 
    if ( triemax < h + 256 ) {
      if ( triesize <= h + 256 ) 
	overflow(15, triesize);
      do {
	incr ( triemax ) ;
#ifdef ORIGTRIE
	trietaken [ triemax ] = false ; 
#else
	ztrie[triemax].trietaken = false;
#endif
	trietrl [ triemax ] = triemax + 1 ; 
	trietro [ triemax ] = triemax - 1 ; 
      } while ( ! ( triemax == h + 256 ) ) ; 
    }
#ifdef ORIGTRIE
    if ( trietaken [ h ] ) 
	goto lab45 ; 
#else
    if ( ztrie[h].trietaken )
	goto lab45 ; 
#endif
    q = trier [ p ] ; 
    while ( q > 0 ) {
      if ( trietrl [ h + triec [ q ] ] == 0 ) 
      goto lab45 ; 
      q = trier [ q ] ; 
    } 
    goto lab40 ; 
lab45:
    z = trietrl [ z ] ; 
  }

lab40:
#ifdef ORIGTRIE
  trietaken [ h ] = true ; 
#else
  ztrie[h].trietaken = true;
#endif
  triehash [ p ] = h ; 
 }

  q = p;
  do {
    register triepointer l, r;
    triepointer z;

    z = h + triec [ q ] ; 
    l = trietro [ z ] ; 
    r = trietrl [ z ] ; 
    trietro [ r ] = l ; 
    trietrl [ l ] = r ; 
    trietrl [ z ] = 0 ; 
    if ( l < 256 ) {
      register short ll;

      if ( z < 256 )
        ll = z;
      else
	ll = 256;
      do {
	triemin [ l ] = r;
	incr ( l );
      } while( l != ll );
    } 
    q = trier [ q ];
  } while ( q != 0 );
}

void triepack ( triepointer p )
{ triepack_regmem 
  register triepointer q;

  do {
    q = triel [ p ] ; 
    if ( ( q > 0 ) && ( triehash [ q ] == 0 ) ) {
      firstfit ( q ) ; 
      triepack ( q ) ; 
    } 
    p = trier [ p ] ; 
  } while ( ! ( p == 0 ) ) ; 
}

void triefix ( triepointer p )
{ triefix_regmem 
  register triepointer q  ; 
  register ASCIIcode c  ; 
  register triepointer z  ; 

  z = triehash [ p ] ; 
  do {
    q = triel [ p ] ; 
    c = triec [ p ] ; 
    trietrl [ z + c ] = triehash [ q ] ; 
    trietrc [ z + c ] = c ; 
#ifdef ORIGTRIE
    trietro [ z + c ] = trieo [ p ] ; 
#else
    trietro [ z + c ] = ztrie[p].trieo;
#endif
    if ( q > 0 )
	triefix ( q ) ; 
    p = trier [ p ] ; 
  } while ( p != 0 );
} 

void newpatterns ( void )
{ newpatterns_regmem
  register short k, l;
  boolean digitsensed;
  trieopcode v;
  register triepointer p, q;
  boolean firstchild;
  register ASCIIcode c;

  if ( ! trienotready ) {
    print_err("Too late for ");
    printesc( STR_PATTERNS );
    zhelp1( STR_H_ALL_PATTERNS );
    error();
    link ( garbage ) = scantoks ( false , false ) ; 
    flushlist ( defref ) ; 
    return;
  }

  if ( language <= 0 ) 
    curlang = 0 ; 
  else if ( language > 255 ) 
    curlang = 0 ; 
  else curlang = language ; 

  scanleftbrace () ; 

  k = 0 ; 
  hyf [ 0 ] = 0 ; 
  digitsensed = false ; 

  while ( true ) {
    register /* eightbits */ long_halfword r_curcmd;

    r_curcmd = getxtoken () ; 
    switch ( r_curcmd ) {
    case letter : 
    case otherchar :
	{ register halfword r_curchr = curchr;

	if ( digitsensed || ( r_curchr < 48 ) || ( r_curchr > 57 ) ) {
	  if ( r_curchr == 46 )
	    r_curchr = 0 ; 
	  else {
	    r_curchr = lccode ( r_curchr ) ; 
	    if ( r_curchr == 0 ) {
	      curchr = r_curchr;
	      print_err("Nonletter");
	      zhelp1( STR_H_SEE_APP_H );
	      error(); 
	      r_curchr = curchr; 
	    } 
	  } 
	  if ( k < 63 ) {
	    incr ( k ) ; 
	    hc [ k ] = r_curchr ; 
	    hyf [ k ] = 0 ; 
	    digitsensed = false ; 
	  } 
	} else if ( k < 63 ) {
	  hyf [ k ] = r_curchr - 48 ; 
	  digitsensed = true ; 
	}
	}
	break ; 
    case spacer : 
    case right_brace : 
	{
	  if ( k > 0 ) {
	    if ( hc [ 1 ] == 0 ) 
	      hyf [ 0 ] = 0 ; 
	    if ( hc [ k ] == 0 ) 
	      hyf [ k ] = 0 ; 
	    l = k ; 
	    v = mintrieop ; 
	    while ( true ) {
	      if ( hyf [ l ] != 0 ) 
		v = newtrieop ( k - l , hyf [ l ] , v ) ; 
	      if ( l > 0 ) 
		decr ( l ) ; 
	      else goto lab31 ; 
	    }
lab31: ; 
	    q = 0 ; 
	    hc [ 0 ] = curlang ; 
	    while ( l <= k ) {
	      c = hc [ l ] ; 
	      incr ( l ) ; 
	      p = triel [ q ] ; 
	      firstchild = true ; 
	      while ( ( p > 0 ) && ( c > triec [ p ] ) ) {
		q = p ; 
		p = trier [ q ] ; 
		firstchild = false ; 
	      } 
	      if ( ( p == 0 ) || ( c < triec [ p ] ) ) {
		if ( trieptr == triesize ) 
		  overflow(15, triesize);
		incr ( trieptr ) ; 
		trier [ trieptr ] = p ; 
		p = trieptr ; 
		triel [ p ] = 0 ; 
		if ( firstchild ) 
		  triel [ q ] = p ; 
		else trier [ q ] = p ; 
		  triec [ p ] = c ; 
#ifdef ORIGTRIE
		trieo [ p ] = mintrieop ; 
#else
		ztrie[p].trieo = mintrieop;
#endif
	      } 
	      q = p ; 
	    } 
	    if (
#ifdef ORIGTRIE
		trieo [ q ] != mintrieop
#else
		ztrie[q].trieo != mintrieop
#endif
						) {
	      print_err("Duplicate pattern");
	      zhelp1( STR_H_SEE_APP_H );
	      error();
	    } 
#ifdef ORIGTRIE
	    trieo [ q ] = v ; 
#else
	    ztrie[q].trieo = v;
#endif
	  } 
	  if ( curcmd == right_brace )
	    goto lab30;

	  k = 0 ; 
	  hyf [ 0 ] = 0 ; 
	  digitsensed = false ; 
	} 
	break ; 
    default: 
	print_err("Bad ");
	printesc( STR_PATTERNS );
	zhelp1( STR_H_SEE_APP_H );
	error();
	break;
      }
  }		/* while ( true ) */
lab30: ; 
}

void inittrie ( void )
{ inittrie_regmem 

 {register integer j;

  opstart [ 0 ] = - (integer) mintrieop ; 
  for( j = 1 ; j <= 255 ; j++ ) {
    opstart [ j ] = opstart [ j - 1 ] + trieused [ j - 1 ] ; 
  }
  {register integer for_end; j = 1 ; for_end = trieopptr ; if ( j <= for_end) 
  do 
    trieophash [ j ] = opstart [ trieoplang [ j ] ] + trieopval [ j ] ; 
  while ( j++ < for_end ) ; } 

  for( j = 1 ; j <= trieopptr ; j++ ) {
    while ( trieophash [ j ] > j ) {
      register integer k, t;

      k = trieophash [ j ] ; 
      t = hyfdistance(k); hyfdistance(k) = hyfdistance(j); hyfdistance(j) = t;
      t = hyfnum(k); hyfnum(k) = hyfnum(j); hyfnum(j) = t;
      t = hyfnext(k); hyfnext(k) = hyfnext(j); hyfnext(j) = t;
      trieophash [ j ] = trieophash [ k ] ; 
      trieophash [ k ] = k ; 
    }
  }
 }

 {register triepointer p;

  {register integer for_end; p = 0 ; for_end = triesize ; if ( p <= for_end) 
  do 
    triehash [ p ] = 0 ; 
  while ( p++ < for_end ) ; } 

  triel [ 0 ] = compresstrie ( triel [ 0 ] ) ; 

  {register integer for_end; p = 0 ; for_end = trieptr ; if ( p <= for_end) 
  do 
    triehash [ p ] = 0 ; 
  while ( p++ < for_end ) ; } 

  for( p = 0 ; p <= 255 ; p++ ) {
    triemin [ p ] = p + 1 ; 
  }
 }

  trietrl [ 0 ] = 1 ; 
  triemax = 0 ; 
  if ( triel [ 0 ] != 0 ) {
    firstfit ( triel [ 0 ] ) ; 
    triepack ( triel [ 0 ] ) ; 
  } 
  if ( triel [ 0 ] == 0 ) {
    register triepointer r;

    for( r = 0 ; r <= 256 ; r++ ) {
	trietrc [ r ] = 0 ; 
	trietrl [ r ] = 0 ; 
	trietro [ r ] = mintrieop ; 
    }
    triemax = 256 ; 
  } else {
    triefix ( triel [ 0 ] ) ; 
    { register triepointer r = 0;

      do {
	register triepointer s;

	s = trietrl [ r ] ; 
	trietrl [ r ] = 0 ; 
	trietro [ r ] = mintrieop ; 
	trietrc [ r ] = 0 ; 
	r = s ; 
      } while ( r <= triemax );
    }
  }
  trietrc [ 0 ] = 63 ; 
  trienotready = false ; 
}
#endif /* INITEX */


	/*
	 * prefixedcommand() wurde nach tex8.c verlegt und nur der
	 * Teil, der in initex/virtex anders ist, hier als separate
	 * Funktion realisiert.
	 * returnvalue: initex=0, virtex!=0
	 */
int call_new_patterns(void)
{
#ifdef INITEX
  newpatterns();
  return(0);
#else /* ! INITEX */
  call_new_patterns_regmem

  print_err("Patterns can be loaded only by INITEX");
  help0;
  error();
  { register eightbits r_curcmd;

    do {
      r_curcmd = gettoken();
    } while( r_curcmd != right_brace );
  }
  return(1);
#endif
}


#if 0

#ifdef INITEX
void storefmtfile ( ) 
#endif /* INITEX */

boolean loadfmtfile ( ) 

#endif


void finalcleanup ( void )
{/* 10 */ finalcleanup_regmem 
  register smallnumber c  ; 

  c = curchr ; 
  if ( jobname == 0 ) 
    openlogfile () ; 
  while ( openparens > 0 ) {
    c_print(" )");
    decr ( openparens ) ; 
  } 
  if ( curlevel > levelone ) {
    printnl ( 40 ) ; 
    c_printesc("end occurred ");
    c_print("inside a group at level ");
    printint ( curlevel - levelone ) ; 
    printchar ( 41 ) ; 
  } 
  while ( condptr != 0 ) {
    printnl ( 40 ) ; 
    c_printesc("end occurred ");
    c_print("when ");
    printcmdchr ( iftest , curif ) ; 
    if ( ifline != 0 ) {
      c_print(" on line ");
      printint ( ifline );
    } 
    c_print(" was incomplete)");
    ifline = iflinefield ( condptr ) ; 
    curif = subtype ( condptr ) ; 
    condptr = link ( condptr ) ; 
  } 
  if ( history != spotless ) 
  if ( ( ( history == warningissued ) || ( interaction < errorstopmode ) ) ) 
  if ( selector == termandlog ) 
  {
    selector = termonly ; 
    c_printnl("(see the transcript file for additional information)");
    selector = termandlog ; 
  } 
  if ( c == 1 ) {
#ifdef INITEX
    storefmtfile () ; 
#else  /* ! INITEX */
    c_printnl("(\\dump is performed only by INITEX)");
#endif /* ! INITEX */
    return ; 
  } 
} 

#if 0

#ifdef INITEX
void initprim ( ) 
#endif /* INITEX */

#endif


   static boolean
check_consistency(void)
{ char bad = 0;
  char *extra_info = NULL;

  if ( halferrorline < 30 || halferrorline > errorline - 15 ) {
    bad = 1;
    extra_info = "`halferrorline' should be between 30 and (errorline-15)";
  }
  if ( maxprintline < 60 ) {
    bad = 2;
    extra_info = "`maxprintline' should be at least 60";
  }
  if ( (dvibufsize < 32) && (dvibufsize % 8 != 0) ) {
    bad = 3;
    extra_info = "`dvibufsize' must be >= 32 and a multiple of 8";
  }
  if ( membot + 1100 > memtop ) {
    bad = 4;
    extra_info = "`memtop' should be at least 1100";
  }
  if ( hashprime > hashsize )
    bad = 5;
  if ( maxinopen >= 128 ) {
    bad = 6;
    extra_info = "`maxinopen' must not exceed 127";
  }
  if ( memtop < 267 )
    bad = 7;

#ifdef INITEX
  if ( memmin != membot || memmax != memtop ) {
    bad = 10;
    extra_info = "`memmax' = `memtop' for IniTeX";
  }
#else
  if ( memmin > membot || memmax < memtop ) {
    bad = 10;
    extra_info = "`memmax' >= `memtop'";
  }
#endif /* INITEX */
  if ( 0 > 0 || maxquarterword < 127 )
    bad = 11;
  if ( 0 > 0 || maxhalfword < 32767 )
    bad = 12;
  if ( 0 < 0 || maxquarterword > maxhalfword )
    bad = 13;
  if ( memmin < 0 || memmax >= maxhalfword || (membot-memmin > maxhalfword+1)) {
    bad = 14;
#ifdef BIG
    /* fuer 524286 memwords benoetigt man 0.5M * 8 bytes = 4MB Speicher */
#ifdef INITEX
    extra_info = "`memtop' must not exceed 524286";
#else
    extra_info = "`memmax' must not exceed 524286";
#endif
#else
#ifdef INITEX
    extra_info = "`memtop' must not exceed 65534";
#else
    extra_info = "`memmax' must not exceed 65534";
#endif
#endif
  }
  if ( 0 < 0 || fontmax > maxquarterword ) {
    bad = 15;
    extra_info = "`fontmax' must not exceed 255";
  }
  if ( fontmax > 256 )
    bad = 16;
  if ( savesize > maxhalfword || maxstrings > maxhalfword ) {
    bad = 17;
    extra_info = "`savesize' and `maxstrings' must not exceed 65535";
  }
  if ( bufsize > maxhalfword ) {
    bad = 18;
    extra_info = "`bufsize' must not exceed 65535";
  }
  if ( maxquarterword - 0 < 255 ) 
    bad = 19;
  if ( cstokenflag + undefinedcontrolsequence > maxhalfword ) 
    bad = 21;
  if ( formatdefaultlength > FILENAMESIZE ) 
    bad = 31;
  if ( 2 * maxhalfword < memtop - memmin ) 
    bad = 41;
  if ( bad > 0 ) {
    (void) printf("\
Ouch---my internal constants have been clobbered!---case %ld\n" , (long)bad );
    if( extra_info != NULL )
      (void) printf("Check: %s!\n", extra_info);
    return(false);
  }
  return(true);
}


void texbody ( void )
{ texbody_regmem 

  history = fatalerrorstop ; 
  setpaths ( TEXFORMATPATHBIT+TEXINPUTPATHBIT+TEXPOOLPATHBIT+TFMFILEPATHBIT );
  if ( readyalready == 314159L ) 
    goto lab1 ; 

  if( ! check_consistency() )
    goto lab9999;

  initialize () ;
#ifdef INITEX
  if ( ! getstringsstarted () ) 
    goto lab9999 ; 
  initprim () ; 
  initstrptr = strptr ; 
  initpoolptr = poolptr ; 
  dateandtime ( ztime , zday , zmonth , zyear ) ; 
#endif /* INITEX */
  readyalready = 314159L ; 

lab1:
  selector = termonly ; 
  tally = 0 ; 
  termoffset = 0 ; 
  fileoffset = 0 ; 
#if 0
  (void) Fputs( stdout ,  "This is TeX, C Version 3.14t2" ) ; 
#else
  (void) Fputs( stdout ,  banner );
#endif

#ifdef INITEX
#if defined(MLTEX) && defined(TEXXET)
  if( is_ML_TeX && is_TeX_XeT)
    (void) Fputs(stdout, "\nThis is TeX--XeT (1.0) with ML-TeX (V.3)");
  else if( is_ML_TeX )
    (void) Fputs(stdout, "\nThis is ML-TeX, Version 3");
  else if( is_TeX_XeT )
    (void) Fputs(stdout, "\nThis is TeX--XeT (1.0)");
#else
# ifdef MLTEX
  /* if user has given ML-TeX switch in IniTeX, output a second line */
  if( is_ML_TeX )
    (void) Fputs(stdout, "\nThis is ML-TeX, Version 3");
# endif
# ifdef TEXXET
  /* if user has given TeX-XeT switch in IniTeX, output a second line */
  if( is_TeX_XeT )
    (void) Fputs(stdout, "\nThis is TeX--XeT (1.0)");
# endif
#endif
#endif

  if ( formatident > 0 ) 
#if 0  /* TeX 3.141 */
    print( formatident );
#else
    slowprint( formatident );
#endif
  println ();
  flush ( stdout );
  jobname = 0;
  nameinprogress = false;
  logopened = false;
  outputfilename = 0;

  inputptr = 0;
#ifdef INP_PTR
  curinput_ptr = &inputstack[ /* inputptr */ 0 ];
#endif
  maxinstack = 0;
  inopen = 0 ; 
  openparens = 0 ; 
  maxbufstack = 0 ; 
  paramptr = 0 ; 
  maxparamstack = 0 ; 
  first = bufsize ; 
  do {
    buffer [ first ] = 0 ; 
    decr ( first ) ; 
  } while ( first != 0 ) ; 
  scannerstatus = normal ; 
  warningindex = 0 ; 
  first = 1 ; 
  curinput .statefield = 33 ; 
  curinput .startfield = 1 ; 
  curinput .indexfield = 0 ; 
  line = 0 ; 
  curinput .namefield = 0 ; 
  forceeof = false ; 
  alignstate = 1000000L ; 
  if ( ! initterminal () ) 
    goto lab9999 ; 
  curinput .limitfield = last ; 
  first = last + 1 ; 

  if ( ( formatident == 0 ) || ( buffer [ curinput .locfield ] == 38 ) ) {
    if ( formatident != 0 ) 
	initialize () ; 
    if ( ! openfmtfile () ) 
	goto lab9999 ; 
    if ( ! loadfmtfile () ) {
	wclose ( fmtfile ) ; 
	goto lab9999 ; 
    } 
    wclose ( fmtfile ) ; 
    while ( ( curinput .locfield < curinput .limitfield ) && ( buffer [ 
      curinput .locfield ] == 32 ) )
	incr ( curinput .locfield ) ; 
  }
  if ( endlinecharinactive () ) 
    decr ( curinput .limitfield ) ; 
  else
    buffer [ curinput .limitfield ] = endlinechar ; 
  dateandtime ( ztime , zday , zmonth , zyear ) ; 
#if 0	/* (br) nicht mehr benoetigt */
  magicoffset = strstart [ 885 ] - 9 * ordnoad ; 
#endif

  /* (br) added: Has User set a \language ? */
  if( user_language >= 0 )
	geqworddefine( intbase+languagecode, user_language );

  /* (br) added: Has User set another Interaction Level ? */
  if( user_interaction != -1 )
	interaction = user_interaction;

  if ( interaction <= batchmode )
    selector = noprint ; 
  else selector = termonly ; 
  if ( ( curinput .locfield < curinput .limitfield )
    && ( catcode ( buffer [ curinput .locfield ] ) != 0 ) ) 
	startinput();

  history = spotless ; 
  maincontrol () ; 
  finalcleanup () ; 
  closefilesandterminate () ; 

lab9999:
  {
    flush ( stdout ) ; 
    readyalready = 0 ; 
    if ( ( history != spotless ) && ( history != warningissued ) ) 
      uexit ( 1 ) ; 
    else uexit ( 0 ) ; 
  } 
}

/* -- end -- */
