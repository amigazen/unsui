#define EXTERN extern
#include "texd.h"

/* Zeige benoetigte Zeit (Debug only) */
#undef TIME


#if defined(TIME) && defined(AMIGA)
# include <time.h>
#endif

/* Nur in Vielfachen von 4 dumpen...
 * dies trifft hauptsaechlich fuer den String Pool zu.
 * ==> Lesen der nachfolgenden Arrays aus dem fmt-file muss nicht von
 *     ungeraden Adressen auf ungerade Adressen kopiert werden.
 * ==> Lesen wird (hoffentlich) schneller.
 */
#define ROUND_FOUR(x)	(((x) + 3) & ~3L)


/* Stringlaengenkomprimierung ?? (War in alter Version fehlerhaft!) */
#define NEW_STRING


/* CheckSum fuer fmt-file: (original @$ war: 127541235) */
/* oberstes Wort besteht aus festem Teil: */
#define FMT_CHECK	0x62720000L		/* "br" */

/* unterstes Wort ist eigentliches Magic Word: */
#define CHECK_MASK	0xffffL

#define BIGTEX_FLAG	0x0080L

/* Release  16.Dezember 90 */
/* besteht aus Datum: 1990 = 0, dez = c, 16. = 10 plus BIGTEX_FLAG */
#if 0
#define BIGFMT		0x0c90L
#define NORMALFMT	0x0c10L
#endif

/* Release  23.Juni 91:  (Werte sollen groesser sein als vorherige Version) */
#if 0
#define BIGFMT		0x1690L
#define NORMALFMT	0x1610L
#endif

/* Release  11. Oktober 93: */
#define BIGFMT		0x3a8bL
#define NORMALFMT	0x3a0bL


/* For additional features, we mark the next long with a '+' and
 * set 2 bit for each feature in the next 3 bytes (= 12 possible features ;-)
 */
/* For ML-TeX, we mark the second long in an fmt-file (normally membot-value)
 * This is necessary, because eqtb has additional 256 + 3 mem_words.
 * In the near future the dumping/undumping of eqtb should be modified to
 * cover these changes.
 */
#define ADDITIONAL	0x2b000000L	/* '+\0\0\0' */
#define MLTEXCHECK	0x0001L		/* features enabled */
#define ADD_MLTEX	0x800000L	/* compiled with this feature? */
#define TEXXETCHECK	0x0002L
#define ADD_TEXXET	0x400000L

/* Beim Dumpen des `eqtb'-Arrays wird zwar versucht moeglichst platzsparend
 * zu sein, indem jeweils ungleiche und gleiche Bereiche mit 2 Laengenangaben
 * (n,m) gedumpt werden.  Jedoch wird sehr oft ein sehr kleiner Bereich (mit
 * Laenge n=2 und kleinem m) gedumpt und die Platzersparnis wird durch die
 * beiden Laengen wieder zunichte gemacht.
 * Deshalb erst Dumpen, wenn `m' > EQTB_THRESHOLD.
 * Am Undumpen aendert sich dabei nichts.
 */
#define EQTB_THRESHOLD	1


/* Meine eigenen Routinen zum Dumpen und Undumpen. */
/* Bei den kleinen Datentypen (char, short) sollte man beim Dumpen
 * noch nachpruefen, ob sie ueberhaupt im Bereich liegen !
 * (Sie sollten es eigentlich, aber besser ist besser)
 * !!!! hash[] sollte genauso gross sein wie ein int !!!!
 */

#undef dumpint
#undef undumpint

#define dumpchar(x)	(void) putc((char) (x), dump_file);
#define undumpchar(x)	(x) = getc(dump_file);

#define dumpshort(x)	my_putw((short) (x));
#define undumpshort(x)	(x) = my_getw()

#define dumpint(x)	 my_putl((long) (x))
#define undumpint(x)	(x) = my_getl()

#undef dumphh
#undef undumphh
#define dumphh(x)	 my_putl( *((long *) &(x)) )
#define undumphh(x)	*((long *) &(x)) = my_getl()


#ifdef INITEX
  static void
my_putw(short c)
{
  union {		/* sizeof(short) == 2 !! */
	char part[2];
	short n;
  } x;

  x.n = c;

  (void) fputc(x.part[0], dump_file);
  (void) fputc(x.part[1], dump_file);
}

  static void
my_putl(long c)
{
  union {		/* sizeof(long) == 4 !! */
	char part[4];
	long n;
  } x;

  x.n = c;

  (void) fputc(x.part[0], dump_file);
  (void) fputc(x.part[1], dump_file);
  (void) fputc(x.part[2], dump_file);
  (void) fputc(x.part[3], dump_file);
}
#endif


  static short
my_getw(void)
{
  register FILE *fp = dump_file;
  short c;
  union {
	char part[2];
	short n;
  } x;

  if((c = getc(fp)) == EOF)
	return(EOF);
  x.part[0] = c;	/* n = (c << 8); */
  if((c = getc(fp)) == EOF)
	return(EOF);
  x.part[1] = c;	/*  n |= (c & 0xFF);  n <<= 16; */

  return x.n;
}


  static long
my_getl(void)
{
  register FILE *fp = dump_file;
  short c;
  union {
	char part[4];
	long n;
  } x;

  if((c = getc(fp)) == EOF)
	return(EOF);
  x.part[0] = c;  /* n = (c << 8); */
  if((c = getc(fp)) == EOF)
	return(EOF);
  x.part[1] = c;  /*  n |= (c & 0xFF);  n <<= 16; */

  if((c = getc(fp)) == EOF)
	return(EOF);
  x.part[2] = c;  /*  n |= ((c & 0xFF) << 8); */
  if((c = getc(fp)) == EOF)
	return(EOF);
  x.part[3] = c;  /*  n |= (c & 0xFF); */

  return x.n;
}



#ifdef INITEX
void storefmtfile ( void )
{/* 41 42 31 32 */ storefmtfile_regmem 
  register integer j, k, l;
  register halfword p, q;
  register integer x;

  /* (br) Dumpe moeglichst viel auf einmal, deshalb ... */
  long morethings[20];
  long hash_low_count;

  if ( saveptr != 0 ) {
    print_err("You can't dump inside a group");
    zhelp1( STR_H_DUMPISANONO );
    succumb ();
  }
  selector = newstring;
  c_print(" (format=");  print( jobname );  printchar( 32 );
  printint( zyear % 100 );  printchar( 46 );  printint( zmonth );
  printchar( 46 );  printint( zday );  printchar( 41 );
  if ( interaction <= batchmode )
    selector = logonly;
  else
    selector = termandlog;
  strroom(1);
  formatident = makestring();
  packjobname( STR_DOT_FMT );
  while ( ! wopenout(fmtfile) )
    promptfilename("format file name", STR_DOT_FMT);
  c_printnl("Beginning to dump on file ");
#if 0  /* TeX 3.141 */
  print ( wmakenamestring ( fmtfile ) );
  flushstring ;
  printnl ( formatident );
#else
  slowprint ( wmakenamestring ( fmtfile ) );
  flushstring ;
  c_printnl ("");  slowprint ( formatident );
#endif

  /* Dump the dynamic memory 1311 (I) */
  /* Call sort_avail() before the dump, because it changes rover,... */
  sortavail () ; 

  /* Dump constants for consistency checking 1307 */

  /* (br) ... Irgendwas magisches ;-) als erstes Wort */
#ifdef BIG
  morethings[0] = (FMT_CHECK | NORMALFMT | BIGTEX_FLAG);
#else
  morethings[0] = (FMT_CHECK | NORMALFMT);
#endif

#if defined(MLTEX) && defined(TEXXET)
  /* Mark this fmt-file only for ML-TeX & TEX--XET*/
  morethings[1] = ADDITIONAL | ADD_MLTEX | ADD_TEXXET;
  morethings[1] |= (( is_ML_TeX ) ? MLTEXCHECK : 0L);
  morethings[1] |= (( is_TeX_XeT ) ? TEXXETCHECK : 0L);
#else
# ifdef MLTEX
  morethings[1] = ADDITIONAL | ADD_MLTEX;
  morethings[1] |= (( is_ML_TeX ) ? MLTEXCHECK : 0L);
# endif
# ifdef TEXXET
  morethings[1] = ADDITIONAL | ADD_TEXXET;
  morethings[1] |= (( is_TeX_XeT ) ? TEXXETCHECK : 0L);
# endif
# if !defined(MLTEX) && !defined(TEXXET)
  morethings[1] = membot;	/* membot == 0,  forever... ??? */
# endif
#endif
  morethings[2] = memtop;
  morethings[3] = eqtbsize;		/* ist von ML-TEX abhaengig */
  morethings[4] = hashprime;
  morethings[5] = hyphsize;

  /* Dump the string pool 1309 (I) */
  morethings[6] = poolptr;
  morethings[7] = strptr;

  /* Dump the dynamic memory 1311 (II) */
  morethings[8] = lomemmax;
  morethings[9] = rover;
  morethings[10] = himemmin;
  morethings[11] = avail;

  /* Dump the table of equivalents 1313 (I) */
  morethings[12] = parloc;
  morethings[13] = writeloc;

  /* Dump the hash table 1318 (I) */
  morethings[14] = hashused;
  cscount = frozencontrolsequence - 1 - hashused ; 
  hash_low_count = 0;
  for( p = hashbase ; p <= hashused ; p++ ) {
    if ( ztext ( p ) != 0 ) {
	incr(hash_low_count);
	incr ( cscount ) ; 
    }
  }
  morethings[15] = cscount;
  morethings[16] = hash_low_count;	/* merken, wie voll Hashtabelle */
#ifndef NEW_STRING
  dumpthings(morethings[0], 17);

  /* Dump the string pool 1309 (II) */
  dumpthings( strstart[0], strptr + 1 );
#else
  /* Typically the most strings have length < 255;
   * and strstart[i] <= strstart[j] for all i,j with i < j
   * => dump only strstart[] differences.
   */
  { unsigned char *buf;
    unsigned char *src;

    k = strstart[0];
    buf = (unsigned char *)&strstart[1];
    for(j = 1 ; j < strptr+1 ; j++) {
	l = strstart[j] - k;	/* save only differences */
	if( l >= 255 ) {
	  *buf++ = 255;		/* flag: strlen >= 255 */
	  *buf++ = l / 256;
	  *buf++ = l % 256;
	} else {
	  *buf++ = (unsigned char) l;
	}
	k = strstart[j];
    }


    k = buf - (unsigned char *)&strstart[0];
    /* `k' aufrunden auf Vielfaches von 4 */
    j = (4 - (k % 4)) % 4;
    while ( j > 0 ) {
	*buf++ = 0;	/* Mit Nullen auffuellen */
	k++;		/* k entsprechend anpassen */
	j--;
    }
    buf =  (unsigned char *)&strstart[0];

    morethings[17] = k;
    dumpthings(morethings[0], 18);

    dumpthings(buf[0], k);

    /* Restore the strstart[] array, it's needed.
     * A better solution: Dump strings at end.
     */
    src = &buf[(maxstrings * sizeof(poolpointer)) - k];
    memcpy(src, &strstart[1], k);

    k = strstart[0];
    for(j = 1 ; j < strptr + 1 ; j++ ) {
	l = (integer) *src++;
	if( l == 255 ) {
	  l = (integer) ((unsigned)((*src++) << 8));
	  l += ((unsigned)*src++);
	}
	k += l;
	strstart[j] = k;
    }

    if( strstart[strptr] != poolptr )
	printf("!! Internal Error (String Pool Inconsistent [%d]) !!\n", 1);
  }
#endif
  /* Nur in Vielfachen von 4 dumpen */
  dumpthings( strpool[0], ROUND_FOUR(poolptr) );

  dumpthings(xord[0], 256);
  dumpthings(xchr[0], 256);
#ifdef ERW_CODEPAGE
  dumpthings(printable[0], 256);
#endif

  println();
  printint( strptr );
  c_print(" strings of total length ");
  printint( poolptr );

  /* Dump the dynamic memory 1311 (III) */
  varused = 0 ; 

  p = membot;
  q = rover ; 
  x = 0 ; 
  do {
#ifdef TIME
printf("dump dynmem: len=%d, next_gap=%d\n", q+2-p, mem[q].hh.v.LH );
#endif
    dumpthings ( mem[p], q + 2 - p );
    x = x + q + 2 - p;
    varused = varused + q - p;
    p = q + nodesize ( q );
    q = rlink ( q );
  } while ( ! ( q == rover ) );
  varused = varused + lomemmax - p;
  dynused = memend + 1 - himemmin;
#ifdef TIME
printf("dump dynmem: len=%d, last\n", lomemmax+1-p);
#endif
  dumpthings ( mem[p], lomemmax + 1 - p );
  x = x + lomemmax + 1 - p;

  dumpthings ( mem[himemmin], memend + 1 - himemmin );
  x = x + memend + 1 - himemmin;
  p = avail;
  while ( p != 0 ) {
    decr ( dynused );
    p = link ( p );
  }

  morethings[0] = varused;
  morethings[1] = dynused;

  println();
  printint( x );
  c_print(" memory locations dumped; current usage is ");
  printint( varused );  printchar( 38 );  printint( dynused );

  /* Dump the font information 1320 (I) */
  morethings[2] = fmemptr;
  morethings[3] = fontptr;

  /* Dump the hyphenation table 1324 (I) */
  morethings[4] = hyphcount;

  dumpthings(morethings[0], 5);


/* Fuer ML-TeX ohne Benutzung von \charsubdef sollte man Bereich von
 * char_sub_base ... char_sub_base + 256  aussparen
 */

  /* Dump the table of equivalents 1313,1315,1316,1318 (II) */
  k = activebase ;
  do {
    j = k;

too_short:
    while( j < intbase - 1 ) {
      if( (equiv(j) == equiv(j+1)) && (eqtype(j) == eqtype(j+1))
	 && (eqlevel(j) == eqlevel(j+1)) )
	goto lab41;
      incr ( j );
    } 
    l = intbase;
    goto lab31;

lab41:
    incr ( j );
    l = j;
    while( j < intbase - 1 ) {
      if ( (equiv(j) != equiv(j+1)) || (eqtype(j) != eqtype(j+1))
	 || (eqlevel(j) != eqlevel(j+1)) )
	goto lab31;
      incr ( j );
    }
lab31:
    /* Falls wir nicht am Ende sind, und m nicht gross genug, nochmal */
    if( l < intbase  &&  (j+1-l) <= EQTB_THRESHOLD ) {
#ifdef TIME
printf("dump eqtb1-4: len=%d, gap_len=%d  not\n", l-k, j+1-l);
#endif
	goto too_short;
    }
    dumpshort ( l - k );
#ifdef TIME
printf("dump eqtb1-4: len=%d, eq_len=%d\n", l-k, j+1-l);
#endif
    dumpthings ( eqtb[k], l - k );
    k = j + 1 ; 
    dumpshort ( k - l );
  } while ( k != intbase );

  do {
    j = k;
too_short2:
    while ( j < eqtbsize ) {
      if( eqtb[j].cint == eqtb[j+1].cint ) 
	goto lab42;
      incr ( j );
    } 
    l = eqtbsize + 1 ;
    goto lab32;

lab42:
    incr ( j );
    l = j;
    while( j < eqtbsize ) {
      if( eqtb[j].cint != eqtb[j+1].cint )
	goto lab32;
      incr ( j );
    }
lab32:
    /* Falls wir nicht am Ende sind, und m nicht gross genug, nochmal */
    if( l < eqtbsize + 1  &&  (j+1-l) <= EQTB_THRESHOLD ) {
#ifdef TIME
printf("dump eqtb5-6: len=%d, gap_len=%d  not\n", l-k, j+1-l);
#endif
	goto too_short2;
    }
    dumpshort ( l - k );
#ifdef TIME
printf("dump eqtb5-6: len=%d, eq_len=%d\n", l-k, j+1-l);
#endif
    dumpthings ( eqtb[k], l - k );
    k = j + 1;
    dumpshort ( k - l );
  } while ( k <= eqtbsize );

  /* Dump the hash table 1318 (II) */
  { long anzahl = 0;
    for( p = hashbase ; p <= hashused ; p++ ) {
      if ( ztext ( p ) != 0 ) {
	incr(anzahl);
        dumpshort ( p );
      }
    }
    if( hash_low_count != anzahl )
	printf("!! Internal error (Hash Table Inconsistent) !!\n");
    anzahl = 0;
    for( p = hashbase ;  p <= hashused ; p++ ) {
      if ( ztext ( p ) != 0 ) {
	incr(anzahl);
#ifdef BIG
	(void) fwrite( (char *)&hash[p], sizeof(hash[p]), 1, dump_file );
#else
        dumphh ( hash[p] );
#endif
      }
    }
    if( hash_low_count != anzahl )
	printf("!! Internal error (Hash Table Inconsistent) !!\n");
  }
			/* v-- LC BUG */
  dumpthings ( hash[(hashused + 1)], undefinedcontrolsequence - 1 - hashused );
  println();
  printint( cscount );
  c_print(" multiletter control sequences");

#ifdef FONTPTR
  /* ...base[] are C pointers in fontinfo[]; dump only "difference" */
  for( p = nullfont ; p <= fontptr ; p++ ) {
	charbase(p)    = (SMALLmemoryword *) (charbase(p)   - &fontinfo[0]);
	widthbase(p)   = (SMALLmemoryword *) (widthbase(p)  - &fontinfo[0]);
	heightbase(p)  = (SMALLmemoryword *) (heightbase(p) - &fontinfo[0]);
	depthbase(p)   = (SMALLmemoryword *) (depthbase(p)  - &fontinfo[0]);
	italicbase(p)  = (SMALLmemoryword *) (italicbase(p) - &fontinfo[0]);
	ligkernbase(p) = (SMALLmemoryword *) (ligkernbase(p) - &fontinfo[0]);
	kernbase(p)    = (SMALLmemoryword *) (kernbase(p)   - &fontinfo[0]);
  }
#endif

  /* Dump the font information 1320,1322 (II) */
  {
    dumpthings ( fontinfo [ nullfont ] , fmemptr ) ; 
    dumpthings ( fontsize(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( fontdsize(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( fontname(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( fontarea(nullfont), fontptr + 1 - nullfont ) ;

#ifdef FONTSTRUCT
    dumpthings(fontdesc[nullfont], fontptr + 1 - nullfont);
#else
    dumpthings ( fontcheck(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( fontparams(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( hyphenchar(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( skewchar(nullfont), fontptr + 1 - nullfont ) ; 
#ifdef FONTPTR
    dumpthings ( font_bcec[nullfont], fontptr + 1 - nullfont );
#else
    dumpthings ( fontbc[nullfont], fontptr + 1 - nullfont );
    dumpthings ( fontec[nullfont], fontptr + 1 - nullfont ); 
#endif
    dumpthings ( charbase(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( widthbase(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( heightbase(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( depthbase(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( italicbase(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( ligkernbase(nullfont), fontptr + 1 - nullfont ) ; 
    dumpthings ( kernbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
    dumpthings ( extenbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
    dumpthings ( parambase [ nullfont ] , fontptr + 1 - nullfont ) ; 
    dumpthings ( fontglue [ nullfont ] , fontptr + 1 - nullfont ) ; 
    dumpthings ( bcharlabel [ nullfont ] , fontptr + 1 - nullfont ) ; 
    dumpthings ( fontbchar [ nullfont ] , fontptr + 1 - nullfont ) ; 
    dumpthings ( fontfalsebchar [ nullfont ] , fontptr + 1 - nullfont ) ; 
#endif
    for( k = nullfont ; k <= fontptr ; k++ ) {
	c_printnl("\\font");
	printesc( fontidtext ( k ) );
	printchar( 61 );
	printfilename( fontname(k), fontarea(k), 335 );
	if ( fontsize(k) != fontdsize(k) ) {
	  print( STR_AT_ );  printscaled( fontsize(k) );  print( STR_PT );
	} 
    }
  }
  println();
  printint( fmemptr - 7 );
  c_print(" words of font info for ");
  printint( fontptr - 0 );
  c_print(" preloaded font");
  if ( fontptr != 1 )
    printchar( 115 );

  /* Dump the hyphenation table 1324 (II) */
  { long anzahl = 0;
    for( k = 0 ; k <= hyphsize ; k++ ) {
      if ( hyphword(k) != 0 ) {
	dumpshort ( k );
	incr(anzahl);
      }
    }
    if( anzahl != hyphcount )
	printf("!! Internal error (HyphException Table Inconsistent) !!\n");
  }
  for( k = 0 ; k <= hyphsize ; k++ ) {
    if ( hyphword(k) != 0 ) {
      dumpshort ( hyphword(k) );
      dumpint ( hyphlist(k) );
    }
  }
  println();
  printint( hyphcount );
  c_print(" hyphenation exception");
  if ( hyphcount != 1 ) 
    printchar( 115 );
  if ( trienotready ) 
    inittrie();

  morethings[0] = triemax;
  morethings[1] = trieopptr;

  /* Dump a couple of more things 1326 (I) */
  morethings[2] = interaction;  /* mit ERW_INTERACTION nicht mehr notwendig */
  morethings[3] = formatident;

  dumpthings(morethings[0], 4);

  /* Dump the hyphenation table 1324 (III) */
  dumpthings ( trietrl[0], triemax + 1 );
  dumpthings ( trietro[0], triemax + 1 );
  dumpthings ( trietrc[0], triemax + 1 );

#ifdef HYPHSTRUCT
  dumpthings ( hyph_op[1], trieopptr );
#else
  dumpthings ( hyfdistance[1] , trieopptr );
  dumpthings ( hyfnum[1], trieopptr );
  dumpthings ( hyfnext[1], trieopptr );
#endif
  c_printnl("Hyphenation trie of length ");
  printint ( triemax ) ;
  printchar(38);		/* = '&' */
  printint( (integer)trieptr ); /* added "real" triemax for IniTeX (br) */
  c_print(" has ");
  printint ( trieopptr ) ; 
  c_print(" op");
  if ( trieopptr != 1 ) 
    printchar ( 115 ) ; 
  c_print(" out of ");
  printint ( trieopsize ) ;

  for( k = 255 ; k >= 0 ; --k ) {
    if ( trieused [ k ] > 0 ) {
      c_printnl("  ");
      printint ( trieused [ k ] ) ; 
      c_print(" for language ");
      printint ( k ) ; 
      dumpchar ( k );
      dumpshort ( trieused [ k ] );
    } 
  }

  /* Dump a couple of more things 1326 (II) */
  dumpint ( 69069L );
  tracingstats = 0;
  wclose ( fmtfile ); 
}
#endif /* INITEX */


boolean loadfmtfile ( void )
{ /* 6666 10 */
  loadfmtfile_regmem
  register integer j, k;
  register long_halfword p, q;
  register integer x;

  /* (br) UnDumpe moeglichst viel auf einmal, deshalb ... */
  unsigned long morethings[20];
  long hash_low_count;
  /* ein paar feste Strings, die ein paarmal verwendet werden */
  char *Must_Increase = "---! Must increase the %s (>%ld)\n";
  char *This_is = "This is a format file %s.\n";
  long len_of_strpool;

#ifdef TIME
#ifdef AMIGA
  unsigned long alt[2], neu[2], sum[2];
  sum[0] = 0; sum[1] = 0;

  timer(alt);
#else
  unsigned long alt, neu;

  alt = clock();
#endif
#endif

#ifndef NEW_STRING	/* For new strstart[] dump: */
  undumpthings(morethings[0], 17);
#else
  undumpthings(morethings[0], 18);
#endif

  if( (morethings[0] & FMT_CHECK) != FMT_CHECK ) {
    wakeupterminal ();
    (void) printf(This_is, "written by another TeX implementation");
    goto lab6666;
  }

#ifdef BIG
  if( (morethings[0] & CHECK_MASK) != BIGFMT ) {
    wakeupterminal ();
    if( (morethings[0] & CHECK_MASK) == NORMALFMT ) {
	(void) printf(This_is, "for a `normal' TeX");
    } else {
	(void) printf(This_is, "written by an older/newer release");
    }
    goto lab6666;
  }
#else
  if( (morethings[0] & CHECK_MASK) != NORMALFMT ) {
    wakeupterminal ();
    if( (morethings[0] & CHECK_MASK) == BIGFMT ) {
	(void) printf(This_is, "for a `big' TeX");
    } else {
	(void) printf(This_is, "written by an older/newer release");
    }
    goto lab6666;
  }
#endif

#if defined(MLTEX) || defined(TEXXET)
  if( (morethings[1] & 0xff000000) != ADDITIONAL ) {
    wakeupterminal ();
    (void) printf(This_is, "of a Standard TeX Version");
    goto lab6666;
  }
# if defined(MLTEX) && defined(TEXXET)
  if( (morethings[1] & (ADD_MLTEX | ADD_TEXXET)) != (ADD_MLTEX | ADD_TEXXET))
# else
#  ifdef MLTEX
  if( (morethings[1] & ADD_MLTEX) != ADD_MLTEX )
#  endif
#  ifdef TEXXET
  if( (morethings[1] & ADD_TEXXET) != ADD_TEXXET )
#  endif
# endif
  {
    wakeupterminal ();
    (void) printf(This_is, "of a TeX with incompatible Patches");
    goto lab6666;
  }

# ifdef TEXXET
  if( morethings[1] & TEXXETCHECK ) {
    wakeupterminal ();
    (void) printf("[This is TeX--XeT (1.0)]\n");
    is_TeX_XeT = true;
  } else {
    is_TeX_XeT = false;
  }
# endif

# ifdef MLTEX
  if( morethings[1] & MLTEXCHECK ) {
    wakeupterminal ();
    (void) printf("[This is ML-TeX, Version 3]\n");
    is_ML_TeX = true;
  } else {
    is_ML_TeX = false;
  }
# endif

#else
  if( morethings[1] != membot ) {
    if( (morethings[1] & 0xff000000) == ADDITIONAL ) {
	wakeupterminal ();
	(void) printf(This_is, "of TeX with additional Patches");
    }
    goto lab6666;
  }
#endif

  if( morethings[2] != memtop ) {
#ifdef INITEX
     /* For IniTeX, memtop cannot be changed (or only both of memtop &
      * memmax), so ...
      */
     (void) printf("--- ! Set `mem_top' = %ld (or use virtex) !\n",
		morethings[2]);
     goto lab6666;
#else
     /* If there's enough memory, we set mem_top accordingly and initialize
      * all once more.  (This should work without problems, but it's better
      * to inform the user.)
      */
     if( morethings[2] > memmax ) {
	(void) printf("--- ! Set `mem_max' >= %ld !!\n", morethings[2]);
	goto lab6666;
     }

#if 0	/* it seems, that this is ok... */
     (void) printf("--- ! memtop (%ld) != fmt-memtop (%ld), but it's ok for me.\n",
	memtop, morethings[2]);
#endif
     memtop = morethings[2];

     /* Don't forget: Set mem[] & other locations for this mem_top value */
     initialize();
#endif
  }
  if( morethings[3] != eqtbsize )
	goto lab6666;
  if( morethings[4] != hashprime )
	goto lab6666;
  if( morethings[5] != hyphsize )
	goto lab6666;

#ifdef TIME
#ifdef AMIGA
  timer(neu);
  printf("Undump constants:  %ld, %ld\n", neu[0]-alt[0], neu[1]-alt[1]);
  sum[0] += neu[0]-alt[0];
  sum[1] += neu[1]-alt[1];
  timer(alt);
#else
  neu = clock();
  printf("Undump constants:  %ld\n", neu-alt);
  alt = clock();
#endif
#endif

  /* Undump the string pool 1310 */
  x = morethings[6];		/* poolptr */
  if ( x < 0 ) 
    goto lab6666 ; 
  if ( x > poolsize ) {
    wakeupterminal () ; 
    (void) printf(Must_Increase, "string pool size", x);
    goto lab6666 ; 
  }
  poolptr = x ;

  x = morethings[7];		/* strptr */
  if ( x < 0 ) 
    goto lab6666 ; 
  if ( x > maxstrings ) {
    wakeupterminal () ; 
    (void) printf(Must_Increase, "max strings", x);
    goto lab6666 ;
  }
  strptr = x ; 

#ifndef NEW_STRING
  undumpthings ( strstart[0], strptr + 1 );
#else
  /* see remarks in storefmtfile() */
  { register unsigned char *src;
    unsigned char *buf = (unsigned char *)&strstart[0];

    len_of_strpool = morethings[17];
    src = &buf[(maxstrings * sizeof(poolpointer)) - len_of_strpool];
    undumpthings(src[0], len_of_strpool);
  }
#endif
  /* Nur in Vielfachen von 4 undumpen */
  undumpthings( strpool[0], ROUND_FOUR(poolptr) );

  initstrptr = strptr ; 
  initpoolptr = poolptr ; 
#ifdef TIME
#ifdef AMIGA
  timer(neu);
  printf("Undump String Pool:  %ld, %ld\n", neu[0]-alt[0], neu[1]-alt[1]);
  sum[0] += neu[0]-alt[0];
  sum[1] += neu[1]-alt[1];
  timer(alt);
#else
  neu = clock();
  printf("Undump String Pool:  %ld\n", neu-alt);
  alt = clock();
#endif
#endif


#ifdef ERW_CODEPAGE
  if ( codepage_given ) {
    /* ignore the arrays */
    {ASCIIcode foo[256];
     undumpthings(foo[0], 256);
     undumpthings(foo[0], 256);
    }
    {boolean foo[256];
     undumpthings(foo[0], 256);
    }
  } else {
#endif
    undumpthings(xord[0], 256);
    undumpthings(xchr[0], 256);
#ifdef ERW_CODEPAGE
    undumpthings(printable[0], 256);
  }
#endif


  /* Undump the dynamic memory 1312 */
  x = morethings[8];
  if ( ( x < lomemstatmax + 1000 ) || ( x > himemstatmin - 1 ) )
    goto lab6666;
  lomemmax = x ;

  x = morethings[9];
  if ( ( x < lomemstatmax + 1 ) || ( x > lomemmax ) ) 
    goto lab6666;
  rover = x; 

  { register long_halfword r_rover = rover;
    register long_halfword r_lomemmax = lomemmax;

    p = membot;
    q = r_rover;
    do {
      undumpthings ( mem [ p ] , q + 2 - p );
      p = q + nodesize ( q );
      if (  p > r_lomemmax  || ( q >= rlink(q) && rlink(q) != r_rover ) )
	goto lab6666;
      q = rlink ( q );
    } while ( q != r_rover );
    undumpthings ( mem[p], r_lomemmax + 1 - p );
  }
  if ( memmin < membot - 2 ) {
    p = llink ( rover );
    q = memmin + 1 ; 
    link ( memmin ) = 0 ; 
    info ( memmin ) = 0 ; 
    rlink ( p ) = q ; 
    llink ( rover ) = q ; 
    rlink ( q ) = rover ; 
    llink ( q ) = p ; 
    link ( q ) = emptyflag ;
    nodesize ( q ) = membot - q;
  } 

  x = morethings[10];
  if ( ( x < lomemmax + 1 ) || ( x > himemstatmin ) ) 
    goto lab6666 ; 
  himemmin = x ; 

  x = morethings[11];
  if ( ( x < 0 ) || ( x > memtop ) ) 
    goto lab6666 ; 
  avail = x ; 

  /* Unddump the table of equivalents 1314 */
  x = morethings[12];
  if ( ( x < hashbase ) || ( x > frozencontrolsequence ) ) 
    goto lab6666 ; 
  parloc = x ; 
  partoken = cstokenflag + parloc ; 

  x = morethings[13];
  if ( ( x < hashbase ) || ( x > frozencontrolsequence ) ) 
    goto lab6666 ; 
  writeloc = x ; 

  x = morethings[14];
  if ( ( x < hashbase ) || ( x > frozencontrolsequence ) )
    goto lab6666 ;
  hashused = x ; 

  cscount = morethings[15];
  hash_low_count = morethings[16];

  /* Undump the dynamic memory 1312 (II) */
  memend = memtop ; 
  undumpthings ( mem [ himemmin ] , memend + 1 - himemmin ) ;

  undumpthings(morethings[0], 5);

  varused = morethings[0];
  dynused = morethings[1];

#ifdef TIME
#ifdef AMIGA
  timer(neu);
  printf("Undump Dynamic Memory:  %ld, %ld\n", neu[0]-alt[0], neu[1]-alt[1]);
  sum[0] += neu[0]-alt[0];
  sum[1] += neu[1]-alt[1];
  timer(alt);
#else
  neu = clock();
  printf("Undump Dynamic Memory:  %ld\n", neu-alt);
  alt = clock();
#endif
#endif

/* Fuer ML-TeX sollte man beim Einlesen eines "normalen" fmt-files
 * der Bereich von char_sub_base ... char_sub_base + 256 ueberlesen werden.
 */

  /* UnDump the table of equivalents 1313,... */
#if 0
  k = activebase ;
  do {
    undumpshort ( x ) ; 
    if ( ( x < 1 ) || ( k + x > eqtbsize + 1 ) )
	goto lab6666 ; 
    undumpthings ( eqtb [ k ] , x ) ; 
    k = k + x ; 
    undumpshort ( x ) ; 
    if ( ( x < 0 ) || ( k + x > eqtbsize + 1 ) )
	goto lab6666 ; 
    for( j = k ; j <= k+x-1 ; j++ ) {
      eqtb [ j ] = eqtb [ k - 1 ];
    }
    k = k + x;
  } while ( ! ( k > eqtbsize ) );
#else
  { register MEDmemoryword *current = &eqtb[activebase];
    MEDmemoryword *maxcur = &eqtb[eqtbsize+1];

    do {
      undumpshort ( x ) ; 
      if ( x < 1  || ( current + x > maxcur ) )
	goto lab6666 ; 
      undumpthings ( *current, x );
      current += x;
      undumpshort ( x ) ; 
      if ( x < 0  || ( current + x > maxcur ) )
	goto lab6666 ;
      { register MEDmemoryword *cur = current;

	--current;
	for( ; x > 0 ; --x )
	  *cur++ = *current++;
      }
      current++;
    } while ( current < maxcur );
  }
#endif

#ifdef TIME
#ifdef AMIGA
  timer(neu);
  printf("Undump Eqtb:  %ld, %ld\n", neu[0]-alt[0], neu[1]-alt[1]);
  sum[0] += neu[0]-alt[0];
  sum[1] += neu[1]-alt[1];
  timer(alt);
#else
  neu = clock();
  printf("Undump Eqtb:  %ld\n", neu-alt);
  alt = clock();
#endif
#endif

  /* Undump the hash table 1319 (II) */
  p = hashbase - 1 ;
  if( hash_low_count > 0 ) {
     /* Dies funktioniert nur mit dem GNU C !!!
      * ausserdem sollte Stack genuegend gross sein... (oder Hash klein)
      */
#ifdef __GNUC__
     short stelle[hash_low_count], *stp = stelle;
#else
     short *stelle, *stp;

     stelle = stp = (short *) xmalloc(sizeof(short) * hash_low_count);
#endif
     /* Hole zuerst mal beide Arrays herein ... */
     undumpthings( stelle[0], hash_low_count);
#ifdef TIME
#ifdef AMIGA
  timer(neu);
  printf("Undump Hash Table (netto):  %ld, %ld\n", neu[0]-alt[0], neu[1]-alt[1]);
  sum[0] += neu[0]-alt[0];
  sum[1] += neu[1]-alt[1];
  timer(alt);
#else
  neu = clock();
  printf("Undump Hash Table(netto):  %ld\n", neu-alt);
#endif
#endif

     p = hashbase - 1 ;
     for( j = 0 ; j < hash_low_count ; j++ ) {
	x = *stp++;
	if ( ( x < p + 1 ) || ( x > hashused ) )
	  goto lab6666;
	p = x;
#ifdef BIG
	(void) fread( (char *)&hash[p], sizeof(hash[p]), 1, dump_file );
#else
	undumphh ( hash [p] );
#endif
     }
#ifndef __GNUC__
     free(stelle);
#endif
  }
			/* v--LC BUG */
  undumpthings ( hash[(hashused+1)], undefinedcontrolsequence - 1 - hashused );
#ifdef TIME
#ifdef AMIGA
  timer(neu);
  printf("Undump Hash Table (gesamt):  %ld, %ld\n", neu[0]-alt[0], neu[1]-alt[1]);
  sum[0] += neu[0]-alt[0];
  sum[1] += neu[1]-alt[1];
  timer(alt);
#else
  neu = clock();
  printf("Undump Hash Table(gesamt):  %ld\n", neu-alt);
  alt = clock();
#endif
#endif

  /* Undump the font information 1321 */
  x = morethings[2];
  if ( x < 7 ) 
    goto lab6666 ; 
  if ( x > fontmemsize ) {
      wakeupterminal ();
      (void) printf(Must_Increase, "font mem size", x);
      goto lab6666 ; 
  }
  fmemptr = x;

  x = morethings[3];
  if ( x < 0 ) 
    goto lab6666 ; 
  if ( x > fontmax ) {
	wakeupterminal () ; 
	(void) printf(Must_Increase, "font max", x);
	goto lab6666 ; 
  }
  fontptr = x ; 

  undumpthings ( fontinfo [ 0 ] , fmemptr ) ; 

  undumpthings ( fontsize(nullfont), fontptr + 1 - nullfont ) ; 
  undumpthings ( fontdsize(nullfont), fontptr + 1 - nullfont ) ; 
  undumpthings ( fontname(nullfont), fontptr + 1 - nullfont ) ; 
  undumpthings ( fontarea(nullfont), fontptr + 1 - nullfont ) ;

#ifdef FONTSTRUCT
  undumpthings( fontdesc[nullfont], fontptr + 1 - nullfont);
#else
  undumpthings ( fontcheck [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( fontparams [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( hyphenchar [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( skewchar [ nullfont ] , fontptr + 1 - nullfont ) ; 
#ifdef FONTPTR
  undumpthings ( font_bcec [ nullfont ] , fontptr + 1 - nullfont );
#else
  undumpthings ( fontbc [ nullfont ] , fontptr + 1 - nullfont );
  undumpthings ( fontec [ nullfont ] , fontptr + 1 - nullfont );
#endif
  undumpthings ( charbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( widthbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( heightbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( depthbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( italicbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( ligkernbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( kernbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( extenbase [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( parambase [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( fontglue [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( bcharlabel [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( fontbchar [ nullfont ] , fontptr + 1 - nullfont ) ; 
  undumpthings ( fontfalsebchar [ nullfont ] , fontptr + 1 - nullfont ) ; 
#endif

  /* Nicht vergessen, evtl. noch Ptr nach fontinfo[] zu berichtigen */

#ifdef TIME
#ifdef AMIGA
  timer(neu);
  printf("Undump Font Info:  %ld, %ld\n", neu[0]-alt[0], neu[1]-alt[1]);
  sum[0] += neu[0]-alt[0];
  sum[1] += neu[1]-alt[1];
  timer(alt);
#else
  neu = clock();
  printf("Undump Font Info:  %ld\n", neu-alt);
  alt = clock();
#endif
#endif

  /* Undump the hyphenation table 1325 */
  x = morethings[4];
  if ( ( x < 0 ) || ( x > hyphsize ) ) 
    goto lab6666 ; 
  hyphcount = x ; 

  if( hyphcount > 0 ) {
    /* ... Wie fuer die Hashtabelle ... */
#ifdef __GNUC__
    short stelle[hyphcount], *stp = stelle;
#else
    short *stelle, *stp;

    stelle = stp = (short *) xmalloc(sizeof(short) * hyphcount);
#endif
    undumpthings(stelle[0], hyphcount);

    for( k = 0 ; k < hyphcount ; k++ ) {
	j = *stp++;
	if ( ( j < 0 ) || ( j > hyphsize ) ) 
	  goto lab6666 ;

	undumpshort ( x ) ; 
	if ( ( x < 0 ) || ( x > strptr ) ) 
	  goto lab6666 ; 
	hyphword(j) = x ; 

	undumpint ( x ) ; 
	if ( ( x < 0 ) || ( x > maxhalfword ) )
	  goto lab6666 ; 
	hyphlist(j) = x ; 
    }
#ifndef __GNUC__
    free(stelle);
#endif
  }

  undumpthings(morethings[0], 4);

  /* more ... */
  x = morethings[0];		/* trie_max */
  if ( x < 0 ) 
    goto lab6666 ; 
  if ( x > triesize ) {
      wakeupterminal () ; 
      (void) printf(Must_Increase, "trie size", x);
      goto lab6666 ;
  }
  j = x ; 
#ifdef INITEX
  triemax = j ; 
#endif /* INITEX */
  undumpthings ( trietrl [ 0 ] , j + 1 ) ; 
  undumpthings ( trietro [ 0 ] , j + 1 ) ; 
  undumpthings ( trietrc [ 0 ] , j + 1 ) ; 

  x = morethings[1];		/* trie_op_ptr */
  if ( x < 0 ) 
    goto lab6666 ; 
  if ( x > trieopsize ) {
      wakeupterminal () ; 
      (void) printf(Must_Increase, "trie op size", x);
      goto lab6666 ; 
  }
  j = x ; 
#ifdef INITEX
  trieopptr = j ; 
#endif /* INITEX */
#ifdef HYPHSTRUCT
  undumpthings ( hyph_op[1], j );
#else
  undumpthings ( hyfdistance [ 1 ] , j );
  undumpthings ( hyfnum [ 1 ] , j );
  undumpthings ( hyfnext [ 1 ] , j );
#endif

#ifdef TIME
#ifdef AMIGA
  timer(neu);
  printf("Undump Hyphenation Table:  %ld, %ld\n", neu[0]-alt[0], neu[1]-alt[1]);
  sum[0] += neu[0]-alt[0];
  sum[1] += neu[1]-alt[1];
  timer(alt);
#else
  neu = clock();
  printf("Undump Hyphenation Table:  %ld\n", neu-alt);
  alt = clock();
#endif
#endif

#ifdef INITEX
  for( k = 0 ; k <= 255 ; k++ )
    trieused [ k ] = 0;
#endif /* INITEX */

  k = 256 ; 
  while ( j > 0 ) {
    {
      undumpchar ( x ) ; 
      if ( ( x < 0 ) || ( x > k - 1 ) ) 
      goto lab6666 ; 
      else k = x ; 
    } 
    {
      undumpshort ( x ) ; 
      if ( ( x < 1 ) || ( x > j ) ) 
      goto lab6666 ; 
      else x = x ; 
    } 
#ifdef INITEX
    trieused [ k ] = x ; 
#endif /* INITEX */
    j = j - x ; 
    opstart [ k ] = j ; 
  } 
#ifdef INITEX
  trienotready = false ;
#endif /* INITEX */

  /* Undump a couple more things 1327 */
#ifndef ERW_INTERACTION
  x = morethings[2];
  if ( ( x < batchmode ) || ( x > errorstopmode ) ) 
    goto lab6666 ; 
  else interaction = x ; 
#endif

  x = morethings[3];
  if ( ( x < 0 ) || ( x > strptr ) ) 
    goto lab6666 ; 
  else formatident = x ; 

  undumpint ( x ) ; 
  if ( ( x != 69069L ) || feof ( fmtfile ) ) 
    goto lab6666 ; 

#ifdef TIME
#ifdef AMIGA
  timer(neu);
  printf("Undump Rest:  %ld, %ld\n", neu[0]-alt[0], neu[1]-alt[1]);
  sum[0] += neu[0]-alt[0];
  sum[1] += neu[1]-alt[1];
  timer(alt);
  if (sum[1] < 0) {
    sum[0]--;
    sum[1] += 1000000;
  }
  printf("Undump SUMME: %lu, %lu\n", sum[0], sum[1]);
#else
  neu = clock();
  printf("Undump Rest:  %ld\n", neu-alt);
  alt = clock();
#endif
#endif

#ifdef NEW_STRING
  /* Hier werden jetzt noch alle notwendigen Umsortierungen, etc. die
   * notwendig sind, gemacht...
   */


  /* see remarks in storefmtfile() */
  { register unsigned char *src;

    { unsigned char *buf = (unsigned char *)&strstart[0];

      src = &buf[(maxstrings * sizeof(poolpointer)) - len_of_strpool];
    }
    /* undumpthings(src[0], len_of_strpool); */

    k = 0;
    /* Hole Inhalt von strstart[0] = Startadresse */
    for( j = sizeof(strstart[0]) ; j > 0 ; --j )
	k = (k * 256) + ((unsigned)*src++);

    strstart[0] = k;
    { poolpointer *pp = &strstart[1];

      for( j = 1 ; j < strptr + 1 ; j++ ) {
	x = (integer) (*src++ & 0xff);
	if( x == 255 ) {
	  x = (integer) ((unsigned)((*src++) << 8));
	  x += ((unsigned)*src++);
	}
	k += x;
	*pp++ = k;	/* strstart[..] = k; */
      }
    }

    if( strstart[strptr] != poolptr )
	printf("!! Internal Error (String Pool Inconsistent [%d]) !!\n", 2);
  }
#endif

#ifdef FONTPTR
  /* ...base[] should be C pointers in fontinfo[] */
  for( p = nullfont ; p <= fontptr ; p++ ) {
	charbase(p)    = &fontinfo[(long)charbase(p)];
	widthbase(p)   = &fontinfo[(long)widthbase(p)];
	heightbase(p)  = &fontinfo[(long)heightbase(p)];
	depthbase(p)   = &fontinfo[(long)depthbase(p)];
	italicbase(p)  = &fontinfo[(long)italicbase(p)];
	ligkernbase(p) = &fontinfo[(long)ligkernbase(p)];
	kernbase(p)    = &fontinfo[(long)kernbase(p)];
  }
#endif

  return(true);


lab6666:
  wakeupterminal ();
  (void) printf("(Fatal format file error; I'm stymied)\n");

  return(false);
}

/* -- end -- */
