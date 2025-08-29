#include <limits.h>
#include <stdlib.h>
#include <ctype.h>

#define INITEXTERN
#define EXTERN extern

#include "texd.h"

#undef VERBOSE		/* Debugging */

#ifdef atarist
extern long crlfflag;	/* write \n as \r\n in textfiles ? */
#endif

#ifdef AMIGA
extern int do_rexx;	/* defined in arexx.c */
int create_info_file = FALSE;
char info_file_name[150] = "TeX:config/dvi.info";
#endif

/*
 *  Name des Konfigurationsfiles wird in `read_configfile()' gesetzt!!
 *
 */


static void read_configfile(void);
static void init_variables(void);
static void debug_print_vals(void);
extern void debug_print_x(void);


#if defined(INITEX) && defined(STAT) && defined(VERBOSE)
/*
 * Da an einigen Stellen nicht ganz saubere Sachen verwendet werden, sollte
 * hier nachgeprueft werden, ob dies auch auf dieser Maschine funktioniert.
 * (Es reicht, wenn man dies nur beim Testen macht, deshalb nur wenn fuer
 * TripTest STAT und INITEX definiert sind)
 */

/* 1. Beim Laden eines Fonts werden nicht einzelne `getc'-Aufrufe verwendet,
 * sondern gleich pack-weise `fread's ausgefuehrt.  Dabei sollte der UNION
 * jedoch so liegen, dass die fourquarters richtig gelesen werden.  Ausserdem
 * sollte er genau 4 Bytes lang sein, da sonst Informationen fehlen.
 */
  void
check_architecture(void)
{
   SMALLmemoryword test;
   int bad = 0;

   test.qqqq.b0 = 0x12;
   test.qqqq.b1 = 0x34;
   test.qqqq.b2 = 0x56;
   test.qqqq.b3 = 0x78;

   if( test.cint == 0x78563412 )	/* LittleEndian, no BigEndian */
	bad |= 1;

   if( test.cint != 0x12345678 )	/* ??? */
	bad |= 2;

   if( sizeof(SMALLmemoryword) != 4 )
	bad |= 4;

   if( bad ) {
	fprintf(stderr, "check_architecture: Sorry bad=0%o\n", bad);
	exit(1);
   }
}
#endif


static short alloc_ok;	/* all malloc()-calls ok ? */

   static void *
check_malloc(char *name, char *type, size_t size, long len)
{
  void *temp;
  long oldlen = len;

#ifdef VERBOSE
  static long gesamt = 0L;

  gesamt += (size * len);
  printf("DBG: allocate(%s, %s, %d, %d)  sum = %ld\n",
	name, type, size, len, gesamt);
#endif

  if( alloc_ok == 0 ) {
     fprintf(stderr, "\
!! (`%s' requires %ld * %ld bytes of memory [parameter `%s'])\n",
	name, len, size, type);
     return NULL;
  }


  /* Hat man nicht genug Speicher ist es besser, wenn man einen moeglichen
     Wert angezeigt bekommt, sodass man besser mit den Parametern spielen
     kann.
   */
  while( (temp = malloc(size*len)) == NULL ) {
    len -= len/1024;
    len -= 8L;
    if( len < 0L )
	break;
  }

  if( temp == NULL ) {
     fprintf(stderr, "\
!! Can't allocate enough memory for `%s' [parameter `%s'].\n\
!! Please decrease parameters!\n",
	name, type, "", "");
     alloc_ok = 0;
  } else if( len != oldlen ) {
     fprintf(stderr, "\
!! Can't allocate enough memory for `%s' [parameter `%s'].\n\
!! (Using %ld elements instead of %ld (elementsize is %ld))\n\
!! Please decrease parameters for proper run!\n",
	name, type, len, oldlen, size);
     alloc_ok = 0;
  }
  return temp;
}


/***************************/
/* EXTERN................. */
/***************************/
   void
init_arrays(int debugflag)
{
#if defined(INITEX) && defined(STAT) && defined(VERBOSE)
  check_architecture();
#endif

  /* in VirTeX mark, if an extra codepage is given in
   * the configuration file, which should be used instead
   * of the one given in the format file
   */
#ifdef ERW_CODEPAGE
  codepage_given = 0;
#endif

  init_variables();
  read_configfile();

  if( debugflag ) {
#if defined(NONASCII)
	debug_print_x();
#endif
	debug_print_vals();
  }

  alloc_ok = 1;

#ifdef EQTB_ALLOC
  zeqtb = (MEDmemoryword *) check_malloc( "Equivalent Table (Internal)",
		"eqtbsize", sizeof(MEDmemoryword), (eqtbsize +1) );

  zzzae = (quarterword *) check_malloc( "Equivalent Table Level (Internal)",
		"xeqlevel", sizeof(quarterword), (eqtbsize-intbase+1) );
  /* it's more efficient to "offset" `xeqlevel' at the beginning! */
  zzzae -= (long) intbase;

  zzzaf = (twohalves *) check_malloc( "Hash Table (Internal)", "hashbase",
		sizeof(twohalves), (undefinedcontrolsequence - hashbase +1) );
  /* it's more efficient to "offset" `hash' at the beginning! */
  zzzaf -= (long)hashbase;

  /* Note: access to locations below xeqlevel[intbase] or hash[hashbase]
   * is forbidden, this is (should be) checked in the code.
   * (This is secure, because the original code substracts these offsets
   * each time it accesses the arrays...)
   */
#endif

  buffer = (ASCIIcode *) check_malloc( "input buffer", "bufsize",
			sizeof(ASCIIcode), (bufsize+1) );
  trickbuf = (ASCIIcode *) check_malloc( "trickbuffer", "errorline",
			sizeof(ASCIIcode), (errorline+1) );
  linestack = (integer *) check_malloc( "linestack", "maxinopen",
			sizeof(integer), (maxinopen+1) );
  inputfile = (alphafile *) check_malloc( "inputfiles", "maxinopen",
			sizeof(alphafile), (maxinopen+1) );
  inputstack = (instaterecord *) check_malloc( "inputstack", "stacksize",
			sizeof(instaterecord), (stacksize+1) );
  paramstack = (halfword *) check_malloc( "parameter stack", "paramsize",
			sizeof(halfword), (paramsize+1) );
  nest = (liststaterecord *) check_malloc( "semantic nest", "nestsize",
			sizeof(liststaterecord), (nestsize+1) );

  fontinfo = (SMALLmemoryword *) check_malloc( "fontinfo", "fontmemsize",
			sizeof(SMALLmemoryword), (fontmemsize+1) );

  fontdsize = (scaled *) check_malloc( "fontdsize", "fontmax",
			sizeof(scaled), (fontmax+1) );
  fontsize = (scaled *) check_malloc( "fontsize", "fontmax",
			sizeof(scaled), (fontmax+1) );

  fontarea = (strnumber *) check_malloc( "fontarea", "fontmax",
			sizeof(strnumber), (fontmax+1) );
  fontname = (strnumber *) check_malloc( "fontname", "fontmax",
			sizeof(strnumber), (fontmax+1) );

  fontused = (boolean *) check_malloc( "fontused", "fontmax",
			sizeof(boolean), (fontmax+1) );

#ifdef FONTSTRUCT

  fontdesc = (struct font_desc *) check_malloc( "font", "fontmax",
			sizeof(struct font_desc), (fontmax+1) );

#else

  fontcheck = (fourquarters *) check_malloc( "fontcheck", "fontmax",
			sizeof(fourquarters), (fontmax+1) );
  fontparams = (SMALLhalfword *) check_malloc( "fontparams", "fontmax",
			sizeof(SMALLhalfword), (fontmax+1) );
#ifdef FONTPTR
  font_bcec = (struct fnt_bcec *) check_malloc( "fontbc/ec", "fontmax",
			sizeof(struct fnt_bcec), (fontmax+1) );
#else
  fontbc = (eightbits *) check_malloc( "fontbc", "fontmax",
			sizeof(eightbits), (fontmax+1) );
  fontec = (eightbits *) check_malloc( "fontec", "fontmax",
			sizeof(eightbits), (fontmax+1) );
#endif
  fontglue = (halfword *) check_malloc( "fontglue", "fontmax",
			sizeof(halfword), (fontmax+1) );
  hyphenchar = (integer *) check_malloc( "hyphenchar", "fontmax",
			sizeof(integer), (fontmax+1) );
  skewchar = (integer *) check_malloc( "skewchar", "fontmax",
			sizeof(integer), (fontmax+1) );
  bcharlabel = (fontindex *) check_malloc( "bcharlabel", "fontmax",
			sizeof(fontindex), (fontmax+1) );
  fontbchar = (short *) check_malloc( "fontbchar", "fontmax",
			sizeof(short), (fontmax+1) );
  fontfalsebchar = (short *) check_malloc( "fontfalsebchar", "fontmax",
			sizeof(short), (fontmax+1) );
#ifdef FONTPTR
# define F_BASE_EL SMALLmemoryword *
#else
# define F_BASE_EL integer
#endif
  charbase = (F_BASE_EL *) check_malloc( "charbase", "fontmax",
			sizeof(F_BASE_EL), (fontmax+1) );
  widthbase = (F_BASE_EL *) check_malloc( "widthbase", "fontmax",
			sizeof(F_BASE_EL), (fontmax+1) );
  heightbase = (F_BASE_EL *) check_malloc( "heightbase", "fontmax",
			sizeof(F_BASE_EL), (fontmax+1) );
  depthbase = (F_BASE_EL *) check_malloc( "depthbase", "fontmax",
			sizeof(F_BASE_EL), (fontmax+1) );
  italicbase = (F_BASE_EL *) check_malloc( "italicbase", "fontmax",
			sizeof(F_BASE_EL), (fontmax+1) );
  ligkernbase = (F_BASE_EL *) check_malloc( "ligkernbase", "fontmax",
			sizeof(F_BASE_EL), (fontmax+1) );
  kernbase = (F_BASE_EL *) check_malloc( "kernbase", "fontmax",
			sizeof(F_BASE_EL), (fontmax+1) );
  extenbase = (integer *) check_malloc( "extenbase", "fontmax",
			sizeof(integer), (fontmax+1) );
  parambase = (integer *) check_malloc( "parambase", "fontmax",
			sizeof(integer), (fontmax+1) );

#endif

  savestack = (MEDmemoryword *) check_malloc( "savestack", "fontmax",
			sizeof(MEDmemoryword), (savesize+1) );
  strpool = (packedASCIIcode *) check_malloc( "strpool", "fontmax",
			sizeof(packedASCIIcode), (poolsize+1) );
  strstart = (poolpointer *) check_malloc( "strstart", "fontmax",
			sizeof(poolpointer), (maxstrings+1) );

#ifdef HYPHSTRUCT
  hyph_op = (struct hyph_op *) check_malloc( "hyphenation operands", "trieopsize",
			sizeof(struct hyph_op), (trieopsize+1) );
#else
  hyfdistance = (smallnumber *) check_malloc( "hyfdistance", "trieopsize",
			sizeof(smallnumber), (trieopsize+1) );
  hyfnum = (smallnumber *) check_malloc( "hyfnum", "trieopsize",
			sizeof(smallnumber), (trieopsize+1) );
  hyfnext = (trieopcode *) check_malloc( "hyfnext", "trieopsize",
			sizeof(trieopcode), (trieopsize+1) );
#endif

#ifdef INITEX
  zzzaj = (SMALLhalfword *) check_malloc( "trieophash", "trieopsize",
			sizeof(SMALLhalfword), (2*trieopsize+1) );
  trieoplang = (ASCIIcode *) check_malloc( "trieoplang", "trieopsize",
			sizeof(ASCIIcode), (trieopsize+1) );
  trieopval = (trieopcode *) check_malloc( "trieopval", "trieopsize",
			sizeof(trieopcode), (trieopsize+1) );
#endif

  trietrl = (halfword *) check_malloc( "trie_trl", "triesize",
			sizeof(halfword), (triesize+1) );
  trietro = (halfword *) check_malloc( "trie_tro", "triesize",
			sizeof(halfword), (triesize+1) );
  trietrc = (quarterword *) check_malloc( "trie_trc", "triesize",
			sizeof(quarterword), (triesize+1) );

#ifdef INITEX
  triec = (packedASCIIcode *) check_malloc( "trie_c", "triesize",
			sizeof(packedASCIIcode), (triesize+1) );
#ifdef ORIGTRIE
  trieo = (trieopcode *) check_malloc( "trie_o", "triesize",
			sizeof(trieopcode), (triesize+1) );
  trietaken = (boolean *) check_malloc( "trie_taken", "triesize",
			sizeof(boolean), (triesize+1) );
#else
  ztrie = (trie_struct *) check_malloc( "trie_o/taken", "triesize",
			sizeof(trie_struct), (triesize+1) );
#endif
  triehash = (triepointer *) check_malloc( "trie_hash", "triesize",
			sizeof(triepointer), (triesize+1) );
  triel = (triepointer *) check_malloc( "trie_l", "triesize",
			sizeof(triepointer), (triesize+1) );
  trier = (triepointer *) check_malloc( "trie_r", "triesize",
			sizeof(triepointer), (triesize+1) );
#endif

  dvibuf = (eightbits *) check_malloc( "DVI buffer", "dvibufsize",
			sizeof(eightbits), (dvibufsize+1) );

#ifdef DEBUG
  zzzab = (boolean *) check_malloc( "free array (debug)", "memmax",
			sizeof(boolean), (memmax - memmin +1) );
  zzzac = (boolean *) check_malloc( "was free array (debug)", "memmax",
			sizeof(boolean), (memmax - memmin +1) );
#endif

#if 0  /* !defined(INITEX) && defined(atarist) */
  /* Fuer `virtex' ist `memmax' der maximal zu holende Speicher.  Falls nicht
   * mehr so viel vorhanden, moeglichst viel holen.
   */
#ifdef VERBOSE
  printf("malloc mem: memmax: %d memtop: %d alloc_ok: %d \n",
	 memmax, memtop, alloc_ok);
#endif
  if( alloc_ok != 0 ) {
    size_t len = memmax - memmin + 1;

    while( len >= memtop ) {
#ifdef VERBOSE
      printf("malloc mem-loop: len: %d memtop: %d\n", len, memtop);
#endif
      if( (zzzaa = (memoryword *) malloc( sizeof(memoryword) * len )) != NULL )
	break;
      len -= 256;
    }
    /* ... und fuer den Fall, dass der malloc gescheitert ist, benutzen wir
     * die normale Routine, um dem Benutzer den freien Speicher noch
     * anzuzeigen.
     */
    if( len < memtop ) {
      zzzaa = (memoryword *) check_malloc( "TeX's main memory", "memmax",
				sizeof(memoryword), (memmax - memmin +1) );
    } else {
      /* ... ansonsten `memmax' auf den neuen Wert setzen */
      memmax = len + memmin - 1;
      if( debugflag )
	printf("DEBUG: set memmax = %ld\n", memmax);
    }
  }
#else
  zzzaa = (memoryword *) check_malloc( "TeX's main memory", "memmax",
			sizeof(memoryword), (memmax - memmin +1) );
#endif

  /* all malloc()-Calls ok ? */
  if( alloc_ok == 0 )
    exit(2);

#ifdef AMIGA
  { char *ptr;

    do_rexx = 0;
    if( (ptr = getenv("TEXREXX")) != NULL ) {
      if( !stricmp(ptr, "edit") )
	do_rexx = 2;		/* edit file immediatly */
      else
	do_rexx = 1;
    }
  }
#endif
}


#  define c_memmax		65530L
#  define c_memtop		c_memmax
#  define c_bufsize		2000 
#  define c_errorline		79 
#  define c_halferrorline	50  
#  define c_maxprintline	79
#  define c_stacksize		200 
#  define c_maxinopen		15 
#  define c_fontmax		120 
#  define c_fontmemsize		36000L
#  define c_maxstrings		4400 
#  define c_stringvacancies	15000
#  define c_poolsize		45000L
#  define c_savesize		2000
#  define c_paramsize		60
#  define c_nestsize		40
#  define c_trieopsize		500
#ifdef INITEX
#  define c_triesize		19000
#else
#  define c_triesize		16000
#endif
#  define c_hyphsize		608
#  define c_dvibufsize		16384


#if defined(NONASCII)
   void
debug_print_x(void)
{ short i;

  for( i = 0 ; i <= 255 ; i++ )
     if( xchr[i] != i || xord[i] != i
	|| (printable[i] && (i < 32 || i > 126)) )

	printf("DEBUG: 0x%x:  xord[]= 0x%x  xchr[]= 0x%x  is%s printable\n",
		i, xord[i], xchr[i], (printable[i] ? "" : " not") );
}
#endif


   static void
init_variables(void)
{ short i;

  /* Initialize charset:  xchr: internalASCII -> external
			  xord: external      -> internalASCII
   */

  /* The ST use an ASCII encoding, so ... (save space) */
  for( i = 0 ; i <= 255 ; i++ ) {
	xchr[i] = chr(i);
	/* ... and the rest is just this: (only for this xchr[] !!) */
	xord[i] = i;	/* xord[xchr[i]] = i;  xord[127] = 127; */
  }
  /* The CodePage is read in init_variables(), it changes both arrays */

  /* Initialize printable table */
  for( i = 0 ; i < 32 ; i++ )
	printable[i] = 0;
  for( i = 32 ; i < 127 ; i++ )
	printable[i] = 1;
  for( i = 127 ; i < 256 ; i++ )
	printable[i] = 0;


#if defined(INITEX) && defined(NONASCII)

#if 0
  /* Vielleicht eine gute Idee... als Default nicht 1:1 Umsetzung, sondern
     auch noch andere anbieten...
   */
  /* Setup standard Translation tables: */
  /* 1. Atari ST --> Cork Extended Font Encoding */
  { unsigned char table[128] = {
	0xc7,	0xfc,	0xe9,	0xe2,	0xe4,	0xe0,	0xe5,	0xe7,
	0xea,	0xeb,	0xe8,	0xef,	0xee,	0xec,	0xc4,	0xc5,
	0xc9,	0xe6,	0xc6,	0xf4,	0xf6,	0xf2,	0xfb,	0xf9,
	0xb8,	0xd6,	0xdc,	0,	0xbf,	0,	0xff,	0,
	0xe1,	0xed,	0xf3,	0xfa,	0,	0,	0,	0,
	0xbe,	0,	0,	0,	0,	0x1a,	0x13,	0x14,
	0xe3,	0xf5,	0xd8,	0xf8,	0xf7,	0xd7,	0xc1,	0xc3,
	0xd5,	0x04,	0x01,	0,	0,	0,	0,	0,
	0xbc,	0x9c,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0x9f,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0x06,	0x0a,	0,	0,	0,	0,	0,	0x09
    }
    if( std_end_flag ) {
      for( i = 0 ; i < 128 ; i++ ) {
	if( table[i] != 0 ) {
	  xchr[i+128] = table[i];
	  printable[i] = 1;
	}
      }
    }
  }
#endif
#endif

   bufsize = c_bufsize;
   errorline = c_errorline;
   halferrorline = c_halferrorline;
   maxprintline = c_maxprintline;
   stacksize = c_stacksize;
   maxinopen = c_maxinopen;
   fontmax = c_fontmax;
   fontmemsize = c_fontmemsize;
   maxstrings = c_maxstrings;
   stringvacancies = c_stringvacancies;
   poolsize = c_poolsize;
   savesize = c_savesize;
   paramsize = c_paramsize;
   nestsize = c_nestsize;
   triesize = c_triesize;
   trieopsize = c_trieopsize;
   memtop = c_memtop;
/*   hyphsize = c_hyphsize; */
   dvibufsize = c_dvibufsize;

   /* In INITEX sollte memtop=memmax sein, in VIRTEX kann memax >= memtop
    * sein, dabei wird nur bis memtop aus dem fmt-File gelesen (es steht ja
    * auch nicht mehr drin ;-)
    */
#ifdef INITEX
   memmax = c_memtop;
#else
   memmax = c_memmax;
#endif
   /* FUER MEMBOT: !!!! */
   /* Jetzt sollte man noch memmin = membot fuer IniTeX setzen und
    * memmin = 0 fuer VirTeX.  Jedoch wird dann bei jedem Zugriff auf das
    * `mem' Array vom Index noch `memmin' abgezogen ==> langsamer....
    */
}

static long crash_int;		/* temporaer */

static struct iname {
	char *name;
	long *value;
} init_var_name[] = {
	"memmax",		&memmax,
	"fontmax",		&fontmax,
	"fontmemsize",		&fontmemsize,
	"stringvacancies",	&stringvacancies,
	"maxstrings",		&maxstrings,
	"poolsize",		&poolsize,
	"savesize",		&savesize,
	"paramsize",		&paramsize,
	"nestsize",		&nestsize,
	"bufsize",		&bufsize,
	"errorline",		&errorline,
	"halferrorline",	&halferrorline,
	"maxprintline",		&maxprintline,
	"maxinopen",		&maxinopen,
	"stacksize",		&stacksize,
	"triesize",		&triesize,
	"trieopsize",		&trieopsize,
	"dvibufsize",		&dvibufsize,
	"memtop",		&memtop,
#ifdef INITEX
	"itriesize",		&triesize,
#else
	"itriesize",		&crash_int,
#endif
#ifdef atarist
	"crlf",			&crlfflag,
#endif
	"language",		&user_language,
	(char *)NULL,		(long *)NULL
};


   static void
debug_print_vals()
{
  struct iname *ip;

  ip = init_var_name;
  while( ip->name != NULL ) {
    if( ip->value != &crash_int )
	printf("DEBUG: %s = %ld\n", ip->name, *(ip->value));
    ip++;
  }
}



   static void
read_cnf(FILE *cf, char *filename)
{ 
  unsigned char readbuf[256];
  signed long temp;
  unsigned char *cp, *endp;
  int index;
  struct iname *ip;
  long cnf_line = 0;

  while (fgets((char *)readbuf, 255, cf) != NULL) {
    cnf_line++;

    if (readbuf[0] == '#')		/* Ende des Files */
	return;


    /* Search for percent sign */
    cp = readbuf;
    while( *cp && *cp != '%' )
	cp++;
    if (*cp == '%')			/* Kommentarzeile */
	*cp = '\0';	/* ignore rest of line */


    /* Get first word starting in column 1 */
    cp = readbuf;
    while( *cp && isascii(*cp) && !isspace(*cp) )
	cp++;
    if( *cp == '\0' )	/* Leerzeile ? */
	continue;
    if( cp == readbuf )	/* faengt mit Spaces an... Zeile ignorieren */
	continue;
    *cp++ = '\0';	/* erstes Wort mit \0 abschliessen */


    /* ... und gleich weiterlesen (schadet nicht) */
    while( *cp && isascii(*cp) && isspace(*cp) )
	cp++;


    /* Is it an environment variable ? */
    index = get_env_var_index((char *)readbuf);
    if( index >= 0 ) {
	if( *cp == '\0' ) {	/* anscheinend keine Dir-Liste */
	  fprintf(stderr,
		  "%s (l.%ld): No Directory list for `%s', ignored.\n",
		  filename, cnf_line, readbuf);
	   continue;
	}
	endp = cp;
	while( *endp && isascii(*endp) && !isspace(*endp) )
	   endp++;
	*endp = '\0';

	/* Setze Pfad */ 
	do_path(index, (char *)cp);
	continue;
    }


#ifdef AMIGA
    /* Info-Files fuer DVI-File erzeugen.. ja oder? */
    if( !strcmp("create-info", (char *)readbuf) ) {
      if( !strncmp("on", cp, 2) ) {
        create_info_file = TRUE;
      }
      continue;		/* go to next line */
    }
    if( !strcmp("info-file-name", (char *)readbuf) ) {
      endp = cp;
      while( *endp && isascii(*endp) && !isspace(*endp) )
        endp++;
      *endp = '\0';
      if( strlen(cp) < sizeof(info_file_name) ) {
        strcpy(info_file_name, cp);
      }
      continue;
    }
#endif


#if 0
    /* Include file ... subfile */
    /* Dies kann zu Rekursion fuehren... aber das merkt man spaetestens
     * dann, wenn keine `file descriptors' mehr vorhanden sind.
     */
    if( !strcmp("include", (char *)readbuf) ) {
	alphafile subfp;
	int flag;

	endp = cp;
	while( *endp && isascii(*endp) && !isspace(*endp) )
	   endp++;
	*endp = '\0';

	sprintf(nameoffile, " %s ", cp);
	namelength = strlen(cp);
	flag = aopenin(subfp, TEXCONFIGPATH);

	/* 'cf' hat undefinierten Wert, falls File nicht lesbar, deshalb ganz
	 * auf Nummer Sicher ...
	 */
	if (!flag || subfp == NULL) {
	  fprintf(stderr,
		  "%s (l.%ld): Includefile `%s' not found, ignored.\n",
		  filename, cnf_line, cp);
	} else {
	  read_cnf(subfp, (char *)cp);
	  aclose(subfp);
	}
        continue;
    }
#endif /* include */


    /* Starting a codepage ... ? */
    if( !strcmp("codepage", (char *)readbuf) ) {

#if defined(NONASCII)
	short c1, c2, op, tmp;

#ifdef ERW_CODEPAGE
	/* codepage for VirTeX is marked with a `*' after `codepage' */
	if( *cp == '*' )
	  codepage_given = 1;
#endif

	while (fgets((char *)readbuf, 255, cf) != NULL) {
	  cnf_line++;
	  cp = readbuf;
	  op = 0;

	  /* read over whitespace before first number/operand */
	  while( *cp && isascii(*cp) && isspace(*cp) )
	    cp++;

#define is_hex(c) \
  ( ((c) >= '0' && (c) <= '9') || ((c) >= 'a' && ((c) <= 'f')) )

	  /* Read number 1 */
	  if( *cp == '<' ) {
	     op = 3;
	     c1 = 1;
	  } else if( *cp == '^' && *(cp + 1) == '^'
		  && *((unsigned char *)(cp + 2)) < 128 ) {
	     cp += 2;
	     c1 = *cp;
	     tmp = *(cp + 1);
	     if( is_hex(c1) && is_hex(tmp) ) {
		c1 = ( (c1 <= '9') ? (c1 - '0') : (c1 - 'a' + 10) ) * 16
			+ ((tmp <= '9') ? (tmp - '0') : (tmp - 'a' + 10)) ;
		cp++;
	     } else
		c1 += ( c1 < 64 ) ? +64 : -64;
	  } else if( *cp == '\0' ) {
	     /* \n und \r sollten natuerlich (isspace() == true) sein !! */
	     continue;		/* Leerzeile ignorieren */
	  } else
	     c1 = *((unsigned char *)cp);
	  cp++;

	  /* Read the operand '<<', '<', '<|', '=' or '>' */
	  if( op == 3 ) {	/* Read '<' before ? */
	     if( *cp == '<' )
		break;		/* while */
	     if( *cp == '|' ) {
		op = 4;
		cp++;
	     }
	  } else {
	     /* read over whitespace before operand */
	     while( *cp && isascii(*cp) && isspace(*cp) )
		cp++;

	     if( *cp == '>' ) {
		op = 2;
		cp++;
	     } else if( *cp == '=' ) {
		op = 1;
 		cp++;
	     } else {
		fprintf(stderr,
		  "%s (l.%ld): Syntax error in Codepage, false operand.\n",
		  filename, cnf_line);
		exit(2);
	     }
	  }

	  /* read over whitespace after operand */
	  while( *cp && isascii(*cp) && isspace(*cp) )
	    cp++;

	  /* Read number2 */
	  if( *cp == '^' && *(cp + 1) == '^'
	   && *((unsigned char *)(cp + 2)) < 128 ) {
	     cp += 2;
	     c2 = *((unsigned char *)cp);
	     tmp = *((unsigned char *)(cp + 1));
	     if( is_hex(c2) && is_hex(tmp) ) {
		c2 = ( (c2 <= '9') ? (c2 - '0') : (c2 - 'a' + 10) ) * 16
			+ ((tmp <= '9') ? (tmp - '0') : (tmp - 'a' + 10)) ;
		cp++;
	     } else
		c2 += ( c2 < 64 ) ? +64 : -64;
	  } else
	     c2 = *((unsigned char *)cp);

	  /* Now we have read 2 number and an operand */

	  /* The 2 numbers must < 32 or > 126 and != [0,10,13] */
	  if( (c1 >= 32 && c1 <= 126) || c1 == 0 || c1 == 10 || c1 == 13 ) {
	    fprintf(stderr,
"%s (l.%ld): Invalid value for %s number (0x%x) in codepage, ignored.\n",
		filename, cnf_line, "first", c2);
	    continue;		/* while */
	  }
	  /* For x>y, y is not restricted */
	  if( (op != 2) && ( (c2 >= 32 && c2 <= 126)
				|| c2 == 0 || c2 == 10 || c2 == 13 ) ) {
	    fprintf(stderr,
"%s (l.%ld): Invalid value for %s number (0x%x) in codepage, ignored.\n",
		filename, cnf_line, "second", c2);
	    continue;		/* while */
	  }

	  switch( op ) {
	  case 4:
#ifdef VERBOSE
	     printf("DBG: Char %d is set unprintable ( xchr[%d] = %d ).\n",
		c2, c2, c2);
#endif
	     xchr[c2] = c2;
	     printable[c2] = 0;
	     break;
	  case 3:
#ifdef VERBOSE
	     printf("DBG: Char %d is set printable ( xchr[%d] = %d ).\n",
		c2, c2, c2);
#endif
	     xchr[c2] = c2;
	     printable[c2] = 1;
	     break;
	  case 2:
#ifdef VERBOSE
	     printf("DBG: read %d as %d ( xord[%d] = %d ).\n",
		c1, c2, c1, c2);
#endif
	     xord[c1] = c2;
	     break;
	  case 1:
#ifdef VERBOSE
	     printf(
    "DBG: read %d as %d, write %d as %d ( xord[%d] = %d ; xchr[%d] = %d ).\n",
		c1, c2, c2, c1, c1, c2, c2, c1);
#endif
	     xord[c1] = c2;
	     xchr[c2] = c1;
	     printable[c2] = 1;
	     break;
	  default:
	     /* dies kann nicht sein, da sonst oben exit()-call */
	     break;
	  }

	}	/* end while */
#else
	/* Otherwise (VirTeX): Read until "<<" found */

	while (fgets((char *)readbuf, 255, cf) != NULL) {
	  cnf_line++;
	  cp = readbuf;
	  /* read over whitespace before first number/operand */
	  while( *cp && isascii(*cp) && isspace(*cp) )
	    cp++;
	  if( *cp == '<' && *(cp+1) == '<' )
		break;
	}
#endif
	continue;	/* Read next lines in Config-File */
    }

    /* ... else look for other parameters. */
    ip = init_var_name;
    while( ip->name != NULL && strcmp(ip->name, (char *)readbuf) )
	ip++;

    if( ip->name == NULL ) {
	fprintf(stderr, "%s (l.%ld): Unknown parameter `%s', ignored.\n",
		filename, cnf_line, readbuf);
	continue;
    }

    /* 'cp' steht schon am Anfang des Wertes (siehe oben) */

    /* Ist der 3.Parameter von 'strtol' 0, kann man -- wie in C --
     * die Basis mit der Zahl angeben, also z.B. 011 ist oktal (dez. = 9)
     * 0xff oder 0Xff ist hexadezimal (dez. = 255).
     */
    temp = strtol((char *)cp, NULL, 0);

    /* Bei Overflow ist 'errno' gesetzt und Wert = LONG_MAX/MIN */
    if( temp != LONG_MAX && temp != LONG_MIN ) {
	*(ip->value) = temp;
    } else {
	fprintf(stderr, "%s (l.%ld): Incorrect value for `%s', ignored.\n",
		filename, cnf_line, ip->name);
    }
  }		/* while( fgets(...) ) */
}


   static void
read_configfile(void)
{
  alphafile cf;
  int flag;

#ifdef AMIGA
#ifdef BIG
  char cnfname[] = "BigTeX.cnf";
#else
  char cnfname[] = "TeX.cnf";
#endif
#else
  /* Fuer den ST ist es unter TOS unwichtig, ob Gross- oder Kleinschreibung.
   * Mit MiNT bzw. den GCC-libs kann es jedoch schon Probleme geben, da man
   * hier ein evtl. Mapping hat (ueber File .dir).
   */
#ifdef BIG
  char cnfname[] = "bigtex.cnf";
#else
  char cnfname[] = "tex.cnf";
#endif
#endif

  sprintf(nameoffile, " %s ", cnfname);
  namelength = strlen(cnfname);
  flag = aopenin(cf, TEXCONFIGPATH);

  /* 'cf' hat undefinierten Wert, falls File nicht lesbar, deshalb ganz
   * auf Nummer Sicher ...
   */
  if (!flag || cf == NULL) {
    fprintf(stderr,
	"Warning: %s not found, using the default values.\n", cnfname);
    return;
  }

  read_cnf(cf, cnfname);

#ifdef INITEX
  memmax = memtop;	/* fuer INITEX: immer memtop = memmax */
			/* Damit kann memmax & memtop fuer Initex & Virtex */
			/* im gleichen Configfile angegeben werden. */
#endif

  if( memtop > memmax ) {
    fprintf(stderr, "Error in Config-File %s: 'mem_top' > 'memmax'!\n",
	cnfname);
    memmax = memtop;	/* memtop <= memmax !! */
    /* Da mindestens 'memtop' Platz benoetigt wird, um das gedumpte
     * Format einzulesen, wird nicht memtop=memmax verwendet.
     */
  }
  /* ... und das ganze auch wieder schliessen ... */
  aclose(cf);
}

/* end of init.c */
