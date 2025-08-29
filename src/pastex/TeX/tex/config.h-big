/*
 *	config.h
 *
 */

/* define BIG   for BIG-TeX Version
 * define STAT  for Statistics (enable \tracingXXXX macros) and
 *		for TripTesting                      [ define it! ]
 * define DEBUG	for debugging version (Gurus only !) [ don't define it! ]
 *
 * define BIGTRIE (only if BIG defined) allows triesize > 65535 in Big-TeX
 *              (requires a lot of memory for Big-IniTeX!!)
 *
 * define MLTEX  for multilingual TeX 3         (incomplete!!)
 * define TEXXET for TeX--XeT (Breitenlohner)	(incomplete!!!!)
 *
 * define EVPATH for recursive path resolution (AMIGA only)
 */

#define BIG

#define STAT
#undef DEBUG


#define EVPATH


#ifndef AMIGA
#undef EVPATH
#endif


/**************************************/
/*     inkompatible Erweiterungen     */
/*                                    */

#undef MLTEX
#undef TEXXET

/*                                    */
/* inkompatible Erweiterungen -- Ende */
/**************************************/



/**********************************************/
/*      eigene, kompatible Erweiterungen      */
/*                                            */

	/* Bei jedem Oeffnen eines Files wird ein neuer String angelegt, d.h.
	 * nach "\openin1 test.aux \ifeof1 \else \input test.aux \fi"
	 * (entspricht einem LaTeX \input) sind zwei! Strings im Stringpool
	 * abgelegt.
	 * Mit FILENAMECACHE wird ein LRU-Cache der zuletzt angesprochenen
	 * Teile eines Filenamens mit Groesse FNCACHELEN angelegt.
	 */
#define FILENAMECACHE
#define FNCACHELEN	16


	/* Erweitere Standard-TeX um CodePage-Definitionsmoeglichkeit
	 */
#define ERW_CODEPAGE


	/*  bei Big-IniTeX benoetigt jedes Trie-Element 30 Bytes (das sind bei
	 *  20000 Elemente ca. 600K allein fuer den Trie), ohne BIGTRIE belegt
	 *  es wie bei der kleinen Version nur 15 Bytes (also ca. 300K).
	 *
	 *  Sollen Patterns fuer voraussichtlich mehr als 4 Sprachen (genauer
	 *  wenn triesize > 65535 sein muss) genutzt werden, so muss BIGTRIE
	 *  definiert sein.  Dies duerfte jedoch sehr unwahrscheinlich sein.
	 */
#ifdef BIG
#  undef BIGTRIE
#endif


	/*  Layout und Allokation von eqtb[], eqlevel[] und hash[]
	 *
	 * EQTB_ALLOC	If defined, eqtb/xeqlevel/hash array will be allocated
	 *		on startup time.  This is necessary if your compiler
	 *		can't handle large arrays (e.g. maximum segment size
	 *		is restricted), but on the 68000 it will make the code
	 *		slower!!!
	 *
	 * BIG_EQTB	effects only if BIG defined.
	 *		If defined, each eqtb array entry uses 8 Bytes instead
	 *		of 6 Bytes.
	 *		It will make the code faster (8 Bytes = shift 3,
	 *		6 Bytes, shift+shift+add), but uses more memory).
	 */
#ifdef AMIGA
#  define EQTB_ALLOC
#  define BIG_EQTB
#else
   /* Der Code wird langsamer mit GCC und allokierten Arrays */
#  undef EQTB_ALLOC
   /* Der Code wird langsamer bei `undef', benoetigt aber 1/3 mehr Platz */
#  undef BIG_EQTB
#endif

/*                                            */
/*  eigene, kompatible Erweiterungen -- ENDE  */
/**********************************************/



/**********************************************/
/*     inkompatible Erweiterungen & Tests     */
/*                                            */

/* sollten alle auf undef sein, um kompatible TeX-Version zu erhalten */


	/*  eigene \tracing-Erweiterungen, dadurch wird TeX evtl. etwas
	 *  langsamer bei \if und \ifcat, jedoch nicht inkompatibel.
	 */
#define ERW_TRACING


	/* Fuehre neues Integerregister \interactionmode ein und definiere
	 * \batchmode ... \errorstopmode mit Hilfe dieses Zaehlers.
	 * (fertig)
	 */
#define ERW_INTERACTION


	/* Eigene Erweiterungen, die jedoch zur Inkompatibilitaet
	 * zu TeX fuehren!!!!  (Unfertig!)
	 */
#undef ERWEITERUNGEN

	/* Fuehre Ligatur-Pass erst nach dem kompletten Lesen aller
	 * Tokens aus.  (Unfertig!)
	 */
#undef ERW_CONSTITUTE

	/* neue \lowercase, \uppercase-Erweiterung, die erst im ``Magen''
	 * ausgefuehrt werden.   (inkompatibel zu TeX !!!!!!)
	 * Vorlaeufig:
	 *   \uccode0 = 1:   lowercase
	 *   \uccode0 = 2:   uppercase
	 *   \uccode0 sonst: normal
	 *
	 * Aenderung nur in `mainctrl.c' (fertig)
	 */
#undef ERW_SHIFTCASE

	/* `language'-Wechsel wird innerhalb einer \hbox{...} nicht
	 * neu gesetzt  (dies kann nur mit \setlanguage gemacht werden).
	 * Ausserdem gibt's Probleme bei so Dingen, wie
	 *   \setbox0=\hbox{english \setlanguage1 deutsch}
	 *   english \unhbox0\ english
	 * da hier das letzte `english' mit den Trennpatterns 1 getrennt
	 * wird (durch den language-node in Box0).
	 *
	 * Drei Aenderungen in mainctrl.c, buildbox.c, tex7.c (fertig)
	 */
	/*
	 * ACHTUNG: Aenderungen muessen fuer TeX 3.1415 (sind fuer
	 * TeX 3.141 gemacht worden!) geaendert werden, da das lhmin-
	 * und rhmin-Feld nicht mehr existieren!
	 *
	 * ==> unfertig!!!
	 */
#undef ERW_LANGUAGE

/*                                            */
/* inkompatible Erweiterungen & Tests -- Ende */
/**********************************************/

/* -- end -- */
