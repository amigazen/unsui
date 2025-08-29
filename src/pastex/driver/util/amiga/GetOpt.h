#ifndef     _GETOPT_H_
#define     _GETOPT_H_


/** GetOpt.h     (c) 30-may-91 Georg Hessmann **/

#ifndef BOOL
#  define BOOL short
#endif
#ifndef FALSE
#  define FALSE 0
#endif
#ifndef TRUE
#  define TRUE (!FALSE)
#endif


struct Options {
   char      *name;		/* Name der Option				*/
   char      *abbrev;		/* Abkuerzung des Optionsnamen			*/
   long       msg_hlp_txt;	/* Locale Nummer für den Hilfetext		*/
   char      *hlp_txt;		/* Text fuer die Fehlerausgabe; Help-Funktion	*/
   long       msg_hlp_txt2;	/* Locale-Nummer für den langen Hilfetext	*/
   char      *hlp_txt2;		/* Text fuer die lange Help-Funktion		*/
   void      *result;		/* Dies ist ein Pointer auf das Ergebnis	*/
   unsigned   len       : 8,	/* Laenge des Optionsnamen			*/
              alen      : 8;	/* Laenge der Abkuerzung			*/
   unsigned   type      : 8,	/* Typ der Variable, siehe #define's unten	*/
              required  : 1,	/* muss die Option angegeben werden?		*/
              is_given  : 1,	/* wurde die Option angegeben			*/
              special   : 1,	/* special Bit fuer TEX Option			*/
              hidden    : 1;	/* versteckte Option?				*/
   };


#define START_PARAM(var)	struct Options var[] = {
#define REQ_PARAM(name,ab,type,res,txt,txt2)		\
			{name, ab, txt, NULL, txt2, NULL, res, 0, 0, type, OPT_REQUIRED, 0, 0, OPT_NORMAL},
#define NOREQ_PARAM(name,ab,type,res,txt,txt2)		\
			{name, ab, txt, NULL, txt2, NULL, res, 0, 0, type, OPT_NOT_REQUIRED, 0, 0, OPT_NORMAL},
#define HIDDEN_PARAM(name,ab,type,res,txt,txt2)		\
			{name, ab, txt, NULL, txt2, NULL, res, 0, 0, type, OPT_NOT_REQUIRED, 0, 0, OPT_HIDDEN},
#define END_PARAM					\
			{ NULL, NULL, 0, NULL, 0, NULL, NULL, 0, 0, 0, 0, 0, 0, 0} };



#define OPT_LAST_ENTRY		0	/* only for internal use! */

#define OPT_BOOLEAN		1	/* set boolean */
#define OPT_STRING		2
#define OPT_LONG		3
#define OPT_FLOAT		4
#define OPT_HELP		5	/* Special Help-Option */
#define OPT_OPTIONSTRING	6	/* String ohne Keyword */

#ifdef TEX
# define OPT_TEX_DIM		15	/* only for ShowDVI/DVIprint */

struct PSizes {
  char * name;
  float  width;		// in inch
  float  height;	// in inch
};

extern struct PSizes psizes[];

#endif


#define OPT_NOT_REQUIRED	0
#define OPT_REQUIRED		1

#define OPT_NORMAL		0	/* normal Option */
#define OPT_HIDDEN		1	/* don't show this option */


#endif
