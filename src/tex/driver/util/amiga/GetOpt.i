#ifndef     _GETOPT_I_
#define     _GETOPT_I_

#ifndef _GETOPT_H_
#  include "GetOpt.h"
#endif

/** Main functions from GetOpt.c: **/

  /**********************************************************************/
  /* GetOptShortHelp: Gibt eine kurze Hilfe aus.			*/
  /*                    helpline    Vorgegebener Zeilenanfang.		*/
  /*			linelength  Auf wieviel Zeichen pro Zeile soll	*/
  /*				    die Ausgabe formatiert werden.	*/
  /*			opt         Array der Parameterdefinitionen.	*/
  /**********************************************************************/
extern void GetOptShortHelp(char *helpline, int linelength, struct Options opt[]);



  /**********************************************************************/
  /* GetOptHelp:  Gibt fuer jede Option eine Zeile Hilfstext aus.	*/
  /*		  Mit Angabe der derzeitigen Belegung des result-Flags	*/
  /*		  als Defaultangabe.					*/
  /*			opt	Array der Parameterdefinitionen.	*/
  /**********************************************************************/
extern void GetOptHelp      (struct Options opt[]);



  /**********************************************************************/
  /* CheckOpt_Given: Wurde Option angegeben ?				*/
  /*	Die Option wird anhand des 'result' Eintrags erkannt.		*/
  /*	Ergebnis: -1: nicht gefunden, 0: nicht angegeben, sonst 1.	*/
  /**********************************************************************/
extern int CheckOpt_Given(void *which, struct Options opt[]);



  /**********************************************************************/
  /* GetOneOpt: Liefert den struct der Option zurueck.			*/
  /**********************************************************************/
extern struct Options * GetOneOpt(void *which, struct Options opt[]);



  /**********************************************************************/
  /* GetOpt:	Parameter-Parse Funktion.				*/
  /*		Liest erst die Environmentvariable aus und wertet dann	*/
  /*		argc/argv aus.						*/
  /*			argc	Direkt aus main() zu uebernehmen.	*/
  /*			argv	Direkt aus main() zu uebernehmen.	*/
  /*			envname	Name der Environmentvariable.		*/
  /*			opt	Array der Parameterdefinitionen.	*/
  /**********************************************************************/
extern BOOL GetOpt  (int argc, char *argv[], char *envname, int interaktiv,
				struct Options opt[]);



  /**********************************************************************/
  /* dimen_to_inch() returns the dimension given in the first argument	*/
  /* in inches in the second argument. It returns 0, if all is ok,	*/
  /* otherwise a value != 0.						*/
  /**********************************************************************/
extern int dimen_to_inch   (char *str, float *inch_value, int *isTrue);


#endif
