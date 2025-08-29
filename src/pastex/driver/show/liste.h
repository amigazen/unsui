/**********************************************************************/
/*                                                                    */
/* liste.h  : Modul zum Verwalten doppeltverketteter Listen           */
/*            mit Hilfe eines Listenkopfes und eines Cursors.         */
/*            15.07.88  Georg Hessmann                                */
/*                                                                    */
/**********************************************************************/


/* head : Listenkopf;
          in first ist ein Zeiger auf den Anfang und
          in last ein Zeiger auf das Ende der Liste.
          cursor ist ein Zeiger auf ein beliebiges Listenelement,
          von dem aus verschiedene Operationen durchgefuehrt werden,
          wie gehe ein Element vor od. zurueck und liefere dessen
          Werte.
*/

struct head {
                 struct liste *cursor;
                 struct liste *first, *last;
                 struct liste *first_phy, *last_phy;
            };

struct liste {
                 long pagenr;			/* logic number */
                 long pageptr;			/* file pointer */
                 long phy_nr;			/* physical number */
                 long secundary_nr;		/* die wievielte Seite mit pagenr? */
                 struct liste *prev, *next;
                 struct liste *next_phy;
             };

