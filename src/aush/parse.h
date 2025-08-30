/*
* This file is part of AUSH.
* Copyright (C) 1994 Denis Gounelle
* 
* AUSH is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* AUSH is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with AUSH.  If not, see <http://www.gnu.org/licenses/>.
*
*/
/***************************************************************************
 *
 * AUSH (c)1992 par Denis GOUNELLE
 *
 ***************************************************************************/

/* gestion de la pile des états */

#define MAXSTACK 32

struct FullStatus
{
  char fs_etat ;
  char fs_mot[MAXLINE+1] ;
  char fs_tmp[MAXLINE+1] ;
} ;

/* définition des positions des caractères spéciaux */

#define CHR_ALONE  20
#define CHR_ALPHA  CHR_ALONE
#define CHR_DIGIT (CHR_ALONE+1)
#define CHR_SPACE (CHR_ALONE+2)
#define CHR_OTHER (CHR_ALONE+3)
#define NBCHARS   (CHR_ALONE+4)

#ifdef _AMIGA
static char ListeMotif[] = "~*#?[]()|" ;
#endif
#ifdef unix
static char ListeMotif[] = "*?[]" ;
#endif

static char ListeChar[CHR_ALONE+1] = ";^§${}!`'\"|,><&@_:/-" ;

/* définition des états de l'automate */

#define E_POP	  -2
#define E_CTR1	   0
#define E_DEB	   1
#define E_REP1	   2
#define E_REP2	   3
#define E_VAR1	   4
#define E_VAR2	   5
#define E_VAR3	   6
#define E_VAR4	   7
#define E_VAR5	   8
#define E_NOR1	   9
#define E_EXP1	   10
#define E_HIS1	   11
#define E_SUB1	   12
#define E_STR1	   13
#define E_STR2	   14
#define E_RED1	   15
#define E_RED2	   16
#define E_RED3	   17
#define E_BGD1	   18
#define E_BGD2	   19
#define E_CHR1	   20

/* définition de l'automate */

#define NBETATS    21

static char Automate[NBETATS][NBCHARS] =
{
/*		     ;	     ^	     §	     $	     {	     }	     !	     `       '       "       |       ,       >       <       &       @       _       :       /       -   alpha   digit   space   autre     */
/* E_CTR1 */  {     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_CTR1,     -1,     -1,     -1 } ,
/* E_DEB  */  {     -1, E_CTR1, E_REP1, E_VAR1, E_EXP1,     -1, E_HIS1, E_SUB1, E_STR1, E_STR2, E_CHR1, E_CHR1, E_RED1, E_RED3, E_BGD1, E_BGD1, E_NOR1, E_NOR1, E_NOR1, E_NOR1, E_NOR1, E_NOR1,     -1, E_NOR1 } ,
/* E_REP1 */  {     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_CHR1, E_CHR1,     -1,     -1,     -1,     -1,     -1,     -1, E_REP2,     -1,     -1, E_REP1,     -1,     -1 } ,
/* E_REP2 */  {     -1,     -1,     -1, E_VAR1, E_EXP1,     -1,     -1,     -1,     -1,     -1, E_CHR1, E_CHR1,     -1,     -1,     -1,     -1, E_REP2,     -1, E_REP2,     -1, E_REP2, E_REP2,     -1, E_REP2 } ,
/* E_VAR1 */  {     -1,     -1,     -1,     -1, E_VAR2,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_VAR4,     -1,     -1,     -1, E_VAR4, E_VAR4,     -1,     -1 } ,
/* E_VAR2 */  {     -1,     -1,     -1,     -1,     -1,  E_POP,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_VAR2, E_VAR3,     -1,     -1, E_VAR2, E_VAR2,     -1,     -1 } ,
/* E_VAR3 */  {     -1,     -1,     -1,     -1,     -1,  E_POP,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_VAR3,     -1,     -1,     -1 } ,
/* E_VAR4 */  {     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_CHR1, E_CHR1,     -1,     -1,     -1,     -1, E_VAR4, E_VAR5,     -1,     -1, E_VAR4, E_VAR4,     -1,     -1 } ,
/* E_VAR5 */  {     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_CHR1, E_CHR1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_VAR5,     -1,     -1,     -1 } ,
/* E_NOR1 */  {     -1,     -1,     -1, E_VAR1, E_EXP1,     -1,     -1,     -1,     -1,     -1, E_CHR1, E_CHR1,     -1,     -1,     -1,     -1, E_NOR1, E_NOR1, E_NOR1, E_NOR1, E_NOR1, E_NOR1,     -1, E_NOR1 } ,
/* E_EXP1 */  {     -1,     -1,     -1, E_VAR1,     -1,  E_POP,     -1,     -1,     -1,     -1, E_EXP1,     -1, E_EXP1, E_EXP1, E_EXP1,     -1,     -1,     -1,     -1, E_EXP1, E_EXP1, E_EXP1, E_EXP1, E_EXP1 } ,
/* E_HIS1 */  {     -1,     -1, E_HIS1, E_HIS1, E_HIS1, E_HIS1, E_HIS1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_HIS1, E_HIS1, E_HIS1, E_HIS1, E_HIS1, E_HIS1,     -1, E_HIS1 } ,
/* E_SUB1 */  {     -1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1,  E_POP, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1, E_SUB1 } ,
/* E_STR1 */  { E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1,  E_POP, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1, E_STR1 } ,
/* E_STR2 */  { E_STR2, E_CTR1, E_REP1, E_VAR1, E_EXP1,     -1, E_HIS1,     -1, E_STR2,  E_POP, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2, E_STR2 } ,
/* E_RED1 */  {     -1,     -1,     -1, E_RED2,     -1,     -1,     -1,     -1, E_RED2, E_RED2,     -1,     -1, E_RED3,     -1,     -1,     -1, E_RED2, E_RED2, E_RED2, E_RED2, E_RED2, E_RED2, E_RED3, E_RED2 } ,
/* E_RED2 */  {     -1,     -1,     -1, E_VAR1,     -1,     -1,     -1,     -1, E_STR1, E_STR2, E_CHR1, E_CHR1,     -1,     -1,     -1,     -1, E_RED2, E_RED2, E_RED2, E_RED2, E_RED2, E_RED2,     -1, E_RED2 } ,
/* E_RED3 */  {     -1,     -1,     -1, E_RED2,     -1,     -1,     -1,     -1, E_RED2, E_RED2,     -1,     -1,     -1,     -1,     -1,     -1, E_RED2, E_RED2, E_RED2, E_RED2, E_RED2, E_RED2, E_RED3, E_RED2 } ,
/* E_BGD1 */  {     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_CHR1, E_CHR1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_BGD2,     -1, E_BGD2,     -1,     -1 } ,
/* E_BGD2 */  {     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_CHR1, E_CHR1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1, E_BGD2,     -1,     -1 } ,
/* E_CHR1 */  {     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1,     -1 } ,
} ;

/* flags pour savoir si on doit empiler l'état courant suivant le nouvel état */

static char PushFlg[NBETATS] =
{
  -1,  0, -1,  0, -1,  0,  0,  0,  0, -1, -1, -1,  0,  0,  0,  0,  0,  0,  0,  0
} ;

/* flags pour savoir si on doit faire une substitution en quittant l'état */

static char SubstFlg[NBETATS] =
{
  -1,  0, -1,  0,  0,  0, -1,  0, -1, -1, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0
} ;

/* définition des bits de status */

#define ST_ERROR	0x0001	/* erreur pendant l'analyse */
#define ST_FESC 	0x0002	/* premier caractère précédé par '\' */
#define ST_END		0x0004	/* fin de la ligne */
#define ST_SUBST	0x0008	/* substitution de cmd */
#define ST_DQUOT	0x0010	/* "chaine" */
#define ST_SQUOT	0x0020	/* 'chaine' */
#define ST_MOTIF	0x0040	/* le mot contient un motif */
#define ST_SPLIT	0x0080	/* "casse" la chaine en sortie */
#define ST_NOEXPAND	0x0100	/* aucune extension */
#define ST_ALIAS	0x0200	/* commande "alias" */

