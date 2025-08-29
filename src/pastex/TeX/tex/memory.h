/* memory.h: types that are too hard to translate automatically from
   Pascal.  This file is included from the change file in the change for
   section 8.113.  */

/* (br) added:
 *
 * Ein paar neue Typen fuer BIG-TeX, die nicht (SMALL) oder nur etwas
 * groesser (MED) werden, wenn halfword doppelt so gross wird.
 */

typedef unsigned short SMALLhalfword;		/* fuer fontparams */


typedef union
{
  struct
  {
    halfword RH, LH;
  } v;

  struct
  {
    halfword junk_space;	/* Make B0,B1 overlap LH.  */
    quarterword B0, B1;
  } u;

  struct
  {
    halfword junk_space;	/* Make B0,B1 overlap this LH also.  */
    SMALLhalfword LH;		/* needed for "set_type_subtype" macro (br) */
  } w;

} twohalves;


/* TeX want to call the following fields `b0', etc.  */
#define	b0	u.B0
#define	b1	u.B1
#define	b2	u.B2
#define	b3	u.B3

typedef union /* struct */
{
  struct
  {
    quarterword B0, B1, B2, B3;
  } u;
  integer cint;
  
} fourquarters;


typedef union
{
  integer cint;
  glueratio gr;
  twohalves hh;
  fourquarters qqqq;
} memoryword;


/* new strutures/unions: */

typedef union
{
  integer cint;
/*  glueratio gr; */	/* wegen typedef double glueratio */
/*  twohalves hh; */	/* beim BIG-TeX sind das 2*4=8 Bytes */
  fourquarters qqqq;
} SMALLmemoryword;				/* fuer fontinfo */


/* Der folgende struct ist 6 Bytes gross.  GNU C berechnet aus dem
 * Index d0 mit  "movel d0,d1; asll #1,d1; addl d0,d1; asll #1,d1"
 * den Offset d1.   Dies benoetigt 8#mem und 32 Zyklen.
 * Manche Compiler erzeugen daraus jedoch "movel d0,d1; mulu #6,d1"
 * Dies benoetigt 6#mem und <78 Zyklen !!!!!
 * Waere der struct 8 Byte gross: "movel d0,d1; asll #3,d1"
 * Dies benoetigt 4#mem und 18 Zyklen.
 */

#ifndef BIG_EQTB

typedef union
{
  struct
  {
    halfword RH;
    quarterword B0, B1;
  } v;
  struct
  {
    halfword RH;
    quarterword B0, B1;
  } u;
} onehalf_halves;

typedef union
{
  integer cint;
  onehalf_halves hh;
  fourquarters qqqq;	/* for ML-TeX: rh overlaps b0, b1 !! */
} MEDmemoryword;			/* fuer savestack, eqtb */

#else

typedef memoryword MEDmemoryword;

#endif

/* end of memory.h */
