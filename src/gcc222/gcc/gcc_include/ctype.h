/*	char type macros for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 		*/

#ifndef	_CTYPE_H
#define _CTYPE_H

/* This file defines some macros for various ASCII character manipulation:
	isalpha(c)	~0 if c is alpha
	isupper(c)	~0 if c is upper case
	islower(c)	~0 if c is lower case
	isdigit(c)	~0 if c is a digit (0 to 9)
	isxdigit(c)	~0 if c is a hexadecimal digit (0 to 9, A to F, a to f)
	isspace(c)	~0 if c is white space
	ispunct(c)	~0 if c is punctuation
	isalnum(c)	~0 if c is alpha or digit
	isprint(c)	~0 if c is printable (including blank)
	isgraph(c)	~0 if c is graphic (excluding blank)
	iscntrl(c)	~0 if c is control character
	isascii(c)	~0 if c is ASCII
	iscsym(c)	~0 if valid character for C symbols
	iscsymf(c)	~0 if valid first character for C symbols
	tolower(c)	return same lowercase char
	toupper(c)	return same uppercase char
	toascii(c)	return ASCII char code
*/

#ifdef	__cplusplus

static const int _U = 1;
static const int _L = 2;
static const int _N = 4;
static const int _S = 8;
static const int _P = 16;
static const int _C = 32;
static const int _B = 64;
static const int _X = 128;

extern "C" {
extern	char	_ctype[];
}

#else	/* not __cplusplus	*/

#define _U	1	/* upper case flag */
#define _L	2	/* lower case flag */
#define _N	4	/* number flag */
#define _S	8	/* space flag */
#define _P	16	/* punctuation flag */
#define _C	32	/* control character flag */
#define _B	64	/* blank flag */
#define _X	128	/* hexadecimal flag */

extern	char	_ctype[];	/* table used for char types */

#endif	/* not __cplusplus	*/

inline static const int isalpha(char c)  { return ((_ctype+1)[c]&(_U|_L)); }
inline static const int isupper(char c)  { return ((_ctype+1)[c]&_U); }
inline static const int islower(char c)  { return ((_ctype+1)[c]&_L); }
inline static const int isdigit(char c)  { return ((_ctype+1)[c]&_N); }
inline static const int isxdigit(char c) { return ((_ctype+1)[c]&_X); }
inline static const int isspace(char c)  { return ((_ctype+1)[c]&_S); }
inline static const int ispunct(char c)  { return ((_ctype+1)[c]&_P); }
inline static const int isalnum(char c)  { return ((_ctype+1)[c]&(_U|_L|_N)); }
inline static const int isprint(char c)  { return ((_ctype+1)[c]&(_P|_U|_L|_N|_B)); }
inline static const int isgraph(char c)  { return ((_ctype+1)[c]&(_P|_U|_L|_N)); }
inline static const int iscntrl(char c)  { return ((_ctype+1)[c]&_C); }
inline static const int isascii(char c)  { return ((unsigned)(c)<=0177); }
inline static const int toupper(char c)  { return islower(c)? (c-'a'+'A') : c; }
inline static const int tolower(char c)  { return isupper(c)? (c-'A'+'a') : c; }
inline static const int toascii(char c)  { return ((c)&0177); }
inline static const int iscsym(char c)   { return ((_ctype+1)[c] & (_N|_U|_L)) || c=='_'; }
inline static const int iscsymf(char c)  { return ((_ctype+1)[c] & (_U|_L)) || c=='_'; }

#undef	_ctype

#endif	/* _CTYPE_H */
