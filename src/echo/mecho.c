/*  mecho (w) and (c) 1997 by Johnny Teveﬂen <j.tevessen@line.org>
 *  All rights reserved.
 *  Distribution of these files is granted as long as the whole
 *  archive is left intact.
 *
 *  Usage: mecho [-e] String String [...]
 *
 *  `-e' causes mecho not to append a LF character (ASCII 10)
 *  to the line.
 *  Strings are separated by a single space (ASCII 32) in the
 *  output.
 *  The following escape sequences are recognized:
 *    o \n, \t, \\ etc. will output LF, TAB, backslash and so on
 *    o \34        outputs char with this decimal ASCII ord.
 *    o \x22       outputs char with this sedecimal ASCII ord.
 *    o \042       outputs char with this octal ASCII ord.
 *
 *  NOTE: This program assumes that the character set used
 *        is ISO 646 IRV (aka ASCII).
 */

#if !defined( __STDC__ )
#  error An ANSI C compliant compiler is needed.
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

static int isodigit( int ch ) {
    return( ( ch >= '0' ) && ( ch <= '7' ) );
}

int main( int argc, char* argv[] ) {
    short append_lf;
    if( argc < 1 )
	return 1;
    if( ( argc > 1 ) && !strcmp( argv[ 1 ], "-e" ) ) {
	append_lf = 0;
	--argc;
	++argv;
    } else {
	append_lf = 1;
    }
    if( argc == 1 ) {
	if( append_lf )
	    putchar( '\n' );
    } else {
	--argc;
	++argv;
	while( argc ) {
	    size_t slen = strlen( *argv );
	    char* news = (char*) malloc( slen + 1 );
	    if( news ) {
		char* oldi = *argv, *newi = news, ch;
		while( ch = *oldi++ ) {
		    if( ch == '\\' ) {
			unsigned int asc = 0;
			ch = *oldi++;
			switch( ch ) {
			    case 'n':
				*newi++ = '\n';
				break;
			    case 'r':
				*newi++ = '\r';
				break;
			    case 't':
				*newi++ = '\t';
				break;
			    case '\\':
			    case '\"':
			    case '\'':
				*newi++ = ch;
				break;
			    case 'x':
			    case 'X':
				while( ch = *oldi++, isxdigit( (unsigned char) ch ) && ( asc < 16 ) ) {
				    asc *= 16;
				    if( isdigit( (unsigned char) ch ) )
					asc += ch - '0';
				    else if( isupper( ( unsigned char) ch ) )
					asc += ch - 'A' + 10;
				    else
					asc += ch - 'a' + 10;
				}
				--oldi;
				*newi++ = asc;
				break;
			    case '0':
				while( ch = *oldi++, isodigit( (unsigned char) ch) && ( asc < 32 ) ) {
				    asc = asc * 8 + ch - '0';
				}
				--oldi;
				*newi++ = asc;
				break;
			    default:
				if( isdigit( (unsigned char) ch ) ) {
				    while( isdigit( (unsigned char) ch ) && ( asc < 26 ) ) {
					asc = asc * 10 + ch - '0';
					ch = *oldi++;
				    }
				    --oldi;
				    *newi++ = asc;
				} else {
				    --oldi;
				}
				break;
			}
		    } else {
			*newi++ = ch;
		    }
		}
		*newi = '\0';
		fputs( news, stdout );
		free( news );
	    }
	    --argc;
	    ++argv;
	    if( argc )
		putchar( ' ' );
	}
	if( append_lf )
	    putchar( '\n' );
    }
    return( EXIT_SUCCESS );
}

