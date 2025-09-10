/* This is a _very_ simple rm command.
 * Options [-] will be just ignored.
 */

#include <stdio.h>

int main( int argc, char *argv[] )
{
	int i;
	
	for( i=1; i<argc; i++ )
	{
		if( argv[i][0] != '-' )
			remove( argv[i] );
	}
}
