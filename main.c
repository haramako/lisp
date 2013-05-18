#include "lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Value run( char *filename )
{
	// printf( "running %s\n", filename);
	
	FILE *f = fopen( filename, "r" );
	if( !f ){
		printf( "cannot open %s\n", filename );
		exit(1);
	}

	return eval_loop( stream_new(f,true,filename) );
}

int main( int argc, char **argv )
{
	init();

	if( argc <= 1 ){
		printf( "usage: ./mlisp file\n" );
		return 0;
	}

	run( "prelude.lisp" );
	run( argv[1] );

	/*
	gc();
	
	bundle_cur = NULL;
	finalize();
	gc();
	*/

	// display_val( "retained: ", retained );
	
	return 0;
}


