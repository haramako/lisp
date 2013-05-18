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
	char buf[8192];
	size_t len = fread( buf, 1, sizeof(buf), f );
	buf[len] = '\0';

	Value src = parse_list(buf,filename);
	src = compile( src );
	Value r = eval_loop( src );
	return r;
}

int main( int argc, char **argv )
{
	init();

	if( argc <= 1 ){
		printf( "usage: ./mlisp file\n" );
		return 0;
	}

	run( "prelude.lisp" );
	run( "prelude2.lisp" );
	run( "prelude3.lisp" );
	run( argv[1] );
	
	gc();
	
	bundle_cur = NULL;
	finalize();
	gc();

	// display_val( "retained: ", retained );
	
	return 0;
}


