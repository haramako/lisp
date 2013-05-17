#include "lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Value src;

int main( int argc, char **argv )
{
	init();

	if( argc <= 1 ){
		printf( "usage: ./mlisp file\n" );
		return 0;
	}
	
	char *filename = argv[1];
	FILE *f = fopen( "prelude.lisp", "r" );
	if( !f ){
		printf( "cannot open %s\n", filename );
		exit(1);
	}
	char buf[8192];
	size_t len = fread( buf, 1, sizeof(buf), f );
	buf[len] = '\0';

	Value src = parse_list(buf);
	eval_loop( src );
	gc();


	f = fopen( filename, "r" );
	if( !f ){
		printf( "cannot open %s\n", filename );
		exit(1);
	}
	len = fread( buf, 1, sizeof(buf), f );
	buf[len] = '\0';
	
	src = parse_list(buf);
	display_val( "src: ", src );
	src = compile( src );
	eval_loop(src);

	gc();
	
	bundle_cur = NULL;
	gc();

	display_val( "retained: ", retained );
	
	return 0;
}


