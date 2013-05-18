#include "lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

void show_help(int err)
{
	printf( "Lisp interpreter\n"
			"usage: ./mlisp <file>\n" );
	exit(err);
}

void gc_show_stat()
{
	printf( "size: %d / %d (%3.2f%%) total: %d\n",
			gc_stat.use, gc_stat.size, (100.0*gc_stat.use/gc_stat.size), gc_stat.alloc_count );
}

int main( int argc, char **argv )
{
	// parse command line options
	int opt_debug = 0;
	int n = 1;
	for( ; n<argc; n++ ){
		if( argv[n][0] != '-' ) break;
		if( strcmp("-h",argv[n]) == 0 || strcmp("--help",argv[n]) == 0 ){
			show_help(0);
		}else if( strcmp("-g",argv[n]) == 0 || strcmp("--gc-debug",argv[n]) == 0 ){
			opt_debug = 1;
		}else if( strcmp("-",argv[n]) == 0 ){
			break;
		}else if( strcmp("--",argv[n]) == 0 ){
			n++;
			break;
		}else{
			printf( "unknown option: %s\n", argv[n] );
			show_help(1);
		}
	}

	init();

	// make *argv*
	Value args = cons(NIL,NIL);
	Value tail = args;
	for( int m=n+1; m < argc; m++ ){
		CDR(tail) = cons( string_new(argv[m]), NIL );
		tail = CDR(tail);
	}
	bundle_define( bundle_cur, intern("*argv*"), CDR(args) );


	// run script
	if( strcmp(argv[n],"-") == 0 ){
		eval_loop( V_STDIN );
	}else{
		FILE *f = fopen( argv[n], "r" );
		if( !f ){
			printf( "cannot open %s\n", argv[n] );
			exit(1);
		}
		eval_loop( stream_new(f,true,argv[n]) );
	}

	if( opt_debug ){
		gc_show_stat();
		gc();
		bundle_cur = NULL;
		finalize();
		gc();
		// display_val( "retained: ", retained );
	}
	
	return 0;
}


