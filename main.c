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
		}else if( strcmp("--trace",argv[n]) == 0 ){
			opt_trace = true;
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

	if( n >= argc ) show_help(0);
	
	init();
	// init_prelude(false);

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
		show_prof();
		gc_run(1);
		bundle_cur = NULL;
		finalize();
		gc_run(1);
		// display_val( "retained: ", retained );
	}
	
	return 0;
}


