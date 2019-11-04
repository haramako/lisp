#include "lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include <getopt.h>
//#include <unistd.h>

void show_help(int err)
{
	printf( "Lisp interpreter\n"
			"usage: ./mlisp <file>\n" );
	exit(err);
}

int main( int argc, char **argv )
{
	char *argv0 = argv[0];
	
	// parse command line options
	int n = 1;
	for( ; n<argc; n++ ){
		if( argv[n][0] != '-' ) break;
		if( strcmp("-h",argv[n]) == 0 || strcmp("--help",argv[n]) == 0 ){
			show_help(0);
		}else if( strcmp("-d",argv[n]) == 0 || strcmp("--debug",argv[n]) == 0 ){
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
	
	init( argv0 );
	// init_prelude(false);

	// make *argv*
	Value args = cons(NIL,NIL);
	Value tail = args;
	for( int m=n+1; m < argc; m++ ){
		CDR(tail) = cons( (Value)string_new(argv[m]), NIL );
		tail = CDR(tail);
	}
	bundle_define( bundle_cur, intern("*argv*"), CDR(args) );

	// run script
	int result = 0;
	Context ctx;
	ctx.cont = NIL;
	ctx.bundle = bundle_cur;
	if( strcmp(argv[n],"-") == 0 ){
		eval_loop( &ctx, V_STDIN );
	}else{
		FILE *f = fopen( argv[n], "r" );
		if( !f ){
			printf( "cannot open %s\n", argv[n] );
			exit(1);
		}
		Value v = eval_loop( &ctx, V_SRC_FILE = stream_new(f,true,argv[n]) );
		if( IS_INT(v) ) result = (int)V2INT(v);
	}
    
	// show_prof();

	if( opt_trace ){
		show_prof();
		finalize();
	}
	
	return result;
}


