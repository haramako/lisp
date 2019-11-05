#include "lisp.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <linux/limits.h>
#include <signal.h>

//********************************************************
// Initialization
//********************************************************

// print backtrace
// See: http://expcodes.com/12895
// See: http://0xcc.net/blog/archives/000067.html
#ifdef WIN32
static void handler(int sig) {
}
#else
#include <execinfo.h>
static void handler(int sig) {
	void *array[10];
	size_t size;

	// get void*'s for all entries on the stack
	size = backtrace(array, 10);

	// print out all the frames to stderr
	fprintf(stderr, "\nError: signal %d:\n", sig);
	backtrace_symbols_fd(array+3, (int)size-3, 2/*=stderr*/);
	if( V_SRC_FILE ){
		if( V_SRC_FILE->stream_type == STREAM_TYPE_FILE ){
			printf( "%s:%d: error\n", STRING_BUF(V_SRC_FILE->u.file.filename), V_SRC_FILE->line );
		}else{
			printf( "(str):%d: error\n", V_SRC_FILE->line );
		}
	}
	exit(1);
}
#endif

Value NIL = NULL;
Value VALUE_T = NULL;
Value VALUE_F = NULL;
Value V_UNDEF = NULL;
Value V_EOF = NULL;
Value V_END_OF_LINE = NULL;
Stream *V_STDOUT, *V_STDIN, *V_SRC_FILE;

Value V_BEGIN;
Value V_APP;
Value V_QUOTE;
Value V_DEFINE, V_DEFINE2;
Value V_SET_I, V_SET_I2;
Value V_LAMBDA, V_MACRO, V_DEFINE_SYNTAX, V_DEFINE_SYNTAX2, V_DEFINE_SYNTAX22;
Value V_IF, V_IF2;
Value V_READ_EVAL, V_READ_EVAL2;

/*{{ declare_symbols */
Symbol *SYM_A_COMPILE_HOOK_A;
Symbol *SYM_QUASIQUOTE;
Symbol *SYM_UNQUOTE;
Symbol *SYM_UNQUOTE_SPLICING;
Symbol *SYM_CURRENT_INPUT_PORT;
Symbol *SYM_CURRENT_OUTPUT_PORT;
Symbol *SYM_END_OF_LINE;
Symbol *SYM_VALUES;
Symbol *SYM_ERROR;
Symbol *SYM_SYNTAX_RULES;
Symbol *SYM_SYNTAX_REST;
Symbol *SYM_RUNTIME_LOAD_PATH;
Symbol *SYM_RUNTIME_HOME_PATH;
Symbol *SYM_LAMBDA;
Symbol *SYM_DEFINE;
Symbol *SYM_IF;
Symbol *SYM_QUOTE;
Symbol *SYM_ELSE;
Symbol *SYM_BEGIN;
Symbol *SYM_SET_I;
Symbol *SYM_DOT;
Symbol *SYM_DOT3;
Symbol *SYM_ARROW;
Symbol *SYM_DEFINE_SYNTAX2;
/*}}*/

#define _INIT_OPERATOR(v,sym,_op) do{\
	v = gc_new(TYPE_SPECIAL);		\
	V2SPECIAL(v)->op = _op;	\
	V2SPECIAL(v)->str = sym;				\
	retain(&v);						\
	}while(0);

bool opt_trace = false;
bool opt_debug = false;

static void _get_home_path( const char *argv0, char *out_path )
{
#if WIN32
	strncpy_s(out_path, 16, ".", 16);
#else
	char cwd[PATH_MAX], path[PATH_MAX];
	getcwd( cwd, sizeof(cwd) );
	if( argv0[0] == '/' ){
		sprintf( path, "%s/..", argv0 );
	}else{
		sprintf( path, "%s/%s/..", cwd, argv0 );
	}
	//realpath( path, out_path );
	strcpy(out_path, path);
#endif
}

void init( const char *argv0 )
{
	init_prelude( argv0, false);
}

void init_prelude( const char *argv0, bool with_prelude )
{
	char home_path[PATH_MAX];
	_get_home_path( argv0, home_path );
	
	signal( SIGABRT, handler );
	signal( SIGSEGV, handler );
	
	gc_init();

	retained = NULL;
	
	NIL = gc_new(TYPE_NIL);
	retain( &NIL );
	VALUE_T = gc_new(TYPE_BOOL);
	retain( &VALUE_T );
	VALUE_F = gc_new(TYPE_BOOL);
	retain( &VALUE_F );
	V_UNDEF = gc_new(TYPE_SPECIAL);
	V2SPECIAL(V_UNDEF)->str = "#<undef>";
	retain( &V_UNDEF );
	
	bundle_cur = bundle_new( NULL );
	retain( (Value*)&bundle_cur );
	symbol_root = dict_new( hash_eqv, eqv );
	
	V_EOF = gc_new(TYPE_SPECIAL);
	V2SPECIAL(V_EOF)->str = "#<eof>";
	retain( &V_EOF );
	V_STDIN = stream_new(stdin, false, "stdin" );
	retain( (Value*)&V_STDIN );
	V_STDOUT = stream_new(stdout, false, "stdout" );
	retain( (Value*)&V_STDOUT );
	V_END_OF_LINE = (Value)string_new("\n");
	retain( &V_END_OF_LINE );
	
	_INIT_OPERATOR(V_BEGIN, "begin", OP_BEGIN);
	_INIT_OPERATOR(V_APP, "%app", OP_APP);
	_INIT_OPERATOR(V_QUOTE, "quote", OP_QUOTE);
	_INIT_OPERATOR(V_DEFINE, "define", OP_DEFINE);
	_INIT_OPERATOR(V_DEFINE2, "#<define2>", OP_DEFINE2);
	_INIT_OPERATOR(V_SET_I, "set!", OP_SET_I);
	_INIT_OPERATOR(V_SET_I2, "#<set!2>", OP_SET_I2);
	_INIT_OPERATOR(V_LAMBDA, "lambda", OP_LAMBDA);
	_INIT_OPERATOR(V_MACRO, "macro", OP_MACRO);
	//_INIT_OPERATOR(V_DEFINE_SYNTAX, "define-syntax", OP_DEFINE_SYNTAX);
	_INIT_OPERATOR(V_DEFINE_SYNTAX2, "%define-syntax", OP_DEFINE_SYNTAX2);
	_INIT_OPERATOR(V_DEFINE_SYNTAX22, "%define-syntax2", OP_DEFINE_SYNTAX22);
	_INIT_OPERATOR(V_IF, "if", OP_IF);
	_INIT_OPERATOR(V_IF2, "#<if2>", OP_IF2);

	/*{{ register_symbols */
	SYM_A_COMPILE_HOOK_A = intern("*compile-hook*");
	SYM_QUASIQUOTE = intern("quasiquote");
	SYM_UNQUOTE = intern("unquote");
	SYM_UNQUOTE_SPLICING = intern("unquote-splicing");
	SYM_CURRENT_INPUT_PORT = intern("current-input-port");
	SYM_CURRENT_OUTPUT_PORT = intern("current-output-port");
	SYM_END_OF_LINE = intern("end-of-line");
	SYM_VALUES = intern("values");
	SYM_ERROR = intern("error");
	SYM_SYNTAX_RULES = intern("syntax-rules");
	SYM_SYNTAX_REST = intern("syntax-rest");
	SYM_RUNTIME_LOAD_PATH = intern("runtime-load-path");
	SYM_RUNTIME_HOME_PATH = intern("runtime-home-path");
	SYM_LAMBDA = intern("lambda");
	SYM_DEFINE = intern("define");
	SYM_IF = intern("if");
	SYM_QUOTE = intern("quote");
	SYM_ELSE = intern("else");
	SYM_BEGIN = intern("begin");
	SYM_SET_I = intern("set!");
	SYM_DOT = intern(".");
	SYM_DOT3 = intern("...");
	SYM_ARROW = intern("=>");
	SYM_DEFINE_SYNTAX2 = intern("%define-syntax");
	/*}}*/

	bundle_define( bundle_cur, SYM_CURRENT_INPUT_PORT, (Value)V_STDIN );
	bundle_define( bundle_cur, SYM_CURRENT_OUTPUT_PORT, (Value)V_STDOUT );
	bundle_define( bundle_cur, SYM_END_OF_LINE, V_END_OF_LINE );

	cfunc_init();

	// define runtime-home-path, runtime-lib-path
	char lib_path[PATH_MAX];
	bundle_define( bundle_cur, SYM_RUNTIME_HOME_PATH, (Value)string_new(home_path) );
	sprintf( lib_path, "%s/lib", home_path );
   	bundle_define( bundle_cur, SYM_RUNTIME_LOAD_PATH,
				   cons4( (Value)string_new("."), (Value)string_new("lib"), (Value)string_new(lib_path),NIL ) );
    
	if( with_prelude ){
		char path[PATH_MAX];
		sprintf( path, "%s/prelude.scm", lib_path );
		FILE *fd = fopen( path, "r" );
		if( !fd ){
			sprintf( path, "./lib/prelude.scm" );
			fd = fopen( path, "r" );
			if( !fd ){
				printf( "cannot open prelude.scm\n" );
				exit(1);
			}
		}
		Context ctx;
		ctx.cont = NIL;
		ctx.bundle = bundle_cur;
		eval_loop( &ctx, stream_new(fd,true,"prelude.scm") );
	}
}

void finalize()
{
	bundle_cur = NULL;
	retained = NULL;
	dict_free( symbol_root );
	symbol_root = NULL;
	gc_run(opt_trace);
    gc_finalize();
}

void show_prof()
{
	printf( "alloc use: %d / %d (%3.2f%%) total: %d\n",
			prof.use, prof.size, (100.0*prof.use/prof.size), prof.alloc_count );
	printf( "cell count:\n" );
	for( int type=1; type<TYPE_MAX; type++ ){
		printf( "  %16s: %8d\n", TYPE_NAMES[type], prof.cell_count[type] );
	}
}

