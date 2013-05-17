#include "lisp.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include "gc.h"

//********************************************************
// Garbage collection
//********************************************************

static void _Cell_mark( void *p )
{
	// display_val( "mark: ", (Value)p );
	Value v = (Cell*)p;
	switch( TYPE_OF(v) ){
	case TYPE_NIL:
	case TYPE_INT:
	case TYPE_BOOL:
		break;
	case TYPE_SYMBOL:
		gc_mark( SYMBOL_NEXT(v) );
		break;
	case TYPE_SPECIAL:
		break;
	case TYPE_PAIR:
		gc_mark( CAR(v) );
		gc_mark( CDR(v) );
		break;
	case TYPE_SLOT:
		gc_mark( SLOT_SYM(v) );
		gc_mark( SLOT_VAL(v) );
		gc_mark( SLOT_NEXT(v) );
		break;
	case TYPE_LAMBDA:
		gc_mark( LAMBDA_ARGS(v) );
		gc_mark( LAMBDA_BODY(v) );
		gc_mark( LAMBDA_BUNDLE(v) );
		break;
	case TYPE_BUNDLE:
		gc_mark( BUNDLE_SLOT(v) );
		gc_mark( BUNDLE_UPPER(v) );
		break;
	case TYPE_CONTINUATION:
		gc_mark( CONTINUATION_BUNDLE(v) );
		gc_mark( CONTINUATION_CODE(v) );
		gc_mark( CONTINUATION_NEXT(v) );
		break;
	}
}

static void _free( void *p )
{
	// printf( "free: %p\n", p );
}

gc_vtbl Cell_vtbl = { _Cell_mark, _free };

Value retained = NULL;

static void _mark_root()
{
	gc_mark( (void*)retained );
	gc_mark( (void*)bundle_cur );
}

Value retain( Value v ){
	retained = cons( v, retained ); return v;
}

Value release( Value v )
{
	assert( V_IS_PAIR(v) );
	for( Value *cur=&retained; *cur != NIL; cur = &CDR(*cur) ){
		if( CAR(*cur) == v ){
			if( CDR(*cur) ){
				CAR(*cur) = CADR(*cur);
				CDR(*cur) = CDDR(*cur);
			}else{
				*cur = NIL;
			}
			break;
		}
	}
	return v;
}

void gc()
{
	gc_run();
}

//********************************************************
// Utility
//********************************************************

Value NIL = NULL;
Value VALUE_T = NULL;
Value VALUE_F = NULL;
Value SYM_A_DEBUG_A = NULL;
Value SYM_A_COMPILE_HOOK_A = NULL;
Value SYM_QUASIQUOTE = NULL;
Value SYM_UNQUOTE = NULL;

Value cell_new( Type type )
{
	Cell *cell = GC_MALLOC(Cell);
	assert(cell);
	cell->type = type;
	return cell;
}

Value int_new( int i )
{
	Cell *cell = GC_MALLOC(Cell);
	cell->type = TYPE_INT;
	cell->d.number = i;
	return cell;
}

Value cons( Value car, Value cdr )
{
	Value v = cell_new(TYPE_PAIR);
	CAR(v) = car;
	CDR(v) = cdr;
	return v;
}

size_t value_length( Value v )
{
	assert( V_IS_PAIR(v) );
	size_t len = 0;
	for( Value cur=v; cur != NIL; cur = CDR(cur) ) len++;
	return len;
}

size_t value_to_str( char *buf, Value v )
{
	char *orig_buf = buf;
	// printf( "%d\n", TYPE_OF(v) );
	switch( TYPE_OF(v) ){
	case TYPE_NIL:
		buf += sprintf( buf, "()" );
		break;
	case TYPE_BOOL:
		buf += sprintf( buf, (v==VALUE_T)?"#t":"#f" );
		break;
	case TYPE_INT:
		buf += sprintf( buf, "%lld", V2INT(v) );
		break;
	case TYPE_SYMBOL:
		buf += sprintf( buf, "%s", SYMBOL_STR(v) );
		break;
	case TYPE_SLOT:
		buf += sprintf( buf, "(SLOT:%p)", v );
		break;
	case TYPE_LAMBDA:
		{
			switch( LAMBDA_KIND(v) ){
			case LAMBDA_TYPE_LAMBDA:
				buf += sprintf( buf, "(LAMBDA:%p)", V2LAMBDA(v) );
				break;
			case LAMBDA_TYPE_MACRO:
				buf += sprintf( buf, "(MACRO:%p)", V2LAMBDA(v) );
				break;
			case LAMBDA_TYPE_CFUNC:
				buf += sprintf( buf, "(CFUNCTION:%p)", V2LAMBDA(v) );
				break;
			case LAMBDA_TYPE_CMACRO:
				buf += sprintf( buf, "(CMACRO:%p)", V2LAMBDA(v) );
				break;
			default:
				assert(0);
			}
		}
		break;
	case TYPE_PAIR:
		buf += sprintf( buf, "(" );
		bool finished = false;
		while( !finished ){
			switch( TYPE_OF(CDR(v)) ){
			case TYPE_NIL:
				buf += value_to_str( buf, CAR(v) );
				finished = true;
				break;
			case TYPE_PAIR:
				buf += value_to_str( buf, CAR(v) );
				buf += sprintf( buf, " " );
				break;
			default:
				buf += value_to_str( buf, CAR(v) );
				buf += sprintf( buf, " . " );
				buf += value_to_str( buf, CDR(v) );
				finished = true;
				break;
			}
			v = CDR(v);
		}
		buf += sprintf( buf, ")" );
		break;
	case TYPE_BUNDLE:
		buf += sprintf( buf, "(BUNDLE:%p)", V2BUNDLE(v) );
		break;
	case TYPE_CONTINUATION:
		buf += sprintf( buf, "(CONTINUATION:%p)", V2CONTINUATION(v) );
		break;
	case TYPE_SPECIAL:
		buf += sprintf( buf, "%s", SPECIAL_STR(v) );
		break;
	}
	return buf - orig_buf;
}

Value lambda_new()
{
	Value v = cell_new(TYPE_LAMBDA);
	LAMBDA_ARGS(v) = NIL;
	LAMBDA_BODY(v) = NIL;
	LAMBDA_BUNDLE(v) = NIL;
	LAMBDA_FUNC(v) = NULL;
	return v;
}

//********************************************************
// Special forms
//********************************************************

Value V_BEGIN;
Value V_CALL0;
Value V_CALL1;
Value V_QUOTE;
Value V_DEFINE, V_DEFINE2;
Value V_SET_I, V_SET_I2;
Value V_LET, V_LET2, V_LET3;
Value V_LAMBDA, V_MACRO, V_EXEC_MACRO;
Value V_IF, V_IF2;

Value _operator( char *sym, Operator op ){
	Value v = cell_new(TYPE_SPECIAL);
	SPECIAL_OP(v) = op;
	SPECIAL_STR(v) = sym;
	bundle_define( bundle_cur, intern(sym), v );
	return retain(v);
}

static void _special_init()
{
	V_BEGIN = _operator("begin", OP_BEGIN);
	V_CALL0 = _operator("*call0*", OP_CALL0);
	V_CALL1 = _operator("*call1*", OP_CALL1);
	V_QUOTE = _operator("quote", OP_QUOTE);
	V_DEFINE = _operator("define", OP_DEFINE);
	V_DEFINE2 = _operator("*define2*", OP_DEFINE2);
	V_SET_I = _operator("set!", OP_SET_I);
	V_SET_I2 = _operator("*set!2*", OP_SET_I2);
	V_LET = _operator("let", OP_LET);
	V_LET2 = _operator("*let2*", OP_LET2);
	V_LET3 = _operator("*let3*", OP_LET3);
	V_LAMBDA = _operator("lambda", OP_LAMBDA);
	V_MACRO = _operator("macro", OP_MACRO);
	V_EXEC_MACRO = _operator("*exec-macro*", OP_EXEC_MACRO);
	V_IF = _operator("if", OP_IF);
	V_IF2 = _operator("*if2*", OP_IF2);
}

//********************************************************
// Symbol
//********************************************************

static Value _symbol_root = NULL;
// static int _gensym_cur = 0;

Value intern( const char *sym )
{
	for( Value cur = _symbol_root; cur != NIL; cur = SYMBOL_NEXT(cur) ){
		if( strcmp( SYMBOL_STR(cur), sym ) == 0 ) return cur;
	}
	// not found, create new atom
	Value v = cell_new(TYPE_SYMBOL);
	SYMBOL_STR(v) = malloc( strlen(sym)+1 );
	assert( SYMBOL_STR(v) );
	strcpy( SYMBOL_STR(v), sym );
	SYMBOL_NEXT(v) = _symbol_root;
	_symbol_root = v;
	return v;
}

//********************************************************
// Bundle and Slot
//********************************************************

Value bundle_cur = NULL;

Value bundle_new( Value upper )
{
	Value v = cell_new(TYPE_BUNDLE);
	BUNDLE_SLOT(v) = NIL;
	BUNDLE_UPPER(v) = upper;
	return v;
}

Value bundle_find_slot( Value b, Value sym, bool find_upper )
{
	for( Value cur = BUNDLE_SLOT(b); cur != NIL; cur = SLOT_NEXT(cur) ){
		if( SLOT_SYM(cur) == sym ) return cur;
	}
	if( find_upper && BUNDLE_UPPER(b) != NIL ) return bundle_find_slot( BUNDLE_UPPER(b), sym, find_upper );
	return NULL;
}

bool bundle_set( Value b, Value sym, Value v )
{
	Value slot = bundle_find_slot( b, sym, true );
	assert( slot );
	SLOT_VAL(slot) = v;
	return true;
}

void bundle_define( Value b, Value sym, Value v )
{
	assert( !bundle_find_slot( b, sym, false ) );
	// not found, create new entry
	Value slot = cell_new(TYPE_SLOT);
	assert( slot );
	SLOT_SYM(slot) = sym;
	SLOT_VAL(slot) = v;
	SLOT_NEXT(slot) = BUNDLE_SLOT(b);
	BUNDLE_SLOT(b) = slot;
}

bool bundle_find( Value b, Value sym, Value *result )
{
	Value slot = bundle_find_slot( b, sym, true );
	if( slot ){
		if( result ) *result = SLOT_VAL(slot);
		return true;
	}else{
		return false;
	}
}

Value bundle_get( Value b, Value sym )
{
	Value slot = bundle_find_slot( b, sym, true );
	if( slot ){
		return SLOT_VAL(slot);
	}else{
		return NIL;
	}
}

//********************************************************
// Continuation
//********************************************************

Value continuation_new( Value code, Value bundle, Value next )
{
	Value v = cell_new( TYPE_CONTINUATION );
	CONTINUATION_CODE(v) = code;
	CONTINUATION_BUNDLE(v) = bundle;
	CONTINUATION_NEXT(v) = next;
	return v;
}

//********************************************************
// Parsing
//********************************************************

typedef struct {
	char *src;
	char *cur;
	int line;
} ParseState;

int _parse( ParseState *state, Value *result );

void _skip_space( ParseState *state )
{
	char c;
	while( (c = *state->cur) ){
		switch( c ){
		case ' ': case '\t': case '\n':
			break;
		case ';':
			while( *state->cur != '\n' && *state->cur != '\0' ) state->cur++;
			break;
		default:
			return;
		}
		state->cur++;
	}
}


int _parse_list( ParseState *state, Value *result )
{
	Value val;
	_skip_space( state );
	switch( *state->cur ){
	case ')':
	case '\0':
		*result = NIL;
		return 0;
	default:
		{
			Value cdr;
			int err = _parse( state, &val );
			if( err ) return err;
			err = _parse_list( state, &cdr );
			if( err ) return err;
			*result = cons( val, cdr );
			return 0;
		}
	}
}

static inline bool _is_val_char( char c )
{
	switch( c ){
	case ' ': case '\t': case '\n': case '(': case ')': case '\0':
		return false;
	default:
		return true;
	}
}

static Value _parse_token( const char *start, const char *end )
{
	if( isnumber(start[0]) || ( start[0] == '-' && isnumber(start[1]) ) ){
		return INT2V(atoi(start));
	}else{
		char tmp[32];
		memcpy( tmp, start, end-start );
		tmp[end-start] = '\0';
		return intern( tmp );
	}
}

int _parse( ParseState *state, Value *result )
{
	int err;
	_skip_space( state );
	switch( *state->cur ){
	case '\0':
		return 0;
	case '(':
		state->cur++;
		err = _parse_list( state, result );
		if( err ) return err;
		assert( *state->cur == ')' );
		state->cur++;
		return err;
	case ')':
		assert(!"paren not matched");
	case '#':
		state->cur++;
		if( *state->cur == 't' ){
			state->cur++;
			*result = VALUE_T;
			return 0;
		}else if( *state->cur == 'f' ){
			state->cur++;
			*result = VALUE_F;
			return 0;
		}else{
			assert(0);
		}
	case '\'':
		state->cur++;
		err = _parse( state, result );
		if( err ) return err;
		*result = cons( V_QUOTE, cons(*result,NIL) );
		return err;
	case '`':
		state->cur++;
		err = _parse( state, result );
		if( err ) return err;
		*result = cons( SYM_QUASIQUOTE, cons(*result,NIL) );
		return err;
	case ',':
		state->cur++;
		err = _parse( state, result );
		if( err ) return err;
		*result = cons( SYM_UNQUOTE, cons(*result,NIL) );
		return err;
	default:
		{
			char *end = state->cur+1;
			while( _is_val_char(*end) ) end++;
			*result = _parse_token( state->cur, end );
			state->cur = end;
			return 0;
		}
	}
	assert(0);
}

Value parse( char *src )
{
	ParseState state;
	state.src = src;
	state.cur = src;
	state.line = 0;
	Value val;
	int err = _parse( &state, &val );
	if( err ){
		printf( "parse error!\n %s\n", state.cur );
		exit(1);
	}
	return val;
}

Value parse_list( char *src )
{
	ParseState state;
	state.src = src;
	state.cur = src;
	state.line = 0;
	Value val;
	int err = _parse_list( &state, &val );
	if( err ){
		printf( "parse error!\n %s\n", state.cur );
		exit(1);
	}
	if( *state.cur != '\0' ){
		printf( "parse error!\n %s\n", state.cur );
		exit(1);
	}
	return val;
}

void register_cfunc( char *sym, LambdaType type, CFunction func )
{
	Value v = lambda_new();
	LAMBDA_FUNC(v) = func;
	LAMBDA_KIND(v) = type;
	bundle_define( bundle_cur, intern(sym), v );
}

void defun( char *sym, CFunction func )
{
	return register_cfunc( sym, LAMBDA_TYPE_CFUNC, func );
}

void defmacro( char *sym, CFunction func )
{
	return register_cfunc( sym, LAMBDA_TYPE_CMACRO, func );
}

//********************************************************
// Evaluation
//********************************************************

Value nthcdr( int n, Value v ){ for(;n>0;n--){ v = CDR(v); } return v; }
Value nth( int n, Value v ){ return CAR(nthcdr(n,v)); }

#define NEXT(_cont,_v) do{ Value r = _v; cont = _cont; result = (r); goto _loop; }while(0)

#define CONT continuation_new
#define CONT_OP(op,a,b,c) continuation_new(cons(op,a),b,c)

Value call( Value lmd, Value vals, Value cont, Value *result )
{
	switch( LAMBDA_KIND(lmd) ){
	case LAMBDA_TYPE_LAMBDA:
	case LAMBDA_TYPE_MACRO:
		{
			Value bundle = bundle_new( LAMBDA_BUNDLE(lmd) );
			for( Value cur=LAMBDA_ARGS(lmd); cur != NIL; cur=CDR(cur), vals=CDR(vals) ){
				if( V_IS_PAIR(cur) ){
					bundle_define( bundle, CAR(cur), CAR(vals) );
				}else{
					bundle_define( bundle, cur, vals );
					break;
				}
			}
			return continuation_new( cons(V_BEGIN, LAMBDA_BODY(lmd)), bundle, CONTINUATION_NEXT(cont) );
		}
	case LAMBDA_TYPE_CFUNC:
	case LAMBDA_TYPE_CMACRO:
		{
			Value next = ((CFunction)LAMBDA_FUNC(lmd))( vals, cont, result );
			return next;
		}
	}
	assert(0);
}

Value compile( Value code )
{
	Value compile_hook = bundle_get( bundle_cur, SYM_A_COMPILE_HOOK_A );
	if( compile_hook != NIL ){
		// display_val( "src:", code );
		code = eval_loop( cons( cons3( compile_hook, cons3( V_QUOTE, code, NIL ), NIL ), NIL) );
		// display_val( "src:", code );
	}
	return code;
}

#define C_BUNDLE CONTINUATION_BUNDLE
#define C_NEXT CONTINUATION_NEXT
#define C_CODE CONTINUATION_CODE

Value eval_loop( Value code )
{
	Value result = NIL;
	Value cont = CONT_OP( V_BEGIN, code, bundle_cur, NIL );
 _loop:
	if( cont == NIL ) return result;
	Value debug = bundle_get( C_BUNDLE(cont), SYM_A_DEBUG_A );
	if( debug != NIL ) display_val( "> ", C_CODE(cont) );
	// display_val( "=> ", result );
	switch( TYPE_OF(C_CODE(cont)) ){
	case TYPE_NIL:
	case TYPE_INT:
	case TYPE_LAMBDA:
	case TYPE_BOOL:
	case TYPE_BUNDLE:
	case TYPE_CONTINUATION:
	case TYPE_SPECIAL:
	case TYPE_SLOT:
		NEXT( C_NEXT(cont), C_CODE(cont) );
	case TYPE_SYMBOL:
		{
			Value val;
			bool found = bundle_find( C_BUNDLE(cont), C_CODE(cont), &val );
			if( !found ){
				display_val( "symbol not found: ", C_CODE(cont) );
				assert(0);
			}
			NEXT( C_NEXT(cont), val );
		}
	case TYPE_PAIR:
		if( !V_IS_SPECIAL(CAR(C_CODE(cont))) ){
			NEXT( CONT( CAR(C_CODE(cont)), C_BUNDLE(cont), 
						CONT_OP( V_CALL0, CDR(C_CODE(cont)), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
		}else{
			Value code = CDR(C_CODE(cont));
			Operator op = SPECIAL_OP(CAR(C_CODE(cont)));
			switch( op ){
			case OP_BEGIN:
				{
					// display_val( "OP_BEGIN: ", code );
					if( code == NIL ){
						NEXT( C_NEXT(cont), result );
					}else if( CDR(code) == NIL ){
						NEXT( CONT( CAR(code), C_BUNDLE(cont), C_NEXT(cont)), result );
					}else{
						NEXT( CONT( CAR(code), C_BUNDLE(cont),
									CONT_OP( V_BEGIN, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
					}
				}
			case OP_QUOTE:
				NEXT( C_NEXT(cont), CAR(code) );
			case OP_CALL0:
				// display_val( "OP_CALL0: ", C_CODE(cont) );
				switch( TYPE_OF(result) ){
				case TYPE_LAMBDA:
					{
						Value args = cons(result,NIL);
						Value lmd = V2LAMBDA(result);
						switch( LAMBDA_KIND(lmd) ){
						case LAMBDA_TYPE_LAMBDA:
						case LAMBDA_TYPE_CFUNC:
							NEXT( CONT( CAR(code), C_BUNDLE(cont),
										CONT_OP( V_CALL1, cons4( CDR(code), args, args, NIL), C_BUNDLE(cont), C_NEXT(cont) ) ),
								  NIL );
						case LAMBDA_TYPE_MACRO:
							{
								Value cont2 = call( lmd, code, cont, NULL );
								C_NEXT(cont2) = CONT_OP( V_EXEC_MACRO, NIL, C_BUNDLE(cont), C_NEXT(cont));
								NEXT( cont2, NIL );
							}
						case LAMBDA_TYPE_CMACRO:
							{
								Value expanded;
								Value next = call( lmd, code, cont, &expanded );
								NEXT( CONT_OP( V_BEGIN, expanded, C_BUNDLE(cont), next ), NIL );
							}
						}
					}
				case TYPE_SPECIAL:
					NEXT( CONT_OP( result, code, C_BUNDLE(cont), C_NEXT(cont) ), NIL );
				default:
					assert(0);
				}
			case OP_CALL1:
				{
					Value rest, vals, tmp;
					bind3(code,rest,vals,tmp);
					CDR(tmp) = cons( result, NIL );
					if( rest != NIL ){
						NEXT( CONT( CAR(rest), C_BUNDLE(cont),
									CONT_OP( V_CALL1, cons4( CDR(rest), vals, CDR(tmp), NIL ), C_BUNDLE(cont), C_NEXT(cont) ) ),
							  NIL );
					}else{
						Value lmd = V2LAMBDA(CAR(vals));
						switch( LAMBDA_KIND(lmd) ){
						case LAMBDA_TYPE_LAMBDA:
							NEXT( call( lmd, CDR(vals), cont, NULL ), NIL );
						case LAMBDA_TYPE_CFUNC:
							{
								Value val = NIL;
								Value next = ((CFunction)LAMBDA_FUNC(lmd))( CDR(vals), cont, &val );
								NEXT( next, val );
							}
						default:
							assert(0);
						}
					}
				}
			case OP_DEFINE:
				NEXT( CONT( CAR(CDR(code)), C_BUNDLE(cont),
							CONT_OP( V_DEFINE2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)) ), NIL );
				
			case OP_DEFINE2:
				bundle_define( C_BUNDLE(cont), code, result );
				NEXT( C_NEXT(cont), NIL );
				
			case OP_SET_I:
				NEXT( CONT( CAR(CDR(code)), C_BUNDLE(cont),
							CONT_OP( V_SET_I2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)) ), NIL );
				
			case OP_SET_I2:
				bundle_set( C_BUNDLE(cont), code, result );
				NEXT( C_NEXT(cont), NIL );

			case OP_LET:
				NEXT( CONT_OP( V_LET2, code, bundle_new( C_BUNDLE(cont) ), C_NEXT(cont)), NIL );
				
			case OP_LET2:
				if( CAR(code) != NIL ){
					Value args = CAR(code);
					NEXT( CONT( CAR(CDR(CAR(args))), C_BUNDLE(cont),
								CONT_OP( V_LET3, cons3( CAR(CAR(args)), CDR(args), CDR(code) ), C_BUNDLE(cont), C_NEXT(cont) )), NIL);
				}else{
					NEXT( CONT_OP( V_BEGIN, nthcdr(1,code), C_BUNDLE(cont), C_NEXT(cont) ), NIL );
				}
				
			case OP_LET3:
				bundle_define( C_BUNDLE(cont), CAR(code), result );
				NEXT( CONT_OP( V_LET2, CDR(code), C_BUNDLE(cont), C_NEXT(cont)), NIL );
				
			case OP_LAMBDA:
			case OP_MACRO:
				{
					// display_val( "lambda2:", result );
					Value lmd = lambda_new();
					LAMBDA_KIND(lmd) = (op==OP_LAMBDA)?LAMBDA_TYPE_LAMBDA:LAMBDA_TYPE_MACRO;
					LAMBDA_BUNDLE(lmd) = C_BUNDLE(cont);
					bind2cdr( code, LAMBDA_ARGS(lmd), LAMBDA_BODY(lmd) );
					NEXT( C_NEXT(cont), LAMBDA2V(lmd) );
				}

			case OP_EXEC_MACRO:
				// display_val( "EXEC_MACRO :", result );
				NEXT( CONT( result, C_BUNDLE(cont), C_NEXT(cont) ), NIL );
				
			case OP_IF:
				NEXT( CONT( CAR(code), C_BUNDLE(cont),
							CONT_OP( V_IF2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
			case OP_IF2:
				if( result != VALUE_F ){
					NEXT( CONT( CAR(code), C_BUNDLE(cont), C_NEXT(cont) ), NIL );
				}else{
					NEXT( CONT_OP( V_BEGIN, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ), NIL );
				}
			}
		}
	}
	assert(0);
}

// print backtrace
// See: http://expcodes.com/12895
// See: http://0xcc.net/blog/archives/000067.html
#include <execinfo.h>
static void handler(int sig) {
	void *array[10];
	size_t size;

	// get void*'s for all entries on the stack
	size = backtrace(array, 10);

	// print out all the frames to stderr
	fprintf(stderr, "Error: signal %d:\n", sig);
	backtrace_symbols_fd(array+3, size-3, 2/*=stderr*/);
	exit(1);
}

void init()
{
	signal( SIGABRT, handler );
	signal( SIGSEGV, handler );
	
	gc_init( _mark_root );

	NIL = cell_new(TYPE_NIL);
	VALUE_T = cell_new(TYPE_BOOL);
	VALUE_F = cell_new(TYPE_BOOL);
	
	bundle_cur = bundle_new( NIL );
	retained = NIL;
	_symbol_root = NIL;

	_special_init();
	
	SYM_A_DEBUG_A = intern("*debug*");
	SYM_A_COMPILE_HOOK_A = intern("*compile-hook*");
	SYM_QUASIQUOTE = intern("quasiquote");
	SYM_UNQUOTE = intern("unquote");
								  
	cfunc_init();
}

