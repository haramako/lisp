#include "lisp.h"
#include <stdio.h>
#include <string.h>
#include <limits.h>

static Value _value( Value bundle, Value v )
{
	return v;
}

static Value _eq_p( Value bundle, Value args )
{
	if( args == NIL ) return VALUE_T;
	Value v = CAR(args);
	for( Value cur=CDR(args); cur != NIL; cur = CDR(cur) ){
		if( !eq( v, CAR(cur) ) ) return VALUE_F;
	}
	return VALUE_T;
}

static Value _eqv_p( Value bundle, Value args )
{
	if( args == NIL ) return VALUE_T;
	Value v = CAR(args);
	for( Value cur=CDR(args); cur != NIL; cur = CDR(cur) ){
		if( !eqv( v, CAR(cur) ) ) return VALUE_F;
	}
	return VALUE_T;
}

static Value _equal_p( Value bundle, Value args )
{
	if( args == NIL ) return VALUE_T;
	Value v = CAR(args);
	for( Value cur=CDR(args); cur != NIL; cur = CDR(cur) ){
		if( !equal( v, CAR(cur) ) ) return VALUE_F;
	}
	return VALUE_T;
}

static Value _defined_p( Value bundle, Value sym )
{
	return bundle_find( bundle, sym, true, false )?VALUE_T:VALUE_F;
}

static Value _add( Value bundle, Value args )
{
	int sum = 0;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		sum += V2INT(CAR(cur));
	}
	return INT2V(sum);
}

static Value _sub( Value bundle, Value args )
{
	int64_t sum = V2INT(CAR(args));
	for( Value cur = CDR(args); cur != NIL; cur = CDR(cur) ){
		sum -= V2INT(CAR(cur));
	}
	return INT2V(sum);
}

static Value _mul( Value bundle, Value args )
{
	int sum = 1;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		sum *= V2INT(CAR(cur));
	}
	return INT2V(sum);
}

static Value _div( Value bundle, Value args )
{
	int64_t sum = V2INT(CAR(args));
	for( Value cur = CDR(args); cur != NIL; cur = CDR(cur) ){
		sum /= V2INT(CAR(cur));
	}
	return INT2V(sum);
}

static Value _modulo( Value bundle, Value args )
{
	int64_t sum = V2INT(CAR(args));
	for( Value cur = CDR(args); cur != NIL; cur = CDR(cur) ){
		sum %= V2INT(CAR(cur));
	}
	return INT2V(sum);
}

static Value _less( Value bundle, Value args )
{
	int64_t last = INT64_MIN;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		int64_t n = V2INT(CAR(cur));
		if( !(last < n) ) return VALUE_F;
		last = n;
	}
	return VALUE_T;
}

static Value _less_eq( Value bundle, Value args )
{
	int64_t last = INT64_MIN;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		int64_t n = V2INT(CAR(cur));
		if( !(last <= n) ) return VALUE_F;
		last = n;
	}
	return VALUE_T;
}

static Value _greater( Value bundle, Value args )
{
	int64_t last = INT64_MAX;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		int64_t n = V2INT(CAR(cur));
		if( !(last > n) ) return VALUE_F;
		last = n;
	}
	return VALUE_T;
}

static Value _greater_eq( Value bundle, Value args )
{
	int64_t last = INT64_MAX;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		int64_t n = V2INT(CAR(cur));
		if( !(last >= n) ) return VALUE_F;
		last = n;
	}
	return VALUE_T;
}

static Value _car( Value bundle, Value v )
{
	return CAR(v);
}

static Value _cdr( Value bundle, Value v )
{
	return CDR(v);
}

static Value _cons( Value bundle, Value v1, Value v2 )
{
	return cons( v1, v2 );
}

static Value _set_car_i( Value bundle, Value pair, Value v )
{
	CAR(pair) = v;
	return NIL;
}

static Value _set_cdr_i( Value bundle, Value pair, Value v )
{
	CDR(pair) = v;
	return NIL;
}

static Value _list( Value bundle, Value args )
{
	return args;
}

static Value _list_a( Value bundle, Value args )
{
	Value li = cons( NIL, NIL );
	Value tail = li;
	for( Value cur=args; cur != NIL; cur=CDR(cur) ){
		if( CDR(cur) != NIL ){
			CDR(tail) = cons( CAR(cur), NIL );
			tail = CDR(tail);
		}else{
			CDR(tail) = CAR(cur);
		}
	}
	return CDR(li);
}

static Value _not( Value bundle, Value v )
{
	return (v==VALUE_F)?VALUE_T:VALUE_F;
}

static Value _number_p( Value bundle, Value v )
{
	return IS_INT(v)?VALUE_T:VALUE_F;
}

static Value _symbol_p( Value bundle, Value v )
{
	return IS_SYMBOL(v)?VALUE_T:VALUE_F;
}

static Value _pair_p( Value bundle, Value v )
{
	return IS_PAIR(v)?VALUE_T:VALUE_F;
}

static Value _null_p( Value bundle, Value v )
{
	return (v==NIL)?VALUE_T:VALUE_F;
}

static Value _list_p( Value bundle, Value v )
{
	return (v==NIL||IS_PAIR(v))?VALUE_T:VALUE_F;
}

static Value _procedure_p( Value bundle, Value v )
{
	return (TYPE_OF(v)==TYPE_CFUNC||TYPE_OF(v)==TYPE_LAMBDA)?VALUE_T:VALUE_F;
}

static Value _macro_p( Value bundle, Value v )
{
	return (TYPE_OF(v)==TYPE_LAMBDA&&LAMBDA_TYPE(v)==LAMBDA_TYPE_MACRO)?VALUE_T:VALUE_F;
}

static Value _apply( Value args, Value cont, Value *result )
{
	// 継続の場合
	switch( TYPE_OF(CAR(args)) ){
	case TYPE_CONTINUATION:
		{
			Value rest = CADR(args);
			if( IS_PAIR(CDR(rest)) ){
				*result = cons( intern("VALUES"), rest );
			}else{
				*result = CAR(rest);
			}
			return CAR(args);
		}
	case TYPE_LAMBDA:
	case TYPE_CFUNC:
		{
			Value lmd = CAR(args);
			Value head = cons( NIL, NIL );
			Value tail = head;
			for( Value cur=CDR(args); cur != NIL; cur = CDR(cur) ){
				if( CDR(cur) == NIL ){
					for( Value v=CAR(cur); v != NIL; v = CDR(v) ){
						tail = CDR(tail) = cons( CAR(v), NIL );
					}
				}else{
					tail = CDR(tail) = cons( CAR(cur), NIL );
				}
			}
			Value c = call( lmd, CDR(head), cont, result );
			return c;
		}
	default:
		assert(0);
	}
}

static Value _syntax_expand1( Value bundle, Value code )
{
	return syntax_expand1( code );
}

static Value _eval( Value args, Value cont, Value *result )
{
	Value code, bundle;
	bind2arg( args, code, bundle );
	if( !code ) assert(0);
	if( !bundle ) bundle = CONTINUATION_BUNDLE(cont);
	Value c = continuation_new( code, bundle, CONTINUATION_NEXT(cont) );
	return c;
}

static Value _current_environment( Value args, Value cont, Value *result )
{
	*result = CONTINUATION_BUNDLE(cont);
	return CONTINUATION_NEXT(cont);
}

static Value _backtrace( Value args, Value cont, Value *result )
{
	printf( "backtrace:\n" );
	for( Value cur=CONTINUATION_NEXT(cont); cur != NIL; cur = CONTINUATION_NEXT(cur) ){
		Value code = CONTINUATION_CODE(cur);
		if( IS_PAIR(code) && CAR(code) == V_READ_EVAL ){
			printf( "  %s:%d: *read-eval*\n", STRING_STR(STREAM_FILENAME(CDR(code))), STREAM_LINE(CDR(code)) );
		}else{
			printf( "  %s in %s\n",
					v2s_limit(code, 60),
					v2s(BUNDLE_LAMBDA(CONTINUATION_BUNDLE(cur))) );
		}
	}
	*result = NIL;
	return CONTINUATION_NEXT(cont);
}

static Value _call_cc( Value args, Value cont, Value *result )
{
	*result = CAR(args);
	return continuation_new( cons3( V_CALL0, CONTINUATION_NEXT(cont), NIL ),
							 CONTINUATION_BUNDLE(cont), CONTINUATION_NEXT(cont) );
}

static Value _load( Value args, Value cont, Value *result )
{
	Value filename, port;
	bind2arg( args, filename, port );
    assert(filename);
	FILE *fd = fopen( STRING_STR(filename), "r" );
	if( !fd ) assert(0);
	Value file = stream_new( fd, true, STRING_STR(filename) );
	return continuation_new( cons( V_READ_EVAL, file ),
							 CONTINUATION_BUNDLE(cont), CONTINUATION_NEXT(cont) );
}

static Value _exit( Value args, Value cont, Value *result )
{
	bind1arg(args,*result);
	if( !*result ) *result = NIL;
	return NIL;
}

static Value _display( Value bundle, Value v, Value rest )
{
	char buf[10240];
	Value port;
	bind1arg( rest, port );
	if( !port ) port = bundle_get( bundle, SYM_CURRENT_OUTPUT_PORT, NULL );
	
	switch( TYPE_OF(v) ){
	case TYPE_STRING:
		fputs( STRING_STR(v), STREAM_FD(port) );
		break;
	default:
		value_to_str(buf, sizeof(buf), v);
		fputs( buf, STREAM_FD(port) );
	}
	return NIL;
}

static Value _write( Value bundle, Value v, Value rest )
{
	Value port;
	bind1arg( rest, port );
	if( !port ) port = bundle_get( bundle, SYM_CURRENT_OUTPUT_PORT, NULL );
	stream_write( port, v );
	return NIL;
}

static Value _read( Value bundle, Value v, Value rest )
{
	Value port;
	bind1arg( rest, port );
	if( !port ) port = bundle_get( bundle, SYM_CURRENT_INPUT_PORT, NULL );
	return stream_read(port);
}


static Value _number_to_string( Value bundle, Value v )
{
	char buf[32];
	sprintf( buf, "%lld", V2INT(v) );
	return string_new(buf);
}

static Value _string_append( Value bundle, Value args )
{
	char buf[10240];
	char *tail = buf;
	for( Value cur=args; cur != NIL; cur=CDR(cur) ){
		tail += sprintf( tail, "%s", STRING_STR(CAR(cur)) );
	}
	return string_new(buf);
}

static Value _runtime_value_set_i( Value bundle, Value _name, Value val )
{
	char *name;
	if( IS_SYMBOL(_name) ){
		name = STRING_STR(SYMBOL_STR(_name));
	}else if( IS_STRING(_name) ){
		name = STRING_STR(_name);
	}else{
		assert(0);
	}
	
	if( strcmp( name, "trace" ) == 0 ){
		opt_trace = V2INT(val);
	}else{
		assert(0);
	}
	return NIL;
}

void cfunc_init()
{
	// basic
	defun( "value", 1, _value );
	defun( "eq?", -1, _eq_p );
	defun( "eqv?", -1, _eqv_p );
	defun( "=", -1, _eqv_p );
	defun( "equal?", -1, _equal_p );
	defun( "defined?", 1, _defined_p );

	// number
	defun( "+", -1, _add );
	defun( "-", -1, _sub );
	defun( "*", -1, _mul );
	defun( "/", -1, _div );
	defun( "quotient", -1, _div );
	defun( "modulo", -1, _modulo );
	defun( "<", -1, _less );
	defun( "<=", -1, _less_eq );
	defun( ">", -1, _greater );
	defun( ">=", -1, _greater_eq );

	// pair/list
	defun( "car", 1, _car );
	defun( "cdr", 1, _cdr );
	defun( "cons", 2, _cons );
	defun( "set-car!", 2, _set_car_i );
	defun( "set-cdr!", 2, _set_cdr_i );
	defun( "list", -1, _list );
	defun( "list*", -1, _list_a );

	// bool
	defun( "not", 1, _not );

	// type
	defun( "number?", 1, _number_p );
	defun( "integer?", 1, _number_p );
	defun( "symbol?", 1, _symbol_p );
	defun( "pair?", 1, _pair_p );
	defun( "null?", 1, _null_p );
	defun( "list?", 1, _list_p );
	defun( "procedure?", 1, _procedure_p );
	defun( "macro?", 1, _macro_p );

	//
	defun( "apply", CFUNC_VARIABLE, _apply );
	defun( "syntax-expand1", 1, _syntax_expand1 );

	//
	defun( "eval", CFUNC_VARIABLE, _eval );
	defun( "current-environment", CFUNC_VARIABLE, _current_environment );
	defun( "backtrace", CFUNC_VARIABLE, _backtrace );
	defun( "call/cc", CFUNC_VARIABLE, _call_cc );
	defun( "call-with-current-continuation", CFUNC_VARIABLE, _call_cc );
	defun( "load", CFUNC_VARIABLE, _load );
	defun( "exit", CFUNC_VARIABLE, _exit );

	// I/O
	defun( "display", -2, _display );
	defun( "write", -2, _write );
	defun( "read", -2, _read );

	// string
	defun( "number->string", 1, _number_to_string );
	defun( "string-append", -1, _string_append );

	// debug
	defun( "runtime-value-set!", 2, _runtime_value_set_i );
	

}
