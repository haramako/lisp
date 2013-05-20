#include "lisp.h"
#include <stdio.h>
#include <string.h>
#include <limits.h>

static Value _value( Value args, Value cont, Value *result )
{
	*result = CAR(args);
	return CONTINUATION_NEXT(cont);
}

static Value _eq_p( Value args, Value cont, Value *result )
{
	*result = VALUE_F;
	for( Value cur=args; cur != NIL; cur = CDR(cur) ){
		if( CDR(cur) == NIL ) break;
		Value car, cdr;
		bind2( cur, car, cdr );
		if( !eq( car, cdr ) ) return CONTINUATION_NEXT(cont);
	}
	*result = VALUE_T;
	return CONTINUATION_NEXT(cont);
}

static Value _eqv_p( Value args, Value cont, Value *result )
{
	*result = VALUE_F;
	for( Value cur=args; cur != NIL; cur = CDR(cur) ){
		if( CDR(cur) == NIL ) break;
		Value car, cdr;
		bind2( cur, car, cdr );
		if( !eqv( car, cdr ) ) return CONTINUATION_NEXT(cont);
	}
	*result = VALUE_T;
	return CONTINUATION_NEXT(cont);
}

static Value _equal_p( Value args, Value cont, Value *result )
{
	*result = VALUE_F;
	for( Value cur=args; cur != NIL; cur = CDR(cur) ){
		if( CDR(cur) == NIL ) break;
		Value car, cdr;
		bind2( cur, car, cdr );
		if( !equal( car, cdr ) ) return CONTINUATION_NEXT(cont);
	}
	*result = VALUE_T;
	return CONTINUATION_NEXT(cont);
}

static Value _define_p( Value args, Value cont, Value *result )
{
	if( bundle_find( CONTINUATION_BUNDLE(cont), CAR(args), true, false ) ){
		*result = VALUE_T;
	}else{
		*result = VALUE_F;
	}
	return CONTINUATION_NEXT(cont);
}

static Value _add( Value args, Value cont, Value *result )
{
	int sum = 0;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		sum += V2INT(CAR(cur));
	}
	*result = INT2V(sum);
	return CONTINUATION_NEXT(cont);
}

static Value _sub( Value args, Value cont, Value *result )
{
	int64_t sum = V2INT(CAR(args));
	for( Value cur = CDR(args); cur != NIL; cur = CDR(cur) ){
		sum -= V2INT(CAR(cur));
	}
	*result = INT2V(sum);
	return CONTINUATION_NEXT(cont);
}

static Value _mul( Value args, Value cont, Value *result )
{
	int sum = 1;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		sum *= V2INT(CAR(cur));
	}
	*result = INT2V(sum);
	return CONTINUATION_NEXT(cont);
}

static Value _div( Value args, Value cont, Value *result )
{
	int sum = 1;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		sum /= V2INT(CAR(cur));
	}
	*result = INT2V(sum);
	return CONTINUATION_NEXT(cont);
}

static Value _modulo( Value args, Value cont, Value *result )
{
	int64_t sum = V2INT(CAR(args));
	for( Value cur = CDR(args); cur != NIL; cur = CDR(cur) ){
		sum %= V2INT(CAR(cur));
	}
	*result = INT2V(sum);
	return CONTINUATION_NEXT(cont);
}

static Value _less( Value args, Value cont, Value *result )
{
	int64_t last = INT64_MIN;
	*result = VALUE_F;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		int64_t n = V2INT(CAR(cur));
		if( !(last < n) ) return CONTINUATION_NEXT(cont);
		last = n;
	}
	*result = VALUE_T;
	return CONTINUATION_NEXT(cont);
}

static Value _less_eq( Value args, Value cont, Value *result )
{
	int64_t last = INT64_MIN;
	*result = VALUE_F;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		int64_t n = V2INT(CAR(cur));
		if( !(last <= n) ) return CONTINUATION_NEXT(cont);
		last = n;
	}
	*result = VALUE_T;
	return CONTINUATION_NEXT(cont);
}

static Value _greater( Value args, Value cont, Value *result )
{
	int64_t last = INT64_MAX;
	*result = VALUE_F;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		int64_t n = V2INT(CAR(cur));
		if( !(last > n) ) return CONTINUATION_NEXT(cont);
		last = n;
	}
	*result = VALUE_T;
	return CONTINUATION_NEXT(cont);
}

static Value _greater_eq( Value args, Value cont, Value *result )
{
	int64_t last = INT64_MAX;
	*result = VALUE_F;
	for( Value cur = args; cur != NIL; cur = CDR(cur) ){
		int64_t n = V2INT(CAR(cur));
		if( !(last >= n) ) return CONTINUATION_NEXT(cont);
		last = n;
	}
	*result = VALUE_T;
	return CONTINUATION_NEXT(cont);
}

static Value _car( Value args, Value cont, Value *result )
{
	*result = CAAR(args);
	return CONTINUATION_NEXT(cont);
}

static Value _cdr( Value args, Value cont, Value *result )
{
	*result = CDAR(args);
	return CONTINUATION_NEXT(cont);
}

static Value _cons( Value args, Value cont, Value *result )
{
	*result = cons( CAR(args), CADR(args) );
	return CONTINUATION_NEXT(cont);
}

static Value _set_car_i( Value args, Value cont, Value *result )
{
	CAR(CAR(args)) = CADR(args);
	return CONTINUATION_NEXT(cont);
}

static Value _set_cdr_i( Value args, Value cont, Value *result )
{
	CDR(CAR(args)) = CADR(args);
	return CONTINUATION_NEXT(cont);
}

static Value _not( Value args, Value cont, Value *result )
{
	*result = (CAR(args)==VALUE_F)?VALUE_T:VALUE_F;
	return CONTINUATION_NEXT(cont);
}

static Value _list( Value args, Value cont, Value *result )
{
	*result = args;
	return CONTINUATION_NEXT(cont);
}

static Value _list_a( Value args, Value cont, Value *result )
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
	*result = CDR(li);
	return CONTINUATION_NEXT(cont);
}

static Value _number_p( Value args, Value cont, Value *result )
{
	*result = V_IS_INT(CAR(args))?VALUE_T:VALUE_F;
	return CONTINUATION_NEXT(cont);
}

static Value _symbol_p( Value args, Value cont, Value *result )
{
	*result = V_IS_SYMBOL(CAR(args))?VALUE_T:VALUE_F;
	return CONTINUATION_NEXT(cont);
}

static Value _pair_p( Value args, Value cont, Value *result )
{
	*result = V_IS_PAIR(CAR(args))?VALUE_T:VALUE_F;
	return CONTINUATION_NEXT(cont);
}

static Value _null_p( Value args, Value cont, Value *result )
{
	*result = (CAR(args)==NIL)?VALUE_T:VALUE_F;
	return CONTINUATION_NEXT(cont);
}

static Value _list_p( Value args, Value cont, Value *result )
{
	*result = (CAR(args)==NIL||V_IS_PAIR(CAR(args)))?VALUE_T:VALUE_F;
	return CONTINUATION_NEXT(cont);
}

static Value _procedure_p( Value args, Value cont, Value *result )
{
	if( V_IS_LAMBDA(CAR(args)) ){
		Value lmd = V2LAMBDA(CAR(args));
		*result = (LAMBDA_KIND(lmd)==LAMBDA_TYPE_CFUNC||LAMBDA_KIND(lmd)==LAMBDA_TYPE_LAMBDA)?VALUE_T:VALUE_F;
	}else{
		*result = VALUE_F;
	}
	return CONTINUATION_NEXT(cont);
}

static Value _macro_p( Value args, Value cont, Value *result )
{
	if( V_IS_LAMBDA(CAR(args)) ){
		Value lmd = V2LAMBDA(CAR(args));
		*result = (LAMBDA_KIND(lmd)==LAMBDA_TYPE_CMACRO||LAMBDA_KIND(lmd)==LAMBDA_TYPE_MACRO)?VALUE_T:VALUE_F;
	}else{
		*result = VALUE_F;
	}
	return CONTINUATION_NEXT(cont);
}

static Value _get_closure_code( Value args, Value cont, Value *result )
{
	Value lmd = V2LAMBDA(CAR(args));
	Value new_lmd = lambda_new();
	LAMBDA_KIND(new_lmd) = LAMBDA_TYPE_LAMBDA;
	LAMBDA_ARGS(new_lmd) = LAMBDA_ARGS(lmd);
	LAMBDA_BODY(new_lmd) = LAMBDA_BODY(lmd);
	LAMBDA_FUNC(new_lmd) = LAMBDA_FUNC(lmd);
	*result = new_lmd;
	return CONTINUATION_NEXT(cont);
}

static Value _apply( Value args, Value cont, Value *result )
{
	Value lmd = V2LAMBDA(CAR(args));
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

static Value _eval( Value args, Value cont, Value *result )
{
	Value c = continuation_new( CAR(args), CONTINUATION_BUNDLE(cont), CONTINUATION_NEXT(cont) );
	return c;
}

static Value _backtrace( Value args, Value cont, Value *result )
{
	printf( "backtrace:\n" );
	for( Value cur=CONTINUATION_NEXT(cont); cur != NIL; cur = CONTINUATION_NEXT(cur) ){
		Value code = CONTINUATION_CODE(cur);
		printf( "  %s in %s\n",
				v2s_limit(code, 60),
				v2s(BUNDLE_LAMBDA(CONTINUATION_BUNDLE(cur))) );
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

static Value _display( Value args, Value cont, Value *result )
{
	char buf[10240];
	Value v, port;
	bind2arg( args, v, port );
    assert( v );
	if( !port ) port = bundle_get( CONTINUATION_BUNDLE(cont), SYM_CURRENT_OUTPUT_PORT );
	switch( TYPE_OF(v) ){
	case TYPE_STRING:
		fputs( STRING_STR(v), STREAM_FD(port) );
		break;
	default:
		value_to_str(buf, v);
		fputs( buf, STREAM_FD(port) );
	}
	return CONTINUATION_NEXT(cont);
}

static Value _write( Value args, Value cont, Value *result )
{
	Value v, port;
	bind2arg( args, v, port );
	if( !port ) port = bundle_get( CONTINUATION_BUNDLE(cont), SYM_CURRENT_OUTPUT_PORT );
	stream_write( port, v );
	return CONTINUATION_NEXT(cont);
}

static Value _read( Value args, Value cont, Value *result )
{
	Value v, port;
	bind2arg( args, v, port );
	if( !port ) port = bundle_get( CONTINUATION_BUNDLE(cont), SYM_CURRENT_INPUT_PORT );
	*result = stream_read(port);
	return CONTINUATION_NEXT(cont);
}

static Value _load( Value args, Value cont, Value *result )
{
	Value filename, port;
	bind2arg( args, filename, port );
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

void cfunc_init()
{
	defun( "value", _value );
	defun( "eq?", _eq_p );
	defun( "eqv?", _eqv_p );
	defun( "=", _eqv_p );
	defun( "equal?", _equal_p );
	defun( "define?", _define_p );
	
	defun( "+", _add );
	defun( "-", _sub );
	defun( "*", _mul );
	defun( "/", _div );
	defun( "modulo", _modulo );
	defun( "<", _less );
	defun( "<=", _less_eq );
	defun( ">", _greater );
	defun( ">=", _greater_eq );
	
	defun( "car", _car );
	defun( "cdr", _cdr );
	defun( "cons", _cons );
	defun( "set-car!", _set_car_i );
	defun( "set-cdr!", _set_cdr_i );
	defun( "list", _list );
	defun( "list*", _list_a );

	defun( "not", _not );

	defun( "number?", _number_p );
	defun( "integer?", _number_p );
	defun( "symbol?", _symbol_p );
	defun( "pair?", _pair_p );
	defun( "null?", _null_p );
	defun( "list?", _list_p );
	defun( "procedure?", _procedure_p );
	defun( "macro?", _macro_p );

	defun( "apply", _apply );
	defun( "get-closure-code", _get_closure_code );

	defun( "eval", _eval );
	defun( "backtrace", _backtrace );
	defun( "call/cc", _call_cc );
	defun( "call-with-current-continuation", _call_cc );

	defun( "display", _display );
	defun( "write", _write );
	defun( "read", _read );
	defun( "load", _load );
	defun( "exit", _exit );

}
