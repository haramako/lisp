#include "lisp.h"
#include <stdio.h>
#include <string.h>

/*
static Value _if( Value args )
{
	Value cond = eval( CAR(args) );
	if( cond != VALUE_F ){
		return eval( CAR(CDR(args)) );
	}else{
		return begin( CDR(CDR(args)) );
	}
}

static Value _quasi_quote_inner( Value args )
{
	switch( TYPE_OF(args) ){
	case TYPE_PAIR:
		if( CAR(args) == intern("unquote") ){
			return eval( CAR( CDR(args) ) );
		}else{
			return cons( _quasi_quote_inner(CAR(args)), _quasi_quote_inner(CDR(args)) );
		}
	default:
		return args;
	}
}

static Value _quasi_quote( Value args )
{
	return _quasi_quote_inner( CAR(args) );
}
*/

void display_val( char* str, Value args )
{
	char buf[10240];
	value_to_str(buf, args);
	printf( "%s%s\n", str, buf );
}

static Value _display( Value args, Value cont, Value *result )
{
	char buf[10240];
	value_to_str(buf, args);
	if( V_IS_PAIR(args) ){
		buf[ strlen(buf) - 1 ] = '\0';
		printf( "%s\n", buf+1 );
	}else{
		printf( "%s\n", buf );
	}
	return CONTINUATION_NEXT(cont);
}

static Value _value( Value args, Value cont, Value *result )
{
	*result = CAR(args);
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
	int sum = V2INT(CAR(args));
	for( Value cur = CDR(args); cur != NIL; cur = CDR(cur) ){
		sum -= V2INT(CAR(cur));
	}
	*result = INT2V(sum);
	return CONTINUATION_NEXT(cont);
}

static Value _eq_p( Value args, Value cont, Value *result )
{
	*result = VALUE_F;
	for( Value cur=args; cur != NIL; cur = CDR(cur) ){
		if( CDR(cur) == NIL ) break;
		Value car, cdr;
		bind2( cur, car, cdr );
		if( TYPE_OF(car) != TYPE_OF(cdr) ) return CONTINUATION_NEXT(cont);
		switch( TYPE_OF(car) ){
		case TYPE_NIL:
			break;
		case TYPE_INT:
			if( V2INT(car) != V2INT(cdr) ) return CONTINUATION_NEXT(cont);
			break;
		default:
			if( car != cdr ) return CONTINUATION_NEXT(cont);
			break;
		}
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
		if( TYPE_OF(car) != TYPE_OF(cdr) ) return CONTINUATION_NEXT(cont);
		switch( TYPE_OF(car) ){
		case TYPE_NIL:
			break;
		case TYPE_INT:
			if( V2INT(car) != V2INT(cdr) ) return CONTINUATION_NEXT(cont);
			break;
		default:
			if( car != cdr ) return CONTINUATION_NEXT(cont);
			break;
		}
	}
	*result = VALUE_T;
	return CONTINUATION_NEXT(cont);
}

static Value _define_p( Value args, Value cont, Value *result )
{
	if( bundle_find( CONTINUATION_BUNDLE(cont), CAR(args), NULL ) ){
		*result = VALUE_T;
	}else{
		*result = VALUE_F;
	}
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
	*result = LAMBDA2V(new_lmd);
	return CONTINUATION_NEXT(cont);
}

static Value _apply( Value args, Value cont, Value *result )
{
	Value lmd = V2LAMBDA(CAR(args));
	Value c = call( lmd, CADR(args), cont, result );
	return c;
}

static Value _eval( Value args, Value cont, Value *result )
{
	Value c = continuation_new( CAR(args), CONTINUATION_BUNDLE(cont), CONTINUATION_NEXT(cont) );
	return c;
}


void cfunc_init()
{
	//defspecial( "begin", begin );
	//defspecial( "if", _if );
	//defspecial( "quasi-quote", _quasi_quote );

	defun( "display", _display );
	defun( "value", _value );
	defun( "+", _add );
	defun( "-", _sub );
	defun( "eq?", _eq_p );
	defun( "eqv?", _eqv_p );
	defun( "define?", _define_p );
	
	defun( "car", _car );
	defun( "cdr", _cdr );
	defun( "cons", _cons );
	defun( "list", _list );

	defun( "not", _not );

	defun( "number?", _number_p );
	defun( "symbol?", _symbol_p );
	defun( "pair?", _pair_p );
	defun( "null?", _null_p );
	defun( "list?", _list_p );
	defun( "procedure?", _procedure_p );
	defun( "macro?", _macro_p );

	defun( "apply", _apply );
	defun( "get-closure-code", _get_closure_code );

	defun( "eval", _eval );
	
}
