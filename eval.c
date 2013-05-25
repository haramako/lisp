#include "lisp.h"

//********************************************************
// Evaluation
//********************************************************

Value cont_error( char *str, Value cont )
{
	return continuation_new( cons3( SYM_ERROR, string_new(str), NIL ),
							 CONTINUATION_BUNDLE(cont), CONTINUATION_NEXT(cont) );
}

Value call( Value lmd, Value vals, Value cont, Value *result )
{
	Value orig_vals = vals;
	switch( TYPE_OF(lmd) ){
	case TYPE_LAMBDA:
		switch( LAMBDA_TYPE(lmd) ){
		case LAMBDA_TYPE_LAMBDA:
		case LAMBDA_TYPE_MACRO:
			{
				Value bundle = bundle_new( LAMBDA_BUNDLE(lmd) );
				BUNDLE_LAMBDA(bundle) = lmd;
				for( Value cur=LAMBDA_ARGS(lmd); cur != NIL; cur=CDR(cur), vals=CDR(vals) ){
					if( IS_PAIR(cur) ){
						if( !IS_PAIR(vals) ){
							printf( "call: %s orig_vals: %s lmd: %s\n", v2s(lmd), v2s(orig_vals), v2s(LAMBDA_ARGS(lmd)) );
						}
						if( !IS_SYMBOL(CAR(cur)) ) return cont_error( "not symbol", cont );
						bundle_define( bundle, CAR(cur), CAR(vals) );
					}else{
						if( !IS_SYMBOL(cur) ) return cont_error( "not symbol", cont );
						bundle_define( bundle, cur, vals );
						break;
					}
				}
				return continuation_new( cons(V_BEGIN, LAMBDA_BODY(lmd)), bundle, CONTINUATION_NEXT(cont) );
			}
		}
	case TYPE_CFUNC:
		{
			void *func = CFUNC_FUNC(lmd);
			int arity = CFUNC_ARITY(lmd);
			if( arity == CFUNC_VARIABLE ){
				return ((CFunction)func)( vals, cont, result );
			}else{
				Value bundle = CONTINUATION_BUNDLE(cont);
				bool has_rest = (arity<0);
				int arg_num = has_rest?(-1-arity):(arity);
				Value v[8] = {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};
				
				for( int i=0; i<arg_num; i++ ){
					if( vals == NIL ){ printf("%s %d\n", v2s(lmd), i); assert(0); }
					v[i] = CAR(vals);
					vals = CDR(vals);
				}
				
				if( has_rest ){
					v[arg_num] = vals;
					arg_num++;
				}else{
					if( vals != NIL ) assert(0);
				}
				
				switch( arg_num ){
				case 0: *result = ((CFunction0)func)( bundle ); break;
				case 1: *result = ((CFunction1)func)( bundle, v[0] ); break;
				case 2: *result = ((CFunction2)func)( bundle, v[0], v[1] ); break;
				case 3: *result = ((CFunction3)func)( bundle, v[0], v[1], v[2] ); break;
				case 4: *result = ((CFunction4)func)( bundle, v[0], v[1], v[2], v[3] ); break;
				case 5: *result = ((CFunction5)func)( bundle, v[0], v[1], v[2], v[3], v[4] ); break;
				case 6: *result = ((CFunction6)func)( bundle, v[0], v[1], v[2], v[3], v[4], v[5] ); break;
				case 7: *result = ((CFunction7)func)( bundle, v[0], v[1], v[2], v[3], v[4], v[5], v[6] ); break;
				default:
					assert(0);
				}
				return CONTINUATION_NEXT(cont);
			}
		}
	default:
		assert(0);
	}
}

#define NEXT(_cont,_v) do{ Value r = _v; cont = _cont; result = (r); goto _loop; }while(0)
#define ERROR(mes) do{													\
		Value _code = C_CODE(cont);										\
		NEXT( CONT( cons4( SYM_ERROR, string_new(mes), cons3(V_QUOTE,_code,NIL), NIL ), \
					C_BUNDLE(cont), C_NEXT(cont)), NIL); }while(0)
#define CHECK(x) do{ if(!x){ ERROR("invalid form"); } }while(0)
#define FAIL() CHECK(0)

#define CONT continuation_new
#define CONT_OP(op,a,b,c) continuation_new(cons(op,a),b,c)
#define C_BUNDLE CONTINUATION_BUNDLE
#define C_NEXT CONTINUATION_NEXT
#define C_CODE CONTINUATION_CODE

Value _eval_direct( Value cont, Value code )
{
	switch( TYPE_OF(code) ){
	case TYPE_UNUSED:
	case TYPE_MAX:
		assert(0);
	case TYPE_NIL:
	case TYPE_INT:
	case TYPE_CHAR:
	case TYPE_LAMBDA:
	case TYPE_CFUNC:
	case TYPE_BOOL:
	case TYPE_STRING:
	case TYPE_BUNDLE:
	case TYPE_CONTINUATION:
	case TYPE_SPECIAL:
	case TYPE_STREAM:
		return code;
	case TYPE_SYMBOL:
		return bundle_get( C_BUNDLE(cont), code, NULL );
	default:
		return NULL;
	}
}

#define NEXT_DIRECT(_code,next_cont) do{					\
		Value _r = _eval_direct(cont,_code);				\
		if( _r ){											\
			cont = next_cont;								\
			result = _r;									\
			goto _loop;										\
		}else{												\
			NEXT( CONT(_code,C_BUNDLE(cont),next_cont), NIL );	\
		}													\
	}while(0)


Value normalize_let( Value code )
{
	if( CAR(code) != SYM_LET ) return code;
	if( IS_PAIR( CADR(code) )) return code;
	if( !IS_SYMBOL( CADR(code) ) ) return code;

	// normalize named let
	Value name, dummy, arg_vals, body;
	bind4cdr( code, dummy, name, arg_vals, body );
	
	Value args = NIL;
	Value vals = NIL;
	if( arg_vals != NIL ){
		args = cons( CAR(CAR(arg_vals)), NIL );
		vals = cons( CADR(CAR(arg_vals)), NIL );
		Value args_tail = args;
		Value vals_tail = vals;
		for( Value cur=CDR(arg_vals); cur != NIL; cur=CDR(cur) ){
			args_tail = CDR(args_tail) = cons( CAR(CAR(cur)), NIL);
			vals_tail = CDR(vals_tail) = cons( CADR(CAR(cur)), NIL);
		}
	}
	Value lambda = cons3( SYM_LAMBDA, args, body );
	Value new_code =
		cons4( SYM_LETREC,
			   cons( cons3( name, lambda, NIL ), NIL),
			   cons( name, vals ), NIL );
	// printf( "normalize_let: %s => %s\n", v2sn(code,40), v2sn(new_code,1000) );
	return new_code;
}

Value eval_loop( Stream *stream )
{
	int gc_count = 10000;
	Value result = NIL;
	Value cont = CONT_OP( V_READ_EVAL, (Value)stream, bundle_cur, NIL );
 _loop:

	if( gc_count-- <= 0 ){
		cont = retain( cont );
		result = retain( result );
		gc_run( opt_trace?1:0 );
		result = release( result );
		cont = release( cont );
		gc_count = 10000;
	}

	if( cont == NIL ) return result;
	// printf( "> %s => %s\n", v2sn(result,20), v2sn(C_CODE(cont), 80) );
	
	switch( TYPE_OF(C_CODE(cont)) ){
	case TYPE_UNUSED:
	case TYPE_MAX:
		assert(0);
	case TYPE_NIL:
	case TYPE_INT:
	case TYPE_CHAR:
	case TYPE_LAMBDA:
	case TYPE_CFUNC:
	case TYPE_BOOL:
	case TYPE_STRING:
	case TYPE_BUNDLE:
	case TYPE_CONTINUATION:
	case TYPE_SPECIAL:
	case TYPE_STREAM:
	case TYPE_SYMBOL:
		{
			Value v = _eval_direct( cont, C_CODE(cont) );
			if( !v ) ERROR( "symbol not found" );
			NEXT( C_NEXT(cont), v );
		}
	case TYPE_PAIR:
		if( !IS_SPECIAL(CAR(C_CODE(cont))) ){
			Value v = CAR(C_CODE(cont));
			NEXT_DIRECT( v,
						 CONT_OP( V_CALL0, CDR(C_CODE(cont)), C_BUNDLE(cont), C_NEXT(cont) ) );
		}
		
		Value code = CDR(C_CODE(cont));
		Operator op = SPECIAL_OP(CAR(C_CODE(cont)));
		switch( op ){
		case OP_BEGIN:
			// display_val( "OP_BEGIN: ", code );
			if( code == NIL ){
				NEXT( C_NEXT(cont), result );
			}else if( CDR(code) == NIL ){
				NEXT_DIRECT( CAR(code), C_NEXT(cont));
			}else{
				NEXT_DIRECT( CAR(code), 
							 CONT_OP( V_BEGIN, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) );
			}
			
		case OP_QUOTE:
			NEXT( C_NEXT(cont), CAR(code) );
			
		case OP_CALL0:
			// printf( "OP_CALL0: %s %s\n", v2sn(result,20), v2sn(C_CODE(cont),80) );
			switch( TYPE_OF(result) ){
			case TYPE_LAMBDA:
				switch( LAMBDA_TYPE(result) ){
				case LAMBDA_TYPE_LAMBDA:
					if( code != NIL ){
						Value args = cons(result,NIL);
						NEXT_DIRECT( CAR(code),
									 CONT_OP( V_CALL1, cons4( CDR(code), args, args, NIL), C_BUNDLE(cont), C_NEXT(cont) ) );
					}else{
						Value res = NIL;
						Value next = call( result, NIL, cont, &res);
						NEXT( next, res );
					}
				default:
					ERROR( "cannot call" );
				}
			case TYPE_CFUNC:
				if( code != NIL ){
					Value args = cons(result,NIL);
					NEXT_DIRECT( CAR(code), 
								 CONT_OP( V_CALL1, cons4( CDR(code), args, args, NIL), C_BUNDLE(cont), C_NEXT(cont) ) );
				}else{
					Value res = NIL;
					Value next = call( result, NIL, cont, &res);
					NEXT( next, res );
				}
			case TYPE_SPECIAL:
				NEXT( CONT_OP( result, code, C_BUNDLE(cont), C_NEXT(cont) ), NIL );
			case TYPE_CONTINUATION:
				{
					Value args = cons(result,NIL);
					NEXT( CONT( CAR(code), C_BUNDLE(cont),
								CONT_OP( V_CALL1, cons4( CDR(code), args, args, NIL), C_BUNDLE(cont), C_NEXT(cont) ) ),
						  NIL );
				}
			default:
				printf( "hogeeeee %s %s\n", v2s(result), v2s(code) );
				assert(0);
				FAIL();
				printf( "OP_CALL0: result: %s code: %s\n", v2s(result), v2s(C_CODE(cont)) );
				assert(0);
			}
			
		case OP_CALL1:
			{
				// display_val( "OP_CALL1: ", C_CODE(cont) );
				Value rest, vals, tmp;
				bind3(code,rest,vals,tmp);
				CDR(tmp) = cons( result, NIL );
				if( rest != NIL ){
					NEXT_DIRECT( CAR(rest),
								 CONT_OP( V_CALL1, cons4( CDR(rest), vals, CDR(tmp), NIL ), C_BUNDLE(cont), C_NEXT(cont) ) );
				}else{
					Value lmd = CAR(vals);
					if( IS_LAMBDA(lmd) || IS_CFUNC(lmd) ){
						Value val = NIL;
						Value next = call(lmd, CDR(vals), cont, &val );
						NEXT( next, val );
					}else if( IS_CONTINUATION(lmd) ){
						vals = CDR(vals);
						if( CDR(vals) != NIL ){
							NEXT( lmd, cons( SYM_VALUES, vals ));
						}else{
							NEXT( lmd, CAR(vals) );
						}
					}else{
						assert(0);
					}
				}
			}
			
		case OP_DEFINE:
			{
				if( IS_SYMBOL(CAR(code)) ){
					// (define sym val) の形
					NEXT_DIRECT(CADR(code),
								CONT_OP( V_DEFINE2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)) );
				}else if( IS_PAIR(CAR(code)) ){
					// (define (sym args ... ) の形
					NEXT( CONT( cons3( V_LAMBDA, CDAR(code), CDR(code) ), C_BUNDLE(cont),
								CONT_OP( V_DEFINE2, CAAR(code), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
				}else{
					assert(0);
				}
			}
				
		case OP_DEFINE2:
			bundle_define( C_BUNDLE(cont), code, result );
			NEXT( C_NEXT(cont), NIL );
				
		case OP_SET_I:
			NEXT_DIRECT( CADR(code),
						 CONT_OP( V_SET_I2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)) );
				
		case OP_SET_I2:
			bundle_set( C_BUNDLE(cont), code, result );
			NEXT( C_NEXT(cont), NIL );

		case OP_LET:
		case OP_LET_A:
		case OP_LETREC:
			//printf( "LET: %s\n", v2sn(code,80) );
			// TODO: read/eval時に変換するようにする
			if( op == OP_LET && IS_SYMBOL( CAR(code) ) ){
				Value tmp_code = cons( SYM_LET, code);
				Value new_code = normalize_let( tmp_code );
				if( new_code != tmp_code ){
					op = OP_LETREC;
					code = CDR(new_code);
				}
			}
			if( IS_PAIR( CAR(code) )){
				// normal let
				Value new_bundle = bundle_new(C_BUNDLE(cont));
				switch( op ){
				case OP_LET:
					NEXT( CONT_OP( V_LET2, cons(C_BUNDLE(cont), code), new_bundle, C_NEXT(cont)), NIL );
				case OP_LET_A:
					NEXT( CONT_OP( V_LET2, cons(new_bundle, code), new_bundle, C_NEXT(cont)), NIL );
				case OP_LETREC:
					for( Value cur=CAR(code); cur != NIL; cur = CDR(cur) ){
						Value sym_val = CAR(cur);
						CHECK( IS_PAIR(sym_val) );
						Value sym = CAR(sym_val);
						CHECK( IS_SYMBOL(sym) );
						bundle_define( new_bundle, sym, NIL );
					}
					NEXT( CONT_OP( V_LET2, cons(new_bundle, code), new_bundle, C_NEXT(cont)), NIL );
				default:
					assert(0);
				}
			}else{
				FAIL();
			}
				
		case OP_LET2:
			{
				Value bundle, vars, body;
				bind3cdr( code, bundle, vars, body );
				// printf( "OP_LET2: %s\n", v2s(vars) );
				if( IS_PAIR(vars) ){
					// printf( "OP_LET2- %s\n", v2s(CADR(CAR(vars))) );
					NEXT( CONT( CADR(CAR(vars)), bundle,
								CONT_OP( V_LET3, cons4( bundle, CAAR(vars), CDR(vars), body ), C_BUNDLE(cont), C_NEXT(cont) )), NIL);
				}else if( vars == NIL ){
					NEXT( CONT_OP( V_BEGIN, body, C_BUNDLE(cont), C_NEXT(cont) ), NIL );
				}else{
					FAIL();
				}
			}
				
		case OP_LET3:
			{
				Value bundle, sym, form;
				bind3cdr( code, bundle, sym, form );
				bundle_define( C_BUNDLE(cont), sym, result );
				NEXT( CONT_OP( V_LET2, cons( bundle, form ), C_BUNDLE(cont), C_NEXT(cont)), NIL );
			}
				
		case OP_LAMBDA:
		case OP_MACRO:
			{
				// display_val( "lambda2:", result );
				Value lmd = lambda_new();
				LAMBDA_TYPE(lmd) = (op==OP_LAMBDA)?LAMBDA_TYPE_LAMBDA:LAMBDA_TYPE_MACRO;
				LAMBDA_BUNDLE(lmd) = C_BUNDLE(cont);
				LAMBDA_DATA(lmd) = cons( NIL, code );
				NEXT( C_NEXT(cont), lmd );
			}

		case OP_DEFINE_SYNTAX:
			{
				bundle_define( C_BUNDLE(cont), CAR(code), CADR(code) );
				NEXT( C_NEXT(cont), NIL );
			}
				
		case OP_IF:
			NEXT_DIRECT( CAR(code),
						 CONT_OP( V_IF2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) );
			
		case OP_IF2:
			if( result != VALUE_F ){
				NEXT_DIRECT( CAR(code), C_NEXT(cont));
			}else{
				NEXT( CONT_OP( V_BEGIN, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ), NIL );
			}

		case OP_AND:
			NEXT_DIRECT( CAR(code),
						 CONT_OP( V_AND2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) );

		case OP_AND2:
			if( result != VALUE_F && IS_PAIR(code) ){
				NEXT_DIRECT( CAR(code),
							 CONT_OP( V_AND2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) );
			}else{
				NEXT( C_NEXT(cont), result );
			}

		case OP_OR:
			NEXT_DIRECT( CAR(code),
						 CONT_OP( V_OR2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) );

		case OP_OR2:
			if( result == VALUE_F && IS_PAIR(code) ){
				NEXT_DIRECT( CAR(code),
							 CONT_OP( V_OR2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) );
			}else{
				NEXT( C_NEXT(cont), result );
			}
				
		case OP_READ_EVAL:
			{
				Value stat = stream_read( V2STREAM(code) );
				if( opt_trace ) printf( "trace: %s\n", v2s_limit(stat,100) );
				if( stat != V_EOF ){
					// stat = syntax_expand1( stat );
						
					Value compile_hook = bundle_get( bundle_cur, SYM_A_COMPILE_HOOK_A, NIL );
					if( compile_hook != NIL ){
						// *compile-hook* があればコンパイルする
						stat = cons3( compile_hook, cons3( V_QUOTE, stat, NIL ), NIL );
						// printf( "READ_EVAL: %s\n", v2s(stat) );
						NEXT( CONT( stat, C_BUNDLE(cont),
									CONT_OP( V_READ_EVAL2, code, C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
					}else{
						NEXT( CONT( stat, C_BUNDLE(cont),
									CONT_OP( V_READ_EVAL, code, C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
					}
				}else{
					NEXT( C_NEXT(cont), NIL );
				}
			}
				
		case OP_READ_EVAL2:
			NEXT( CONT( result, C_BUNDLE(cont),
						CONT_OP( V_READ_EVAL, code, C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
		}
	}
	assert(0);
}

