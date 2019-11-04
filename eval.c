#include "lisp.h"

//********************************************************
// Evaluation
//********************************************************

Value cont_error( char *str, Value cont )
{
	return continuation_new( cons3( V(SYM_ERROR), (Value)string_new(str), NIL ),
							 CONTINUATION_BUNDLE(cont), CONTINUATION_NEXT(cont) );
}

Value call( Context *ctx, Value _lmd, Value vals, Value cont, Value *result )
{
	Value orig_vals = vals;
	switch( TYPE_OF(_lmd) ){
	case TYPE_LAMBDA:
		{
			Lambda *lmd = V2LAMBDA(_lmd);
			switch( lmd->type ){
			case LAMBDA_TYPE_LAMBDA:
			case LAMBDA_TYPE_MACRO:
				{
					Bundle *bundle = bundle_new( lmd->bundle );
					bundle->lambda = lmd;
					for( Value cur=lmd->args; cur != NIL; cur=CDR(cur), vals=CDR(vals) ){
						if( IS_PAIR(cur) ){
							if( !IS_PAIR(vals) ){
								printf( "call: %s orig_vals: %s lmd: %s\n", v2s(V(lmd)), v2s(orig_vals), v2s(lmd->args) );
							}
							if( !IS_SYMBOL(CAR(cur)) ) return cont_error( "not symbol", cont );
							bundle_define( bundle, V2SYMBOL(CAR(cur)), CAR(vals) );
						}else{
							if( !IS_SYMBOL(cur) ) return cont_error( "not symbol", cont );
							bundle_define( bundle, V2SYMBOL(cur), vals );
							break;
						}
					}
					return continuation_new( cons(V_BEGIN, lmd->body), bundle, CONTINUATION_NEXT(cont) );
				}
			}
		}
	case TYPE_CFUNC:
		{
			CFunc *func = V2CFUNC(_lmd);
			int arity = func->arity;
			if( arity == CFUNC_ARITY_RAW ){
				return ((CFunction)func->func)( ctx, vals, cont, result );
			}else{
				bool has_rest = (arity<0);
				int arg_num = has_rest?(-1-arity):(arity);
				Value v[8] = {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};
				
				for( int i=0; i<arg_num; i++ ){
					if( vals == NIL ) goto error_args;
					v[i] = CAR(vals);
					vals = CDR(vals);
				}
				
				if( has_rest ){
					v[arg_num] = vals;
					arg_num++;
				}else{
					if( vals != NIL ) goto error_args;
				}
				
				switch( arg_num ){
				case 0: *result = ((CFunction0)func->func)( ctx ); break;
				case 1: *result = ((CFunction1)func->func)( ctx, v[0] ); break;
				case 2: *result = ((CFunction2)func->func)( ctx, v[0], v[1] ); break;
				case 3: *result = ((CFunction3)func->func)( ctx, v[0], v[1], v[2] ); break;
				case 4: *result = ((CFunction4)func->func)( ctx, v[0], v[1], v[2], v[3] ); break;
				case 5: *result = ((CFunction5)func->func)( ctx, v[0], v[1], v[2], v[3], v[4] ); break;
				case 6: *result = ((CFunction6)func->func)( ctx, v[0], v[1], v[2], v[3], v[4], v[5] ); break;
				case 7: *result = ((CFunction7)func->func)( ctx, v[0], v[1], v[2], v[3], v[4], v[5], v[6] ); break;
				default:
					assert(0);
				}
				
				// handle error
				if( IS_ERROR(*result) ){
					Error *err = V2ERROR(*result);
					*result = NIL;
					char str[128];
					snprintf( str, sizeof(str)-1, "in %s", v2s(V(func->name)));
					return continuation_new( cons4( V(SYM_ERROR), (Value)err->str, (Value)string_new(str), NIL),
											   CONTINUATION_BUNDLE(cont), CONTINUATION_NEXT(cont) );
				}else{
					return CONTINUATION_NEXT(cont);
				}
			}
		}
	default:
		assert(0);
	}
 error_args:
	return continuation_new( cons4( V(SYM_ERROR), (Value)string_new("invalid argument number"), _lmd, NIL),
							 CONTINUATION_BUNDLE(cont), CONTINUATION_NEXT(cont) );
}

#define NEXT(_cont,_v) do{ Value r = _v; cont = _cont; result = (r); goto _loop; }while(0)
#define ERROR(mes) do{													\
		Value _code = C_CODE(cont);										\
		NEXT( CONT( cons4( V(SYM_ERROR), (Value)string_new(mes), cons3(V_QUOTE,_code,NIL), NIL ), \
					C_BUNDLE(cont), C_NEXT(cont)), NIL); }while(0)
#define CHECK(x) if(!x){ ERROR("invalid form"); }
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
		return bundle_get( C_BUNDLE(cont), V2SYMBOL(code), NULL );
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

Value eval_loop( Context *ctx, Stream *stream )
{
	Value result = NIL;
	for(;;){
		Value stat = stream_read_value( stream );
		if( opt_trace ) printf( "trace: %s\n", v2s_limit(stat,100) );
		if( stat == V_EOF ) break;

		/*
		  Value compile_hook = bundle_get( ctx->bundle, SYM_A_COMPILE_HOOK_A, NIL );
		  if( compile_hook != NIL ){
		  // *compile-hook* があれば呼び出す
		  stat = cons3( compile_hook, cons3( V_QUOTE, stat, NIL ), NIL );
		  NEXT( CONT( stat, C_BUNDLE(cont),
		  CONT_OP( V_READ_EVAL2, code, C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
		  }else{
		  NEXT( CONT_OP( V_READ_EVAL2, code, C_BUNDLE(cont), C_NEXT(cont) ), stat );
		  }
		*/
				
		stat = normalize_sexp(ctx, stat );
		if( opt_trace ) printf( "trace-ex: %s\n", v2s_limit(stat,1000) );
		result = eval(ctx, stat);
	}
	return result;
}

Value eval( Context *ctx, Value sexp )
{
	printf( "eval: %s\n", v2s(sexp));
	
	int GC_FREQUENCY = 10000;
	int gc_count = GC_FREQUENCY;
	Value result = NIL;
	Value cont = CONT( sexp, ctx->bundle, NIL);
	retain( &result );
	retain( &cont );

 _loop:

	if( gc_count-- <= 0 ){
		gc_run( opt_trace?1:0 );
		gc_count = GC_FREQUENCY;
	}

	if( cont == NIL ){
		release( &result );
		release( &cont );
		return result;
	}
	//printf( "> %s => %s\n", v2sn(result,20), v2sn(C_CODE(cont), 80) );
	
	switch( TYPE_OF(C_CODE(cont)) ){
	case TYPE_UNUSED:
	case TYPE_STRING_BODY:
	case TYPE_POINTER:
	case TYPE_ERROR:
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
			char buf[256];
			sprintf(buf, "symbol '%s' not found", v2s(C_CODE(cont)));
			if( !v ) ERROR(buf);
			NEXT( C_NEXT(cont), v );
		}
	case TYPE_PAIR:
		if( !IS_SPECIAL(CAR(C_CODE(cont))) ){
			NEXT_DIRECT( CAR(C_CODE(cont)),
						 CONT_OP( V_APP, cons( CDR(C_CODE(cont)), NIL ), C_BUNDLE(cont), C_NEXT(cont) ) );
		}

		Value code = CDR(C_CODE(cont));
		Operator op = V2SPECIAL(CAR(C_CODE(cont)))->op;
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
			
		case OP_APP:
			{
				//printf( "OP_APP: %s %s\n", v2s_limit(code,100), v2s_limit(result,10) );
				Value rest, tmp;
				bind2cdr(code,rest,tmp);
				tmp = cons( result, tmp );
				if( rest != NIL ){
					NEXT_DIRECT( CAR(rest), CONT_OP( V_APP, cons(CDR(rest), tmp), C_BUNDLE(cont), C_NEXT(cont) ) );
				}else{
					Value vals = NIL;
					LIST_EACH(it,tmp){
						vals = cons( it, vals );
					}

					Value lmd = CAR(vals);
					if( IS_LAMBDA(lmd) || IS_CFUNC(lmd) ){
						Value val = NIL;
						Value next = call(ctx, lmd, CDR(vals), cont, &val );
						NEXT( next, val );
					}else if( IS_CONTINUATION(lmd) ){
						vals = CDR(vals);
						if( CDR(vals) != NIL ){
							NEXT( lmd, cons( V(SYM_VALUES), vals ));
						}else{
							NEXT( lmd, CAR(vals) );
						}
					}else{
						assert(0);
					}
				}
			}
			
		case OP_DEFINE:
			NEXT_DIRECT(CADR(code),
						CONT_OP( V_DEFINE2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)) );
				
		case OP_DEFINE2:
			bundle_define( C_BUNDLE(cont), V2SYMBOL(code), result );
			NEXT( C_NEXT(cont), NIL );
				
		case OP_SET_I:
			NEXT_DIRECT( CADR(code),
						 CONT_OP( V_SET_I2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)) );
				
		case OP_SET_I2:
			bundle_set( C_BUNDLE(cont), V2SYMBOL(code), result );
			NEXT( C_NEXT(cont), NIL );

		case OP_LAMBDA:
		case OP_MACRO:
			{
				// display_val( "lambda2:", result );
				Lambda *lmd = lambda_new();
				lmd->type = (op==OP_LAMBDA)?LAMBDA_TYPE_LAMBDA:LAMBDA_TYPE_MACRO;
				lmd->bundle = C_BUNDLE(cont);
				lmd->name = NULL;
				lmd->args = CAR(code);
				lmd->body = CDR(code);
				NEXT( C_NEXT(cont), V(lmd) );
			}

		case OP_DEFINE_SYNTAX2:
			//printf("SYNTAX2: %s\n", v2s(code));
			NEXT_DIRECT( CADR(code),
						 CONT_OP( V_DEFINE_SYNTAX22, CAR(code), C_BUNDLE(cont), C_NEXT(cont) ) );

		case OP_DEFINE_SYNTAX22:
			{
				//printf("SYNTAX22: %s %s\n", v2s(code), v2s(result));
				Lambda *lmd = V2LAMBDA(result);
				lmd->type = LAMBDA_TYPE_MACRO;
				bundle_define( C_BUNDLE(cont), V2SYMBOL(code), result );
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
		}
	}
	assert(0);
}

Value normalize_list( Context *ctx,  Value s );
Value normalize_begin( Context *ctx,  Value s );
Value normalize_syntax( Context *ctx,  Value s );

Value normalize_sexp( Context *ctx, Value s )
{
	// printf( "s:%s\n", v2s_limit(s,30) );
	if( !IS_PAIR(s) ) return s;
	if( IS_PAIR(CAR(s)) ) return normalize_list(ctx, s );
	if( TYPE_OF(CAR( s )) != TYPE_SYMBOL ) return s;

	Symbol *sym = V2SYMBOL(CAR( s ));
	Value rest = CDR(s);
	if( sym == SYM_DEFINE ){
		if( IS_SYMBOL(CAR(rest)) ){
			// (define sym val) の形
			return cons( V_DEFINE, normalize_list(ctx, rest) );
		}else if( IS_PAIR(CAR(rest)) ){
			// (define (sym args ...) ... ) の形
			return cons4( V_DEFINE, CAAR(rest),
						  cons3( V_LAMBDA, CDAR(rest), normalize_list(ctx, CDR(rest)) ), NIL );
		}else{
			assert(0);
		}
	}else if( sym == SYM_LAMBDA ){
		return cons3( V_LAMBDA, CAR(rest), normalize_list(ctx, CDR(rest) ) );

	}else if( sym == SYM_DEFINE_SYNTAX2 ){
		return cons( V_DEFINE_SYNTAX2, normalize_list(ctx, rest) );
		
	}else if( sym == SYM_IF ){
		Value _cond, _then, _else;
		bind3cdr( rest, _cond, _then, _else );
		if( _else == NIL ) _else = cons(V_UNDEF, NIL);
		return cons4( V_IF, normalize_sexp(ctx, _cond), normalize_sexp(ctx, _then), normalize_list(ctx, _else) );
		
	}else if( sym == SYM_BEGIN ){
		if( rest == NIL ) return V_UNDEF;
		if( CDR(rest) == NIL ) return normalize_sexp(ctx, CAR(rest));
		return cons( V_BEGIN, normalize_list(ctx, rest) );
		
	}else if( sym == SYM_SET_I ){
		return cons3( V_SET_I, CAR(rest), normalize_sexp(ctx, CDR(rest) ) );
		
	}else if( sym == SYM_QUOTE ){
		return cons( V_QUOTE, rest );
		
	}else{
		s = normalize_syntax(ctx, s);
		return normalize_list(ctx, s);
	}
}

// implicit begin
Value normalize_begin( Context *ctx, Value list )
{
	return normalize_sexp(ctx, cons( (Value)SYM_BEGIN, list) );
}

Value normalize_list( Context *ctx, Value list )
{
	if( IS_PAIR(list) ){
		return cons( normalize_sexp(ctx, CAR(list)), normalize_list(ctx, CDR(list)) );
	}else{
		return list;
	}
}

Value normalize_syntax( Context *ctx,  Value s )
{
	if( !IS_SYMBOL(CAR(s)) ) return s;
	
	//printf("hoge: %s\n", v2s(bundle_get( ctx->bundle, intern("define-syntax"), NIL )));
	Value v = bundle_get( ctx->bundle, V2SYMBOL(CAR(s)), NIL);
	if( !IS_LAMBDA(v) ) return s;
	
	Lambda *lmd = V2LAMBDA(v);
	if( lmd->type != LAMBDA_TYPE_MACRO ){
		return s;
	}

	//printf("normalize_syntax: %s %s\n", v2s(v), v2s(s));
	
	s = eval(ctx, cons5(v, cons(V_QUOTE, cons(s,NIL)), NIL, NIL, NIL));
	
	//printf("normalize_syntax2: %s\n", v2s(s));
	
	return normalize_sexp(ctx, s);
}
