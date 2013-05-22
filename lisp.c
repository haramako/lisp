#include "lisp.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>

Profile prof;

const char *TYPE_NAMES[] = {
	"UNUSED",
	"NIL",
	"BOOL",
	"INT",
	"SYMBOL",
	"STRING",
	"PAIR",
	"LAMBDA",
	"BUNDLE",
	"CONTINUATION",
	"SPECIAL",
	"STREAM",
};

const char* LAMBDA_TYPE_NAME[] = {
	"LAMBDA",
	"CFUNC",
	"MACRO",
	"CMACRO"
};

//********************************************************
// Utility
//********************************************************

static size_t _value_to_str( char *buf, int len, Value v )
{
	int n = 0;
	if( n >= len ) return len;
	
	if( !v ){
		n += snprintf( buf, len, "(NULL)" );
		if( n >= len ) return len;
		return n;
	}
	
	// printf( "%s\n", TYPE_NAMES[(int)TYPE_OF(v)] );
	switch( TYPE_OF(v) ){
	case TYPE_UNUSED:
		assert(0);
	case TYPE_NIL:
		n += snprintf( buf, len,"()" );
		break;
	case TYPE_BOOL:
		n += snprintf( buf, len, (v==VALUE_T)?"#t":"#f" );
		break;
	case TYPE_INT:
		n += snprintf( buf, len, "%lld", V2INT(v) );
		break;
	case TYPE_SYMBOL:
		n += snprintf( buf, len, "%s", STRING_STR(SYMBOL_STR(v)) );
		break;
	case TYPE_STRING:
		n += snprintf( buf, len, "\"%s\"", STRING_STR(v) );
		break;
	case TYPE_LAMBDA:
		{
			if( LAMBDA_NAME(v) != NIL ){
				n += snprintf( buf, len, "(%s:%s:%p)",
							   LAMBDA_TYPE_NAME[LAMBDA_TYPE(v)], STRING_STR(SYMBOL_STR(LAMBDA_NAME(v))), v );
			}else{
				n += snprintf( buf, len, "(%s:%p)", LAMBDA_TYPE_NAME[LAMBDA_TYPE(v)], v );
			}
		}
		break;
	case TYPE_PAIR:
		n += snprintf( buf, len, "(" );
		if( n >= len ) return len;
		bool finished = false;
		while( !finished ){
			switch( TYPE_OF(CDR(v)) ){
			case TYPE_NIL:
				n += _value_to_str( buf+n, len-n, CAR(v) );
				if( n >= len ) return len;
				finished = true;
				break;
			case TYPE_PAIR:
				n += _value_to_str( buf+n, len-n, CAR(v) );
				if( n >= len ) return len;
				n += snprintf( buf+n, len-n, " " );
				if( n >= len ) return len;
				break;
			default:
				n += _value_to_str( buf+n, len-n, CAR(v) );
				if( n >= len ) return len;
				n += snprintf( buf+n, len-n, " . " );
				if( n >= len ) return len;
				n += _value_to_str( buf+n, len-n, CDR(v) );
				if( n >= len ) return len;
				finished = true;
				break;
			}
			v = CDR(v);
		}
		n += snprintf( buf+n, len-n, ")" );
		break;
	case TYPE_BUNDLE:
		n += snprintf( buf+n, len-n, "(BUNDLE:%p)", v );
		break;
	case TYPE_CONTINUATION:
		n += snprintf( buf+n, len-n, "(CONTINUATION:%p)", v );
		break;
	case TYPE_SPECIAL:
		n += snprintf( buf+n, len-n, "%s", SPECIAL_STR(v) );
		break;
	case TYPE_STREAM:
		n += snprintf( buf+n, len-n, "(STREAM:%s)", STRING_STR(STREAM_FILENAME(v)) );
		break;
	default:
		assert(0);
	}

	if( n >= len ) return len;
	return n;
}

size_t value_to_str( char *buf, int len, Value v )
{
	size_t n = _value_to_str( buf, len, v );
	if( n >= len ) buf[len-1] = '\0'; // snprintf()が長さ以上に書き込むと終端してくれないので、念のため、終端させておく
	return n;
}

char* v2s( Value v )
{
	return v2s_limit( v, 10240 );
}

char* v2s_limit( Value v, int limit )
{
	char buf[10240+1];
	assert( limit < sizeof(buf) );
	int len = value_to_str(buf, limit, v);
	if( len >= (limit-1) ) strcpy( buf+limit-4, "..." );
	return STRING_STR(string_new(buf));
}

void vdump( Value v )
{
	printf( "%s\n", v2s(v) );
}

bool eq( Value a, Value b )
{
	if( TYPE_OF(a) != TYPE_OF(b) ) return false;
	switch( TYPE_OF(a) ){
	case TYPE_NIL:
		return true;
	case TYPE_INT:
		return ( V2INT(a) == V2INT(b) );
	default:
		return ( a == b );
	}
}

bool eqv( Value a, Value b )
{
	if( eq(a,b) ) return true;
	switch( TYPE_OF(a) ){
	case TYPE_STRING:
		if( !IS_STRING(b) ) return false;
		return ( strcmp(STRING_STR(a),STRING_STR(b)) == 0 );
	default:
		return false;
	}
}

bool equal( Value a, Value b )
{
	if( eqv(a,b) ) return true;
	switch( TYPE_OF(a) ){
	case TYPE_PAIR:
		if( !IS_PAIR(b) ) return false;
		if( equal( CAR(a), CAR(b) ) ){
			return equal(CDR(a), CDR(b));
		}else{
			return false;
		}
	default:
		return false;
	}
}

unsigned int hash_eq( Value v )
{
	switch( TYPE_OF(v) ){
	case TYPE_INT:
		return (unsigned int)V2INT(v);
	default:
		return (unsigned int)(((uintptr_t)v) >> 4) * 31;
	}
}

unsigned int hash_eqv( Value v )
{
	switch( TYPE_OF(v) ){
	case TYPE_STRING:
		{
			char *str = STRING_STR(v);
			size_t len = strlen(str);
			int hash = 0;
			for( int i=0; i<len; i++ ){
				hash = hash * 31 + str[i];
			}
			return hash;
		}
	default:
		return hash_eq(v);
	}
}

unsigned int hash_equal( Value v )
{
	switch( TYPE_OF(v) ){
	case TYPE_PAIR:
		return hash_equal(CAR(v)) * 31 + hash_equal(CDR(v));
	default:
		return hash_eqv(v);
	}
}

//********************************************************
// Int
//********************************************************

Value int_new( int64_t i )
{
	Value v = gc_new(TYPE_INT);
	v->d.number = i;
	return v;
}

//********************************************************
// Symbol
//********************************************************

Value symbol_root = NULL;

Value intern( char *sym )
{
	Value str = string_new(sym);
	DictEntry *entry = bundle_find( symbol_root, str, false, true );
	if( entry->val == NIL ){
		Value val = gc_new(TYPE_SYMBOL);
		SYMBOL_STR(val) = str;
		entry->val = val;
	}
	return entry->val;
}

//********************************************************
// String
//********************************************************

Value string_new( char *str )
{
	Value v = gc_new(TYPE_STRING);
	char *s = malloc(strlen(str)+1);
	assert( s );
	strcpy( s, str );
	STRING_STR(v) = s;
	return v;
}

//********************************************************
// Lambda
//********************************************************

Value lambda_new()
{
	Value v = gc_new(TYPE_LAMBDA);
	LAMBDA_NAME(v) = NIL;
	LAMBDA_ARGS(v) = NIL;
	LAMBDA_BODY(v) = NIL;
	LAMBDA_BUNDLE(v) = NIL;
	LAMBDA_FUNC(v) = NULL;
	return v;
}

//********************************************************
// Pair
//********************************************************

Value cons( Value car, Value cdr )
{
	Value v = gc_new(TYPE_PAIR);
	CAR(v) = car;
	CDR(v) = cdr;
	return v;
}

size_t list_length( Value v )
{
	size_t len = 0;
	for( Value cur=v; cur != NIL; cur = CDR(cur) ) len++;
	return len;
}

Value list_copy( Value list )
{
	if( !IS_PAIR(list) ) return list;
	Value r = cons( CAR(list), NIL );
	Value tail = r;
	for( Value cur=CDR(list); cur != NIL; cur=CDR(cur) ){
		tail = CDR(tail) = cons( CAR(cur), NIL );
	}
	return r;
}

//********************************************************
// Bundle
//********************************************************

Value bundle_cur = NULL;

Value bundle_new( Value upper )
{
	Value v = gc_new(TYPE_BUNDLE);
	BUNDLE_DICT(v) = dict_new( hash_eqv, eqv );
	BUNDLE_UPPER(v) = upper;
	BUNDLE_LAMBDA(v) = NIL;
	return v;
}

DictEntry* bundle_find( Value b, Value sym, bool find_upper, bool create )
{
	if( find_upper && BUNDLE_UPPER(b) != NIL ){
		// 自分のを探す
		DictEntry *entry = dict_find( BUNDLE_DICT(b), sym, false );
		if( entry ) return entry;
		
		// 親のを探す
		entry = bundle_find( BUNDLE_UPPER(b), sym, find_upper, false );
		if( entry ) return entry;

		// 新しく作る
		if( create ){
			return bundle_find( b, sym, find_upper, true );
		}else{
			return NULL;
		}
	}else{
		return dict_find( BUNDLE_DICT(b), sym, create );
	}
}

void bundle_set( Value b, Value sym, Value v )
{
	DictEntry *entry = bundle_find( b, sym, true, false );
	if( !entry ){
		printf( "bundle_set: %s\n", v2s(sym) );
		assert( !"cannot set" );
	}
	entry->val = v;
}

void bundle_define( Value b, Value sym, Value v )
{
	// サイズが大きいならリサイズ
	Dict *d = BUNDLE_DICT(b);
	if( d->use >= d->size ) BUNDLE_DICT(b) = dict_rehash(d);
	
	DictEntry *entry = bundle_find( b, sym, false, true );
	if( entry->val != NIL ){
		// printf( "bundle_define: %s\n", v2s(sym) );
		// assert( !"already set" );
	}
	entry->val = v;
	
	if( IS_LAMBDA(v) && LAMBDA_NAME(v) == NIL ) LAMBDA_NAME(v) = sym;
}

Value bundle_get( Value b, Value sym, Value def )
{
	DictEntry *entry = bundle_find( b, sym, true, false );
	if( entry ){
		return entry->val;
	}else{
		return def;
	}
}

//********************************************************
// Continuation
//********************************************************

Value continuation_new( Value code, Value bundle, Value next )
{
	Value v = gc_new( TYPE_CONTINUATION );
	CONTINUATION_CODE(v) = code;
	CONTINUATION_BUNDLE(v) = bundle;
	CONTINUATION_NEXT(v) = next;
	return v;
}

//********************************************************
// Parsing
//********************************************************

int _parse( Value s, Value *result );
	
void _skip_space( Value s )
{
	for(;;){
		int c = stream_getc(s);
		switch( c ){
		case '\n': case '\r': case ' ': case '\t': 
			break;
		case ';':
			for(;;){
				c = stream_getc(s);
				if( c == '\n' || c == '\r' || c == '\0' || c == -1 ) break;
			}
			break;
		default:
			stream_ungetc(c, s);
			return;
		}
	}
}

int _parse_list( Value s, Value *result )
{
	_skip_space(s);
	int c, err;
	Value val, cdr;
	switch( c = stream_getc(s) ){
	case -1:
	case ')':
	case '\0':
		stream_ungetc(c,s);
		*result = NIL;
		return 0;
	default:
		stream_ungetc(c,s);
		err = _parse( s, &val );
		if( err ) return err;

		if( val == SYM_DOT ){
			err = _parse( s, &cdr );
			if( err ) return err;
			*result = cdr;
			_skip_space(s);
			return 0;
		}else{
			err = _parse_list( s, &cdr );
			if( err ) return err;
			*result = cons( val, cdr );
			return 0;
		}
	}
}

static inline bool _is_val_char( int c )
{
	switch( c ){
	case ' ': case '\t': case '\n': case '\r': case '(': case ')': case '\0': case -1:
		return false;
	default:
		return true;
	}
}

static int _read_token( Value s, char *buf )
{
	int c;
	int i = 0;
	for(;;){
		c = stream_getc(s);
		if( !_is_val_char(c) ) break;
		buf[i++] = c;
	}
	buf[i] = '\0';
	stream_ungetc( c, s );
	return 0;
}

static Value _parse_token( char *str )
{
	if( isnumber(str[0]) || (str[0] == '-' && isnumber(str[1])) ){
		for( char *s = str+1; *s != '\0'; s++ ){
			if( !isnumber(*s) ){
				return intern(str);
			}
		}
		return INT2V(atoi(str));
	}else{
		return intern( str );
	}
}

int _parse( Value s, Value *result )
{
	char buf[1024];
	int err;
	_skip_space( s );
	int c;
	int level;
	switch( c = stream_getc(s) ){
	case -1:
	case '\0':
		return 0;
	case '(':
		err = _parse_list( s, result );
		if( err ) return err;
		if( stream_getc(s) != ')' ) return -2;
		return 0;
	case ')':
		assert(!"paren not matched");
	case '#':
		switch( c = stream_getc(s) ){
		case 't':
			*result = VALUE_T;
			return 0;
		case 'f':
			*result = VALUE_F;
			return 0;
		case 'p':
			// for debug pringint
			err = _parse(s,result);
			if( err ) return err;
			*result = cons3( intern("*tee*"), *result, NIL );
			return 0;
		case '|':
			// multi-line comment
			level = 1;
			for(;;){
				switch( c = stream_getc(s) ){
				case '|':
					if( stream_getc(s) == '#' ){
						level--;
						if( level <= 0 ) return _parse( s, result );
					}
				case '#':
					if( stream_getc(s) == '|' ) level++;
					break;
				case -1:
				case '\0':
					return -4;
				default:
					break;
				}
			}
			return 0;
		case '(':
			// vector literal
			// とりあえず、リストにする
			err = _parse_list( s, result );
			if( err ) return err;
			if( stream_getc(s) != ')' ) return -2;
			return 0;
		default:
			assert(0);
		}
	case '"':
		{
			int i = 0;
			while( (c = stream_getc(s)) != '"') buf[i++] = c;
			buf[i] = '\0';
			*result = string_new(buf);
			return 0;
		}
	case '\'':
		err = _parse( s, result );
		if( err ) return err;
		*result = cons( V_QUOTE, cons(*result,NIL) );
		return err;
	case '`':
		err = _parse( s, result );
		if( err ) return err;
		*result = cons( SYM_QUASIQUOTE, cons(*result,NIL) );
		return err;
	case ',':
		{
			Value sym = SYM_UNQUOTE;
			if( (c = stream_getc(s)) == '@' ){
				sym = SYM_UNQUOTE_SPLICING;
			}else{
				stream_ungetc(c,s);
			}
			
			err = _parse( s, result );
			if( err ) return err;
			*result = cons( sym, cons(*result,NIL) );
			return err;
		}
	default:
		{
			stream_ungetc( c, s );
			char buf[1024];
			_read_token( s, buf );
			*result = _parse_token( buf );
			return 0;
		}
	}
	assert(0);
}

//********************************************************
// Stream
//********************************************************

Value stream_new( FILE *fd, bool close, char *filename )
{
	Value v = gc_new( TYPE_STREAM );
	STREAM_FD(v) = fd;
	STREAM_CLOSE(v) = close;
	STREAM_FILENAME(v) = string_new(filename);
	STREAM_LINE(v) = 1;
	return v;
}

int stream_getc( Value s )
{
	int c = fgetc( STREAM_FD(s) );
	if( c == '\n' ) STREAM_LINE(s) = STREAM_LINE(s) + 1;
	return c;
}

void stream_ungetc( int c, Value s )
{
	if( c == '\n' ) STREAM_LINE(s) = STREAM_LINE(s) - 1;
	ungetc( c, STREAM_FD(s) );
}

Value stream_read( Value s )
{
	Value val = V_EOF;
	int err = _parse( s, &val );
	if( err ){
		printf( "parse error: err=%d\n", err );
		assert(0);
	}
	return val;
}

Value stream_write( Value s, Value v )
{
	char buf[10240];
	value_to_str(buf, sizeof(buf), v);
	fputs( buf, STREAM_FD(s) );
	return NIL;
}

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
	switch( LAMBDA_TYPE(lmd) ){
	case LAMBDA_TYPE_LAMBDA:
	case LAMBDA_TYPE_MACRO:
		{
			Value bundle = bundle_new( LAMBDA_BUNDLE(lmd) );
			BUNDLE_LAMBDA(bundle) = lmd;
			for( Value cur=LAMBDA_ARGS(lmd); cur != NIL; cur=CDR(cur), vals=CDR(vals) ){
				if( IS_PAIR(cur) ){
					if( !IS_PAIR(vals) ){
						printf( "call: %s orig_vals: %s lmd: %s\n", v2s(LAMBDA_BODY(lmd)), v2s(orig_vals), v2s(LAMBDA_ARGS(lmd)) );
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
	case LAMBDA_TYPE_CFUNC:
	case LAMBDA_TYPE_CMACRO:
		{
			Value next = LAMBDA_FUNC(lmd)( vals, cont, result );
			return next;
		}
	}
	assert(0);
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

Value eval_loop( Value code )
{
	int gc_count = 10000;
	Value result = NIL;
	Value cont = CONT_OP( V_READ_EVAL, code, bundle_cur, NIL );
 _loop:
	
	if( gc_count-- <= 0 ){
		retain( cont );
		retain( result );
		gc_run( opt_trace?1:0 );
		release( result );
		release( cont );
		gc_count = 10000;
	}
	
	if( cont == NIL ) return result;
	// printf( "> %s\n", v2s_limit(C_CODE(cont), 80) );
	// display_val( "=> ", result );
	switch( TYPE_OF(C_CODE(cont)) ){
	case TYPE_UNUSED:
		assert(0);
	case TYPE_NIL:
	case TYPE_INT:
	case TYPE_LAMBDA:
	case TYPE_BOOL:
	case TYPE_STRING:
	case TYPE_BUNDLE:
	case TYPE_CONTINUATION:
	case TYPE_SPECIAL:
	case TYPE_STREAM:
		NEXT( C_NEXT(cont), C_CODE(cont) );
	case TYPE_SYMBOL:
		{
			DictEntry *found = bundle_find( C_BUNDLE(cont), C_CODE(cont), true, false );
			if( !found ) ERROR( "symbol not found" );
			NEXT( C_NEXT(cont), found->val );
		}
	case TYPE_PAIR:
		if( !IS_SPECIAL(CAR(C_CODE(cont))) ){
			NEXT( CONT( CAR(C_CODE(cont)), C_BUNDLE(cont), 
						CONT_OP( V_CALL0, CDR(C_CODE(cont)), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
		}
		
		Value code = CDR(C_CODE(cont));
		Operator op = SPECIAL_OP(CAR(C_CODE(cont)));
		switch( op ){
		case OP_BEGIN:
			// display_val( "OP_BEGIN: ", code );
			if( code == NIL ){
				NEXT( C_NEXT(cont), result );
			}else if( CDR(code) == NIL ){
				NEXT( CONT( CAR(code), C_BUNDLE(cont), C_NEXT(cont)), result );
			}else{
				NEXT( CONT( CAR(code), C_BUNDLE(cont),
							CONT_OP( V_BEGIN, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
			}
			
		case OP_QUOTE:
			NEXT( C_NEXT(cont), CAR(code) );
			
		case OP_CALL0:
			// display_val( "OP_CALL0: ", cons( result, C_CODE(cont) ) );
			switch( TYPE_OF(result) ){
			case TYPE_LAMBDA:
				{
					Value args = cons(result,NIL);
					Value lmd = result;
					switch( LAMBDA_TYPE(lmd) ){
					case LAMBDA_TYPE_LAMBDA:
					case LAMBDA_TYPE_CFUNC:
						if( code != NIL ){
							NEXT( CONT( CAR(code), C_BUNDLE(cont),
										CONT_OP( V_CALL1, cons4( CDR(code), args, args, NIL), C_BUNDLE(cont), C_NEXT(cont) ) ),
								  NIL );
						}else{
							Value res = NIL;
							Value next = call( lmd, NIL, cont, &res);
							NEXT( next, res );
						}
					default:
						ERROR( "cannot call" );
					}
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
					NEXT( CONT( CAR(rest), C_BUNDLE(cont),
								CONT_OP( V_CALL1, cons4( CDR(rest), vals, CDR(tmp), NIL ), C_BUNDLE(cont), C_NEXT(cont) ) ),
						  NIL );
				}else{
					Value lmd = CAR(vals);
					if( IS_LAMBDA(lmd) ){
						switch( LAMBDA_TYPE(lmd) ){
						case LAMBDA_TYPE_LAMBDA:
							NEXT( call( lmd, CDR(vals), cont, NULL ), NIL );
						case LAMBDA_TYPE_CFUNC:
							{
								Value val = NIL;
								Value next = LAMBDA_FUNC(lmd)( CDR(vals), cont, &val );
								NEXT( next, val );
							}
						default:
							assert(0);
						}
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
					NEXT( CONT( CADR(code), C_BUNDLE(cont),
								CONT_OP( V_DEFINE2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)) ), NIL );
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
			NEXT( CONT( CAR(CDR(code)), C_BUNDLE(cont),
						CONT_OP( V_SET_I2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)) ), NIL );
				
		case OP_SET_I2:
			bundle_set( C_BUNDLE(cont), code, result );
			NEXT( C_NEXT(cont), NIL );

		case OP_LET:
		case OP_LET_A:
		case OP_LETREC:
			// display_val( "LET: ", code );
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
			}else if( IS_SYMBOL( CAR(code) ) ){
				CHECK( op == OP_LET );
				// named let
				Value bundle = bundle_new( C_BUNDLE(cont) );
				Value name = CAR(code);
				Value lmd = lambda_new();
				LAMBDA_TYPE(lmd) = LAMBDA_TYPE_LAMBDA;
				LAMBDA_BUNDLE(lmd) = bundle;
				Value lmd_args = cons( NIL, NIL );
				Value lmd_args_tail = lmd_args;
				Value lmd_vals = cons( NIL, NIL );
				Value lmd_vals_tail = lmd_vals;
				for( Value cur=CADR(code); cur != NIL; cur=CDR(cur) ){
					lmd_args_tail = CDR(lmd_args_tail) = cons( CAR(CAR(cur)), NIL);
					lmd_vals_tail = CDR(lmd_vals_tail) = cons( CADR(CAR(cur)), NIL);
				}
				LAMBDA_ARGS(lmd) = CDR(lmd_args);
				LAMBDA_BODY(lmd) = CDDR(code);
				bundle_define( bundle, name, lmd );
				Value call_form = cons( lmd, CDR(lmd_vals) );
				//display_val( "let: ", cons3( LAMBDA_ARGS(lmd), LAMBDA_BODY(lmd), NIL) );
				//display_val( "let: ", call_form );
				NEXT( CONT( call_form, bundle, C_NEXT(cont)), NIL );
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
				bind2cdr( code, LAMBDA_ARGS(lmd), LAMBDA_BODY(lmd) );
				NEXT( C_NEXT(cont), lmd );
			}

		case OP_EXEC_MACRO:
			// display_val( "EXEC_MACRO :", result );
			NEXT( CONT( result, C_BUNDLE(cont), C_NEXT(cont) ), NIL );

		case OP_DEFINE_SYNTAX:
			{
				bundle_define( C_BUNDLE(cont), CAR(code), CADR(code) );
				NEXT( C_NEXT(cont), NIL );
			}
				
		case OP_IF:
			NEXT( CONT( CAR(code), C_BUNDLE(cont),
						CONT_OP( V_IF2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
			
		case OP_IF2:
			if( result != VALUE_F ){
				NEXT( CONT( CAR(code), C_BUNDLE(cont), C_NEXT(cont) ), NIL );
			}else{
				NEXT( CONT_OP( V_BEGIN, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ), NIL );
			}

		case OP_AND:
			NEXT( CONT( CAR(code), C_BUNDLE(cont),
						CONT_OP( V_AND2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );

		case OP_AND2:
			if( result != VALUE_F && IS_PAIR(code) ){
				NEXT( CONT( CAR(code), C_BUNDLE(cont),
							CONT_OP( V_AND2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
			}else{
				NEXT( C_NEXT(cont), result );
			}

		case OP_OR:
			NEXT( CONT( CAR(code), C_BUNDLE(cont),
						CONT_OP( V_OR2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );

		case OP_OR2:
			if( result == VALUE_F && IS_PAIR(code) ){
				NEXT( CONT( CAR(code), C_BUNDLE(cont),
							CONT_OP( V_OR2, CDR(code), C_BUNDLE(cont), C_NEXT(cont) ) ), NIL );
			}else{
				NEXT( C_NEXT(cont), result );
			}
				
		case OP_READ_EVAL:
			{
				Value stat = stream_read( code );
				if( opt_trace ) printf( "trace: %s\n", v2s_limit(stat,100) );
				if( stat != V_EOF ){
					// stat = syntax_expand1( stat );
						
					Value compile_hook = bundle_get( bundle_cur, SYM_A_COMPILE_HOOK_A, NIL );
					if( compile_hook != NIL ){
						// *compile-hook* があればコンパイルする
						stat = cons3( compile_hook, cons3( V_QUOTE, stat, NIL ), NIL );
						// display_val( "READ_EVAL: ", stat );
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

Value list_tail( Value list )
{
	if( list == NIL ) return NIL;
	for(;CDR(list) != NIL; list=CDR(list));
	return list;
}

Value _syntax_expand_body( Value body, Dict *bundle )
{
	body = list_copy(body);
	for( Value cur=body; cur != NIL; cur=CDR(cur) ){
		Value found = dict_get( bundle, CAR(cur) );
		if( found ){
			if( IS_PAIR(CDR(cur)) && CADR(cur) == SYM_DOT3 ){
				Value next = CDDR(cur);
				if( CAR(found) != SYM_SYNTAX_REST ) assert(0);
				found = CDR(found);
				if( found != NIL ){
					found = list_copy( found );
					CAR(cur) = CAR(found);
					CDR(cur) = CDR(found);
					cur = list_tail( cur );
				}
				CDR(cur) = next;
			}else{
				CAR(cur) = found;
			}
		}else if( IS_PAIR(CAR(cur)) ){
			CAR(cur) = _syntax_expand_body( CAR(cur), bundle );
		}
	}
	return body;
}

bool _syntax_match( Value rule, Value code, Dict *bundle )
{
	// printf( "syntax_match: %s %s\n", v2s(rule), v2s(code) );
	for( Value c = rule; c != NIL; c=CDR(c), code=CDR(code) ){
		if( IS_PAIR(CDR(c)) && CADR(c) == SYM_DOT3 ){
			dict_set( bundle, CAR(c), cons( SYM_SYNTAX_REST, code ));
			// printf( "bind %s => %s\n", v2s(CAR(c)), v2s(code) );
			return true;
		}else{
			if( !IS_PAIR(code) ) return false;
			if( IS_PAIR(CAR(c)) ){
				if( !_syntax_match( CAR(c), CAR(code), bundle ) ) return false;
			}else if(IS_SYMBOL(CAR(c)) ){
				dict_set( bundle, CAR(c), CAR(code) );
				// printf( "bind %s => %s\n", v2s(CAR(c)), v2s(CAR(code)) );
			}else{
				assert(0);
			}
		}
	}
	if( code != NIL ) return false;
	return true;
}

Value syntax_expand1( Value code )
{
	if( !IS_PAIR(code) ) return code;
	Value syntax = bundle_get( bundle_cur, CAR(code), NIL );
	if( !IS_PAIR(syntax) ) return code;
	Value sym, keywords, rules;
	bind3cdr( syntax, sym, keywords, rules );
	if( sym != SYM_SYNTAX_RULES ) return code;
	if( keywords == NULL || rules == NULL ) assert(0);
	
	// printf( "matching: %s %s\n", v2s(rules), v2s(code) );
	// 名前のバインド
	Dict *bundle = dict_new( hash_equal, equal );
	for( Value cur=rules; cur != NIL; cur=CDR(cur) ){
		// printf( "rule: %s\n", v2s(CAAR(cur)) );
		bool matched = _syntax_match( CAAR(cur), code, bundle );
		if( matched ){
			Value new_code = _syntax_expand_body( CADR(CAR(cur)), bundle );
			// printf( "expand-syntax: %s => %s\n", v2s(code), v2s(new_code) );
			dict_free( bundle );
			return new_code;
		}
	}
	dict_free( bundle );
	printf( "no pattern matched" );
	assert(0);
}

//********************************************************
// Initialization
//********************************************************

void register_cfunc( char *sym, LambdaType type, CFunction func )
{
	Value v = lambda_new();
	LAMBDA_FUNC(v) = func;
	LAMBDA_TYPE(v) = type;
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
	fprintf(stderr, "\nError: signal %d:\n", sig);
	backtrace_symbols_fd(array+3, (int)size-3, 2/*=stderr*/);
	if( V_SRC_FILE ){
		printf( "%s:%d: error\n", STRING_STR(STREAM_FILENAME( V_SRC_FILE )), STREAM_LINE( V_SRC_FILE ) );
	}
	exit(1);
}

Value NIL = NULL;
Value VALUE_T = NULL;
Value VALUE_F = NULL;
Value V_EOF = NULL;
Value V_STDOUT = NULL;
Value V_STDIN = NULL;
Value V_SRC_FILE = NULL;
Value V_END_OF_LINE = NULL;
Value V_BEGIN;
Value V_CALL0;
Value V_CALL1;
Value V_QUOTE;
Value V_DEFINE, V_DEFINE2;
Value V_SET_I, V_SET_I2;
Value V_LET, V_LET_A, V_LETREC, V_LET2, V_LET3;
Value V_LAMBDA, V_MACRO, V_EXEC_MACRO, V_DEFINE_SYNTAX;
Value V_IF, V_IF2, V_AND, V_AND2, V_OR, V_OR2;
Value V_READ_EVAL, V_READ_EVAL2;

Value SYM_A_DEBUG_A;
Value SYM_A_COMPILE_HOOK_A;
Value SYM_QUASIQUOTE;
Value SYM_UNQUOTE;
Value SYM_UNQUOTE_SPLICING;
Value SYM_CURRENT_INPUT_PORT;
Value SYM_CURRENT_OUTPUT_PORT;
Value SYM_END_OF_LINE;
Value SYM_VALUES;
Value SYM_DOT, SYM_DOT3, SYM_ERROR, SYM_SYNTAX_RULES, SYM_SYNTAX_REST;

Value _operator( char *sym, Operator op ){
	Value v = gc_new(TYPE_SPECIAL);
	SPECIAL_OP(v) = op;
	SPECIAL_STR(v) = sym;
	bundle_define( bundle_cur, intern(sym), v );
	return retain(v);
}

bool opt_trace = false;
bool opt_debug = false;

void init()
{
	init_prelude(true);
}

void init_prelude( bool with_prelude )
{
	signal( SIGABRT, handler );
	signal( SIGSEGV, handler );
	
	gc_init();

	NIL = gc_new(TYPE_NIL);
	VALUE_T = gc_new(TYPE_BOOL);
	VALUE_F = gc_new(TYPE_BOOL);
	
	bundle_cur = bundle_new( NIL );
	retained = NIL;
	symbol_root = bundle_new( NIL );
	
	V_EOF = retain(gc_new(TYPE_SPECIAL));
	V_STDIN = retain(stream_new(stdin, false, "stdin" ));
	V_STDOUT = retain(stream_new(stdout, false, "stdout" ));
	V_END_OF_LINE = string_new("\n");
	V_BEGIN = _operator("begin", OP_BEGIN);
	V_CALL0 = _operator("*call0*", OP_CALL0);
	V_CALL1 = _operator("*call1*", OP_CALL1);
	V_QUOTE = _operator("quote", OP_QUOTE);
	V_DEFINE = _operator("define", OP_DEFINE);
	V_DEFINE2 = _operator("*define2*", OP_DEFINE2);
	V_SET_I = _operator("set!", OP_SET_I);
	V_SET_I2 = _operator("*set!2*", OP_SET_I2);
	V_LET = _operator("let", OP_LET);
	V_LET_A = _operator("let*", OP_LET_A);
	V_LETREC = _operator("letrec", OP_LETREC);
	V_LET2 = _operator("*let2*", OP_LET2);
	V_LET3 = _operator("*let3*", OP_LET3);
	V_LAMBDA = _operator("lambda", OP_LAMBDA);
	V_MACRO = _operator("macro", OP_MACRO);
	V_DEFINE_SYNTAX = _operator("define-syntax", OP_DEFINE_SYNTAX);
	V_EXEC_MACRO = _operator("*exec-macro*", OP_EXEC_MACRO);
	V_IF = _operator("if", OP_IF);
	V_IF2 = _operator("*if2*", OP_IF2);
	V_AND = _operator("and", OP_AND);
	V_AND2 = _operator("*and2*", OP_AND2);
	V_OR = _operator("or", OP_OR);
	V_OR2 = _operator("*or2*", OP_OR2);
	V_READ_EVAL = _operator("*read-eval*", OP_READ_EVAL);
	V_READ_EVAL2 = _operator("*read-eval2*", OP_READ_EVAL2);

	SYM_A_DEBUG_A = intern("*debug*");
	SYM_A_COMPILE_HOOK_A = intern("*compile-hook*");
	SYM_QUASIQUOTE = intern("quasiquote");
	SYM_UNQUOTE = intern("unquote");
	SYM_UNQUOTE_SPLICING = intern("unquote-splicing");
	SYM_CURRENT_INPUT_PORT = intern("current-input-port");
	SYM_CURRENT_OUTPUT_PORT = intern("current-output-port");
	SYM_END_OF_LINE = intern("end-of-line");
	SYM_VALUES = intern("VALUES");
	SYM_DOT = intern(".");
	SYM_DOT3 = intern("...");
	SYM_ERROR = intern("error");
	SYM_SYNTAX_RULES = intern("syntax-rules");
	SYM_SYNTAX_REST = intern("*syntax-rest*");

	bundle_define( bundle_cur, SYM_CURRENT_INPUT_PORT, V_STDIN );
	bundle_define( bundle_cur, SYM_CURRENT_OUTPUT_PORT, V_STDOUT );
	bundle_define( bundle_cur, SYM_END_OF_LINE, V_END_OF_LINE );

	cfunc_init();

	if( with_prelude ){
		FILE *fd = fopen( "prelude.scm", "r" );
		if( !fd ){
			printf( "cannot open prelude.sch\n" );
			exit(1);
		}
		eval_loop( stream_new(fd,true,"prelude.sch") );
	}
}

void finalize()
{
	bundle_cur = NULL;
	retained = NULL;
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

