#include "lisp.h"
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <stdlib.h>
#include <sys/stat.h>

static Value _identity( Value bundle, Value v )
{
	return v;
}

static Value _eq_p( Value bundle, Value args )
{
	if( args == NIL ) return VALUE_T;
	Value v = CAR(args);
	LIST_EACH( x, CDR(args) ){
		if( !eq( v, x ) ) return VALUE_F;
	}
	return VALUE_T;
}

static Value _eqv_p( Value bundle, Value args )
{
	if( args == NIL ) return VALUE_T;
	Value v = CAR(args);
	LIST_EACH( x, CDR(args) ){
		if( !eqv( v, x ) ) return VALUE_F;
	}
	return VALUE_T;
}

static Value _equal_p( Value bundle, Value args )
{
	if( args == NIL ) return VALUE_T;
	Value v = CAR(args);
	LIST_EACH( x, CDR(args) ){
		if( !equal( v, x ) ) return VALUE_F;
	}
	return VALUE_T;
}

static Value _define_p( Value bundle, Value sym )
{
	ERROR_IF_NOT_SYMBOL( sym );
	return bundle_find( bundle, sym, true, false )?VALUE_T:VALUE_F;
}

static Value _add( Value bundle, Value args )
{
	int sum = 0;
	LIST_EACH( n, args ){
		ERROR_IF_NOT_INT(n);
		sum += V2INT(n);
	}
	return INT2V(sum);
}

static Value _sub( Value bundle, Value args )
{
	ERROR_IF_NOT_INT(CAR(args));
	int64_t sum = V2INT(CAR(args));
	LIST_EACH( n, CDR(args) ){
		ERROR_IF_NOT_INT(n);
		sum -= V2INT(n);
	}
	return INT2V(sum);
}

static Value _mul( Value bundle, Value args )
{
	int sum = 1;
	LIST_EACH( n, args ){
		ERROR_IF_NOT_INT(n);
		sum *= V2INT(n);
	}
	return INT2V(sum);
}

static Value _div( Value bundle, Value args )
{
	ERROR_IF_NOT_INT(CAR(args));
	int64_t sum = V2INT(CAR(args));
	LIST_EACH( n, CDR(args) ){
		ERROR_IF_NOT_INT(n);
		sum /= V2INT(n);
	}
	return INT2V(sum);
}

static Value _modulo( Value bundle, Value args )
{
	ERROR_IF_NOT_INT(CAR(args));
	int64_t sum = V2INT(CAR(args));
	LIST_EACH( n, CDR(args) ){
		ERROR_IF_NOT_INT(n);
		sum %= V2INT(n);
	}
	return INT2V(sum);
}

static Value _eq( Value bundle, Value args )
{
	if( args == NIL ) return VALUE_T;
	ERROR_IF_NOT_INT(CAR(args));
	int64_t last = V2INT(CAR(args));
	LIST_EACH( n, CDR(args) ){
		ERROR_IF_NOT_INT(n);
		if( !(last == V2INT(n)) ){ return VALUE_F; }else{ last = V2INT(n); }
	}
	return VALUE_T;
}

#define _INT_COMPARE_FUNC(name,cmp,first)								\
	static Value name( Value bundle, Value ns ){						\
	int64_t last = first;													\
	LIST_EACH( n, ns ){													\
		if( !(last cmp V2INT(n)) ){ return VALUE_F; }else{ last = V2INT(n); } \
	}																	\
	return VALUE_T;														\
	}

_INT_COMPARE_FUNC( _less, <, INT64_MIN )
_INT_COMPARE_FUNC( _less_eq, <=, INT64_MIN )
_INT_COMPARE_FUNC( _greater, >, INT64_MAX )
_INT_COMPARE_FUNC( _greater_eq, >=, INT64_MAX )

static Value _symbol_to_string( Value bundle, Value v )
{
	ERROR_IF_NOT_SYMBOL(v);
	return (Value)V2SYMBOL(v)->str;
}



static Value _car( Value bundle, Value v )
{
	ERROR_IF_NOT_PAIR(v);
	return CAR(v);
}

static Value _cdr( Value bundle, Value v )
{
	ERROR_IF_NOT_PAIR(v);
	return CDR(v);
}

static Value _cons( Value bundle, Value v1, Value v2 )
{
	return cons( v1, v2 );
}

static Value _set_car_i( Value bundle, Value pair, Value v )
{
	ERROR_IF_NOT_PAIR(pair);
	CAR(pair) = v;
	return NIL;
}

static Value _set_cdr_i( Value bundle, Value pair, Value v )
{
	ERROR_IF_NOT_PAIR(pair);
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

static Value _char_p( Value bundle, Value v )
{
	return IS_CHAR(v)?VALUE_T:VALUE_F;
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

static Value _string_p( Value bundle, Value v )
{
	return (IS_STRING(v))?VALUE_T:VALUE_F;
}

static Value _procedure_p( Value bundle, Value v )
{
	return ((TYPE_OF(v)==TYPE_CFUNC)||(TYPE_OF(v)==TYPE_LAMBDA))?VALUE_T:VALUE_F;
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
			Stream *s = V2STREAM(CDR(code));
			printf( "  %s:%d: *read-eval*\n", STRING_BUF(s->u.file.filename), s->line );
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
	Value vfilename;
	bind1arg( args, vfilename);
    assert(vfilename);
	String *filename = V2STRING(vfilename);
	FILE *fd = fopen( STRING_BUF(filename), "r" );
	if( !fd ) assert(0);
	Stream *file = stream_new( fd, true, STRING_BUF(filename) );
	return continuation_new( cons( V_READ_EVAL, (Value)file ),
							 bundle_cur, CONTINUATION_NEXT(cont) );
}

static Value _exit( Value args, Value cont, Value *result )
{
	bind1arg(args,*result);
	if( !*result ) *result = NIL;
	return NIL;
}

static Value _eof_object_p( Value bundle, Value v )
{
	return (v==V_EOF)?VALUE_T:VALUE_F;
}

static Value _display( Value bundle, Value v, Value rest )
{
	char buf[10240];
	size_t len;
	Value vport;
	bind1arg( rest, vport );
	if( !vport ) vport = bundle_get( bundle, SYM_CURRENT_OUTPUT_PORT, NULL );
	ERROR_IF_NOT_STREAM(vport);
	Stream *port = V2STREAM(vport);
	
	switch( TYPE_OF(v) ){
	case TYPE_CHAR:
		{
			int c = V2CHAR(v);
			if( c >= 32 && c <= 126 ){
				buf[0] = c;
				stream_write( port, buf, 1 );
			}else{
				len = sprintf( buf, "#\\%02x", c );
				stream_write( port, buf, len );
			}
		}
		break;
	case TYPE_STRING:
		{
			String *s = V2STRING(v);
			stream_write( port, s->body->buf + s->start, s->len );
		}
		break;
	default:
		len = value_to_str(buf, sizeof(buf), v);
		stream_write( port, buf, len );
	}
	return NIL;
}

static Value _write( Value bundle, Value v, Value rest )
{
	Value port;
	bind1arg( rest, port );
	if( !port ) port = bundle_get( bundle, SYM_CURRENT_OUTPUT_PORT, NULL );
	ERROR_IF_NOT_STREAM( port );
	stream_write_value( V2STREAM(port), v );
	return NIL;
}

static Value _read( Value bundle, Value rest )
{
	Value port;
	bind1arg( rest, port );
	if( !port ) port = bundle_get( bundle, SYM_CURRENT_INPUT_PORT, NULL );
	return stream_read_value(V2STREAM(port));
}

static Value _write_char( Value bundle, Value v, Value rest )
{
	Value port;
	bind1arg( rest, port );
	if( !port ) port = bundle_get( bundle, SYM_CURRENT_OUTPUT_PORT, NULL );
	ERROR_IF_NOT_CHAR(v);
	char c = V2CHAR(v);
	stream_write( V2STREAM(port), &c, 1 );
	return NIL;
}

static Value _open_input_file( Value bundle, Value _filename )
{
	ERROR_IF_NOT_STRING(_filename);
	char *filename = STRING_BUF(V2STRING(_filename));
	FILE *fd = fopen( filename, "r" );
	assert( fd );
	return (Value)stream_new( fd, true, filename );
}

static Value _open_output_file( Value bundle, Value _filename )
{
	ERROR_IF_NOT_STRING(_filename);
	char *filename = STRING_BUF(V2STRING(_filename));
	FILE *fd = fopen( filename, "a" );
	assert( fd );
	return (Value)stream_new( fd, true, filename );
}

static Value _open_input_string( Value bundle, Value str )
{
	ERROR_IF_NOT_STRING(str);
	Stream *s = stream_new_str( V2STRING(str) );
	return (Value)s;
}

static Value _open_output_string( Value bundle )
{
	Stream *s = stream_new_str( string_new_len("",8192) );
	return (Value)s;
}

static Value _close_input_port( Value bundle, Value v )
{
	ERROR_IF_NOT_STREAM(v);
	Stream *s = V2STREAM(v);
	stream_close( s );
	return NIL;
}

static Value _get_output_string( Value bundle, Value v )
{
	ERROR_IF_NOT_STREAM(v);
	Stream *s = V2STREAM(v);
	if( s->stream_type != STREAM_TYPE_STRING ) return error_new("not string stream");
	return (Value)string_substr( s->u.str, 0, s->pos );
}

static Value _char_eq_p( Value bundle, Value first, Value rest )
{
	ERROR_IF_NOT_CHAR(first);
	LIST_EACH( c, rest ){
		ERROR_IF_NOT_CHAR(c);
		if( V2CHAR(first) != V2CHAR(c) ) return VALUE_F;
	}
	return VALUE_T;
}

#define _CHAR_COMPARE_FUNC(name,cmp,first)								\
	static Value name( Value bundle, Value cs ){						\
	int last = first;													\
	LIST_EACH( c, cs ){													\
		ERROR_IF_NOT_CHAR(c);											\
		if( !(last cmp V2CHAR(c)) ){ return VALUE_F; }else{ last = V2CHAR(c); } \
	}																	\
	return VALUE_T;														\
	}

_CHAR_COMPARE_FUNC( _char_lt_p, < , INT_MIN )
_CHAR_COMPARE_FUNC( _char_le_p, <=, INT_MIN )
_CHAR_COMPARE_FUNC( _char_gt_p, > , INT_MAX )
_CHAR_COMPARE_FUNC( _char_ge_p, >=, INT_MAX )

static Value _char_to_integer( Value bundle, Value v )
{
	ERROR_IF_NOT_CHAR(v);
	return INT2V(V2CHAR(v));
}

static Value _integer_to_char( Value bundle, Value v )
{
	ERROR_IF_NOT_INT(v);
	return CHAR2V(V2INT(v));
}

static Value _char_upcase( Value bundle, Value v )
{
	ERROR_IF_NOT_CHAR(v);
	int c = V2CHAR(v);
	return ( c >= 'a' && c <= 'z' )?CHAR2V(c-32):v;
}

static Value _char_downcase( Value bundle, Value v )
{
	ERROR_IF_NOT_CHAR(v);
	int c = V2CHAR(v);
	return ( c >= 'A' && c <= 'Z' )?CHAR2V(c+32):v;
}

static Value _number_to_string( Value bundle, Value v )
{
	ERROR_IF_NOT_INT(v);
	char buf[32];
	sprintf( buf, "%lld", V2INT(v) );
	return (Value)string_new(buf);
}

static Value _string_to_number( Value bundle, Value v )
{
	ERROR_IF_NOT_STRING(v);
	int num;
	sscanf( STRING_BUF(V2STRING(v)), "%d", &num );
	return INT2V(num);
}

static Value _string_append( Value bundle, Value args )
{
	char buf[10240];
	char *tail = buf;
	for( Value cur=args; cur != NIL; cur=CDR(cur) ){
		ERROR_IF_NOT_STRING(CAR(cur));
		tail += sprintf( tail, "%s", STRING_BUF(V2STRING(CAR(cur))) );
	}
	return (Value)string_new(buf);
}

static Value _string_to_list( Value bundle, Value v )
{
	ERROR_IF_NOT_STRING(v);
	char *str = STRING_BUF(V2STRING(v));
	if( str[0] == '\0' ) return NIL;

	Value r = cons(CHAR2V(str[0]),NIL);
	Value tail = r;
	for( int i=1; str[i]; i++ ){
		tail = CDR(tail) = cons( CHAR2V(str[i]), NIL );
	}
	return r;
}

static Value _list_to_string( Value bundle, Value v )
{
	if( v == NIL ) return (Value)string_new("");
	
	ERROR_IF_NOT_PAIR(v);
	char buf[1024];
	int i=0;
	LIST_EACH(cur,v){
		buf[i] = (char)V2CHAR(cur);
		i++;
	}
	buf[i] = '\0';
	return (Value)string_new(buf);
}

static Value _string( Value bundle, Value cs )
{
	if( cs == NIL ) return (Value)string_new("");
	char buf[1024];
	int i = 0;
	LIST_EACH(c,cs){
		ERROR_IF_NOT_CHAR(c);
		buf[i] = (char)V2CHAR(c);
		i++;
	}
	buf[i] = '\0';
	return (Value)string_new(buf);
}

static Value _make_string( Value bundle, Value _len, Value rest )
{
	ERROR_IF_NOT_INT(_len);
	char buf[1024];
	int len = (int)V2INT(_len);
	int c = '\0';
	Value _c;
	bind1arg( rest, _c );
	if( _c ){
		ERROR_IF_NOT_CHAR(_c);
		c = V2CHAR(_c);
	}
	memset( buf, c, len );
	buf[len] = '\0';
	return (Value)string_new_len(buf, len);
}

static Value _string_null_p( Value bundle, Value v )
{
	ERROR_IF_NOT_STRING(v);
	String *s = V2STRING(v);
	return (s->len == 0)?VALUE_T:VALUE_F;
}

static Value _string_length( Value bundle, Value v )
{
	ERROR_IF_NOT_STRING(v);
	return INT2V( V2STRING(v)->len );
}

static Value _string_ref( Value bundle, Value v, Value _idx )
{
	ERROR_IF_NOT_STRING(v);
	ERROR_IF_NOT_INT(_idx);
	char *str = STRING_BUF(V2STRING(v));
	int idx = (int)V2INT(_idx);
	return CHAR2V( str[idx] );
}

static Value _string_set_i( Value bundle, Value v, Value _idx, Value _c )
{
	char *str = STRING_BUF(V2STRING(v));
	int idx = (int)V2INT(_idx);
	if( idx >= V2STRING(v)->len ){ printf("%d %zd\n", (int)idx, V2STRING(v)->len ); assert(0); }
	str[idx] = V2CHAR(_c);
	return NIL;
}

static Value _substring( Value bundle, Value v, Value _start, Value rest )
{
	int start = (int)V2INT(_start);
	int end = V2STRING(v)->len;
	Value _end;
	bind1arg( rest, _end );
	if( _end ) end = (int)V2INT(_end);
	String *s = V2STRING(v);
	return (Value)string_substr( s, start, end-start );
}

static Value _sys_getenv( Value bundle, Value name )
{
	char *str = getenv( STRING_BUF(V2STRING(name)) );
	if( str ){
		return (Value)string_new( str );
	}else{
		return VALUE_F;
	}
}

static Value _file_exists_p( Value bundle, Value _path )
{
	char *path = STRING_BUF(V2STRING(_path));
	struct stat file_stat;
	int err = stat( path, &file_stat );
	if( err ) return VALUE_F;
	return VALUE_T;
}

static Value _runtime_value_set_i( Value bundle, Value _name, Value val )
{
	char *name;
	if( IS_SYMBOL(_name) ){
		name = STRING_BUF(V2SYMBOL(_name)->str);
	}else if( IS_STRING(_name) ){
		name = STRING_BUF(V2STRING(_name));
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
	defun( "identity", 1, _identity );
	defun( "eq?", -1, _eq_p );
	defun( "eqv?", -1, _eqv_p );
	defun( "equal?", -1, _equal_p );
	defun( "define?", 1, _define_p );

	// number
	defun( "+", -1, _add );
	defun( "-", -1, _sub );
	defun( "*", -1, _mul );
	defun( "/", -1, _div );
	defun( "quotient", -1, _div );
	defun( "modulo", -1, _modulo );
	defun( "=", -1, _eq );
	defun( "<", -1, _less );
	defun( "<=", -1, _less_eq );
	defun( ">", -1, _greater );
	defun( ">=", -1, _greater_eq );

	// symbol
	defun( "symbol->string", 1, _symbol_to_string );

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
	defun( "char?", 1, _char_p );
	defun( "symbol?", 1, _symbol_p );
	defun( "pair?", 1, _pair_p );
	defun( "null?", 1, _null_p );
	defun( "list?", 1, _list_p );
	defun( "string?", 1, _string_p );
	defun( "procedure?", 1, _procedure_p );
	defun( "macro?", 1, _macro_p );

	//
	defun( "apply", CFUNC_ARITY_RAW, _apply );
	defun( "syntax-expand1", 1, _syntax_expand1 );

	//
	defun( "eval", CFUNC_ARITY_RAW, _eval );
	defun( "current-environment", CFUNC_ARITY_RAW, _current_environment );
	defun( "backtrace", CFUNC_ARITY_RAW, _backtrace );
	defun( "call/cc", CFUNC_ARITY_RAW, _call_cc );
	defun( "call-with-current-continuation", CFUNC_ARITY_RAW, _call_cc );
	defun( "load", CFUNC_ARITY_RAW, _load );
	defun( "exit", CFUNC_ARITY_RAW, _exit );

	// port
	defun( "eof-object?", 1, _eof_object_p );
	defun( "display", -2, _display );
	defun( "write", -2, _write );
	defun( "read", -1, _read );
	defun( "write-char", -2, _write_char );
	defun( "open-input-file", 1, _open_input_file );
	defun( "open-output-file", 1, _open_output_file );
	defun( "open-input-string", 1, _open_input_string );
	defun( "open-output-string", 0, _open_output_string );
	defun( "close-input-port", 1, _close_input_port );
	defun( "close-output-port", 1, _close_input_port ); // TODO: inputと変える
	defun( "get-output-string", 1, _get_output_string );

	// char
	defun( "char=?", -2, _char_eq_p );
	defun( "char<?", -1, _char_lt_p );
	defun( "char<=?", -1, _char_le_p );
	defun( "char>?", -1, _char_gt_p );
	defun( "char>=?", -1, _char_ge_p );
	defun( "char->integer", 1, _char_to_integer );
	defun( "integer->char", 1, _integer_to_char );
	defun( "char-upcase", 1, _char_upcase );
	defun( "char-downcase", 1, _char_downcase );

	// string
	defun( "number->string", 1, _number_to_string );
	defun( "string->number", 1, _string_to_number );
	defun( "string-append", -1, _string_append );
	defun( "string->list", 1, _string_to_list );
	defun( "list->string", 1, _list_to_string );
	defun( "string", -1, _string );
	defun( "make-string", -2, _make_string );
	defun( "string-null?", 1, _string_null_p );
	defun( "string-length", 1, _string_length );
	defun( "string-ref", 2, _string_ref );
	defun( "string-set!", 3, _string_set_i );
	defun( "substring", -3, _substring );

	// os
	defun( "sys-getenv", 1, _sys_getenv );
	//defun( "sys-environ", 1, _sys_environ );

	// filesystem
	defun( "file-exists?", 1, _file_exists_p );
	
	// debug
	defun( "runtime-value-set!", 2, _runtime_value_set_i );
	

}
