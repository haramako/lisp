#include "lisp.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <limits.h>
#include <unistd.h>

Profile prof;

const char *TYPE_NAMES[] = {
	"UNUSED",
	"NIL",
	"BOOL",
	"INT",
	"CHAR",
	"SYMBOL",
	"STRING",
	"PAIR",
	"LAMBDA",
	"CFUNC",
	"BUNDLE",
	"CONTINUATION",
	"SPECIAL",
	"STREAM",
};

const char* LAMBDA_TYPE_NAME[] = {
	"LAMBDA",
	"MACRO",
};

//********************************************************
// Utility
//********************************************************

static size_t _escape_str( char *buf, size_t len, char *str )
{
	int n = 0;
	int slen = strlen(str);
	for( int i=0; i<slen; i++ ){
		char c = str[i];
		if( c < 32 || c >= 127 ){
			char *escaped = NULL;
			switch( c ){
			case '"': escaped = "\\\""; break;
			case '\\': escaped = "\\\\"; break;
			case '\n': escaped = "\\n"; break;
			case '\r': escaped = "\\r"; break;
			case '\f': escaped = "\\f"; break;
			case '\t': escaped = "\\t"; break;
			case '\0': escaped = "\\0"; break;
			}
			if( escaped ){
				n += snprintf( buf+n, len-n, "%s", escaped );
			}else{
				n += snprintf( buf+n, len-n, "\\x%02x", c );
			}
		}else{
			buf[n++] = c;
		}
		if( n >= len ) break;
	}
	return n;
}

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
		n += snprintf( buf, len, "()" );
		break;
	case TYPE_BOOL:
		n += snprintf( buf, len, (v==VALUE_T)?"#t":"#f" );
		break;
	case TYPE_INT:
		n += snprintf( buf, len, "%lld", V2INT(v) );
		break;
	case TYPE_CHAR:
		{
			int c = V2CHAR(v);
			if( c >= 32 && c <= 126 ){
				n += snprintf( buf, len, "#\\%c", (char)c );
			}else{
				n += snprintf( buf, len, "#\\x%02x", c );
			}
		}
		break;
	case TYPE_SYMBOL:
		n += snprintf( buf, len, "%s", STRING_STR(SYMBOL_STR(v)) );
		break;
	case TYPE_STRING:
		n += snprintf( buf, len, "\"" );
		if( n >= len ) return len;
		n += _escape_str( buf+n, len-n, STRING_STR(v) );
		if( n >= len ) return len;
		n += snprintf( buf+n, len-n, "\"" );
		break;
	case TYPE_LAMBDA:
		if( LAMBDA_NAME(v) != NIL ){
			n += snprintf( buf, len, "(%s:%s:%p)",
						   LAMBDA_TYPE_NAME[LAMBDA_TYPE(v)], STRING_STR(SYMBOL_STR(LAMBDA_NAME(v))), v );
		}else{
			n += snprintf( buf, len, "(%s:%p)", LAMBDA_TYPE_NAME[LAMBDA_TYPE(v)], v );
		}
		break;
	case TYPE_CFUNC:
		if( CFUNC_NAME(v) != NIL ){
			n += snprintf( buf, len, "(CFUNC:%s)", STRING_STR(SYMBOL_STR(CFUNC_NAME(v))) );
		}else{
			n += snprintf( buf, len, "(CFUNC:%p)", CFUNC_FUNC(v) );
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
	size_t len = value_to_str(buf, limit, v);
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
	case TYPE_CHAR:
		return ( V2CHAR(a) == V2CHAR(b) );
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
// Char
//********************************************************

Value char_new( int i )
{
	Value v = gc_new(TYPE_CHAR);
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
	return string_new_len( str, strlen(str) );
}

Value string_new_len( char *str, int len )
{
	Value v = gc_new(TYPE_STRING);
	char *s = malloc(len+1);
	assert( s );
	memcpy( s, str, len );
	s[len] = '\0';
	STRING_STR(v) = s;
	STRING_LEN(v) = len;
	return v;
}

//********************************************************
// Lambda
//********************************************************

Value lambda_new()
{
	Value v = gc_new(TYPE_LAMBDA);
	LAMBDA_DATA(v) = NIL;
	LAMBDA_BUNDLE(v) = NIL;
	return v;
}

//********************************************************
// CFunc
//********************************************************

Value cfunc_new(int arity, void *func )
{
	Value v = gc_new(TYPE_CFUNC);
	CFUNC_ARITY(v) = arity;
	CFUNC_NAME(v) = NIL;
	CFUNC_FUNC(v) = func;
	return v;
}

void defun( char *sym, int arity, void *func )
{
	Value v = cfunc_new(arity, func);
	bundle_define( bundle_cur, intern(sym), v );
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

Value list_tail( Value list )
{
	if( list == NIL ) return NIL;
	for(;CDR(list) != NIL; list=CDR(list));
	return list;
}

//********************************************************
// Bundle
//********************************************************

Value bundle_cur = NULL;

Value bundle_new( Value upper )
{
	Value v = gc_new(TYPE_BUNDLE);
	BUNDLE_DICT(v) = dict_new( hash_eqv, eqv );
	BUNDLE_DATA(v) = cons( upper, NIL );
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
	entry->val = v;

	if( IS_LAMBDA(v) && LAMBDA_NAME(v) == NIL ) LAMBDA_NAME(v) = sym;
	if( IS_CFUNC(v) && CFUNC_NAME(v) == NIL ) CFUNC_NAME(v) = sym;
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
	CONTINUATION_BUNDLE(v) = bundle;
	CONTINUATION_DATA(v) = cons( code, next );
	return v;
}

//********************************************************
// Parsing
//********************************************************

int _parse( Stream *s, Value *result );
	
void _skip_space( Stream *s )
{
	for(;;){
		int c = stream_getc(s);
		switch( c ){
		case '\n': case '\r': case '\x0c': case ' ': case '\t': 
			break;
		case ';':
			for(;;){
				c = stream_getc(s);
				if( c == '\n' || c == '\r' || c == '\x0c' || c == '\0' || c == -1 ) break;
			}
			break;
		default:
			stream_ungetc(c, s);
			return;
		}
	}
}

int _parse_list( Stream *s, Value *result )
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
	case ' ': case '\t': case '\n': case '\r': case '\x0c': case '(': case ')': case '\0': case -1:
		return false;
	default:
		return true;
	}
}

static int _read_token( Stream *s, char *buf )
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

static int _unescape_char( Stream *s )
{
	int c = stream_getc(s);
	char buf[9];
	switch( c ){
	case '"': return '"';
	case '\\': return '\\';
	case 'n': return '\n';
	case 'r': return '\r';
	case 'f': return '\f';
	case 't': return '\t';
	case '0': return '\0';
	case 'x':
		stream_read_chars(s,buf,2);
		buf[2] = '\0';
		sscanf( buf, "%x", &c );
		return c;
	case 'u':
		stream_read_chars(s,buf,4);
		buf[4] = '\0';
		sscanf( buf, "%x", &c );
		return c;
	case 'U':
		stream_read_chars(s,buf,8);
		buf[8] = '\0';
		sscanf( buf, "%x", &c );
		return c;
	case ' ': case '\n': case '\r':
		{
			bool feeded = false;
			for(;;){
				if( c == '\n' ){
					if( feeded ) assert(0);
					feeded = true;
				}else if( c == ' ' || c == '\f' || c == '\r' || c == '\t' ){
					// skip
				}else{
					if( !feeded ) assert(0);
					stream_ungetc(c,s);
					break;
				}
				c = stream_getc(s);
			}
			return '\n';
		}
	default:
		printf( "unkonwn escaped string \\%c\n", c );
		assert(0);
	}
}

static Value _read_string( Stream *s )
{
	char buf[1024];
	int i = 0;
	for(;;){
		int c = stream_getc(s);
		switch( c ){
		case '"':
			return string_new_len(buf,i);
		case '\\':
			c = _unescape_char(s);
			break;
		case '\n': case '\r':
			assert(0);
		case '\0':
			assert(0);
		}
		buf[i++] = c;
	}
}

int _parse( Stream *s, Value *result )
{
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
		case '!':
			// shebang
			while( (c = stream_getc(s)) ){
				if( c == '\n' ) break;
			}
			return _parse(s, result);
		case ';':
			// s-exp comment
			err = _parse(s, result);
			if( err ) return err;
			return _parse(s, result);
		case 'x':
			{
				char buf[32];
				int num;
				_read_token(s,buf);
				sscanf( buf, "%x", &num);
				*result = INT2V(num);
				return 0;
			}
			
		case '\\':
			{
				char buf[32];
				_read_token(s,buf);
				if( strlen(buf) == 1 ){
					*result = CHAR2V(buf[0]);
				}else if( buf[0] == 'x' || buf[0] == 'u' ){
					int num;
					sscanf( buf+1, "%x", &num);
					*result = CHAR2V(num);
				}else if( strcmp(buf,"space") == 0 ){
					*result = CHAR2V(' ');
				}else if( strcmp(buf,"newline") == 0 || strcmp(buf,"nl") == 0 || strcmp(buf,"lf") == 0 ){
					*result = CHAR2V('\n');
				}else if( strcmp(buf,"return") == 0 || strcmp(buf,"cr") == 0 ){
					*result = CHAR2V('\r');
				}else if( strcmp(buf,"tab") == 0 || strcmp(buf,"ht") == 0 ){
					*result = CHAR2V('\t');
				}else if( strcmp(buf,"page") == 0 ){
					*result = CHAR2V(0x0c);
				}else if( strcmp(buf,"escape") == 0 || strcmp(buf,"esc") == 0 ){
					*result = CHAR2V(0x1b);
				}else if( strcmp(buf,"delete") == 0 || strcmp(buf,"del") == 0 ){
					*result = CHAR2V(0x7f);
				}else if( strcmp(buf,"null") == 0 ){
					*result = CHAR2V('\0');
				}else{
					printf( "invalid #\\%s\n", buf );
					assert(0);
				}
				return 0;
			}
		default:
			printf( "invalid #char %c\n", c );
			assert(0);
		}
	case '"':
		*result = _read_string(s);
		if( !*result ) return -4;
		return 0;
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

Stream* stream_new( FILE *fd, bool close, char *filename )
{
	Stream *v = V2STREAM(gc_new( TYPE_STREAM ));
	STREAM_FD(v) = fd;
	v->flag = ( v->flag & ~STREAM_MASK_CLOSE ) | (close?1:0)<<15; // STREAM_CLOSE(v) = close;
	STREAM_FILENAME(v) = string_new(filename);
	STREAM_LINE(v) = 1;
	return v;
}

int stream_getc( Stream *s )
{
	int c = fgetc( STREAM_FD(s) );
	if( c == '\n' ) STREAM_LINE(s) += 1;
	return c;
}

void stream_ungetc( int c, Stream *s )
{
	if( c == '\n' ) STREAM_LINE(s) -= 1;
	ungetc( c, STREAM_FD(s) );
}

Value stream_read( Stream *s )
{
	Value val = V_EOF;
	int err = _parse( s, &val );
	if( err ){
		printf( "parse error: err=%d\n", err );
		assert(0);
	}
	return val;
}

Value stream_write( Stream *s, Value v )
{
	char buf[10240];
	value_to_str(buf, sizeof(buf), v);
	fputs( buf, STREAM_FD(s) );
	return NIL;
}

size_t stream_read_chars( Stream *s, char *buf, size_t len )
{
	size_t read_len = fread( buf, len, 1, STREAM_FD(s) );
	assert( read_len >= 0 );
	return read_len;
}

//********************************************************
// Syntax
//********************************************************

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

bool _syntax_match( Value keywords, Value rule, Value code, Dict *bundle )
{
	// printf( "syntax_match: %s %s\n", v2s(rule), v2s(code) );
	for( Value c = rule; c != NIL; c=CDR(c), code=CDR(code) ){
		
		// match literal
		if( IS_STRING(CAR(c)) ){
			if( eqv( CAR(c), CAR(code) ) ){
				continue;
			}else{
				return false;
			}
		}
		
		// match keywords
		// printf( "%s %s\n", v2s(keywords), v2s(CAR(code)) );
		if( IS_PAIR(code) ){
			LIST_EACH( kw, keywords ){
				if( kw == CAR(c) ){
					if( kw == CAR(code) ){
						continue;
					}else{
						return false;
					}
				}
			}
		}

		// match pattern
		if( IS_PAIR(CDR(c)) && CADR(c) == SYM_DOT3 ){
			dict_set( bundle, CAR(c), cons( SYM_SYNTAX_REST, code ));
			// printf( "bind %s => %s\n", v2s(CAR(c)), v2s(code) );
			return true;
		}else{
			if( !IS_PAIR(code) ) return false;
			if( IS_PAIR(CAR(c)) ){
				if( !_syntax_match( keywords, CAR(c), CAR(code), bundle ) ) return false;
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
	code = normalize_let( code );
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
		bool matched = _syntax_match( keywords, CAAR(cur), code, bundle );
		if( matched ){
			Value new_code = syntax_expand1( _syntax_expand_body( CADR(CAR(cur)), bundle ) );
			// if( opt_trace ) printf( "expand-syntax: %s => %s\n", v2s(code), v2s(new_code) );
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
Value V_END_OF_LINE = NULL;
Stream *V_STDOUT, *V_STDIN, *V_SRC_FILE;
Value V_BEGIN;
Value V_CALL0;
Value V_CALL1;
Value V_QUOTE;
Value V_DEFINE, V_DEFINE2;
Value V_SET_I, V_SET_I2;
Value V_LET, V_LET_A, V_LETREC, V_LET2, V_LET3;
Value V_LAMBDA, V_MACRO, V_DEFINE_SYNTAX;
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
Value SYM_DOT, SYM_DOT3, SYM_ERROR, SYM_SYNTAX_RULES, SYM_SYNTAX_REST,
	SYM_RUNTIME_LOAD_PATH, SYM_RUNTIME_HOME_PATH, SYM_LAMBDA, SYM_LET, SYM_LETREC;

Value _operator( char *sym, Operator op ){
	Value v = gc_new(TYPE_SPECIAL);
	SPECIAL_OP(v) = op;
	SPECIAL_STR(v) = sym;
	bundle_define( bundle_cur, intern(sym), v );
	return retain(v);
}

bool opt_trace = false;
bool opt_debug = false;

static void _get_home_path( const char *argv0, char *out_path )
{
	char cwd[PATH_MAX], path[PATH_MAX];
	getcwd( cwd, sizeof(cwd) );
	sprintf( path, "%s/%s/..", cwd, argv0 );
	realpath( path, out_path );
}

void init( const char *argv0 )
{
	init_prelude( argv0, true);
}

void init_prelude( const char *argv0, bool with_prelude )
{
	char home_path[PATH_MAX];
	_get_home_path( argv0, home_path );
	
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
	SYM_RUNTIME_LOAD_PATH = intern("runtime-load-path");
	SYM_RUNTIME_HOME_PATH = intern("runtime-home-path");
	SYM_LAMBDA = intern("lambda");
	SYM_LET = intern("let");
	SYM_LETREC = intern("letrec");

	bundle_define( bundle_cur, SYM_CURRENT_INPUT_PORT, (Value)V_STDIN );
	bundle_define( bundle_cur, SYM_CURRENT_OUTPUT_PORT, (Value)V_STDOUT );
	bundle_define( bundle_cur, SYM_END_OF_LINE, V_END_OF_LINE );

	cfunc_init();

	// define runtime-home-path, runtime-lib-path
	char lib_path[PATH_MAX];
	bundle_define( bundle_cur, SYM_RUNTIME_HOME_PATH, string_new(home_path) );
	sprintf( lib_path, "%s/lib", home_path );
	bundle_define( bundle_cur, SYM_RUNTIME_LOAD_PATH, cons3( string_new("."), string_new(lib_path), NIL ) );
	sprintf( lib_path, "lib" );
   	bundle_define( bundle_cur, SYM_RUNTIME_LOAD_PATH, cons3( string_new("."), string_new(lib_path), NIL ) );
    
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

