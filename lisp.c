#include "lisp.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <limits.h>
#include <inttypes.h>
#include <unistd.h>
#include <stdarg.h>
#ifdef WIN32
#include <windows.h>
#ifndef PATH_MAX
#define PATH_MAX MAX_PATH
#endif
#else
#include <linux/limits.h>
#include <signal.h>
#endif

Profile prof;

const char *TYPE_NAMES[] = {
	"unused",
	"nil",
	"bool",
	"int",
	"char",
	"symbol",
	"string",
	"string-body",
	"pair",
	"lambda",
	"cfunc",
	"bundle",
	"continuation",
	"special",
	"port",
	"pointer",
	"error",
};

const int TYPE_SIZE[] = {
	sizeof(Unused),
	sizeof(CellHeader),
	sizeof(CellHeader),
	sizeof(Integer),
	sizeof(Integer),
	sizeof(Symbol),
	sizeof(String),
	sizeof(StringBody),
	sizeof(Pair),
	sizeof(Lambda),
	sizeof(CFunc),
	sizeof(Bundle),
	sizeof(Continuation),
	sizeof(Special),
	sizeof(Stream),
	sizeof(Pointer),
	sizeof(Error),
};

const char* LAMBDA_TYPE_NAME[] = {
	"lambda",
	"macro",
};

extern inline Unused* V2UNUSED(Value v);
extern inline Special* V2SPECIAL(Value v);
extern inline int64_t V2INT(Value v);
extern inline int V2CHAR(Value v);
extern inline Symbol* V2SYMBOL(Value v);
extern inline String* V2STRING(Value v);
extern inline StringBody* V2STRING_BODY(Value v);
extern inline Pair* V2PAIR(Value v);
extern inline Lambda* V2LAMBDA(Value v);
extern inline CFunc* V2CFUNC(Value v);
extern inline Bundle* V2BUNDLE(Value v);
extern inline Continuation* V2CONTINUATION(Value v);
extern inline Stream* V2STREAM(Value v);
extern inline Pointer* V2POINTER(Value v);
extern inline Error* V2ERROR(Value v);

//********************************************************
// Utility
//********************************************************

static size_t _value_to_str( char *buf, int len, Value v )
{
	int n = 0;
	if( n >= len ) return len;
	
	if( !v ){
		n += snprintf( buf, len, "#<null>" );
		if( n >= len ) return len;
		return n;
	}
	
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
		n += snprintf( buf, len, "%" PRId64, V2INT(v) );
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
		n += string_puts( V2SYMBOL(v)->str, buf, len );
		break;
	case TYPE_STRING:
		buf[n++] = '"';
		if( n >= len ) return len;
		n += string_puts_escape( V2STRING(v), buf+n, len-n );
		if( n >= len ) return len;
		buf[n++] = '"';
		break;
	case TYPE_LAMBDA:
		{
			Lambda *lmd = V2LAMBDA(v);
			if( lmd->name != NULL ){
				char str[128];
				string_puts_escape( lmd->name->str, str, sizeof(str)-1 );
				n += snprintf( buf, len, "#<%s:%s>", LAMBDA_TYPE_NAME[lmd->type], str );
			}else{
				n += snprintf( buf, len, "#<%s:%p>", LAMBDA_TYPE_NAME[lmd->type], v );
			}
		}
		break;
	case TYPE_CFUNC:
		{
			CFunc *func = V2CFUNC(v);
			if( func->name ){
				char str[128];
				string_puts_escape( func->name->str, str, sizeof(str)-1 );
				n += snprintf( buf, len, "#<cfunc:%s>", str );
			}else{
				n += snprintf( buf, len, "#<cfunc:%p>", func );
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
		n += snprintf( buf+n, len-n, "#<bundle:%p>", v );
		break;
	case TYPE_CONTINUATION:
		n += snprintf( buf+n, len-n, "#<continuation:%p>", v );
		break;
	case TYPE_SPECIAL:
		n += snprintf( buf+n, len-n, "%%%s", V2SPECIAL(v)->str );
		break;
	case TYPE_STREAM:
		{
			Stream *s = V2STREAM(v);
			if( s->stream_type == STREAM_TYPE_FILE ){
				char str[PATH_MAX];
				string_puts_escape( s->u.file.filename, str, sizeof(str)-1 );
				n += snprintf( buf+n, len-n, "#<port:\"%s\">", str );
			}else{
				n += snprintf( buf+n, len-n, "#<port:string>" );
			}
		}
		break;
	case TYPE_ERROR:
		{
			Error *err = V2ERROR(v);
			char str[128];
			string_puts_escape( err->str, str, sizeof(str)-1 );
			n += snprintf( buf+n, len-n, "#<error:%s>", str );
		}
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
	// snprintf()が長さ以上に書き込むと終端してくれないので、念のため、終端させておく
	if( n >= len ) buf[len-1] = '\0'; 
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
	String *str = string_new(buf);
	return str->body->buf;
}

void vdump( Value v )
{
	printf( "%s\n", v2s(v) );
}

bool eq( Value a, Value b )
{
	if( TYPE_OF(a) != TYPE_OF(b) ) return false;
	switch( TYPE_OF(a) ){
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
		String *sa = V2STRING(a);
		String *sb = V2STRING(b);
		return (sa->len==sb->len) &&
			( sa->body->buf == sb->body->buf ||
			  (strncmp(sa->body->buf+sa->start, sb->body->buf+sb->start, sa->len) == 0) );
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
			String *s = V2STRING(v);
			char *str = s->body->buf + s->start;
			int hash = 0;
			for( int i=0; i<s->len; i++ ){
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

Integer* int_new( int64_t i )
{
	Integer *v = (Integer*)gc_new(TYPE_INT);
	v->number = i;
	return v;
}

//********************************************************
// Char
//********************************************************

Integer* char_new( int i )
{
	Integer *v = (Integer*)gc_new(TYPE_CHAR);
	v->number = i;
	return v;
}

//********************************************************
// Symbol
//********************************************************

Dict *symbol_root = NULL;

Symbol* intern( char *sym )
{
	String* str = string_new(sym);
	DictEntry *entry = dict_find( symbol_root, V(str), true );
	if( entry->val == NIL ){
		Symbol* val = V2SYMBOL(gc_new(TYPE_SYMBOL));
		val->str = str;
		entry->val = V(val);
	}
	return V2SYMBOL(entry->val);
}

Value gensym()
{
	static int i = 0;
	char buf[32];
	sprintf( buf, "#<gensym:%d>", i );
	i++;
	
	Symbol* val = V2SYMBOL(gc_new(TYPE_SYMBOL));
	val->str = string_new(buf);
	return V(val);
}

//********************************************************
// String
//********************************************************

String* string_new( char *str )
{
	return string_new_len( str, (int)strlen(str) );
}

String* string_new_len( char *str, int len )
{
	StringBody *body = V2STRING_BODY(gc_new(TYPE_STRING_BODY));
	body->buf = malloc( len+1 );
	assert( body->buf);
	memcpy( body->buf, str, len );
	body->buf[len] = '\0';
	body->len = len;
		
	String *s = V2STRING(gc_new(TYPE_STRING));
	s->body = body;
	s->start = 0;
	s->len = len;
	
	return s;
}

size_t string_puts( String *s, char *buf, size_t len )
{
	if( len > s->len ) len = s->len;
	memcpy( buf, s->body->buf + s->start, len );
	buf[len] = '\0';
	return len;
}

size_t string_puts_escape( String *s, char *buf, size_t len )
{
	size_t n = 0;
	char *str = s->body->buf + s->start;
	size_t slen = s->len;
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
				memcpy( buf+n, escaped, 2 );
				n += 2;
			}else{
				n += snprintf( buf+n, len-n, "\\x%02x", c );
			}
		}else{
			buf[n++] = c;
		}
		if( n >= len ) break;
	}
	buf[n] = '\0';
	return n;
}

String* string_substr( String *s, int start, int len )
{
	String *new_str = V2STRING(gc_new(TYPE_STRING));
	new_str->body = s->body;
	new_str->start = s->start + start;
	new_str->len = len;
	return new_str;
}

//********************************************************
// Lambda
//********************************************************

Lambda* lambda_new()
{
	Lambda *lmd = V2LAMBDA(gc_new(TYPE_LAMBDA));
	lmd->type = 0;
	lmd->name = NULL;
	lmd->args = NULL;
	lmd->body = NULL;
	lmd->bundle = NULL;
	return lmd;
}

//********************************************************
// CFunc
//********************************************************

CFunc* cfunc_new(int arity, void *func )
{
	CFunc *f = V2CFUNC(gc_new(TYPE_CFUNC));
	f->arity = arity;
	f->name = NULL;
	f->func = func;
	return f;
}

void defun( char *sym, int arity, void *func )
{
	bundle_define( bundle_cur, intern(sym), V(cfunc_new(arity, func)) );
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

Bundle *bundle_cur = NULL;

Bundle *bundle_new( Bundle *upper )
{
	Bundle *b = V2BUNDLE(gc_new(TYPE_BUNDLE));
	b->dict = dict_new( hash_eqv, eqv );
	b->upper = upper;
	b->lambda = NULL;
	return b;
}

DictEntry* bundle_find( Bundle *b, Symbol *sym, bool find_upper, bool create )
{
	if( find_upper && b->upper != NULL ){
		// 自分のを探す
		DictEntry *entry = dict_find( b->dict, V(sym), false );
		if( entry ) return entry;
		
		// 親のを探す
		entry = bundle_find( b->upper, sym, find_upper, false );
		if( entry ) return entry;

		// 新しく作る
		if( create ){
			return bundle_find( b, sym, find_upper, true );
		}else{
			return NULL;
		}
	}else{
		return dict_find( b->dict, V(sym), create );
	}
}

void bundle_set( Bundle *b, Symbol *sym, Value v )
{
	DictEntry *entry = bundle_find( b, sym, true, false );
	if( !entry ){
		printf( "bundle_set: %s\n", v2s(V(sym)) );
		assert( !"cannot set" );
	}
	entry->val = v;
}

void bundle_define( Bundle *b, Symbol *sym, Value v )
{
	// サイズが大きいならリサイズ
	Dict *d = b->dict;
	if( d->use >= d->size ) b->dict = dict_rehash(d);
	
	DictEntry *entry = bundle_find( b, sym, false, true );
	entry->val = v;

	if( IS_LAMBDA(v) && !V2LAMBDA(v)->name ) V2LAMBDA(v)->name = sym;
	if( IS_CFUNC(v) && !V2CFUNC(v)->name ) V2CFUNC(v)->name = sym;
}

Value bundle_get( Bundle *b, Symbol *sym, Value def )
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

Value continuation_new( Value code, Bundle *bundle, Value next )
{
	Continuation *v = (Continuation*)gc_new( TYPE_CONTINUATION );
	v->bundle = bundle;
	v->data = NIL;
	v->code = code;
	v->next = next;
	return V(v);
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
	case ']':
	case '\0':
		stream_ungetc(c,s);
		*result = NIL;
		return 0;
	default:
		stream_ungetc(c,s);
		err = _parse( s, &val );
		if( err ) return err;

		if( val == V(SYM_DOT) ){
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
	case ' ': case '\t': case '\n': case '\r': case '\x0c': case '(': case ')': case '[': case ']': case '\0': case -1:
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

#define isnumber isdigit

static Value _parse_token( char *str )
{
	if( isnumber(str[0]) || (str[0] == '-' && isnumber(str[1])) ){
		for( char *s = str+1; *s != '\0'; s++ ){
			if( !isnumber(*s) ){
				return V(intern(str));
			}
		}
		return INT2V(atoi(str));
	}else{
		return V(intern( str ));
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
		stream_read(s,buf,2);
		buf[2] = '\0';
		sscanf( buf, "%x", &c );
		return c;
	case 'u':
		stream_read(s,buf,4);
		buf[4] = '\0';
		sscanf( buf, "%x", &c );
		return c;
	case 'U':
		stream_read(s,buf,8);
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
			return (Value)string_new_len(buf,i);
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
	int c, c2;
	int level;
	switch( c = stream_getc(s) ){
	case -1:
	case '\0':
		return 0;
	case '(':
	case '[':
		err = _parse_list( s, result );
		if( err ) return err;
		c2 = stream_getc(s);
		if( c == '(' && c2 != ')' ) return -2;
		if( c == '[' && c2 != ']' ) return -2;
		return 0;
	case ')':
	case ']':
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
			*result = cons3( V(intern("*tee*")), *result, NIL );
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
		case '[':
			// vector literal
			// とりあえず、リストにする
			err = _parse_list( s, result );
			if( err ) return err;
			c2 = stream_getc(s);
			if( c == '(' && c2 != ')' ) return -2;
			if( c == '[' && c2 != ']' ) return -2;
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
		*result = cons( V(SYM_QUASIQUOTE), cons(*result,NIL) );
		return err;
	case ',':
		{
			Value sym = V(SYM_UNQUOTE);
			if( (c = stream_getc(s)) == '@' ){
				sym = V(SYM_UNQUOTE_SPLICING);
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
	Stream *s = V2STREAM(gc_new( TYPE_STREAM ));
	s->line = 1;
	s->pos = 0;
	s->stream_type = STREAM_TYPE_FILE;
	s->u.file.close = close;
	s->u.file.fd = fd;
	s->u.file.filename = string_new(filename);
	return s;
}

Stream* stream_new_str( String *str )
{
	Stream *s = V2STREAM(gc_new( TYPE_STREAM ));
	s->line = 1;
	s->pos = 0;
	s->stream_type = STREAM_TYPE_STRING;
	s->u.str = str;
	return s;
}

int stream_getc( Stream *s )
{
	int c;
	if( s->stream_type == STREAM_TYPE_FILE ){
		c = fgetc( s->u.file.fd );
	}else{
		char *str = STRING_BUF(s->u.str);
		c = str[s->pos];
	}
	s->pos++;
	if( c == '\n' ) s->line += 1;
	return c;
}

void stream_ungetc( int c, Stream *s )
{
	if( c == '\n' ) s->line -= 1;
	s->pos--;
	if( s->stream_type == STREAM_TYPE_FILE ){
		ungetc( c, s->u.file.fd );
	}
}

Value stream_read_value( Stream *s )
{
	Value val = V_EOF;
	int err = _parse( s, &val );
	if( err ){
		printf( "parse error: err=%d\n", err );
		assert(0);
	}
	return val;
}

void stream_write_value( Stream *s, Value v )
{
	char buf[10240];
	size_t len = value_to_str(buf, sizeof(buf), v);
	stream_write( s, buf, len );
}

size_t stream_read( Stream *s, char *buf, size_t len )
{
	size_t read_len;
	if( s->stream_type == STREAM_TYPE_FILE ){
		read_len = fread( buf, len, 1, s->u.file.fd );
	}else{
		read_len = len;
		strncpy( buf, STRING_BUF(s->u.str), len );
	}
	assert( read_len >= 0 );
	return read_len;
}

size_t stream_write( Stream *s, char *buf, size_t len )
{
	size_t write_len;
	if( s->stream_type == STREAM_TYPE_FILE ){
		write_len = fwrite( buf, len, 1, s->u.file.fd );
	}else{
		write_len = len;
		char *str = STRING_BUF(s->u.str);
		memcpy( str+s->pos, buf, len );
		str[s->pos+len] = '\0';
	}
	s->pos += write_len;
	assert( write_len >= 0 );
	return write_len;
}

void stream_close( Stream *s )
{
	if( s->stream_type == STREAM_TYPE_FILE ){
		fclose( s->u.file.fd );
		s->u.file.fd = 0;
	}
}

//********************************************************
// Error
//********************************************************

Value error_new( char *str )
{
	Error *e = V2ERROR(gc_new( TYPE_ERROR ));
	e->str = string_new( str );
	return (Value)e;
}

Value error_newf( char *str, ... )
{
	char buf[1024];
	va_list list;
	va_start( list, str );
	vsnprintf( buf, sizeof(buf)-1, str, list );
	va_end( list );

	return error_new( buf );
}

//********************************************************
// Syntax
//********************************************************

static Value _syntax_expand_body( Value body, Dict *bundle )
{
	body = list_copy(body);
	for( Value cur=body; cur != NIL; cur=CDR(cur) ){
		Value found = dict_get( bundle, CAR(cur) );
		if( found ){
			if( IS_PAIR(CDR(cur)) && CADR(cur) == V(SYM_DOT3) ){
				Value next = CDDR(cur);
				if( CAR(found) != V(SYM_SYNTAX_REST) ) assert(0);
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

static bool _syntax_match( Value keywords, Value rule, Value code, Dict *bundle )
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
		if( IS_PAIR(CDR(c)) && CADR(c) == V(SYM_DOT3) ){
			dict_set( bundle, CAR(c), cons( V(SYM_SYNTAX_REST), code ));
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
	Value syntax = bundle_get( bundle_cur, (Symbol*)CAR(code), NIL );
	if( !IS_PAIR(syntax) ) return code;
	Value sym, keywords, rules;
	bind3cdr( syntax, sym, keywords, rules );
	if( sym != V(SYM_SYNTAX_RULES) ) return code;
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
Symbol *SYM_LET;
Symbol *SYM_LETREC;
Symbol *SYM_LET_A;
Symbol *SYM_DEFINE;
Symbol *SYM_IF;
Symbol *SYM_COND;
Symbol *SYM_QUOTE;
Symbol *SYM_ELSE;
Symbol *SYM_BEGIN;
Symbol *SYM_AND;
Symbol *SYM_OR;
Symbol *SYM_MACRO;
Symbol *SYM_DEFINE_SYNTAX;
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
	realpath( path, out_path );
#endif
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
	_INIT_OPERATOR(V_READ_EVAL, "#<read-eval>", OP_READ_EVAL);
	_INIT_OPERATOR(V_READ_EVAL2, "#<read-eval2>", OP_READ_EVAL2);

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
	SYM_LET = intern("let");
	SYM_LETREC = intern("letrec");
	SYM_LET_A = intern("let*");
	SYM_DEFINE = intern("define");
	SYM_IF = intern("if");
	SYM_COND = intern("cond");
	SYM_QUOTE = intern("quote");
	SYM_ELSE = intern("else");
	SYM_BEGIN = intern("begin");
	SYM_AND = intern("and");
	SYM_OR = intern("or");
	SYM_MACRO = intern("macro");
	SYM_DEFINE_SYNTAX = intern("define-syntax");
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
		eval_loop( stream_new(fd,true,"prelude.scm") );
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

