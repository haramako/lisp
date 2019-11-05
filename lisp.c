#include "lisp.h"
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#ifdef WIN32
#include <windows.h>
#ifndef PATH_MAX
#define PATH_MAX MAX_PATH
#endif
#else
#include <linux/limits.h>
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

bool eq( Value a, Value b )
{
	if( TYPE_OF(a) != TYPE_OF(b) ) return false;
	switch( TYPE_OF(a) ) {
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
	if( eq(a, b) ) return true;
	switch( TYPE_OF(a) ) {
	case TYPE_STRING:
		if( !IS_STRING(b) ) return false;
		String *sa = V2STRING(a);
		String *sb = V2STRING(b);
		return (sa->len == sb->len) &&
			   ( sa->body->buf == sb->body->buf ||
				 (strncmp(sa->body->buf + sa->start, sb->body->buf + sb->start, sa->len) == 0) );
	default:
		return false;
	}
}

bool equal( Value a, Value b )
{
	if( eqv(a, b) ) return true;
	switch( TYPE_OF(a) ) {
	case TYPE_PAIR:
		if( !IS_PAIR(b) ) return false;
		if( equal( CAR(a), CAR(b) ) ) {
			return equal(CDR(a), CDR(b));
		} else {
			return false;
		}
	default:
		return false;
	}
}

unsigned int hash_eq( Value v )
{
	switch( TYPE_OF(v) ) {
	case TYPE_INT:
		return (unsigned int)V2INT(v);
	default:
		return (unsigned int)(((uintptr_t)v) >> 4) * 31;
	}
}

unsigned int hash_eqv( Value v )
{
	switch( TYPE_OF(v) ) {
	case TYPE_STRING:
		{
			String *s = V2STRING(v);
			char *str = s->body->buf + s->start;
			int hash = 0;
			for( int i = 0; i < s->len; i++ ) {
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
	switch( TYPE_OF(v) ) {
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
	if( entry->val == NIL ) {
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
	for( Value cur = v; cur != NIL; cur = CDR(cur) ) len++;
	return len;
}

Value list_copy( Value list )
{
	if( !IS_PAIR(list) ) return list;
	Value r = cons( CAR(list), NIL );
	Value tail = r;
	for( Value cur = CDR(list); cur != NIL; cur = CDR(cur) ) {
		tail = CDR(tail) = cons( CAR(cur), NIL );
	}
	return r;
}

Value list_tail( Value list )
{
	if( list == NIL ) return NIL;
	for(; CDR(list) != NIL; list = CDR(list));
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
	if( find_upper && b->upper != NULL ) {
		// 自分のを探す
		DictEntry *entry = dict_find( b->dict, V(sym), false );
		if( entry ) return entry;

		// 親のを探す
		entry = bundle_find( b->upper, sym, find_upper, false );
		if( entry ) return entry;

		// 新しく作る
		if( create ) {
			return bundle_find( b, sym, find_upper, true );
		} else {
			return NULL;
		}
	} else {
		return dict_find( b->dict, V(sym), create );
	}
}

void bundle_set( Bundle *b, Symbol *sym, Value v )
{
	DictEntry *entry = bundle_find( b, sym, true, false );
	if( !entry ) {
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
	if( entry ) {
		return entry->val;
	} else {
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
	vsnprintf( buf, sizeof(buf) - 1, str, list );
	va_end( list );

	return error_new( buf );
}

