#pragma once

#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

typedef enum {
	TYPE_UNUSED = 0,
	TYPE_NIL,
	TYPE_BOOL,
	TYPE_INT,
	TYPE_CHAR,
	TYPE_SYMBOL,
	TYPE_STRING,
	TYPE_STRING_BODY,
	TYPE_PAIR,
	TYPE_LAMBDA,
	TYPE_CFUNC,
	TYPE_BUNDLE,
	TYPE_CONTINUATION,
	TYPE_SPECIAL,
	TYPE_STREAM,
	TYPE_POINTER,
	TYPE_ERROR,
	TYPE_MAX,
} Type;

extern const char *TYPE_NAMES[];
extern const int TYPE_SIZE[];

typedef enum {
	OP_BEGIN = 0,
	OP_CALL0, 
	OP_CALL1,
	OP_QUOTE,
	OP_DEFINE,
	OP_DEFINE2,
	OP_SET_I,
	OP_SET_I2,
	OP_LET,
	OP_LET_A,
	OP_LETREC,
	OP_LET2,
	OP_LET3,
	OP_LAMBDA,
	OP_MACRO,
	OP_DEFINE_SYNTAX,
	OP_IF,
	OP_IF2,
	OP_AND,
	OP_AND2,
	OP_OR,
	OP_OR2,
	OP_READ_EVAL,
	OP_READ_EVAL2,
} Operator;

typedef struct Cell* Value;

typedef struct {
	char type;
	char marked;
} CellHeader;

typedef struct Cell {
	CellHeader h;
	union {
		struct {
			struct Bundle *bundle;
			Value data; // (code . next )
		} continuation;
	} d;
} Cell;

typedef struct Unused {
	CellHeader h;
	Value next;
} Unused;

typedef struct Special {
	CellHeader h;
	Operator op;
	char *str;
} Special;

typedef struct Integer {
	CellHeader h;
	int64_t number;
} Integer;

typedef struct Symbol {
	CellHeader h;
	struct String *str;
} Symbol;

typedef struct {
	CellHeader h;
	size_t len;
	char *buf;
} StringBody;

typedef struct String {
	CellHeader h;
	StringBody *body;
	size_t start;
	size_t len;
} String;

typedef struct Pair {
	CellHeader h;
	Value car;
	Value cdr;
} Pair;

typedef enum {
	LAMBDA_TYPE_LAMBDA = 0,
	LAMBDA_TYPE_MACRO,
} LambdaType;

extern const char* LAMBDA_TYPE_NAME[];

typedef struct Lambda {
	CellHeader h;
	struct Bundle *bundle;
	LambdaType type;
	Symbol *name;
	Value args;
	Value body;
} Lambda;

typedef struct Bundle {
	CellHeader h;
	struct Dict *dict;
	struct Bundle *upper;
	Lambda *lambda;
} Bundle;

typedef struct CFunc {
	CellHeader h;
	int arity;
	Symbol *name;
	void *func;
} CFunc;

typedef enum {
	STREAM_TYPE_FILE,
	STREAM_TYPE_STRING,
} StreamType;

typedef struct Stream {
	CellHeader h;
	StreamType stream_type;
	int line;
	int pos;
	union {
		struct {
			FILE *fd;
			String *filename;
			char close;
		} file;
		String *str;
	} u;
} Stream;

typedef struct Pointer {
	CellHeader h;
	Value *ptr;
	struct Pointer *next;
} Pointer;

typedef struct {
	CellHeader h;
	String *str;
} Error;

#define CFUNC_ARITY_RAW 127

typedef Value (*CFunction)( Value args, Value cont, Value *result );
typedef Value (*CFunction0)( Bundle *bundle );
typedef Value (*CFunction1)( Bundle *bundle, Value v1 );
typedef Value (*CFunction2)( Bundle *bundle, Value v1, Value v2 );
typedef Value (*CFunction3)( Bundle *bundle, Value v1, Value v2, Value v3 );
typedef Value (*CFunction4)( Bundle *bundle, Value v1, Value v2, Value v3, Value v4 );
typedef Value (*CFunction5)( Bundle *bundle, Value v1, Value v2, Value v3, Value v4, Value v5 );
typedef Value (*CFunction6)( Bundle *bundle, Value v1, Value v2, Value v3, Value v4, Value v5, Value v6 );
typedef Value (*CFunction7)( Bundle *bundle, Value v1, Value v2, Value v3, Value v4, Value v5, Value v6, Value v7 );

#define TYPE_OF(v) ((Type)(v)->h.type)

#define IS_UNUSED(v) ((v)->h.type==TYPE_UNUSED)
#define IS_SPECIAL(v) ((v)->h.type==TYPE_SPECIAL)
#define IS_INT(v) ((v)->h.type==TYPE_INT)
#define IS_CHAR(v) ((v)->h.type==TYPE_CHAR)
#define IS_SYMBOL(v) ((v)->h.type==TYPE_SYMBOL)
#define IS_STRING(v) ((v)->h.type==TYPE_STRING)
#define IS_STRING_BODY(v) ((v)->h.type==TYPE_STRING_BODY)
#define IS_PAIR(v) ((v)->h.type==TYPE_PAIR)
#define IS_LAMBDA(v) ((v)->h.type==TYPE_LAMBDA)
#define IS_CFUNC(v) ((v)->h.type==TYPE_CFUNC)
#define IS_BUNDLE(v) ((v)->h.type==TYPE_BUNDLE)
#define IS_CONTINUATION(v) ((v)->h.type==TYPE_CONTINUATION)
#define IS_STREAM(v) ((v)->h.type==TYPE_STREAM)
#define IS_POINTER(v) ((v)->h.type==TYPE_POINTER)
#define IS_ERROR(v) ((v)->h.type==TYPE_ERROR)

#define V(v) ((Value)v)
inline Unused* V2UNUSED(Value v){ assert(IS_UNUSED(v)); return (Unused*)v; }
inline Special* V2SPECIAL(Value v){ assert(IS_SPECIAL(v)); return (Special*)v; }
inline int64_t V2INT(Value v){ assert(IS_INT(v)); return ((Integer*)v)->number; }
#define INT2V(v) ((Value)int_new(v))
inline int V2CHAR(Value v){ assert(IS_CHAR(v)); return (int)((Integer*)v)->number; }
#define CHAR2V(v) ((Value)char_new((int)v))
inline Symbol* V2SYMBOL(Value v){ assert(IS_SYMBOL(v)); return (Symbol*)v; }
inline String* V2STRING(Value v){ assert(IS_STRING(v)); return (String*)v; }
inline StringBody* V2STRING_BODY(Value v){ assert(IS_STRING_BODY(v)); return (StringBody*)v; }
inline Pair* V2PAIR(Value v){ assert(IS_PAIR(v)); return (Pair*)v; }
inline Lambda* V2LAMBDA(Value v){ assert(IS_LAMBDA(v)); return (Lambda*)v; }
inline CFunc* V2CFUNC(Value v){ assert(IS_CFUNC(v)); return (CFunc*)v; }
inline Bundle* V2BUNDLE(Value v){ assert(IS_BUNDLE(v)); return (Bundle*)v; }
#define V2CONTINUATION(v) (assert(IS_CONTINUATION(v)),v)
inline Stream* V2STREAM(Value v){ assert(IS_STREAM(v)); return (Stream*)v; }
inline Pointer* V2POINTER(Value v){ assert(IS_POINTER(v)); return (Pointer*)v; }
inline Error* V2ERROR(Value v){ assert(IS_ERROR(v)); return (Error*)v; }

size_t value_to_str( char *buf, int len, Value v );
char* v2s( Value v );
char* v2s_limit( Value v, int limit );
#define v2sn v2s_limit
void vdump( Value v );

bool eq( Value a, Value b );
bool eqv( Value a, Value b );
bool equal( Value a, Value b );
unsigned int hash_eq( Value v );
unsigned int hash_eqv( Value v );
unsigned int hash_equal( Value v );

// Gabage collection in gc.c

extern Pointer* retained;

Value gc_new( Type type );
void gc_init();
void gc_finalize();
void gc_run( int verbose );

void retain( Value *v );
void release( Value *v );

// Dictionary ( hashtable ) in dict.c

typedef struct DictEntry {
	Value key;
	Value val;
	struct DictEntry *next;
} DictEntry;

typedef unsigned int (*HashFunction)( Value v);
typedef bool (*CompareFunction)( Value a, Value b);

typedef struct Dict {
	int size;
	int use;
	HashFunction hash_func;
	CompareFunction comp_func;
	DictEntry *entry[1];
} Dict;

Dict* dict_new( HashFunction hash_func, CompareFunction comp_func );
void dict_free( Dict *d );
DictEntry* dict_find( Dict *d, Value key, bool create );
void dict_set( Dict *d, Value key, Value val );
Value dict_get( Dict *d, Value key );
Dict* dict_rehash( Dict *d );

// Int

Integer* int_new( int64_t i );

// Char

Integer* char_new( int i );

// Symbol

extern Dict *symbol_root;
Symbol* intern( char *sym );

// String

#define STRING_BUF(s) (s->body->buf+s->start)

String* string_new( char *str );
String* string_new_len( char *str, int len );
size_t string_puts( String *s, char *buf, size_t len );
size_t string_puts_escape( String *s, char *buf, size_t len );
String* string_substr( String *s, int start, int len );

// Lambda

Lambda *lambda_new();

// CFunc

CFunc* cfunc_new(int arity, void *func );
void defun( char *sym, int arity, void *func );

// Pair

#define CAR(v) (V2PAIR(v)->car)
#define CDR(v) (V2PAIR(v)->cdr)
#define CAAR(v) (CAR(CAR(v)))
#define CADR(v) (CAR(CDR(v)))
#define CDAR(v) (CDR(CAR(v)))
#define CDDR(v) (CDR(CDR(v)))

#define cons3(v1,v2,v3) (cons( v1, cons( v2, v3 ) ))
#define cons4(v1,v2,v3,v4) (cons( v1, cons( v2, cons( v3, v4 ) ) ))
#define cons5(v1,v2,v3,v4,v5) (cons( v1, cons( v2, cons( v3, cons( v4, v5 ) ) ))
#define cons6(v1,v2,v3,v4,v5,v6) (cons( v1, cons( v2, cons( v3, cons( v4, cons( v5, v6 ) ) ) ))
#define cons7(v1,v2,v3,v4,v5,v6) (cons( v1, cons( v2, cons( v3, cons( v4, cons( v5, v6 ) ) ) ))
#define bind2cdr(list,v1,v2) do{Value _=(list);v1=CAR(_);v2=CDR(_);}while(0);
#define bind3cdr(list,v1,v2,v3) do{Value _=(list);v1=CAR(_);_=CDR(_);v2=CAR(_);v3=CDR(_);}while(0);
#define bind4cdr(list,v1,v2,v3,v4) do{Value _=(list);v1=CAR(_);_=CDR(_);v2=CAR(_);_=CDR(_);v3=CAR(_);v4=CDR(_);}while(0);
#define bind5cdr(list,v1,v2,v3,v4,v5) do{Value _=(list);v1=CAR(_);_=CDR(_);v2=CAR(_);_=CDR(_);v3=CAR(_);_=CDR(_);v4=CAR(_);v5=CDR(_);}while(0);
#define bind6cdr(list,v1,v2,v3,v4,v5,v6) do{Value _=(list);v1=CAR(_);_=CDR(_);v2=CAR(_);_=CDR(_);v3=CAR(_);_=CDR(_);v4=CDR(_);_=CDR(_);v5=CAR(_);v6=CDR(_);}while(0);
#define bind7cdr(list,v1,v2,v3,v4,v5,v6,v7) do{Value _=(list);v1=CAR(_);_=CDR(_);v2=CAR(_);_=CDR(_);v3=CAR(_);_=CDR(_);v4=CDR(_);_=CDR(_);v5=CAR(_);_=CDR(_);v6=CAR(_);v7=CDR(_);}while(0);
#define bind2(list,v1,v2) do{bind2cdr(list,v1,v2);v2=CAR(v2);}while(0);
#define bind3(list,v1,v2,v3) do{bind3cdr(list,v1,v2,v3);v3=CAR(v3);}while(0);
#define bind4(list,v1,v2,v3,v4) do{bind4cdr(list,v1,v2,v3,v4);v4=CAR(v4);}while(0);
#define bind5(list,v1,v2,v3,v4,v5) do{bind5cdr(list,v1,v2,v3,v4,v5);v5=CAR(v5);}while(0);
#define bind6(list,v1,v2,v3,v4,v5,v6) do{bind6cdr(list,v1,v2,v3,v4,v5,v6);v6=CAR(v6);}while(0);
#define bind7(list,v1,v2,v3,v4,v5,v6,v7) do{bind7cdr(list,v1,v2,v3,v4,v5,v6,v6);v7=CAR(v7);}while(0);
#define BINDX(v) if(IS_PAIR(_)){v=CAR(_);_=CDR(_);}else{v=NULL;}
#define bind1arg(list,v1) do{Value _=(list);BINDX(v1);}while(0);
#define bind2arg(list,v1,v2) do{Value _=(list);BINDX(v1);BINDX(v2);}while(0);
#define bind3arg(list,v1,v2,v3) do{Value _=(list);BINDX(v1);BINDX(v2);BINDX(v3);}while(0);
#define bind4arg(list,v1,v2,v3,v4) do{Value _=(list);BINDX(v1);BINDX(v2);BINDX(v3);BINDX(v4);}while(0);
#define bind5arg(list,v1,v2,v3,v4,v5) do{Value _=(list);BINDX(v1);BINDX(v2);BINDX(v3);BINDX(v4);BINDX(v5);}while(0);
#define bind6arg(list,v1,v2,v3,v4,v5,v6) do{Value _=(list);BINDX(v1);BINDX(v2);BINDX(v3);BINDX(v4);BINDX(v5);BINDX(v6);}while(0);
#define bind7arg(list,v1,v2,v3,v4,v5,v6,v7) do{Value _=(list);BINDX(v1);BINDX(v2);BINDX(v3);BINDX(v4);BINDX(v5);BINDX(v6);BINDX(v7);}while(0);

// usage:
//   LIST_EACH(cur,list){
//     vdump(cur);
//   }
#define LIST_EACH(it,list) Value it; for( Value it##_pair=list; it##_pair != NIL && (it=CAR(it##_pair),1); it##_pair=CDR(it##_pair) )

Value cons( Value car, Value cdr );
size_t list_length( Value v );
Value list_copy( Value list );
Value list_tail( Value list );

// Bundle

extern Bundle *bundle_cur;
Bundle *bundle_new( Bundle *upper );
DictEntry* bundle_find( Bundle *b, Symbol *sym, bool find_upper, bool create );
void bundle_set( Bundle *b, Symbol *sym, Value v );
void bundle_define( Bundle *b, Symbol *sym, Value v );
Value bundle_get( Bundle *b, Symbol *sym, Value def );

// Continuation

#define CONTINUATION_BUNDLE(v) (V2CONTINUATION(v)->d.continuation.bundle)
#define CONTINUATION_DATA(v) (V2CONTINUATION(v)->d.continuation.data)
#define CONTINUATION_CODE(v) (CAR(V2CONTINUATION(v)->d.continuation.data))
#define CONTINUATION_NEXT(v) (CDR(V2CONTINUATION(v)->d.continuation.data))

Value continuation_new( Value code, Bundle *bundle, Value next );

// Special

// Stream

Stream* stream_new( FILE *fd, bool close, char *filename );
Stream* stream_new_str( String *str );
int stream_getc( Stream *s );
void stream_ungetc( int c, Stream *s );
int stream_peekc( Stream *s );
Value stream_read_value( Stream *s );
void stream_write_value( Stream *s, Value v );
size_t stream_read( Stream *s, char *buf, size_t len );
size_t stream_write( Stream *s, char *buf, size_t len );
void stream_close( Stream *s );

// Error

// error handling used in c-functions.
#define ERROR_IF_NOT_INT(v) if( !IS_INT(v) ) return error_new( "not integer" );
#define ERROR_IF_NOT_CHAR(v) if( !IS_CHAR(v) ) return error_new( "not char" );
#define ERROR_IF_NOT_SYMBOL(v) if( !IS_SYMBOL(v) ) return error_new( "not symbol" );
#define ERROR_IF_NOT_PAIR(v) if( !IS_PAIR(v) ) return error_new( "not pair" );
#define ERROR_IF_NOT_STRING(v) if( !IS_STRING(v) ) return error_new( "not string" );
#define ERROR_IF_NOT_STREAM(v) if( !IS_STREAM(v) ) return error_new( "not stream" );

Value error_new( char *str );
Value error_newf( char *str, ... );


// Eval in eval.c

Value call( Value lmd, Value vals, Value cont, Value *result );
Value compile( Value code );
Value eval_loop( Stream *s );
Value syntax_expand1( Value code );
Value normalize_let( Value code );
Value vm_compile( Value code );

// Initialization

typedef struct {
	int size;
	int use;
	int alloc_count;
	int cell_count[TYPE_MAX];
} Profile;
extern Profile prof;

extern Value NIL;
extern Value VALUE_T;
extern Value VALUE_F;
extern Value V_EOF;
extern Value V_END_OF_LINE;
extern Stream *V_STDOUT, *V_STDIN, *V_SRC_FILE;

extern Value V_BEGIN;
extern Value V_CALL0;
extern Value V_CALL1;
extern Value V_QUOTE;
extern Value V_DEFINE, V_DEFINE2;
extern Value V_SET_I, V_SET_I2;
extern Value V_LET, V_LET_A, V_LETREC, V_LET2, V_LET3;
extern Value V_LAMBDA, V_MACRO, V_DEFINE_SYNTAX;
extern Value V_IF, V_IF2, V_AND, V_AND2, V_OR, V_OR2;
extern Value V_READ_EVAL, V_READ_EVAL2;

/*{{ define_symbols(
  %w( *compile-hook* quasiquote unquote unquote-splicing
  current-input-port current-output-port end-of-line values
  error syntax-rules syntax-rest
  runtime-load-path runtime-home-path lambda let letrec ) +
  [[".","SYM_DOT"], ["...","SYM_DOT3"]] )
*/
extern Symbol *SYM_A_COMPILE_HOOK_A;
extern Symbol *SYM_QUASIQUOTE;
extern Symbol *SYM_UNQUOTE;
extern Symbol *SYM_UNQUOTE_SPLICING;
extern Symbol *SYM_CURRENT_INPUT_PORT;
extern Symbol *SYM_CURRENT_OUTPUT_PORT;
extern Symbol *SYM_END_OF_LINE;
extern Symbol *SYM_VALUES;
extern Symbol *SYM_ERROR;
extern Symbol *SYM_SYNTAX_RULES;
extern Symbol *SYM_SYNTAX_REST;
extern Symbol *SYM_RUNTIME_LOAD_PATH;
extern Symbol *SYM_RUNTIME_HOME_PATH;
extern Symbol *SYM_LAMBDA;
extern Symbol *SYM_LET;
extern Symbol *SYM_LETREC;
extern Symbol *SYM_DOT;
extern Symbol *SYM_DOT3;
/*}}*/

extern bool opt_trace;
extern bool opt_debug;

void init( const char *argv0 );
void init_prelude( const char *argv0, bool with_prelude );
void finalize();
void show_prof();

void cfunc_init();

