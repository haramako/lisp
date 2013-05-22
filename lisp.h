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
	TYPE_SYMBOL,
	TYPE_STRING,
	TYPE_PAIR,
	TYPE_LAMBDA,
	TYPE_CFUNC,
	TYPE_BUNDLE,
	TYPE_CONTINUATION,
	TYPE_SPECIAL,
	TYPE_STREAM,
	TYPE_MAX,
} Type;

extern const char *TYPE_NAMES[];

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
	OP_EXEC_MACRO,
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

typedef enum {
	LAMBDA_TYPE_LAMBDA = 0,
	LAMBDA_TYPE_MACRO,
} LambdaType;

extern const char* LAMBDA_TYPE_NAME[];

typedef struct Cell* Value;

#define CFUNC_VARIABLE 127

typedef Value (*CFunction)( Value args, Value cont, Value *result );
typedef Value (*CFunction0)( Value bundle );
typedef Value (*CFunction1)( Value bundle, Value v1 );
typedef Value (*CFunction2)( Value bundle, Value v1, Value v2 );
typedef Value (*CFunction3)( Value bundle, Value v1, Value v2, Value v3 );
typedef Value (*CFunction4)( Value bundle, Value v1, Value v2, Value v3, Value v4 );
typedef Value (*CFunction5)( Value bundle, Value v1, Value v2, Value v3, Value v4, Value v5 );
typedef Value (*CFunction6)( Value bundle, Value v1, Value v2, Value v3, Value v4, Value v5, Value v6 );
typedef Value (*CFunction7)( Value bundle, Value v1, Value v2, Value v3, Value v4, Value v5, Value v6, Value v7 );

typedef struct Cell {
	char type;
	char marked;
	int16_t flag;
	union {
		int64_t number;
		struct {
			Value next;
		} unused;
		struct {
			Value car;
			Value cdr;
		} pair;
		struct {
			Value str;
		} symbol;
		struct {
			char *str;
		} string;
		struct {
			struct Dict *dict;
			Value data; // ( upper . lambda )
		} bundle;
		struct {
			Value bundle;
			Value data; // (name arg . body)
		} lambda;
		struct {
			int arity;
			void *func;
		} cfunc;
		struct {
			Operator op;
			char *str;
		} special;
		struct {
			Value bundle;
			Value data; // (code . next )
		} continuation;
		struct {
			FILE *fd;
            Value filename;
		} stream;
	} d;
} Cell;

#define TYPE_OF(v) ((Type)v->type)

#define IS_INT(v) (v->type==TYPE_INT)
#define IS_SYMBOL(v) ((v)->type==TYPE_SYMBOL)
#define IS_STRING(v) ((v)->type==TYPE_STRING)
#define IS_PAIR(v) ((v)->type==TYPE_PAIR)
#define IS_LAMBDA(v) ((v)->type==TYPE_LAMBDA)
#define IS_CFUNC(v) ((v)->type==TYPE_CFUNC)
#define IS_BUNDLE(v) ((v)->type==TYPE_BUNDLE)
#define IS_CONTINUATION(v) ((v)->type==TYPE_CONTINUATION)
#define IS_SPECIAL(v) ((v)->type==TYPE_SPECIAL)
#define IS_STREAM(v) ((v)->type==TYPE_STREAM)

#define V2INT(v) (assert(IS_INT(v)),v->d.number)
#define INT2V(v) (int_new(v))
#define V2SYMBOL(v) (assert(IS_SYMBOL(v)),v)
#define V2STRING(v) (assert(IS_STRING(v)),v)
#define V2PAIR(v) (assert(IS_PAIR(v)),v)
#define V2LAMBDA(v) (assert(IS_LAMBDA(v)),v)
#define V2CFUNC(v) (assert(IS_CFUNC(v)),v)
#define V2BUNDLE(v) (assert(IS_BUNDLE(v)),v)
#define V2CONTINUATION(v) (assert(IS_CONTINUATION(v)),v)
#define V2SPECIAL(v) (assert(IS_SPECIAL(v)),v)
#define V2STREAM(v) (assert(IS_STREAM(v)),v)

size_t value_to_str( char *buf, int len, Value v );
char* v2s( Value v );
char* v2s_limit( Value v, int limit );
void vdump( Value v );

bool eq( Value a, Value b );
bool eqv( Value a, Value b );
bool equal( Value a, Value b );
unsigned int hash_eqv( Value v );

// Gabage collection in gc.c

extern Value retained;

Value gc_new( Type type );
void gc_init();
void gc_finalize();
void gc_run( int verbose );

Value retain( Value v );
Value release( Value v );

// Dictionary ( Hash Map ) in dict.c

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

Value int_new( int64_t i );

// Symbol

#define SYMBOL_STR(v) (V2SYMBOL(v)->d.symbol.str)
#define SYMBOL_NEXT(v) (V2SYMBOL(v)->d.symbol.next)

extern Value symbol_root;
Value intern( char *sym );

// String

#define STRING_STR(v) (V2STRING(v)->d.string.str)
Value string_new( char *str );

// Lambda

#define LAMBDA_TYPE(v) (V2LAMBDA(v)->flag)
#define LAMBDA_DATA(v) (V2LAMBDA(v)->d.lambda.data)
#define LAMBDA_NAME(v) (CAR(V2LAMBDA(v)->d.lambda.data))
#define LAMBDA_ARGS(v) (CAR(CDR(V2LAMBDA(v)->d.lambda.data)))
#define LAMBDA_BODY(v) (CDR(CDR(V2LAMBDA(v)->d.lambda.data)))
#define LAMBDA_BUNDLE(v) (V2LAMBDA(v)->d.lambda.bundle)

Value lambda_new();

// CFunc

#define CFUNC_ARITY(v) (V2CFUNC(v)->d.cfunc.arity)
#define CFUNC_FUNC(v) (V2CFUNC(v)->d.cfunc.func)

Value cfunc_new(int arity, void *func );
void defun( char *sym, int arity, void *func );

// Pair

#define CAR(v) (V2PAIR(v)->d.pair.car)
#define CDR(v) (V2PAIR(v)->d.pair.cdr)
#define CAAR(v) (CAR(CAR(v)))
#define CADR(v) (CAR(CDR(v)))
#define CDAR(v) (CDR(CAR(v)))
#define CDDR(v) (CDR(CDR(v)))

#define cons3(v1,v2,v3) (cons( v1, cons( v2, v3 ) ))
#define cons4(v1,v2,v3,v4) (cons( v1, cons( v2, cons( v3, v4 ) ) ))
#define cons5(v1,v2,v3,v4,v5) (cons( v1, cons( v2, cons( v3, cons( v4, v5 ) ) ))
#define cons6(v1,v2,v3,v4,v5,v6) (cons( v1, cons( v2, cons( v3, cons( v4, cons( v5, v6 ) ) ) ))
#define bind2cdr(list,v1,v2) do{Value _=(list);v1=CAR(_);v2=CDR(_);}while(0);
#define bind3cdr(list,v1,v2,v3) do{Value _=(list);v1=CAR(_);_=CDR(_);v2=CAR(_);v3=CDR(_);}while(0);
#define bind4cdr(list,v1,v2,v3,v4) do{Value _=(list);v1=CAR(_);_=CDR(_);v2=CAR(_);_=CDR(_);v3=CAR(_);v4=CDR(_);}while(0);
#define bind2(list,v1,v2) do{bind2cdr(list,v1,v2);v2=CAR(v2);}while(0);
#define bind3(list,v1,v2,v3) do{bind3cdr(list,v1,v2,v3);v3=CAR(v3);}while(0);
#define bind4(list,v1,v2,v3,v4) do{bind4cdr(list,v1,v2,v3,v4);v4=CAR(v4);}while(0);
#define BINDX(v) if(IS_PAIR(_)){v=CAR(_);_=CDR(_);}else{v=NULL;}
#define bind1arg(list,v1) do{Value _=(list);BINDX(v1);}while(0);
#define bind2arg(list,v1,v2) do{Value _=(list);BINDX(v1);BINDX(v2);}while(0);
#define bind3arg(list,v1,v2,v3) do{Value _=(list);BINDX(v1);BINDX(v2);BINDX(v3);}while(0);

Value cons( Value car, Value cdr );
size_t list_length( Value v );
Value list_copy( Value list );

// Bundle

#define BUNDLE_DICT(v) (V2BUNDLE(v)->d.bundle.dict)
#define BUNDLE_DATA(v) (V2BUNDLE(v)->d.bundle.data)
#define BUNDLE_UPPER(v) (CAR(V2BUNDLE(v)->d.bundle.data))
#define BUNDLE_LAMBDA(v) (CDR(V2BUNDLE(v)->d.bundle.data))

extern Value bundle_cur;
Value bundle_new( Value upper );
DictEntry* bundle_find( Value b, Value sym, bool find_upper, bool create );
void bundle_set( Value b, Value sym, Value v );
void bundle_define( Value b, Value sym, Value v );
Value bundle_get( Value b, Value sym, Value def );

// Continuation

#define CONTINUATION_BUNDLE(v) (V2CONTINUATION(v)->d.continuation.bundle)
#define CONTINUATION_DATA(v) (V2CONTINUATION(v)->d.continuation.data)
#define CONTINUATION_CODE(v) (CAR(V2CONTINUATION(v)->d.continuation.data))
#define CONTINUATION_NEXT(v) (CDR(V2CONTINUATION(v)->d.continuation.data))

Value continuation_new( Value code, Value bundle, Value next );

// Special

#define SPECIAL_OP(v) (V2SPECIAL(v)->d.special.op)
#define SPECIAL_STR(v) (V2SPECIAL(v)->d.special.str)

// Stream

#define STREAM_MASK_CLOSE (1>>15)
#define STREAM_MASK_LINE ((1>>15)-1)
#define STREAM_FD(v) (V2STREAM(v)->d.stream.fd)
#define STREAM_CLOSE(v) ((V2STREAM(v)->flag) & STREAM_MASK_CLOSE)
#define STREAM_FILENAME(v) (V2STREAM(v)->d.stream.filename)
#define STREAM_LINE(v) (V2STREAM(v)->flag & STREAM_MASK_LINE)

Value stream_new( FILE *fd, bool close, char *filename );
int stream_getc( Value s );
void stream_ungetc( int c, Value s );
int stream_peekc( Value s );
Value stream_read( Value s );
Value stream_write( Value s, Value v );
	
// Eval

Value call( Value lmd, Value vals, Value cont, Value *result );
Value compile( Value code );
Value eval_loop( Value v );
Value syntax_expand1( Value code );

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
extern Value V_STDOUT, V_STDIN, V_SRC_FILE, V_END_OF_LINE;

extern Value V_BEGIN;
extern Value V_CALL0;
extern Value V_CALL1;
extern Value V_QUOTE;
extern Value V_DEFINE, V_DEFINE2;
extern Value V_SET_I, V_SET_I2;
extern Value V_LET, V_LET_A, V_LETREC, V_LET2, V_LET3;
extern Value V_LAMBDA, V_MACRO, V_EXEC_MACRO, V_DEFINE_SYNTAX;
extern Value V_IF, V_IF2, V_AND, V_AND2, V_OR, V_OR2;
extern Value V_READ_EVAL, V_READ_EVAL2;

extern Value SYM_A_DEBUG_A, SYM_A_COMPILE_HOOK_A, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING,
	SYM_CURRENT_INPUT_PORT, SYM_CURRENT_OUTPUT_PORT, SYM_END_OF_LINE, SYM_VALUES,
	SYM_DOT, SYM_DOT3, SYM_ERROR, SYM_SYNTAX_RULES, SYM_SYNTAX_REST;

extern bool opt_trace;
extern bool opt_debug;

void init();
void init_prelude( bool with_prelude );
void cfunc_init();
void finalize();
void show_prof();

