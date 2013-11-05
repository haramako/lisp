#include "lisp.h"
#include <stdlib.h>

//********************************************************
// Evaluation
//********************************************************

typedef enum {
	VM_NONE,
	VM_DEFINE,
	VM_DEFINE_GLOBAL,
	VM_PUSH,
	VM_PUSH_SYMBOL,
	VM_MAX,
} VmInst;

const char *VM_INST_NAME[] = {
	"none",
	"define",
	"define-global",
	"push",
	"push-symbol",
};

typedef struct VmCode {
	char code[8192];
	char *tail;
	Value values[256];
	int value_num;
} VmCode;

VmCode* vm_code_new()
{
	VmCode *c = malloc( sizeof(VmCode) );
	c->tail = c->code;
	c->value_num = 0;
	return c;
}

VmCode* vm_code_emit_value( VmCode *c, Value v )
{
	*((Value*)c->tail) = v;
	c->tail += sizeof(Value);
	return c;
}

VmCode* vm_code_emit( VmCode *c, VmInst op, Value v )
{
	*((char*)c->tail) = (char)op;
	c->tail++;
	vm_code_emit_value( c, v );
	return c;
}

void vm_code_printf( VmCode *c )
{
	void *cur = c->code;
	while( cur < c->tail ){
		VmInst inst = *((char*)cur);
		printf( "%04d: %s ", (int)(cur - (void*)c->code), VM_INST_NAME[inst] );
		cur++;
		switch( inst ){
		case VM_DEFINE: case VM_DEFINE_GLOBAL: case VM_PUSH: case VM_PUSH_SYMBOL:
			printf( "%s\n", v2s(*((Value*)cur)) );
			cur += sizeof(Value);
			break;
		default:
			assert(0);
		}
	}
}

void _compile( VmCode *vc, Bundle *bundle, Value code );

void _compile_call( VmCode *vc, Bundle *bundle, Value code )
{
	Value op = bundle_get( bundle, V2SYMBOL(CAR(code)), NULL );
	switch( TYPE_OF(op) ){
	case TYPE_SPECIAL:
		switch( V2SPECIAL(op)->op ){
		case OP_DEFINE:
			{
				Value sym, val;
				bind2( CDR(code), sym, val);
				_compile( vc, bundle, val );
				Value sym_slot = bundle_get( bundle, V2SYMBOL(sym), NULL );
				if( sym_slot ){
					vm_code_emit( vc, VM_DEFINE, sym_slot );
				}else{
					vm_code_emit( vc, VM_DEFINE_GLOBAL, sym );
				}
			}
			break;
		default:
			assert(0);
		}
	case TYPE_LAMBDA:
	case TYPE_CFUNC:
		break;
	default:
		assert(0);
	}
}


void _compile( VmCode *vc, Bundle *bundle, Value code )
{
	switch( TYPE_OF(code) ){
	case TYPE_NIL:
	case TYPE_BOOL:
	case TYPE_INT:
	case TYPE_STRING:
		vm_code_emit( vc, VM_PUSH, code );
		break;
	case TYPE_SYMBOL:
		vm_code_emit( vc, VM_PUSH_SYMBOL, code );
		break;
	case TYPE_PAIR:
		_compile_call( vc, bundle, code );
		break;
	default:
		assert(0);
	}
}

Value vm_compile( Value code )
{
	Bundle *bundle = bundle_cur;
	VmCode *vc = vm_code_new();
	printf( "> %s\n", v2sn(code,100) );
	_compile( vc, bundle, code );
	vm_code_printf( vc );
	printf( "\n" );
	return NIL;
}
