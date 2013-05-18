#include <stdio.h>
#include <stdlib.h>
#include <malloc/malloc.h>
#include <stdint.h>
#include "lisp.h"

static int _color = 0;
static Arena *_arena_root = NULL;
static Value _cell_next = NULL;

GcStat gc_stat;

Value gc_new( Type type )
{
	// 残りがない！
	if( !_cell_next ){
		Arena *arena = malloc(sizeof(Arena));
		for( int i=0; i<ARENA_SIZE; i++ ){
			arena->cells[i].type = TYPE_UNUSED;
			arena->cells[i].d.unused.next = (i < ARENA_SIZE-1)?(&arena->cells[i+1]):(NULL);
		}
		_cell_next = &arena->cells[0];
		arena->next = _arena_root;
		_arena_root = arena;
		gc_stat.size += ARENA_SIZE;
	}
	
	Cell *cell = _cell_next;
	assert( cell->type == TYPE_UNUSED );
	_cell_next = cell->d.unused.next;
	cell->type = type;
	cell->marked = -1;
	// printf( "cell: %p\n", cell );
	gc_stat.use++;
	gc_stat.alloc_count++;
	return cell;
}

void gc_mark( Value v )
{
	if( !v ) return;
	
	// printf( "mark: %p\n", p );
	if( v->marked == _color ) return;
	// printf( "mark %p\n", p );
	v->marked = _color;
	
	// display_val( "mark: ", (Value)p );
	switch( TYPE_OF(v) ){
	case TYPE_UNUSED:
		assert(0);
	case TYPE_NIL:
	case TYPE_INT:
	case TYPE_BOOL:
		break;
	case TYPE_SYMBOL:
		gc_mark( SYMBOL_NEXT(v) );
		break;
	case TYPE_STRING:
		break;
	case TYPE_SPECIAL:
		break;
	case TYPE_PAIR:
		gc_mark( CAR(v) );
		gc_mark( CDR(v) );
		break;
	case TYPE_SLOT:
		gc_mark( SLOT_SYM(v) );
		gc_mark( SLOT_VAL(v) );
		gc_mark( SLOT_NEXT(v) );
		break;
	case TYPE_LAMBDA:
		gc_mark( LAMBDA_ARGS(v) );
		gc_mark( LAMBDA_BODY(v) );
		gc_mark( LAMBDA_BUNDLE(v) );
		break;
	case TYPE_BUNDLE:
		gc_mark( BUNDLE_SLOT(v) );
		gc_mark( BUNDLE_UPPER(v) );
		break;
	case TYPE_CONTINUATION:
		gc_mark( CONTINUATION_BUNDLE(v) );
		gc_mark( CONTINUATION_CODE(v) );
		gc_mark( CONTINUATION_NEXT(v) );
		break;
	case TYPE_STREAM:
		break;
	}
}

static void _free( void *p )
{
	Value v = p;
	switch( TYPE_OF(v) ){
	case TYPE_STRING:
		free( STRING_STR(v) );
		break;
	case TYPE_STREAM:
		if( STREAM_CLOSE(v) ){
			// display_val( "_free: close ", v );
			fclose( STREAM_FD(v) );
		}
		break;
	default:
		break;
	}
	v->type = TYPE_UNUSED;
	v->d.unused.next = _cell_next;
	_cell_next = v;
	gc_stat.use--;
}

Value retained = NULL;

static void _root_mark()
{
	gc_mark( (void*)retained );
	gc_mark( (void*)bundle_cur );
	gc_mark( (void*)symbol_root );
}

Value retain( Value v ){
	retained = cons( v, retained ); return v;
}

Value release( Value v )
{
	for( Value *cur=&retained; *cur != NIL; cur = &CDR(*cur) ){
		if( CAR(*cur) == v ){
			if( CDR(*cur) ){
				CAR(*cur) = CADR(*cur);
				CDR(*cur) = CDDR(*cur);
			}else{
				*cur = NIL;
			}
			break;
		}
	}
	return v;
}



void gc()
{
	gc_run( true );
}


void gc_init()
{
}

void gc_run( int verbose )
{
	_color = 1 - _color;
	_root_mark();

	// sweep
	int all = 0, kill = 0;
	for( Arena *arena = _arena_root; arena != NULL; arena = arena->next ){
		for( int i=0; i<ARENA_SIZE; i++){
			Value cur = &arena->cells[i];
			if( TYPE_OF(cur) == TYPE_UNUSED ) continue;
			all++;

			if( cur->marked != _color ){
				// printf( "sweep %p\n", cur );
				_free( cur );
				kill++;
			}
		}
	}
	if( verbose ) printf( "finish gc. %d - %d => %d\n", all, kill, all-kill );
}

