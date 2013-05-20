#include <stdio.h>
#include <stdlib.h>
#include <malloc/malloc.h>
#include <stdint.h>
#include "lisp.h"

#define ARENA_SIZE ((64*1024-sizeof(void*))/sizeof(Cell))
typedef struct Arena {
	struct Arena *next;
	Cell cells[ARENA_SIZE];
} Arena;

static int _color = 0;
static Arena *_arena_root = NULL;
static Value _cell_next = NULL;
Value retained = NULL;

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
		prof.size += ARENA_SIZE;
	}

	// cellのallocate
	Cell *cell = _cell_next;
	assert( cell->type == TYPE_UNUSED );
	_cell_next = cell->d.unused.next;
	cell->type = type;
	cell->marked = -1;
	prof.use++;
	prof.alloc_count++;
	return cell;
}

Value retain( Value v )
{
	retained = cons( v, retained );
	return v;
}

Value release( Value v )
{
	for( Value *cur=&retained; *cur != NIL; cur = &CDR(*cur) ){
		if( CAR(*cur) != v ) continue;
		if( CDR(*cur) ){
			CAR(*cur) = CADR(*cur);
			CDR(*cur) = CDDR(*cur);
		}else{
			*cur = NIL;
		}
	}
	return v;
}

static void _mark( Value v );

static void _mark_dict( Dict *d )
{
	for( int i=0; i<d->size; i++ ){
		for( DictEntry *cur = d->entry[i]; cur; cur = cur->next ){
			// display_val( "mark_dict: ", cons( cur->key, cur->val ) );
			_mark( cur->key );
			_mark( cur->val );
		}
	}
}

static void _mark( Value v )
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
		_mark( SYMBOL_STR(v) );
		break;
	case TYPE_STRING:
		break;
	case TYPE_SPECIAL:
		break;
	case TYPE_PAIR:
		_mark( CAR(v) );
		_mark( CDR(v) );
		break;
	case TYPE_LAMBDA:
		_mark( LAMBDA_ARGS(v) );
		_mark( LAMBDA_BODY(v) );
		_mark( LAMBDA_BUNDLE(v) );
		break;
	case TYPE_BUNDLE:
		_mark_dict( BUNDLE_DICT(v) );
		_mark( BUNDLE_UPPER(v) );
		break;
	case TYPE_CONTINUATION:
		_mark( CONTINUATION_BUNDLE(v) );
		_mark( CONTINUATION_CODE(v) );
		_mark( CONTINUATION_NEXT(v) );
		break;
	case TYPE_STREAM:
		_mark( STREAM_FILENAME(v) );
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
	case TYPE_BUNDLE:
		dict_free( BUNDLE_DICT(v) );
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
	prof.use--;
}

void gc_init()
{
}

void gc_finalize()
{
    while( _arena_root != NULL ){
        Arena *cur = _arena_root;
        _arena_root = _arena_root->next;
        // printf( "free arena %p\n", cur );
        free( cur );
    }
}

void gc_run( int verbose )
{
	_color = 1 - _color;
	
	// root mark
	_mark( (void*)retained );
	_mark( (void*)bundle_cur );
	_mark( (void*)symbol_root );

	// sweep
	int all = 0, kill = 0;
	for( Arena *arena = _arena_root; arena != NULL; arena = arena->next ){
		for( int i=0; i<ARENA_SIZE; i++){
			Value cur = &arena->cells[i];
			if( TYPE_OF(cur) == TYPE_UNUSED ) continue;
			all++;

			if( cur->marked != _color ){
				_free( cur );
				kill++;
			}
		}
	}
	if( verbose ) printf( "finish gc. %d - %d => %d\n", all, kill, all-kill );
}

