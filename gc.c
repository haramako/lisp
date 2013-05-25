#include <stdio.h>
#include <stdlib.h>
#include <malloc/malloc.h>
#include <stdint.h>
#include <math.h>
#include "lisp.h"

#define ARENA_SIZE 64*1024

typedef struct Arena {
	struct Arena *next;
	int size;
	int count;
} Arena;

#define ARENA_ENTRY(a) (((void*)a)+sizeof(Arena))

static int _color = 0;
static Arena *_arena_root[2] = { NULL, NULL };
static Value _cell_next[2] = { NULL, NULL };
static int _arena_size[2] = { 0, 0 };
Value retained = NULL;

Arena* arena_new( int arena_idx )
{
	Arena *arena = malloc(ARENA_SIZE);
	int cell_size = _arena_size[arena_idx];
	int count = ( ARENA_SIZE - sizeof(Arena) ) / cell_size;
	void *cur = ARENA_ENTRY(arena);
	// printf( "arena_new: %d %d %p\n", cell_size, count, cur );
	for( int i=0; i<count; i++, cur += cell_size ){
		((Cell*)cur)->h.type = TYPE_UNUSED;
		((Cell*)cur)->d.unused.next = (i<(count-1))?(cur+cell_size):(NULL);
	}
	arena->size = cell_size;
	arena->count = count;
	arena->next = _arena_root[arena_idx];
	_arena_root[arena_idx] = arena;
	_cell_next[arena_idx] = ARENA_ENTRY(arena);
	prof.size += count;
	return arena;
}

Value gc_new( Type type )
{
	int arena_idx;
	switch( type ){
	case TYPE_STREAM:
		arena_idx = 1;
		break;
	default:
		arena_idx = 0;
	}
	if( !_cell_next[arena_idx] ) arena_new( arena_idx );
	
	// cellのallocate
	Cell *cell = _cell_next[arena_idx];
	assert( cell->h.type == TYPE_UNUSED );
	_cell_next[arena_idx] = cell->d.unused.next;
	cell->h.type = type;
	cell->h.marked = -1;
	prof.use++;
	prof.alloc_count++;
	prof.cell_count[type]++;

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
	if( v->h.marked == _color ) return;
	// printf( "mark %p\n", p );
	v->h.marked = _color;
	
	// display_val( "mark: ", (Value)p );
	switch( TYPE_OF(v) ){
	case TYPE_UNUSED:
	case TYPE_MAX:
		assert(0);
	case TYPE_NIL:
	case TYPE_INT:
	case TYPE_CHAR:
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
		_mark( LAMBDA_DATA(v) );
		_mark( LAMBDA_BUNDLE(v) );
		break;
	case TYPE_CFUNC:
		_mark( CFUNC_NAME(v) );
		break;
	case TYPE_BUNDLE:
		_mark_dict( BUNDLE_DICT(v) );
		_mark( BUNDLE_DATA(v) );
		break;
	case TYPE_CONTINUATION:
		_mark( CONTINUATION_BUNDLE(v) );
		_mark( CONTINUATION_DATA(v) );
		break;
	case TYPE_STREAM:
		_mark( V2STREAM(v)->filename );
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
		{
			Stream *s = V2STREAM(v);
			if( s->close ) fclose( s->fd );
		}
		break;
	default:
		break;
	}
}

void gc_init()
{
	_arena_size[0] = sizeof(Cell);
	_arena_size[1] = sizeof(Cell);
	if( _arena_size[1] < sizeof(Stream) ) _arena_size[1] = sizeof(Stream);
}

void gc_finalize()
{
	for( int arena_idx = 0; arena_idx<2; arena_idx++ ){
		while( _arena_root[arena_idx] != NULL ){
			Arena *cur = _arena_root[arena_idx];
			_arena_root[arena_idx] = _arena_root[arena_idx]->next;
			// printf( "free arena %p\n", cur );
			free( cur );
		}
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
	for( int arena_idx=0; arena_idx<2; arena_idx++ ){
		for( Arena *arena = _arena_root[arena_idx]; arena != NULL; arena = arena->next ){
			void *p = ARENA_ENTRY(arena);
			int count = arena->count;
			int size = arena->size;
			for( int i=0; i<count; i++, p+=size){
				Value cur = (Cell*)p;
				if( TYPE_OF(cur) == TYPE_UNUSED ) continue;
				all++;

				if( cur->h.marked != _color ){
					_free( cur );
					cur->h.type = TYPE_UNUSED;
					cur->d.unused.next = _cell_next[arena_idx];
					_cell_next[arena_idx] = cur;
					prof.use--;
					kill++;
				}
			}
		}
	}
	if( verbose ) printf( "finish gc. %d - %d => %d\n", all, kill, all-kill );
}

