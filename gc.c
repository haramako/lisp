#include <stdio.h>
#include <stdlib.h>
#include <malloc/malloc.h>
#include <stdint.h>
#include <math.h>
#include "lisp.h"

#define ARENA_SIZE 256*1024

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
Pointer* retained = NULL;

static Cell* _alloc( int arena_idx )
{
	if( !_cell_next[arena_idx] ){
		Arena *arena = malloc(ARENA_SIZE);
		int cell_size = _arena_size[arena_idx];
		int count = ( ARENA_SIZE - sizeof(Arena) ) / cell_size;
		void *cur = ARENA_ENTRY(arena);
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
	}
	
	return _cell_next[arena_idx];
}

Value gc_new( Type type )
{
	int arena_idx;
	switch( type ){
	case TYPE_STREAM:
	case TYPE_STRING:
	case TYPE_STRING_BODY:
	case TYPE_BUNDLE:
		arena_idx = 1;
		break;
	default:
		arena_idx = 0;
	}
	
	// allocate cell
	Cell *cell = _alloc(arena_idx);
	assert( cell->h.type == TYPE_UNUSED );
	_cell_next[arena_idx] = cell->d.unused.next;
	cell->h.type = type;
	cell->h.marked = -1;
	prof.use++;
	prof.alloc_count++;
	prof.cell_count[type]++;

	return cell;
}

void retain( Value *v )
{
	Pointer *p = V2POINTER(gc_new(TYPE_POINTER));
	p->ptr = v;
	p->next = retained;
	retained = p;
}

void release( Value *v )
{
	for( Pointer **cur=&retained; *cur; cur = &((*cur)->next) ){
		if( (*cur)->ptr != v ) continue;
		if( (*cur)->next ){
			(*cur)->ptr = (*cur)->next->ptr;
			(*cur)->next = (*cur)->next->next;
		}else{
			*cur = NULL;
		}
	}
}

static void _mark( Value v );

static void _mark_dict( Dict *d )
{
	for( int i=0; i<d->size; i++ ){
		for( DictEntry *cur = d->entry[i]; cur; cur = cur->next ){
			_mark( cur->key );
			_mark( cur->val );
		}
	}
}

static void _mark( Value v )
{
	if( !v ) return;
	
	if( v->h.marked == _color ) return;
	v->h.marked = _color;
	
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
		_mark( (Value)V2SYMBOL(v)->str );
		break;
	case TYPE_STRING_BODY:
		break;
	case TYPE_STRING:
		_mark( (Value)V2STRING(v)->body );
		break;
	case TYPE_SPECIAL:
		break;
	case TYPE_PAIR:
		_mark( CAR(v) );
		_mark( CDR(v) );
		break;
	case TYPE_LAMBDA:
		_mark( LAMBDA_DATA(v) );
		_mark( (Value)LAMBDA_BUNDLE(v) );
		break;
	case TYPE_CFUNC:
		_mark( CFUNC_NAME(v) );
		break;
	case TYPE_BUNDLE:
		{
			Bundle *b = V2BUNDLE(v);
			_mark_dict( b->dict );
			_mark( (Value)b->upper );
			_mark( b->lambda );
		}
		break;
	case TYPE_CONTINUATION:
		_mark( (Value)CONTINUATION_BUNDLE(v) );
		_mark( CONTINUATION_DATA(v) );
		break;
	case TYPE_STREAM:
		{
			Stream *s = V2STREAM(v);
			if( s->stream_type == STREAM_TYPE_FILE ){
				_mark( (Value)s->u.file.filename );
			}else{
				_mark( (Value)s->u.str );
			}
		}
		break;
	case TYPE_POINTER:
		{
			Pointer *p = V2POINTER(v);
			_mark( *(p->ptr) );
			_mark( (Value)p->next );
		}
		break;
	case TYPE_ERROR:
		_mark( (Value)V2ERROR(v)->str );
	}
}

static void _free( Value v )
{
	switch( TYPE_OF(v) ){
	case TYPE_STRING_BODY:
		free( V2STRING_BODY(v)->buf );
		break;
	case TYPE_BUNDLE:
		dict_free( V2BUNDLE(v)->dict );
		break;
	case TYPE_STREAM:
		{
			Stream *s = V2STREAM(v);
			if( s->stream_type == STREAM_TYPE_FILE ){
				if( s->u.file.close && s->u.file.fd != 0 ) fclose( s->u.file.fd );
			}
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
			free( cur );
		}
	}
}

void gc_run( int verbose )
{
	_color = 1 - _color;
	
	// root mark
	_mark( (Value)retained );

	// sweep
	int all = 0, kill = 0;
	for( int arena_idx=0; arena_idx<2; arena_idx++ ){
		for( Arena *arena = _arena_root[arena_idx]; arena != NULL; arena = arena->next ){
			void *p = ARENA_ENTRY(arena);
			int count = arena->count;
			int size = arena->size;
			for( int i=0; i<count; i++, p+=size){
				Value cur = (Cell*)p;
				if( cur->h.type == TYPE_UNUSED ) continue;
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

