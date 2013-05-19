#include "lisp.h"
#include <string.h>
#include <stdlib.h>

unsigned int value_hash( Value v )
{
	switch( TYPE_OF(v) ){
	case TYPE_INT:
		return (unsigned int)V2INT(v);
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
		return (unsigned int)(((uintptr_t)v) >> 4) * 31;
	}
}

#define DICT_INIT_SIZE 15

Dict* dict_new_size( int size )
{
	// printf( "dict_new_size: %d\n", size );
	Dict *dict = malloc(sizeof(Dict)+sizeof(DictEntry*)*(size-1));
	dict->size = size;
	memset( &dict->entry, 0, sizeof(DictEntry*)*size );
	return dict;
}

Dict* dict_new()
{
	return dict_new_size(DICT_INIT_SIZE);
}

void dict_free( Dict *d )
{
	for( int i=0; i<d->size; i++ ){
        for( DictEntry *cur = d->entry[i]; cur; ){
            DictEntry *next = cur->next;
            free( cur );
            cur = next;
		}
	}
	free( d );
}

Dict* dict_rehash( Dict *d )
{
	// printf( "rehash %p size:%d\n", d, d->size );
	Dict *new_dict = dict_new_size( d->size * 1.7 );
	for( int i=0; i<d->size; i++ ){
		for( DictEntry *cur = d->entry[i]; cur; cur = cur->next ){
			dict_set( new_dict, cur->key, cur->val );
		}
	}
	dict_free( d );
	return new_dict;
}

DictEntry* dict_find( Dict *d, Value key, bool create )
{
	int idx = value_hash( key ) % d->size;
	assert( idx < d->size );
	for( DictEntry *cur = d->entry[idx]; cur; cur = cur->next ){
		// display_val( "dict_find: ", cons( cur->key, key ) );
		if( eqv( cur->key, key ) ) return cur;
	}
	if( create ){
		DictEntry *new_entry = malloc(sizeof(DictEntry));
		new_entry->key = key;
		new_entry->val = NIL;
		new_entry->next = d->entry[idx];
		d->entry[idx] = new_entry;
		d->use++;
		return new_entry;
	}else{
		return NULL;
	}
}

void dict_set( Dict *d, Value key, Value val )
{
	DictEntry *entry = dict_find( d, key, true );
	entry->val = val;
}

Value dict_get( Dict *d, Value key )
{
	DictEntry *entry = dict_find( d, key, false );
	if( entry ){
		return entry->val;
	}else{
		return NULL;
	}
}

