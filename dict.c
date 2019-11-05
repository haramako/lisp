#include "lisp.h"
#include <string.h>
#include <stdlib.h>

#define DICT_INIT_SIZE 15

Dict* dict_new_size( int size, HashFunction hash_func, CompareFunction comp_func )
{
	Dict *dict = malloc(sizeof(Dict) + sizeof(DictEntry*) * (size - 1));
	dict->size = size;
	dict->hash_func = hash_func;
	dict->comp_func = comp_func;
	memset( &dict->entry, 0, sizeof(DictEntry*)*size );
	return dict;
}

Dict* dict_new( HashFunction hash_func, CompareFunction comp_func )
{
	return dict_new_size(DICT_INIT_SIZE, hash_func, comp_func );
}

void dict_free( Dict *d )
{
	for( int i = 0; i < d->size; i++ ) {
		for( DictEntry *cur = d->entry[i]; cur; ) {
			DictEntry *next = cur->next;
			free( cur );
			cur = next;
		}
	}
	free( d );
}

Dict* dict_rehash( Dict *d )
{
	Dict *new_dict = dict_new_size( d->size * 1.7, d->hash_func, d->comp_func );
	for( int i = 0; i < d->size; i++ ) {
		for( DictEntry *cur = d->entry[i]; cur; cur = cur->next ) {
			dict_set( new_dict, cur->key, cur->val );
		}
	}
	dict_free( d );
	return new_dict;
}

DictEntry* dict_find( Dict *d, Value key, bool create )
{
	int idx = d->hash_func( key ) % d->size;
	assert( idx < d->size );
	for( DictEntry *cur = d->entry[idx]; cur; cur = cur->next ) {
		if( d->comp_func( cur->key, key ) ) return cur;
	}
	if( create ) {
		DictEntry *new_entry = malloc(sizeof(DictEntry));
		new_entry->key = key;
		new_entry->val = NIL;
		new_entry->next = d->entry[idx];
		d->entry[idx] = new_entry;
		d->use++;
		return new_entry;
	} else {
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
	if( entry ) {
		return entry->val;
	} else {
		return NULL;
	}
}

