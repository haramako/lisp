#include "lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned int value_hash( Value v );

int main( int argc, char **argv )
{
	init_prelude(false);

	/*
	printf( "%u\n", value_hash(string_new("hoge")) );
	printf( "%u\n", value_hash(string_new("hoge")) );
	printf( "%u\n", value_hash(string_new("fuga")) );
	printf( "%u\n", value_hash(INT2V(1)));
	printf( "%u\n", value_hash(INT2V(2)));
	printf( "%u\n", value_hash(intern("hoge")));
	printf( "%u\n", value_hash(intern("hoge")));
	printf( "%u\n", value_hash(intern("fuga")));
	*/

	/*
	Dict *d1 = dict_new();
	printf( "a\n" );
	dict_set( d1, string_new("hoge"), INT2V(1) );
	display_val( "d1[hoge] ", dict_get( d1, string_new("hoge") ) );
	dict_set( d1, string_new("hoge"), INT2V(2) );
	display_val( "d1[hoge] ", dict_get( d1, string_new("hoge") ) );

	display_val( "hoge: ", intern("hoge") );
	printf( "%p\n", intern("hoge") );
	printf( "%p\n", intern("hoge") );
	
	dict_set( d1, intern("hoge"), INT2V(1) );
	display_val( "d1[hoge] ", dict_get( d1, intern("hoge") ) );
	dict_set( d1, intern("hoge"), INT2V(2) );
	display_val( "d1[hoge] ", dict_get( d1, intern("hoge") ) );

	dict_set( d1, intern("fuga"), INT2V(3) );
	display_val( "d1[fuga] ", dict_get( d1, intern("fuga") ) );
	display_val( "d1[hoge] ", dict_get( d1, intern("hoge") ) );

	for( int i=0; i<10; i++ ){
		if( i % 5 == 0 ) d1 = dict_rehash(d1);
		dict_set( d1, INT2V(i*2), INT2V(i) );
	}
	for( int i=0; i<10; i++ ){
		display_val( "d1: ", dict_get( d1, INT2V(i) ) );
	}
	*/
	
	return 0;
}

