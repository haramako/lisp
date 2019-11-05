#include "lisp.h"
#include <stdlib.h>
#include <string.h>

//********************************************************
// String
//********************************************************

String* string_new( char *str )
{
	return string_new_len( str, (int)strlen(str) );
}

String* string_new_len( char *str, int len )
{
	StringBody *body = V2STRING_BODY(gc_new(TYPE_STRING_BODY));
	body->buf = malloc( len+1 );
	assert( body->buf);
	memcpy( body->buf, str, len );
	body->buf[len] = '\0';
	body->len = len;
		
	String *s = V2STRING(gc_new(TYPE_STRING));
	s->body = body;
	s->start = 0;
	s->len = len;
	
	return s;
}

size_t string_puts( String *s, char *buf, size_t len )
{
	if( len > s->len ) len = s->len;
	memcpy( buf, s->body->buf + s->start, len );
	buf[len] = '\0';
	return len;
}

size_t string_puts_escape( String *s, char *buf, size_t len )
{
	size_t n = 0;
	char *str = s->body->buf + s->start;
	size_t slen = s->len;
	for( int i=0; i<slen; i++ ){
		char c = str[i];
		if( c < 32 || c >= 127 ){
			char *escaped = NULL;
			switch( c ){
			case '"': escaped = "\\\""; break;
			case '\\': escaped = "\\\\"; break;
			case '\n': escaped = "\\n"; break;
			case '\r': escaped = "\\r"; break;
			case '\f': escaped = "\\f"; break;
			case '\t': escaped = "\\t"; break;
			case '\0': escaped = "\\0"; break;
			}
			if( escaped ){
				memcpy( buf+n, escaped, 2 );
				n += 2;
			}else{
				n += snprintf( buf+n, len-n, "\\x%02x", c );
			}
		}else{
			buf[n++] = c;
		}
		if( n >= len ) break;
	}
	buf[n] = '\0';
	return n;
}

String* string_substr( String *s, int start, int len )
{
	String *new_str = V2STRING(gc_new(TYPE_STRING));
	new_str->body = s->body;
	new_str->start = s->start + start;
	new_str->len = len;
	return new_str;
}

