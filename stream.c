#include "lisp.h"
#include <string.h>

//********************************************************
// Stream
//********************************************************

Stream* stream_new( FILE *fd, bool close, char *filename )
{
	Stream *s = V2STREAM(gc_new( TYPE_STREAM ));
	s->line = 1;
	s->pos = 0;
	s->stream_type = STREAM_TYPE_FILE;
	s->u.file.close = close;
	s->u.file.fd = fd;
	s->u.file.filename = string_new(filename);
	return s;
}

Stream* stream_new_str( String *str )
{
	Stream *s = V2STREAM(gc_new( TYPE_STREAM ));
	s->line = 1;
	s->pos = 0;
	s->stream_type = STREAM_TYPE_STRING;
	s->u.str = str;
	return s;
}

int stream_getc( Stream *s )
{
	int c;
	if( s->stream_type == STREAM_TYPE_FILE ){
		c = fgetc( s->u.file.fd );
	}else{
		char *str = STRING_BUF(s->u.str);
		c = str[s->pos];
	}
	s->pos++;
	if( c == '\n' ) s->line += 1;
	return c;
}

void stream_ungetc( int c, Stream *s )
{
	if( c == '\n' ) s->line -= 1;
	s->pos--;
	if( s->stream_type == STREAM_TYPE_FILE ){
		ungetc( c, s->u.file.fd );
	}
}

Value stream_read_value( Stream *s )
{
	Value val = V_EOF;
	int err = parse( s, &val );
	if( err ){
		printf( "parse error: err=%d\n", err );
		assert(0);
	}
	return val;
}

void stream_write_value( Stream *s, Value v )
{
	char buf[10240];
	size_t len = value_to_str(buf, sizeof(buf), v);
	stream_write( s, buf, len );
}

size_t stream_read( Stream *s, char *buf, size_t len )
{
	size_t read_len;
	if( s->stream_type == STREAM_TYPE_FILE ){
		read_len = fread( buf, len, 1, s->u.file.fd );
	}else{
		read_len = len;
		strncpy( buf, STRING_BUF(s->u.str), len );
	}
	assert( read_len >= 0 );
	return read_len;
}

size_t stream_write( Stream *s, char *buf, size_t len )
{
	size_t write_len;
	if( s->stream_type == STREAM_TYPE_FILE ){
		write_len = fwrite( buf, len, 1, s->u.file.fd );
	}else{
		write_len = len;
		char *str = STRING_BUF(s->u.str);
		memcpy( str+s->pos, buf, len );
		str[s->pos+len] = '\0';
	}
	s->pos += write_len;
	assert( write_len >= 0 );
	return write_len;
}

void stream_close( Stream *s )
{
	if( s->stream_type == STREAM_TYPE_FILE ){
		fclose( s->u.file.fd );
		s->u.file.fd = 0;
	}
}

