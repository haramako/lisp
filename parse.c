#include "lisp.h"
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

//********************************************************
// Parsing
//********************************************************

static void _skip_space( Stream *s )
{
	for(;;){
		int c = stream_getc(s);
		switch( c ){
		case '\n': case '\r': case '\x0c': case ' ': case '\t': 
			break;
		case ';':
			for(;;){
				c = stream_getc(s);
				if( c == '\n' || c == '\r' || c == '\x0c' || c == '\0' || c == -1 ) break;
			}
			break;
		default:
			stream_ungetc(c, s);
			return;
		}
	}
}

static int _parse_list( Stream *s, Value *result )
{
	_skip_space(s);
	int c, err;
	Value val, cdr;
	switch( c = stream_getc(s) ){
	case -1:
	case ')':
	case ']':
	case '\0':
		stream_ungetc(c,s);
		*result = NIL;
		return 0;
	default:
		stream_ungetc(c,s);
		err = parse( s, &val );
		if( err ) return err;

		if( val == V(SYM_DOT) ){
			err = parse( s, &cdr );
			if( err ) return err;
			*result = cdr;
			_skip_space(s);
			return 0;
		}else{
			err = _parse_list( s, &cdr );
			if( err ) return err;
			*result = cons( val, cdr );
			return 0;
		}
	}
}

static inline bool _is_val_char( int c )
{
	switch( c ){
	case ' ': case '\t': case '\n': case '\r': case '\x0c': case '(': case ')': case '[': case ']': case '\0': case -1:
		return false;
	default:
		return true;
	}
}

static int _read_token( Stream *s, char *buf )
{
	int c;
	int i = 0;
	for(;;){
		c = stream_getc(s);
		if( !_is_val_char(c) ) break;
		buf[i++] = c;
	}
	buf[i] = '\0';
	stream_ungetc( c, s );
	return 0;
}

#define isnumber isdigit

static Value _parse_token( char *str )
{
	if( isnumber(str[0]) || (str[0] == '-' && isnumber(str[1])) ){
		for( char *s = str+1; *s != '\0'; s++ ){
			if( !isnumber(*s) ){
				return V(intern(str));
			}
		}
		return INT2V(atoi(str));
	}else{
		return V(intern( str ));
	}
}

static int _unescape_char( Stream *s )
{
	int c = stream_getc(s);
	char buf[9];
	switch( c ){
	case '"': return '"';
	case '\\': return '\\';
	case 'n': return '\n';
	case 'r': return '\r';
	case 'f': return '\f';
	case 't': return '\t';
	case '0': return '\0';
	case 'x':
		stream_read(s,buf,2);
		buf[2] = '\0';
		sscanf( buf, "%x", &c );
		return c;
	case 'u':
		stream_read(s,buf,4);
		buf[4] = '\0';
		sscanf( buf, "%x", &c );
		return c;
	case 'U':
		stream_read(s,buf,8);
		buf[8] = '\0';
		sscanf( buf, "%x", &c );
		return c;
	case ' ': case '\n': case '\r':
		{
			bool feeded = false;
			for(;;){
				if( c == '\n' ){
					if( feeded ) assert(0);
					feeded = true;
				}else if( c == ' ' || c == '\f' || c == '\r' || c == '\t' ){
					// skip
				}else{
					if( !feeded ) assert(0);
					stream_ungetc(c,s);
					break;
				}
				c = stream_getc(s);
			}
			return '\n';
		}
	default:
		printf( "unkonwn escaped string \\%c\n", c );
		assert(0);
	}
}

static Value _read_string( Stream *s )
{
	char buf[1024];
	int i = 0;
	for(;;){
		int c = stream_getc(s);
		switch( c ){
		case '"':
			return (Value)string_new_len(buf,i);
		case '\\':
			c = _unescape_char(s);
			break;
		case '\n': case '\r':
			assert(0);
		case '\0':
			assert(0);
		}
		buf[i++] = c;
	}
}

int parse( Stream *s, Value *result )
{
	int err;
	_skip_space( s );
	int c, c2;
	int level;
	switch( c = stream_getc(s) ){
	case -1:
	case '\0':
		return 0;
	case '(':
	case '[':
		err = _parse_list( s, result );
		if( err ) return err;
		c2 = stream_getc(s);
		if( c == '(' && c2 != ')' ) return -2;
		if( c == '[' && c2 != ']' ) return -2;
		return 0;
	case ')':
	case ']':
		assert(!"paren not matched");
	case '#':
		switch( c = stream_getc(s) ){
		case 't':
			*result = VALUE_T;
			return 0;
		case 'f':
			*result = VALUE_F;
			return 0;
		case 'p':
			// for debug pringint
			err = parse(s,result);
			if( err ) return err;
			*result = cons3( V(intern("*tee*")), *result, NIL );
			return 0;
		case '|':
			// multi-line comment
			level = 1;
			for(;;){
				switch( c = stream_getc(s) ){
				case '|':
					if( stream_getc(s) == '#' ){
						level--;
						if( level <= 0 ) return parse( s, result );
					}
				case '#':
					if( stream_getc(s) == '|' ) level++;
					break;
				case -1:
				case '\0':
					return -4;
				default:
					break;
				}
			}
			return 0;
		case '(':
		case '[':
			// vector literal
			// とりあえず、リストにする
			err = _parse_list( s, result );
			if( err ) return err;
			c2 = stream_getc(s);
			if( c == '(' && c2 != ')' ) return -2;
			if( c == '[' && c2 != ']' ) return -2;
			return 0;
		case '!':
			// shebang
			while( (c = stream_getc(s)) ){
				if( c == '\n' ) break;
			}
			return parse(s, result);
		case ';':
			// s-exp comment
			err = parse(s, result);
			if( err ) return err;
			return parse(s, result);
		case 'x':
			{
				char buf[32];
				int num;
				_read_token(s,buf);
				sscanf( buf, "%x", &num);
				*result = INT2V(num);
				return 0;
			}
			
		case '\\':
			{
				char buf[32];
				_read_token(s,buf);
				if( strlen(buf) == 1 ){
					*result = CHAR2V(buf[0]);
				}else if( buf[0] == 'x' || buf[0] == 'u' ){
					int num;
					sscanf( buf+1, "%x", &num);
					*result = CHAR2V(num);
				}else if( strcmp(buf,"space") == 0 ){
					*result = CHAR2V(' ');
				}else if( strcmp(buf,"newline") == 0 || strcmp(buf,"nl") == 0 || strcmp(buf,"lf") == 0 ){
					*result = CHAR2V('\n');
				}else if( strcmp(buf,"return") == 0 || strcmp(buf,"cr") == 0 ){
					*result = CHAR2V('\r');
				}else if( strcmp(buf,"tab") == 0 || strcmp(buf,"ht") == 0 ){
					*result = CHAR2V('\t');
				}else if( strcmp(buf,"page") == 0 ){
					*result = CHAR2V(0x0c);
				}else if( strcmp(buf,"escape") == 0 || strcmp(buf,"esc") == 0 ){
					*result = CHAR2V(0x1b);
				}else if( strcmp(buf,"delete") == 0 || strcmp(buf,"del") == 0 ){
					*result = CHAR2V(0x7f);
				}else if( strcmp(buf,"null") == 0 ){
					*result = CHAR2V('\0');
				}else{
					printf( "invalid #\\%s\n", buf );
					assert(0);
				}
				return 0;
			}
		default:
			printf( "invalid #char %c\n", c );
			assert(0);
		}
	case '"':
		*result = _read_string(s);
		if( !*result ) return -4;
		return 0;
	case '\'':
		err = parse( s, result );
		if( err ) return err;
		*result = cons( V_QUOTE, cons(*result,NIL) );
		return err;
	case '`':
		err = parse( s, result );
		if( err ) return err;
		*result = cons( V(SYM_QUASIQUOTE), cons(*result,NIL) );
		return err;
	case ',':
		{
			Value sym = V(SYM_UNQUOTE);
			if( (c = stream_getc(s)) == '@' ){
				sym = V(SYM_UNQUOTE_SPLICING);
			}else{
				stream_ungetc(c,s);
			}
			
			err = parse( s, result );
			if( err ) return err;
			*result = cons( sym, cons(*result,NIL) );
			return err;
		}
	default:
		{
			stream_ungetc( c, s );
			char buf[1024];
			_read_token( s, buf );
			*result = _parse_token( buf );
			return 0;
		}
	}
	assert(0);
}

