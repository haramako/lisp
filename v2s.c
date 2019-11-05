#include "lisp.h"
#include <string.h>
#include <inttypes.h>
#include <linux/limits.h>

//********************************************************
// Value to string
//********************************************************

static size_t _value_to_str( char *buf, int len, Value v )
{
	int n = 0;
	if( n >= len ) return len;

	if( !v ) {
		n += snprintf( buf, len, "#<null>" );
		if( n >= len ) return len;
		return n;
	}

	switch( TYPE_OF(v) ) {
	case TYPE_UNUSED:
		assert(0);
	case TYPE_NIL:
		n += snprintf( buf, len, "()" );
		break;
	case TYPE_BOOL:
		n += snprintf( buf, len, (v == VALUE_T) ? "#t" : "#f" );
		break;
	case TYPE_INT:
		n += snprintf( buf, len, "%" PRId64, V2INT(v) );
		break;
	case TYPE_CHAR:
		{
			int c = V2CHAR(v);
			if( c >= 32 && c <= 126 ) {
				n += snprintf( buf, len, "#\\%c", (char)c );
			} else {
				n += snprintf( buf, len, "#\\x%02x", c );
			}
		}
		break;
	case TYPE_SYMBOL:
		n += string_puts( V2SYMBOL(v)->str, buf, len );
		break;
	case TYPE_STRING:
		buf[n++] = '"';
		if( n >= len ) return len;
		n += string_puts_escape( V2STRING(v), buf + n, len - n );
		if( n >= len ) return len;
		buf[n++] = '"';
		break;
	case TYPE_LAMBDA:
		{
			Lambda *lmd = V2LAMBDA(v);
			if( lmd->name != NULL ) {
				char str[128];
				string_puts_escape( lmd->name->str, str, sizeof(str) - 1 );
				n += snprintf( buf, len, "#<%s:%s>", LAMBDA_TYPE_NAME[lmd->type], str );
			} else {
				n += snprintf( buf, len, "#<%s:%p>", LAMBDA_TYPE_NAME[lmd->type], v );
			}
		}
		break;
	case TYPE_CFUNC:
		{
			CFunc *func = V2CFUNC(v);
			if( func->name ) {
				char str[128];
				string_puts_escape( func->name->str, str, sizeof(str) - 1 );
				n += snprintf( buf, len, "#<cfunc:%s>", str );
			} else {
				n += snprintf( buf, len, "#<cfunc:%p>", func );
			}
		}
		break;
	case TYPE_PAIR:
	case TYPE_PAIR_SOURCE:
		n += snprintf( buf, len, "(" );
		if( n >= len ) return len;
		bool finished = false;
		while( !finished ) {
			switch( TYPE_OF(CDR(v)) ) {
			case TYPE_NIL:
				n += _value_to_str( buf + n, len - n, CAR(v) );
				if( n >= len ) return len;
				finished = true;
				break;
			case TYPE_PAIR:
				n += _value_to_str( buf + n, len - n, CAR(v) );
				if( n >= len ) return len;
				n += snprintf( buf + n, len - n, " " );
				if( n >= len ) return len;
				break;
			default:
				n += _value_to_str( buf + n, len - n, CAR(v) );
				if( n >= len ) return len;
				n += snprintf( buf + n, len - n, " . " );
				if( n >= len ) return len;
				n += _value_to_str( buf + n, len - n, CDR(v) );
				if( n >= len ) return len;
				finished = true;
				break;
			}
			v = CDR(v);
		}
		n += snprintf( buf + n, len - n, ")" );
		break;
	case TYPE_BUNDLE:
		n += snprintf( buf + n, len - n, "#<bundle:%p>", v );
		break;
	case TYPE_CONTINUATION:
		n += snprintf( buf + n, len - n, "#<continuation:%p>", v );
		break;
	case TYPE_SPECIAL:
		n += snprintf( buf + n, len - n, "%%%s", V2SPECIAL(v)->str );
		break;
	case TYPE_STREAM:
		{
			Stream *s = V2STREAM(v);
			if( s->stream_type == STREAM_TYPE_FILE ) {
				char str[PATH_MAX];
				string_puts_escape( s->u.file.filename, str, sizeof(str) - 1 );
				n += snprintf( buf + n, len - n, "#<port:\"%s\">", str );
			} else {
				n += snprintf( buf + n, len - n, "#<port:string>" );
			}
		}
		break;
	case TYPE_ERROR:
		{
			Error *err = V2ERROR(v);
			char str[128];
			string_puts_escape( err->str, str, sizeof(str) - 1 );
			n += snprintf( buf + n, len - n, "#<error:%s>", str );
		}
		break;
	default:
		assert(0);
	}

	if( n >= len ) return len;
	return n;
}

size_t value_to_str( char *buf, int len, Value v )
{
	size_t n = _value_to_str( buf, len, v );
	// snprintf()が長さ以上に書き込むと終端してくれないので、念のため、終端させておく
	if( n >= len ) buf[len - 1] = '\0';
	return n;
}

char* v2s( Value v )
{
	return v2s_limit( v, 10240 );
}

char* v2s_limit( Value v, int limit )
{
	char buf[10240 + 1];
	assert( limit < sizeof(buf) );
	size_t len = value_to_str(buf, limit, v);
	if( len >= (limit - 1) ) strcpy( buf + limit - 4, "..." );
	String *str = string_new(buf);
	return str->body->buf;
}

void vdump( Value v )
{
	printf( "%s\n", v2s(v) );
}

