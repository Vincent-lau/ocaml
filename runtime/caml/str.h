#define CAML_INTERNALS

/* Operations on strings */

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"
#include "misc.h"

/* returns a number of bytes (chars) */
CAMLexport mlsize_t caml_string_length(value s);

/* returns a value that represents a number of bytes (chars) */
CAMLprim value caml_ml_string_length(value s);

CAMLprim value caml_ml_bytes_length(value s);

CAMLexport int caml_string_is_c_safe (value s);

/**
 * [caml_create_string] is deprecated,
 * use [caml_create_bytes] instead
 */
CAMLprim value caml_create_string(value len);

/* [len] is a value that represents a number of bytes (chars) */
CAMLprim value caml_create_bytes(value len);



CAMLprim value caml_string_get(value str, value index);

CAMLprim value caml_bytes_get(value str, value index);

CAMLprim value caml_bytes_set(value str, value index, value newval);

/**
 * [caml_string_set] is deprecated,
 * use [caml_bytes_set] instead
 */
CAMLprim value caml_string_set(value str, value index, value newval);


CAMLprim value caml_string_get16(value str, value index);

CAMLprim value caml_bytes_get16(value str, value index);

CAMLprim value caml_string_get32(value str, value index);

CAMLprim value caml_bytes_get32(value str, value index);

CAMLprim value caml_string_get64(value str, value index);

CAMLprim value caml_bytes_get64(value str, value index);

CAMLprim value caml_bytes_set16(value str, value index, value newval);

CAMLprim value caml_bytes_set32(value str, value index, value newval);


CAMLprim value caml_bytes_set64(value str, value index, value newval);

CAMLprim value caml_string_equal(value s1, value s2);

CAMLprim value caml_bytes_equal(value s1, value s2);


CAMLprim value caml_string_notequal(value s1, value s2);


CAMLprim value caml_bytes_notequal(value s1, value s2);

CAMLprim value caml_string_compare(value s1, value s2);

CAMLprim value caml_bytes_compare(value s1, value s2);

CAMLprim value caml_string_lessthan(value s1, value s2);

CAMLprim value caml_bytes_lessthan(value s1, value s2);


CAMLprim value caml_string_lessequal(value s1, value s2);

CAMLprim value caml_bytes_lessequal(value s1, value s2);


CAMLprim value caml_string_greaterthan(value s1, value s2);

CAMLprim value caml_bytes_greaterthan(value s1, value s2);
CAMLprim value caml_string_greaterequal(value s1, value s2);

CAMLprim value caml_bytes_greaterequal(value s1, value s2);

CAMLprim value caml_blit_bytes(value s1, value ofs1, value s2, value ofs2,
                                value n);

CAMLprim value caml_blit_string(value s1, value ofs1, value s2, value ofs2,
                                value n);

CAMLprim value caml_fill_bytes(value s, value offset, value len, value init);

/**
 * [caml_fill_string] is deprecated, use [caml_fill_bytes] instead
 */
CAMLprim value caml_fill_string(value s, value offset, value len, value init);
CAMLexport value caml_alloc_sprintf(const char * format, ...);

CAMLprim value caml_string_of_bytes(value bv);
CAMLprim value caml_bytes_of_string(value bv);

CAMLprim mlsize_t caml_rope_length(value r);

CAMLprim value caml_ml_rope_length(value r);

CAMLprim value caml_rope_to_string_rec(value r, value b, value ofs);
CAMLprim value caml_rope_to_string(value r);

CAMLprim value caml_rope_branch(value leftlen, value left, value right);