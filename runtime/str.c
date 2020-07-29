/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Operations on strings */

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/callback.h"

/* returns a number of bytes (chars) */
CAMLexport mlsize_t caml_string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  CAMLassert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}

/* returns a value that represents a number of bytes (chars) */
CAMLprim value caml_ml_string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  CAMLassert (Byte (s, temp - Byte (s, temp)) == 0);
  return Val_long(temp - Byte (s, temp));
}

CAMLprim value caml_ml_bytes_length(value s)
{
  return caml_ml_string_length(s);
}

CAMLexport int caml_string_is_c_safe (value s)
{
  return strlen(String_val(s)) == caml_string_length(s);
}

/**
 * [caml_create_string] is deprecated,
 * use [caml_create_bytes] instead
 */
CAMLprim value caml_create_string(value len)
{
  mlsize_t size = Long_val(len);
  if (size > Bsize_wsize (Max_wosize) - 1){
    caml_invalid_argument("String.create");
  }
  return caml_alloc_string(size);
}

/* [len] is a value that represents a number of bytes (chars) */
CAMLprim value caml_create_bytes(value len)
{
  mlsize_t size = Long_val(len);
  if (size > Bsize_wsize (Max_wosize) - 1){
    caml_invalid_argument("Bytes.create");
  }
  return caml_alloc_string(size);
}



CAMLprim value caml_string_get(value str, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str)) caml_array_bound_error();
  return Val_int(Byte_u(str, idx));
}

CAMLprim value caml_bytes_get(value str, value index)
{
  return caml_string_get(str, index);
}

CAMLprim value caml_bytes_set(value str, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= caml_string_length(str)) caml_array_bound_error();
  Byte_u(str, idx) = Int_val(newval);
  return Val_unit;
}

/**
 * [caml_string_set] is deprecated,
 * use [caml_bytes_set] instead
 */
CAMLprim value caml_string_set(value str, value index, value newval)
{
  return caml_bytes_set(str,index,newval);
}


CAMLprim value caml_string_get16(value str, value index)
{
  intnat res;
  unsigned char b1, b2;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 1 >= caml_string_length(str)) caml_array_bound_error();
  b1 = Byte_u(str, idx);
  b2 = Byte_u(str, idx + 1);
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 8 | b2;
#else
  res = b2 << 8 | b1;
#endif
  return Val_int(res);
}

CAMLprim value caml_bytes_get16(value str, value index)
{
  return caml_string_get16(str,index);
}

CAMLprim value caml_string_get32(value str, value index)
{
  int32_t res;
  unsigned char b1, b2, b3, b4;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 3 >= caml_string_length(str)) caml_array_bound_error();
  b1 = Byte_u(str, idx);
  b2 = Byte_u(str, idx + 1);
  b3 = Byte_u(str, idx + 2);
  b4 = Byte_u(str, idx + 3);
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 24 | b2 << 16 | b3 << 8 | b4;
#else
  res = b4 << 24 | b3 << 16 | b2 << 8 | b1;
#endif
  return caml_copy_int32(res);
}

CAMLprim value caml_bytes_get32(value str, value index)
{
  return caml_string_get32(str,index);
}

CAMLprim value caml_string_get64(value str, value index)
{
  uint64_t res;
  unsigned char b1, b2, b3, b4, b5, b6, b7, b8;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 7 >= caml_string_length(str)) caml_array_bound_error();
  b1 = Byte_u(str, idx);
  b2 = Byte_u(str, idx + 1);
  b3 = Byte_u(str, idx + 2);
  b4 = Byte_u(str, idx + 3);
  b5 = Byte_u(str, idx + 4);
  b6 = Byte_u(str, idx + 5);
  b7 = Byte_u(str, idx + 6);
  b8 = Byte_u(str, idx + 7);
#ifdef ARCH_BIG_ENDIAN
  res = (uint64_t) b1 << 56 | (uint64_t) b2 << 48
        | (uint64_t) b3 << 40 | (uint64_t) b4 << 32
        | (uint64_t) b5 << 24 | (uint64_t) b6 << 16
        | (uint64_t) b7 << 8 | (uint64_t) b8;
#else
  res = (uint64_t) b8 << 56 | (uint64_t) b7 << 48
        | (uint64_t) b6 << 40 | (uint64_t) b5 << 32
        | (uint64_t) b4 << 24 | (uint64_t) b3 << 16
        | (uint64_t) b2 << 8 | (uint64_t) b1;
#endif
  return caml_copy_int64(res);
}

CAMLprim value caml_bytes_get64(value str, value index)
{
  return caml_string_get64(str,index);
}

CAMLprim value caml_bytes_set16(value str, value index, value newval)
{
  unsigned char b1, b2;
  intnat val;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 1 >= caml_string_length(str)) caml_array_bound_error();
  val = Long_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 8;
  b2 = 0xFF & val;
#else
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  Byte_u(str, idx) = b1;
  Byte_u(str, idx + 1) = b2;
  return Val_unit;
}

CAMLprim value caml_bytes_set32(value str, value index, value newval)
{
  unsigned char b1, b2, b3, b4;
  intnat val;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 3 >= caml_string_length(str)) caml_array_bound_error();
  val = Int32_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 24;
  b2 = 0xFF & val >> 16;
  b3 = 0xFF & val >> 8;
  b4 = 0xFF & val;
#else
  b4 = 0xFF & val >> 24;
  b3 = 0xFF & val >> 16;
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  Byte_u(str, idx) = b1;
  Byte_u(str, idx + 1) = b2;
  Byte_u(str, idx + 2) = b3;
  Byte_u(str, idx + 3) = b4;
  return Val_unit;
}

CAMLprim value caml_bytes_set64(value str, value index, value newval)
{
  unsigned char b1, b2, b3, b4, b5, b6, b7, b8;
  int64_t val;
  intnat idx = Long_val(index);
  if (idx < 0 || idx + 7 >= caml_string_length(str)) caml_array_bound_error();
  val = Int64_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 56;
  b2 = 0xFF & val >> 48;
  b3 = 0xFF & val >> 40;
  b4 = 0xFF & val >> 32;
  b5 = 0xFF & val >> 24;
  b6 = 0xFF & val >> 16;
  b7 = 0xFF & val >> 8;
  b8 = 0xFF & val;
#else
  b8 = 0xFF & val >> 56;
  b7 = 0xFF & val >> 48;
  b6 = 0xFF & val >> 40;
  b5 = 0xFF & val >> 32;
  b4 = 0xFF & val >> 24;
  b3 = 0xFF & val >> 16;
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  Byte_u(str, idx) = b1;
  Byte_u(str, idx + 1) = b2;
  Byte_u(str, idx + 2) = b3;
  Byte_u(str, idx + 3) = b4;
  Byte_u(str, idx + 4) = b5;
  Byte_u(str, idx + 5) = b6;
  Byte_u(str, idx + 6) = b7;
  Byte_u(str, idx + 7) = b8;
  return Val_unit;
}

CAMLprim value caml_string_equal(value s1, value s2)
{
  mlsize_t sz1, sz2;
  value * p1, * p2;

  if (s1 == s2) return Val_true;
  sz1 = Wosize_val(s1);
  sz2 = Wosize_val(s2);
  if (sz1 != sz2) return Val_false;
  for(p1 = Op_val(s1), p2 = Op_val(s2); sz1 > 0; sz1--, p1++, p2++)
    if (*p1 != *p2) return Val_false;
  return Val_true;
}

CAMLprim value caml_bytes_equal(value s1, value s2)
{
  return caml_string_equal(s1,s2);
}

CAMLprim value caml_string_notequal(value s1, value s2)
{
  return Val_not(caml_string_equal(s1, s2));
}

CAMLprim value caml_bytes_notequal(value s1, value s2)
{
  return caml_string_notequal(s1,s2);
}

CAMLprim value caml_string_compare(value s1, value s2)
{
  mlsize_t len1, len2;
  int res;

  if (s1 == s2) return Val_int(0);
  len1 = caml_string_length(s1);
  len2 = caml_string_length(s2);
  res = memcmp(String_val(s1), String_val(s2), len1 <= len2 ? len1 : len2);
  if (res < 0) return Val_int(-1);
  if (res > 0) return Val_int(1);
  if (len1 < len2) return Val_int(-1);
  if (len1 > len2) return Val_int(1);
  return Val_int(0);
}

CAMLprim value caml_bytes_compare(value s1, value s2)
{
  return caml_string_compare(s1,s2);
}

CAMLprim value caml_string_lessthan(value s1, value s2)
{
  return caml_string_compare(s1, s2) < Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_bytes_lessthan(value s1, value s2)
{
  return caml_string_lessthan(s1,s2);
}


CAMLprim value caml_string_lessequal(value s1, value s2)
{
  return caml_string_compare(s1, s2) <= Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_bytes_lessequal(value s1, value s2)
{
  return caml_string_lessequal(s1,s2);
}


CAMLprim value caml_string_greaterthan(value s1, value s2)
{
  return caml_string_compare(s1, s2) > Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_bytes_greaterthan(value s1, value s2)
{
  return caml_string_greaterthan(s1,s2);
}

CAMLprim value caml_string_greaterequal(value s1, value s2)
{
  return caml_string_compare(s1, s2) >= Val_int(0) ? Val_true : Val_false;
}

CAMLprim value caml_bytes_greaterequal(value s1, value s2)
{
  return caml_string_greaterequal(s1,s2);
}

CAMLprim value caml_blit_bytes(value s1, value ofs1, value s2, value ofs2,
                                value n)
{
  memmove(&Byte(s2, Long_val(ofs2)), &Byte(s1, Long_val(ofs1)), Long_val(n));
  return Val_unit;
}

CAMLprim value caml_blit_string(value s1, value ofs1, value s2, value ofs2,
                                value n)
{
  return caml_blit_bytes (s1, ofs1, s2, ofs2, n);
}

CAMLprim value caml_fill_bytes(value s, value offset, value len, value init)
{
  memset(&Byte(s, Long_val(offset)), Int_val(init), Long_val(len));
  return Val_unit;
}

/**
 * [caml_fill_string] is deprecated, use [caml_fill_bytes] instead
 */
CAMLprim value caml_fill_string(value s, value offset, value len, value init)
{
  return caml_fill_bytes (s, offset, len, init);
}

CAMLexport value caml_alloc_sprintf(const char * format, ...)
{
  va_list args;
  char buf[128];
  int n;
  value res;

#if !defined(_WIN32) || defined(_UCRT)
  /* C99-compliant implementation */
  va_start(args, format);
  /* "vsnprintf(dest, sz, format, args)" writes at most "sz" characters
     into "dest", including the terminating '\0'.
     It returns the number of characters of the formatted string,
     excluding the terminating '\0'. */
  n = vsnprintf(buf, sizeof(buf), format, args);
  va_end(args);
  if (n < sizeof(buf)) {
    /* All output characters were written to buf, including the
       terminating '\0'.  Allocate a Caml string with length "n"
       as computed by vsnprintf, and copy the output of vsnprintf into it. */
    res = caml_alloc_initialized_string(n, buf);
  } else {
    /* PR#7568: if the format is in the Caml heap, the following
       caml_alloc_string could move or free the format.  To prevent
       this, take a copy of the format outside the Caml heap. */
    char * saved_format = caml_stat_strdup(format);
    /* Allocate a Caml string with length "n" as computed by vsnprintf. */
    res = caml_alloc_string(n);
    /* Re-do the formatting, outputting directly in the Caml string.
       Note that caml_alloc_string left room for a '\0' at position n,
       so the size passed to vsnprintf is n+1. */
    va_start(args, format);
    vsnprintf((char *)String_val(res), n + 1, saved_format, args);
    va_end(args);
    caml_stat_free(saved_format);
  }
  return res;
#else
  /* Implementation specific to the Microsoft CRT library */
  va_start(args, format);
  /* "_vsnprintf(dest, sz, format, args)" writes at most "sz" characters
     into "dest".  Let "len" be the number of characters of the formatted
     string.
     If "len" < "sz", a null terminator was appended, and "len" is returned.
     If "len" == "sz", no null termination, and "len" is returned.
     If "len" > "sz", a negative value is returned. */
  n = _vsnprintf(buf, sizeof(buf), format, args);
  va_end(args);
  if (n >= 0 && n <= sizeof(buf)) {
    /* All output characters were written to buf.
       "n" is the actual length of the output.
       Allocate a Caml string of length "n" and copy the characters into it. */
    res = caml_alloc_string(n);
    memcpy((char *)String_val(res), buf, n);
  } else {
    /* PR#7568: if the format is in the Caml heap, the following
       caml_alloc_string could move or free the format.  To prevent
       this, take a copy of the format outside the Caml heap. */
    char * saved_format = caml_stat_strdup(format);
    /* Determine actual length of output, excluding final '\0' */
    va_start(args, format);
    n = _vscprintf(format, args);
    va_end(args);
    res = caml_alloc_string(n);
    /* Re-do the formatting, outputting directly in the Caml string.
       Note that caml_alloc_string left room for a '\0' at position n,
       so the size passed to _vsnprintf is n+1. */
    va_start(args, format);
    _vsnprintf((char *)String_val(res), n + 1, saved_format, args);
    va_end(args);
    caml_stat_free(saved_format);
  }
  return res;
#endif
}

CAMLprim value caml_string_of_bytes(value bv)
{
  return bv;
}

CAMLprim value caml_bytes_of_string(value bv)
{
  return bv;
}

CAMLexport mlsize_t caml_rope_length(value r){
  if(Tag_val(r) == String_tag){
    return caml_string_length(r);
  }
  else if (Tag_val(r) == Forward_tag){
    value field1 = Field(r,1);
    if(Is_long(field1)){
      // this is a branch 
      value leftlen, right;
      leftlen = Field(r, 1);
      right = Field(r, 3);
      return Unsigned_long_val(leftlen) + caml_rope_length(right);
    }
    else{
      // this is a sub
      CAMLassert (Tag_val(field1) == Forward_tag);
      return Unsigned_long_val(Field(r,3));
    }
  } 
  else{
    // similar to the case in caml_oldify_rope where children might have been forwarded
    // printf("caml_rope_length:  tag : %d, is_young : %d, is_block : %d, header : %lu\n", 
    // Tag_val(r), Is_young(r), Is_block(r), Hd_val(r));
    CAMLassert(Hd_val(r) == 0);
    return caml_rope_length(Field(r, 0));
  }
}

CAMLprim value caml_ml_rope_length(value r){
    return Val_long(caml_rope_length(r));   
}

static inline mlsize_t min(long x, long y){
  return x < y? x : y;
}

static inline mlsize_t max(long x, long y){
  return x > y? x : y;
}

CAMLprim value caml_rope_to_string_rec(value r, value b, 
  mlsize_t ofs, mlsize_t len, mlsize_t idx)
{
  if (Tag_val(r) == String_tag){
    caml_blit_bytes(r, Val_long(ofs), b, Val_long(idx), Val_long(len));           
  }
  else{
    CAMLassert(Tag_val(r) == Forward_tag);
    value field1 = Field(r, 1);
    if (Is_long(field1)){
      value left, right;
      mlsize_t leftlen;
      mlsize_t actual_left_ofs, actual_left_len, actual_right_ofs, actual_right_len;
      leftlen = Unsigned_long_val(Field(r, 1));
      left = Field(r, 2);
      right = Field(r, 3);

      actual_left_ofs = min(ofs, leftlen);
      actual_left_len = max(min(len, leftlen - ofs), 0);
      actual_right_ofs = max(ofs - leftlen, 0);
      actual_right_len = len - actual_left_len;      

      caml_rope_to_string_rec(left, b, actual_left_ofs, actual_left_len, idx);
      caml_rope_to_string_rec(right, b, actual_right_ofs, actual_right_len, 
        idx + actual_left_len);
    }
    else{
      // this is a sub
      value rp;
      mlsize_t rp_start, rp_len;
      CAMLassert (Tag_val(field1) == Forward_tag);
      rp = Field(r, 1);
      rp_start = Unsigned_long_val(Field(r, 2));
      rp_len = Unsigned_long_val(Field(r, 3));
      
      caml_rope_to_string_rec(rp, b, rp_start + ofs, min(len, rp_len), idx);
    }
  }
  return Val_unit;
}

CAMLprim value caml_rope_to_string(value r)
{
  CAMLparam1(r);
  CAMLlocal2(b, len);
  len = caml_ml_rope_length(r);
  b = caml_create_bytes(len);
  caml_rope_to_string_rec(r, b, 0, len, 0);
  CAMLreturn(b);
}


static value caml_get_rope_promoter(void);

CAMLprim value caml_rope_branch(value leftlen, value left, value right){
  CAMLparam3(leftlen, left, right);
  CAMLlocal1(b);
  b = caml_alloc_small(4, Forward_tag);
  Field(b, 0) = caml_get_rope_promoter();
  Field(b, 1) = leftlen;
  Field(b, 2) = left;
  Field(b, 3) = right;
  CAMLreturn(b);
}

CAMLprim value caml_rope_sub_cons(value rp, value start, value len){
  CAMLparam3(rp, start, len);
  CAMLlocal1(b);
  b = caml_alloc_small(4, Forward_tag);
  Field(b, 0) = caml_get_rope_promoter();
  Field(b, 1) = rp;
  Field(b, 2) = start;
  Field(b, 3) = len;
  CAMLreturn(b);
}

// ofs is the start position a particular node
// not the number of bytes that have been copied
// which was used for the dst pointer but is not used anymore
static void caml_oldify_rope_promotion(
  value v, 
  char *dst, 
  mlsize_t ofs,
  mlsize_t len)
{
  CAMLassert(Is_young(v) && Is_block(v));
  if (Tag_val(v) == String_tag){
    char *s = Bp_val(v);
    strncpy(dst, s + ofs, len);
  }
  else if (Tag_val(v) == Forward_tag){
    value field1 = Field(v, 1);
    if(Is_long(field1))
    {
      // is a branch node
      mlsize_t leftlen = Unsigned_long_val(Field(v, 1));
      value left = Field(v, 2);
      value right = Field(v, 3);

      mlsize_t actual_left_ofs = min(ofs, leftlen);
      mlsize_t actual_left_len = max(min(len, leftlen - ofs), 0);
      mlsize_t actual_right_ofs = max(ofs - leftlen, 0);
      mlsize_t actual_right_len = len - actual_left_len;      


      caml_oldify_rope_promotion(left, dst, actual_left_ofs, actual_left_len);
      caml_oldify_rope_promotion(right, dst + actual_left_len, actual_right_ofs,
        actual_right_len);
    }
    else{
      CAMLassert(Tag_val(field1) == Forward_tag);
      mlsize_t rp_start = Unsigned_long_val(Field(v, 2));
      mlsize_t rp_len = Unsigned_long_val(Field(v, 3));
      // printf("ofs : %lu, rp_start : %lu len : %lu\n", ofs, rp_start, min(len, rp_len));
      caml_oldify_rope_promotion(field1, dst, ofs + rp_start, min(len, rp_len));

    }
  }
  else{ 
    // it might be that the children of a rope have been already forwarded
    // i.e. header is 0
    // the printing statement are just to check this

    // printf("caml_oldify_rope: tag : %d, is_young : %d, is_block : %d, header : %lu\n", 
    // Tag_val(v), Is_young(v), Is_block(v), Hd_val(v));
    // printf("and the forward pointer is pointing to a tag: %d, whose content is %s\n",
    // Tag_val(Field(v,0)), Bp_val(Field(v, 0)) );

    CAMLassert(Hd_val(v) == 0);
    caml_oldify_rope_promotion(Field(v, 0), dst, ofs, len);
  }
}

static value caml_oldify_rope(value v, value *p, header_t hd){
  // not sure if CAMLparam, CAMLreturn is needed
  // but since they are not used in other code in caml_oldify_one
  // it's probably not needed

  value result;
  mlsize_t sz, i;
  mlsize_t len, offset_index;
  printf("hello, I am a rope:)\n");
  
  len = caml_rope_length(v);
  sz = Wsize_bsize(len) + 1; // size in words, +1 for padding
  
  result = caml_alloc_shr_for_minor_gc(sz, String_tag, hd);
  caml_oldify_rope_promotion(v, Bp_val(result), 0, len);

  // need to preserve the padding of the string
  offset_index = Bsize_wsize (sz) - 1;
  for(i = len; i < offset_index; ++i)
    Byte(result, i) = 0;
  Byte (result, offset_index) = offset_index - len;
  return result;
}

static value caml_get_rope_promoter(void){
  value (*fun_ptr)(value, value *, header_t) = &caml_oldify_rope;
  return Val_long(((long) fun_ptr));
}
