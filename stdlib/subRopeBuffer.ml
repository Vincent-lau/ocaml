(* In this implementation each buffer stores a rope rather than bytes/string
decrease implementation complexity*)

type t = SubRope.t ref

(* create n does not make much sense here because it is better
to keep, e.g. small rope optimisation in the rope module 
therefore for a rope buffer, it is always true that b.position = b.length *)
let create _ = ref SubRope.empty

let contents b = SubRope.to_string !b

(* convert the rope to bytes
and sub it *)
let to_bytes b = 
  let s = SubRope.to_string !b in 
  let bytes = Bytes.of_string s in
  Bytes.sub bytes 0 (SubRope.length !b)

let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > (SubRope.length !b) - len
  then invalid_arg "SubRopeBuffer.sub"
  else SubRope.to_string (SubRope.sub !b ofs len)


let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > (SubRope.length !src) - len
             || dstoff < 0 || dstoff > (Bytes.length dst) - len
  then invalid_arg "SubRopeBuffer.blit"
  else
    SubRope.blit !src srcoff dst dstoff len


let nth b ofs =
  if ofs < 0 || ofs >= (SubRope.length !b) then
   invalid_arg "SubRopeBuffer.nth"
  else SubRope.get !b ofs


let length b = SubRope.length !b


let reset b = b := SubRope.empty

(* if rope is used, it makes sense that clear and reset are the same *)
let clear = reset


(* this add_char will use SubRope.(^) to add a char to the buffer/rope *)
let add_char b c =
  let new_r = SubRope.make 1 c in
  b := (SubRope.(^) !b new_r)


let add_utf_8_uchar b u = match Uchar.to_int u with
 | u when u < 0 -> assert false
 | u when u <= 0x007F ->
    add_char b (Char.unsafe_chr u)
 | u when u <= 0x07FF ->
  add_char b
    (Char.unsafe_chr (0xC0 lor (u lsr 6)));
  add_char b
    (Char.unsafe_chr (0x80 lor (u land 0x3F)));
 | u when u <= 0xFFFF ->
    add_char b
      (Char.unsafe_chr (0xE0 lor (u lsr 12)));
    add_char b
      (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
    add_char b
      (Char.unsafe_chr (0x80 lor (u land 0x3F)));
 | u when u <= 0x10FFFF ->
    add_char b
      (Char.unsafe_chr (0xF0 lor (u lsr 18)));
    add_char b
      (Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F)));
    add_char b
      (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
    add_char b
      (Char.unsafe_chr (0x80 lor (u land 0x3F)));
 | _ -> assert false

 let add_utf_16be_uchar b u = match Uchar.to_int u with
 | u when u < 0 -> assert false
 | u when u <= 0xFFFF ->
    add_char b (Char.unsafe_chr (u lsr 8));
    add_char b (Char.unsafe_chr (u land 0xFF));
 | u when u <= 0x10FFFF ->
     let u' = u - 0x10000 in
     let hi = 0xD800 lor (u' lsr 10) in
     let lo = 0xDC00 lor (u' land 0x3FF) in
     add_char b (Char.unsafe_chr (hi lsr 8));
     add_char b (Char.unsafe_chr (hi land 0xFF));
     add_char b (Char.unsafe_chr (lo lsr 8));
     add_char b (Char.unsafe_chr (lo land 0xFF));
 | _ -> assert false

 let add_utf_16le_uchar b u = match Uchar.to_int u with
 | u when u < 0 -> assert false
 | u when u <= 0xFFFF ->
  add_char b (Char.unsafe_chr (u land 0xFF));
  add_char b (Char.unsafe_chr (u lsr 8));
 | u when u <= 0x10FFFF ->
     let u' = u - 0x10000 in
     let hi = 0xD800 lor (u' lsr 10) in
     let lo = 0xDC00 lor (u' land 0x3FF) in
     add_char b (Char.unsafe_chr (hi land 0xFF));
     add_char b (Char.unsafe_chr (hi lsr 8));
     add_char b (Char.unsafe_chr (lo land 0xFF));
     add_char b (Char.unsafe_chr (lo lsr 8));
 | _ -> assert false

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len
  then invalid_arg "SubRopeBuffer.add_substring/add_subbytes";
  let sub_r = SubRope.of_string (String.sub s offset len) in
  b := (SubRope.(^) !b sub_r)


let add_subbytes b s offset len =
  add_substring b (Bytes.unsafe_to_string s) offset len

let add_string b s =
  let r = SubRope.of_string s in
  b := (SubRope.(^) !b r)


let add_bytes b s = add_string b (Bytes.unsafe_to_string s)

let add_buffer b bs =
  add_subbytes b (to_bytes bs) 0 (length bs)

(* I changed the signature of the original function
which was in_channel -> bytes -> int -> int -> int *)
let really_input_up_to ic buf ofs len =
  let rec loop ic buf ~already_read ~ofs ~to_read =
    if to_read = 0 then already_read
    else begin
      let b = Bytes.create to_read in
      let r = input ic b ofs to_read in
      let rope = SubRope.of_string (Bytes.to_string (Bytes.sub b 0 r)) in
      buf := (SubRope.(^) !buf rope);
      if r = 0 then already_read
      else begin
        let already_read = already_read + r in
        let ofs = ofs + r in
        let to_read = to_read - r in
        loop ic buf ~already_read ~ofs ~to_read
      end
    end
  in loop ic buf ~already_read:0 ~ofs ~to_read:len


let unsafe_add_channel_up_to b ic len =
  let buf_len = length b in
  let n = really_input_up_to ic b buf_len len in
  (* The assertion below may fail in weird scenario where
     threaded/finalizer code, run asynchronously during the
     [really_input_up_to] call, races on the buffer; we don't ensure
     correctness in this case, but need to preserve the invariants for
     memory-safety (see discussion of [resize]). *)
  assert (buf_len + n = length b);
  n

let add_channel b ic len =
  if len < 0 || len > Sys.max_string_length then   (* PR#5004 *)
    invalid_arg "SubRopeBuffer.add_channel";
  let n = unsafe_add_channel_up_to b ic len in
  (* It is intentional that a consumer catching End_of_file
     will see the data written (see #6719, #7136). *)
  if n < len then raise End_of_file;
  ()

let output_buffer oc b =
  output oc (to_bytes b) 0 (length b)

let closing = function
  | '(' -> ')'
  | '{' -> '}'
  | _ -> assert false

(* opening and closing: open and close characters, typically ( and )
   k: balance of opening and closing chars
   s: the string where we are searching
   start: the index where we start the search. *)
let advance_to_closing opening closing k s start =
  let rec advance k i lim =
    if i >= lim then raise Not_found else
    if s.[i] = opening then advance (k + 1) (i + 1) lim else
    if s.[i] = closing then
      if k = 0 then i else advance (k - 1) (i + 1) lim
    else advance k (i + 1) lim in
  advance k start (String.length s)

let advance_to_non_alpha s start =
  let rec advance i lim =
    if i >= lim then lim else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> advance (i + 1) lim
    | _ -> i in
  advance start (String.length s)

(* We are just at the beginning of an ident in s, starting at start. *)
let find_ident s start lim =
  if start >= lim then raise Not_found else
  match s.[start] with
  (* Parenthesized ident ? *)
  | '(' | '{' as c ->
     let new_start = start + 1 in
     let stop = advance_to_closing c (closing c) 0 s new_start in
     String.sub s new_start (stop - start - 1), stop + 1
  (* Regular ident *)
  | _ ->
     let stop = advance_to_non_alpha s (start + 1) in
     String.sub s start (stop - start), stop

(* Substitute $ident, $(ident), or ${ident} in s,
    according to the function mapping f. *)
let add_substitute b f s =
  let lim = String.length s in
  let rec subst previous i =
    if i < lim then begin
      match s.[i] with
      | '$' as current when previous = '\\' ->
         add_char b current;
         subst ' ' (i + 1)
      | '$' ->
         let j = i + 1 in
         let ident, next_i = find_ident s j lim in
         add_string b (f ident);
         subst ' ' next_i
      | current when previous == '\\' ->
         add_char b '\\';
         add_char b current;
         subst ' ' (i + 1)
      | '\\' as current ->
         subst current (i + 1)
      | current ->
         add_char b current;
         subst current (i + 1)
    end else
    if previous = '\\' then add_char b previous in
  subst ' ' 0

let truncate b len =
    if len < 0 || len > length b then
      invalid_arg "SubRopeBuffer.truncate"
    else
      b := (SubRope.sub !b 0 len)

(** {1 Iterators} *)

let to_seq b =
  let rec aux i () =
    (* Note that b.position is not a constant and cannot be lifted out of aux *)
    if i >= (length b) then Seq.Nil
    else
      let x = Bytes.unsafe_get (to_bytes b) i in
      Seq.Cons (x, aux (i+1))
  in
  aux 0

let to_seqi b =
  let rec aux i () =
    (* Note that b.position is not a constant and cannot be lifted out of aux *)
    if i >= (length b) then Seq.Nil
    else
      let x = Bytes.unsafe_get (to_bytes b) i in
      Seq.Cons ((i,x), aux (i+1))
  in
  aux 0

let add_seq b seq = Seq.iter (add_char b) seq

let of_seq i =
  let b = create 32 in
  add_seq b i;
  b

(** {6 Binary encoding of integers} *)

external unsafe_set_int8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"
external unsafe_set_int16 : bytes -> int -> int -> unit = "%caml_bytes_set16u"
external unsafe_set_int32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
external unsafe_set_int64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"


let add_int8 b x =
  let byte = Bytes.create 1 in
  unsafe_set_int8 byte 0 x;
  let rope = SubRope.of_string (Bytes.to_string byte) in
  b := SubRope.(^) !b rope

let add_int16_ne b x =
  let byte = Bytes.create 2 in
  unsafe_set_int16 byte 0 x;
  let rope = SubRope.of_string (Bytes.to_string byte) in
  b := SubRope.(^) !b rope

let add_int32_ne b x =
  let byte = Bytes.create 4 in
  unsafe_set_int32 byte 0 x;
  let rope = SubRope.of_string (Bytes.to_string byte) in
  b := SubRope.(^) !b rope


let add_int64_ne b x =
  let byte = Bytes.create 8 in
  unsafe_set_int64 byte 0 x;
  let rope = SubRope.of_string (Bytes.to_string byte) in
  b := SubRope.(^) !b rope


let add_int16_le b x =
  add_int16_ne b (if Sys.big_endian then swap16 x else x)

let add_int16_be b x =
  add_int16_ne b (if Sys.big_endian then x else swap16 x)

let add_int32_le b x =
  add_int32_ne b (if Sys.big_endian then swap32 x else x)

let add_int32_be b x =
  add_int32_ne b (if Sys.big_endian then x else swap32 x)

let add_int64_le b x =
  add_int64_ne b (if Sys.big_endian then swap64 x else x)

let add_int64_be b x =
  add_int64_ne b (if Sys.big_endian then x else swap64 x)

let add_uint8 = add_int8
let add_uint16_ne = add_int16_ne
let add_uint16_le = add_int16_le
let add_uint16_be = add_int16_be
