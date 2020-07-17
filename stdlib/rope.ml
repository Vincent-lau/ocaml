(* external length : string -> int = "%string_length" *)
(* external set : bytes -> int -> char -> unit = "%string_safe_set"
external create : int -> bytes = "caml_create_string"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int ->  bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]
external unsafe_fill : bytes -> int -> int -> char -> unit
                     = "caml_fill_string" [@@noalloc] *)

                     
(* external functions above ignored for the moment *)

module B = Bytes

type t = Leaf of string 
  | Branch of {leftlen : int; left : t; right : t}

type rope = t


let bts = B.unsafe_to_string
let bos = B.unsafe_of_string

let empty = Leaf ""

let rec length = function
  | Leaf s -> String.length s
  | Branch b -> b.leftlen + length b.right

let of_string s = Leaf s

let to_string r = 
  let b = Bytes.create (length r) in
  let rec to_string_rec r b ofs = match r with
    | Leaf str -> Bytes.blit_string str 0 b ofs (String.length str)
    | Branch {leftlen; left; right} -> 
      to_string_rec left b ofs;
      to_string_rec right b (ofs + leftlen)
  in 
  to_string_rec r b 0; b |> bts


let rope_concat r1 r2 = 
  if r1 = empty then r2 
  else if r2 = empty then r1
  else Branch {leftlen = (length r1); left = r1; right = r2}

let (^) = rope_concat

let is_empty r = (length r = 0)

let make n c =
  B.make n c |> bts |> of_string

let init n f =
  B.init n f |> bts |> of_string

  
let copy r =
  let s = to_string r in
  B.copy (bos s) |> bts |> of_string 
(* deprecated, so just pure conversion *)


let sub r ofs len = 
  if ofs < 0 || len < 0 || ofs + len > (length r) then 
    invalid_arg "Rope.sub"
  else if len = 0 then
    empty
  else
    let rec sub_rec r ofs len = match r with
        | Leaf str -> 
          let actual_len = min len (String.length str - ofs) in 
          Leaf (String.sub str ofs actual_len)
        | Branch {leftlen; left; right} ->
          let left_sub = 
            if ofs = 0 && ofs + len = leftlen then
              left
            else if ofs >= leftlen then
              empty
            else
              sub_rec left ofs len
          in let left_sub_len = length left_sub
          in let right_sub = 
            if ofs + len < leftlen then
              empty
            else
              sub_rec right (max (ofs - leftlen) 0) (len - left_sub_len)
          in left_sub ^ right_sub
    in sub_rec r ofs len

let get r i = 
  String.get (to_string (sub r i 1)) 0

(* deprecated *)
let fill = 
  B.fill
  
(* pure conversion might not be efficient *)  
let blit r = 
  to_string r |> B.blit_string


(* let rec sum_lengths acc seplen = function
  | [] -> acc
  | hd :: [] -> length hd + acc
  | hd :: tl -> sum_lengths (ensure_ge (length hd + seplen + acc) acc) seplen tl *)

(* unsafe, so ignored, not listed in string interface anyway *)
(* let rec unsafe_blits dst pos sep seplen = function 
    [] -> dst
  | hd :: [] ->
    unsafe_blit hd 0 dst pos (length hd); dst
  | hd :: tl ->
    unsafe_blit hd 0 dst pos (length hd);
    unsafe_blit sep 0 dst (pos + length hd) seplen;
    unsafe_blits dst (pos + length hd + seplen) sep seplen tl *)

    
let rec concat sep = function
    [] -> empty
  | hd :: [] -> hd
  | hd :: tl -> (hd ^ sep) ^ (concat sep tl)
  
let rec iter f = function
  | Leaf s -> String.iter f s
  | Branch b -> 
    iter f b.left;
    iter f b.right

let rec iteri_rec f ofs r = 
  let f_ofs o i c = f (i + o) c in
  match r with
  | Leaf s -> String.iteri (f_ofs ofs) s
  | Branch b -> 
    iteri_rec f ofs b.left;
    iteri_rec f (ofs + b.leftlen) b.right

    
let iteri f r = iteri_rec f 0 r

let rec map f = function
     | Leaf str -> Leaf (String.map f str)
     | Branch b ->
      let left = map f b.left and right = map f b.right 
      in left ^ right


let rec mapi_rec f ofs r = 
  let f_ofs o i c = f (i + o) c in
  match r with
  | Leaf str -> Leaf (String.mapi (f_ofs ofs) str)
  | Branch b ->
  let left = mapi_rec f ofs b.left 
  and right = mapi_rec f (ofs + b.leftlen) b.right in
  Branch {leftlen = b.leftlen; left; right}
  

let mapi f r = mapi_rec f 0 r

let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let rec trim_left = function
  | Leaf str -> 
    let i = ref 0 and len = String.length str in 
    while !i < len && is_space (String.unsafe_get str !i) do incr i done;
    if !i = len then empty else Leaf (String.sub str !i (len - !i))
  | Branch b ->
    let l_trimmed = trim_left b.left in
    if is_empty l_trimmed then
      trim_left b.right
    else
      l_trimmed ^ b.right
      
let rec trim_right = function
    | Leaf str ->
      let len = String.length str in 
      let i = ref (len - 1) in
      while !i >= 0 && is_space (String.unsafe_get str !i) do decr i done;
      if !i < 0 then empty else Leaf (String.sub str 0 (!i + 1))
    | Branch b ->
      let r_trimmed = trim_right b.right in
      if is_empty r_trimmed then 
        trim_right b.left
      else
        b.left ^ r_trimmed
      
let trim r = trim_left (trim_right r)

let rec escaped = function
  | Leaf str -> Leaf (String.escaped str)
  | Branch b ->
    let left = escaped b.left and 
    right = escaped b.right in 
    left ^ right
  

(* ofs is the length of the string we have already discarded,
i is the from-index *)
let rec index_from_rec r ofs i c = match r with 
| Leaf str -> (String.index_from str i c) + ofs
| Branch {leftlen; left; right} ->
  if i >= leftlen then
    index_from_rec right (ofs + leftlen) (i - leftlen) c 
  else
    try 
      index_from_rec left ofs i c
    with  
      Not_found -> index_from_rec right (ofs + leftlen) 0 c
    

let index_rec r i c = index_from_rec r 0 i c

let index r c = index_rec r 0 c

let index_opt r c = 
    try Some (index r c)
    with Not_found -> None

let index_from r i c =  
  if i < 0 || i > (length r) then
    invalid_arg "Rope.index_from_opt"
  else
    index_from_rec r 0 i c


let index_from_opt r i c =
  if i < 0 || i > (length r) then
    invalid_arg "Rope.index_from_opt"
  else
    try 
      Some (index_from r i c)
    with
      Not_found -> None


let rec rindex_from_rec r ofs i c = match r with 
  | Leaf str -> (String.rindex_from str i c) + ofs
  | Branch {leftlen; left; right} ->
    if i < leftlen then
      rindex_from_rec left ofs i c
    else
      try
        rindex_from_rec right (leftlen + ofs) (i - leftlen) c 
      with 
        Not_found -> rindex_from_rec left ofs (leftlen - 1) c


let rindex_from r i c =
  if i < -1 || i >= length r then
    invalid_arg "Rope.rindex_from"
  else
    rindex_from_rec r 0 i c

let rindex r c = rindex_from r (length r - 1) c


let rindex_from_opt r i c =
  if i < -1 || i >= length r then
    invalid_arg "Rope.rindex_from_opt"
  else
    try Some (rindex_from r i c)
    with Not_found -> None


let rindex_opt r c = rindex_from_opt r (length r - 1) c


let contains_from r i c =
  if i < 0 || i > length r  then
    invalid_arg "Rope.contains_from"
  else
    try ignore (index_from r i c); true 
    with Not_found -> false

let contains r c = contains_from r 0 c


let rcontains_from r i c =
  if i < 0 || i >= length r then
    invalid_arg "Rope.rcontains_from"
  else
    try ignore (rindex_from r i c); true with Not_found -> false

let uppercase_ascii r = map Char.uppercase_ascii r

let lowercase_ascii r = map Char.lowercase_ascii r

let rec capitalize_ascii = function
  | Leaf str -> Leaf (String.capitalize_ascii str)
  | Branch {leftlen; left; right} ->
    if leftlen <> 0 then
      (capitalize_ascii left) ^ right
    else
      capitalize_ascii right

let rec uncapitalize_ascii = function
  | Leaf str -> Leaf (String.uncapitalize_ascii str)
  | Branch {leftlen; left; right} ->
    if leftlen <> 0 then
      (uncapitalize_ascii left) ^ right
    else
      uncapitalize_ascii right
    

exception Out_of_bounds of string;;

module Iterator = struct
  type t = {
    rope : rope;
    rope_len : int; (* length of the rope globally *)
    mutable len : int; (* length of current cache*)
    mutable i : int; (* index of the local string cache *)
    mutable g_index : int; (* global index of the iterator *)
    mutable current : string; (* local cache *)
  }

  let rec find_local_index r i = 
    match r with
    | Leaf str -> (i, str)
    | Branch {leftlen; left; right} ->
      if i < leftlen then
        find_local_index left i
      else
        find_local_index right (i - leftlen)

  let norm iter = 
    if iter.g_index < 0 || iter.g_index >= iter.rope_len then 
      raise (Out_of_bounds "Rope.Iterator.norm")
    else
      if iter.i  >= iter.len || iter.i < 0 then 
        let (i, current) = find_local_index iter.rope iter.g_index in
        begin
          iter.len <- String.length current;
          iter.i <- i;
          iter.current <- current
        end


  let make r i0 = 
    if i0 < 0 || i0 > length r then 
      raise (Out_of_bounds "Rope.Iterator.make")
    else
      let (i, current) = find_local_index r i0 in
      {
        rope = r;
        rope_len = length r;
        len = String.length current;
        i;
        g_index = i0;
        current;
      }


  let get iter =
    norm iter;
    iter.current.[iter.i]


  let peek iter i = 
    let left_end = iter.g_index - iter.i in
    if left_end <= i && i < left_end + iter.len then
      iter.current.[i - left_end]        
    else
      String.get (to_string (sub iter.rope i 1)) 0


  let pos iter = iter.g_index


  let incr iter = 
    iter.g_index <- iter.g_index + 1;
    iter.i <- iter.i + 1



  let decr iter = 
    iter.g_index <- iter.g_index - 1;
    iter.i <- iter.i - 1


  let goto iter i = 
    let dis = i - iter.g_index in 
    if iter.i + dis >=0 && iter.i + dis < iter.len then
      iter.i <- iter.i + dis;
    iter.g_index <- i   

  let move iter i = 
    iter.g_index <- iter.g_index + i;
    iter.i <- iter.i + i

  let rope iter = iter.rope

end

(* this compare function also comes from 
https://github.com/Chris00/ocaml-rope/tree/master/src *)
exception Less 
exception Greater
let compare (x: t) (y: t) = 
  let len_x = length x and len_y = length y in
  let it_x = Iterator.make x 0 and it_y = Iterator.make y 0 in 
  try
    for _i = 1 to min len_x len_y do 
      let cx = Iterator.get it_x 
      and cy = Iterator.get it_y in 
      if cx > cy then raise Greater;
      if cx < cy then raise Less;
      Iterator.incr it_x;
      Iterator.incr it_y;
    done;
    Stdlib.compare len_x len_y
  with
    | Less -> -1
    | Greater -> 1


let equal x y = (compare x y) = 0

let rec split_on_char sep = function
    | Leaf str -> List.map of_string (String.split_on_char sep str)
    | Branch b ->
      (split_on_char sep b.left) @ (split_on_char sep b.right)
    

(* Deprecated functions implemented via other deprecated functions *)
[@@@ocaml.warning "-3"]
let uppercase = uppercase_ascii

let lowercase = lowercase_ascii

let capitalize = capitalize_ascii 

let uncapitalize = uncapitalize_ascii

(** {1 Iterators} *)

(* lazy data structure, will implement later *)

(* let to_seq s = bos s |> B.to_seq

let to_seqi s = bos s |> B.to_seqi

let of_seq g = B.of_seq g |> bts *)

