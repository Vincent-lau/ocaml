module B = Bytes


let bts = B.unsafe_to_string
let bos = B.unsafe_of_string


type t 
type compactRope = t

let leaf (s : string) : t = Obj.obj (Obj.repr s)

external branch : int -> t -> t -> t = "caml_rope_branch"

(* the constructor sub, to avoid the same name as sub *)
external sub_cons : t -> int -> int -> t = "caml_rope_sub_cons" 


let destruct (r: t)
    ~(lf: string -> 'a) 
    ~(br: leftlen:int -> left:t -> right:t -> 'a)
    ~(sub: rp:t -> rp_start:int -> rp_len:int -> 'a) =
  let rt_r = Obj.repr r in
  if Obj.tag rt_r = Obj.string_tag then 
    lf (Obj.obj rt_r) 
  else(
    assert (Obj.tag rt_r = Obj.forward_tag);
    let field1_tag = Obj.tag (Obj.repr (Obj.field rt_r 1)) in
    if field1_tag = Obj.int_tag then
        br ~leftlen:(Obj.obj (Obj.repr (Obj.field rt_r 1)) : int)
          ~left:(Obj.obj (Obj.repr (Obj.field rt_r 2)) : t)
          ~right:(Obj.obj (Obj.repr (Obj.field rt_r 3)) : t)
    else(
        assert (field1_tag = Obj.forward_tag);
        sub ~rp:(Obj.obj (Obj.repr (Obj.field rt_r 1)) : t)
          ~rp_start:(Obj.obj (Obj.repr (Obj.field rt_r 2)) : int)
          ~rp_len:(Obj.obj (Obj.repr (Obj.field rt_r 3)) : int)
    )
  )

external length : t -> int = "caml_ml_rope_length"

external to_string: t -> string = "caml_rope_to_string"

let empty = leaf ""

let is_empty r = (length r = 0)


let of_string s = leaf s


(* let to_string r = 
  let b = Bytes.create (length r) in
  let rec to_string_rec r b ofs = 
    destruct r 
      ~lf: (fun str -> Bytes.blit_string str 0 b ofs (String.length str))
      ~br: (fun ~leftlen ~left ~right ->
        to_string_rec left b ofs;
        to_string_rec right b (ofs + leftlen))
  in 
  to_string_rec r b 0; b |> bts *)


let rope_concat r1 r2 = 
  if is_empty r1 then r2 
  else if is_empty r2 then r1
  else branch (length r1) r1 r2

let (^) = rope_concat


let make n c =
  B.make n c |> bts |> of_string

let init n f =
  B.init n f |> bts |> of_string

let copy r =
  let s = to_string r in
  B.copy (bos s) |> bts |> of_string 
(* deprecated, so just pure conversion *)


let sub_copy r ofs len = 
  if ofs < 0 || len < 0 || ofs + len > (length r) then 
    invalid_arg "compactRope.sub_copy"
  else if len = 0 then
    empty
  else
    let rec sub_rec r ofs len = 
        destruct r
          ~lf: (fun str -> 
              let actual_len = min len (String.length str - ofs) in 
              leaf (String.sub str ofs actual_len))
          ~br: (fun ~leftlen ~left ~right ->
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
            in left_sub ^ right_sub)
          ~sub: (fun ~rp ~rp_start ~rp_len -> sub_rec rp rp_start rp_len)
    in sub_rec r ofs len

(* need to register this sub_copy to be used in C
or could write this in C directly *)
let _ = Callback.register "compactRope sub_copy" sub_copy

let sub r ofs len = 
  if ofs < 0 || len < 0 || ofs + len > (length r) then 
    invalid_arg "compactRope.sub"
  else if len = 0 then
    empty
  else
    sub_cons r ofs len
       

let get r i = 
  String.get (to_string (sub r i 1)) 0


(* deprecated *)
let fill = 
  B.fill

(* pure conversion should make sense because 
it does not make much sese to blit a rope?
or maybe could use sub instead to avoid converting
the whole rope *)  
let blit r = 
  to_string r |> B.blit_string


let rec concat sep = function
    [] -> empty
  | hd :: [] -> hd
  | hd :: tl -> (hd ^ sep) ^ (concat sep tl)
  
let rec fold
  ~(lf:int -> int -> string -> 'a) 
  ~(br:'a ->'a -> 'a)
  ofs len r = destruct r
    ~lf: (fun str -> lf ofs len str)
    ~br: (fun ~leftlen ~left ~right ->
      let actual_left_ofs = min ofs leftlen
      and actual_left_len = max (min len (leftlen - ofs)) 0 in
      let actual_right_ofs = max (ofs - leftlen) 0
      and actual_right_len = len - actual_left_len in
      let left_sub = 
        fold ~lf ~br actual_left_ofs actual_left_len left
      and right_sub = 
        fold ~lf ~br actual_right_ofs actual_right_len right
      in br left_sub right_sub)
    ~sub: (fun ~rp ~rp_start ~rp_len -> 
      fold ~lf ~br (rp_start + ofs) (min len rp_len) rp)


let rec foldi
  ~(lf:int -> int -> int -> string -> 'a)
  ~(br:'a ->'a -> 'a)
  ofs len idx r = destruct r
    ~lf: (fun str -> lf ofs len idx str)
    ~br: (fun ~leftlen ~left ~right ->
      let actual_left_ofs = min ofs leftlen
      and actual_left_len = max (min len (leftlen - ofs)) 0 in
      let actual_right_ofs = max (ofs - leftlen) 0
      and actual_right_len = len - actual_left_len in
      let left_sub = 
        foldi ~lf ~br actual_left_ofs actual_left_len idx left 
      and right_sub = foldi ~lf ~br actual_right_ofs actual_right_len
        (idx + actual_left_len) right
      in br left_sub right_sub)
    ~sub: (fun ~rp ~rp_start ~rp_len -> 
      foldi ~lf ~br (rp_start + ofs) (min len rp_len) idx rp)

let rec fold_left 
  ~(lf:int -> int -> string -> 'a)
  ~(br:'a -> 'a -> 'a)
  ~(p: 'a -> bool)
  ofs len r = destruct r
    ~lf: (fun str -> lf ofs len str)
    ~br: (fun ~leftlen ~left ~right ->
      let actual_left_ofs = min ofs leftlen
      and actual_left_len = max (min len (leftlen - ofs)) 0 in
      let actual_right_ofs = max (ofs - leftlen) 0
      and actual_right_len = len - actual_left_len in
      let left_sub = 
        fold_left ~lf ~br ~p actual_left_ofs actual_left_len left
      in 
      if p left_sub then
        fold_left ~lf ~br ~p actual_right_ofs actual_right_len right
      else
        br left_sub (sub right actual_right_ofs actual_right_len))
    ~sub:(fun ~rp ~rp_start ~rp_len-> 
      fold_left ~lf ~br ~p (rp_start + ofs) (min len rp_len) rp)
    
let rec fold_right 
  ~(lf:int -> int -> string -> 'a)
  ~(br:'a -> 'a -> 'a)
  ~(p: 'a -> bool)
  ofs len r = destruct r
    ~lf: (fun str -> lf ofs len str)
    ~br: (fun ~leftlen ~left ~right ->
      let actual_left_ofs = min ofs leftlen
      and actual_left_len = max (min len (leftlen - ofs)) 0 in
      let actual_right_ofs = max (ofs - leftlen) 0
      and actual_right_len = len - actual_left_len in
      let right_sub = 
        fold_right ~lf ~br ~p actual_right_ofs actual_right_len right
      in 
      if p right_sub then
        fold_right ~lf ~br ~p actual_left_ofs actual_left_len left
      else
        br (sub left actual_left_ofs actual_left_len) right_sub)
    ~sub: (fun ~rp ~rp_start ~rp_len -> 
      fold_left ~lf ~br ~p (rp_start + ofs) (min len rp_len) rp)    

let iter f r = 
  let lf ofs len str = String.iter f (String.sub str ofs len)
  and br () () = () in
  fold ~lf:lf ~br:br 0 (length r) r


let iteri_rec f idx r = 
  let f_idx o i c = f (i + o) c in
  let lf ofs len idx str = String.iteri (f_idx idx) (String.sub str ofs len)
  and br () () = () in
  foldi ~lf ~br 0 (length r) idx r

    
let iteri f r = iteri_rec f 0 r

let map f r =
  let lf ofs len str = 
    of_string (String.init len (fun i -> f (String.unsafe_get str (ofs + i)))) in
  fold ~lf ~br:(^) 0 (length r) r

let mapi_rec f idx r = 
  let f_idx o i c = f (i + o) c in
  let lf ofs len idx str = 
    of_string (String.init len (fun i -> (f_idx idx i) (String.unsafe_get str (ofs + i))))
  in
  foldi ~lf ~br:(^) 0 (length r) idx r

let mapi f r = mapi_rec f 0 r

let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let trim_left r = 
  let lf ofs len str = 
    let i = ref 0 in
    while !i < len && is_space (String.unsafe_get str (!i + ofs)) do incr i done;
    if !i = len then 
      empty 
    else
      of_string (String.sub str (!i + ofs) (len - !i))
  in fold_left ~lf ~br:(^) ~p:is_empty 0 (length r) r

    
let trim_right r = 
  let lf ofs len str = 
    let i = ref (len - 1) in
    while !i >= 0 && is_space (String.unsafe_get str (!i + ofs)) do decr i done;
    if !i < 0 then 
      empty 
    else 
      of_string (String.sub str ofs (!i + 1))
  in fold_right ~lf ~br:(^) ~p:is_empty 0 (length r) r
      
let trim r = trim_left (trim_right r)

let escaped r= 
  let lf len ofs str = 
    of_string (String.escaped (String.sub str len ofs))
  in fold ~lf ~br:(^) 0 (length r) r
  


let rec index_from_helper
  ~(lf:int -> int -> int -> int -> string -> 'a)
  ofs len idx fi r = destruct r
    ~lf: (fun str -> lf ofs len idx fi str)
    ~br: (fun ~leftlen ~left ~right ->
      let actual_left_ofs = min ofs leftlen
      and actual_left_len = max (min len (leftlen - ofs)) 0 in
      let actual_right_ofs = max (ofs - leftlen) 0
      and actual_right_len = len - actual_left_len in
      let actual_left_fi = min fi actual_left_len
      and actual_right_fi = max (fi - actual_left_len) 0 in
      (
      try
        index_from_helper ~lf actual_left_ofs actual_left_len idx actual_left_fi left
      with Not_found -> 
        index_from_helper ~lf actual_right_ofs actual_right_len (idx + actual_left_len) actual_right_fi right
      ))
    ~sub: (fun ~rp ~rp_start ~rp_len -> 
      index_from_helper ~lf (rp_start + ofs) (min len rp_len) idx fi rp)

let rec rindex_from_helper
  ~(lf:int -> int -> int -> int -> string -> 'a)
  ofs len idx fi r = destruct r
    ~lf: (fun str -> lf ofs len idx fi str)
    ~br: (fun ~leftlen ~left ~right ->
      let actual_left_ofs = min ofs leftlen
      and actual_left_len = max (min len (leftlen - ofs)) 0 in
      let actual_right_ofs = max (ofs - leftlen) 0
      and actual_right_len = len - actual_left_len in
      let actual_left_fi = min fi (actual_left_len - 1)
      and actual_right_fi = max (fi - actual_left_len) 0 in
      (
      try
        rindex_from_helper ~lf actual_right_ofs actual_right_len (idx + actual_left_len) actual_right_fi right
      with Not_found -> 
        rindex_from_helper ~lf actual_left_ofs actual_left_len idx actual_left_fi left
      ))
    ~sub: (fun ~rp ~rp_start ~rp_len -> 
      rindex_from_helper ~lf (rp_start + ofs) (min len rp_len) idx fi rp)

  

let index_from_rec r idx fi c = 
  let lf ofs len idx fi str = 
    (String.index_from (String.sub str ofs len) fi c) + idx
  in index_from_helper ~lf 0 (length r) idx fi r
    

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


let rindex_from_rec r idx fi c = 
  let lf ofs len idx fi str = 
    (String.rindex_from (String.sub str ofs len) fi c) + idx
  in rindex_from_helper ~lf 0 (length r) idx fi r


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

let capitalize_ascii r = 
  let lf ofs len str = of_string (String.capitalize_ascii (String.sub str ofs len))
  in fold_left ~lf ~br:(^) ~p:is_empty 0 (length r) r

      
let uncapitalize_ascii r =
  let lf ofs len str = of_string (String.uncapitalize_ascii (String.sub str ofs len))
  in fold_left ~lf ~br:(^) ~p:is_empty 0 (length r) r
    

exception Out_of_bounds of string;;

module Iterator = struct
  type t = {
    rope : compactRope;
    rope_len : int; (* length of the rope globally *)
    mutable len : int; (* length of current cache*)
    mutable i : int; (* index of the local string cache *)
    mutable g_index : int; (* global index of the iterator *)
    mutable current : string; (* local cache *)
  }

  let rec find_local_index r i = 
    destruct r
      ~lf: (fun str -> (i, str))
      ~br: (fun ~leftlen ~left ~right ->
        if i < leftlen then
          find_local_index left i
        else
          find_local_index right (i - leftlen))
      ~sub: (fun ~rp ~rp_start ~rp_len -> 
        let sub_rp = sub_copy rp rp_start rp_len in
        find_local_index sub_rp i)

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

let split_on_char sep r = 
  let lf ofs len str = 
    List.map of_string (String.split_on_char sep (String.sub str ofs len))
  in fold ~lf ~br:(@) 0 (length r) r
    

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

