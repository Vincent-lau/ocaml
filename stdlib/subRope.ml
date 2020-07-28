
module B = Bytes

type t = Leaf of string 
  | Branch of {leftlen: int; left: t; right: t}
  | Sub of {rp: t; start: int; len: int}

type rope = t


let bts = B.unsafe_to_string
let bos = B.unsafe_of_string

let empty = Leaf ""

let of_string s = Leaf s



let rec length = function
  | Leaf str -> String.length str
  | Branch b -> b.leftlen + length b.right
  | Sub sub -> sub.len



let rope_concat r1 r2 = 
  if r1 = empty then r2 
  else if r2 = empty then r1
  else Branch {leftlen = (length r1); left = r1; right = r2}

let (^) = rope_concat

let is_empty r = (length r = 0)

let sub r ofs len = 
  if ofs < 0 || len < 0 || ofs + len > (length r) then 
    invalid_arg "Rope.sub"
  else if len = 0 then
    empty
  else
    Sub {rp = r; start = ofs; len}


let rec fold
  ~(lf:int -> int -> string -> 'a) 
  ~(br:'a ->'a -> 'a)
  ofs len = function
    | Leaf str -> lf ofs len str 
    | Branch {leftlen; left; right} ->
        let actual_left_ofs = min ofs leftlen
        and actual_left_len = max (min len (leftlen - ofs)) 0 in
        let actual_right_ofs = max (ofs - leftlen) 0
        and actual_right_len = len - actual_left_len in
        let left_sub = 
          fold ~lf:lf ~br:br actual_left_ofs actual_left_len left
        and right_sub = 
          fold ~lf:lf ~br:br actual_right_ofs actual_right_len right
        in br left_sub right_sub
    | Sub s -> 
      fold ~lf:lf ~br:br (s.start + ofs) (min len s.len)  s.rp
    

(* ofs is the start position of a substring
idx is the index we need to add, i.e. the length
of the string to the left of the current node *)
let rec foldi
  ~(lf:int -> int -> int -> string -> 'a)
  ~(br:'a ->'a -> 'a)
  ofs len idx = function 
    | Leaf str -> lf ofs len idx str
    | Branch {leftlen; left; right} ->
      let actual_left_ofs = min ofs leftlen
      and actual_left_len = max (min len (leftlen - ofs)) 0 in
      let actual_right_ofs = max (ofs - leftlen) 0
      and actual_right_len = len - actual_left_len in
      let left_sub = 
        foldi ~lf:lf ~br:br actual_left_ofs actual_left_len idx left 
      and right_sub = foldi ~lf:lf ~br:br actual_right_ofs actual_right_len
        (idx + actual_left_len) right
      in br left_sub right_sub
    | Sub s -> 
      foldi ~lf:lf ~br:br (s.start + ofs) (min len s.len) idx s.rp


let iter f r = 
  let lf ofs len str = String.iter f (String.sub str ofs len)
  and br () () = () in
  fold ~lf:lf ~br:br 0 (length r) r


let iteri_rec f idx r = 
  let f_idx o i c = f (i + o) c in
  let lf ofs len idx str = String.iteri (f_idx idx) (String.sub str ofs len)
  and br () () = () in
  foldi ~lf:lf ~br:br 0 (length r) idx r


    (* match rp with 
    | Leaf str -> iteri_rec f ofs (of_string (String.sub str start len))
    | Branch {leftlen; left; right} ->
      let actual_left_len = max (min len (leftlen - start)) 0 in
        if start < leftlen then
          iteri_rec f ofs (Sub {rp = left; start; len = actual_left_len});
        if start + len >= leftlen then
          let actual_start = max (start - leftlen) 0
          and actual_right_len = len - actual_left_len in
          iteri_rec f (ofs + actual_left_len) (Sub {rp = right; start = actual_start; len = actual_right_len})
    | Sub r -> 
      iteri_rec f ofs (Sub {rp = r.rp; start = start + r.start; len}) *)

    
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

let to_string r = 
  let b = Bytes.create (length r) in
  let to_string_rec b idx r =
    let lf ofs len idx str = Bytes.blit_string str ofs b idx len
    and br () () = () in
    foldi ~lf ~br 0 (length r) idx r
  in 
  to_string_rec b 0 r; b |> bts
    


let make n c =
  B.make n c |> bts |> of_string

let init n f =
  B.init n f |> bts |> of_string

  
let copy r =
  let s = to_string r in
  B.copy (bos s) |> bts |> of_string 
(* deprecated, so just pure conversion *)



let get r i = 
  String.get (to_string (sub r i 1)) 0

(* deprecated *)
let fill = 
  B.fill
  
(* pure conversion might not be efficient *)  
let blit r = 
  to_string r |> B.blit_string

    
let rec concat sep = function
    [] -> empty
  | hd :: [] -> hd
  | hd :: tl -> (hd ^ sep) ^ (concat sep tl)


(* let rec iter f = function
  | Leaf str -> String.iter f str
  | Branch b -> 
    begin
      iter f b.left;
      iter f b.right
    end
  | Sub {rp; start; len} ->
    let lf str =  String.iter f str
    and br () () = () in
    sub_helper rp start len ~lf:lf ~br:br *)



    (* match rp with 
    | Leaf str -> iter f (of_string (String.sub str start len))
    | Branch {leftlen; left; right} ->
      let actual_left_len = max (min len (leftlen - start)) 0 in
        if start < leftlen then
          iter f (Sub {rp = left; start; len = actual_left_len});
        if start + len >= leftlen then
          let actual_start = max (start - leftlen) 0
          and actual_right_len = len - actual_left_len in
          iter f (Sub {rp = right; start = actual_start; len = actual_right_len})
    | Sub r -> 
      iter f (Sub {rp = r.rp; start = start + r.start; len}) *)
      

(* let rec iteri_rec f ofs r = 
  let f_ofs o i c = f (i + o) c in
  match r with
  | Leaf str -> String.iteri (f_ofs ofs) str
  | Branch b -> 
    iteri_rec f ofs b.left;
    iteri_rec f (ofs + b.leftlen) b.right
  | Sub {rp; start; len} ->
    let lf str ofs = String.iteri (f_ofs ofs) str
    and br () () = () in
    sub_helper_ofs rp start len ofs ~lf:lf ~br:br *)




    (* match rp with 
    | Leaf str -> iteri_rec f ofs (of_string (String.sub str start len))
    | Branch {leftlen; left; right} ->
      let actual_left_len = max (min len (leftlen - start)) 0 in
        if start < leftlen then
          iteri_rec f ofs (Sub {rp = left; start; len = actual_left_len});
        if start + len >= leftlen then
          let actual_start = max (start - leftlen) 0
          and actual_right_len = len - actual_left_len in
          iteri_rec f (ofs + actual_left_len) (Sub {rp = right; start = actual_start; len = actual_right_len})
    | Sub r -> 
      iteri_rec f ofs (Sub {rp = r.rp; start = start + r.start; len}) *)

    
(* let iteri f r = iteri_rec f 0 r

let rec map f = function
  | Leaf str -> Leaf (String.map f str)
  | Branch b ->
    let left = map f b.left and right = map f b.right 
    in left ^ right
  | Sub {rp; start; len} ->
    let lf str = Leaf (String.map f str)
    and br = (^) in
    sub_helper rp start len ~lf:lf ~br:br
   *)




let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let rec fold_left 
  ~(lf:int -> int -> string -> 'a)
  ~(br:'a -> 'a -> 'a)
  ~(p: 'a -> bool)
  ofs len = function
    | Leaf str -> lf ofs len str
    | Branch {leftlen; left; right} ->
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
        br left_sub right
    | Sub s -> 
      fold_left ~lf ~br ~p (s.start + ofs) (min len s.len) s.rp
    
let rec fold_right 
  ~(lf:int -> int -> string -> 'a)
  ~(br:'a -> 'a -> 'a)
  ~(p: 'a -> bool)
  ofs len = function
    | Leaf str -> lf ofs len str
    | Branch {leftlen; left; right} ->
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
        br left right_sub
    | Sub s -> 
      fold_left ~lf ~br ~p (s.start + ofs) (min len s.len) s.rp
    


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


(* fi is the from index for each node
unfortunately this implementation is still quite complicated
and I could not find a way to simplify it *)
let rec index_from_helper
  ~(lf:int -> int -> int -> int -> string -> 'a)
  ofs len idx fi = function
    | Leaf str -> lf ofs len idx fi str
    | Branch {leftlen; left; right} ->
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
      )
    | Sub s -> 
      index_from_helper ~lf (s.start + ofs) (min len s.len) idx fi s.rp
    
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


let rec rindex_from_helper
  ~(lf:int -> int -> int -> int -> string -> 'a)
  ofs len idx fi = function
    | Leaf str -> lf ofs len idx fi str
    | Branch {leftlen; left; right} ->
      let actual_left_ofs = min ofs leftlen
      and actual_left_len = max (min len (leftlen - ofs)) 0 in
      let actual_right_ofs = max (ofs - leftlen) 0
      and actual_right_len = len - actual_left_len in
      let actual_left_fi = min fi actual_left_len
      and actual_right_fi = max (fi - actual_left_len) 0 in
      (
      try
        rindex_from_helper ~lf actual_right_ofs actual_right_len (idx + actual_left_len) actual_right_fi right
      with Not_found -> 
        rindex_from_helper ~lf actual_left_ofs actual_left_len idx actual_left_fi left
      )
    | Sub s -> 
      rindex_from_helper ~lf (s.start + ofs) (min len s.len) idx fi s.rp

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
    | Sub {rp; start; len} ->
      match rp with 
      | Leaf str -> find_local_index (of_string (String.sub str start len)) i
      | Branch {leftlen; left; right} ->
        let actual_left_len = max (min len (leftlen - start)) 0 in
        let actual_right_start = max (start - leftlen) 0
        and actual_right_len = len - actual_left_len in
        if start < leftlen && start + len >= leftlen then(
          if i < actual_left_len then
            find_local_index (Sub {rp = left; start; len = actual_left_len}) i
          else
            find_local_index 
              (Sub {rp = right; start = actual_right_start;len = actual_right_len}) (i - actual_left_len)
        )
        else if start < leftlen then(
          find_local_index (Sub {rp = left; start; len = actual_left_len}) i
        )
        else(
          find_local_index 
            (Sub {rp = right; start = actual_right_start;len = actual_right_len}) (i - actual_left_len)
        )
      | Sub r -> 
        find_local_index (Sub {rp = r.rp; start = start + r.start; len}) i
      
      
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
