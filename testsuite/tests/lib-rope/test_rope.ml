(* TEST
*)

let rec build_rope f n accu =
  if n <= 0
    then Rope.concat (Rope.of_string "") accu
    else build_rope f (n-1) (f (n-1) :: accu)
;;

let char n = Rope.make 1 (Char.chr n);;

let reference n =
  if n = 8 then Rope.of_string "\\b"
  else if n = 9 then Rope.of_string "\\t"
  else if n = 10 then Rope.of_string "\\n"
  else if n = 13 then Rope.of_string "\\r"
  else if n = Char.code '\"' then Rope.of_string "\\\""
  else if n = Char.code '\\' then Rope.of_string "\\\\"
  else if n < 32 || n > 126 then Rope.of_string (Printf.sprintf "\\%03d" n)
  else char n
;;

let raw_string = build_rope char 256 [];;
let ref_string = build_rope reference 256 [];;

if Rope.escaped raw_string <> ref_string then failwith "test:Rope.escaped";;


(* for this checking I am converting between rope and string 
because a same string might have different rope representations *)
let check_split sep s =
  let r = Rope.of_string s in
  let l = Rope.split_on_char sep r in
  assert(List.length l > 0);
  assert(Rope.to_string (Rope.concat (Rope.make 1 sep) l) = s);
  List.iter (Rope.iter (fun c -> assert (c <> sep))) l
;;  

let () =
  let s =" abc def " in
  for i = 0 to String.length s do
    check_split ' ' (String.sub s 0 i)
  done
;;

(* GPR#805/815/833 *)

let ()  =
  if Sys.word_size = 32 then begin
    let big = String.make Sys.max_string_length 'x' in
    let push x l = l := x :: !l in
    let (+=) a b = a := !a + b in
    let sz, l = ref 0, ref [] in
    while !sz >= 0 do push big l; sz += Sys.max_string_length done;
    while !sz <= 0 do push big l; sz += Sys.max_string_length done;
    try ignore (String.concat "" !l); assert false
    with Invalid_argument _ -> ()
  end
