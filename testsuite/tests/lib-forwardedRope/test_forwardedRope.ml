(* TEST
*)
open ForwardedRope;;

let rec build_rope f n accu =
  if n <= 0
    then concat (of_string "") accu
    else build_rope f (n-1) (f (n-1) :: accu)
;;

let char n = make 1 (Char.chr n);;

let reference n =
  if n = 8 then of_string "\\b"
  else if n = 9 then of_string "\\t"
  else if n = 10 then of_string "\\n"
  else if n = 13 then of_string "\\r"
  else if n = Char.code '\"' then of_string "\\\""
  else if n = Char.code '\\' then of_string "\\\\"
  else if n < 32 || n > 126 then of_string (Printf.sprintf "\\%03d" n)
  else char n
;;

(* build 
       /                                       \
    /                          \ 
  /    \                /             \
Hello_  my_         /      \        /     \
                   na      me_i    s      _Simon

"Hello_my_name_is_Simon" *)

let build_rope2 () = 
  let r1 = of_string "Hello_"
  and r2 = of_string "my_"
  in let r3 = r1 ^ r2 
  in
  let r4 =  of_string "na"
  and r5 =  of_string "me_i"
  in let r6 = r4 ^ r5
  in
  let r7 = of_string "s" 
  and r8 = of_string "_Simon"
  in 
  let r9 =  r7 ^ r8
  in
  let r10 = r6 ^ r9
  in
  let r11 = r3 ^ r10
  in
  r11 ^ empty
;;


let raw_string = build_rope char 256 [];;
let ref_string = build_rope reference 256 [];;

if escaped raw_string <> ref_string then failwith "test:Rope.escaped";;


(* for this checking I am converting between rope and string 
because a same string might have different rope representations *)
let check_split sep s =
  let r = of_string s in
  let l = split_on_char sep r in
  assert(List.length l > 0);
  assert(to_string (concat (make 1 sep) l) = s);
  List.iter (iter (fun c -> assert (c <> sep))) l
;;  


let () =
  let s =" abc def " in
  for i = 0 to String.length s do
    check_split ' ' (String.sub s 0 i)
  done
;;

(* test length *)

let () =
  let r1 = of_string "abc"
  and r2 = of_string "de" in 
  let r3 = r1 ^ r2 in
  let r4 = sub r3 2 3 in 
  let r5 = sub r4 1 2 in 
  assert (length r5 = 2)


(* test to_string *)

let () = 
  let r1 = build_rope2 () in
  let r2 = sub r1 0 21 in
  let r3 = sub r2 1 20 in
  assert(to_string r3 = "ello_my_name_is_Simo")


(* test sub *)

let () = 
  let sub_string r s l = to_string (sub r s l) 
  and r = build_rope2 () in
  assert (sub_string r 2 4 = "llo_");
  assert (sub_string r 2 5 = "llo_m");
  assert (sub_string r 8 5 = "_name");
  assert (sub_string r 15 3 = "s_S")


(* test concat *)
let () = 
  let sep = sub (of_string "ab") 0 1
  and rl = [of_string "b"; of_string "c"; of_string "e"]
  in
  assert (to_string (concat sep rl) = "bacae") 

(* test mapi *)

let () = 
  let f i c = Char.chr (Char.code c + i) in
  let r1 = of_string "abc" ^ of_string "def" in
  let r2 = sub r1 2 4 in
  let r3 = of_string "ab" ^ r2 in
  assert (to_string (mapi f r3) = "acegik")

(* test trim *)

let () = 
  let r1 = of_string " a   b " ^ of_string " d e " in
  let r = (sub r1 0 5) ^ (of_string " d e ")
  in assert (to_string (trim r) = "a    d e")

(* test rindex_from_opt *)
let () = 
  let r1 = build_rope2 () in 
  let r2 = sub r1 1 21 in
  let r = of_string "H" ^ r2 in
  assert(rindex_from_opt r 6 'S' = None);
  assert(rindex_from_opt r 21 'S' = Some 17)

let () = 
  let r = sub (of_string "abc" ^ of_string "de") 2 3 in
  assert(rindex_from_opt r 2 'c' = Some 0)

(* test compare *)

let () = 
  let r1 = build_rope2 ()
  and r2 = of_string "Hello_my_name_is_Simom" in
  let r3 = sub r2 0 21 in
  let r4 = r3 ^ of_string "m"
  in assert ((compare r1 r4) > 0)

(* test equals for different representation of the same string *)

let () = 
  let r1 = (of_string "abc") ^ (of_string "de") in
  let r2 = sub r1 1 2
  and r3 = sub r1 3 1 in
  let r4 = r2 ^ r3
  and r5 = sub r1 1 3 in
  assert (equal r4 r5)

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
