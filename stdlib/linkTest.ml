type t = 
  | Lf of int
  | Lk of (t->t) * t [@forward collpase_link]

(* let leaf (n: int): t = Obj.obj (Obj.repr n)

external link: t -> t = "caml_testlink_link" *)


let collpase_link l = 
  match l with 
    | Lf _ -> l
    | Lk (_, cl) -> cl


(* let destruct (l: t)
    ~(lf: int -> 'a) 
    ~(lk: t -> 'a) =
  let rt_l = Obj.repr l in
  if Obj.tag rt_l = Obj.int_tag then(
    lf (Obj.obj rt_l : int) 
  )
  else 
  begin
    assert (Obj.tag rt_l = Obj.forward_tag);
    lk (Obj.obj (Obj.repr (Obj.field rt_l 1)) : t)
  end *)

let rec build_link n =
  if n == 0 then Lf 1
  else Lk (collpase_link ,(build_link (n - 1)))

let print_link l =  
  let rec print_link_rec = function
    | Lf n -> print_int n
    | Lk (_, l) -> 
      print_string "-> "; print_link_rec l
  in
  print_link_rec l; print_newline ()


