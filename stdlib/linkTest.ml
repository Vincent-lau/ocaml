type 'a fwd = int

type t = 
  | Lf of int
  | Lk of {__fwd_fun: t fwd; mylink: t} [@forward "caml_mylink_get_fwd"];;

external get_mylink_fwd_fun: unit -> t fwd = "caml_mylink_get_fwd"

let my_link_fwd_fun = get_mylink_fwd_fun ()

(* let collpase_link l = 
  match l with 
    | Lf _ -> l
    | Lk {link} -> link *)

let cons_link l = 
  Lk {__fwd_fun = my_link_fwd_fun; mylink = l}


let rec build_link n =
  if n == 0 then Lf 1
  else cons_link (build_link (n - 1))

let print_link l =  
  let rec print_link_rec = function
    | Lf n -> print_int n
    | Lk {mylink} -> 
      print_string "-> "; print_link_rec mylink
  in
  print_link_rec l; print_newline ()


