type tyvar = int
type 'a fwd = int
type typ' =
| Fun of typ * typ
| Var of tyvar
| Link of typ 
and typ = {collapse_links: typ fwd; 
  mutable contents : typ' }[@@forward collapse_links]

external get_unify_collpase_links: unit -> typ fwd = "caml_unify_get_fwd"

let collapse_links = get_unify_collpase_links ()

(* let collapse_links ({collapse_links = _; contents} as t: typ): typ =
  match contents with
  | Link t' -> t'
  | _ -> t *)


let (:=) (t:typ) (t':typ') = t.contents <- t'
let (!) {contents} = contents
let tref contents = {collapse_links; contents}


let pp_typ  : Format.formatter -> typ -> unit =
  fun fmt t ->
  let module Teq : Hashtbl.HashedType with type t = typ =
    struct
      type t = typ
      let equal = (==)
      let hash = Hashtbl.hash
    end in
  let module H = Hashtbl.Make(Teq) in 
  let tbl = H.create 10 in
  let counter = ref 0 in
  let rec pp_typ : Format.formatter -> typ -> unit =
    fun fmt t ->
    match H.find tbl t with
    | x -> Format.fprintf fmt "@[ref[%d]@ @[(%a)@]@]" x pp_typ' !t
    | exception Not_found ->
       H.add tbl t Stdlib.(!counter);
       incr counter;
       pp_typ fmt t
  and pp_typ' : Format.formatter -> typ' -> unit =
    fun fmt t ->
    match t with
    | Fun (l, r) -> Format.fprintf fmt "@[Fun (@[%a@],@,@[%a@])@]" pp_typ l pp_typ r
    | Var x -> Format.fprintf fmt "?%d" x
    | Link t -> Format.fprintf fmt "@[Link@ @[%a@]@]@," pp_typ t
  in pp_typ fmt t

(* Run this in the top-level to use the pretty printer:
#install_printer pp_typ;;
*)

let rec occurs : tyvar -> typ -> bool =
  fun x typ -> occurs' x !typ
and occurs' : tyvar -> typ' -> bool =
  fun x typ ->
  match typ with
  | Fun (l, r) -> occurs x l || occurs x r
  | Var y -> x = y
  | Link t -> occurs x t

exception Unify
let rec unify : typ -> typ -> unit =
  fun l r ->
  match !l, !r with
  | Link l, _ -> unify l r
  | _, Link r -> unify l r
  | _ when l == r   -> ()
  | _               -> l := unify' !l !r;
                       r := Link l
and unify' : typ' -> typ' -> typ' =
  fun l r ->
  match l, r with
  | Var x, t
  | t, Var x -> if occurs' x t then raise Unify else t
  | Fun (a, b), Fun (c, d) -> unify a c;
                              unify b d;
                              l
  | _ -> raise Unify


let var : unit -> typ =
  let counter = ref 0 in
  fun () -> incr counter; tref (Var Stdlib.(!counter))

let (@->) x y : typ = tref (Fun (x, y))
(* 
let example =
  let a = var () and b = var () in
  let x = a @-> b
  and y = b @-> a in
  unify x y;
  (x, y) *)
