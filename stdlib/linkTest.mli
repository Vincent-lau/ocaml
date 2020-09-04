(* for testing major collector purpose only *)
type 'a fwd
type t

external get_mylink_fwd_fun: unit -> t fwd = "caml_mylink_get_fwd"

val build_link: int -> t

val print_link: t -> unit
