(* for testing major collector purpose only *)
type t

val build_link: int -> t

val print_link: t -> unit

val wrap: t -> t