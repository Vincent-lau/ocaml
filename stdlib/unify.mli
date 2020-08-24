type tyvar
type typ' and typ



val (:=) : typ -> typ' -> unit
val (!) : typ -> typ'
val tref : typ' -> typ

val build_link_typ : typ -> typ

val pp_typ : Format.formatter -> typ -> unit 

val occurs : tyvar -> typ -> bool
  
val occurs' : tyvar -> typ' -> bool
 
val unify : typ -> typ -> unit
 
val unify' : typ' -> typ' -> typ'

val var : unit -> typ
 
val (@->): typ -> typ -> typ

