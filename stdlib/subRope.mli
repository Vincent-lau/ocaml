(* external length : string -> int = "%t_length"
(** Return the length (number of characters) of the given t. *)

external get : string -> int -> char = "%t_safe_get"
(** [t.get s n] returns the character at index [n] in t [s].
   You can also write [s.[n]] instead of [t.get s n].

   Raise [Invalid_argument] if [n] not a valid index in [s]. *)


external set : bytes -> int -> char -> unit = "%t_safe_set"
  [@@ocaml.deprecated "Use Bytes.set instead."]
(** [t.set s n c] modifies byte sequence [s] in place,
   replacing the byte at index [n] with [c].
   You can also write [s.[n] <- c] instead of [t.set s n c].

   Raise [Invalid_argument] if [n] is not a valid index in [s].

   @deprecated This is a deprecated alias of {!Bytes.set}.[ ] *)

external create : int -> bytes = "caml_create_t"
  [@@ocaml.deprecated "Use Bytes.create instead."]
(** [t.create n] returns a fresh byte sequence of length [n].
   The sequence is uninitialized and contains arbitrary bytes.

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_t_length}.

   @deprecated This is a deprecated alias of {!Bytes.create}.[ ] *) *)

(* external functions above ignored for the moment *)

type t
(** An alias for the type of ts. *)

type rope = t
(** alias for type rope *)

val empty : rope

val length : t -> int

val to_string: t -> string

val of_string: string -> t

val get: rope -> int -> char

val (^): rope -> rope -> rope

val make : int -> char -> t
(** [t.make n c] returns a fresh rope of length [n],
   filled with the character [c].

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_t_length}. *)

val init : int -> (int -> char) -> t
(** [Rope.init n f] returns a rope of length [n], with character
    [i] initialized to the result of [f i] (called in increasing
    index order).

    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_t_length}.

    @since 4.02.0
*)

val copy : t -> t [@@ocaml.deprecated]
(** Return a copy of the given t.

    @deprecated Because ts are immutable, it doesn't make much
    sense to make identical copies of them. *)

val sub : t -> int -> int -> t
(** [t.sub s start len] returns a fresh t of length [len],
   containing the subt of [s] that starts at position [start] and
   has length [len].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid substring of [s]. *)

val fill : bytes -> int -> int -> char -> unit
  [@@ocaml.deprecated "Use Bytes.fill instead."]
(** [t.fill s start len c] modifies byte sequence [s] in place,
   replacing [len] bytes with [c], starting at [start].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid range of [s].

   @deprecated This is a deprecated alias of {!Bytes.fill}.[ ] *)

val blit : t -> int -> bytes -> int -> int -> unit
(** Same as {!Bytes.blit_t}. *)

val concat : t -> t list -> t
(** [t.concat sep sl] concatenates the list of ts [sl],
    inserting the separator t [sep] between each.

    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_t_length} bytes. However, ropes should not 
   have a physical limit as it uses pointer *)

val iter : (char -> unit) -> t -> unit
(** [t.iter f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[t.length s - 1]; ()]. *)

val iteri : (int -> char -> unit) -> t -> unit
(** Same as {!t.iter}, but the
   function is applied to the index of the element as first argument
   (counting from 0), and the character itself as second argument.
   @since 4.00.0 *)

val map : (char -> char) -> t -> t
(** [t.map f s] applies function [f] in turn to all the
    characters of [s] (in increasing index order) and stores the
    results in a new t that is returned.
    @since 4.00.0 *)

val mapi : (int -> char -> char) -> t -> t
(** [t.mapi f s] calls [f] with each character of [s] and its
    index (in increasing index order) and stores the results in a new
    t that is returned.
    @since 4.02.0 *)

val trim : t -> t
(** Return a copy of the argument, without leading and trailing
   whitespace.  The characters regarded as whitespace are: [' '],
   ['\012'], ['\n'], ['\r'], and ['\t'].  If there is neither leading nor
   trailing whitespace character in the argument, return the original
   t itself, not a copy.
   @since 4.00.0 *)

val escaped : t -> t
(** Return a copy of the argument, with special characters
    represented by escape sequences, following the lexical
    conventions of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash and double-quote.

    If there is no special character in the argument that needs
    escaping, return the original t itself, not a copy.

    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_t_length} bytes.

    The function {!Scanf.unescaped} is a left inverse of [escaped],
    i.e. [Scanf.unescaped (escaped s) = s] for any t [s] (unless
    [escape s] fails). *)

val index : t -> char -> int
(** [t.index s c] returns the index of the first
   occurrence of character [c] in t [s].

   Raise [Not_found] if [c] does not occur in [s]. *)

val index_opt: t -> char -> int option
(** [t.index_opt s c] returns the index of the first
    occurrence of character [c] in t [s], or
    [None] if [c] does not occur in [s].
    @since 4.05 *)

val rindex : t -> char -> int
(** [t.rindex s c] returns the index of the last
   occurrence of character [c] in t [s].

   Raise [Not_found] if [c] does not occur in [s]. *)

val rindex_opt: t -> char -> int option
(** [t.rindex_opt s c] returns the index of the last occurrence
    of character [c] in t [s], or [None] if [c] does not occur in
    [s].
    @since 4.05 *)

val index_from : t -> int -> char -> int
(** [t.index_from s i c] returns the index of the
   first occurrence of character [c] in t [s] after position [i].
   [t.index s c] is equivalent to [t.index_from s 0 c].

   Raise [Invalid_argument] if [i] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] after position [i]. *)

val index_from_opt: t -> int -> char -> int option
(** [t.index_from_opt s i c] returns the index of the
    first occurrence of character [c] in t [s] after position [i]
    or [None] if [c] does not occur in [s] after position [i].

    [t.index_opt s c] is equivalent to [t.index_from_opt s 0 c].
    Raise [Invalid_argument] if [i] is not a valid position in [s].

    @since 4.05
*)

val rindex_from : t -> int -> char -> int
(** [t.rindex_from s i c] returns the index of the
   last occurrence of character [c] in t [s] before position [i+1].
   [t.rindex s c] is equivalent to
   [t.rindex_from s (t.length s - 1) c].

   Raise [Invalid_argument] if [i+1] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] before position [i+1]. *)

val rindex_from_opt: t -> int -> char -> int option
(** [t.rindex_from_opt s i c] returns the index of the
   last occurrence of character [c] in t [s] before position [i+1]
   or [None] if [c] does not occur in [s] before position [i+1].

   [t.rindex_opt s c] is equivalent to
   [t.rindex_from_opt s (t.length s - 1) c].

   Raise [Invalid_argument] if [i+1] is not a valid position in [s].

    @since 4.05
*)

val contains : t -> char -> bool
(** [t.contains s c] tests if character [c]
   appears in the t [s]. *)

val contains_from : t -> int -> char -> bool
(** [t.contains_from s start c] tests if character [c]
   appears in [s] after position [start].
   [t.contains s c] is equivalent to
   [t.contains_from s 0 c].

   Raise [Invalid_argument] if [start] is not a valid position in [s]. *)

val rcontains_from : t -> int -> char -> bool
(** [t.rcontains_from s stop c] tests if character [c]
   appears in [s] before position [stop+1].

   Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
   position in [s]. *)

val uppercase : t -> t
  [@@ocaml.deprecated "Use t.uppercase_ascii instead."]
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, including accented letters of the ISO
   Latin-1 (8859-1) character set.
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val lowercase : t -> t
  [@@ocaml.deprecated "Use t.lowercase_ascii instead."]
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set.
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val capitalize : t -> t
  [@@ocaml.deprecated "Use t.capitalize_ascii instead."]
(** Return a copy of the argument, with the first character set to uppercase,
   using the ISO Latin-1 (8859-1) character set..
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val uncapitalize : t -> t
  [@@ocaml.deprecated "Use t.uncapitalize_ascii instead."]
(** Return a copy of the argument, with the first character set to lowercase,
   using the ISO Latin-1 (8859-1) character set..
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val uppercase_ascii : t -> t
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, using the US-ASCII character set.
   @since 4.03.0 *)

val lowercase_ascii : t -> t
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, using the US-ASCII character set.
   @since 4.03.0 *)

val capitalize_ascii : t -> t
(** Return a copy of the argument, with the first character set to uppercase,
   using the US-ASCII character set.
   @since 4.03.0 *)

val uncapitalize_ascii : t -> t
(** Return a copy of the argument, with the first character set to lowercase,
   using the US-ASCII character set.
   @since 4.03.0 *)



val compare: t -> t -> int
(** The comparison function for ts, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [t] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for ts.
    @since 4.03.0 *)

val split_on_char: char -> t -> t list
(** [t.split_on_char sep s] returns the list of all (possibly empty)
    subts of [s] that are delimited by the [sep] character.

    The function's output is specified by the following invariants:

    - The list is not empty.
    - Concatenating its elements using [sep] as a separator returns a
      t equal to the input ([t.concat (t.make 1 sep)
      (t.split_on_char sep s) = s]).
    - No t in the result contains the [sep] character.

    @since 4.04.0
*)

(* module interface copied from 
https://github.com/Chris00/ocaml-rope/tree/master/src *)
module Iterator : sig

    type t
      (** Mutable iterator on a rope.  Iterators are less efficient than
          {!Rope.get} on small ropes (of length [<= 1024] chars). *)
  
    val make : rope -> int -> t
      (** [make r i0] returns a new iterator for the rope [r].  It is
          initially at position [i0]. *)
  
    val get : t -> char
      (** [get itr] returns the character of the rope at the current
          position.  O(1) time.  This does not change the current position.
          @raise Out_of_bounds if the position is outside the rope. *)
  
    val peek : t -> int -> char
      (** [peek itr i] returns the character [i] of the rope.  If [i] is
          close to the current position of the iterator, this will in
          general be more efficient than [get rope i].  *)
  
    val pos : t -> int
      (** [pos itr] returns the current position.  It may not be a valid
          position of the rope.  O(1) time. *)
  
    val incr : t -> unit
      (** [incr itr] moves to the next character.  O(1) time. *)
  
    val decr : t -> unit
      (** [decr itr] moves to the previous character.  O(1) time.  *)
  
    val goto : t -> int -> unit
      (** [goto itr i] move to position [i].  O(1) time but the next
          call to [get] may be slower. *)
  
    val move : t -> int -> unit
      (** [mode itr i] move the current position by [i] chars ([i] may
          be negative or null).  O(1) time but the next call to [get]
          may be slower. *)
  
    val rope : t -> rope
      (** [rope itr] returns the rope from which the iterator was
          constructed. *)
  end
  


(** {1 Iterators} *)

(* val to_seq : t -> char Seq.t *)
(** Iterate on the t, in increasing index order. Modifications of the
    t during iteration will be reflected in the iterator.
    @since 4.07 *)

(* val to_seqi : t -> (int * char) Seq.t *)
(** Iterate on the t, in increasing order, yielding indices along chars
    @since 4.07 *)

(* val of_seq : char Seq.t -> t *)
(** Create a t from the generator
    @since 4.07 *)

(**/**)

(* The following is for system use only. Do not call directly. *)

(* external unsafe_get : t -> int -> char = "%t_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%t_unsafe_set"
  [@@ocaml.deprecated]
external unsafe_blit :
  t -> int -> bytes -> int -> int -> unit
  = "caml_blit_t" [@@noalloc]
external unsafe_fill :
  bytes -> int -> int -> char -> unit = "caml_fill_t" [@@noalloc]
  [@@ocaml.deprecated] *)
