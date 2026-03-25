(** Two-way hashtable between two data types *)

(** The type of two-way hashtables, meant to be abstract *)
type ('a, 'b) t =
  { xy : ('a, 'b) Hashhe.t
  ; yx : ('b, 'a) Hashhe.t
  }

val hashx : ('a, 'b) t -> ('a, 'b) Hashhe.t

(** Return the correspondance hashtable resp. from x to y and from y to x.
      Never modify it !!! *)
val hashy : ('a, 'b) t -> ('b, 'a) Hashhe.t

(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)
val clear : ('a, 'b) t -> unit

(** Empty a hash table and shrink the size of the bucket table
    to its initial size. *)
val reset : ('a, 'b) t -> unit

(** Create a new table, with the specified initial size *)
val create : int -> ('a, 'b) t

(** Add a new binding to the table. *)
val add : ('a, 'b) t -> 'a -> 'b -> unit

(** Association. *)
val y_of_x : ('a, 'b) t -> 'a -> 'b

(** Inverse association. *)
val x_of_y : ('a, 'b) t -> 'b -> 'a

(** Remove a binding defined by its first element. *)
val removex : ('a, 'b) t -> 'a -> unit

(** Remove a binding defined by its second element. *)
val removey : ('a, 'b) t -> 'b -> unit

(** Is the object registered ? *)
val memx : ('a, 'b) t -> 'a -> bool

(** Is the object registered  ? *)
val memy : ('a, 'b) t -> 'b -> bool

(** Iterate on bindings. *)
val iter : ('a, 'b) t -> ('a -> 'b -> unit) -> unit

(** Iterate on bindings and accumulating a result. *)
val fold : ('a, 'b) t -> 'c -> ('a -> 'b -> 'c -> 'c) -> 'c

(** Return the number of bindings. *)
val cardinal : ('a, 'b) t -> int

(** Print the set of bindings. *)
val print
  :  ?first:(unit, Format.formatter, unit) format
  -> ?sep:(unit, Format.formatter, unit) format
  -> ?last:(unit, Format.formatter, unit) format
  -> ?firstbind:(unit, Format.formatter, unit) format
  -> ?sepbind:(unit, Format.formatter, unit) format
  -> ?lastbind:(unit, Format.formatter, unit) format
  -> (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) t
  -> unit

(** Input signature of the functor {!DHashhe.Make}. *)
module type Param = sig
  (** Hashtable for objects in the first place of bindings *)
  module HashX : Hashhe.S

  (** Hashtable for objects in the second place of bindings *)
  module HashY : Hashhe.S
end

(** Output signature of the functor {!DHashhe.Make}. *)
module type S = sig
  module HashX : Hashhe.S
  module HashY : Hashhe.S

  type x = HashX.key
  type y = HashY.key
  type t

  val hashx : t -> y HashX.t
  val hashy : t -> x HashY.t
  val clear : t -> unit
  val reset : t -> unit
  val create : int -> t
  val add : t -> x -> y -> unit
  val y_of_x : t -> x -> y
  val x_of_y : t -> y -> x
  val removex : t -> x -> unit
  val removey : t -> y -> unit
  val memx : t -> x -> bool
  val memy : t -> y -> bool
  val iter : t -> (x -> y -> unit) -> unit
  val fold : t -> 'a -> (x -> y -> 'a -> 'a) -> 'a
  val cardinal : t -> int

  val print
    :  ?first:(unit, Format.formatter, unit) format
    -> ?sep:(unit, Format.formatter, unit) format
    -> ?last:(unit, Format.formatter, unit) format
    -> ?firstbind:(unit, Format.formatter, unit) format
    -> ?sepbind:(unit, Format.formatter, unit) format
    -> ?lastbind:(unit, Format.formatter, unit) format
    -> (Format.formatter -> x -> unit)
    -> (Format.formatter -> y -> unit)
    -> Format.formatter
    -> t
    -> unit
end

(** Functor building an implementation of the DHashtbl structure
   given two hashtables *)
module Make (P : Param) : S with module HashX = P.HashX and module HashY = P.HashY
