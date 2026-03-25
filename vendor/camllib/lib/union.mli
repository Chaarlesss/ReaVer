(** Union-find Abstract Data Types *)

(** The type of the data structure storing set membership (the universe) *)
type 'a t

(** Create a new universe set *)
val create : int -> 'a t

(** Add an element to the universe (initially belonging to its singleton) *)
val add : 'a t -> 'a -> unit

val find : 'a t -> 'a -> 'a
(* Find the set of an element *)

(** Computes the union of two sets and returns the resulting set *)
val union : 'a t -> 'a -> 'a -> 'a

(** Extract the list of sets *)
val extract : 'a t -> 'a list list
