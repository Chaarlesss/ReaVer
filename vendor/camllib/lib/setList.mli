(** Sets over totally ordered type with lists. All operations over sets are
    purely applicative (no side-effects). *)

(** The type of sets over elements of type ['a]. *)
type 'a t

(** Printing function *)
val print
  :  ?first:(unit, Format.formatter, unit) format
  -> ?sep:(unit, Format.formatter, unit) format
  -> ?last:(unit, Format.formatter, unit) format
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit

(** The empty set. *)
val empty : 'a t

(** Test whether a set is empty or not. *)
val is_empty : 'a t -> bool

(** [mem x s] tests whether [x] belongs to the set [s]. *)
val mem : 'a -> 'a t -> bool

(** Conversion from a list (unsafe operation) *)
val of_list : 'a list -> 'a t

(** Conversion to a list *)
val to_list : 'a t -> 'a list

(** [singleton x] returns the one-element set containing only [x]. *)
val singleton : 'a -> 'a t

(** [add x s] returns a set containing all elements of [s],
	   plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
val add : 'a -> 'a t -> 'a t

(** [remove x s] returns a set containing all elements of [s], except
       [x]. If [x] was not in [s], [s] is returned unchanged. *)
val remove : 'a -> 'a t -> 'a t

val union : 'a t -> 'a t -> 'a t
val inter : 'a t -> 'a t -> 'a t

(** Union, intersection and set difference. *)
val diff : 'a t -> 'a t -> 'a t

(** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)
val compare : 'a t -> 'a t -> int

(** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)
val equal : 'a t -> 'a t -> bool

(** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)
val subset : 'a t -> 'a t -> bool

(** [iter f s] applies [f] in turn to all elements of [s].
       The order in which the elements of [s] are presented to [f]
       is in ascending order. *)
val iter : ('a -> unit) -> 'a t -> unit

(** [fold f s a]
       computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s].
       The order in which elements of [s] are presented to [f] is
       in ascending order.
      @param f function
      @param s set
      @param a accumulator
      @return the computed accumulator
      @raise Not_found if no fount
    *)
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(** Idem as [List.fold_X] functions *)
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

(** Return the number of elements of a set. *)
val cardinal : 'a t -> int

(** Return the list of all elements of the given set.  The returned list
       is sorted in increasing order with respect to the ordering
       [Stdlib.compare]. *)
val elements : 'a t -> 'a list

(** Return the smallest element of the given set (with respect to the
       [Stdlib.compare] ordering), or raise [Not_found] if the set is
       empty. *)
val min_elt : 'a t -> 'a

(** Same as [min_elt], but returns the largest element of the given
       set. *)
val max_elt : 'a t -> 'a

(** Return one element of the given set, or raise [Not_found] if the set
       is empty. Which element is chosen is unspecified, but equal elements
       will be chosen for equal sets. *)
val choose : 'a t -> 'a

(** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** [partition p s] returns a pair of sets [(s1, s2)], where [s1] is the
       set of all the elements of [s] that satisfy the predicate [p], and [s2]
       is the set of all the elements of [s] that do not satisfy [p]. *)
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

(** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)
val exists : ('a -> bool) -> 'a t -> bool

(** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)
val for_all : ('a -> bool) -> 'a t -> bool

(** Output signature of the functor {!SetList.Make} *)
module type S = sig
  (** The type of the set elements. *)
  type elt

  (** The type of sets. *)
  type t

  (** Printing function *)
  val print
    :  ?first:(unit, Format.formatter, unit) format
    -> ?sep:(unit, Format.formatter, unit) format
    -> ?last:(unit, Format.formatter, unit) format
    -> (Format.formatter -> elt -> unit)
    -> Format.formatter
    -> t
    -> unit

  (** The empty set. *)
  val empty : t

  (** Test whether a set is empty or not. *)
  val is_empty : t -> bool

  (** [mem x s] tests whether [x] belongs to the set [s]. *)
  val mem : elt -> t -> bool

  (** Conversion from a list (unsafe operation) *)
  val of_list : elt list -> t

  (** Conversion to a list *)
  val to_list : t -> elt list

  (** [singleton x] returns the one-element set containing only [x]. *)
  val singleton : elt -> t

  (** [add x s] returns a set containing all elements of [s],
	   plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
  val add : elt -> t -> t

  (** [remove x s] returns a set containing all elements of [s], except
	[x]. If [x] was not in [s], [s] is returned unchanged. *)
  val remove : elt -> t -> t

  val union : t -> t -> t
  val inter : t -> t -> t

  (** Union, intersection and set difference. *)
  val diff : t -> t -> t

  (** Total ordering between sets. Can be used as the ordering function
	for doing sets of sets. *)
  val compare : t -> t -> int

  (** [equal s1 s2] tests whether the sets [s1] and [s2] are
	equal, that is, contain equal elements. *)
  val equal : t -> t -> bool

  (** [subset s1 s2] tests whether the set [s1] is a subset of
	the set [s2]. *)
  val subset : t -> t -> bool

  (** [iter f s] applies [f] in turn to all elements of [s].
	The order in which the elements of [s] are presented to [f]
	is in ascending order. *)
  val iter : (elt -> unit) -> t -> unit

  (** [fold f s a]
	computes [(f xN ... (f x2 (f x1 a))...)],
	where [x1 ... xN] are the elements of [s].
	The order in which elements of [s] are presented to [f] is
	in ascending order.
	@param f function
	@param s set
	@param a accumulator
	@return the computed accumulator
	@raise Not_found if no fount
      *)
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  (** Idem as [List.fold_X] functions *)
  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a

  (** Return the number of elements of a set. *)
  val cardinal : t -> int

  (** Return the list of all elements of the given set.  The returned list
	is sorted in increasing order. *)
  val elements : t -> elt list

  (** Return the smallest element of the given set (with respect to the
	[Ord.compare] ordering), or raise [Not_found] if the set is empty. *)
  val min_elt : t -> elt

  (** Same as [min_elt], but returns the largest element of the given
	set. *)
  val max_elt : t -> elt

  (** Return one element of the given set, or raise [Not_found] if the set
	is empty. Which element is chosen is unspecified, but equal elements
	will be chosen for equal sets. *)
  val choose : t -> elt

  (** [filter p s] returns the set of all elements in [s]
	that satisfy predicate [p]. *)
  val filter : (elt -> bool) -> t -> t

  (** [partition p s] returns a pair of sets [(s1, s2)], where [s1] is the
	set of all the elements of [s] that satisfy the predicate [p], and [s2]
	is the set of all the elements of [s] that do not satisfy [p]. *)
  val partition : (elt -> bool) -> t -> t * t

  (** [exists p s] checks if at least one element of
	the set satisfies the predicate [p]. *)
  val exists : (elt -> bool) -> t -> bool

  (** [for_all p s] checks if all elements of the set satisfy the predicate
	[p]. *)
  val for_all : (elt -> bool) -> t -> bool
end

(** Functor building an implementation of the SetList structure
   given a totally ordered type. *)
module Make (Ord : Set.OrderedType) : S with type elt = Ord.t
