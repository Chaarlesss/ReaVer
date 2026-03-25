(** Standard statistics functions *)

(** Returns the mean of the array. *)
val mean : float array -> float

(** Given the mean, returns the variance of the array. *)
val variance : float -> float array -> float

(** Given the mean, returns the standard deviation of the array. *)
val std_deviation : float -> float array -> float
