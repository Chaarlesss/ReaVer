type timemeas_t =
  { mutable running : bool
  ; mutable starttime : float
  ; mutable accutime : float
  }

let create () = { running = false; starttime = 0.0; accutime = 0.0 }

let start t =
  if not t.running
  then (
    t.running <- true;
    t.starttime <- Sys.time ())
;;

let stop t =
  if t.running
  then (
    t.running <- false;
    t.accutime <- t.accutime +. (Sys.time () -. t.starttime))
;;

let reset t =
  t.running <- false;
  t.starttime <- 0.0;
  t.accutime <- 0.0
;;

let print fmt t =
  if not t.running
  then Format.pp_print_float fmt t.accutime
  else Format.pp_print_float fmt (t.accutime +. (Sys.time () -. t.starttime))
;;
