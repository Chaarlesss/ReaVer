(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Fixpoint analysis of an equation system: inference of thresholds *)

(*  ********************************************************************** *)
(** {2 Public datatypes} *)
(*  ********************************************************************** *)

open FixpointType

type ('vertex, 'hedge, 'threshold) parameter =
  { mutable compare : 'threshold -> 'threshold -> int
  ; mutable print : Format.formatter -> 'threshold -> unit
  ; mutable init : 'vertex -> 'threshold PSette.t
  ; mutable apply : 'hedge -> 'threshold PSette.t array -> 'threshold PSette.t
  ; mutable iteration_nb : int
  }

let make_threshold_manager
      (manager : ('a, 'b, 'c, 'd) FixpointType.manager)
      (parameter : ('a, 'b, 'e) parameter)
  : ('a, 'b, 'e PSette.t, unit) FixpointType.manager
  =
  { manager with
    bottom = (fun _ -> failwith "")
  ; canonical = (fun _ _ -> ())
  ; is_bottom = (fun _ set -> PSette.is_empty set)
  ; is_leq = (fun _ set1 set2 -> PSette.subset set1 set2)
  ; join = (fun _ set1 set2 -> PSette.union set1 set2)
  ; join_list =
      (fun _ lset -> List.fold_left PSette.union (PSette.empty parameter.compare) lset)
  ; widening = (fun _ _ _ -> failwith "")
  ; odiff = Some (fun _ set1 set2 -> PSette.diff set1 set2)
  ; abstract_init = parameter.init
  ; FixpointType.arc_init = (fun _ -> ())
  ; FixpointType.apply = (fun h tabs -> (), parameter.apply h tabs)
  ; print_abstract = (fun fmt set -> PSette.print parameter.print fmt set)
  ; print_arc = (fun _ () -> ())
  ; accumulate = true
  }
;;

(*  ********************************************************************** *)
(** {2 Process the toplevel strategy} *)
(*  ********************************************************************** *)

let process_main_strategy
      (manager : ('vertex, 'hedge, 'abstract, unit) FixpointType.manager)
      (graph : ('vertex, 'hedge, 'abstract, unit) FixpointType.graph)
      ((_, sstrategy) : ('vertex, 'hedge) FixpointType.strategy)
      (iteration_nb : int)
  : unit
  =
  let info = PSHGraph.info graph in
  let process_vertex strategy_vertex =
    if PHashhe.mem info.iworkvertex strategy_vertex.vertex
    then FixpointStd.process_vertex manager graph ~widening:false strategy_vertex
    else false
  in
  List.iter
    (fun elt ->
       match elt with
       | Ilist.Atome strategy_vertex -> ignore (process_vertex strategy_vertex)
       | Ilist.List strategy ->
         (try
            let growing = ref false in
            let revstrategy = Ilist.rev strategy in
            for i = 1 to iteration_nb do
              growing := false;
              Ilist.iter
                (fun _ _ strategy_vertex ->
                   let change = process_vertex strategy_vertex in
                   growing := !growing || change)
                (if i mod 2 = 1 then strategy else revstrategy);
              if not !growing then raise Exit
            done
          with
          | Exit -> ());
         ())
    sstrategy;
  ()
;;

let inference
      (manager : ('vertex, 'hedge, 'abstract, 'arc) FixpointType.manager)
      (parameter : ('vertex, 'hedge, 'threshold) parameter)
      (input : ('vertex, 'hedge, 'a, 'b, 'c) PSHGraph.t)
      strategy
  : ('vertex, 'threshold PSette.t) PHashhe.t
  =
  let manager = make_threshold_manager manager parameter in
  let comparev = input.PSHGraph.compare.PSHGraph.comparev in
  let hashv = input.PSHGraph.compare.PSHGraph.hashv in
  let (sinit : 'vertex PSette.t) =
    PSHGraph.fold_vertex
      input
      (fun vtx _ ~pred:_ ~succ:_ set -> PSette.add vtx set)
      (PSette.empty comparev)
  in
  let graph = FixpointStd.init manager input sinit in
  process_main_strategy manager graph strategy parameter.iteration_nb;
  let res = PHashhe.create_compare hashv 23 in
  Ilist.iter
    (fun _ _ svertex ->
       if svertex.widen
       then (
         let attr = PSHGraph.attrvertex graph svertex.vertex in
         PHashhe.add res svertex.vertex attr.reach))
    strategy;
  res
;;
