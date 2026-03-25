(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Fixpoint analysis of a dynamically explored equation system *)

open Format
open FixpointType

(*  ============================================================= *)
(*  Utilities *)
(*  ============================================================= *)

let add_vertex
      (manager : ('vertex, 'hedge, 'abstract, 'arc) manager)
      (graph : ('vertex, 'hedge, 'abstract, 'arc) graph)
      (vertex : 'vertex)
  : unit
  =
  let bottom = manager.bottom vertex in
  let attr = { reach = bottom; diff = bottom; empty = true } in
  PSHGraph.add_vertex graph vertex attr
;;

let add_hedge
      (manager : ('vertex, 'hedge, 'abstract, 'arc) manager)
      (graph : ('vertex, 'hedge, 'abstract, 'arc) graph)
      (hedge : 'hedge)
      ~(pred : 'vertex array)
      ~(succ : 'vertex)
  : bool
  =
  assert (FixpointStd.is_tvertex graph pred);
  let newsucc =
    if PSHGraph.is_vertex graph succ
    then false
    else (
      add_vertex manager graph succ;
      true)
  in
  let arc = { arc = manager.arc_init hedge; aempty = false } in
  PSHGraph.add_hedge graph hedge arc ~pred ~succ:[| succ |];
  newsucc
;;

(*  ============================================================= *)
(*  Init *)
(*  ============================================================= *)
let init
      compare
      (manager : ('vertex, 'hedge, 'abstract, 'arc) manager)
      (equation : ('vertex, 'hedge) equation)
      (sinit : 'vertex PSette.t)
  : ('vertex, 'hedge, 'abstract, 'arc) graph
  =
  let infodyn =
    { iaddhedge = PHashhe.create_compare compare.PSHGraph.hashh 7; iequation = equation }
  in
  let info =
    { iinit = sinit
    ; itime = ref 0.0
    ; iascending = { nb = 0; stable = false }, []
    ; idescending = { nb = 0; stable = false }, []
    ; iworkvertex = PHashhe.create_compare compare.PSHGraph.hashv 7
    ; iworkhedge = PHashhe.create_compare compare.PSHGraph.hashh 7
    ; iinfodyn = Some infodyn
    }
  in
  let graph = PSHGraph.create compare 31 info in
  PSette.iter
    (fun vertex ->
       let reach = manager.abstract_init vertex in
       let attr = { reach; diff = reach; empty = false } in
       PSHGraph.add_vertex graph vertex attr)
    sinit;
  PSette.iter (FixpointStd.update_workingsets ~hedge:manager.accumulate graph) sinit;
  if manager.print_workingsets
  then fprintf manager.print_fmt "  %a@ " (print_workingsets manager) graph;
  graph
;;

(*  ============================================================= *)
(*  Propagation *)
(*  ============================================================= *)

let propagate
      ~(guided : bool)
      (manager : ('vertex, 'hedge, 'abstract, 'arc) manager)
      (graph : ('vertex, 'hedge, 'abstract, 'arc) graph)
  : bool
  =
  if manager.print_component
  then (
    fprintf manager.print_fmt "* Propagation@ ";
    if manager.dot_fmt <> None then dot_graph manager graph ~title:"Propagation");
  let info = PSHGraph.info graph in
  let dyn =
    match info.iinfodyn with
    | None -> failwith ""
    | Some x -> x
  in
  (* Iteratively adds new hedges and possibly new vertices,
      compute their initial value, and loops until no new
      vertex can be added. *)
  let newsucc = ref [] in
  let newhedge = ref [] in
  (try
     while true do
       if manager.print_postpre
       then
         fprintf
           manager.print_fmt
           "  Iteration@   iaddhegde = %a@ "
           (PHashhe.print manager.print_hedge (fun _ _ -> ()))
           dyn.iaddhedge;
       PHashhe.iter
         (fun hedge (tpredvertex, succvertex) ->
            if FixpointStd.is_tvertex graph tpredvertex
            then (
              let succ_added =
                add_hedge manager graph hedge ~pred:tpredvertex ~succ:succvertex
              in
              if manager.print_postpre
              then
                fprintf
                  manager.print_fmt
                  "  Adding hedge %a %a@ "
                  (Print.array manager.print_vertex)
                  tpredvertex
                  manager.print_hedge
                  hedge;
              if succ_added
              then newsucc := succvertex :: !newsucc
              else newhedge := hedge :: !newhedge;
              PHashhe.replace info.iworkhedge hedge ();
              PHashhe.replace info.iworkvertex succvertex ()))
         dyn.iaddhedge;
       PHashhe.clear dyn.iaddhedge;
       if !newsucc <> []
       then (
         List.iter
           (fun vertex ->
              let svertex =
                { vertex
                ; hedges = PSette.elements (PSHGraph.predhedge graph vertex)
                ; widen = false
                }
              in
              ignore (FixpointStd.process_vertex manager graph ~widening:false svertex))
           !newsucc;
         newsucc := [])
       else raise Exit;
       if manager.print_postpre && manager.dot_fmt <> None
       then dot_graph manager graph ~title:"End Iteration"
     done
   with
   | Exit -> ());
  (* if guided, we have to remove inactive hedge that could become
     active during the fixpoint computation (for instance,
     backedges) *)
  if guided
  then (
    if manager.print_postpre
    then
      fprintf
        manager.print_fmt
        "  newhegde = %a@ "
        (Print.list manager.print_hedge)
        !newhedge;
    List.iter
      (fun hedge ->
         let tpredvertex = PSHGraph.predvertex graph hedge in
         if FixpointStd.is_tvertex graph tpredvertex
         then (
           let succ = PSHGraph.succvertex graph hedge in
           let vertex = succ.(0) in
           let treach = FixpointStd.treach_of_tvertex ~descend:false graph tpredvertex in
           let _, post = manager.apply hedge treach in
           let attrhedge = PSHGraph.attrhedge graph hedge in
           attrhedge.aempty <- manager.is_bottom vertex post;
           if not attrhedge.aempty then PHashhe.replace info.iworkvertex succ.(0) ()))
      !newhedge;
    let toremove =
      PSHGraph.fold_hedge
        graph
        (fun hedge arc ~pred:_ ~succ:_ toremove ->
           if arc.aempty
           then (
             PHashhe.remove info.iworkhedge hedge;
             hedge :: toremove)
           else toremove)
        []
    in
    if manager.print_postpre
    then
      fprintf
        manager.print_fmt
        "  toremove = %a@ "
        (Print.list manager.print_hedge)
        toremove;
    List.iter (PSHGraph.remove_hedge graph) toremove);
  if manager.print_component
  then (
    fprintf manager.print_fmt "* End Propagation@ ";
    if manager.dot_fmt <> None then dot_graph manager graph ~title:"End propagation");
  PHashhe.length info.iworkhedge <> 0
;;

(*  ============================================================= *)
(*  Fixpoint *)
(*  ============================================================= *)

let fixpoint
      ~(guided : bool)
      (manager : ('vertex, 'hedge, 'abstract, 'arc) manager)
      (graph : ('vertex, 'hedge, 'abstract, 'arc) graph)
      make_strategy
  : bool
  =
  let info = PSHGraph.info graph in
  let dyn =
    match info.iinfodyn with
    | None -> failwith ""
    | Some x -> x
  in
  let change = ref true in
  let notstable = ref true in
  while !change do
    change := propagate manager graph ~guided;
    if !change
    then (
      let strategy = make_strategy graph in
      let growing, reducing =
        FixpointStd.process_toplevel_strategy manager graph strategy
      in
      if guided
      then (
        notstable := reducing;
        PHashhe.clear info.iworkvertex;
        PHashhe.clear info.iworkhedge)
      else change := growing || PHashhe.length dyn.iaddhedge > 0)
  done;
  !notstable
;;

let analysis
      compare
      ~(guided : bool)
      (manager : ('vertex, 'hedge, 'abstract, 'arc) manager)
      (equation : ('vertex, 'hedge) equation)
      (sinit : 'vertex PSette.t)
      make_strategy
  : ('vertex, 'hedge, 'abstract, 'arc) output
  =
  if manager.print_analysis then fprintf manager.print_fmt "*** Analysis...@.";
  let graph = init compare manager equation sinit in
  let info = PSHGraph.info graph in
  if not (PSette.is_empty info.iinit)
  then
    Time.wrap_duration_add info.itime (fun () ->
      ignore (fixpoint ~guided manager graph make_strategy));
  if manager.print_analysis then fprintf manager.print_fmt "... end@.";
  if manager.print_analysis && manager.dot_fmt <> None
  then dot_graph manager graph ~title:"Result";
  FixpointStd.output_of_graph graph
;;

(*  ============================================================= *)
(*  Equation from a graph *)
(*  ============================================================= *)

let equation_of_graph ?filter (graph : ('vertex, 'hedge, 'a, 'b, 'c) PSHGraph.t)
  : ('vertex, 'hedge) equation
  =
  fun vertex ->
  let succhedge = PSHGraph.succhedge graph vertex in
  let map =
    match filter with
    | None ->
      PMappe.mapofset
        (fun (hedge : 'hedge) ->
           let predvertex = PSHGraph.predvertex graph hedge in
           let succvertex = PSHGraph.succvertex graph hedge in
           assert (Array.length succvertex = 1);
           predvertex, succvertex.(0))
        succhedge
    | Some _ ->
      PSette.fold
        (fun (hedge : 'hedge) map ->
           let predvertex = PSHGraph.predvertex graph hedge in
           let succvertex = PSHGraph.succvertex graph hedge in
           assert (Array.length succvertex = 1);
           PMappe.add hedge (predvertex, succvertex.(0)) map)
        succhedge
        (PMappe.empty graph.PSHGraph.compare.PSHGraph.compareh)
  in
  map
;;

let graph_of_equation
      compare
      ?filter
      ~(make_attrvertex : 'vertex -> 'attr)
      ~(make_attrhedge : 'hedge -> 'arc)
      ~(info : 'info)
      (equation : ('vertex, 'hedge) equation)
      (sinit : 'vertex PSette.t)
  : ('vertex, 'hedge, 'attr, 'arc, 'info) PSHGraph.t
  =
  let graph = PSHGraph.create compare 9 info in
  let rec explore vertex =
    PSHGraph.add_vertex graph vertex (make_attrvertex vertex);
    let succhedge = equation vertex in
    PMappe.iter
      (fun succhedge (tpredvertex, succvertex) ->
         let allpredcreated =
           Array.fold_left
             (fun res predvertex -> res && PSHGraph.is_vertex graph predvertex)
             true
             tpredvertex
         in
         if allpredcreated
         then (
           let takeit =
             match filter with
             | None -> true
             | Some p -> p succhedge
           in
           if takeit
           then (
             if not (PSHGraph.is_vertex graph succvertex) then explore succvertex;
             if not (PSHGraph.is_hedge graph succhedge)
             then
               PSHGraph.add_hedge
                 graph
                 succhedge
                 (make_attrhedge succhedge)
                 ~pred:tpredvertex
                 ~succ:[| succvertex |])))
      succhedge
  in
  PSette.iter explore sinit;
  graph
;;
