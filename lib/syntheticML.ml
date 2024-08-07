open Lang

let search_depth = 4

let is_correct (expr : expr) (example : example) =
  match eval example.x example.y expr with
  | Some a -> a = example.answer
  | None -> false

let is_terminal = function X | Y -> true | _ -> false

let rec is_complete (prog : expr) =
  match prog with
  | X | Y -> true
  | Binop (Some a, Some _, Some c) ->
      if is_complete a && is_complete c then true else false
  | _ -> false

let rec depth = function
  | X | Y | Binop (None, _, None) -> 1
  | Binop (Some e1, op, Some e2) -> 1 + max (depth e1) (depth e2)
  | Binop (None, _, Some e2) -> 1 + depth e2
  | Binop (Some e1, _, None) -> 1 + depth e1

(* let expr_in_worklist (expr : expr) (wl : expr Queue.t) (example : example) = *)
(* let ans = eval example.x example.y expr in *)
(* let found = *)
(* wl |> Queue.to_seq |> List.of_seq *)
(* |> List.map (fun e -> (e, eval example.x example.y e)) *)
(* |> List.filter (fun (e, ans) -> Option.is_some ans) *)
(* |> List.map (fun (_, ans) -> Option.get ans) *)
(* in *)
(* match ans with None -> false | Some a -> found |> List.mem a *)

let rec get_new_terms t =
  (* TODO: this is only taking one step, but it needs to loop until hitting the depth limit*)
  (* to do this, use an aux func with a depth icrementer, and keep calling it on the left most non terminal (this is built in rn)*)
  (* TODO: could this be a simple fold at the call site instead? *)
  if depth t = search_depth then []
  else
    match t with
    | Binop (None, op, e2) ->
        [
          Binop (Some X, op, e2);
          Binop (Some Y, op, e2);
          Binop (Some (Binop (None, None, None)), op, e2);
        ]
    | Binop (e1, op, None) ->
        [
          Binop (e1, op, Some X);
          Binop (e1, op, Some Y);
          Binop (e1, op, Some (Binop (None, None, None)));
        ]
    | Binop (e1, None, e2) ->
        [
          Binop (e1, Some Add, e2);
          Binop (e1, Some Sub, e2);
          Binop (e1, Some Mult, e2);
          Binop (e1, Some Div, e2);
        ]
    | Binop (Some e1, op, e2) when not @@ is_complete e1 ->
        let subproblem = get_new_terms e1 in
        List.map (fun e' -> Binop (Some e', op, e2)) subproblem
    | Binop (e1, op, Some e2) when not @@ is_complete e2 ->
        let subproblem = get_new_terms e2 in
        List.map (fun e' -> Binop (e1, op, Some e')) subproblem
    | e ->
        (* print_endline (Printf.sprintf "Unmatched: %s" (show_expr e)); *)
        []

let is_feasible (prog : expr) =
  (* Here we can add feasibility checks to prune the search space as we go *)
  if prog |> is_complete then true else true

let top_down_search examples =
  let worklist = Queue.create () in
  Queue.add (Binop (None, None, None)) worklist;
  Queue.add X worklist;
  Queue.add Y worklist;
  let rec loop () =
    (* OCaml Queue's are fifo so this is doing breadth first by default *)
    if not @@ Queue.is_empty worklist then
      let t = Queue.take worklist in
      if t |> is_feasible then (
        if examples |> List.for_all (is_correct t) then Some t
        else
          let new_terms = get_new_terms t in

          if List.length new_terms > 0 then
            Printf.printf "\nNew Enumerations: %i, Worklist Entries: %i\n"
              (List.length new_terms) (Queue.length worklist);
          new_terms
          |> List.iteri (fun i t' ->
                 Printf.printf "%s\n" (show_expr t');
                 (* if is_complete t' then *)
                 (* Printf.printf "Found complete program: %s\n" (show_expr t') *)
                 (* else (); *)
                 Queue.add t' worklist);
          loop ())
      else loop ()
    else failwith "worklist empty"
  in
  loop ()
