type op = Add | Sub | Mult | Div [@@deriving show]

type expr = X | Y | Binop of expr option * op option * expr option
[@@deriving show]

let test_prog = Binop (Some X, Some Add, Some Y)

type example = { x : int; y : int; answer : int }

let test_examples = [ { x = 3; y = 4; answer = 80 } ]
let bad_prog = Binop (Some (Binop (None, None, None)), Some Add, Some Y)
let search_depth = 7
let safe_div l r = try l / r with _ -> 0

let rec eval x y = function
  | X -> x
  | Y -> y
  | Binop (Some e1, Some Add, Some e2) -> eval x y e1 + eval x y e2
  | Binop (Some e1, Some Sub, Some e2) -> eval x y e1 - eval x y e2
  | Binop (Some e1, Some Mult, Some e2) -> eval x y e1 * eval x y e2
  | Binop (Some e1, Some Div, Some e2) -> safe_div (eval x y e1) (eval x y e2)
  | _ -> failwith "incomplete"

let is_correct (expr : expr) (example : example) =
  eval example.x example.y expr = example.answer

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
        print_endline (Printf.sprintf "Unmatched: %s" (show_expr e));
        []

let top_down_search examples =
  let worklist = Queue.create () in
  Queue.add (Binop (None, None, None)) worklist;
  Queue.add X worklist;
  Queue.add Y worklist;
  let rec loop () =
    if not @@ Queue.is_empty worklist then (
      let t = Queue.take worklist in
      if t |> is_complete && examples |> List.for_all (is_correct t) then Some t
      else
        let new_terms = get_new_terms t in
        new_terms
        |> List.iteri (fun i t' ->
               Printf.printf "Enumerating expr #%i: \n%s\n\n" i (show_expr t');
               if is_complete t' then
                 Printf.printf "Found complete program: %s\n" (show_expr t')
               else ();
               Queue.add t' worklist);
        loop ())
    else failwith "worklist empty"
  in
  loop ()

(*
proc Top-Down-Search
  worklist = Num _
  while not empty worklist
    t = wl.dequeue
    if t is a valid program
      if t works on all examples
        return t
    else
      for t' in New-Terms do
        wl.enqueue t'

proc New-Terms (returns the number of programs corresponding to number of productions)
  A = left-most non-terminal in t
  for all productions from A
    t' = t with production
    if t' is within bounds then
      yield t'
*)
