type op = Add | Sub | Mult | Div [@@deriving show]

type expr = X | Y | Binop of expr option * op option * expr option
[@@deriving show]

let test_prog = Binop (Some X, Some Add, Some Y)

type example = { x : int; y : int; answer : int }

let test_examples = [ { x = 3; y = 4; answer = 80 } ]
let bad_prog = Binop (Some (Binop (None, None, None)), Some Add, Some Y)
let safe_div l r = try l / r with _ -> 0

let rec eval x y = function
  | X -> Some x
  | Y -> Some y
  | Binop (Some e1, Some Add, Some e2) ->
      Option.bind (eval x y e1) @@ fun a ->
      Option.bind (eval x y e2) @@ fun b -> Some (a + b)
  | Binop (Some e1, Some Sub, Some e2) ->
      Option.bind (eval x y e1) @@ fun a ->
      Option.bind (eval x y e2) @@ fun b -> Some (a - b)
  | Binop (Some e1, Some Mult, Some e2) ->
      Option.bind (eval x y e1) @@ fun a ->
      Option.bind (eval x y e2) @@ fun b -> Some (a * b)
  | Binop (Some e1, Some Div, Some e2) ->
      Option.bind (eval x y e1) @@ fun a ->
      Option.bind (eval x y e2) @@ fun b -> Some (safe_div a b)
  | _ -> None
