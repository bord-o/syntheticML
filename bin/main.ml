open SyntheticML

let answer = Lang.top_down_search Lang.test_examples |> Option.get
let () = Printf.printf "answer: %s\n" (Lang.show_expr answer)
