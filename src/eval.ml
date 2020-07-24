open Expr

let rec eval = function
  | Lit i -> Vector ([| i |], Int)
  | Combine es ->
    let eval_extract e = match (eval e) with
    | Vector (a, _) -> a in
    let vec = Array.concat (List.map eval_extract es) in
    Vector (vec, Int)
