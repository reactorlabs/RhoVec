open Expr

let () =
  let e1 = Combine [(Lit 1); (Lit 2); (Lit 3)] in
  let res = Eval.eval e1 in
  assert (res = Vector ([| 1; 2; 3 |], Int))
