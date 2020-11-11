open Lib

(* TODO:
  - interactive mode / repl
*)

let () =
  let str = "Combine(1, -Combine(-2, -3), 4, 5, \n NA_i);" in
  let expr = Parse.parse str in
  let res = Eval.run expr in
  Format.printf "Input:\n%S\n\n" str ;
  Format.printf "Parsed expression:\n%s\n\n" (Deparse.to_r expr) ;
  Format.printf "Evaluated result:\n%s\n\n" (Expr.show_val res) ;
  print_endline "Nothing else for now, maybe you meant to run the tests?"
