open Lib

(* TODO: two modes
  - read file
      - parse and eval
      - print final result
        - no print command so only the final result gets printed
      - maybe add a print command for output?
      - mode to deparse to R
  - interactive mode / repl
      - read a line of input, eval and print it
      - add some mode to deparse to R
*)

let () =
  let env = Expr.Env.empty in
  try
    let line = Stdlib.read_line () in
    let exp = Parse.parse line in
    let _, result = Eval.eval env exp in
    print_endline @@ Expr.show_val result
  with
  | Parse.Parse_error msg -> Printf.printf "Parse error%s\n" msg
  | Eval.Type_error { expected; received } ->
      Printf.printf "Eval error: expected type %s but received %s\n" (Expr.show_type expected)
        (Expr.show_type received)
  | e ->
      let msg = Eval.excptn_to_string e in
      Printf.printf "Eval error: %s\n" msg

(*
  let str = "Combine(1, -Combine(-2, -3), 4, 5, \n NA_i);" in
  let expr = Parse.parse str in
  let res = Eval.run expr in
  Format.printf "Input:\n%S\n\n" str ;
  Format.printf "Parsed expression:\n%s\n\n" (Expr.show_expression expr) ;
  Format.printf "Evaluated result:\n%s\n\n" (Expr.show_val res) ;
  print_endline "Nothing else for now, maybe you meant to run the tests?"
*)
