open Lib
open Util

(* TODO: git push hook for ocamlformat *)
(* TODO: code coverage in CI? *)

(* TODO:
    - file mode: read and execute RhoVec file, or translate it to R
    - also need a flag for help
*)

let repl_help () =
  Stdlib.print_endline "Enter a RhoVec expression to be evaluated." ;
  Stdlib.print_endline "Type '#h' to print this message." ;
  Stdlib.print_endline "Type '#r [exp]' to translate exp to R syntax." ;
  Stdlib.print_endline "Type '#q' or CTRL+D to quit."

let run_once env =
  let handle_directive input =
    if input = "#h" then repl_help ()
    else if String.prefix ~pre:"#r " input then
      let rv_input = Option.get @@ String.chop_prefix ~pre:"#r " input in
      try
        let exp = Parse.parse rv_input in
        Stdlib.print_endline @@ Deparse.to_r exp
      with Parse.Parse_error msg -> Printf.printf "Error%s\n" msg
    else if input = "#q" then raise End_of_file
    else Printf.printf "Error: unknown directive\n" in

  let run input =
    try
      let exp = Parse.parse input in
      let env', result = Eval.eval env exp in
      Stdlib.print_endline @@ Expr.show_val result ;
      (* return the new environment *)
      env'
    with e ->
      (match e with
      | Parse.Parse_error msg -> Printf.printf "Error%s\n" msg
      | Eval.Type_error { expected; received } ->
          Printf.printf "Error: expected type %s but received %s\n" (Expr.show_type expected)
            (Expr.show_type received)
      | e ->
          let msg = Eval.excptn_to_string e in
          Printf.printf "Error: %s\n" msg) ;
      (* return the old environment *)
      env in

  Stdlib.print_string "> " ;
  let input = Stdlib.read_line () in
  if String.prefix ~pre:"#" input then (
    handle_directive input ;
    env)
  else run input

let repl () =
  let rec loop env =
    let env' = run_once env in
    (loop [@tailcall]) env' in

  let env = Expr.Env.empty in
  Stdlib.print_endline "Welcome to the RhoVec REPL.\n" ;
  repl_help () ;
  try loop env with End_of_file -> Stdlib.print_endline "\nGoodbye!"

let () = repl ()
