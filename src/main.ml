open Lib

(* TODO: git push hook for ocamlformat *)

(* TODO: two modes
  - help flag
  - read file flag
      - parse and eval
      - print final result
        - no print command so only the final result gets printed
      - maybe add a print command for output?
      - mode to deparse to R
  - interactive mode / repl (no flags)
      - directives:
          - help
          - to R
          - quit
*)

(* TODO: tests for repl? *)

let run_once env =
  Stdlib.print_string "> " ;
  let input = Stdlib.read_line () in
  try
    let exp = Parse.parse input in
    let env', result = Eval.eval env exp in
    Stdlib.print_endline @@ Expr.show_val result ;
    (* return the new environment *)
    env'
  with e ->
    ( match e with
    | Parse.Parse_error msg -> Printf.eprintf "Error%s\n" msg
    | Eval.Type_error { expected; received } ->
        Printf.eprintf "Error: expected type %s but received %s\n" (Expr.show_type expected)
          (Expr.show_type received)
    | e ->
        let msg = Eval.excptn_to_string e in
        Printf.eprintf "Error: %s\n" msg ) ;
    (* return the old environment *)
    env

let repl () =
  let rec loop env =
    let env' = run_once env in
    (loop [@tailcall]) env' in

  let env = Expr.Env.empty in
  Stdlib.print_endline "Welcome to the RhoVec REPL. CTRL+D exits." ;
  try loop env with End_of_file -> Stdlib.print_endline "\nGoodbye!"

let () = repl ()
