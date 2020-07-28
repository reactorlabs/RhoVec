open Expr

let run_test test =
  let id, expr, expected = test in
  let res = Eval.eval expr in
  if res <> expected then begin
    print_endline ("Test " ^ string_of_int id ^ " failed!");
    assert false
  end

let tests = [
  ( 1, Lit 0, int_vec [0]);
  ( 2, Combine [Lit 42], int_vec [42]);
  ( 3, Combine [Lit 1; Lit 2; Lit 3], int_vec [1; 2; 3]);
  ( 4, Combine [Lit 5; (Combine [Lit 6; Lit 7; (Combine [Lit 8]); Lit 9])],
       int_vec [5; 6; 7; 8; 9]);
]

let () =
  List.iter run_test tests;
  print_endline "All tests passed!"
