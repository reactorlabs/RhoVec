open Lib

module A = Alcotest

let testable_expr = A.testable Expr.pp_expression Expr.equal_expression

let test_parse desc (expected, str) =
  let run_parser () =
    let ast = Parser.parse str in
    A.(check testable_expr) "same expression" expected ast in
  A.test_case desc `Quick run_parser

let () =
  let open Expr in
  A.run "parse testsuite" [ ("foo", [ test_parse "int" (int_exp 42, "42") ]) ]

(* dune runtest --> dune build @runtest *)
(* parse alias test/runtest *)
(* test alias test/runtest *)
