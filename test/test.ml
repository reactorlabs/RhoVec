(* open Lib *)
(* open Expr *)

module A = Alcotest

let empty_test () = ()
let test_int () = A.(check int) "same int" 1 2

let () = A.run "packagename" [
  "testsuite1", [
    A.test_case "test1" `Quick empty_test ;
    A.test_case "test2" `Quick test_int ;
  ];
]
