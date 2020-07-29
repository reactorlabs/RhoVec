open Expr

exception Test_failed of int

let run_test_pos test =
  let id, expr, expected = test in
  let res = Eval.eval expr in
  if res <> expected then raise (Test_failed id)

let run_test_neg test =
  let id, expr, excptn = test in
  let raise_exn n = raise (Test_failed n) in
  try ignore (Eval.eval expr); raise_exn id with
  | e -> if e <> excptn then raise_exn id

let tests_pos = [
  ( 1, Lit 0, vec_of_int 0);
  ( 2, Combine [Lit 42], vec_of_int 42);
  ( 3, Combine [Lit 1; Lit 2; Lit 3], vec_of_intlist [1; 2; 3]);
  ( 4, Combine [Lit 5; (Combine [Lit 6; Lit 7; (Combine [Lit 8]); Lit 9])],
       vec_of_intlist [5; 6; 7; 8; 9]);
  ( 5, Subset1_Nothing (Lit 2), vec_of_int 2);
  ( 6, Subset1_Nothing (Combine [Lit 3; Lit 1; Lit 4]), vec_of_intlist [3; 1; 4]);
  ( 7, Subset1_Nothing (Combine [Lit 5; (Combine [Lit 6; Lit 7])]),
       vec_of_intlist [5; 6; 7]);
  ( 8, Subset2 (Combine [Lit 1; Lit 2; Lit 3], Lit 1), vec_of_int 1);
  ( 9, Subset2 (Combine [Lit 1; Lit 2; Lit 3], Lit 2), vec_of_int 2);
  (10, Subset2 (Combine [Lit 1; Lit 2; Lit 3], Lit 3), vec_of_int 3);
  (11, Subset2 (Combine [Lit 7; Lit 6; Lit 5], Combine [Lit 1]), vec_of_int 7);
  (12, Subset2 (Combine [Lit 7; Lit 6; Lit 5], Combine [Lit 2]), vec_of_int 6);
  (13, Subset2 (Combine [Lit 7; Lit 6; Lit 5], Combine [Lit 3]), vec_of_int 5);
  (13, Subset1 (Combine [Lit 7; Lit 6; Lit 5], Lit 0), empty_intvec);
  (14, Subset1 (Combine [Lit 7; Lit 6; Lit 5], Combine [Lit 0]), empty_intvec);
  (15, Subset1 (Lit 7, Combine [Lit 0]), empty_intvec);
]

let tests_neg = [
  (1001, Subset2 (Combine [Lit 1; Lit 2; Lit 3], Lit 0),
         Eval.Selecting_lt_one_element);
  (1002, Subset2 (Combine [Lit 1; Lit 2; Lit 3], Lit 4),
         Eval.Subscript_out_of_bounds);
  (1003, Subset2 (Combine [Lit 1; Lit 2; Lit 3], Lit 5),
         Eval.Subscript_out_of_bounds);
  (1004, Subset2 (Combine [Lit 7; Lit 6; Lit 5], Combine [Lit 0]),
         Eval.Selecting_lt_one_element);
  (1005, Subset2 (Combine [Lit 7; Lit 6; Lit 5], Combine [Lit 4]),
         Eval.Subscript_out_of_bounds);
  (1006, Subset2 (Combine [Lit 7; Lit 6; Lit 5], Combine [Lit 5]),
         Eval.Subscript_out_of_bounds);
  (1007, Subset2 (Combine [Lit 7; Lit 6; Lit 5], Combine []),
         Eval.Selecting_lt_one_element);
  (1008, Subset2 (Combine [Lit 7; Lit 6; Lit 5], Combine [Lit 5; Lit 6]),
         Eval.Selecting_gt_one_element);
  (1009, Subset2 (Combine [Lit 7; Lit 6; Lit 5], Combine [Lit (-1)]),
         Eval.Selecting_gt_one_element);
  (1010, Subset2 (Combine [], Combine [Lit 1]),
         Eval.Subscript_out_of_bounds);
]

let () =
  List.iter run_test_pos tests_pos;
  List.iter run_test_neg tests_neg;
  print_endline "All tests passed!"
