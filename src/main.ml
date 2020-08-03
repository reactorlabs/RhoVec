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
  (* int lit *)
  ( 1, int_exp 0, vec_of_int 0);

  (* combine for ints *)
  ( 2, Combine [int_exp 42], vec_of_int 42);
  ( 3, Combine [int_exp 1; int_exp 2; int_exp 3], vec_of_intlist [1; 2; 3]);
  ( 4, Combine [int_exp 5; (Combine [int_exp 6; int_exp 7;
                                    (Combine [int_exp 8]); int_exp 9])],
       vec_of_intlist [5; 6; 7; 8; 9]);

  (* subset1 nothing for ints *)
  ( 5, Subset1_Nothing (int_exp 2), vec_of_int 2);
  ( 6, Subset1_Nothing (Combine [int_exp 3; int_exp 1; int_exp 4]),
       vec_of_intlist [3; 1; 4]);
  ( 7, Subset1_Nothing (Combine [int_exp 5; (Combine [int_exp 6; int_exp 7])]),
       vec_of_intlist [5; 6; 7]);

  (* subset2 for ints *)
  ( 8, Subset2 (Combine [int_exp 1; int_exp 2; int_exp 3], int_exp 1),
       vec_of_int 1);
  ( 9, Subset2 (Combine [int_exp 1; int_exp 2; int_exp 3], int_exp 2),
       vec_of_int 2);
  (10, Subset2 (Combine [int_exp 1; int_exp 2; int_exp 3], int_exp 3),
       vec_of_int 3);
  (11, Subset2 (Combine [int_exp 7; int_exp 6; int_exp 5], Combine [int_exp 1]),
       vec_of_int 7);
  (12, Subset2 (Combine [int_exp 7; int_exp 6; int_exp 5], Combine [int_exp 2]),
       vec_of_int 6);
  (13, Subset2 (Combine [int_exp 7; int_exp 6; int_exp 5], Combine [int_exp 3]),
       vec_of_int 5);

  (* subset1 zero for ints *)
  (14, Subset1 (Combine [int_exp 7; int_exp 6; int_exp 5], int_exp 0),
       empty_vec Int);
  (15, Subset1 (Combine [int_exp 7; int_exp 6; int_exp 5], Combine [int_exp 0]),
       empty_vec Int);
  (16, Subset1 (int_exp 7, Combine [int_exp 0]), empty_vec Int);

  (* bool lit *)
  (17, true_exp, vec_of_bool true);

  (* combine for bools *)
  (18, Combine [true_exp], vec_of_bool true);
  (19, Combine [true_exp; false_exp; true_exp],
       vec_of_boollist [true; false; true]);
  (20, Combine [false_exp; (Combine [false_exp; true_exp;
                                    (Combine [false_exp]); true_exp])],
       vec_of_boollist [false; false; true; false; true]);

  (* subset1 nothing for bools *)
  (21, Subset1_Nothing (false_exp), vec_of_bool false);
  (22, Subset1_Nothing (Combine [true_exp; false_exp; false_exp]),
       vec_of_boollist [true; false; false]);
  (23, Subset1_Nothing (Combine [false_exp; (Combine [false_exp; true_exp])]),
       vec_of_boollist [false; false; true]);

  (* subset2 for bools *)
  (24, Subset2 (Combine [true_exp; true_exp; false_exp], int_exp 1),
       vec_of_bool true);
  (25, Subset2 (Combine [true_exp; true_exp; false_exp], int_exp 2),
       vec_of_bool true);
  (26, Subset2 (Combine [true_exp; true_exp; false_exp], int_exp 3),
       vec_of_bool false);
  (27, Subset2 (Combine [false_exp; true_exp; false_exp], Combine [int_exp 1]),
       vec_of_bool false);
  (28, Subset2 (Combine [false_exp; true_exp; false_exp], Combine [int_exp 2]),
       vec_of_bool true);
  (29, Subset2 (Combine [false_exp; true_exp; false_exp], Combine [int_exp 3]),
       vec_of_bool false);

  (* subset1 zero for bools *)
  (30, Subset1 (Combine [false_exp; true_exp; false_exp], int_exp 0),
       empty_vec Bool);
  (31, Subset1 (Combine [false_exp; true_exp; false_exp], Combine [int_exp 0]),
       empty_vec Bool);
  (32, Subset1 (true_exp, Combine [int_exp 0]), empty_vec Bool);
]

let tests_neg = [
  (* subset2 errors *)
  (1001, Subset2 (Combine [int_exp 1; int_exp 2; int_exp 3], int_exp 0),
         Eval.Selecting_lt_one_element);
  (1002, Subset2 (Combine [int_exp 1; int_exp 2; int_exp 3], int_exp 4),
         Eval.Subscript_out_of_bounds);
  (1003, Subset2 (Combine [true_exp; false_exp; false_exp], int_exp 5),
         Eval.Subscript_out_of_bounds);
  (1004, Subset2 (Combine [true_exp; true_exp],
                  Combine [int_exp 0]),
         Eval.Selecting_lt_one_element);
  (1005, Subset2 (Combine [int_exp 7; int_exp 6; int_exp 5],
                  Combine [int_exp 4]),
         Eval.Subscript_out_of_bounds);
  (1006, Subset2 (Combine [false_exp; true_exp; false_exp],
                  Combine [int_exp 5]),
         Eval.Subscript_out_of_bounds);
  (1007, Subset2 (Combine [int_exp 7; int_exp 6; int_exp 5],
                 Subset1 (int_exp 1, int_exp 0)),
         Eval.Selecting_lt_one_element);
  (1008, Subset2 (Combine [true_exp; true_exp; false_exp; false_exp],
                  Combine [int_exp 5; int_exp 6]),
         Eval.Selecting_gt_one_element);
  (1009, Subset2 (Combine [int_exp 7; int_exp 6; int_exp 5],
                  Combine [int_exp (-1)]),
         Eval.Selecting_gt_one_element);
  (1010, Subset2 (Subset1 (int_exp 1, int_exp 0), Combine [int_exp 1]),
         Eval.Subscript_out_of_bounds);
  (1011, Subset2 (Combine [int_exp 1; int_exp 2; int_exp 3], true_exp),
         Eval.Type_error { expected = Int; received = Bool });

  (* combine errors *)
  (1012, Combine [], Eval.Expected_nonempty_vector);
  (1013, Combine [int_exp 0; true_exp; false_exp],
         Eval.Type_error { expected = Int; received = Bool });
  (1014, Combine [false_exp; true_exp; int_exp 1; false_exp],
         Eval.Type_error { expected = Bool; received = Int });
]

let () =
  List.iter run_test_pos tests_pos;
  List.iter run_test_neg tests_neg;
  print_endline "All tests passed!"
