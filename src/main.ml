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

  (* subset1 logical *)
  (33, Subset1 (Combine [int_exp 1; int_exp 2; int_exp 3; int_exp 4],
                Combine [true_exp; false_exp; false_exp; true_exp]),
       vec_of_intlist [1; 4]);
  (34, Subset1 (Combine [int_exp 1; int_exp 2; int_exp 3; int_exp 4],
                Combine [true_exp; true_exp; true_exp; true_exp]),
       vec_of_intlist [1; 2; 3; 4]);
  (35, Subset1 (Combine [int_exp 1; int_exp 2; int_exp 3; int_exp 4],
               Combine [false_exp; false_exp; false_exp; false_exp]),
       empty_vec Int);
  (36, Subset1 (Combine [true_exp; true_exp; false_exp; false_exp; true_exp],
                Combine [true_exp; false_exp; false_exp; true_exp; true_exp]),
       vec_of_boollist [true; false; true]);
  (37, Subset1 (Combine [true_exp; true_exp; false_exp; false_exp; true_exp],
                Combine [true_exp; true_exp; true_exp; true_exp; true_exp]),
       vec_of_boollist [true; true; false; false; true]);
  (38, Subset1 (Combine [true_exp; true_exp; false_exp; false_exp; true_exp],
                Combine [false_exp; false_exp; false_exp; false_exp;
                         false_exp]),
       empty_vec Bool);

  (* NA lit *)
  (39, na_exp Int, vec_of_lit NA_int);
  (40, na_exp Bool, vec_of_lit NA_bool);

  (* Combine with NA *)
  (41, Combine [na_exp Int], vec_of_lit NA_int);
  (42, Combine [na_exp Bool], vec_of_lit NA_bool);
  (43, Combine [int_exp 5; (Combine [int_exp 6; int_exp 7;
                                    (Combine [na_exp Int]); int_exp 9])],
       Vector ([| int_lit 5; int_lit 6; int_lit 7; na_lit Int; int_lit 9 |],
               Int));
  (44, Combine [true_exp; (Combine [true_exp; false_exp;
                                    (Combine [na_exp Bool]); true_exp])],
       Vector ([| true_lit; true_lit; false_lit; na_lit Bool; true_lit |],
               Bool));

  (* subset1 nothing with NA *)
  (45, Subset1_Nothing (Combine [int_exp 3; int_exp 1; na_exp Int]),
       Vector ([| int_lit 3; int_lit 1; na_lit Int |], Int));
  (46, Subset1_Nothing (Combine [true_exp; true_exp; na_exp Bool]),
       Vector ([| true_lit; true_lit; na_lit Bool |], Bool));

  (* subset2 with NA *)
  (47, Subset2 (Combine [na_exp Int; int_exp 2; int_exp 3], int_exp 1),
       vec_of_lit (na_lit Int));
  (48, Subset2 (Combine [na_exp Bool; true_exp; false_exp], int_exp 1),
       vec_of_lit (na_lit Bool));

  (* subset1 zero with NA *)
  (49, Subset1 (Combine [na_exp Bool; na_exp Bool; na_exp Bool],
                Combine [int_exp 0]),
       empty_vec Bool);
  (50, Subset1 (Combine [na_exp Int; na_exp Int; na_exp Int],
                Combine [int_exp 0]),
       empty_vec Int);

  (* subset1 logical with NA *)
  (51, Subset1 (Combine [int_exp 1; int_exp 2; int_exp 3; int_exp 4],
                Combine [true_exp; na_exp Bool; false_exp; true_exp]),
       Vector ([| int_lit 1; na_lit Int; int_lit 4 |], Int));
  (52, Subset1 (Combine [true_exp; true_exp; false_exp; false_exp; true_exp],
                Combine [true_exp; false_exp; true_exp; na_exp Bool; true_exp]),
       Vector ([| true_lit; false_lit; na_lit Bool; true_lit |], Bool));

  (* subset1 positive *)
  (53, Subset1 (Combine [int_exp 11; int_exp 12; int_exp 13; int_exp 14],
                int_exp 1),
       vec_of_intlist [11]);
  (54, Subset1 (Combine [int_exp 11; int_exp 12; int_exp 13; int_exp 14],
                int_exp 2),
       vec_of_intlist [12]);
  (55, Subset1 (Combine [int_exp 11; int_exp 12; int_exp 13; int_exp 14],
                int_exp 3),
       vec_of_intlist [13]);
  (56, Subset1 (Combine [int_exp 11; int_exp 12; int_exp 13; int_exp 14],
                int_exp 4),
       vec_of_intlist [14]);
  (57, Subset1 (Combine [int_exp 11; int_exp 12; int_exp 13; int_exp 14],
                int_exp 5),
       Vector ([| na_lit Int |], Int));
  (58, Subset1 (Combine [int_exp 11; int_exp 12; int_exp 13; int_exp 14],
                Combine [int_exp 1]),
       vec_of_intlist [11]);
  (59, Subset1 (Combine [int_exp 11; int_exp 12; int_exp 13; int_exp 14],
                Combine [int_exp 1; int_exp 3]),
       vec_of_intlist [11; 13]);
  (60, Subset1 (Combine [int_exp 11; int_exp 12; int_exp 13; int_exp 14],
                Combine [int_exp 0; int_exp 1; int_exp 3; int_exp 5]),
       Vector ([| na_lit Int; int_lit 11; int_lit 13; na_lit Int |], Int));
  (61, Subset1 (Combine [int_exp 11; int_exp 12; int_exp 13; int_exp 14],
                Combine [int_exp 3; int_exp 3; int_exp 2; int_exp 4]),
       vec_of_intlist [13; 13; 12; 14]);
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

  (* subset2 with NA errors *)
  (1015, Subset2 (Subset1 (int_exp 1, int_exp 0), na_exp Int),
         Eval.Subscript_out_of_bounds);
  (1016, Subset2 (Subset1 (int_exp 1, int_exp 0), na_exp Bool),
         Eval.Type_error { expected = Int; received = Bool });
  (1017, Subset2 (Subset1 (int_exp 1, int_exp 0), Combine [na_exp Int]),
         Eval.Subscript_out_of_bounds);
]

let () =
  List.iter run_test_pos tests_pos;
  List.iter run_test_neg tests_neg;
  print_endline "All tests passed!"
