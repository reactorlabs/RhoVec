open Lib
open Expr

(* Big list of TODO:
  - environments
  - assignment
  - various other bugs that were fixed in the semantics
  - rename this project
  - use an actual test framework
  - set up code coverage tools
  - refactor and clean up tests

  - parser
  - test oracle (using ocaml-r?)
  - interactive mode / repl
*)

exception Test_failed of int

exception Unexpected_fail

exception Supposed_to_fail

let run_test_pos test =
  let id, expr, expected = test in
  try if Eval.eval expr <> expected then raise Unexpected_fail with _ -> raise (Test_failed id)

let run_test_neg test =
  let id, expr, excptn = test in
  try
    ignore (Eval.eval expr) ;
    raise Supposed_to_fail
  with e -> if e <> excptn then raise (Test_failed id)

let tests_pos =
  [ (* int lit *)
    (1, int_exp 0, vec_of_int 0)
  ; (* combine for ints *)
    (2, Combine [ int_exp 42 ], vec_of_int 42)
  ; (3, Combine [ int_exp 1; int_exp 2; int_exp 3 ], vec_of_intlist [ 1; 2; 3 ])
  ; ( 4
    , Combine [ int_exp 5; Combine [ int_exp 6; int_exp 7; Combine [ int_exp 8 ]; int_exp 9 ] ]
    , vec_of_intlist [ 5; 6; 7; 8; 9 ] )
  ; (* subset1 nothing for ints *)
    (5, Subset1_Nothing (int_exp 2), vec_of_int 2)
  ; (6, Subset1_Nothing (Combine [ int_exp 3; int_exp 1; int_exp 4 ]), vec_of_intlist [ 3; 1; 4 ])
  ; ( 7
    , Subset1_Nothing (Combine [ int_exp 5; Combine [ int_exp 6; int_exp 7 ] ])
    , vec_of_intlist [ 5; 6; 7 ] )
  ; (* subset2 for ints *)
    (8, Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], int_exp 1), vec_of_int 1)
  ; (9, Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], int_exp 2), vec_of_int 2)
  ; (10, Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], int_exp 3), vec_of_int 3)
  ; (11, Subset2 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], Combine [ int_exp 1 ]), vec_of_int 7)
  ; (12, Subset2 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], Combine [ int_exp 2 ]), vec_of_int 6)
  ; (13, Subset2 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], Combine [ int_exp 3 ]), vec_of_int 5)
  ; (* subset1 zero for ints *)
    (14, Subset1 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], int_exp 0), empty_vec T_Int)
  ; ( 15
    , Subset1 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], Combine [ int_exp 0 ])
    , empty_vec T_Int )
  ; (16, Subset1 (int_exp 7, Combine [ int_exp 0 ]), empty_vec T_Int)
  ; (* bool lit *)
    (17, true_exp, vec_of_bool true)
  ; (* combine for bools *)
    (18, Combine [ true_exp ], vec_of_bool true)
  ; (19, Combine [ true_exp; false_exp; true_exp ], vec_of_boollist [ true; false; true ])
  ; ( 20
    , Combine [ false_exp; Combine [ false_exp; true_exp; Combine [ false_exp ]; true_exp ] ]
    , vec_of_boollist [ false; false; true; false; true ] )
  ; (* subset1 nothing for bools *)
    (21, Subset1_Nothing false_exp, vec_of_bool false)
  ; ( 22
    , Subset1_Nothing (Combine [ true_exp; false_exp; false_exp ])
    , vec_of_boollist [ true; false; false ] )
  ; ( 23
    , Subset1_Nothing (Combine [ false_exp; Combine [ false_exp; true_exp ] ])
    , vec_of_boollist [ false; false; true ] )
  ; (* subset2 for bools *)
    (24, Subset2 (Combine [ true_exp; true_exp; false_exp ], int_exp 1), vec_of_bool true)
  ; (25, Subset2 (Combine [ true_exp; true_exp; false_exp ], int_exp 2), vec_of_bool true)
  ; (26, Subset2 (Combine [ true_exp; true_exp; false_exp ], int_exp 3), vec_of_bool false)
  ; ( 27
    , Subset2 (Combine [ false_exp; true_exp; false_exp ], Combine [ int_exp 1 ])
    , vec_of_bool false )
  ; ( 28
    , Subset2 (Combine [ false_exp; true_exp; false_exp ], Combine [ int_exp 2 ])
    , vec_of_bool true )
  ; ( 29
    , Subset2 (Combine [ false_exp; true_exp; false_exp ], Combine [ int_exp 3 ])
    , vec_of_bool false )
  ; (* subset1 zero for bools *)
    (30, Subset1 (Combine [ false_exp; true_exp; false_exp ], int_exp 0), empty_vec T_Bool)
  ; ( 31
    , Subset1 (Combine [ false_exp; true_exp; false_exp ], Combine [ int_exp 0 ])
    , empty_vec T_Bool )
  ; (32, Subset1 (true_exp, Combine [ int_exp 0 ]), empty_vec T_Bool)
  ; (* subset1 logical *)
    ( 33
    , Subset1
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Combine [ true_exp; false_exp; false_exp; true_exp ] )
    , vec_of_intlist [ 1; 4 ] )
  ; ( 34
    , Subset1
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Combine [ true_exp; true_exp; true_exp; true_exp ] )
    , vec_of_intlist [ 1; 2; 3; 4 ] )
  ; ( 35
    , Subset1
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Combine [ false_exp; false_exp; false_exp; false_exp ] )
    , empty_vec T_Int )
  ; ( 36
    , Subset1
        ( Combine [ true_exp; true_exp; false_exp; false_exp; true_exp ]
        , Combine [ true_exp; false_exp; false_exp; true_exp; true_exp ] )
    , vec_of_boollist [ true; false; true ] )
  ; ( 37
    , Subset1
        ( Combine [ true_exp; true_exp; false_exp; false_exp; true_exp ]
        , Combine [ true_exp; true_exp; true_exp; true_exp; true_exp ] )
    , vec_of_boollist [ true; true; false; false; true ] )
  ; ( 38
    , Subset1
        ( Combine [ true_exp; true_exp; false_exp; false_exp; true_exp ]
        , Combine [ false_exp; false_exp; false_exp; false_exp; false_exp ] )
    , empty_vec T_Bool )
  ; (* NA lit *)
    (39, na_exp T_Int, vec_of_lit NA_int)
  ; (40, na_exp T_Bool, vec_of_lit NA_bool)
  ; (* Combine with NA *)
    (41, Combine [ na_exp T_Int ], vec_of_lit NA_int)
  ; (42, Combine [ na_exp T_Bool ], vec_of_lit NA_bool)
  ; ( 43
    , Combine [ int_exp 5; Combine [ int_exp 6; int_exp 7; Combine [ na_exp T_Int ]; int_exp 9 ] ]
    , vec_of_intoptlist [ Some 5; Some 6; Some 7; None; Some 9 ] )
  ; ( 44
    , Combine [ true_exp; Combine [ true_exp; false_exp; Combine [ na_exp T_Bool ]; true_exp ] ]
    , vec_of_booloptlist [ Some true; Some true; Some false; None; Some true ] )
  ; (* subset1 nothing with NA *)
    ( 45
    , Subset1_Nothing (Combine [ int_exp 3; int_exp 1; na_exp T_Int ])
    , vec_of_intoptlist [ Some 3; Some 1; None ] )
  ; ( 46
    , Subset1_Nothing (Combine [ true_exp; true_exp; na_exp T_Bool ])
    , vec_of_booloptlist [ Some true; Some true; None ] )
  ; (* subset2 with NA *)
    ( 47
    , Subset2 (Combine [ na_exp T_Int; int_exp 2; int_exp 3 ], int_exp 1)
    , vec_of_lit (na_lit T_Int) )
  ; ( 48
    , Subset2 (Combine [ na_exp T_Bool; true_exp; false_exp ], int_exp 1)
    , vec_of_lit (na_lit T_Bool) )
  ; (* subset1 zero with NA *)
    ( 49
    , Subset1 (Combine [ na_exp T_Bool; na_exp T_Bool; na_exp T_Bool ], Combine [ int_exp 0 ])
    , empty_vec T_Bool )
  ; ( 50
    , Subset1 (Combine [ na_exp T_Int; na_exp T_Int; na_exp T_Int ], Combine [ int_exp 0 ])
    , empty_vec T_Int )
  ; (* subset1 logical with NA *)
    ( 51
    , Subset1
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Combine [ true_exp; na_exp T_Bool; false_exp; true_exp ] )
    , vec_of_intoptlist [ Some 1; None; Some 4 ] )
  ; ( 52
    , Subset1
        ( Combine [ true_exp; true_exp; false_exp; false_exp; true_exp ]
        , Combine [ true_exp; false_exp; true_exp; na_exp T_Bool; true_exp ] )
    , vec_of_booloptlist [ Some true; Some false; None; Some true ] )
  ; (* subset1 positive *)
    ( 53
    , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 1)
    , vec_of_intlist [ 11 ] )
  ; ( 54
    , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 2)
    , vec_of_intlist [ 12 ] )
  ; ( 55
    , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 3)
    , vec_of_intlist [ 13 ] )
  ; ( 56
    , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 4)
    , vec_of_intlist [ 14 ] )
  ; ( 57
    , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 5)
    , vec_of_lit (na_lit T_Int) )
  ; ( 58
    , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], Combine [ int_exp 1 ])
    , vec_of_intlist [ 11 ] )
  ; ( 59
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; int_exp 3 ] )
    , vec_of_intlist [ 11; 13 ] )
  ; ( 60
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 0; int_exp 1; int_exp 3; int_exp 5 ] )
    , vec_of_intoptlist [ Some 11; Some 13; None ] )
  ; ( 61
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 3; int_exp 3; int_exp 2; int_exp 4 ] )
    , vec_of_intlist [ 13; 13; 12; 14 ] )
  ; (* subset1 logical with extending *)
    ( 62
    , Subset1
        (Combine [ int_exp 11; int_exp 12 ], Combine [ true_exp; true_exp; true_exp; true_exp ])
    , vec_of_intoptlist [ Some 11; Some 12; None; None ] )
  ; ( 63
    , Subset1
        (Combine [ int_exp 11; int_exp 12 ], Combine [ true_exp; false_exp; false_exp; true_exp ])
    , vec_of_intoptlist [ Some 11; None ] )
  ; ( 64
    , Subset1
        ( Combine [ int_exp 11; int_exp 12 ]
        , Combine [ true_exp; na_exp T_Bool; false_exp; true_exp ] )
    , vec_of_intoptlist [ Some 11; None; None ] )
  ; ( 65
    , Subset1 (Subset1 (int_exp 0, int_exp 0), Combine [ true_exp; true_exp ])
    , vec_of_intoptlist [ None; None ] )
  ; (* subset1 logical with recycling *)
    ( 66
    , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], Combine [ true_exp ])
    , vec_of_intlist [ 11; 12; 13; 14 ] )
  ; ( 67
    , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], Combine [ false_exp ])
    , vec_of_intlist [] )
  ; ( 68
    , Subset1
        (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], Combine [ true_exp; false_exp ])
    , vec_of_intlist [ 11; 13 ] )
  ; ( 69
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ true_exp; na_exp T_Bool ] )
    , vec_of_intoptlist [ Some 11; None; Some 13; None ] )
  ; ( 70
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ false_exp; true_exp; na_exp T_Bool ] )
    , vec_of_intoptlist [ Some 12; None ] )
  ; (* subset1 neg *)
    ( 70
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp ~-1; int_exp ~-2; int_exp ~-3; int_exp ~-4 ] )
    , empty_vec T_Int )
  ; ( 71
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Negate (Combine [ int_exp 1; int_exp 2; int_exp 5; int_exp 0 ]) )
    , vec_of_intlist [ 13; 14 ] )
  ; ( 72
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Negate (Combine [ int_exp 0; int_exp 20; int_exp 5; int_exp 0 ]) )
    , vec_of_intlist [ 11; 12; 13; 14 ] )
  ; ( 73
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Negate (Combine [ int_exp 4; int_exp 20; int_exp 1; int_exp 0 ]) )
    , vec_of_intlist [ 12; 13 ] )
  ; (* subset1_nothing_assign *)
    ( 74
    , Subset1_Nothing_Assign (Combine [ int_exp 11; int_exp 12; int_exp 13 ], int_exp 0)
    , vec_of_intlist [ 0; 0; 0 ] )
  ; ( 75
    , Subset1_Nothing_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; int_exp 2 ] )
    , vec_of_intlist [ 1; 2; 1; 2 ] )
  ; (* subset1 assign *)
    ( 76
    , Subset1_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; int_exp 2 ]
        , Combine [ int_exp 9; int_exp 8 ] )
    , vec_of_intlist [ 9; 8; 13; 14 ] )
  ; ( 77
    , Subset1_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; int_exp 2 ]
        , Combine [ int_exp 9 ] )
    , vec_of_intlist [ 9; 9; 13; 14 ] )
  ; (* subset2 assign *)
    ( 78
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1 ]
        , Combine [ int_exp 9 ] )
    , vec_of_intlist [ 9; 12; 13; 14 ] )
  ; ( 79
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 4 ]
        , Combine [ int_exp 9 ] )
    , vec_of_intlist [ 11; 12; 13; 9 ] )
  ; (* subset1 with NAs *)
    ( 79
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; na_exp T_Int; int_exp 2 ] )
    , vec_of_intoptlist [ Some 11; None; Some 12 ] )
  ; (* subset1 assign *)
    ( 80
    , Subset1_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 2; int_exp 0; int_exp 1 ]
        , Combine [ int_exp 9; int_exp 8 ] )
    , vec_of_intlist [ 8; 9; 13; 14 ] )
  ; (* subset1 bool assign and subset1 bool *)
    ( 81
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Combine [ true_exp ]
        , Combine [ int_exp 0 ] )
    , vec_of_intlist [ 0; 0; 0; 0; 0 ] )
  ; ( 82
    , Subset1
        (Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ], Combine [ false_exp ])
    , empty_vec T_Int )
  ; ( 83
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Combine [ true_exp; false_exp; true_exp ]
        , Combine [ int_exp 0 ] )
    , vec_of_intlist [ 0; 2; 0; 0; 5 ] )
  ; ( 85
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Combine [ true_exp; true_exp; true_exp; true_exp; true_exp ]
        , Combine [ int_exp 0 ] )
    , vec_of_intlist [ 0; 0; 0; 0; 0 ] )
  ; ( 86
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5; int_exp 6 ]
        , Combine [ true_exp ]
        , Combine [ int_exp 10; int_exp 11 ] )
    , vec_of_intlist [ 10; 11; 10; 11; 10; 11 ] )
  ; ( 87
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5; int_exp 6 ]
        , Combine [ true_exp; false_exp ]
        , Combine [ int_exp 10; int_exp 11; int_exp 12 ] )
    , vec_of_intlist [ 10; 2; 11; 4; 12; 6 ] )
  ; ( 88
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
        , Combine [ true_exp; true_exp; false_exp ]
        , Combine [ int_exp 9; int_exp 8 ] )
    , vec_of_intlist [ 9; 8; 3 ] )
  ; ( 89
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
        , Combine [ true_exp; true_exp; true_exp; true_exp; false_exp ]
        , Combine [ int_exp 9; int_exp 8 ] )
    , vec_of_intoptlist [ Some 9; Some 8; Some 9; Some 8; None ] )
  ; (* more fun with NAs *)
    ( 90
    , Subset1 (Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ], na_exp T_Int)
    , vec_of_lit (na_lit T_Int) )
  ; ( 91
    , Subset1 (Combine [ int_exp ~-1; int_exp ~-2; int_exp ~-3; int_exp ~-4 ], na_exp T_Int)
    , vec_of_lit (na_lit T_Int) )
  ; ( 92
    , Subset1
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Negate (Combine [ na_exp T_Int; na_exp T_Int ]) )
    , vec_of_intoptlist [ None; None ] )
  ; ( 93
    , Subset1
        (Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ], Combine [ int_exp 1; na_exp T_Int ])
    , vec_of_intoptlist [ Some 1; None ] )
  ; (* negate *)
    ( 96
    , Negate (Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ])
    , vec_of_intlist [ ~-1; ~-2; ~-3; ~-4 ] )
  ; ( 97
    , Negate (Combine [ int_exp 1; int_exp ~-2; int_exp ~-3; int_exp 4 ])
    , vec_of_intlist [ ~-1; 2; 3; ~-4 ] )
  ; ( 98
    , Negate (Combine [ int_exp 1; int_exp ~-2; na_exp T_Int; int_exp 4 ])
    , vec_of_intoptlist [ Some ~-1; Some 2; None; Some ~-4 ] )
  ; (* subset1 assign negative *)
    ( 99
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (int_exp 1)
        , int_exp 0 )
    , vec_of_intlist [ 1; 0; 0; 0; 0 ] )
  ; ( 100
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (int_exp 1)
        , Combine [ int_exp 10; int_exp 11 ] )
    , vec_of_intlist [ 1; 10; 11; 10; 11 ] )
  ; ( 101
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , int_exp ~-1
        , Combine [ int_exp 10; int_exp 11; int_exp 12; int_exp 13 ] )
    , vec_of_intlist [ 1; 10; 11; 12; 13 ] )
  ; ( 102
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 3 ])
        , Combine [ int_exp 0 ] )
    , vec_of_intlist [ 1; 0; 3; 0; 0 ] )
  ; ( 103
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 3 ])
        , Combine [ int_exp 10; int_exp 11; int_exp 12 ] )
    , vec_of_intlist [ 1; 10; 3; 11; 12 ] )
  ; ( 104
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 0; int_exp 3; int_exp 10 ])
        , Combine [ int_exp 0 ] )
    , vec_of_intlist [ 1; 0; 3; 0; 0 ] )
  ; ( 105
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 0; int_exp 3; int_exp 10 ])
        , Combine [ int_exp 10; int_exp 11; int_exp 12 ] )
    , vec_of_intlist [ 1; 10; 3; 11; 12 ] )
  ; ( 106
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 3; int_exp 1 ])
        , Combine [ int_exp 0 ] )
    , vec_of_intlist [ 1; 0; 3; 0; 0 ] )
  ; ( 107
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 3; int_exp 1 ])
        , Combine [ int_exp 10; int_exp 11; int_exp 12 ] )
    , vec_of_intlist [ 1; 10; 3; 11; 12 ] )
  ]

let tests_neg =
  [ (* subset2 errors *)
    ( 1001
    , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], int_exp 0)
    , Eval.Selecting_lt_one_element )
  ; ( 1002
    , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], int_exp 4)
    , Eval.Subscript_out_of_bounds )
  ; ( 1003
    , Subset2 (Combine [ true_exp; false_exp; false_exp ], int_exp 5)
    , Eval.Subscript_out_of_bounds )
  ; ( 1004
    , Subset2 (Combine [ true_exp; true_exp ], Combine [ int_exp 0 ])
    , Eval.Selecting_lt_one_element )
  ; ( 1005
    , Subset2 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], Combine [ int_exp 4 ])
    , Eval.Subscript_out_of_bounds )
  ; ( 1006
    , Subset2 (Combine [ false_exp; true_exp; false_exp ], Combine [ int_exp 5 ])
    , Eval.Subscript_out_of_bounds )
  ; ( 1007
    , Subset2 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], Subset1 (int_exp 1, int_exp 0))
    , Eval.Selecting_lt_one_element )
  ; ( 1008
    , Subset2
        (Combine [ true_exp; true_exp; false_exp; false_exp ], Combine [ int_exp 5; int_exp 6 ])
    , Eval.Selecting_gt_one_element )
  ; ( 1009
    , Subset2 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], Combine [ int_exp ~-1 ])
    , Eval.Selecting_gt_one_element )
  ; ( 1010
    , Subset2 (Subset1 (int_exp 1, int_exp 0), Combine [ int_exp 1 ])
    , Eval.Subscript_out_of_bounds )
  ; ( 1011
    , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], true_exp)
    , Eval.type_error T_Int T_Bool )
  ; (* combine errors *)
    (1012, Combine [], Eval.Expected_nonempty_vector)
  ; (1013, Combine [ int_exp 0; true_exp; false_exp ], Eval.type_error T_Int T_Bool)
  ; (1014, Combine [ false_exp; true_exp; int_exp 1; false_exp ], Eval.type_error T_Bool T_Int)
  ; (* subset2 with NA errors *)
    (1015, Subset2 (Subset1 (int_exp 1, int_exp 0), na_exp T_Int), Eval.Subscript_out_of_bounds)
  ; (1016, Subset2 (Subset1 (int_exp 1, int_exp 0), na_exp T_Bool), Eval.type_error T_Int T_Bool)
  ; ( 1017
    , Subset2 (Subset1 (int_exp 1, int_exp 0), Combine [ na_exp T_Int ])
    , Eval.Subscript_out_of_bounds )
  ; (* subset1 mixing subscripts *)
    ( 1018
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 0; int_exp 1; na_exp T_Int; int_exp ~-3 ] )
    , Eval.Mixing_with_neg_subscripts )
  ; ( 1019
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Negate (Combine [ int_exp 4; int_exp ~-1; int_exp 0 ]) )
    , Eval.Mixing_with_neg_subscripts )
  ; ( 1020
    , Subset1
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Negate (Combine [ int_exp 4; na_exp T_Int; int_exp 0 ]) )
    , Eval.Mixing_with_neg_subscripts )
  ; (* subset1_nothing_assign *)
    ( 1021
    , Subset1_Nothing_Assign
        (Combine [ int_exp 11; int_exp 12; int_exp 13 ], Combine [ int_exp 1; int_exp 2 ])
    , Eval.Replacement_length_not_multiple )
  ; ( 1022
    , Subset1_Nothing_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13 ]
        , Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1023
    , Subset1_Nothing_Assign (Combine [ int_exp 11; int_exp 12; int_exp 13 ], Combine [ true_exp ])
    , Eval.type_error T_Int T_Bool )
  ; (* subset1 assign *)
    ( 1024
    , Subset1_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; na_exp T_Int; int_exp 2 ]
        , Combine [ int_exp 9; int_exp 8; int_exp 7 ] )
    , Eval.No_NAs_in_subscripted_assignment )
  ; ( 1025
    , Subset1_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; int_exp 2 ]
        , Combine [ int_exp 9; int_exp 8; int_exp 7 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1026
    , Subset1_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; int_exp 2; int_exp 3 ]
        , Combine [ int_exp 9; int_exp 8 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1026
    , Subset1_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; int_exp 2; int_exp 3 ]
        , Combine [ false_exp ] )
    , Eval.type_error T_Int T_Bool )
  ; (* subset2 assign *)
    ( 1027
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 0 ]
        , Combine [ int_exp 9 ] )
    , Eval.Selecting_lt_one_element )
  ; ( 1028
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp ~-1 ]
        , Combine [ int_exp 9 ] )
    , Eval.Selecting_gt_one_element )
  ; ( 1029
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 11 ]
        , Combine [ int_exp 9 ] )
    , Eval.Subscript_out_of_bounds )
  ; ( 1030
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 11 ]
        , Combine [ na_exp T_Int ] )
    , Eval.Subscript_out_of_bounds )
  ; ( 1031
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1; int_exp 2 ]
        , Combine [ int_exp 9 ] )
    , Eval.Selecting_gt_one_element )
  ; ( 1032
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1 ]
        , Combine [ int_exp 9; int_exp 8 ] )
    , Eval.Too_many_elements_supplied )
  ; ( 1033
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ true_exp ]
        , Combine [ int_exp 8 ] )
    , Eval.type_error T_Int T_Bool )
  ; ( 1034
    , Subset2_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 1 ]
        , Combine [ true_exp ] )
    , Eval.type_error T_Int T_Bool )
  ; (* subset1 assign *)
    ( 1035
    , Subset1_Assign
        ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
        , Combine [ int_exp 2; int_exp 0; int_exp 1 ]
        , Combine [ int_exp 9; int_exp 8; int_exp 7 ] )
    , Eval.Replacement_length_not_multiple )
  ; (* subset1 bool assign *)
    ( 1036
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Combine [ true_exp ]
        , Combine [ int_exp 10; int_exp 11 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1037
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5; int_exp 6 ]
        , Combine [ true_exp; false_exp ]
        , Combine [ int_exp 10; int_exp 11 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1038
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Combine [ true_exp; na_exp T_Bool ]
        , Combine [ int_exp 10; int_exp 11; int_exp 12 ] )
    , Eval.No_NAs_in_subscripted_assignment )
  ; ( 1039
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5; int_exp 6 ]
        , Combine [ true_exp; false_exp; false_exp ]
        , Combine [ int_exp 10; int_exp 11; int_exp 12 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1040
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
        , Combine [ true_exp; true_exp; false_exp ]
        , Combine [ int_exp 10; int_exp 11; int_exp 12 ] )
    , Eval.Replacement_length_not_multiple )
  ; (* more fun with NAs *)
    ( 1041
    , Subset1
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Negate (Combine [ int_exp 1; na_exp T_Int ]) )
    , Eval.Mixing_with_neg_subscripts )
  ; ( 1042
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Combine [ na_exp T_Int ]
        , Combine [ int_exp 1; int_exp 2 ] )
    , Eval.No_NAs_in_subscripted_assignment )
  ; ( 1043
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Combine [ int_exp 1; na_exp T_Int ]
        , Combine [ int_exp 1; int_exp 2 ] )
    , Eval.No_NAs_in_subscripted_assignment )
  ; ( 1044
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Combine [ true_exp; false_exp; na_exp T_Bool ]
        , Combine [ int_exp 0 ] )
    , Eval.No_NAs_in_subscripted_assignment )
  ; ( 1045
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Combine [ na_exp T_Int ]
        , Combine [ int_exp 1 ] )
    , Eval.No_NAs_in_subscripted_assignment )
  ; ( 1046
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
        , Combine [ int_exp 1; int_exp 4; na_exp T_Int ]
        , Combine [ int_exp 0 ] )
    , Eval.No_NAs_in_subscripted_assignment )
  ; (* subset1 assign negative *)
    ( 1047
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (int_exp 1)
        , Combine [ int_exp 10; int_exp 11; int_exp 12 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1048
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 3 ])
        , Combine [ int_exp 10; int_exp 11 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1049
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 3 ])
        , Combine [ int_exp 10; int_exp 11; int_exp 12; int_exp 13 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1050
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 0; int_exp 3; int_exp 10 ])
        , Combine [ int_exp 10; int_exp 11 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1051
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 0; int_exp 3; int_exp 10 ])
        , Combine [ int_exp 10; int_exp 11; int_exp 12; int_exp 13 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1052
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 0; int_exp 3; int_exp 10 ])
        , Combine [ int_exp 10; int_exp 11; int_exp 12; int_exp 13 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1053
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 3; int_exp 1 ])
        , Combine [ int_exp 10; int_exp 11 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1054
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; int_exp 3; int_exp 1 ])
        , Combine [ int_exp 10; int_exp 11; int_exp 12; int_exp 13 ] )
    , Eval.Replacement_length_not_multiple )
  ; ( 1055
    , Subset1_Assign
        ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
        , Negate (Combine [ int_exp 1; na_exp T_Int ])
        , int_exp 13 )
    , Eval.Mixing_with_neg_subscripts )
  ]

let () =
  List.iter run_test_pos tests_pos ;
  List.iter run_test_neg tests_neg ;
  print_endline "All tests passed!"
