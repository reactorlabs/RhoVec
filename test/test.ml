open Lib
open Expr

module A = Alcotest

let pp_value ppf v = Fmt.pf ppf "%s" (show_val v)
let testable_value = A.testable pp_value equal_value

let test_eval desc (expected, expr) =
  (* Also check that all elements of the vector have the right type. *)
  let assert_vec_elt_types = function
    | Vector (a, t) as vec ->
        if not (Array.for_all (fun x -> get_tag x = t) a) then
          A.failf "Vector `%s` not consistent with its type!" (show_val vec) in
  let run_eval () =
    let res = Eval.eval expr in
    assert_vec_elt_types expected ;
    assert_vec_elt_types res ;
    A.(check testable_value) "same value" expected res in
  A.test_case desc `Quick run_eval

let test_eval_err desc (excptn, expr) =
  let run_eval () = A.check_raises "same exception" excptn (fun _ -> ignore (Eval.eval expr)) in
  A.test_case desc `Quick run_eval

let () =
  A.run "testsuite"
    [ ( "literals"
      , [ test_eval "int 42" (vec_of_int 42, int_exp 42)
        ; test_eval "int -1" (vec_of_int ~-1, int_exp ~-1)
        ; test_eval "int NA" (vec_of_intoptlist [ None ], na_exp T_Int)
        ; test_eval "bool true" (vec_of_bool true, true_exp)
        ; test_eval "bool false" (vec_of_bool false, false_exp)
        ; test_eval "bool NA" (vec_of_booloptlist [ None ], na_exp T_Bool)
        ] )
    ; ( "combine"
      , [ test_eval "single int" (vec_of_int 42, Combine [ int_exp 42 ])
        ; test_eval "multiple int"
            (vec_of_intlist [ 1; 2; 3 ], Combine [ int_exp 1; int_exp 2; int_exp 3 ])
        ; test_eval "nested int"
            ( vec_of_intlist [ 5; 6; 7; 8; 9 ]
            , Combine
                [ int_exp 5; Combine [ int_exp 6; int_exp 7; Combine [ int_exp 8 ]; int_exp 9 ] ] )
        ; test_eval "nested int with empty int vector"
            ( vec_of_intlist [ 5; 6; 7; 9 ]
            , Combine
                [ int_exp 5
                ; Combine [ int_exp 6; int_exp 7; Subset1 (int_exp 8, int_exp 0); int_exp 9 ]
                ] )
        ; test_eval "nested int with NA"
            ( vec_of_intoptlist [ Some 5; None; Some 7; Some 8; Some 9 ]
            , Combine
                [ int_exp 5; Combine [ na_exp T_Int; int_exp 7; Combine [ int_exp 8 ]; int_exp 9 ] ]
            )
        ; test_eval "single bool" (vec_of_bool true, Combine [ true_exp ])
        ; test_eval "multiple bool"
            (vec_of_boollist [ true; false; true ], Combine [ true_exp; false_exp; true_exp ])
        ; test_eval "nested bool"
            ( vec_of_boollist [ true; true; false; false; true ]
            , Combine [ true_exp; Combine [ true_exp; false_exp; Combine [ false_exp ]; true_exp ] ]
            )
        ; test_eval "nested bool with empty bool vector"
            ( vec_of_boollist [ true; true; false; true ]
            , Combine
                [ true_exp
                ; Combine [ true_exp; false_exp; Subset1 (false_exp, int_exp 0); true_exp ]
                ] )
        ; test_eval "nested bool with NA"
            ( vec_of_booloptlist [ None; Some true; Some false; Some false; Some true ]
            , Combine
                [ na_exp T_Bool; Combine [ true_exp; false_exp; Combine [ false_exp ]; true_exp ] ]
            )
        ] )
    ; ( "combine.err"
      , [ test_eval_err "empty" (Eval.Expected_nonempty_vector, Combine [])
        ; test_eval_err "mixed types 1"
            (Eval.type_error T_Int T_Bool, Combine [ int_exp 0; true_exp; false_exp ])
        ; test_eval_err "mixed types 2"
            (Eval.type_error T_Bool T_Int, Combine [ false_exp; true_exp; int_exp 1; false_exp ])
        ] )
    ; ( "negate"
      , [ test_eval "positives"
            ( vec_of_intlist [ 0; ~-1; ~-2; ~-3; ~-4 ]
            , Negate (Combine [ int_exp 0; int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]) )
        ; test_eval "negatives"
            ( vec_of_intlist [ 0; 1; 2; 3; 4 ]
            , Negate (Combine [ int_exp ~-0; int_exp ~-1; int_exp ~-2; int_exp ~-3; int_exp ~-4 ])
            )
        ; test_eval "mixed"
            ( vec_of_intlist [ ~-1; 2; 3; ~-4 ]
            , Negate (Combine [ int_exp 1; int_exp ~-2; int_exp ~-3; int_exp 4 ]) )
        ; test_eval "empty" (empty_vec T_Int, Negate (Subset1 (int_exp 0, int_exp 0)))
        ; test_eval "NAs"
            ( vec_of_intoptlist [ Some ~-1; None; Some 2; Some 3; Some ~-4; None ]
            , Negate
                (Combine
                   [ int_exp 1; na_exp T_Int; int_exp ~-2; int_exp ~-3; int_exp 4; na_exp T_Int ])
            )
        ] )
    ; ( "negate.err"
      , [ test_eval_err "bool"
            (Eval.type_error T_Int T_Bool, Negate (Combine [ true_exp; false_exp ]))
        ] )
    ; ( "subset1_nothing"
      , [ test_eval "int" (vec_of_int 2, Subset1_Nothing (int_exp 2))
        ; test_eval "empty int" (empty_vec T_Int, Subset1_Nothing (Subset1 (int_exp 0, int_exp 0)))
        ; test_eval "multiple ints"
            ( vec_of_intlist [ 3; 1; 4 ]
            , Subset1_Nothing (Combine [ int_exp 3; int_exp 1; int_exp 4 ]) )
        ; test_eval "nested ints"
            ( vec_of_intlist [ 5; 6; 7 ]
            , Subset1_Nothing (Combine [ int_exp 5; Combine [ int_exp 6; int_exp 7 ] ]) )
        ; test_eval "nested ints with NAs"
            ( vec_of_intoptlist [ Some 5; None; Some 6; Some 7; None ]
            , Subset1_Nothing
                (Combine [ int_exp 5; na_exp T_Int; Combine [ int_exp 6; int_exp 7; na_exp T_Int ] ])
            )
        ] )
    ; ( "subset1_nothing.err"
      , [ test_eval_err "mixed types"
            (Eval.type_error T_Int T_Bool, Subset1_Nothing (Combine [ int_exp 0; false_exp ]))
        ] )
    ; ( "subset1.logical"
      , [ test_eval "int vector 1"
            ( vec_of_intlist [ 1; 4 ]
            , Subset1
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , Combine [ true_exp; false_exp; false_exp; true_exp ] ) )
        ; test_eval "int vector 2"
            ( vec_of_intlist [ 1; 2; 3; 4 ]
            , Subset1
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , Combine [ true_exp; true_exp; true_exp; true_exp ] ) )
        ; test_eval "int vector 3"
            ( empty_vec T_Int
            , Subset1
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , Combine [ false_exp; false_exp; false_exp; false_exp ] ) )
        ; test_eval "int vector with NA index"
            ( vec_of_intoptlist [ Some 1; None; Some 4 ]
            , Subset1
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , Combine [ true_exp; na_exp T_Bool; false_exp; true_exp ] ) )
        ] )
    ; ( "subset1.logical.extension"
      , [ test_eval "int vector 1"
            ( vec_of_intoptlist [ Some 11; Some 12; None; None ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12 ]
                , Combine [ true_exp; true_exp; true_exp; true_exp ] ) )
        ; test_eval "int vector 2"
            ( vec_of_intoptlist [ Some 11; None ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12 ]
                , Combine [ true_exp; false_exp; false_exp; true_exp ] ) )
        ; test_eval "int vector 3"
            ( vec_of_intoptlist [ Some 11; None; None ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12 ]
                , Combine [ true_exp; na_exp T_Bool; false_exp; true_exp ] ) )
        ; test_eval "empty int vector"
            ( vec_of_intoptlist [ None; None ]
            , Subset1 (Subset1 (int_exp 0, int_exp 0), Combine [ true_exp; true_exp ]) )
        ] )
    ; ( "subset1.logical.recycling"
      , [ test_eval "int vector 1"
            ( vec_of_intlist [ 11; 12; 13; 14 ]
            , Subset1
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], Combine [ true_exp ])
            )
        ; test_eval "int vector 2"
            ( empty_vec T_Int
            , Subset1
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], Combine [ false_exp ])
            )
        ; test_eval "int vector 3"
            ( vec_of_intlist [ 11; 13 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ true_exp; false_exp ] ) )
        ; test_eval "int vector 4"
            ( vec_of_intoptlist [ Some 11; None; Some 13; None ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ true_exp; na_exp T_Bool ] ) )
        ; test_eval "int vector 5"
            ( vec_of_intoptlist [ Some 11; Some 12; None; Some 14 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ true_exp; true_exp; na_exp T_Bool ] ) )
        ; test_eval "int vector 6"
            ( vec_of_intoptlist [ None; None; None; None ]
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], na_exp T_Bool) )
        ] )
    ; ( "subset1.zero"
      , [ test_eval "int vector 1" (empty_vec T_Int, Subset1 (int_exp 7, int_exp 0))
        ; test_eval "int vector 2"
            (empty_vec T_Int, Subset1 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], int_exp 0))
        ; test_eval "int vector 3"
            ( empty_vec T_Int
            , Subset1 (Combine [ int_exp 7; int_exp 6; int_exp 5 ], Combine [ int_exp 0 ]) )
        ; test_eval "empty int vector"
            (empty_vec T_Int, Subset1 (Subset1 (int_exp 0, int_exp 0), int_exp 0))
        ; test_eval "NA int vector"
            ( empty_vec T_Int
            , Subset1 (Combine [ na_exp T_Int; na_exp T_Int; na_exp T_Int ], Combine [ int_exp 0 ])
            )
        ] )
    ; ( "subset1.positive"
      , [ test_eval "index 1"
            ( vec_of_int 11
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 1) )
        ; test_eval "index 2"
            ( vec_of_int 12
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 2) )
        ; test_eval "index 3"
            ( vec_of_int 13
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 3) )
        ; test_eval "index 4"
            ( vec_of_int 14
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 4) )
        ; test_eval "index 5 out-of-bounds"
            ( vec_of_na T_Int
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 5) )
        ; test_eval "index NA"
            ( vec_of_na T_Int
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], na_exp T_Int) )
        ; test_eval "multiple indexes"
            ( vec_of_intlist [ 11; 13 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 3 ] ) )
        ; test_eval "multiple indexes and ignored zero"
            ( vec_of_intlist [ 11; 13 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 0; int_exp 1; int_exp 3 ] ) )
        ; test_eval "multiple indexes and out-of-bounds"
            ( vec_of_intoptlist [ Some 11; Some 13; None ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 3; int_exp 5 ] ) )
        ; test_eval "multiple indexes repeated"
            ( vec_of_intlist [ 13; 13; 12; 14; 13 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 3; int_exp 3; int_exp 2; int_exp 4; int_exp 3 ] ) )
        ; test_eval "multiple indexes with NA"
            ( vec_of_intoptlist [ Some 12; None; Some 11 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 2; na_exp T_Int; int_exp 1 ] ) )
        ] )
    ; ( "subset1.negative"
      , [ test_eval "exclude 1"
            ( vec_of_intlist [ 12; 13; 14 ]
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp ~-1) )
        ; test_eval "exclude 2"
            ( vec_of_intlist [ 11; 13; 14 ]
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp ~-2) )
        ; test_eval "exclude 3"
            ( vec_of_intlist [ 11; 12; 14 ]
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp ~-3) )
        ; test_eval "exclude 4"
            ( vec_of_intlist [ 11; 12; 13 ]
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp ~-4) )
        ; test_eval "exclude 5 out-of-bounds"
            ( vec_of_intlist [ 11; 12; 13; 14 ]
            , Subset1 (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp ~-5) )
        ; test_eval "exclude all"
            ( empty_vec T_Int
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp ~-1; int_exp ~-2; int_exp ~-3; int_exp ~-4 ] ) )
        ; test_eval "exclude multiple"
            ( vec_of_intlist [ 13; 14 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Negate (Combine [ int_exp 2; int_exp 1 ]) ) )
        ; test_eval "exclude multiple and ignored zero"
            ( vec_of_intlist [ 13; 14 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Negate (Combine [ int_exp 1; int_exp 2; int_exp 0 ]) ) )
        ; test_eval "exclude multiple and out-of-bounds"
            ( vec_of_intlist [ 13; 14 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Negate (Combine [ int_exp 1; int_exp 5; int_exp 2; int_exp 0 ]) ) )
        ; test_eval "exclude multiple repeated"
            ( vec_of_intlist [ 12; 13; 14 ]
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Negate (Combine [ int_exp 1; int_exp 1 ]) ) )
        ] )
    ; ( "subset1.err"
      , [ test_eval_err "mixing subscripts 1"
            ( Eval.Mixing_with_negative_subscripts
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 0; int_exp 1; na_exp T_Int; int_exp ~-3 ] ) )
        ; test_eval_err "mixing subscripts 2"
            ( Eval.Mixing_with_negative_subscripts
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Negate (Combine [ int_exp 4; int_exp ~-1; int_exp 0 ]) ) )
        ; test_eval_err "mixing subscripts 3"
            ( Eval.Mixing_with_negative_subscripts
            , Subset1
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Negate (Combine [ int_exp 4; na_exp T_Int; int_exp 0 ]) ) )
        ] )
    ; ( "subset2"
      , [ test_eval "index 1"
            ( vec_of_int 1
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3; na_exp T_Int ], int_exp 1) )
        ; test_eval "index 2"
            ( vec_of_int 2
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3; na_exp T_Int ], int_exp 2) )
        ; test_eval "index 3"
            ( vec_of_int 3
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3; na_exp T_Int ], int_exp 3) )
        ; test_eval "index 4"
            ( vec_of_na T_Int
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3; na_exp T_Int ], int_exp 4) )
        ] )
    ; ( "subset2.err"
      , [ test_eval_err "index 0"
            ( Eval.Selecting_lt_one_element
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], int_exp 0) )
        ; test_eval_err "empty vector as index"
            ( Eval.Selecting_lt_one_element
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], Subset1 (int_exp 1, int_exp 0))
            )
        ; test_eval_err "single negative index"
            ( Eval.Invalid_negative_subscript
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], int_exp ~-1) )
        ; test_eval_err "multiple index 1"
            ( Eval.Selecting_gt_one_element
            , Subset2
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Negate (Combine [ int_exp 1; int_exp 2 ]) ) )
        ; test_eval_err "multiple index 2"
            ( Eval.Selecting_gt_one_element
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], Combine [ int_exp 1; int_exp 2 ])
            )
        ; test_eval_err "out-of-bounds 1"
            ( Eval.Subscript_out_of_bounds
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], int_exp 4) )
        ; test_eval_err "out-of-bounds 2"
            (Eval.Subscript_out_of_bounds, Subset2 (Subset1 (int_exp 1, int_exp 0), int_exp 1))
        ; test_eval_err "out-of-bounds with NA"
            ( Eval.Subscript_out_of_bounds
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ], na_exp T_Int) )
        ; test_eval_err "mixed types"
            ( Eval.type_error T_Int T_Bool
            , Subset2 (Combine [ int_exp 1; int_exp 2; int_exp 3 ], true_exp) )
        ] )
    ; ( "subset1_nothing_assign"
      , [ test_eval "single"
            ( vec_of_intlist [ 0; 0; 0 ]
            , Subset1_Nothing_Assign (Combine [ int_exp 11; int_exp 12; int_exp 13 ], int_exp 0) )
        ; test_eval "NA"
            ( vec_of_intoptlist [ None; None; None; None ]
            , Subset1_Nothing_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], na_exp T_Int) )
        ; test_eval "multiple 1"
            ( vec_of_intlist [ 1; 2; 1; 2 ]
            , Subset1_Nothing_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 2 ] ) )
        ; test_eval "multiple 2"
            ( vec_of_intlist [ 1; 2; 3; 4 ]
            , Subset1_Nothing_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ] ) )
        ] )
    ; ( "subset1_nothing_assign.err"
      , [ test_eval_err "wrong length 1"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Nothing_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13 ], Combine [ int_exp 1; int_exp 2 ])
            )
        ; test_eval_err "wrong length 2"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Nothing_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13 ]
                , Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ] ) )
        ; test_eval_err "wrong length 3"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Nothing_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13 ]
                , Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5; int_exp 6 ] ) )
        ; test_eval_err "empty replacement"
            ( Eval.Replacement_length_is_zero
            , Subset1_Nothing_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13 ], Subset1 (int_exp 1, int_exp 0)) )
        ; test_eval_err "mixed types"
            ( Eval.type_error T_Int T_Bool
            , Subset1_Nothing_Assign (Combine [ int_exp 11; int_exp 12; int_exp 13 ], true_exp) )
        ; test_eval_err "mixed types"
            ( Eval.type_error T_Int T_Bool
            , Subset1_Nothing_Assign (Combine [ int_exp 11; int_exp 12; int_exp 13 ], true_exp) )
        ] )
    ; ( "subset1_assign.logical"
      , [ test_eval "no recycling or extension 1"
            ( vec_of_intlist [ 1; 12; 13; 14; 5 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ false_exp; true_exp; true_exp; true_exp; false_exp ]
                , Combine [ int_exp 12; int_exp 13; int_exp 14 ] ) )
        ; test_eval "no recycling or extension 2"
            ( vec_of_intlist [ 9; 8; 3 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ true_exp; true_exp; false_exp ]
                , Combine [ int_exp 9; int_exp 8 ] ) )
        ; test_eval "recycle replacement"
            ( vec_of_intlist [ 1; 0; 0; 0; 5 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ false_exp; true_exp; true_exp; true_exp; false_exp ]
                , Combine [ int_exp 0 ] ) )
        ; test_eval "recycle index"
            ( vec_of_intlist [ 11; 2; 12; 13; 5 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ true_exp; false_exp; true_exp ]
                , Combine [ int_exp 11; int_exp 12; int_exp 13 ] ) )
        ; test_eval "recycle index and replacement 1"
            ( vec_of_intlist [ 0; 0; 0; 0; 0 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ true_exp ]
                , Combine [ int_exp 0 ] ) )
        ; test_eval "recycle index and replacement 2"
            ( vec_of_intlist [ 0; 2; 0; 0; 5 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ true_exp; false_exp; true_exp ]
                , Combine [ int_exp 0 ] ) )
        ; test_eval "recycle index and replacement 3"
            ( vec_of_intlist [ 10; 11; 10; 11; 10; 11 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5; int_exp 6 ]
                , Combine [ true_exp ]
                , Combine [ int_exp 10; int_exp 11 ] ) )
        ; test_eval "recycle index and replacement 4"
            ( vec_of_intlist [ 10; 2; 11; 4; 12; 6 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5; int_exp 6 ]
                , Combine [ true_exp; false_exp ]
                , Combine [ int_exp 10; int_exp 11; int_exp 12 ] ) )
        ; test_eval "extension 1"
            ( vec_of_intlist [ 11; 12; 13; 14 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ true_exp; true_exp; true_exp; true_exp ]
                , Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ] ) )
        ; test_eval "extension 2"
            ( vec_of_intlist [ 11; 12; 3; 13 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ true_exp; true_exp; false_exp; true_exp ]
                , Combine [ int_exp 11; int_exp 12; int_exp 13 ] ) )
        ; test_eval "extension 3"
            ( vec_of_intoptlist [ Some 11; Some 12; Some 13; Some 14; None ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ true_exp; true_exp; true_exp; true_exp; false_exp ]
                , Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ] ) )
        ; test_eval "extension and recycle replacement 1"
            ( vec_of_intoptlist [ Some 0; Some 0; Some 0; Some 0; None ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ true_exp; true_exp; true_exp; true_exp; false_exp ]
                , Combine [ int_exp 0 ] ) )
        ; test_eval "extension and recycle replacement 2"
            ( vec_of_intoptlist [ Some 9; Some 8; Some 9; Some 8; None ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ true_exp; true_exp; true_exp; true_exp; false_exp ]
                , Combine [ int_exp 9; int_exp 8 ] ) )
        ; test_eval "extension and recycle replacement 3"
            ( vec_of_intoptlist [ Some 9; Some 8; Some 7; Some 6; None ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ true_exp; true_exp; true_exp; true_exp; false_exp ]
                , Combine [ int_exp 9; int_exp 8; int_exp 7; int_exp 6 ] ) )
        ] )
    ; ( "subset1_assign.logical.err"
      , [ test_eval_err "empty replacement"
            ( Eval.Replacement_length_is_zero
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ true_exp ]
                , Subset1 (int_exp 1, int_exp 0) ) )
        ; test_eval_err "wrong replacement length 1"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ true_exp ]
                , Combine [ int_exp 10; int_exp 11 ] ) )
        ; test_eval_err "wrong replacement length 2"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5; int_exp 6 ]
                , Combine [ true_exp; false_exp ]
                , Combine [ int_exp 10; int_exp 11 ] ) )
        ; test_eval_err "wrong replacement length 3"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5; int_exp 6 ]
                , Combine [ true_exp; false_exp; false_exp ]
                , Combine [ int_exp 10; int_exp 11; int_exp 12 ] ) )
        ; test_eval_err "wrong replacement length 4"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ true_exp; true_exp; false_exp ]
                , Combine [ int_exp 10; int_exp 11; int_exp 12 ] ) )
        ; test_eval_err "extension and recycle replacement 3"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ true_exp; true_exp; true_exp; true_exp; false_exp ]
                , Combine [ int_exp 9; int_exp 8; int_exp 7 ] ) )
        ; test_eval_err "NA in index"
            ( Eval.No_NAs_in_subscripted_assignment
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ true_exp; false_exp; na_exp T_Bool ]
                , Combine [ int_exp 0 ] ) )
        ; test_eval_err "mixed types"
            ( Eval.type_error T_Int T_Bool
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ true_exp; false_exp ]
                , Combine [ false_exp ] ) )
        ] )
    ; ( "subset1_assign.zero"
      , [ test_eval "int vector 1" (vec_of_int 7, Subset1_Assign (int_exp 7, int_exp 0, int_exp 42))
        ; test_eval "int vector 2"
            (vec_of_int 7, Subset1_Assign (int_exp 7, int_exp 0, na_exp T_Int))
        ; test_eval "empty int vector"
            (empty_vec T_Int, Subset1_Assign (Subset1 (int_exp 0, int_exp 0), int_exp 0, int_exp 1))
        ; test_eval "empty index vector"
            (vec_of_int 7, Subset1_Assign (int_exp 7, Subset1 (int_exp 0, int_exp 0), int_exp 1))
        ; test_eval "ignore wrong type"
            (vec_of_int 7, Subset1_Assign (int_exp 7, Subset1 (int_exp 0, int_exp 0), true_exp))
        ] )
    ; ( "subset1_assign.zero.err"
      , [ test_eval_err "NAs in index"
            ( Eval.No_NAs_in_subscripted_assignment
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , Combine [ int_exp 0; na_exp T_Int ]
                , Combine [ int_exp 0 ] ) )
        ] )
    ; ( "subset1_assign.positive"
      , [ test_eval "single index"
            ( vec_of_intlist [ 0; 12; 13; 14 ]
            , Subset1_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 1, int_exp 0)
            )
        ; test_eval "single index extension"
            ( vec_of_intoptlist [ Some 11; Some 12; Some 13; Some 14; None; None; None; Some 0 ]
            , Subset1_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 8, int_exp 0)
            )
        ; test_eval "multiple indexes"
            ( vec_of_intlist [ 9; 8; 13; 14 ]
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 2 ]
                , Combine [ int_exp 9; int_exp 8 ] ) )
        ; test_eval "multiple indexes with repeats 1"
            ( vec_of_intlist [ 8; 7; 13; 14 ]
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 1; int_exp 2 ]
                , Combine [ int_exp 9; int_exp 8; int_exp 7 ] ) )
        ; test_eval "multiple indexes with repeats 2"
            ( vec_of_intlist [ 8; 9; 8; 14; 15; 16 ]
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14; int_exp 15; int_exp 16 ]
                , Combine [ int_exp 1; int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ int_exp 9; int_exp 8 ] ) )
        ; test_eval "multiple indexes extension"
            ( vec_of_intlist [ 0; 12; 9; 8 ]
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12 ]
                , Combine [ int_exp 1; int_exp 3; int_exp 4 ]
                , Combine [ int_exp 0; int_exp 9; int_exp 8 ] ) )
        ; test_eval "multiple indexes recycling 1"
            ( vec_of_intlist [ 9; 9; 13; 14 ]
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 2 ]
                , Combine [ int_exp 9 ] ) )
        ; test_eval "multiple indexes recycling 2"
            ( vec_of_intlist [ 9; 8; 9; 8; 15; 16 ]
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14; int_exp 15; int_exp 16 ]
                , Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , Combine [ int_exp 9; int_exp 8 ] ) )
        ; test_eval "multiple indexes with zero"
            ( vec_of_intlist [ 8; 9; 13; 14 ]
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 2; int_exp 0; int_exp 1 ]
                , Combine [ int_exp 9; int_exp 8 ] ) )
        ] )
    ; ( "subset1_assign.positive.err"
      , [ test_eval_err "NA index 1"
            ( Eval.No_NAs_in_subscripted_assignment
            , Subset1_Assign
                (Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ], na_exp T_Int, int_exp 0) )
        ; test_eval_err "NA index 2"
            ( Eval.No_NAs_in_subscripted_assignment
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , na_exp T_Int
                , Combine [ int_exp 10; int_exp 11 ] ) )
        ; test_eval_err "NA index 3"
            ( Eval.No_NAs_in_subscripted_assignment
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , Combine [ int_exp 1; na_exp T_Int ]
                , int_exp 10 ) )
        ; test_eval_err "NA index 4"
            ( Eval.No_NAs_in_subscripted_assignment
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , Combine [ int_exp 1; na_exp T_Int ]
                , Combine [ int_exp 10; int_exp 11 ] ) )
        ; test_eval_err "empty replacement"
            ( Eval.Replacement_length_is_zero
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4 ]
                , int_exp 1
                , Subset1 (int_exp 1, int_exp 0) ) )
        ; test_eval_err "wrong replacement length 1"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 2 ]
                , Combine [ int_exp 9; int_exp 8; int_exp 7 ] ) )
        ; test_eval_err "wrong replacement length 2"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 1; int_exp 3 ]
                , Combine [ int_exp 9; int_exp 8 ] ) )
        ; test_eval_err "wrong replacement length 3"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , Combine [ int_exp 9; int_exp 8 ] ) )
        ; test_eval_err "wrong replacement length 4"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 2; int_exp 0; int_exp 1 ]
                , Combine [ int_exp 9; int_exp 8; int_exp 7 ] ) )
        ; test_eval_err "mixing with negatives"
            ( Eval.Mixing_with_negative_subscripts
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Combine [ int_exp 1; int_exp ~-1 ]
                , int_exp 13 ) )
        ; test_eval_err "mixed types"
            ( Eval.type_error T_Int T_Bool
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 2; int_exp 3 ]
                , false_exp ) )
        ] )
    ; ( "subset1_assign.negative"
      , [ test_eval "single index"
            ( vec_of_intlist [ 1; 10; 11; 12; 13 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , int_exp ~-1
                , Combine [ int_exp 10; int_exp 11; int_exp 12; int_exp 13 ] ) )
        ; test_eval "single index and recycle replacement 1"
            ( vec_of_intlist [ 1; 0; 0; 0; 0 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (int_exp 1)
                , int_exp 0 ) )
        ; test_eval "single index and recycle replacement 2"
            ( vec_of_intlist [ 1; 10; 11; 10; 11 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (int_exp 1)
                , Combine [ int_exp 10; int_exp 11 ] ) )
        ; test_eval "multiple index"
            ( vec_of_intlist [ 1; 10; 3; 11; 12 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; int_exp 3 ])
                , Combine [ int_exp 10; int_exp 11; int_exp 12 ] ) )
        ; test_eval "multiple index and recycle replacement"
            ( vec_of_intlist [ 1; 0; 3; 0; 0 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; int_exp 3 ])
                , Combine [ int_exp 0 ] ) )
        ; test_eval "multiple index with out-of-bounds"
            ( vec_of_intlist [ 1; 10; 3; 11; 12 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 3; int_exp 0; int_exp 1; int_exp 10 ])
                , Combine [ int_exp 10; int_exp 11; int_exp 12 ] ) )
        ; test_eval "multiple index with out-of-bounds and recycle replacement"
            ( vec_of_intlist [ 1; 0; 3; 0; 0 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 3; int_exp 0; int_exp 1; int_exp 10 ])
                , Combine [ int_exp 0 ] ) )
        ; test_eval "multiple index with repeats"
            ( vec_of_intlist [ 1; 0; 3; 0; 0 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 3; int_exp 1; int_exp 1 ])
                , Combine [ int_exp 0 ] ) )
        ; test_eval "multiple index with repeats and recycle replacement"
            ( vec_of_intlist [ 1; 10; 3; 11; 12 ]
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 3; int_exp 1; int_exp 1 ])
                , Combine [ int_exp 10; int_exp 11; int_exp 12 ] ) )
        ] )
    ; ( "subset1_assign.negative.err"
      , [ test_eval_err "NA index"
            ( Eval.No_NAs_in_subscripted_assignment
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; na_exp T_Int ])
                , int_exp 13 ) )
        ; test_eval_err "empty replacement"
            ( Eval.Replacement_length_is_zero
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (int_exp 1)
                , Subset1 (int_exp 1, int_exp 0) ) )
        ; test_eval_err "wrong replacement length 1"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (int_exp 1)
                , Combine [ int_exp 10; int_exp 11; int_exp 12 ] ) )
        ; test_eval_err "wrong replacement length 2"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; int_exp 3 ])
                , Combine [ int_exp 10; int_exp 11 ] ) )
        ; test_eval_err "wrong replacement length 3"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; int_exp 3 ])
                , Combine [ int_exp 10; int_exp 11; int_exp 12; int_exp 13 ] ) )
        ; test_eval_err "ignored index and wrong replacement length 1"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; int_exp 0; int_exp 3; int_exp 10 ])
                , Combine [ int_exp 10; int_exp 11 ] ) )
        ; test_eval_err "ignored index and wrong replacement length 2"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; int_exp 0; int_exp 3; int_exp 10 ])
                , Combine [ int_exp 10; int_exp 11; int_exp 12; int_exp 13 ] ) )
        ; test_eval_err "repeated index and wrong replacement length 1"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; int_exp 3; int_exp 1 ])
                , Combine [ int_exp 10; int_exp 11 ] ) )
        ; test_eval_err "repeated index and wrong replacement length 2"
            ( Eval.Replacement_length_not_multiple
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; int_exp 3; int_exp 1 ])
                , Combine [ int_exp 10; int_exp 11; int_exp 12; int_exp 13 ] ) )
        ; test_eval_err "mixing with negatives"
            ( Eval.Mixing_with_negative_subscripts
            , Subset1_Assign
                ( Combine [ int_exp 1; int_exp 2; int_exp 3; int_exp 4; int_exp 5 ]
                , Negate (Combine [ int_exp 1; int_exp ~-1 ])
                , int_exp 13 ) )
        ; test_eval_err "mixed types"
            ( Eval.type_error T_Int T_Bool
            , Subset1_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Negate (Combine [ int_exp 1; int_exp 2; int_exp 3 ])
                , false_exp ) )
        ] )
    ; ( "subset2_assign"
      , [ test_eval "int vector 1"
            ( vec_of_intlist [ 9; 12; 13; 14 ]
            , Subset2_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1 ]
                , Combine [ int_exp 9 ] ) )
        ; test_eval "int vector 2"
            ( vec_of_intlist [ 11; 12; 13; 9 ]
            , Subset2_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 4 ]
                , Combine [ int_exp 9 ] ) )
        ; test_eval "extension 1"
            ( vec_of_intoptlist [ Some 11; Some 12; Some 13; Some 14; None; None; Some 9 ]
            , Subset2_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 7, int_exp 9)
            )
        ; test_eval "extension 2"
            ( vec_of_intlist [ 9 ]
            , Subset2_Assign (Subset1 (int_exp 1, int_exp 0), int_exp 1, int_exp 9) )
        ] )
    ; ( "subset2_assign.err"
      , [ test_eval_err "index 0"
            ( Eval.Selecting_lt_one_element
            , Subset2_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp 0, int_exp 9)
            )
        ; test_eval_err "empty vector as index"
            ( Eval.Selecting_lt_one_element
            , Subset2_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Subset1 (int_exp 1, int_exp 0)
                , int_exp 9 ) )
        ; test_eval_err "single negative index"
            ( Eval.Selecting_gt_one_element
            , Subset2_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], int_exp ~-1, int_exp 9)
            )
        ; test_eval_err "multiple index 1"
            ( Eval.Selecting_gt_one_element
            , Subset2_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Combine [ int_exp 1; int_exp 2 ]
                , int_exp 9 ) )
        ; test_eval_err "multiple index 2"
            ( Eval.Selecting_gt_one_element
            , Subset2_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , Negate (Combine [ int_exp 1; int_exp 2 ])
                , int_exp 9 ) )
        ; test_eval_err "out-of-bounds with NA"
            ( Eval.Subscript_out_of_bounds
            , Subset2_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], na_exp T_Int, int_exp 9)
            )
        ; test_eval_err "replacement vector too long"
            ( Eval.Too_many_elements_supplied
            , Subset2_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , int_exp 1
                , Combine [ int_exp 9; int_exp 8 ] ) )
        ; test_eval_err "replacement vector too short"
            ( Eval.Replacement_length_is_zero
            , Subset2_Assign
                ( Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ]
                , int_exp 1
                , Subset1 (int_exp 1, int_exp 0) ) )
        ; test_eval_err "mixed types 1"
            ( Eval.type_error T_Int T_Bool
            , Subset2_Assign
                (Combine [ int_exp 11; int_exp 12; int_exp 13; int_exp 14 ], true_exp, int_exp 8) )
        ; test_eval_err "mixed types 2"
            ( Eval.type_error T_Bool T_Int
            , Subset2_Assign
                (Combine [ true_exp; true_exp; false_exp; true_exp ], int_exp 1, int_exp 8) )
        ] )
    ]
