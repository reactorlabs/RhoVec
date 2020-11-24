open Lib
open Expr

(* When this executable is run with the environment variable `DUMP=file`, the test cases are
   dumped to `file` as an R script. Executing the R script ensures that evaluating the test cases
   as R code results in the same expected values. *)

(* TODO: see if some parser tests can be used here *)

module A = Alcotest

let pp_value ppf v = Fmt.pf ppf "%s" (show_val v)
let testable_value = A.testable pp_value equal_value

let excptn_to_string excptn =
  let open Eval in
  match excptn with
  | Subscript_out_of_bounds -> "subscript out of bounds"
  | Selecting_lt_one_element -> "attempt to select less than one element"
  | Selecting_gt_one_element -> "attempt to select more than one element"
  | Mixing_with_negative_subscripts -> "only 0's may be mixed with negative subscripts"
  | No_NAs_in_subscripted_assignment -> "NAs are not allowed in subscripted assignments"
  | Replacement_length_not_multiple ->
      "number of items to replace is not a multiple of replacement length"
  | Replacement_length_is_zero -> "replacement has length zero"
  | Too_many_elements_supplied -> "more elements supplied than there are to replace"
  | Object_not_found -> "not found"
  | e ->
      Stdlib.prerr_endline "Unrecognized exception" ;
      raise e

(* If we're dumping to an R script, open the file and output the test harness code. *)
let dump_file =
  match Sys.getenv_opt "DUMP" with
  | None -> None
  | Some file ->
      let fout = Stdlib.open_out file in
      let pf = Printf.fprintf fout in
      pf "### We use eval and substitute to keep the global environment clean.\n" ;
      pf "runtest <- function(expected, expr) {\n" ;
      pf "  stopifnot(identical( eval(substitute(expected)), eval(substitute(expr)) ))\n}\n\n" ;
      pf "### To check for errors, we grep the error message.\n" ;
      pf "runerr <- function(excptn, expr) {\n" ;
      pf "  stopifnot(tryCatch(\n" ;
      pf "    eval(substitute(expr)),\n" ;
      pf "    finally = FALSE,\n" ;
      pf "    error = function(c) grepl(excptn, conditionMessage(c), fixed = TRUE)\n  ))\n}\n\n" ;
      pf "runwarn <- function(excptn, expr) {\n" ;
      pf "  stopifnot(tryCatch(\n" ;
      pf "    eval(substitute(expr)),\n" ;
      pf "    finally = FALSE,\n" ;
      pf "    warning = function(c) grepl(excptn, conditionMessage(c), fixed = TRUE)\n  ))\n}\n\n" ;
      Some fout

(* Create a test case that parses `input`, evaluates the expression, and checks that it matches
   the `expected` value. *)
let test_eval desc (expected, input) =
  (* Also check that all elements of the vector have the right type. *)
  let assert_vec_elt_types = function
    | Vector (a, t) as vec ->
        if not (Array.for_all (fun x -> get_tag x = t) a) then
          A.failf "Vector `%s` not consistent with its type!" (show_val vec) in
  let expr = Parse.parse input in
  let res = Eval.run expr in
  let check_res () =
    assert_vec_elt_types expected ;
    assert_vec_elt_types res ;
    A.(check testable_value) "same value" expected res in
  ( match dump_file with
  | None -> ()
  | Some fout ->
      Printf.fprintf fout "# %s\n" desc ;
      Printf.fprintf fout "runtest(%s, %s)\n\n" (Deparse.val_to_r res) (Deparse.to_r expr) ) ;
  A.test_case desc `Quick check_res

(* Create a test case that parses `input` and expects evaluating the expression to throw an
   exception.
   For the R tests, the harness uses catches the error (or warning) and greps the message for
   expected phrases.

   Optional argument `is_r` is set to false if we don't want to validate a test case against R.
   Optional argument `is_r_warning` is set to true if a RhoVec error should be an R warning. *)
let test_eval_err desc ?(is_r = true) ?(is_r_warning = false) (excptn, input) =
  let expr = Parse.parse input in
  let run_eval () = A.check_raises "same exception" excptn (fun _ -> ignore (Eval.run expr)) in
  ( match dump_file with
  | None -> ()
  | Some fout ->
      if is_r then (
        let tester = if not is_r_warning then "runerr" else "runwarn" in
        Printf.fprintf fout "# ERROR: %s\n" desc ;
        Printf.fprintf fout "%s(\"%s\", %s)\n\n" tester (excptn_to_string excptn)
          (Deparse.to_r expr) ) ) ;
  A.test_case desc `Quick run_eval

let () =
  A.run "eval-testsuite"
    (* The dummy test is so that we have something to run, when generating the R test suite. *)
    [ ("dummy", [ A.test_case "dummy test" `Quick (fun () -> A.(check int) "same value" 1 1) ])
    ; ( "literals"
      , [ test_eval "int 42" (vec_of_int 42, "42")
        ; test_eval "int -1" (vec_of_int ~-1, "-1")
        ; test_eval "int NA" (vec_of_intoptlist [ None ], "NA_i")
        ; test_eval "bool true" (vec_of_bool true, "T")
        ; test_eval "bool false" (vec_of_bool false, "F")
        ; test_eval "bool NA" (vec_of_booloptlist [ None ], "NA_b")
        ] )
    ; ( "combine"
      , [ test_eval "single int" (vec_of_int 42, "Combine(42)")
        ; test_eval "multiple int" (vec_of_intlist [ 1; 2; 3 ], "Combine(1, 2, 3)")
        ; test_eval "nested int"
            (vec_of_intlist [ 5; 6; 7; 8; 9 ], "Combine(5, Combine(6, 7, Combine(8), 9))")
        ; test_eval "nested int with empty int vector"
            (vec_of_intlist [ 5; 6; 7; 9 ], "Combine(5, Combine(6, 7, 8[0]), 9)")
        ; test_eval "nested int with NA"
            ( vec_of_intoptlist [ Some 5; None; Some 7; Some 8; Some 9 ]
            , "Combine(5, Combine(NA_i, 7, Combine(8), 9))" )
        ; test_eval "single bool" (vec_of_bool true, "Combine(T)")
        ; test_eval "multiple bool" (vec_of_boollist [ true; false; true ], "Combine(T, F, T)")
        ; test_eval "nested bool"
            ( vec_of_boollist [ true; true; false; false; true ]
            , "Combine(T, Combine(T, F, Combine(F), T))" )
        ; test_eval "nested bool with empty bool vector"
            (vec_of_boollist [ true; true; false; true ], "Combine(T, Combine(T, F, F[0]), T)")
        ; test_eval "nested bool with NA"
            ( vec_of_booloptlist [ None; Some true; Some false; Some false; Some true ]
            , "Combine(NA_b, Combine(T, F, Combine(F), T))" )
        ] )
    ; ( "combine.err"
      , [ test_eval_err "empty" ~is_r:false (Eval.Expected_nonempty_vector, "Combine()")
        ; test_eval_err "mixed types 1" ~is_r:false
            (Eval.type_error T_Int T_Bool, "Combine(0, T, F)")
        ; test_eval_err "mixed types 2" ~is_r:false
            (Eval.type_error T_Bool T_Int, "Combine(F, T, 1, F)")
        ] )
    ; ( "negate"
      , [ test_eval "positives" (vec_of_intlist [ 0; ~-1; ~-2; ~-3; ~-4 ], "-Combine(0, 1, 2, 3, 4)")
        ; test_eval "negatives" (vec_of_intlist [ 0; 1; 2; 3; 4 ], "-Combine(-0, -1, -2, -3, -4)")
        ; test_eval "mixed" (vec_of_intlist [ ~-1; 2; 3; ~-4 ], "-Combine(1, -2, -3, 4)")
        ; test_eval "empty" (empty_vec T_Int, "-0[0]")
        ; test_eval "NAs"
            ( vec_of_intoptlist [ Some ~-1; None; Some 2; Some 3; Some ~-4; None ]
            , "-Combine(1, NA_i, -2, -3, 4, NA_i)" )
        ; test_eval "nested" (vec_of_int ~-1, "---1")
        ] )
    ; ( "negate.err"
      , [ test_eval_err "bool" ~is_r:false (Eval.type_error T_Int T_Bool, "-Combine(T, F)") ] )
    ; ( "subset1_nothing"
      , [ test_eval "int" (vec_of_int 2, "2[]")
        ; test_eval "empty int" (empty_vec T_Int, "0[0][]")
        ; test_eval "multiple ints" (vec_of_intlist [ 3; 1; 4 ], "Combine(3, 1, 4)[]")
        ; test_eval "nested ints" (vec_of_intlist [ 5; 6; 7 ], "Combine(5, Combine(6, 7))[]")
        ; test_eval "nested ints with NAs"
            ( vec_of_intoptlist [ Some 5; None; Some 6; Some 7; None ]
            , "Combine(5, NA_i, Combine(6, 7, NA_i))[]" )
        ; test_eval "empty bool" (empty_vec T_Bool, "T[0][]")
        ] )
    ; ( "subset1_nothing.err"
      , [ test_eval_err "mixed types" ~is_r:false (Eval.type_error T_Int T_Bool, "Combine(0, F)[]")
        ] )
    ; ( "subset1.logical"
      , [ test_eval "int vector 1"
            (vec_of_intlist [ 1; 4 ], "Combine(1, 2, 3, 4)[Combine(T, F, F, T)]")
        ; test_eval "int vector 2"
            (vec_of_intlist [ 1; 2; 3; 4 ], "Combine(1, 2, 3, 4)[Combine(T, T, T, T)]")
        ; test_eval "int vector 3" (empty_vec T_Int, "Combine(1, 2, 3, 4)[Combine(F, F, F, F)]")
        ; test_eval "int vector with NA index"
            ( vec_of_intoptlist [ Some 1; None; Some 4 ]
            , "Combine(1, 2, 3, 4)[Combine(T, NA_b, F, T)]" )
        ] )
    ; ( "subset1.logical.extension"
      , [ test_eval "int vector 1"
            ( vec_of_intoptlist [ Some 11; Some 12; None; None ]
            , "Combine(11, 12)[Combine(T, T, T, T)]" )
        ; test_eval "int vector 2"
            (vec_of_intoptlist [ Some 11; None ], "Combine(11, 12)[Combine(T, F, F, T)]")
        ; test_eval "int vector 3"
            (vec_of_intoptlist [ Some 11; None; None ], "Combine(11, 12)[Combine(T, NA_b, F, T)]")
        ; test_eval "empty int vector" (vec_of_intoptlist [ None; None ], "(0[0])[Combine(T, T)]")
        ] )
    ; ( "subset1.logical.recycling"
      , [ test_eval "int vector 1"
            (vec_of_intlist [ 11; 12; 13; 14 ], "Combine(11, 12, 13, 14)[Combine(T)]")
        ; test_eval "int vector 2" (empty_vec T_Int, "Combine(11, 12, 13, 14)[Combine(F)]")
        ; test_eval "int vector 3"
            (vec_of_intlist [ 11; 13 ], "Combine(11, 12, 13, 14)[Combine(T, F)]")
        ; test_eval "int vector 4"
            ( vec_of_intoptlist [ Some 11; None; Some 13; None ]
            , "Combine(11, 12, 13, 14)[Combine(T, NA_b)]" )
        ; test_eval "int vector 5"
            ( vec_of_intoptlist [ Some 11; Some 12; None; Some 14 ]
            , "Combine(11, 12, 13, 14)[Combine(T, T, NA_b)]" )
        ; test_eval "int vector 6"
            (vec_of_intoptlist [ None; None; None; None ], "Combine(11, 12, 13, 14)[Combine(NA_b)]")
        ] )
    ; ( "subset1.zero"
      , [ test_eval "int vector 1" (empty_vec T_Int, "7[0]")
        ; test_eval "int vector 2" (empty_vec T_Int, "Combine(7, 6, 5)[0]")
        ; test_eval "int vector 3" (empty_vec T_Int, "Combine(7, 6, 5)[Combine(0)]")
        ; test_eval "empty int vector" (empty_vec T_Int, "(0[0])[0]")
        ; test_eval "NA int vector" (empty_vec T_Int, "Combine(NA_i, NA_i, NA_i)[0]")
        ] )
    ; ( "subset1.positive"
      , [ test_eval "index 1" (vec_of_int 11, "Combine(11, 12, 13, 14)[1]")
        ; test_eval "index 2" (vec_of_int 12, "Combine(11, 12, 13, 14)[2]")
        ; test_eval "index 3" (vec_of_int 13, "Combine(11, 12, 13, 14)[3]")
        ; test_eval "index 4" (vec_of_int 14, "Combine(11, 12, 13, 14)[4]")
        ; test_eval "index 5 out-of-bounds" (vec_of_na T_Int, "Combine(11, 12, 13, 14)[5]")
        ; test_eval "index NA" (vec_of_na T_Int, "Combine(11, 12, 13, 14)[NA_i]")
        ; test_eval "multiple indexes"
            (vec_of_intlist [ 11; 13 ], "Combine(11, 12, 13, 14)[Combine(1, 3)]")
        ; test_eval "multiple indexes and ignored zero"
            (vec_of_intlist [ 11; 13 ], "Combine(11, 12, 13, 14)[Combine(0, 1, 3)]")
        ; test_eval "multiple indexes and out-of-bounds"
            ( vec_of_intoptlist [ Some 11; Some 13; None ]
            , "Combine(11, 12, 13, 14)[Combine(1, 3, 5)]" )
        ; test_eval "multiple indexes repeated"
            ( vec_of_intlist [ 13; 13; 12; 14; 13 ]
            , "Combine(11, 12, 13, 14)[Combine(3, 3, 2, 4, 3)]" )
        ; test_eval "multiple indexes with NA"
            ( vec_of_intoptlist [ Some 12; None; Some 11 ]
            , "Combine(11, 12, 13, 14)[Combine(2, NA_i, 1)]" )
        ] )
    ; ( "subset1.negative"
      , [ test_eval "exclude 1" (vec_of_intlist [ 12; 13; 14 ], "Combine(11, 12, 13, 14)[-1]")
        ; test_eval "exclude 2" (vec_of_intlist [ 11; 13; 14 ], "Combine(11, 12, 13, 14)[-2]")
        ; test_eval "exclude 3" (vec_of_intlist [ 11; 12; 14 ], "Combine(11, 12, 13, 14)[-3]")
        ; test_eval "exclude 4" (vec_of_intlist [ 11; 12; 13 ], "Combine(11, 12, 13, 14)[-4]")
        ; test_eval "exclude 5 out-of-bounds"
            (vec_of_intlist [ 11; 12; 13; 14 ], "Combine(11, 12, 13, 14)[-5]")
        ; test_eval "exclude all"
            (empty_vec T_Int, "Combine(11, 12, 13, 14)[Combine(-1, -2, -3, -4)]")
        ; test_eval "exclude multiple"
            (vec_of_intlist [ 13; 14 ], "Combine(11, 12, 13, 14)[-Combine(2, 1)]")
        ; test_eval "exclude multiple and ignored zero"
            (vec_of_intlist [ 13; 14 ], "Combine(11, 12, 13, 14)[-Combine(1, 2, 0)]")
        ; test_eval "exclude multiple and out-of-bounds"
            (vec_of_intlist [ 13; 14 ], "Combine(11, 12, 13, 14)[-Combine(1, 5, 2, 0)]")
        ; test_eval "exclude multiple repeated"
            (vec_of_intlist [ 12; 13; 14 ], "Combine(11, 12, 13, 14)[-Combine(1, 1)]")
        ] )
    ; ( "subset1.err"
      , [ test_eval_err "mixing subscripts 1"
            ( Eval.Mixing_with_negative_subscripts
            , "Combine(11, 12, 13, 14)[Combine(0, 1, NA_i, -3)]" )
        ; test_eval_err "mixing subscripts 2"
            (Eval.Mixing_with_negative_subscripts, "Combine(11, 12, 13, 14)[-Combine(4, -1, 0)]")
        ; test_eval_err "mixing subscripts 3"
            (Eval.Mixing_with_negative_subscripts, "Combine(11, 12, 13, 14)[-Combine(4, NA_i, 0)]")
        ] )
    ; ( "subset2"
      , [ test_eval "index 1" (vec_of_int 1, "Combine(1, 2, 3, NA_i)[[1]]")
        ; test_eval "index 2" (vec_of_int 2, "Combine(1, 2, 3, NA_i)[[2]]")
        ; test_eval "index 3" (vec_of_int 3, "Combine(1, 2, 3, NA_i)[[3]]")
        ; test_eval "index 4" (vec_of_na T_Int, "Combine(1, 2, 3, NA_i)[[4]]")
        ] )
    ; ( "subset2.err"
      , [ test_eval_err "index 0" (Eval.Selecting_lt_one_element, "Combine(1, 2, 3)[[0]]")
        ; test_eval_err "empty vector as index"
            (Eval.Selecting_lt_one_element, "Combine(1, 2, 3)[[1[0]]]")
        ; test_eval_err "single negative index"
            (Eval.Selecting_gt_one_element, "Combine(1, 2, 3)[[-1]]")
        ; test_eval_err "multiple index 1"
            (Eval.Selecting_gt_one_element, "Combine(1, 2, 3)[[-Combine(1, 2)]]")
        ; test_eval_err "multiple index 2"
            (Eval.Selecting_gt_one_element, "Combine(1, 2, 3)[[Combine(1, 2)]]")
        ; test_eval_err "out-of-bounds 1" (Eval.Subscript_out_of_bounds, "Combine(1, 2, 3)[[4]]")
        ; test_eval_err "out-of-bounds 2" (Eval.Subscript_out_of_bounds, "(1[0])[[1]]")
        ; test_eval_err "out-of-bounds with NA"
            (Eval.Subscript_out_of_bounds, "Combine(1, 2, 3, 4)[[NA_i]]")
        ; test_eval_err "mixed types" ~is_r:false
            (Eval.type_error T_Int T_Bool, "Combine(1, 2, 3)[[T]]")
        ] )
    ; ( "subset1_nothing_assign"
      , [ test_eval "single" (vec_of_intlist [ 0; 0; 0 ], "x <- Combine(11, 12, 13); x[] <- 0; x")
        ; test_eval "NA"
            ( vec_of_intoptlist [ None; None; None; None ]
            , "x <- Combine(11, 12, 13, 14); x[] <- NA_i; x" )
        ; test_eval "multiple 1"
            (vec_of_intlist [ 1; 2; 1; 2 ], "x <- Combine(11, 12, 13, 14); x[] <- Combine(1, 2); x")
        ; test_eval "multiple 2"
            ( vec_of_intlist [ 1; 2; 3; 4 ]
            , "x <- Combine(11, 12, 13, 14); x[] <- Combine(1, 2, 3, 4); x" )
        ; test_eval "return value 1" (vec_of_int 0, "x <- Combine(11, 12, 13); x[] <- 0")
        ; test_eval "return value 2" (vec_of_na T_Int, "x <- Combine(11, 12, 13, 14); x[] <- NA_i")
        ; test_eval "return value 3"
            (vec_of_intlist [ 1; 2 ], "x <- Combine(11, 12, 13, 14); x[] <- Combine(1, 2)")
        ] )
    ; ( "subset1_nothing_assign.err"
      , [ test_eval_err "wrong length 1" ~is_r_warning:true
            (Eval.Replacement_length_not_multiple, "x <- Combine(11, 12, 13); x[] <- Combine(1, 2)")
        ; test_eval_err "wrong length 2" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(11, 12, 13); x[] <- Combine(1, 2, 3, 4)" )
        ; test_eval_err "wrong length 3" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(11, 12, 13); x[] <- Combine(1, 2, 3, 4, 5, 6)" )
        ; test_eval_err "empty replacement"
            (Eval.Replacement_length_is_zero, "x <- Combine(11, 12, 13); x[] <- 1[0]")
        ; test_eval_err "mixed types" ~is_r:false
            (Eval.type_error T_Int T_Bool, "x <- Combine(11, 12, 13); x[] <- T")
        ] )
    ; ( "subset1_assign.logical"
      , [ test_eval "no recycling or extension 1"
            ( vec_of_intlist [ 1; 12; 13; 14; 5 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[Combine(F, T, T, T, F)] <- Combine(12, 13, 14); x" )
        ; test_eval "no recycling or extension 2"
            ( vec_of_intlist [ 9; 8; 3 ]
            , "x <- Combine(1, 2, 3); x[Combine(T, T, F)] <- Combine(9, 8); x" )
        ; test_eval "recycle replacement"
            ( vec_of_intlist [ 1; 0; 0; 0; 5 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[Combine(F, T, T, T, F)] <- Combine(0); x" )
        ; test_eval "recycle index"
            ( vec_of_intlist [ 11; 2; 12; 13; 5 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[Combine(T, F, T)] <- Combine(11, 12, 13); x" )
        ; test_eval "recycle index and replacement 1"
            ( vec_of_intlist [ 0; 0; 0; 0; 0 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[Combine(T)] <- Combine(0); x" )
        ; test_eval "recycle index and replacement 2"
            ( vec_of_intlist [ 0; 2; 0; 0; 5 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[Combine(T, F, T)] <- Combine(0); x" )
        ; test_eval "recycle index and replacement 3"
            ( vec_of_intlist [ 10; 11; 10; 11; 10; 11 ]
            , "x <- Combine(1, 2, 3, 4, 5, 6); x[Combine(T)] <- Combine(10, 11); x" )
        ; test_eval "recycle index and replacement 4"
            ( vec_of_intlist [ 10; 2; 11; 4; 12; 6 ]
            , "x <- Combine(1, 2, 3, 4, 5, 6); x[Combine(T, F)] <- Combine(10, 11, 12); x" )
        ; test_eval "extension 1"
            ( vec_of_intlist [ 11; 12; 13; 14 ]
            , "x <- Combine(1, 2, 3); x[Combine(T, T, T, T)] <- Combine(11, 12, 13, 14); x" )
        ; test_eval "extension 2"
            ( vec_of_intlist [ 11; 12; 3; 13 ]
            , "x <- Combine(1, 2, 3); x[Combine(T, T, F, T)] <- Combine(11, 12, 13); x" )
        ; test_eval "extension 3"
            ( vec_of_intoptlist [ Some 11; Some 12; Some 13; Some 14; None ]
            , "x <- Combine(1, 2, 3); x[Combine(T, T, T, T, F)] <- Combine(11, 12, 13, 14); x" )
        ; test_eval "extension and recycle replacement 1"
            ( vec_of_intoptlist [ Some 0; Some 0; Some 0; Some 0; None ]
            , "x <- Combine(1, 2, 3); x[Combine(T, T, T, T, F)] <- Combine(0); x" )
        ; test_eval "extension and recycle replacement 2"
            ( vec_of_intoptlist [ Some 9; Some 8; Some 9; Some 8; None ]
            , "x <- Combine(1, 2, 3); x[Combine(T, T, T, T, F)] <- Combine(9, 8); x" )
        ; test_eval "extension and recycle replacement 3"
            ( vec_of_intoptlist [ Some 9; Some 8; Some 7; Some 6; None ]
            , "x <- Combine(1, 2, 3); x[Combine(T, T, T, T, F)] <- Combine(9, 8, 7, 6); x" )
        ; test_eval "return value 1"
            (vec_of_intlist [ 9; 8 ], "x <- Combine(1, 2, 3); x[Combine(T, T, F)] <- Combine(9, 8)")
        ; test_eval "return value 2"
            ( vec_of_intlist [ 10; 11; 12 ]
            , "x <- Combine(1, 2, 3, 4, 5, 6); x[Combine(T, F)] <- Combine(10, 11, 12)" )
        ; test_eval "return value 3"
            ( vec_of_intlist [ 11; 12; 13; 14 ]
            , "x <- Combine(1, 2, 3); x[Combine(T, T, T, T, F)] <- Combine(11, 12, 13, 14)" )
        ; test_eval "return value 4"
            ( vec_of_intlist [ 9; 8; 7; 6 ]
            , "x <- Combine(1, 2, 3); x[Combine(T, T, T, T, F)] <- Combine(9, 8, 7, 6)" )
        ] )
    ; ( "subset1_assign.logical.err"
      , [ test_eval_err "empty replacement"
            (Eval.Replacement_length_is_zero, "x <- Combine(1, 2, 3, 4, 5); x[Combine(T)] <- 1[0]")
        ; test_eval_err "wrong replacement length 1" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5); x[Combine(T)] <- Combine(10, 11)" )
        ; test_eval_err "wrong replacement length 2" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5, 6); x[Combine(T, F)] <- Combine(10, 11)" )
        ; test_eval_err "wrong replacement length 3" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5, 6); x[Combine(T, F, F)] <- Combine(10, 11, 12)" )
        ; test_eval_err "wrong replacement length 4" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3); x[Combine(T, T, F)] <- Combine(10, 11, 12)" )
        ; test_eval_err "extension and recycle replacement 3" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3); x[Combine(T, T, T, T, F)] <- Combine(9, 8, 7)" )
        ; test_eval_err "NA in index" ~is_r:false
            (* R allows this because the RHS has only one element *)
            ( Eval.No_NAs_in_subscripted_assignment
            , "x <- Combine(1, 2, 3, 4, 5); x[Combine(T, F, NA_b)] <- Combine(0)" )
        ; test_eval_err "mixed types" ~is_r:false
            ( Eval.type_error T_Int T_Bool
            , "x <- Combine(1, 2, 3, 4, 5); x[Combine(T, F)] <- Combine(F)" )
        ] )
    ; ( "subset1_assign.zero"
      , [ test_eval "int vector 1" (vec_of_int 7, "x <- 7; x[0] <- 42; x")
        ; test_eval "int vector 2" (vec_of_int 7, "x <- 7; x[0] <- NA_i; x")
        ; test_eval "empty int vector" (empty_vec T_Int, "x <- 0[0]; x[0] <- 1; x")
        ; test_eval "empty index vector" (vec_of_int 7, "x <- 7; x[0[0]] <- 1; x")
        ; test_eval "ignore wrong type" (vec_of_int 7, "x <- 7; x[0] <- T; x")
        ; test_eval "return value 1" (vec_of_na T_Int, "x <- 7; x[0] <- NA_i")
        ; test_eval "return value 2" (vec_of_int 1, "x <- 0[0]; x[0] <- 1")
        ; test_eval "return value 3" (vec_of_int 1, "x <- 7; x[0[0]] <- 1")
        ; test_eval "return value 4" (vec_of_bool true, "x <- 7; x[0[0]] <- T")
        ] )
    ; ( "subset1_assign.zero.err"
      , [ test_eval_err "NA in index" ~is_r:false
            (* R allows this because the RHS has only one element *)
            ( Eval.No_NAs_in_subscripted_assignment
            , "x <- Combine(1, 2, 3, 4); x[Combine(0, NA_i)] <- Combine(0)" )
        ] )
    ; ( "subset1_assign.positive"
      , [ test_eval "single index"
            (vec_of_intlist [ 0; 12; 13; 14 ], "x <- Combine(11, 12, 13, 14); x[1] <- 0; x")
        ; test_eval "single index extension"
            ( vec_of_intoptlist [ Some 11; Some 12; Some 13; Some 14; None; None; None; Some 0 ]
            , "x <- Combine(11, 12, 13, 14); x[8] <- 0; x" )
        ; test_eval "multiple indexes"
            ( vec_of_intlist [ 9; 8; 13; 14 ]
            , "x <- Combine(11, 12, 13, 14); x[Combine(1, 2)] <- Combine(9, 8); x" )
        ; test_eval "multiple indexes with repeats 1"
            ( vec_of_intlist [ 8; 7; 13; 14 ]
            , "x <- Combine(11, 12, 13, 14); x[Combine(1, 1, 2)] <- Combine(9, 8, 7); x" )
        ; test_eval "multiple indexes with repeats 2"
            ( vec_of_intlist [ 8; 9; 8; 14; 15; 16 ]
            , "x <- Combine(11, 12, 13, 14, 15, 16); x[Combine(1, 1, 2, 3)] <- Combine(9, 8); x" )
        ; test_eval "multiple indexes extension"
            ( vec_of_intlist [ 0; 12; 9; 8 ]
            , "x <- Combine(11, 12); x[Combine(1, 3, 4)] <- Combine(0, 9, 8); x" )
        ; test_eval "multiple indexes recycling 1"
            ( vec_of_intlist [ 9; 9; 13; 14 ]
            , "x <- Combine(11, 12, 13, 14); x[Combine(1, 2)] <- Combine(9); x" )
        ; test_eval "multiple indexes recycling 2"
            ( vec_of_intlist [ 9; 8; 9; 8; 15; 16 ]
            , "x <- Combine(11, 12, 13, 14, 15, 16); x[Combine(1, 2, 3, 4)] <- Combine(9, 8); x" )
        ; test_eval "multiple indexes with zero"
            ( vec_of_intlist [ 8; 9; 13; 14 ]
            , "x <- Combine(11, 12, 13, 14); x[Combine(2, 0, 1)] <- Combine(9, 8); x" )
        ; test_eval "return value 1"
            ( vec_of_intlist [ 9; 8 ]
            , "x <- Combine(11, 12, 13, 14); x[Combine(1, 2)] <- Combine(9, 8)" )
        ; test_eval "return value 2"
            ( vec_of_intlist [ 0; 9; 8 ]
            , "x <- Combine(11, 12); x[Combine(1, 3, 4)] <- Combine(0, 9, 8)" )
        ; test_eval "return value 3"
            ( vec_of_intlist [ 9; 8 ]
            , "x <- Combine(11, 12, 13, 14, 15, 16); x[Combine(1, 2, 3, 4)] <- Combine(9, 8)" )
        ] )
    ; ( "subset1_assign.positive.err"
      , [ test_eval_err "NA index 1" ~is_r:false
            (* R allows this because the RHS has only one element *)
            (Eval.No_NAs_in_subscripted_assignment, "x <- Combine(1, 2, 3, 4); x[NA_i] <- 0")
        ; test_eval_err "NA index 2"
            ( Eval.No_NAs_in_subscripted_assignment
            , "x <- Combine(1, 2, 3, 4); x[NA_i] <- Combine(10, 11)" )
        ; test_eval_err "NA index 3" ~is_r:false
            (* R allows this because the RHS has only one element *)
            ( Eval.No_NAs_in_subscripted_assignment
            , "x <- Combine(1, 2, 3, 4); x[Combine(1, NA_i)] <- 10" )
        ; test_eval_err "NA index 4"
            ( Eval.No_NAs_in_subscripted_assignment
            , "x <- Combine(1, 2, 3, 4); x[Combine(1, NA_i)] <- Combine(10, 11)" )
        ; test_eval_err "empty replacement"
            (Eval.Replacement_length_is_zero, "x <- Combine(1, 2, 3, 4); x[1] <- 1[0]")
        ; test_eval_err "wrong replacement length 1" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(11, 12, 13, 14); x[Combine(1, 2)] <- Combine(9, 8, 7)" )
        ; test_eval_err "wrong replacement length 2" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(11, 12, 13, 14); x[Combine(1, 1, 3)] <- Combine(9, 8)" )
        ; test_eval_err "wrong replacement length 3" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(11, 12, 13, 14); x[Combine(1, 2, 3)] <- Combine(9, 8)" )
        ; test_eval_err "wrong replacement length 4" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(11, 12, 13, 14); x[Combine(2, 0, 1)] <- Combine(9, 8, 7)" )
        ; test_eval_err "mixing with negatives"
            ( Eval.Mixing_with_negative_subscripts
            , "x <- Combine(1, 2, 3, 4, 5); x[Combine(1, -1)] <- 13" )
        ; test_eval_err "mixed types" ~is_r:false
            (Eval.type_error T_Int T_Bool, "x <- Combine(11, 12, 13, 14); x[Combine(1, 2, 3)] <- F")
        ] )
    ; ( "subset1_assign.negative"
      , [ test_eval "single index"
            ( vec_of_intlist [ 1; 10; 11; 12; 13 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[-1] <- Combine(10, 11, 12, 13); x" )
        ; test_eval "single index and recycle replacement 1"
            (vec_of_intlist [ 1; 0; 0; 0; 0 ], "x <- Combine(1, 2, 3, 4, 5); x[-1] <- 0; x")
        ; test_eval "single index and recycle replacement 2"
            ( vec_of_intlist [ 1; 10; 11; 10; 11 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[-1] <- Combine(10, 11); x" )
        ; test_eval "multiple index"
            ( vec_of_intlist [ 1; 10; 3; 11; 12 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, 3)] <- Combine(10, 11, 12); x" )
        ; test_eval "multiple index and recycle replacement"
            ( vec_of_intlist [ 1; 0; 3; 0; 0 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, 3)] <- Combine(0); x" )
        ; test_eval "multiple index with out-of-bounds"
            ( vec_of_intlist [ 1; 10; 3; 11; 12 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(3, 0, 1, 10)] <- Combine(10, 11, 12); x" )
        ; test_eval "multiple index with out-of-bounds and recycle replacement"
            ( vec_of_intlist [ 1; 0; 3; 0; 0 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(3, 0, 1, 10)] <- Combine(0); x" )
        ; test_eval "multiple index with repeats"
            ( vec_of_intlist [ 1; 0; 3; 0; 0 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(3, 1, 1)] <- Combine(0); x" )
        ; test_eval "multiple index with repeats and recycle replacement"
            ( vec_of_intlist [ 1; 10; 3; 11; 12 ]
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(3, 1, 1)] <- Combine(10, 11, 12); x" )
        ; test_eval "return value"
            (vec_of_intlist [ 10; 11 ], "x <- Combine(1, 2, 3, 4, 5); x[-1] <- Combine(10, 11)")
        ] )
    ; ( "subset1_assign.negative.err"
      , [ test_eval_err "NA index"
            ( Eval.Mixing_with_negative_subscripts
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, NA_i)] <- 13" )
        ; test_eval_err "empty replacement"
            (Eval.Replacement_length_is_zero, "x <- Combine(1, 2, 3, 4, 5); x[-1] <- 1[0]")
        ; test_eval_err "wrong replacement length 1" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5); x[-1] <- Combine(10, 11, 12)" )
        ; test_eval_err "wrong replacement length 2" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, 3)] <- Combine(10, 11)" )
        ; test_eval_err "wrong replacement length 3" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, 3)] <- Combine(10, 11, 12, 13)" )
        ; test_eval_err "ignored index and wrong replacement length 1" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, 0, 3, 10)] <- Combine(10, 11)" )
        ; test_eval_err "ignored index and wrong replacement length 2" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, 0, 3, 10)] <- Combine(10, 11, 12, 13)" )
        ; test_eval_err "repeated index and wrong replacement length 1" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, 3, 1)] <- Combine(10, 11)" )
        ; test_eval_err "repeated index and wrong replacement length 2" ~is_r_warning:true
            ( Eval.Replacement_length_not_multiple
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, 3, 1)] <- Combine(10, 11, 12, 13)" )
        ; test_eval_err "mixing with negatives"
            ( Eval.Mixing_with_negative_subscripts
            , "x <- Combine(1, 2, 3, 4, 5); x[-Combine(1, -1)] <- 13" )
        ; test_eval_err "mixed types" ~is_r:false
            (Eval.type_error T_Int T_Bool, "x <- Combine(11, 12, 13, 14); x[-Combine(1, 2, 3)] <- F")
        ] )
    ; ( "subset2_assign"
      , [ test_eval "int vector 1"
            ( vec_of_intlist [ 9; 12; 13; 14 ]
            , "x <- Combine(11, 12, 13, 14); x[[Combine(1)]] <- Combine(9); x" )
        ; test_eval "int vector 2"
            ( vec_of_intlist [ 11; 12; 13; 9 ]
            , "x <- Combine(11, 12, 13, 14); x[[Combine(4)]] <- Combine(9); x" )
        ; test_eval "extension 1"
            ( vec_of_intoptlist [ Some 11; Some 12; Some 13; Some 14; None; None; Some 9 ]
            , "x <- Combine(11, 12, 13, 14); x[[7]] <- 9; x" )
        ; test_eval "extension 2" (vec_of_intlist [ 9 ], "x <- 1[0]; x[[1]] <- 9; x")
        ; test_eval "return value"
            (vec_of_int 9, "x <- Combine(11, 12, 13, 14); x[[Combine(4)]] <- Combine(9)")
        ] )
    ; ( "subset2_assign.err"
      , [ test_eval_err "index 0"
            (Eval.Selecting_lt_one_element, "x <- Combine(11, 12, 13, 14); x[[0]] <- 9")
        ; test_eval_err "empty vector as index"
            (Eval.Selecting_lt_one_element, "x <- Combine(11, 12, 13, 14); x[[1[0]]] <- 9")
        ; test_eval_err "single negative index"
            (Eval.Selecting_gt_one_element, "x <- Combine(11, 12, 13, 14); x[[-1]] <- 9")
        ; test_eval_err "multiple index 1"
            (Eval.Selecting_gt_one_element, "x <- Combine(11, 12, 13, 14); x[[Combine(1, 2)]] <- 9")
        ; test_eval_err "multiple index 2"
            (Eval.Selecting_gt_one_element, "x <- Combine(11, 12, 13, 14); x[[-Combine(1, 2)]] <- 9")
        ; test_eval_err "assign with NA index"
            (Eval.Selecting_gt_one_element, "x <- Combine(11, 12, 13, 14); x[[NA_i]] <- 9")
        ; test_eval_err "replacement vector too long"
            ( Eval.Too_many_elements_supplied
            , "x <- Combine(11, 12, 13, 14); x[[1]] <- Combine(9, 8)" )
        ; test_eval_err "replacement vector too short"
            (Eval.Replacement_length_is_zero, "x <- Combine(11, 12, 13, 14); x[[1]] <- 1[0]")
        ; test_eval_err "mixed types 1" ~is_r:false
            (Eval.type_error T_Int T_Bool, "x <- Combine(11, 12, 13, 14); x[[T]] <- 8")
        ; test_eval_err "mixed types 2" ~is_r:false
            (Eval.type_error T_Bool T_Int, "x <- Combine(T, T, F, T); x[[1]] <- 8")
        ] )
    ; ( "environments"
      , [ test_eval "lookup 1" (vec_of_int 42, "x <- 42; x")
        ; test_eval "lookup 2" (vec_of_int 1, "x <- 42; x <- 1; x")
        ; test_eval "lookup 3" (vec_of_int 42, "x <- 42; y <- 1; z <- 2; x")
        ; test_eval "lookup 4" (vec_of_int 2, "x <- 42; y <- 1; z <- 2; z")
        ; test_eval "lookup 5"
            (vec_of_intlist [ 3; 2; 1 ], "x <- 1; y <- 2; z <- 3; Combine(z, y, x)")
        ; test_eval "assignment as expression 1"
            ( vec_of_intlist [ 11; 12; 13; 13; 12; 13; 13; 14; 14; 14; 14 ]
            , "Combine(a <- 11, b <- 12, c <- 13, d <- c, b, d, c, c <- 14, b <- c, c, b)" )
        ; test_eval "assignment as expression 2"
            ( vec_of_intlist [ 11; 12; 12; 2; 3; 13; 14; 12; 13; 14 ]
            , "x <- Combine(1, 2, 3); Combine(11, x[[1]] <- 12, x, x[Combine(2, 3)] <- Combine(13, \
               14), x)" )
        ; test_eval "assignment as expression 3"
            ( vec_of_intlist [ 11; 13; 14; 1; 13; 14; 12; 12; 13; 14 ]
            , "x <- Combine(1, 2, 3); Combine(11, x[Combine(2, 3)] <- Combine(13, 14), x, x[[1]] \
               <- 12, x)" )
        ] )
    ; ("environments.err", [ test_eval_err "object not found" (Eval.Object_not_found, "x") ])
    ; ( "seq"
      , [ test_eval "multiple expressions" (vec_of_int 3, "1; 2; 3")
        ; test_eval "nested" (vec_of_int 6, "{ 1; { 2; 3 }; { { 4; { 5 } }; 6 } }")
        ] )
    ]
