open Lib

module A = Alcotest

exception Expected_failure

let testable_expr = A.testable Expr.pp_expression Expr.equal_expression

let test_parse desc (expected, str) =
  let run_parser () =
    let ast = Parser.parse str in
    A.(check testable_expr) "same expression" expected ast in
  A.test_case desc `Quick run_parser

let test_parse_err desc str =
  let run_parser () =
    try
      ignore (Parser.parse str) ;
      raise Expected_failure
    with
    | Parser.Parse_error _ -> ()
    | Expected_failure -> A.failf "Expected a parse exception but got success!"
    | _ -> A.failf "Incorrect exception received!" in
  A.test_case desc `Quick run_parser

let () =
  let open Expr in
  A.run "parse-testsuite"
    [ ( "variable"
      , [ test_parse "x" (Var "x", "x")
        ; test_parse "x123" (Var "x123", "x123")
        ; test_parse "ab.c" (Var "ab.c", "ab.c")
        ; test_parse ".abc" (Var ".abc", ".abc")
        ; test_parse "ab_c" (Var "ab_c", "ab_c")
        ; test_parse "a.b_c" (Var "a.b_c", "a.b_c")
        ; test_parse "a.b1_c" (Var "a.b1_c", "a.b1_c")
        ; test_parse "Tt" (Var "Tt", "Tt")
        ; test_parse "F1" (Var "F1", "F1")
        ; test_parse "NA_bi" (Var "NA_bi", "NA_bi")
        ; test_parse "NA_i2" (Var "NA_i2", "NA_i2")
        ; test_parse "whitespace 1" (Var "x", " x")
        ; test_parse "whitespace 2" (Var "x", "x ")
        ; test_parse "whitespace 3" (Var "x", " x ")
        ; test_parse "whitespace 4" (Var "x", "\nx")
        ; test_parse "whitespace 5" (Var "x", "x\n")
        ; test_parse "whitespace 6" (Var "x", "\nx\n")
        ; test_parse "whitespace 7" (Var "x", " \n x\n ")
        ; test_parse "semicolon 1" (Var "x", "x;")
        ; test_parse "semicolon 2" (Var "x", "x ; ")
        ; test_parse "semicolon 3" (Var "x", "x \n; \n")
        ; test_parse_err "keyword" "Combine"
        ; test_parse_err "underscore 1" "_abc"
        ; test_parse_err "underscore 2" "_123"
        ] )
    ; ( "boolean"
      , [ test_parse "T" (true_exp, "T")
        ; test_parse "F" (false_exp, "F")
        ; test_parse "whitespace 1" (true_exp, " T")
        ; test_parse "whitespace 2" (true_exp, "T ")
        ; test_parse "whitespace 3" (true_exp, " T ")
        ; test_parse "whitespace 4" (true_exp, "\nT")
        ; test_parse "whitespace 5" (true_exp, "T\n")
        ; test_parse "whitespace 6" (true_exp, "\nT\n")
        ; test_parse "whitespace 7" (true_exp, " \n T\n ")
        ; test_parse "semicolon 1" (true_exp, "T;")
        ; test_parse "semicolon 2" (true_exp, "T ; ")
        ; test_parse "semicolon 3" (true_exp, "T \n; \n")
        ; test_parse "NA_b" (na_exp T_Bool, "NA_b")
        ] )
    ; ( "integer"
      , [ test_parse "0" (int_exp 0, "0")
        ; test_parse "01" (int_exp 1, "01")
        ; test_parse "42" (int_exp 42, "42")
        ; test_parse "NA_i" (na_exp T_Int, "NA_i")
        ; test_parse "whitespace 1" (int_exp 1, " 1")
        ; test_parse "whitespace 2" (int_exp 1, "1 ")
        ; test_parse "whitespace 3" (int_exp 1, " 1 ")
        ; test_parse "whitespace 4" (int_exp 1, "\n1")
        ; test_parse "whitespace 5" (int_exp 1, "1\n")
        ; test_parse "whitespace 6" (int_exp 1, "\n1\n")
        ; test_parse "whitespace 7" (int_exp 1, " \n 1\n ")
        ; test_parse "semicolon 1" (int_exp 1, "1;")
        ; test_parse "semicolon 2" (int_exp 1, "1 ; ")
        ; test_parse "semicolon 3" (int_exp 1, "1 \n; \n")
        ; test_parse_err "123xyz" "123xyz"
        ] )
    ; ( "combine"
      , [ test_parse "Combine(1)" (Combine [ int_exp 1 ], "Combine(1)")
        ; test_parse "Combine(1,x)" (Combine [ int_exp 1; Var "x" ], "Combine(1,x)")
        ; test_parse "Combine(Combine(Combine(1),2),Combine(3),4)"
            ( Combine
                [ Combine [ Combine [ int_exp 1 ]; int_exp 2 ]; Combine [ int_exp 3 ]; int_exp 4 ]
            , "Combine(Combine(Combine(1),2),Combine(3),4)" )
        ; test_parse "whitespace 1" (Combine [ int_exp 1; Var "x" ], " Combine ( 1 , x ) ")
        ; test_parse "whitespace 2" (Combine [ int_exp 1; Var "x" ], "\nCombine ( 1 , x )\n")
        ; test_parse "whitespace 3" (Combine [ int_exp 1; Var "x" ], "\n Combine ( 1 , x )\n ")
        ; test_parse "whitespace 4" (Combine [ int_exp 1; Var "x" ], "\n Combine (\n1\n,\nx\n)\n ")
        ; test_parse "semicolon 1" (Combine [ int_exp 1; Var "x" ], "Combine(1, x);")
        ; test_parse "semicolon 2" (Combine [ int_exp 1; Var "x" ], "Combine(1, x) ; ")
        ; test_parse "semicolon 3" (Combine [ int_exp 1; Var "x" ], "Combine(1, x)\n ;\n ")
        ; test_parse_err "whitespace err" "Combine\n(1,x) "
        ; test_parse_err "semicolon err" "Combine(1,x);\n;"
        ; test_parse_err "Combine()" "Combine()"
        ] )
    ; ( "parens"
      , [ test_parse "(1)" (int_exp 1, "(1)")
        ; test_parse "(x)" (Var "x", "(x)")
        ; test_parse "(Combine((1),((x))))" (Combine [ int_exp 1; Var "x" ], "(Combine((1),((x))))")
        ; test_parse "whitespace 1" (int_exp 1, " ( 1 ) ")
        ; test_parse "whitespace 2" (int_exp 1, "\n(\n1\n)\n")
        ; test_parse "semicolon 1" (int_exp 1, "(1);")
        ; test_parse "semicolon 2" (int_exp 1, "(1) ; ")
        ; test_parse "semicolon 3" (int_exp 1, "(1)\n ;\n ")
        ] )
      (* indexing *)
      (* neg *)
      (* assign *)
      (* seq *)
    ]
