open Angstrom
open Expr

let ( let* ) = ( >>= )

let reserved = [ "NA_b"; "F"; "T"; "NA_i"; "Combine" ]

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false
let is_eol = function
  | '\n' | '\r' -> true
  | _ -> false
let is_ws x = is_space x || is_eol x
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
let is_first_ident x = '.' == x || is_letter x
let is_ident x = '_' == x || is_first_ident x || is_digit x

let ws = skip_while is_space
let wsnl = skip_while is_ws
let digits = take_while1 is_digit
let with_ws p = ws *> p <* ws

let leftarrow = string "<-"
let comma = char ','
let semicolon = char ';'
let parens p = char '(' *> p <* char ')'
let bracks1 p = char '[' *> p <* char ']'
let bracks2 p = string "[[" *> p <* string "]]"

(* NA_b | F | T *)
let boolean =
  string "NA_b" *> return NA_bool
  <|> string "F" *> return (Bool false)
  <|> string "T" *> return (Bool true)

(* NA_i | /[0-9]+/ *)
let integer = string "NA_i" *> return NA_int <|> (digits >>| fun i -> Int (int_of_string i))

(* /[a-zA-Z.][a-zA-Z0-9._]+/ *)
let ident =
  let* c = peek_char_fail in
  if is_first_ident c then take_while is_ident else fail "ident"

(* An identifier that is not reserved. *)
let variable =
  let* s = ident in
  if List.mem s reserved then fail "keyword" else return s

let lit = boolean <|> integer >>| fun l -> Lit l
let var = variable >>| fun s -> Var s

let combine expr = string "Combine" *> parens (sep_by1 comma expr) >>| fun es -> Combine es

let base expr = lit <|> combine expr <|> parens expr

let index expr be =
  string "[]" *> return (Subset1 (be, None))
  <|> (bracks1 expr >>| fun e -> Subset1 (be, Some e))
  <|> (bracks2 expr >>| fun e -> Subset2 (be, e))

let rvalue expr =
  let rec rvalue' expr be =
    peek_char >>= function
    | Some '[' -> index expr be >>= rvalue' expr
    | _ -> return be in
  base expr >>= rvalue' expr

let lvalue expr =
  let rec lvalue' expr v =
    peek_char >>= function
    | Some '[' -> index expr v >>= lvalue' expr
    | _ -> return v in
  var >>= lvalue' expr

let neg expr =
  fix (fun neg -> rvalue expr <|> lvalue expr <|> (char '-' *> neg >>| fun e -> Negate e))

let assign expr =
  let[@warning "-4-8"] assign' lhs _ rhs =
    match lhs with
    | Var x -> Assign (x, rhs)
    | Subset1 (Var x, e2) -> Subset1_Assign (x, e2, rhs)
    | Subset2 (Var x, e2) -> Subset2_Assign (x, e2, rhs) in
  lift3 assign' (lvalue expr) leftarrow expr

let expr = fix (fun expr -> assign expr <|> neg expr) <?> "expr"

(*
(* EXPR ; ... ; EXPR *)
let sequence expr = sep_by1 semicolon expr >>| fun es -> Seq es
*)

let tryparse p (str : string) =
  match parse_string ~consume:All p str with
  | Ok v -> v
  | Error msg -> failwith msg

let parse (str : string) =
  match parse_string ~consume:All expr str with
  | Ok v -> v
  | Error msg -> failwith msg

let run (str : string) =
  let v = Eval.run @@ parse str in
  Expr.show_val v
