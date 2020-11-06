open Angstrom
open Expr

exception Parse_error of string

(* TODO: rename this file to parse *)
(* TODO: refactor? cleanup and comment *)

let reserved = [ "NA_b"; "F"; "T"; "NA_i"; "Combine" ]

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false
let is_eol = function
  | '\n' | '\r' -> true
  | _ -> false
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
let is_blank x = is_space x || is_eol x
let is_first_ident x = '.' == x || is_letter x
let is_ident x = '_' == x || is_first_ident x || is_digit x

let ws = take_while is_space
let ws1 = take_while1 is_space
let eol = take_while is_eol
let eol1 = take_while1 is_eol
let blank = take_while is_blank
let blank1 = take_while1 is_blank
let digits = take_while1 is_digit
let with_ws p = ws *> p <* ws
let with_ws1 p = ws1 *> p <* ws1
let with_eol p = eol *> p <* eol
let with_eol1 p = eol1 *> p <* eol1
let with_blank p = blank *> p <* blank
let with_blank_ws p = blank *> p <* ws

let leftarrow = string "<-"
let comma = char ','
let seq_sep = with_blank (char ';') <|> eol1 *> return '\n'
let trailing = with_blank (char ';') <|> blank *> return '\n'
let parens p = char '(' *> with_blank p <* char ')'
let bracks1 p = char '[' *> with_blank p <* char ']'
let bracks2 p = string "[[" *> with_blank p <* string "]]"
let braces p = char '{' *> p <* char '}'

(* NA_b | F | T *)
let boolean =
  string "NA_b" *> return NA_bool
  <|> string "F" *> return (Bool false)
  <|> string "T" *> return (Bool true)

(* NA_i | /[0-9]+/ *)
let integer = string "NA_i" *> return NA_int <|> (digits >>| fun i -> Int (int_of_string i))

(* /[a-zA-Z.][a-zA-Z0-9._]+/ *)
let ident =
  peek_char_fail >>= fun c -> if is_first_ident c then take_while is_ident else fail "ident"

(* An identifier that is not reserved. *)
let variable = ident >>= fun s -> if List.mem s reserved then fail "keyword" else return s

let lit = boolean <|> integer >>| fun l -> Lit l
let var = variable >>| fun s -> Var s

let combine expr =
  string "Combine" *> ws *> parens (sep_by1 comma (with_blank expr)) >>| fun es -> Combine es

let base expr = var <|> lit <|> combine expr <|> parens expr

let rec subset expr e =
  let brackets be =
    char '[' *> blank *> char ']' *> return (Subset1 (be, None))
    <|> (bracks1 expr >>| fun e -> Subset1 (be, Some e))
    <|> (bracks2 expr >>| fun e -> Subset2 (be, e)) in
  peek_char >>= function
  | Some '[' -> brackets e <* ws >>= subset expr
  | _ -> return e

let rvalue expr = with_blank_ws (base expr) >>= subset expr
let lvalue expr = with_blank_ws var >>= subset expr

(* TODO: is both lvalue and rvalue redundant? *)
let neg expr =
  fix (fun neg ->
      rvalue expr <|> lvalue expr <|> (with_blank_ws (char '-') *> neg >>| fun e -> Negate e))

let assign expr =
  let[@warning "-4-8"] assign' lhs _ rhs =
    match lhs with
    | Var x -> Assign (x, rhs)
    | Subset1 (Var x, e2) -> Subset1_Assign (x, e2, rhs)
    | Subset2 (Var x, e2) -> Subset2_Assign (x, e2, rhs)
    | _ -> raise (Parse_error "nested assignment") in
  lift3 assign' (lvalue expr) leftarrow expr

(* only allow eol for sequences; otherwise could be confusing, but it's also
   context-sensitive since open paren/bracket allows eols to separate tokens
   without ending the expression *)

let seq expr =
  sep_by1 seq_sep expr <* trailing >>| function
  | [ e ] -> e
  | es -> Seq es

let expr' = fix (fun expr -> assign expr <|> neg expr <|> braces (seq expr))

let expr = seq expr'

let tryparse p (str : string) =
  match parse_string ~consume:All p str with
  | Ok v -> v
  | Error msg -> raise (Parse_error msg)

let parse (str : string) =
  match parse_string ~consume:All expr str with
  | Ok v -> v
  | Error msg -> raise (Parse_error msg)

let run (str : string) =
  let v = Eval.run @@ parse str in
  Expr.show_val v
