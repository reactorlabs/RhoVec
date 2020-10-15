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

let ws = skip_while is_ws
let digits = take_while1 is_digit
let with_ws p = ws *> p <* ws

let combine = string "Combine"
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
let atom = lit <|> var

(* Combine ( EXPR , ... , EXPR ) *)
let combine expr = combine *> parens (sep_by1 comma expr) >>| fun es -> Combine es

(* - EXPR *)
let negate expr = char '-' *> expr >>| fun e -> Negate e

(* EXPR ([] | [EXPR]) *)
let subset1 expr =
  let subset1 e1 e2 = Subset1 (e1, e2) in
  let empty = string "[]" *> return None in
  let index = bracks1 expr >>| Option.some in
  lift2 subset1 expr (empty <|> index)

(* EXPR [[ EXPR ]] *)
let subset2 expr =
  let subset2 e1 e2 = Subset2 (e1, e2) in
  lift2 subset2 expr (bracks2 expr)

(* EXPR ; ... ; EXPR *)
let sequence expr = sep_by1 semicolon expr >>| fun es -> Seq es

(* IDENT <- EXPR *)
let assign expr =
  let assign x _ e = Assign (x, e) in
  lift3 assign variable leftarrow expr

(* IDENT ([] | [EXPR]) <- EXPR *)
let subset1_assign expr =
  let subset1 x1 e2 _ e3 = Subset1_Assign (x1, e2, e3) in
  let empty = string "[]" *> return None in
  let index = bracks1 expr >>| Option.some in
  lift4 subset1 variable (empty <|> index) leftarrow expr

(* IDENT [[ EXPR ]] <- EXPR *)
let subset2_assign expr =
  let subset2 x1 e2 _ e3 = Subset2_Assign (x1, e2, e3) in
  lift4 subset2 variable (bracks2 expr) leftarrow expr

let expr = atom <?> "expr"

(* let expr = *)
(*   fix (fun expr -> *)
(*       subset2 <|> assign2 <|> assign <|> atom <|> combine <|> negate) *)
(*   <?> "expr" *)

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
