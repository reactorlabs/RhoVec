open Angstrom
open Expr

let reserved = [ "NA_b"; "F"; "T"; "NA_i"; "Combine" ]

(* let combine = token "Combine" *)
(* let leftarrow = token "<-" *)
(* let parens = between (token "(") (token ")") *)
(* let bracks1 = between (token "[") (token "]") *)
(* let bracks2 = between (token "[[") (token "]]") *)

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
let is_first_ident x = '_' == x || '.' == x || is_letter x
let is_ident x = is_first_ident x || is_digit x

let ws = skip_while is_ws
let digits = take_while1 is_digit
let with_ws p = ws *> p <* ws

let boolean =
  string "NA_b" *> return NA_bool
  <|> string "F" *> return (Bool false)
  <|> string "T" *> return (Bool true)

let integer = string "NA_i" *> return NA_int <|> (digits >>| fun i -> Int (int_of_string i))

let literal = boolean <|> integer >>| fun l -> Lit l

let variable =
  let* fst = satisfy is_first_ident >>| Char.escaped in
  let* rst = take_while is_ident in
  let str = fst ^ rst in
  if List.mem str reserved then fail "keyword" else return (Var str)

let atom = with_ws (literal <|> variable)

(*
let combine_op input = (return (fun es -> Combine es)) input
let neg_op input = (token "-" >> return (fun e -> Negate e)) input
let subset1_op input = (return (fun e1 e2 -> Subset1 (e1, e2))) input
let subset2_op input = (return (fun e1 e2 -> Subset2 (e1, e2))) input
let seq_op input = (return (fun es -> Seq es)) input
let assign_op input = (return (fun x1 e2 -> Assign (x1, e2))) input
let subset1_assign_op input = (return (fun x1 e2 e3 -> Subset1_Assign (x1, e2, e3))) input
let subset2_assign_op input = (return (fun x1 e2 e3 -> Subset2_Assign (x1, e2, e3))) input

let rec expr input = (atom <|> negate_exp) input
and negate_exp input =
  (let* _ = exactly '-' in
  let* e = expr in
  return (Negate e)
  ) input

let test_parse p s = parse p (LazyStream.of_string s)
let tryparse s = parse expr (LazyStream.of_string s)
*)

let tryparse p (str : string) =
  match parse_string ~consume:All p str with
  | Ok v -> v
  | Error msg -> failwith msg
