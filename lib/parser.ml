open Opal
open Expr

let reserved = [ "NA_b"; "F"; "T"; "NA_i"; "Combine" ]

let boolean =
  token "NA_b" >> return NA_bool
  <|> (token "F" >> return (Bool false))
  <|> (token "T" >> return (Bool true))

let integer =
  token "NA_i" >> return NA_int
  <|> (spaces >> many1 digit => implode % Stdlib.int_of_string => fun i -> Int i)

let literal = boolean <|> integer

let ident_chars = alpha_num <|> exactly '_' <|> exactly '.'
let identifier =
  spaces >> letter <~> many ident_chars => implode >>= function
  | s when List.mem s reserved -> mzero
  | s -> return (Var s)

let test_parse p s = parse p (LazyStream.of_string s)
