open Angstrom
open Expr

exception Parse_error of string

(* Reserved keywords/literals *)
let reserved = [ "NA_b"; "F"; "T"; "NA_i"; "Combine" ]

(* Common helpers for parsers *)
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
let blank = take_while is_blank
let with_blank p = blank *> p <* blank
let with_blank_ws p = blank *> p <* ws

let parens p = char '(' *> with_blank p <* char ')'
let bracks1 p = char '[' *> with_blank p <* char ']'
let bracks2 p = string "[[" *> with_blank p <* string "]]"
let braces p = blank *> char '{' *> p <* char '}'

(* The actual expression parser. Note that the "grammar" in expr.ml needs to be refactored to avoid
   left recursion. *)
let expr =
  (* literal ::= boolean | integer
     boolean ::= NA_b | F | T
     integer ::= NA_i | [0-9]+ *)
  let literal =
    let boolean =
      string "NA_b" *> return NA_bool
      <|> string "F" *> return (Bool false)
      <|> string "T" *> return (Bool true) in
    let integer =
      string "NA_i" *> return NA_int <|> (take_while1 is_digit >>| fun i -> Int (int_of_string i))
    in
    boolean <|> integer >>| fun l -> Lit l in

  (* An identifier
      - starts with a letter or a .
      - uses alphanumeric characters or . or _
      - is not a reserved word *)
  let variable =
    let identifier =
      peek_char_fail >>= fun c -> if is_first_ident c then take_while is_ident else fail "ident"
    in
    identifier >>= fun s ->
    if List.mem s reserved then fail "keyword" else return s >>| fun s -> Var s in

  (* sequence ::= expr
                | expr SEP ... SEP expr
     SEP ::= ; | [\n\r]+

     The sequence parser parses a top-level sequence of expressions, separated by semicolons or
     newlines. The sequence does not need to be wrapped by curly braces, and may be followed by a
     trailing semicolon or trailing newlines.

     If the sequence contains only one expression, then the outer Seq node is removed.
  *)
  let sequence expr =
    let trailing = with_blank (char ';') <|> blank *> return '\n' in
    let seq_sep = with_blank (char ';') <|> take_while1 is_eol *> return '\n' in
    sep_by1 seq_sep expr <* trailing >>| function
    | [ e ] -> e
    | es -> Seq es in

  (* The subexpr parser parses expressions that can appear as subexpressions. It does not parse
     top-level expressions (but a subexpression can appear as a top-level expression). The key
     point is that a sequence is a subexpression only if it is wrapped by curly braces. *)
  let subexpr =
    fix (fun subexpr ->
        (* indexable ::= variable | literal | Combine(subexpr, ..., subexpr) | (subexpr)

           indexable is an expression that can be indexed:
             - a variable
             - a literal
             - a Combine expression
             - any subexpression that is parenthesized

           NOTE: Order is significant! The variable parser must run before the literal parser.
           This allows "Tt" to be accepted by the variable parser, but "T" is rejected (because it
           is reserved) and then parsed by the literal parser. Otherwise, "Tt" is parsed as a
           literal but with only "T" consumed and "t" remaining in the input. *)
        let indexable =
          (* The arguments to Combine are comma-separated, and there must be at least one argument.
             Any kind of whitespace is allowed within the parentheses of Combine. *)
          let combine =
            string "Combine" *> ws *> parens (sep_by1 (char ',') (with_blank subexpr)) >>| fun es ->
            Combine es in
          variable <|> literal <|> combine <|> parens subexpr in

        (* subset ::= [] subset | [subexpr] subset | [[subexpr]] subset | <empty>

           This parser handles the square brackets used for subsetting. It is written this way to
           avoid left recursion; a sequence of 0 or more subsets is valid.
        *)
        let rec subset e =
          let brackets be =
            char '[' *> blank *> char ']' *> return (Subset1 (be, None))
            <|> (bracks1 subexpr >>| fun e -> Subset1 (be, Some e))
            <|> (bracks2 subexpr >>| fun e -> Subset2 (be, e)) in
          peek_char >>= function
          | Some '[' -> brackets e <* ws >>= subset
          | _ -> return e in

        (* assign ::= assignable <- subexpr
           assignable ::= variable assignable'
           assignable' ::= subset assignable' | <empty>

           Effectively, this is:
             assign ::= variable subset* <- subexpr

           However, for now, assignment with multiple subsets is not allowed.
        *)
        let assign =
          let assignable = with_blank_ws variable >>= subset in
          let[@warning "-4-8"] assign' lhs _ rhs =
            match lhs with
            | Var x -> Assign (x, rhs)
            | Subset1 (Var x, e2) -> Subset1_Assign (x, e2, rhs)
            | Subset2 (Var x, e2) -> Subset2_Assign (x, e2, rhs)
            | _ -> raise (Parse_error "nested assignment") in
          lift3 assign' assignable (string "<-") subexpr in

        (* nonassign ::= nonneg | neg

           neg ::= - nonassign
           nonneg ::= indexable subset* | { sequence }
        *)
        let nonassign =
          fix (fun nonassign ->
              let neg = with_blank_ws (char '-') *> nonassign >>| fun e -> Negate e in
              let nonneg = with_blank_ws indexable >>= subset <|> braces (sequence subexpr) in
              neg <|> nonneg) in

        (* Putting everything together, a subexpression is an assignment expression or a
           non-assignment expression.

           NOTE: Order is significant! The assign parser must run before the nonassign parser.
           Otherwise, the left-hand side of an assignment expression is parsed as a complete
           nonassign expression. *)
        assign <|> nonassign) in

  (* The expression parser parses a sequence of subexpressions, which do not need to be wrapped by
     curly braces. *)
  sequence subexpr

let parse (str : string) =
  match parse_string ~consume:All expr str with
  | Ok v -> v
  | Error msg -> raise (Parse_error msg)
