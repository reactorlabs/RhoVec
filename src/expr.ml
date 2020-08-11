type expression =
  | Lit of literal
  | Combine of expression list
  | Subset1_Nothing of expression
  | Subset1_Nothing_Assign of expression * expression
  | Subset1 of expression * expression
  | Subset1_Neg of expression * expression (* TODO: use a Negate expr instead *)
  | Subset2 of expression * expression
and literal =
  | Bool of bool
  | Int of int
  | NA_bool
  | NA_int
and value =
  | Vector of literal array * type_tag
and type_tag =
  | Bool
  | Int

let get_tag : literal -> type_tag = function
  | Bool _ | NA_bool -> Bool
  | Int _ | NA_int -> Int

(* Helpers for constructing ASTs *)
let true_lit = Bool true
let false_lit = Bool false
let bool_lit b = Bool b
let int_lit i = Int i
let na_lit (ty : type_tag) =
  match ty with
  | Bool -> NA_bool
  | Int -> NA_int

let true_exp = Lit (Bool true)
let false_exp = Lit (Bool false)
let bool_exp b = Lit (Bool b)
let int_exp i = Lit (Int i)
let na_exp ty = Lit (na_lit ty)

let empty_vec t = Vector ([| |], t)

let vec_of_int x = Vector ([| Int x |], Int)
let vec_of_intlist xs = Vector (Array.of_list (List.map int_lit xs), Int)

let vec_of_bool x = Vector ([| bool_lit x |], Bool)
let vec_of_boollist xs = Vector (Array.of_list (List.map bool_lit xs), Bool)

let vec_of_lit l =
  match l with
  | Bool b -> vec_of_bool b
  | Int i -> vec_of_int i
  | NA_bool | NA_int -> Vector ([| l |], get_tag l)
