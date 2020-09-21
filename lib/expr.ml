type literal =
  | NA_bool
  | Bool    of bool
  | NA_int
  | Int     of int
[@@deriving eq]

let show_lit = function
  | Bool b -> if b then "T" else "F"
  | Int i -> Int.to_string i
  | NA_bool | NA_int -> "NA"

module Identifier = struct
  type t = string
  let compare = String.compare
  let equal = String.equal
  let pp = Format.pp_print_text
end

type identifier = Identifier.t [@@deriving eq, show]

type expression =
  | Lit                    of literal [@printer fun fmt l -> fprintf fmt "%s" (show_lit l)]
  | Var                    of identifier [@printer fun fmt -> fprintf fmt "%s"]
  | Combine                of expression list
  | Negate                 of expression
  | Subset1_Nothing        of expression
  | Subset1                of expression * expression
  | Subset2                of expression * expression
  | Seq                    of expression list
  | Assign                 of identifier * expression
  | Subset1_Nothing_Assign of identifier * expression
  | Subset1_Assign         of identifier * expression * expression
  | Subset2_Assign         of identifier * expression * expression
[@@deriving eq, show { with_path = false }]

type type_tag =
  | T_Bool
  | T_Int
[@@deriving eq]

let show_type = function
  | T_Bool -> "Bool"
  | T_Int -> "Int"

type value = Vector of literal array * type_tag [@@deriving eq]

let show_val = function
  | Vector (a, t) ->
      let inner = a |> Array.map show_lit |> Array.to_list |> String.concat " " in
      "[" ^ inner ^ "]," ^ show_type t

module Env = Map.Make (Identifier)

type environment = value Env.t

let get_tag = function
  | Bool _ | NA_bool -> T_Bool
  | Int _ | NA_int -> T_Int

let is_na = function
  | Bool _ | Int _ -> false
  | NA_bool | NA_int -> true

(* Helpers for constructing ASTs *)
let true_lit = Bool true
let false_lit = Bool false
let bool_lit b = Bool b

let int_lit i = Int i

let na_lit = function
  | T_Bool -> NA_bool
  | T_Int -> NA_int
let opt_int_lit = function
  | Some i -> Int i
  | None -> NA_int
let opt_bool_lit = function
  | Some b -> Bool b
  | None -> NA_bool

let true_exp = Lit (Bool true)
let false_exp = Lit (Bool false)
let bool_exp b = Lit (Bool b)

let int_exp i = Lit (Int i)

let na_exp ty = Lit (na_lit ty)

let empty_vec t = Vector ([||], t)
let vec_of_int x = Vector ([| Int x |], T_Int)
let vec_of_intlist xs = Vector (Array.of_list (List.map int_lit xs), T_Int)
let vec_of_intoptlist xs = Vector (Array.of_list (List.map opt_int_lit xs), T_Int)
let vec_of_bool x = Vector ([| bool_lit x |], T_Bool)
let vec_of_boollist xs = Vector (Array.of_list (List.map bool_lit xs), T_Bool)
let vec_of_booloptlist xs = Vector (Array.of_list (List.map opt_bool_lit xs), T_Bool)

let vector v t = Vector (v, t)

let vec_of_lit l =
  match l with
  | Bool b -> vec_of_bool b
  | Int i -> vec_of_int i
  | NA_bool -> Vector ([| l |], T_Bool)
  | NA_int -> Vector ([| l |], T_Int)
let vec_of_na t = vec_of_lit (na_lit t)
