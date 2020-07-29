type expression =
  | Lit of int
  | Combine of expression list
  | Subset1 of expression * expression
  | Subset1_Nothing of expression
  | Subset2 of expression * expression
and type_tag =
  | Int
and value =
  | Vector of int array * type_tag

(* Helpers for constructing ASTs *)
let vec_of_intlist xs = Vector ((Array.of_list xs), Int)
let vec_of_int x = Vector ([| x |], Int)
let empty_intvec = Vector ([| |], Int)
