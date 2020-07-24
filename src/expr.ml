type expression =
  | Lit of int
  | Combine of expression list
and type_tag =
  | Int
and value =
  | Vector of int array * type_tag
