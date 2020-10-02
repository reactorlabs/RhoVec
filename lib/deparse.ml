open Expr

let lit_to_r = function
  | Int i -> Int.to_string i ^ "L"
  | Bool b -> if b then "TRUE" else "FALSE"
  | NA_bool -> "NA"
  | NA_int -> "NA_integer_"

let rec to_r = function
  | Lit l -> lit_to_r l
  | Var x -> x
  | Combine es ->
      let inner = es |> List.map to_r |> String.concat ", " in
      Printf.sprintf "c(%s)" inner
  | Negate e -> Printf.sprintf "-%s" (to_r e)
  | Subset1 (e1, None) -> Printf.sprintf "%s[]" (to_r e1)
  | Subset1 (e1, Some e2) -> Printf.sprintf "%s[%s]" (to_r e1) (to_r e2)
  | Subset2 (e1, e2) -> Printf.sprintf "%s[[%s]]" (to_r e1) (to_r e2)
  | Seq es ->
      let inner = es |> List.map to_r |> String.concat "\n  " in
      Printf.sprintf "{\n  %s\n}" inner
  | Assign (x1, e2) -> Printf.sprintf "%s <- %s" x1 (to_r e2)
  | Subset1_Assign (x1, None, e3) -> Printf.sprintf "%s[] <- %s" x1 (to_r e3)
  | Subset1_Assign (x1, Some e2, e3) -> Printf.sprintf "%s[%s] <- %s" x1 (to_r e2) (to_r e3)
  | Subset2_Assign (x1, e2, e3) -> Printf.sprintf "%s[[%s]] <- %s" x1 (to_r e2) (to_r e3)

let val_to_r = function
  | Vector (a, t) ->
      (* Need a workaround because RhoVec doesn't have the NULL vector, only typed empty vectors. *)
      if Array.length a = 0 then
        match t with
        | T_Bool -> Printf.sprintf "as.logical(NULL)"
        | T_Int -> Printf.sprintf "as.integer(NULL)"
      else
        let inner = a |> Array.map lit_to_r |> Array.to_list |> String.concat ", " in
        Printf.sprintf "c(%s)" inner
