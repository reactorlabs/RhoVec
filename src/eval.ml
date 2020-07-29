open Expr

exception Subscript_out_of_bounds
exception Selecting_lt_one_element
exception Selecting_gt_one_element

let rec eval e =
  let get_array e =
    match (eval e) with
    | Vector (a, _) -> a in

  match e with
  | Lit i -> vec_of_int i
  | Combine es ->
      let vec = Array.concat (List.map get_array es) in
      Vector (vec, Int)
  | Subset1 (e1, e2) ->
      (* let v1 = get_array e1 in *)
      let v2 = get_array e2 in
      (* A lot of TODOs here *)
      if Array.length v2 < 1 then assert false;
      if Array.length v2 > 1 then assert false;
      let n = v2.(0) in
      if n <> 0 then assert false;
      empty_intvec
  | Subset1_Nothing e1 -> eval e1
  | Subset2 (e1, e2) ->
      let v1 = get_array e1 in
      let v2 = get_array e2 in
      if Array.length v2 < 1 then raise Selecting_lt_one_element;
      if Array.length v2 > 1 then raise Selecting_gt_one_element;
      let n = v2.(0) in
      if n = 0 then raise Selecting_lt_one_element;
      if n < 0 then raise Selecting_gt_one_element;
      if n > Array.length v1 then raise Subscript_out_of_bounds;
      vec_of_int v1.(n - 1)
