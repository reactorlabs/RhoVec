open Expr

type type_error = {
  expected : type_tag;
  received : type_tag;
}

exception Type_error of type_error

exception Subscript_out_of_bounds
exception Selecting_lt_one_element
exception Selecting_gt_one_element

exception Expected_nonempty_vector

let extract_array = function
  | Vector (a, _) -> a

let extract_type = function
  | Vector (_, t) -> t

let extract_bool (l : literal) =
  match l with
  | Bool b -> b
  | Int _ -> raise (Type_error { expected = Bool; received = get_tag l })

let extract_int (l : literal) =
  match l with
  | Int i -> i
  | Bool _ -> raise (Type_error { expected = Int; received = get_tag l })

let check_all_types_same ts =
  if List.length ts = 0 then raise Expected_nonempty_vector;
  let hd = List.hd ts in
  let tl = List.tl ts in
  let res = List.find_opt (fun x -> x <> hd) tl in
  match res with
  | Some t -> raise (Type_error { expected = hd; received = t })
  | None -> hd

let rec eval e =
  match e with
  | Lit l -> vec_of_lit l
  | Combine es ->
      let vecs = List.map eval es in
      let ts = List.map extract_type vecs in
      let ty = check_all_types_same ts in
      let arrs = List.map extract_array vecs in
      let vec = Array.concat arrs in
      Vector (vec, ty)
  | Subset1 (e1, e2) ->
      let Vector (a1, t1) = eval e1 in
      let Vector (a2, t2) = eval e2 in
      (* TODO: for now we only support Subset1_Zero *)
      (* TODO: currently omit checks on a1 *)
      if t2 <> Int then assert false;
      if Array.length a2 < 1 then assert false;
      if Array.length a2 > 1 then assert false;
      let n = extract_int a2.(0) in
      if n <> 0 then assert false;
      empty_vec t1
  | Subset1_Nothing e1 -> eval e1
  | Subset2 (e1, e2) ->
      let Vector (a1, t1) = eval e1 in
      let Vector (a2, t2) = eval e2 in
      if Array.length a2 < 1 then raise Selecting_lt_one_element;
      if Array.length a2 > 1 then raise Selecting_gt_one_element;
      if t2 <> Int then raise (Type_error { expected = Int; received = t2 });
      let n = extract_int a2.(0) in
      if n = 0 then raise Selecting_lt_one_element;
      if n < 0 then raise Selecting_gt_one_element;
      if n > Array.length a1 then raise Subscript_out_of_bounds;
      vec_of_lit a1.(n - 1)

