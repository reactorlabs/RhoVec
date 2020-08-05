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

exception Todo

let extract_array = function
  | Vector (a, _) -> a

let extract_type = function
  | Vector (_, t) -> t

let extract_bool (l : literal) =
  match l with
  | Bool b -> Some b
  | NA_bool -> None
  | Int _ | NA_int -> raise (Type_error { expected = Bool; received = get_tag l })

let extract_int (l : literal) =
  match l with
  | Int i -> Some i
  | NA_int -> None
  | Bool _ | NA_bool -> raise (Type_error { expected = Int; received = get_tag l })

let check_all_types_same (ts : type_tag list) =
  if List.length ts = 0 then raise Expected_nonempty_vector;
  let hd = List.hd ts in
  let tl = List.tl ts in
  let res = List.find_opt (fun x -> x <> hd) tl in
  match res with
  | Some t -> raise (Type_error { expected = hd; received = t })
  | None -> hd


(* TODO:
   A function like this can be written elegantly using lists and recursion. But
   the implementation mostly uses arrays, so we have to convert back and forth.
   Revisit this decision later.
 *)
let rec get_at_true (l1 : literal list) (l2 : bool option list) ty =
  match (l1, l2) with
  | ([], []) -> []
  | (hd1 :: tl1, hd2 :: tl2) ->
      let rest = get_at_true tl1 tl2 ty in
      begin match hd2 with
      | Some true -> hd1 :: rest
      | Some false -> rest
      | None -> (na_lit ty) :: rest
      end
  | ([], _) | (_, []) -> assert false

let rec get_at_pos (a1 : literal array) (l2 : int option list) ty =
  match l2 with
  | [] -> []
  | hd2 :: tl2 ->
      let rest = get_at_pos a1 tl2 ty in
      begin match hd2 with
      | Some i when 1 <= i && i <= Array.length a1 -> a1.(i - 1) :: rest
      | Some _ | None -> (na_lit ty) :: rest
      end

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
      (* TODO: currently omit checks on a1 *)
      begin match t2 with
      | Bool ->
          (* TODO: for now, both vectors must have the same length *)
          if Array.length a1 <> Array.length a2 then raise Todo;
          let l1 = Array.to_list a1 in
          let l2 = Array.to_list (Array.map extract_bool a2) in
          let res = Array.of_list (get_at_true l1 l2 t1) in
          Vector (res, t1)
      | Int when Array.length a2 = 1 && (extract_int a2.(0)) = Some 0 ->
          empty_vec t1
      | Int ->
          let l2 = Array.to_list (Array.map extract_int a2) in
          let res = Array.of_list (get_at_pos a1 l2 t1) in
          Vector (res, t1)
      end
  | Subset1_Nothing e1 -> eval e1
  | Subset2 (e1, e2) ->
      let Vector (a1, t1) = eval e1 in
      let Vector (a2, t2) = eval e2 in
      if Array.length a2 < 1 then raise Selecting_lt_one_element;
      if Array.length a2 > 1 then raise Selecting_gt_one_element;
      if t2 <> Int then raise (Type_error { expected = Int; received = t2 });
      begin match extract_int a2.(0) with
      | None -> raise Subscript_out_of_bounds
      | Some n when n = 0 -> raise Selecting_lt_one_element
      | Some n when n < 0 -> raise Selecting_gt_one_element
      | Some n when n > Array.length a1 -> raise Subscript_out_of_bounds
      | Some n -> vec_of_lit a1.(n - 1)
      end
