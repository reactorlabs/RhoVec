open Expr

type type_error = {
  expected : type_tag;
  received : type_tag;
}
exception Type_error of type_error
let type_error expected received = Type_error { expected; received }

exception Subscript_out_of_bounds
exception Mixing_with_neg_subscripts
exception Selecting_lt_one_element
exception Selecting_gt_one_element

exception Expected_nonempty_vector

exception NAs_not_allowed_in_subscripted_assignment
exception Replacement_length_not_multiple
exception Too_many_elements_supplied

exception Todo

let extract_array = function
  | Vector (a, _) -> a

let extract_type = function
  | Vector (_, t) -> t

let extract_bool (l : literal) =
  match l with
  | Bool b -> Some b
  | NA_bool -> None
  | Int _ | NA_int -> raise (type_error Bool (get_tag l))

let extract_int (l : literal) =
  match l with
  | Int i -> Some i
  | NA_int -> None
  | Bool _ | NA_bool -> raise (type_error Int (get_tag l))

let check_all_types_same (ts : type_tag list) =
  if List.length ts = 0 then raise Expected_nonempty_vector;
  let hd = List.hd ts in
  let tl = List.tl ts in
  let res = List.find_opt (fun x -> x <> hd) tl in
  match res with
  | Some t -> raise (type_error hd t)
  | None -> hd

(* TODO: better way of handling arrays vs lists, literals vs Options, etc *)

(* TODO:
   A function like this can be written elegantly using lists and recursion. But
   the implementation mostly uses arrays, so we have to convert back and forth.
   Revisit this decision later.
 *)
let rec get_at_pos (a1 : literal array) (l2 : int option list) ty =
  match l2 with
  | [] -> []
  | hd2 :: tl2 ->
      let rest = get_at_pos a1 tl2 ty in
      begin match hd2 with
      | Some i when 1 <= i && i <= Array.length a1 -> a1.(i - 1) :: rest
      | Some 0 -> rest
      | Some i when i > 0 -> (na_lit ty) :: rest
      | None -> (na_lit ty) :: rest
      | Some _ -> raise Mixing_with_neg_subscripts
      end

let rec bool_vec_to_pos (l : bool option list) n =
  match l with
  | [] -> []
  | hd :: tl ->
      let rest = bool_vec_to_pos tl (n + 1) in
      begin match hd with
      | Some true -> Some n :: rest
      | Some false -> rest
      | None -> None :: rest
      end

let neg_vec_to_bool (l : int option list) n =
  let a = Array.make n (Some true) in
  List.iter (fun x ->
    match x with
    | Some i when 1 <= i && i <= n -> Array.set a (i - 1) (Some false)
    | Some i when i >= 0 -> ()
    | Some _ | None -> raise Mixing_with_neg_subscripts) l;
  a

let update_at_pos (a1 : literal array)
                  (l2 : int option list)
                  (l3 : literal list) =
  let len = Array.length a1 in
  let a1' = Array.copy a1 in
  List.iter2 (fun n x ->
    match n with
    | Some i when 1 <= i && i <= len -> Array.set a1' (i - 1) x
    | Some i when i >= 0 -> ()
    | Some _ -> raise Mixing_with_neg_subscripts
    | None -> ()
  ) l2 l3;
  a1'

let extend (a : literal array) n ty =
  if Array.length a < n then
    (* Prefill with NAs (some might not be overwritten) *)
    let res = Array.make n (na_lit ty) in
    Array.iteri (fun i x -> Array.set res i x) a;
    res
  else
    a

let recycle (a : literal array) n =
  let m = Array.length a in
  if m < n then
    (* Use NA_bool as a placeholder that will be overwritten *)
    let res = Array.make n NA_bool in
    for i = 0 to (n - 1) do
      Array.set res i a.(i mod m)
    done;
    res
  else
    a

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
  | Subset1_Nothing e1 -> eval e1
  | Subset1_Nothing_Assign (e1, e2) ->
      let Vector (a1, t1) = eval e1 in
      let Vector (a2, t2) = eval e2 in
      let n, m = (Array.length a1, Array.length a2) in
      if n mod m <> 0 then raise Replacement_length_not_multiple;
      if t1 <> t2 then raise (type_error t1 t2);
      let res = recycle a2 n in
      Vector (res, t1)
  | Subset1 (e1, e2) ->
      let Vector (a1, t1) = eval e1 in
      let Vector (a2, t2) = eval e2 in
      begin match t2 with
      | Bool ->
          let len = Stdlib.max (Array.length a1) (Array.length a2) in
          let a1' = extend a1 len t1 in
          let a2' = recycle a2 len in
          let l2 = Array.to_list (Array.map extract_bool a2') in
          let l2' = bool_vec_to_pos l2 1 in
          let res = Array.of_list (get_at_pos a1' l2' t1) in
          Vector (res, t1)
      | Int ->
          let l2 = Array.to_list (Array.map extract_int a2) in
          let res = Array.of_list (get_at_pos a1 l2 t1) in
          Vector (res, t1)
      end
  | Subset1_Assign (e1, e2, e3) ->
      let Vector (a1, t1) = eval e1 in
      let Vector (a2, t2) = eval e2 in
      let Vector (a3, t3) = eval e3 in
      if Array.exists is_na a2 && Array.length a3 > 1 then
        raise NAs_not_allowed_in_subscripted_assignment;
      begin match t2 with
      | Bool ->
          let len = Stdlib.max (Array.length a1) (Array.length a2) in
          let a1' = extend a1 len t1 in
          let a2' = recycle a2 len in
          let l2 = Array.to_list (Array.map extract_bool a2') in
          let l2' = bool_vec_to_pos l2 1 in
          let (n, m) = (List.length l2', Array.length a3) in
          if n mod m <> 0 then raise Replacement_length_not_multiple;
          if t1 <> t3 then raise (type_error t1 t3);
          let a3' = recycle a3 n in
          let l3 = Array.to_list a3' in
          let res = update_at_pos a1' l2' l3 in
          Vector (res, t1)
      | Int ->
          let l2 = Array.to_list (Array.map extract_int a2) in
          let l2' = List.filter (fun x -> x <> Some 0) l2 in
          let n, m = (List.length l2', Array.length a3) in
          if n mod m <> 0 then raise Replacement_length_not_multiple;
          if t1 <> t3 then raise (type_error t1 t3);
          let a3' = recycle a3 n in
          let l3 = Array.to_list a3' in
          let res = update_at_pos a1 l2' l3 in
          Vector (res, t1)
      end
  | Subset1_Neg (e1, e2) ->
      (* TODO: Really should make a separate Negate expression,
         handling NAs is rough*)
      let Vector (a1, t1) = eval e1 in
      let Vector (a2, t2) = eval e2 in
      if t2 <> Int then raise (type_error Int t2);
      if Array.for_all is_na a2 then
        let res = Array.make (Array.length a2) NA_int in
        Vector (res, t1)
      else
        let l2 = Array.to_list (Array.map extract_int a2) in
        let a2' = neg_vec_to_bool l2 (Array.length a1) in
        let l2' = bool_vec_to_pos (Array.to_list a2') 1 in
        let res = Array.of_list (get_at_pos a1 l2' t1) in
        Vector (res, t1)
  | Subset2 (e1, e2) ->
      let Vector (a1, t1) = eval e1 in
      let Vector (a2, t2) = eval e2 in
      if Array.length a2 < 1 then raise Selecting_lt_one_element;
      if Array.length a2 > 1 then raise Selecting_gt_one_element;
      if t2 <> Int then raise (type_error Int t2);
      begin match extract_int a2.(0) with
      | None -> raise Subscript_out_of_bounds
      | Some n when n = 0 -> raise Selecting_lt_one_element
      | Some n when n < 0 -> raise Selecting_gt_one_element
      | Some n when n > Array.length a1 -> raise Subscript_out_of_bounds
      | Some n -> vec_of_lit a1.(n - 1)
      end
  | Subset2_Assign (e1, e2, e3) ->
      let Vector (a1, t1) = eval e1 in
      let Vector (a2, t2) = eval e2 in
      let Vector (a3, t3) = eval e3 in
      let n, m = (Array.length a2, Array.length a3) in
      if n <> 1 then raise Selecting_gt_one_element;
      if m <> 1 then raise Too_many_elements_supplied;
      if t1 <> t3 then raise (type_error t1 t3);
      if t2 <> Int then raise (type_error Int t2);
      begin match extract_int a2.(0) with
      | None -> raise Subscript_out_of_bounds
      | Some n when n = 0 -> raise Selecting_lt_one_element
      | Some n when n < 0 -> raise Selecting_gt_one_element
      | Some n when n > Array.length a1 -> raise Subscript_out_of_bounds
      | Some n ->
          let a1' = Array.copy a1 in
          Array.set a1' (n - 1) a3.(0);
          Vector (a1', t1)
      end
