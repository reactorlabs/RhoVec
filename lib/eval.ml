open Expr
open Util

type type_error =
  { expected : type_tag
  ; received : type_tag
  }
exception Type_error of type_error
let type_error expected received = Type_error { expected; received }

exception Subscript_out_of_bounds
exception Selecting_lt_one_element
exception Selecting_gt_one_element

exception Mixing_with_negative_subscripts
exception Invalid_negative_subscript
exception No_NAs_in_subscripted_assignment

exception Expected_nonempty_vector
exception Replacement_length_not_multiple
exception Replacement_length_is_zero
exception Too_many_elements_supplied

exception Object_not_found

(* Gets a boolean value from a bool Literal.
   Wraps the result in an Option, so that NA is represented by None. *)
let get_bool = function
  | Bool b -> Some b
  | NA_bool -> None
  | (Int _ | NA_int) as l -> raise (type_error T_Bool (get_tag l))

(* Gets an integer value from a int Literal.
   Wraps the result in an Option, so that NA is represented by None. *)
let get_int = function
  | Int i -> Some i
  | NA_int -> None
  | (Bool _ | NA_bool) as l -> raise (type_error T_Int (get_tag l))

(* Checks that all types are the same.
   If they all have type T, returns T. Raises exceptions if there are multiple types, or if the
   vector is empty (since we can't type it). *)
let get_common_type = function
  | [] -> assert false
  | hd :: tl -> (
      (* Try to find a different type. *)
      let res = List.find_opt (fun x -> x <> hd) tl in
      match res with
      | None -> hd
      | Some t -> raise (type_error hd t) )

(* Negates an int literal.
   Negating an NA returns an NA. Raises if given a non-int Literal. *)
let negate_int = function
  | Int i -> Int ~-i
  | NA_int -> NA_int
  | Bool _ | NA_bool -> raise (type_error T_Int T_Bool)

(* Checks that all elements are non-negative or NA.
   0 and NA are allowed for positive subsetting. *)
let is_positive_subsetting = Array.for_all @@ Option.map_or ~default:true (fun x -> x >= 0)

let is_zero_subsetting = Array.for_all (fun x -> x = Some 0)

(* Checks that all elements are non-positive.
   NA is _not_ allowed for negative subsetting, but we check that elsewhere. *)
let is_negative_subsetting = Array.for_all @@ Option.map_or ~default:true (fun x -> x <= 0)

let contains_na = Array.exists (fun x -> x = None)

(* Grows an array `a` so it has length `n`. Returns a copy if `a` is longer than `n`.
   If the mode is `Extend, then new elements are filled with NAs.
   If the mode is `Recycle, then new elements are filled by repeating the elements of `a`. *)
let grow ~mode n t (a : literal array) =
  let m = Array.length a in
  if m >= n then Array.copy a
  else
    let res = Array.make n (na_lit t) in
    ( match mode with
    | `Extend ->
        for i = 0 to m - 1 do
          (* Only copy over m elements from a to res; leave the rest as NA. *)
          res.(i) <- a.(i)
        done
    | `Recycle ->
        for i = 0 to n - 1 do
          (* Cycle through a; all elements of res are written to. *)
          res.(i) <- a.(i mod m)
        done ) ;
    res

(* Extends an array `a` so it has length `n`, filling empty elements with NAs of type `t`.
   Does nothing if `a` is longer than `n`. *)
let extend n t a = grow ~mode:`Extend n t a

(* Returns a new array of size `n` that recycles the elements in `a`.
   Does nothing if `a` is longer than `n`. *)
let recycle n t a = grow ~mode:`Recycle n t a

(* Uses the indices in `idx` to select elements out of the array `a`:
   - Valid indices are converted from 1-based indexing (R) to 0-based indexing (OCaml).
   - 0 indices are dropped.
   - Out-of-bounds and NA indices return NA.
   Expects the inputs to be valid. *)
let get_at_pos t a (idxs : int option array) =
  assert (is_positive_subsetting idxs) ;
  idxs
  |> Array.filter_map (function
       | Some i when 1 <= i && i <= Array.length a -> Some a.(i - 1)
       | Some 0 -> None
       | Some _ | None -> Some (na_lit t))

(* Convert a boolean vector (used for subsetting) to a positional vector:
  - True indices are converted to positional indices.
  - False indices are dropped.
  - NA indices stay as NA. *)
let bool_to_pos_vec (idxs : bool option array) =
  idxs
  |> Array.filter_mapi (fun i x ->
         match x with
         | Some true -> Some (Some (i + 1))
         | Some false -> None
         | None -> Some None)

(* Convert a negative vector (used for subsetting) to a boolean vector. Negate the indices and use
   them to exclude values, i.e. convert them to false indices. Out-of-bounds are ignored.
   Expects the inputs to be valid (i.e. no NAs). *)
let neg_to_bool_vec n (idxs : int option array) =
  assert (is_negative_subsetting idxs) ;
  assert (Stdlib.not @@ contains_na idxs) ;
  let res = Array.make n (Some true) in
  idxs |> Array.map Option.get
  |> Array.map (fun x -> ~-x)
  |> Array.iter (fun i -> if 1 <= i && i <= n then res.(i - 1) <- Some false) ;
  res

(* Update the vector `a` with the values in vector `rpl` at positions specified by `idx`.
   Extends `a` with NAs if an index is (positive and) out-of-bounds.
   Expects the inputs to be valid (i.e. no 0 or NA indices). *)
let update_at_pos t a (idxs : int option array) rpl =
  assert (is_positive_subsetting idxs) ;
  assert (Array.for_all (fun x -> x <> Some 0) idxs) ;
  assert (Stdlib.not @@ contains_na idxs) ;
  let idxs = Array.map Option.get idxs in
  let max_idx = Array.fold_left (fun mx i -> max mx i) (Array.length a) idxs in
  let res = extend max_idx t a in
  Array.iter2 (fun i x -> res.(i - 1) <- x) idxs rpl ;
  res

let check_subset1_assign_err base_ty idx_len repl_len repl_ty =
  if repl_len = 0 then raise Replacement_length_is_zero ;
  if idx_len mod repl_len <> 0 then raise Replacement_length_not_multiple ;
  if base_ty <> repl_ty then raise (type_error base_ty repl_ty) ;
  ()

let check_subset2_err n t =
  if n = 0 then raise Selecting_lt_one_element ;
  if n > 1 then raise Selecting_gt_one_element ;
  if t <> T_Int then raise (type_error T_Int t) ;
  ()

let lookup env x =
  match Env.find_opt x env with
  | None -> raise Object_not_found
  | Some v -> v

let rec eval env = function
  | Lit l -> (env, vec_of_lit l)
  | Var x -> (env, lookup env x)
  | Combine es ->
      if List.length es = 0 then raise Expected_nonempty_vector ;
      let env, vecs = List.fold_map eval env es in
      let t = vecs |> List.map (function Vector (_, t) -> t) |> get_common_type in
      let v = vecs |> List.map (function Vector (a, _) -> a) |> Array.concat in
      (env, vector v t)
  | Negate e1 -> (
      let env, Vector (a1, t1) = eval env e1 in
      match t1 with
      | T_Int ->
          let res = Array.map negate_int a1 in
          (env, vector res t1)
      | T_Bool -> raise (type_error T_Int t1) )
  | Subset1 (e1, None) -> eval env e1
  | Subset1 (e1, Some e2) -> (
      let env, Vector (a1, t1) = eval env e1 in
      let env, Vector (a2, t2) = eval env e2 in
      let n1, n2 = (Array.length a1, Array.length a2) in
      match t2 with
      | T_Bool ->
          let len = Stdlib.max n1 n2 in
          let a1 = extend len t1 a1 in
          let res =
            a2 |> recycle len t2 |> Array.map get_bool |> bool_to_pos_vec |> get_at_pos t1 a1 in
          (env, vector res t1)
      | T_Int ->
          let a2 = Array.map get_int a2 in
          if is_positive_subsetting a2 then
            let res = get_at_pos t1 a1 a2 in
            (env, vector res t1)
          else if is_negative_subsetting a2 && not (contains_na a2) then
            let res = a2 |> neg_to_bool_vec n1 |> bool_to_pos_vec |> get_at_pos t1 a1 in
            (env, vector res t1)
          else raise Mixing_with_negative_subscripts )
  | Subset2 (e1, e2) -> (
      let env, Vector (a1, _) = eval env e1 in
      let env, Vector (a2, t2) = eval env e2 in
      let n1, n2 = (Array.length a1, Array.length a2) in
      check_subset2_err n2 t2 ;
      match get_int a2.(0) with
      | None -> raise Subscript_out_of_bounds
      | Some i ->
          if i = 0 then raise Selecting_lt_one_element ;
          if i < 0 then raise Invalid_negative_subscript ;
          if i > n1 then raise Subscript_out_of_bounds ;
          (env, vec_of_lit a1.(i - 1)) )
  | Assign (x, e) ->
      let env, vec = eval env e in
      let env = Env.add x vec env in
      (env, vec)
  | Seq es ->
      assert (List.length es <> 0) ;
      let rec inner env acc = function
        | [] -> (env, acc)
        | hd :: tl ->
            let env, acc = eval env hd in
            inner env acc tl in
      let env, vec = eval env (List.hd es) in
      inner env vec (List.tl es)
  | Subset1_Assign (x1, None, e3) ->
      let (Vector (a1, t1)) = lookup env x1 in
      let env, (Vector (a3, t3) as rhs) = eval env e3 in
      let n1, n3 = (Array.length a1, Array.length a3) in
      check_subset1_assign_err t1 n1 n3 t3 ;
      let res = recycle n1 t3 a3 in
      let env = Env.add x1 (vector res t3) env in
      (env, rhs)
  | Subset1_Assign (x1, Some e2, e3) -> (
      let (Vector (a1, t1)) = lookup env x1 in
      let env, Vector (a2, t2) = eval env e2 in
      let env, (Vector (a3, t3) as rhs) = eval env e3 in
      let n1, n2, n3 = (Array.length a1, Array.length a2, Array.length a3) in
      (* We diverge from R and ban NAs as indices during assignment *)
      if Array.exists is_na a2 then raise No_NAs_in_subscripted_assignment ;
      match t2 with
      | T_Bool ->
          let len = Stdlib.max n1 n2 in
          let a1 = extend len t1 a1 in
          let a2 = a2 |> recycle len t2 |> Array.map get_bool |> bool_to_pos_vec in
          let n2 = Array.length a2 in
          check_subset1_assign_err t1 n2 n3 t3 ;
          let res = a3 |> recycle n2 t3 |> update_at_pos t1 a1 a2 in
          let env = Env.add x1 (vector res t1) env in
          (env, rhs)
      | T_Int ->
          let a2 = Array.map get_int a2 in
          if is_zero_subsetting a2 then (env, rhs)
          else if is_positive_subsetting a2 then (
            let a2 = Array.filter (fun x -> x <> Some 0) a2 in
            let n2 = Array.length a2 in
            check_subset1_assign_err t1 n2 n3 t3 ;
            let res = a3 |> recycle n2 t3 |> update_at_pos t1 a1 a2 in
            let env = Env.add x1 (vector res t1) env in
            (env, rhs) )
          else if is_negative_subsetting a2 then (
            let a2 = a2 |> neg_to_bool_vec n1 |> bool_to_pos_vec in
            let n2 = Array.length a2 in
            check_subset1_assign_err t1 n2 n3 t3 ;
            let res = a3 |> recycle n2 t3 |> update_at_pos t1 a1 a2 in
            let env = Env.add x1 (vector res t1) env in
            (env, rhs) )
          else raise Mixing_with_negative_subscripts )
  | Subset2_Assign (x1, e2, e3) -> (
      let (Vector (a1, t1)) = lookup env x1 in
      let env, Vector (a2, t2) = eval env e2 in
      let env, (Vector (a3, t3) as rhs) = eval env e3 in
      let n2, n3 = (Array.length a2, Array.length a3) in
      check_subset2_err n2 t2 ;
      if n3 = 0 then raise Replacement_length_is_zero ;
      if n3 > 1 then raise Too_many_elements_supplied ;
      if t1 <> t3 then raise (type_error t1 t3) ;
      match get_int a2.(0) with
      | None -> raise Subscript_out_of_bounds
      | Some i ->
          if i = 0 then raise Selecting_lt_one_element ;
          if i < 0 then raise Selecting_gt_one_element ;
          let a1 = extend i t1 a1 in
          a1.(i - 1) <- a3.(0) ;
          let env = Env.add x1 (vector a1 t1) env in
          (env, rhs) )

let run exp = Stdlib.snd (eval Env.empty exp)
