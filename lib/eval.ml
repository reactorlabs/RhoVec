open Expr

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

exception Replacement_length_not_multiple
exception Replacement_length_is_zero
exception Too_many_elements_supplied

exception Expected_nonempty_vector

exception Todo

let get_bool l =
  match l with
  | Bool b -> Some b
  | NA_bool -> None
  | Int _ | NA_int -> raise (type_error T_Bool (get_tag l))

let get_int l =
  match l with
  | Int i -> Some i
  | NA_int -> None
  | Bool _ | NA_bool -> raise (type_error T_Int (get_tag l))

let get_array = function
  | Vector (a, _) -> a
let get_type = function
  | Vector (_, t) -> t

(* Check that all types are the same.
   If they all have type T, return Some T; otherwise return None. *)
let get_common_type = function
  | [] -> raise Expected_nonempty_vector
  | hd :: tl -> (
      (* Try to find a different type. *)
      let res = List.find_opt (fun x -> x <> hd) tl in
      match res with
      | Some t -> raise (type_error hd t)
      | None -> hd )

let negate_int = function
  | Int i -> Int ~-i
  | NA_int -> NA_int
  | Bool _ | NA_bool -> raise (type_error T_Int T_Bool)

(* Check that all elements are non-negative or NA.
   0 and NA are allowed for positive subsetting. *)
let is_positive_subsetting =
  let is_non_neg_or_na = function
    | None -> true
    | Some i -> i >= 0 in
  Array.for_all is_non_neg_or_na

(* Check that all indices are 0 *)
let is_zero_subsetting = Array.for_all (fun x -> x = Some 0)

(* Check that all elements are negative.
   0 and NA are not allowed for negative subsetting, but we check that elsewhere. *)
let is_negative_subsetting =
  let is_non_pos_or_na = function
    | None -> true
    | Some i -> i <= 0 in
  Array.for_all is_non_pos_or_na

let contains_na = Array.exists (fun x -> x = None)

(* Extends an array `a` so it has length `n`, filling empty elements with NAs of type `t`.
   Does nothing if `a` is longer than `n`. *)
let extend n t a =
  let m = Array.length a in
  if m >= n then a
  else
    (* Create a new array, pre-filling it with NAs. *)
    let res = Array.make n (na_lit t) in
    for i = 0 to m - 1 do
      res.(i) <- a.(i)
    done ;
    res

(* Returns a new array of size `n` that recycles the elements in `a`.
   Does nothing if `a` is longer than `n`. *)
let recycle n t a =
  let m = Array.length a in
  if m >= n then a
  else
    (* Use NA as a placeholder that will be overwritten. *)
    let res = Array.make n (na_lit t) in
    for i = 0 to n - 1 do
      res.(i) <- a.(i mod m)
    done ;
    res

(* TODO: better way of handling arrays vs lists, literals vs Options, etc *)
(* TODO: Just get rid of lists, go back to arrays *)

(* Uses the indices in `idx` to select elements out of the array `a`, using 1-indexing. *)
let get_at_pos t a idx =
  let is_neg = function
    | Some i -> i < 0
    | None -> false in
  if Array.exists is_neg idx then raise Mixing_with_negative_subscripts ;
  idx |>
  Array.map
    (function
      | Some 0 -> None
      | Some i when 1 <= i && i <= Array.length a -> Some a.(i - 1)
      | Some _ | None -> Some (na_lit t))
  |> Array.to_list |> List.filter Option.is_some |> Array.of_list |> Array.map Option.get

(* Convert a boolean vector (used for subsetting) to a positional vector. *)
let bool_to_pos_vec idx =
  idx
  |> Array.mapi (fun i x ->
         match x with
         | Some true -> Some (Some (i + 1))
         | None -> Some None
         | Some false -> None)
  |> Array.to_list |> List.filter Option.is_some |> Array.of_list |> Array.map Option.get

(* Convert a negative vector (used for subsetting) to a boolean vector. *)
let neg_to_bool_vec n idx =
  let is_pos_or_na = function
    | Some i -> i > 0
    | None -> true in
  if Array.exists is_pos_or_na idx then raise Mixing_with_negative_subscripts ;
  let res = Array.make n (Some true) in
  idx
  |> Array.map (Option.map (fun x -> ~-x))
  |> Array.iter (function
       | Some i when 1 <= i && i <= n -> res.(i - 1) <- Some false
       | Some _ | None -> ()) ;
  res

(* Update the vector `a` with the values in vector `rpl` at positions specified by `idx`. *)
let update_at_pos t a idx rpl =
  let len = Array.length a in
  let max_idx = Array.fold_left (fun n x ->
    match x with
    | Some i -> max i n
    | None -> n)
  len idx in
  let res = extend max_idx t a in
  Array.iter2 (fun n x ->
    match n with
    | Some i when 1 <= i -> res.(i - 1) <- x
    | Some _ -> raise Mixing_with_negative_subscripts
    | None -> ()) idx rpl ;
  res

let rec eval e =
  match e with
  | Lit l -> vec_of_lit l
  | Combine es ->
      let vecs = List.map eval es in
      let ty = vecs |> List.map get_type |> get_common_type in
      let vec = vecs |> List.map get_array |> Array.concat in
      Vector (vec, ty)
  | Negate e1 -> (
      let (Vector (a1, t1)) = eval e1 in
      match t1 with
      | T_Int ->
          let res = Array.map negate_int a1 in
          Vector (res, t1)
      | T_Bool -> raise (type_error T_Int t1) )
  | Subset1_Nothing e1 -> eval e1
  | Subset1 (e1, e2) -> (
      let (Vector (a1, t1)) = eval e1 in
      let (Vector (a2, t2)) = eval e2 in
      let n1, n2 = (Array.length a1, Array.length a2) in
      match t2 with
      | T_Bool ->
          let len = Stdlib.max n1 n2 in
          let a1 = extend len t1 a1 in
          let res =
            a2 |> recycle len t2 |> Array.map get_bool |> bool_to_pos_vec |> get_at_pos t1 a1 in
          Vector (res, t1)
      | T_Int ->
          let a2 = Array.map get_int a2 in
          if is_positive_subsetting a2 then
            let res = get_at_pos t1 a1 a2 in
            Vector (res, t1)
          else if is_negative_subsetting a2 then
            let res = a2 |> neg_to_bool_vec n1 |> bool_to_pos_vec |> get_at_pos t1 a1 in
            Vector (res, t1)
          else raise Mixing_with_negative_subscripts )
  | Subset2 (e1, e2) -> (
      let (Vector (a1, _)) = eval e1 in
      let (Vector (a2, t2)) = eval e2 in
      let n1, n2 = (Array.length a1, Array.length a2) in
      if n2 = 0 then raise Selecting_lt_one_element ;
      if n2 > 1 then raise Selecting_gt_one_element ;
      if t2 <> T_Int then raise (type_error T_Int t2) ;
      match get_int a2.(0) with
      | None -> raise Subscript_out_of_bounds
      | Some i ->
          if i = 0 then raise Selecting_lt_one_element ;
          if i < 0 then raise Invalid_negative_subscript ;
          if i > n1 then raise Subscript_out_of_bounds ;
          vec_of_lit a1.(i - 1) )
  | Subset1_Nothing_Assign (e1, e2) ->
      let (Vector (a1, t1)) = eval e1 in
      let (Vector (a2, t2)) = eval e2 in
      let n1, n2 = (Array.length a1, Array.length a2) in
      if n2 = 0 then raise Replacement_length_is_zero ;
      if n1 mod n2 <> 0 then raise Replacement_length_not_multiple ;
      if t1 <> t2 then raise (type_error t1 t2) ;
      let res = recycle n1 t2 a2 in
      Vector (res, t2)
  | Subset1_Assign (e1, e2, e3) -> (
      let (Vector (a1, t1) as base) = eval e1 in
      let (Vector (a2, t2)) = eval e2 in
      let (Vector (a3, t3)) = eval e3 in
      let n1, n2, n3 = (Array.length a1, Array.length a2, Array.length a3) in
      match t2 with
      | T_Bool ->
          if Array.exists is_na a2 then
            (* We diverge from R and ban NAs as indices during assignment *)
            raise No_NAs_in_subscripted_assignment ;
          let len = Stdlib.max n1 n2 in
          let a1 = extend len t1 a1 in
          let a2 = a2 |> recycle len t2 |> Array.map get_bool |> bool_to_pos_vec in
          let n2 = Array.length a2 in
          if n3 = 0 then raise Replacement_length_is_zero ;
          if n2 mod n3 <> 0 then raise Replacement_length_not_multiple ;
          if t1 <> t3 then raise (type_error t1 t3) ;
          let res = a3 |> recycle n2 t3 |> update_at_pos t1 a1 a2 in
          Vector (res, t1)
      | T_Int ->
          let a2 = Array.map get_int a2 in
          if is_zero_subsetting a2 then base
          else if is_positive_subsetting a2 then (
            if contains_na a2 then
              (* We diverge from R and ban NAs as indices during assignment *)
              raise No_NAs_in_subscripted_assignment ;
            let a2 = a2 |> Array.to_list |> List.filter (fun x -> x <> Some 0) |> Array.of_list in
            let n2 = Array.length a2 in
            if n3 = 0 then raise Replacement_length_is_zero ;
            if n2 mod n3 <> 0 then raise Replacement_length_not_multiple ;
            if t1 <> t3 then raise (type_error t1 t3) ;
            let res = a3 |> recycle n2 t3 |> update_at_pos t1 a1 a2 in
            Vector (res, t1) )
          else if is_negative_subsetting a2 then (
            if contains_na a2 then
              (* We diverge from R and ban NAs as indices during assignment *)
              raise No_NAs_in_subscripted_assignment ;
            let a2 = a2 |> neg_to_bool_vec n1 |> bool_to_pos_vec in
            let n2 = Array.length a2 in
            if n3 = 0 then raise Replacement_length_is_zero ;
            if n2 mod n3 <> 0 then raise Replacement_length_not_multiple ;
            if t1 <> t3 then raise (type_error t1 t3) ;
            let res = a3 |> recycle n2 t3 |> update_at_pos t1 a1 a2 in
            Vector (res, t1) )
          else raise Mixing_with_negative_subscripts )
  | Subset2_Assign (e1, e2, e3) -> (
      let (Vector (a1, t1)) = eval e1 in
      let (Vector (a2, t2)) = eval e2 in
      let (Vector (a3, t3)) = eval e3 in
      let n1, n2, n3 = (Array.length a1, Array.length a2, Array.length a3) in
      if n2 = 0 then raise Selecting_lt_one_element ;
      if n2 > 1 then raise Selecting_gt_one_element ;
      if n3 = 0 then raise Replacement_length_is_zero ;
      if n3 > 1 then raise Too_many_elements_supplied ;
      if t1 <> t3 then raise (type_error t1 t3) ;
      if t2 <> T_Int then raise (type_error T_Int t2) ;
      match get_int a2.(0) with
      | None -> raise Subscript_out_of_bounds
      | Some i ->
          if i = 0 then raise Selecting_lt_one_element ;
          if i < 0 then raise Selecting_gt_one_element ;
          if i > n1 then raise Subscript_out_of_bounds ;
          let a1' = Array.copy a1 in
          a1'.(i - 1) <- a3.(0) ;
          Vector (a1', t1) )
