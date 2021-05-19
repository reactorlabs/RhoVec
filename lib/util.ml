module Array = struct
  include Array

  let filter f a = a |> Array.to_list |> List.filter f |> Array.of_list

  let filter_map f a = a |> Array.to_list |> List.filter_map f |> Array.of_list

  let filter_mapi f a = a |> Array.mapi f |> filter_map Fun.id
end

module Option = struct
  include Option

  let map_or ~default f = function
    | None -> default
    | Some x -> f x
end

module String = struct
  include String

  let prefix ~pre s =
    let len_p = String.length pre in
    if len_p > String.length s then false
    else
      let rec aux i =
        if i = len_p then true
        else if String.unsafe_get pre i <> String.unsafe_get s i then false
        else aux (i + 1) in
      aux 0

  let chop_prefix ~pre s =
    if prefix ~pre s then
      let len_p = String.length pre in
      Some (String.sub s len_p (String.length s - len_p))
    else None
end
