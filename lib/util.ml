module Array = struct
  include Array

  let filter f a = a |> Array.to_list |> List.filter f |> Array.of_list

  let filter_map f a = a |> Array.to_list |> List.filter_map f |> Array.of_list

  let filter_mapi f a = a |> Array.mapi f |> filter_map Fun.id
end

module List = struct
  include List

  let rec fold_map f acc l =
    match l with
    | [] -> (acc, [])
    | hd :: tl ->
        let acc, hd = f acc hd in
        let acc, tl = fold_map f acc tl in
        (acc, hd :: tl)
end

module Option = struct
  include Option

  let map_or ~default f = function
    | None -> default
    | Some x -> f x
end
