include Expression_intf

let ( @ ) a b = Pair (a, b)

let rec pp : type a. Format.formatter -> (a, 'tag) t -> unit =
 fun f ->
  let open Fmt in
  function
  | Column { selectable; column } ->
    (const string selectable ++ const string "." ++ const string column.name)
      f ()
  | Pair (a, b) -> (const pp a ++ const string ", " ++ const pp b) f ()
