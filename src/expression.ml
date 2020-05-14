include Expression_intf

let pp f =
  let open Fmt in
  function
  | Column { selectable; column } ->
    (const string selectable ++ const string "." ++ const string column.name)
      f ()
