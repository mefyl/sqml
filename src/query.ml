include Query_intf

let pp f =
  let open Fmt in
  function
  | Select (e, s) ->
    let module Selectable = (val s) in
    ( box
    @@ const string "SELECT" ++ sp ++ const Expression.pp e ++ sp
       ++ const string "FROM" ++ sp ++ const Selectable.pp () )
      f ()
