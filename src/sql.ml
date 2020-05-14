module Join (L : Selectable.S) (R : Selectable.S) = struct
  type tag = unit

  let left = function
    | Expression.Column { selectable; column } ->
      Expression.Column { selectable = "l_" ^ selectable; column }

  let right = function
    | Expression.Column { selectable; column } ->
      Expression.Column { selectable = "r_" ^ selectable; column }

  let pp_ ~prefix:p =
    let open Fmt in
    const (L.pp_ ~prefix:(p ^ "l_")) ()
    ++ sp ++ const string "JOIN" ++ sp
    ++ const (R.pp_ ~prefix:(p ^ "r_")) ()

  let pp = pp_ ~prefix:""
end

let select t e = Query.Select (e, t)
