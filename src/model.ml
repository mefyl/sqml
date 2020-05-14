module BindTable (Meta : Table.Meta) = struct
  type tag = unit

  let name = Meta.name

  open Expression

  module Expression = struct
    include Expression

    type nonrec 't t = ('t, tag) t

    module List = S_list.Make (struct
      type nonrec 'a t = 'a t
    end)
  end

  let rec bind_columns : type l. l Column.List.t -> l Expression.List.t =
    function
    | Column.List.Cons (hd, tl) ->
      Expression.List.Cons
        (Column { selectable = name; column = hd }, bind_columns tl)
    | Column.List.Empty -> Expression.List.Empty

  type columns = Meta.columns

  let columns = bind_columns Meta.columns

  let pp_ ~prefix:p =
    if p = "" then
      let open Fmt in
      const string name
    else
      let open Fmt in
      const string name ++ sp ++ const string (p ^ name)

  let pp = pp_ ~prefix:""
end
