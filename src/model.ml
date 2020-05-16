open Base
open Poly
include Model_intf

module Make () = struct
  type tag = unit

  type t = { mutable tables : (module Table.Meta) Array.t }

  let v = { tables = [||] }

  let tables () = v.tables

  module BindTable (Meta : Table.Meta) = struct
    type model = unit

    let _ = v.tables <- Array.append v.tables [| (module Meta) |]

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

  module Join
      (L : Selectable.S with type model = tag)
      (R : Selectable.S with type model = tag) =
  struct
    type model = tag

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

  module Query = struct
    type 't t =
      | Select :
          ('t, 'tag) Expression.t
          * (module Selectable.S with type model = tag and type tag = 'tag)
          -> 't t

    let pp f =
      let open Fmt in
      function
      | Select (e, s) ->
        let module Selectable = (val s) in
        ( box
        @@ const string "SELECT" ++ sp ++ const Expression.pp e ++ sp
           ++ const string "FROM" ++ sp ++ const Selectable.pp () )
          f ()
  end

  let select t e = Query.Select (e, t)
end
