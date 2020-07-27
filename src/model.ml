open Base
open Poly
include Model_intf

module Make (Meta : sig
  val version : Semver.t
end) =
struct
  let version = Meta.version

  type tag = unit

  type t = { mutable tables : (module Table.Meta) Array.t }

  let v = { tables = [||] }

  let tables () = v.tables

  module BindTable (Meta : Table.Meta) = struct
    type model = unit

    module Meta = Meta

    let _ = v.tables <- Array.append v.tables [| (module Meta) |]

    type tag = unit

    let name = Meta.name

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
        Expression.List.Cons (Expression.make_column name hd, bind_columns tl)
      | Column.List.Empty -> Expression.List.Empty

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

    let left : type a. (a, L.tag) Expression.t -> (a, tag) Expression.t =
     fun e -> Expression.map_name (fun n -> "l_" ^ n) e

    let right : type a. (a, R.tag) Expression.t -> (a, tag) Expression.t =
     fun e -> Expression.map_name (fun n -> "r_" ^ n) e

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
          * (bool, 'tag) Expression.t option
          -> 't t

    let pp f =
      let open Fmt in
      function
      | Select (e, s, where) ->
        let module Selectable = (val s) in
        ( box
        @@ const string "SELECT" ++ sp ++ const Expression.pp e ++ sp
           ++ const string "FROM" ++ sp ++ const Selectable.pp ()
           ++ const
                (option (sp ++ const string "WHERE" ++ sp ++ Expression.pp))
                where )
          f ()
  end

  let select ~from ?where e = Query.Select (e, from, where)

  (* Metadata *)

  module Metadata = struct
    type t = {
      major : int;
      minor : int;
      subminor : int;
      migrated : Ptime.t;
    }

    module Meta_ = struct
      type mapped = t

      let schema = Some "sqml"

      let name = "metadata"

      let major = Column.make "major" Type.Int

      let minor = Column.make "minor" Type.Int

      let subminor = Column.make "subminor" Type.Int

      let migrated = Column.make "migrated" Type.Timestamp

      type columns = int * (int * (int * (Ptime.t * unit)))

      let columns =
        Column.List.(
          Cons (major, Cons (minor, Cons (subminor, Cons (migrated, Empty)))))

      let map (major, (minor, (subminor, (migrated, ())))) =
        { major; minor; subminor; migrated }

      let unmap { major; minor; subminor; migrated } =
        (major, (minor, (subminor, (migrated, ()))))
    end

    include BindTable (Meta_)
  end
end
