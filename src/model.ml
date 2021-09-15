open Base
open Poly
open Utils
include Model_intf

module Make (Meta : sig
  val version : Semver.t
end) =
struct
  let version = Meta.version

  type tag = unit

  type t = { mutable tables : (module Table.S) Array.t }

  let v = { tables = [||] }

  let tables () = v.tables

  module BindTable (Meta : Table.META) = struct
    module Res = struct
      type model = unit

      module Meta = Meta

      type tag = unit

      let name = Meta.name

      module Column = struct
        type nonrec 't t = ('t, tag) Expression.column

        let rec bind_columns :
            type l.
            l Table.Column.List.t ->
            (l, tag) Expression.Column_list.t * tag Table.constraint_ list =
          function
          | Table.Column.List.Cons ({ name; type_; foreign_keys; _ }, tl) ->
            let column =
              Expression.make_column ~selectable:Meta.name ~name type_
            in
            let constraints =
              let f (Table.Column.Reference target) =
                Table.(
                  Foreign_key
                    (Table.Nonempty_list.make
                       (Foreign_reference { target; column })))
              in
              List.map ~f foreign_keys
            and rest, rest_constraints = bind_columns tl in
            ( Expression.Column_list.Cons (column, rest),
              constraints @ rest_constraints )
          | Empty -> (Expression.Column_list.Empty, [])

        let columns, constraints = bind_columns Meta.columns
      end

      module Expression = struct
        include Expression

        type nonrec 't t = ('t, tag) t

        let rec bind_expressions :
            type l tag.
            (l, tag) Expression.Column_list.t -> (l, tag) Expression.List.t =
          function
          | Expression.Column_list.Cons (hd, tl) ->
            List.Cons (column hd, bind_expressions tl)
          | Empty -> List.Empty

        let columns = bind_expressions Column.columns
      end

      let all =
        let rec pairs_of_list :
            type l. (l, tag) Expression.List.t -> l Expression.t = function
          | Expression.List.Cons (hd, tl) -> Expression.(hd @ pairs_of_list tl)
          | Expression.List.Empty -> Expression.null
        in
        Expression.map ~encode:Meta.unmap ~decode:Meta.map
          (pairs_of_list Expression.columns)

      let pp_ ~prefix:p =
        if p = "" then
          let open Fmt in
          const string name
        else
          let open Fmt in
          const string name ++ sp ++ const string (p ^ name)

      let pp = pp_ ~prefix:""

      let constraints = Column.constraints @ Meta.constraints Column.columns
    end

    let _ = v.tables <- Array.append v.tables [| (module Res) |]

    include Res
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
    type many = Many

    type zero = Zero

    type _ arity =
      | Many : many arity
      | Zero : zero arity

    type ('t, 'arity, 'error) result =
      | Many : ('t, 'error) Sequence.t -> ('t, many, 'error) result
      | Zero : ('t, zero, 'error) result

    type on_conflict =
      | Ignore
      | Update

    type ('t, 'arity) t =
      | Delete : {
          from : (module Table.S with type model = tag);
          where : (bool, 'tag) Expression.t option;
        }
          -> (unit, zero) t
      | Delete_returning : {
          from : (module Table.S with type model = tag);
          where : (bool, 'tag) Expression.t option;
          returning : ('t, 'tag) Expression.t;
        }
          -> ('t, many) t
      | Insert : {
          into :
            (module Table.S with type model = tag and type Meta.mapped = 'mapped);
          on_conflict : on_conflict option;
          value : 'mapped;
        }
          -> (unit, zero) t
      | Select : {
          from :
            (module Selectable.S with type model = tag and type tag = 'tag);
          what : ('t, 'tag) Expression.t;
          where : (bool, 'tag) Expression.t option;
        }
          -> ('t, many) t

    let arity : type a tag. (tag, a) t -> a arity = function
      | Delete _ -> Zero
      | Delete_returning _ -> Many
      | Insert _ -> Zero
      | Select _ -> Many

    let rec pp : type tag arity. Formatter.t -> (tag, arity) t -> unit =
      let open Fmt in
      let pp_clause keyword f e = Fmt.pf f "@ %s %a" keyword Expression.pp e in
      fun f -> function
        | Delete { from; where } ->
          let module Table = (val from) in
          Fmt.pf f "DELETE FROM@ %a%a" Table.pp ()
            (option (pp_clause "WHERE"))
            where
        | Delete_returning { from; returning; where } ->
          Fmt.pf f "%a%a" pp
            (Delete { from; where })
            (pp_clause "RETURNING") returning
        | Insert { into; _ } ->
          let module Table = (val into) in
          (box
          @@ const string "INSERT" ++ sp ++ const string "INTO" ++ sp
             ++ const Table.pp () ++ sp ++ const string "VALUES" ++ sp
             ++ const char '(' ++ const string "<val>" ++ const char ')')
            f ()
        | Select { from; what; where } ->
          let module Selectable = (val from) in
          (box
          @@ const string "SELECT" ++ sp ++ const Expression.pp what ++ sp
             ++ const string "FROM" ++ sp ++ const Selectable.pp ()
             ++ const
                  (option (sp ++ const string "WHERE" ++ sp ++ Expression.pp))
                  where)
            f ()
  end

  let select ~from ?where what = Query.Select { from; what; where }

  let insert ~into ?on_conflict value =
    Query.Insert { into; on_conflict; value }

  let delete ~from ?where () = Query.Delete { from; where }

  let delete_returning ~from ?where returning =
    Query.Delete_returning { from; returning; where }

  (* Metadata *)

  module Metadata = struct
    type t = {
      major : int;
      minor : int;
      subminor : int;
      migrated : Timmy.Time.t;
    }

    module Meta_ = struct
      type mapped = t

      let schema = Some "sqml"

      let name = "metadata"

      let major = Table.Column.make "major" Type.Int

      let minor = Table.Column.make "minor" Type.Int

      let subminor = Table.Column.make "subminor" Type.Int

      let migrated = Table.Column.make "migrated" Type.Time

      type columns = int * (int * (int * (Timmy.Time.t * unit)))

      let columns =
        Table.Column.List.(
          Cons (major, Cons (minor, Cons (subminor, Cons (migrated, Empty)))))

      let map (major, (minor, (subminor, (migrated, ())))) =
        { major; minor; subminor; migrated }

      let unmap { major; minor; subminor; migrated } =
        (major, (minor, (subminor, (migrated, ()))))

      let constraints _ = []
    end

    include BindTable (Meta_)
  end
end

type column_creation = {
  name : string;
  type_ : string;
  primary : bool;
}

let schema (module Dialect : Dialect.S) (module Model : S) =
  let create_table (module T : Table.S) =
    let columns =
      Table.Column.List.map
        ~f:
          {
            f =
              (fun { name; primary; type_; _ } ->
                Some { name; primary; type_ = Dialect.string_of_type type_ });
          }
        T.Meta.columns
    in
    let definitions =
      List.map ~f:(fun { name; type_; _ } -> Fmt.str "%s %s" name type_) columns
    in
    let definitions =
      match List.filter ~f:(fun c -> c.primary) columns with
      | [] -> definitions
      | primary ->
        definitions
        @ [
            Fmt.(
              str "PRIMARY KEY (%a)"
                (hbox @@ list ~sep:comma (using (fun c -> c.name) string))
                primary);
          ]
    in
    let definitions =
      let f = function
        | Table.Foreign_key references ->
          let f (Table.Foreign_reference { target; column }) =
            (column.name, target.column.name)
          in
          let columns, references =
            Table.Nonempty_list.to_list references |> List.map ~f |> List.unzip
          and target_table =
            let (Table.Foreign_reference { target; _ }) =
              Table.Nonempty_list.hd_nonempty references
            in
            Table.name target.table
          in
          Fmt.(
            str "FOREIGN KEY (%a) REFERENCES %s(%a)"
              (hbox @@ list ~sep:comma string)
              columns target_table
              (hbox @@ list ~sep:comma string)
              references)
      in
      definitions @ List.map ~f T.constraints
    in
    Fmt.(
      str "CREATE TABLE %s (%a)"
        (Dialect.table_name ?schema:T.Meta.schema T.Meta.name)
        (hbox @@ list ~sep:comma string)
        definitions)
  in
  Array.map ~f:create_table (Model.tables ()) |> Array.to_list
