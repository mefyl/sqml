open Acid
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
        type nonrec 't t = ('t, tag) Expression.Column.t

        let rec bind_columns :
            type l.
            l Table.Column.List.t ->
            (l, tag) Expression.Column.List.t * tag Table.constraint_ list =
          function
          | Table.Column.List.({ name; type_; foreign_keys; _ } :: tl) ->
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
            ( Expression.Column.List.(column :: rest),
              constraints @ rest_constraints )
          | [] -> (Expression.Column.List.[], [])

        let columns, constraints = bind_columns Meta.columns
      end

      let column_types =
        let rec type_of_columns :
            type a. (a, _) Expression.Column.List.t -> a Expression.Type.List.t
            = function
          | hd :: tl ->
            Expression.Type.of_column_type hd.type_ :: type_of_columns tl
          | [] -> []
        in
        type_of_columns Column.columns

      let type_ =
        let rec type_of_columns :
            type a.
            (a, _) Expression.Column.List.t -> a Hlist.t Expression.Type.t =
          function
          | hd :: tl ->
            Expression.Type.of_column_type hd.type_ :: type_of_columns tl
          | [] -> []
        in
        let decode columns = Meta.map columns |> Result.return
        and encode = Meta.unmap in
        Expression.Type.Custom
          { decode; encode; repr = type_of_columns Column.columns }

      let const value = Expression.const type_ value

      module Expression = struct
        let rec bind_expressions :
            type l tag.
            (l, tag) Expression.Column.List.t -> (l, tag) Expression.List.t =
          function
          | Expression.Column.List.(head :: tail) ->
            Expression.column head :: bind_expressions tail
          | Expression.Column.List.[] -> []

        let columns = bind_expressions Column.columns

        include Expression

        type nonrec 't t = ('t, tag) t

        type nonrec 't column = ('t, tag) Column.t
      end

      let all =
        let columns = Expression.(of_list columns) in
        Expression.map
          ~decode:(fun v -> Result.return (Meta.map v))
          ~encode:Meta.unmap columns

      let to_ast () = Selectable.Selectable { alias = None; name }

      let constraints = Column.constraints @ Meta.constraints Column.columns
    end

    let _ = v.tables <- Array.append v.tables [| (module Res) |]

    include Res
  end

  module type JOIN = sig
    include Selectable.S

    module L : Selectable.S with type model = model

    module R : Selectable.S with type model = model

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t

    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  let rec map_name f = function
    | Selectable.Selectable { name; alias } ->
      Selectable.Selectable
        { name; alias = Some (f (Option.value ~default:name alias)) }
    | Join ({ left; right; _ } as pl) ->
      Join { pl with left = map_name f left; right = map_name f right }

  module Join
      (L : Selectable.S with type model = tag)
      (R : Selectable.S with type model = tag) =
  struct
    module Content = struct
      module type S =
        JOIN with type model = tag and module L = L and module R = R

      type model = tag

      type tag = unit

      module L = L
      module R = R

      let left : type a. (a, L.tag) Expression.t -> (a, tag) Expression.t =
       fun e -> Expression.map_name (fun n -> "l_" ^ n) e

      let right : type a. (a, R.tag) Expression.t -> (a, tag) Expression.t =
       fun e -> Expression.map_name (fun n -> "r_" ^ n) e

      let to_ast_ on =
        Selectable.Join
          {
            left = map_name (fun n -> "l_" ^ n) @@ L.to_ast ();
            right = map_name (fun n -> "r_" ^ n) @@ R.to_ast ();
            on;
          }
    end

    let on expr =
      (module struct
        include Content

        let to_ast () = to_ast_ (Some (Expression.to_ast expr))
      end : Content.S)

    include Content

    let to_ast () = to_ast_ None
  end

  module Query = struct
    type many = Many

    type one = One

    type zero = Zero

    type _ arity =
      | Many : many arity
      | One : one arity
      | Zero : zero arity

    type ('t, 'arity, 'error) result =
      | Many : ('t, 'error) Sequence.t -> ('t, many, 'error) result
      | One : 't -> ('t, one, 'error) result
      | Zero : ('t, zero, 'error) result

    type 'tag excluded = {
      excluded : 't. ('t, 'tag) Expression.t -> ('t, 'tag) Expression.t;
    }

    type 'tag on_conflict =
      | Ignore : _ on_conflict
      | Set :
          ('tag excluded -> 'tag Expression.assignment list)
          -> 'tag on_conflict
      | Update : _ on_conflict

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
          excluded : 't. ('t, 'tag) Expression.t -> ('t, 'tag) Expression.t;
          into :
            (module Table.S
               with type model = tag
                and type Meta.mapped = 'mapped
                and type tag = 'tag);
          on_conflict : 'tag on_conflict option;
          value : ('mapped, 'free) Expression.t;
        }
          -> (unit, zero) t
      | Insert_returning : {
          excluded : 't. ('t, 'tag) Expression.t -> ('t, 'tag) Expression.t;
          into :
            (module Table.S
               with type model = tag
                and type Meta.mapped = 'mapped
                and type tag = 'tag);
          on_conflict : 'tag on_conflict option;
          returning : ('t, 'tag) Expression.t;
          value : ('mapped, 'free) Expression.t;
        }
          -> ('t, one) t
      | Select : {
          from :
            (module Selectable.S with type model = tag and type tag = 'tag);
          what : ('t, 'tag) Expression.t;
          where : (bool, 'tag) Expression.t option;
        }
          -> ('t, many) t
      | Update : {
          into : (module Table.S with type model = tag and type tag = 'tag);
          what : 'tag Expression.assignment list;
          where : (bool, 'tag) Expression.t option;
        }
          -> (unit, zero) t
      | Update_returning : {
          into : (module Table.S with type model = tag and type tag = 'tag);
          what : 'tag Expression.assignment list;
          where : (bool, 'tag) Expression.t option;
          returning : ('t, 'tag) Expression.t;
        }
          -> ('t, many) t

    let arity : type a tag. (tag, a) t -> a arity = function
      | Delete _ -> Zero
      | Delete_returning _ -> Many
      | Insert _ -> Zero
      | Insert_returning _ -> One
      | Select _ -> Many
      | Update _ -> Zero
      | Update_returning _ -> Many
  end

  let select ~from ?where what = Query.Select { from; what; where }

  let insert ~into ?on_conflict value =
    Query.Insert
      {
        excluded = (fun e -> Expression.map_name (fun _ -> "excluded") e);
        into;
        on_conflict;
        value;
      }

  let insert_returning ~into ~returning ?on_conflict value =
    Query.Insert_returning
      {
        excluded = (fun e -> Expression.map_name (fun _ -> "excluded") e);
        into;
        on_conflict;
        returning;
        value;
      }

  let delete ~from ?where () = Query.Delete { from; where }

  let delete_returning ~from ?where returning =
    Query.Delete_returning { from; returning; where }

  let update into ?where what = Query.Update { into; where; what }

  let update_returning into ?where ~returning what =
    Query.Update_returning { into; where; what; returning }

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

      let major = Table.Column.(make "major" Type.Int)

      let minor = Table.Column.(make "minor" Type.Int)

      let subminor = Table.Column.(make "subminor" Type.Int)

      let migrated = Table.Column.(make "migrated" Type.Time)

      type columns = int * (int * (int * (Timmy.Time.t * unit)))

      let columns = Table.Column.List.[ major; minor; subminor; migrated ]

      let map Hlist.[ major; minor; subminor; migrated ] =
        { major; minor; subminor; migrated }

      let unmap { major; minor; subminor; migrated } =
        Hlist.[ major; minor; subminor; migrated ]

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
      Table.Column.List.filter_map
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
