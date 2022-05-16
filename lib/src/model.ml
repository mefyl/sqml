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
          | Table.Column.List.({ foreign_keys; name; primary; type_; _ } :: tl)
            ->
            let column =
              Expression.make_column ~name ~primary ~selectable:Meta.name type_
            in
            let constraints =
              List.map
                ~f:(function
                  | Table.Column.Foreign_key.Reference
                      { reference = target; on_delete; on_update } ->
                    Table.(
                      Foreign_key
                        {
                          reference =
                            List_nonempty.
                              [ Foreign_reference { column; target } ];
                          on_delete;
                          on_update;
                        })
                  | Table.Column.Foreign_key.Reference_opt
                      { reference = target; on_delete; on_update } ->
                    Table.(
                      Foreign_key
                        {
                          reference =
                            List_nonempty.
                              [ Foreign_reference_opt { column; target } ];
                          on_delete;
                          on_update;
                        }))
                foreign_keys
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

        module List = struct
          include List

          type 't t = ('t, tag) List.t
          type 't value = ('t, tag) List.value
        end
      end

      let all =
        let columns = Expression.(of_list columns) in
        Expression.map
          ~decode:(fun v -> Result.return (Meta.map v))
          ~encode:Meta.unmap columns

      let make columns =
        let columns = Expression.of_list columns in
        Expression.map
          ~decode:(fun v -> Result.return (Meta.map v))
          ~encode:Meta.unmap columns

      let to_ast () = Selectable.Selectable { alias = None; name }
      let constraints = Column.constraints @ Meta.constraints Column.columns
      let primary = Meta.primary Column.columns
      let indexes = Meta.indexes Column.columns
    end

    let _ = v.tables <- Array.append v.tables [| (module Res) |]

    include Res
  end

  module type JOIN = sig
    include Selectable.S
    module L : Selectable.S with type model = model
    module R : Selectable.S with type model = model
  end

  module type INNER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  module type LEFT_OUTER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t Option.t, tag) Expression.t
  end

  module type RIGHT_OUTER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t Option.t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  module type OUTER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t Option.t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t Option.t, tag) Expression.t
  end

  let map_name f selectable =
    let rec map_name = function
      | Selectable.Selectable { name; alias } ->
        let alias = Option.value ~default:name alias in
        ( Selectable.Selectable { name; alias = Some (f alias) },
          Set.of_list (module String) [ alias ] )
      | Join ({ left; right; on; _ } as pl) ->
        let left, left_mapped = map_name left
        and right, right_mapped = map_name right in
        let mapped = Set.union left_mapped right_mapped in
        let on =
          Option.map ~f:(Expression.traverse_ast ~f:(map_name_ast mapped)) on
        in
        (Join { pl with left; right; on }, mapped)
    and map_name_ast mapped = function
      | Column ({ selectable; _ } as pl) when Set.mem mapped selectable ->
        Column { pl with selectable = f selectable }
      | ast -> ast
    in
    map_name selectable |> fst

  module Join
      (L : Selectable.S with type model = tag)
      (R : Selectable.S with type model = tag) =
  struct
    module Content = struct
      module type INNER =
        INNER_JOIN with type model = tag and module L = L and module R = R

      module type OUTER =
        OUTER_JOIN with type model = tag and module L = L and module R = R

      module type LEFT_OUTER =
        LEFT_OUTER_JOIN with type model = tag and module L = L and module R = R

      module type RIGHT_OUTER =
        RIGHT_OUTER_JOIN with type model = tag and module L = L and module R = R

      type model = tag
      type tag = unit

      module L = L
      module R = R

      let left : type a. (a, L.tag) Expression.t -> (a, tag) Expression.t =
       fun e -> Expression.map_name (fun n -> "l_" ^ n) e

      let right : type a. (a, R.tag) Expression.t -> (a, tag) Expression.t =
       fun e -> Expression.map_name (fun n -> "r_" ^ n) e

      let to_ast_ type_ on =
        Selectable.Join
          {
            left = map_name (fun n -> "l_" ^ n) @@ L.to_ast ();
            right = map_name (fun n -> "r_" ^ n) @@ R.to_ast ();
            on;
            type_;
          }
    end

    let on expr =
      (module struct
        include Content

        let to_ast () = to_ast_ Inner (Some (Expression.to_ast expr))
      end : Content.INNER)

    let outer_on expr =
      (module struct
        include Content

        let left e =
          let e = left e in
          Expression.some e

        let right e =
          let e = right e in
          Expression.some e

        let to_ast () = to_ast_ Outer (Some (Expression.to_ast expr))
      end : Content.OUTER)

    let left_on expr =
      (module struct
        include Content

        let right e =
          let e = right e in
          Expression.some e

        let to_ast () = to_ast_ Outer_left (Some (Expression.to_ast expr))
      end : Content.LEFT_OUTER)

    let right_on expr =
      (module struct
        include Content

        let left e =
          let e = left e in
          Expression.some e

        let to_ast () = to_ast_ Outer_right (Some (Expression.to_ast expr))
      end : Content.RIGHT_OUTER)

    include Content

    let to_ast () = to_ast_ Inner None
  end

  module Query = struct
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
          for_ : for_ option;
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
          from :
            (module Selectable.S with type model = tag and type tag = 'from)
            option;
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

  let select ~from ?where ?for_ what = Query.Select { for_; from; what; where }

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
    Query.Update_returning { from = None; into; returning; what; where }

  let update_from_returning (type from) into
      ~from:
        (module From : Selectable.S with type model = tag and type tag = from)
      ?where ~returning what =
    let rename name = "from_" ^ name in
    let map e = Expression.map_name rename e in
    let from =
      (module struct
        include From

        let to_ast () = map_name rename (to_ast ())
      end : Selectable.S
        with type model = tag
         and type tag = from)
    and returning = returning { map }
    and what = what { map }
    and where = Option.map ~f:(fun where -> where { map }) where in
    Query.Update_returning { from = Some from; into; returning; what; where }

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

      let primary Expression.Column.List.[ major; minor; subminor; _ ] =
        [
          Table.Primary { column = major };
          Table.Primary { column = minor };
          Table.Primary { column = subminor };
        ]

      let indexes _ = []
    end

    include BindTable (Meta_)
  end
end

type column_creation = {
  name : string;
  type_ : string;
  primary : bool;
}

let table_schema (type tag) (module Dialect : Dialect.S) (table : tag Table.t) =
  let module T = (val table) in
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
    match T.primary with
    | [] -> definitions
    | primary ->
      definitions
      @ [
          Fmt.(
            str "PRIMARY KEY (%a)"
              (hbox
              @@ list ~sep:comma
                   (using
                      (fun (Table.Primary { column }) -> column.name)
                      string))
              primary);
        ]
  and table_name = Dialect.table_name ?schema:T.Meta.schema T.Meta.name in
  let definitions =
    let constraints =
      let f = function
        | Table.Foreign_key { reference; on_delete; on_update } ->
          let f = function
            | Table.Foreign_reference { target; column } ->
              (column.name, target.column.name)
            | Table.Foreign_reference_opt { target; column } ->
              (column.name, target.column.name)
          in
          let on_delete =
            match on_delete with No_action -> None | action -> Some action
          and on_update =
            match on_update with No_action -> None | action -> Some action
          in
          let columns, references =
            List_nonempty.to_list reference |> List.map ~f |> List.unzip
          and target_table =
            match List_nonempty.hd reference with
            | Table.Foreign_reference { target; _ } -> Table.name target.table
            | Table.Foreign_reference_opt { target; _ } ->
              Table.name target.table
          in
          Fmt.(
            str "FOREIGN KEY (%a) REFERENCES %s(%a)%a%a"
              (hbox @@ list ~sep:comma string)
              columns target_table
              (hbox @@ list ~sep:comma string)
              references
              (hbox
              @@ option
                   (sp ++ const string "ON DELETE "
                  ++ Table.pp_referential_action))
              on_delete
              (hbox
              @@ option
                   (sp ++ const string "ON UPDATE "
                  ++ Table.pp_referential_action))
              on_update)
      in
      List.map ~f T.constraints
    in
    definitions @ constraints
  and indexes =
    let f Table.{ columns; name } =
      let f = function
        | Table.Index { column; order } ->
          let order =
            match order with Ascending -> "ASC" | Descending -> "DESC"
          in
          Fmt.str "%s %s" column.name order
      in
      Fmt.(
        str "@[<2>CREATE INDEX %s@ ON %s@ (%a)@]"
          (Dialect.index_name ?schema:T.Meta.schema name)
          table_name
          (box @@ list ~sep:comma string)
        @@ List.map ~f
        @@ List_nonempty.to_list columns)
    in
    List.map ~f T.indexes
  in
  Fmt.(
    str "@[<hv 2>CREATE TABLE %s (@,%a)@]" table_name
      (box @@ list ~sep:comma string)
      definitions)
  :: indexes

let schema dialect (module Model : S) =
  let create_table (module T : Table.S) = table_schema dialect (module T) in
  Array.map ~f:create_table (Model.tables ()) |> Array.to_list |> List.concat
