open Acid
include Table_intf

let rec pp_ordering : 'a. 'a ordering Fmt.t =
 fun (type t) f (ordering : t ordering) ->
  let open Fmt in
  match ordering with
  | Ordering.Ascending -> string f "ASC"
  | Descending -> string f "DESC"
  | Nulls_first ordering -> pf f "%a NULLS FIRST" pp_ordering ordering
  | Nulls_last ordering -> pf f "%a NULLS LAST" pp_ordering ordering

let pp_referential_action f =
  let open Fmt in
  function
  | No_action -> string f "NO ACTION"
  | Restrict -> string f "RESTRICT"
  | Cascade -> string f "CASCADE"
  | Set_null -> string f "SET NULL"
  | Set_default -> string f "SET DEFAULT"

let pp_deferrability f =
  let open Fmt in
  function
  | Deferrable -> string f "DEFERRABLE"
  | Deferred_initially -> string f "DEFERRABLE INITIALLY DEFERRED"
  | Not_deferrable -> string f "NOT DEFERRABLE"

let referential_action_of_string = function
  | "NO ACTION" -> Some No_action
  | "RESTRICT" -> Some Restrict
  | "CASCADE" -> Some Cascade
  | "SET NULL" -> Some Set_null
  | "SET DEFAULT" -> Some Set_default
  | _ -> None

module Column = struct
  include Column

  let make_composite ?(foreign_keys = []) name type_ =
    { name; type_; foreign_keys }

  let make ?(foreign_keys = []) name type_ =
    make_composite ~foreign_keys name (Atomic type_)

  let make_nullable ?(foreign_keys = []) name type_ =
    make ~foreign_keys name (Nullable type_)

  module Foreign_key = struct
    include Foreign_key

    let make ?(deferrability = Not_deferrable) ?(on_delete = No_action)
        ?(on_update = No_action) reference =
      Reference { deferrability; reference; on_delete; on_update }

    let make_nullable ?(deferrability = Not_deferrable) ?(on_delete = No_action)
        ?(on_update = No_action) reference =
      Reference_opt { deferrability; reference; on_delete; on_update }
  end
end

let name (type tag) (module T : S' with type tag = tag) = T.Meta.name

module Meta (Minimal : sig
  val name : string

  type columns

  val columns : columns Column.List.t

  type primary

  val primary :
    (columns, 'tag) Expression.Column.List.t ->
    (primary, 'tag) Expression.Column.List.t
end) =
struct
  let schema = None
  let name = Minimal.name

  type columns = Minimal.columns

  let columns = Minimal.columns

  type mapped = columns Hlist.t

  let map v = v
  let unmap v = v
  let constraints _ = []

  type primary = Minimal.primary

  let primary = Minimal.primary

  type indexes = unit

  let indexes _ = Index.List.[]
end

module Schema = struct
  let rec unroll_column ({ name; type_ } : Schema.column) :
      Schema.column_atomic list =
    match type_ with
    | Type (Atomic t) -> [ { name; type_ = Type t } ]
    | Type (Composite { columns; _ }) ->
      let rec of_types : type t.
          t Column.Type.List.t -> Schema.column_atomic list = function
        | Column.Type.List.[] -> []
        | (field, t) :: tl ->
          { name = name ^ "_" ^ field; type_ = Type t } :: of_types tl
      in
      of_types columns
    | Type (Custom { repr; _ }) -> unroll_column { name; type_ = Type repr }

  let of_table (type tag) (module T : S with type tag = tag) =
    let columns =
      let f Column.{ name; type_; _ } =
        Schema.{ name; type_ = Type type_ } |> unroll_column
      in
      Column.List.map ~f:{ f } T.Meta.columns
      |> List.concat
      |> Set.of_list (module Schema.Column_by_name)
    and constraints =
      let foreign =
        let f = function
          | Foreign_key { deferrability; reference; on_delete; on_update } ->
            let f = function
              | Foreign_reference { target; column } ->
                (column.name, target.column.name)
              | Foreign_reference_opt { target; column } ->
                (column.name, target.column.name)
            in
            let source, target =
              List_nonempty.to_list reference |> List.map ~f |> List.unzip
            and table =
              match List_nonempty.hd reference with
              | Foreign_reference { target = { table = (module T); _ }; _ }
              | Foreign_reference_opt { target = { table = (module T); _ }; _ }
                ->
                Schema.{ name = T.Meta.name; schema = T.Meta.schema }
            in
            Schema.Constraint.Foreign_key
              {
                deferrability;
                name =
                  Fmt.(
                    str "%s_%a_fkey" T.Meta.name
                      (list ~sep:(const char '_') string)
                      source);
                table;
                source;
                target;
                on_delete;
                on_update;
              }
        in
        List.map ~f T.constraints
      in
      let (Unique { columns; _ }) = T.primary in
      match
        Index.Column.List.map
          ~f:{ f = (fun { column = { name; _ }; _ } -> name) }
          columns
      with
      | [] -> foreign
      | columns ->
        Schema.Constraint.Primary_key
          { name = Fmt.(str "%s_pkey" T.Meta.name); columns }
        :: foreign
    and indexes =
      let f : type kind tag. (kind, tag) Index.t -> _ =
        let of_columns columns =
          let f = function
            | { Index.Column.column; order } ->
              let rec index_order : 't. 't ordering -> Schema.Index.ordering =
               fun (type t) (ordering : t ordering) ->
                match ordering with
                | Ascending ->
                  { Schema.Index.ascending = true; nulls_first = None }
                | Descending -> { ascending = false; nulls_first = None }
                | Nulls_first order ->
                  { (index_order order) with nulls_first = Some true }
                | Nulls_last order ->
                  { (index_order order) with nulls_first = Some false }
              in
              (column.name, index_order order)
          in
          Index.Column.List.map ~f:{ f } columns |> List_nonempty.of_list_exn
        in
        function
        | Index.Non_unique { columns; name } ->
          {
            Schema.Index.columns = of_columns columns;
            name = { name; schema = None };
            unique = false;
          }
        | Index.Unique { columns; name } ->
          Schema.
            {
              columns = of_columns columns;
              name = { name; schema = None };
              unique = true;
            }
      in
      Index.List.map ~f:{ f } T.indexes
    in
    Schema.
      {
        columns;
        constraints;
        indexes;
        name = { name = T.Meta.name; schema = T.Meta.schema };
      }

  include Schema

  module Constraint = struct
    include Constraint
    include Base.Comparable.Make (Constraint)
  end

  module Index = struct
    include Index
    include Base.Comparable.Make (Index)
  end
end

module Query = struct
  type t =
    | Add_column of Schema.name * Schema.column_atomic
    | Add_constraint of Schema.name * Schema.Constraint.t
    | Alter_column_nullable of Schema.name * string * bool
    | Alter_column_type of
        Schema.name
        * string
        * Expression.Column.Type.any_atomic
        * Expression.Column.Type.any_atomic
    | Create_index of Schema.name * Schema.Index.t
    | Create_table of Schema.t
    | Drop_column of Schema.name * Schema.column_atomic
    | Drop_constraint of Schema.name * string
    | Drop_index of Schema.name
    | Drop_table of Schema.t

  module Schema_ = struct
    let add_constraint table c = Add_constraint (table, c)
    let add_column table c = Add_column (table, c)

    let alter_column_nullable table c nullable =
      Alter_column_nullable (table, c, nullable)

    let alter_column_type table c from to_ =
      Alter_column_type (table, c, from, to_)

    let create (schema : Schema.t) =
      let indexes =
        List.map schema.indexes ~f:(fun index ->
            Create_index (schema.name, index))
      in
      Create_table schema :: indexes

    let create_index table index = [ Create_index (table, index) ]
    let drop schema = [ Drop_table schema ]
    let drop_constraint table name = [ Drop_constraint (table, name) ]
    let drop_column table column = [ Drop_column (table, column) ]
    let drop_index name = [ Drop_index name ]
  end

  let create t = Schema.of_table t |> Schema_.create
  let drop t = Schema.of_table t |> Schema_.drop

  let drop_column (table : (module S with type tag = 'tag))
      ({ name; type_; _ } : ('t, 'tag) Expression.Column.t) =
    let table = Schema.of_table table
    and columns : Schema.column_atomic list =
      Schema.unroll_column { name; type_ = Type type_ }
    in
    List.map columns ~f:(fun column -> Drop_column (table.name, column))

  let pp_either l r f = function Either.First v -> l f v | Second v -> r f v

  let pp_table_name (module Dialect : Dialect.S) f Schema.{ name; schema } =
    Fmt.string f (Dialect.table_name ?schema name)

  let pp_constraint dialect f =
    let open Fmt in
    function
    | Schema.Constraint.Primary_key { name; columns } ->
      pf f "@[<b 2>CONSTRAINT %s@ @[PRIMARY KEY (@[<h>%a@])@]@]" name
        (list ~sep:comma string) columns
    | Foreign_key
        { deferrability; name; table; source; target; on_delete; on_update } ->
      let deferrability =
        match deferrability with
        | Not_deferrable -> None
        | deferrability -> Some deferrability
      and on_delete =
        match on_delete with No_action -> None | action -> Some action
      and on_update =
        match on_update with No_action -> None | action -> Some action
      in
      pf f
        "@[<b 2>CONSTRAINT %s@ @[FOREIGN KEY (%a)@ REFERENCES %a(%a)%a%a%a@]@]"
        name
        (hvbox @@ list ~sep:comma string)
        source (pp_table_name dialect) table
        (hbox @@ list ~sep:comma string)
        target
        (hbox
        @@ option (sp ++ const string "ON DELETE " ++ pp_referential_action))
        on_delete
        (hbox
        @@ option (sp ++ const string "ON UPDATE " ++ pp_referential_action))
        on_update
        (hbox @@ option (sp ++ pp_deferrability))
        deferrability

  let pp ((module Dialect : Dialect.S) as dialect) f =
    let open Fmt in
    function
    | Add_column (table, { name; type_ = Type type_ }) ->
      pf f "@[<h>ALTER TABLE %a@ @[ADD COLUMN %s@ %a@]@]"
        (pp_table_name dialect) table name Dialect.pp_type type_
    | Add_constraint (table, c) ->
      pf f "@[<h>ALTER TABLE %a ADD %a@]" (pp_table_name dialect) table
        (pp_constraint dialect) c
    | Alter_column_nullable (table, column, nullable) ->
      pf f "@[<h>ALTER TABLE %a ALTER COLUMN %s SET %s@]"
        (pp_table_name dialect) table column
        (if nullable then "NULL" else "NOT NULL")
    | Alter_column_type (table, column, Type from, Type to_) ->
      pf f "@[<h>ALTER TABLE %a ALTER COLUMN %s TYPE %s /* from %s */@]"
        (pp_table_name dialect) table column
        (Dialect.string_of_type ~include_nullable:false to_)
        (Dialect.string_of_type ~include_nullable:false from)
    | Create_index (table_name, { columns; name = { name; schema }; unique }) ->
      let rec pp_ordering f =
        let open Fmt in
        function
        | { Schema.Index.ascending = true; nulls_first = None } ->
          string f "ASC"
        | { Schema.Index.ascending = false; nulls_first = None } ->
          string f "DESC"
        | { nulls_first = Some true; _ } as ordering ->
          pf f "%a NULLS FIRST" pp_ordering ordering
        | { nulls_first = Some false; _ } as ordering ->
          pf f "%a NULLS LAST" pp_ordering ordering
      in
      pf f "@[<2>CREATE %sINDEX %s@ ON %a@ (%a)@]"
        (if unique then "UNIQUE " else "")
        (Dialect.index_name ?schema name)
        (pp_table_name dialect) table_name
        (box
        @@ list ~sep:comma (fun f (name, ordering) ->
               Fmt.pf f "%s %a" name pp_ordering ordering))
        (List_nonempty.to_list columns)
    | Create_table { columns; constraints; name; _ } ->
      let columns =
        Set.to_list columns
        |> List.map
             ~f:(fun ({ name; type_ = Type type_ } : Schema.column_atomic) ->
               Either.First (Fmt.str "%s %a" name Dialect.pp_type type_))
      and constraints = List.map ~f:Either.second constraints in
      pf f "@[<hv 2>CREATE TABLE %a (@,%a)@]" (pp_table_name dialect) name
        (list ~sep:comma (box @@ pp_either string (pp_constraint dialect)))
        (columns @ constraints)
    | Drop_column (table, { name; _ }) ->
      pf f "@[<h>ALTER TABLE %a DROP COLUMN %s@]" (pp_table_name dialect) table
        name
    | Drop_constraint (table, name) ->
      pf f "@[<h>ALTER TABLE %a DROP CONSTRAINT %s@]" (pp_table_name dialect)
        table name
    | Drop_index name ->
      pf f "@[<h>DROP INDEX %a@]" (pp_table_name dialect) name
    | Drop_table { name; _ } ->
      pf f "@[<h> DROP TABLE %a@]" (pp_table_name dialect) name

  module Schema = Schema_
end
