open Acid
include Table_intf

let pp_referential_action f =
  let open Fmt in
  function
  | No_action -> string f "NO ACTION"
  | Restrict -> string f "RESTRICT"
  | Cascade -> string f "CASCADE"
  | Set_null -> string f "SET NULL"
  | Set_default -> string f "SET DEFAULT"

let referential_action_of_string = function
  | "NO ACTION" -> Some No_action
  | "RESTRICT" -> Some Restrict
  | "CASCADE" -> Some Cascade
  | "SET NULL" -> Some Set_null
  | "SET DEFAULT" -> Some Set_default
  | _ -> None

let pp_ordering f =
  let open Fmt in
  function Ascending -> string f "ASC" | Descending -> string f "DESC"

module Column = struct
  include Column

  let make_composite ?primary ?(foreign_keys = []) name type_ =
    { name; type_; primary = Option.value ~default:false primary; foreign_keys }

  let make ?primary ?(foreign_keys = []) name type_ =
    make_composite ?primary ~foreign_keys name (Atomic type_)

  let make_nullable ?primary ?(foreign_keys = []) name type_ =
    make ?primary ~foreign_keys name (Nullable type_)

  module Foreign_key = struct
    include Foreign_key

    let make ?(on_delete = No_action) ?(on_update = No_action) reference =
      Reference { reference; on_delete; on_update }

    let make_nullable ?(on_delete = No_action) ?(on_update = No_action)
        reference =
      Reference_opt { reference; on_delete; on_update }
  end
end

let name (type tag) (module T : S' with type tag = tag) = T.Meta.name

module Meta (Minimal : sig
  val name : string

  type columns

  val columns : columns Column.List.t
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

  let primary columns =
    Expression.Column.List.filter_map
      ~f:
        {
          f = (fun column -> Option.some_if column.primary (Primary { column }));
        }
      columns

  type indexes = unit

  let indexes _ = Index.List.[]
end

module Schema = struct
  let unroll_column ({ name; type_ } : Schema.column) :
      Schema.column_atomic list =
    match type_ with
    | Type (Atomic t) -> [ { name; type_ = Type t } ]
    | Type (Composite { columns; _ }) ->
      let rec of_types :
          type t. t Column.Type.List.t -> Schema.column_atomic list = function
        | Column.Type.List.[] -> []
        | (field, t) :: tl ->
          { name = name ^ "_" ^ field; type_ = Type t } :: of_types tl
      in
      of_types columns

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
          | Foreign_key { reference; on_delete; on_update } ->
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
      match T.primary with
      | [] -> foreign
      | primary ->
        Schema.Constraint.Primary_key
          {
            name = Fmt.(str "%s_pkey" T.Meta.name);
            columns =
              List.map ~f:(fun (Primary { column }) -> column.name) primary;
          }
        :: foreign
    and indexes =
      let f { Index.columns; name } =
        let f = function
          | { Index.Column.column; order } -> (column.name, order)
        in
        Schema.
          {
            name = { name; schema = None };
            columns =
              Index.Column.List.map ~f:{ f } columns
              |> List_nonempty.of_list_exn;
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
    | Create_index of Schema.name * Schema.index
    | Create_table of Schema.t
    | Drop_column of Schema.name * Schema.column_atomic
    | Drop_constraint of Schema.name * string
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

    let drop schema = [ Drop_table schema ]
    let drop_constraint table name = [ Drop_constraint (table, name) ]
    let drop_column table column = [ Drop_column (table, column) ]
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
    | Foreign_key { name; table; source; target; on_delete; on_update } ->
      let on_delete =
        match on_delete with No_action -> None | action -> Some action
      and on_update =
        match on_update with No_action -> None | action -> Some action
      in
      pf f "@[<b 2>CONSTRAINT %s@ @[FOREIGN KEY (%a)@ REFERENCES %a(%a)%a%a@]@]"
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
    | Create_index (table_name, { name = { name; schema }; columns }) ->
      pf f "@[<2>CREATE INDEX %s@ ON %a@ (%a)@]"
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
    | Drop_table { name; _ } ->
      pf f "@[<h> DROP TABLE %a@]" (pp_table_name dialect) name

  module Schema = Schema_
end
