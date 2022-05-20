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

let pp_ordering f =
  let open Fmt in
  function Ascending -> string f "ASC" | Descending -> string f "DESC"

module Column = struct
  include Column

  let make ?primary ?(foreign_keys = []) name type_ =
    { name; type_; primary = Option.value ~default:false primary; foreign_keys }

  let make_nullable ?primary ?(foreign_keys = []) name type_ =
    {
      name;
      type_ = Nullable type_;
      primary = Option.value ~default:false primary;
      foreign_keys;
    }

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

  let indexes _ = []
end

module Schema = struct
  let of_table (module T : S) =
    let columns =
      let f Column.{ name; type_; _ } = Schema.{ name; type_ = Type type_ } in
      Column.List.map ~f:{ f } T.Meta.columns
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
            Schema.Foreign_key { table; source; target; on_delete; on_update }
        in
        List.map ~f T.constraints
      in
      match T.primary with
      | [] -> foreign
      | primary ->
        Schema.Primary_key
          (List.map ~f:(fun (Primary { column }) -> column.name) primary)
        :: foreign
    and indexes =
      let f { columns; name } =
        let f = function Index { column; order } -> (column.name, order) in
        Schema.
          {
            name = { name; schema = None };
            columns = List_nonempty.map ~f columns;
          }
      in
      List.map ~f T.indexes
    in
    Schema.
      {
        columns;
        constraints;
        indexes;
        name = { name = T.Meta.name; schema = T.Meta.schema };
      }

  include Schema

  let equal_column (l : column) (r : column) = String.equal l.name r.name
end

module Query = struct
  type t =
    | Create_index of Schema.name * Schema.index
    | Create_table of Schema.t
    | Drop_table of Schema.t

  let create t =
    let schema = Schema.of_table t in
    let indexes =
      List.map schema.indexes ~f:(fun index ->
          Create_index (schema.name, index))
    in
    Create_table schema :: indexes

  let drop t = [ Drop_table (Schema.of_table t) ]
  let pp_either l r f = function Either.First v -> l f v | Second v -> r f v

  let pp_table_name (module Dialect : Dialect.S) f Schema.{ name; schema } =
    Fmt.string f (Dialect.table_name ?schema name)

  let pp_constraint dialect f = function
    | Schema.Primary_key columns ->
      Fmt.(pf f "PRIMARY KEY (@[<h>%a@])" (list ~sep:comma string) columns)
    | Schema.Foreign_key { table; source; target; on_delete; on_update } ->
      let on_delete =
        match on_delete with No_action -> None | action -> Some action
      and on_update =
        match on_update with No_action -> None | action -> Some action
      in
      Fmt.(
        pf f "FOREIGN KEY (%a)@ REFERENCES %a(%a)%a%a"
          (hvbox @@ list ~sep:comma string)
          source (pp_table_name dialect) table
          (hbox @@ list ~sep:comma string)
          target
          (hbox
          @@ option (sp ++ const string "ON DELETE " ++ pp_referential_action))
          on_delete
          (hbox
          @@ option (sp ++ const string "ON UPDATE " ++ pp_referential_action))
          on_update)

  let pp ((module Dialect : Dialect.S) as dialect) f =
    let open Fmt in
    function
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
        |> List.map ~f:(fun Schema.{ name; type_ = Type type_ } ->
               Either.First
                 (Fmt.str "%s %s" name (Dialect.string_of_type type_)))
      and constraints = List.map ~f:Either.second constraints in
      pf f "@[<hv 2>CREATE TABLE %a (@,%a)@]" (pp_table_name dialect) name
        (list ~sep:comma (box @@ pp_either string (pp_constraint dialect)))
        (columns @ constraints)
    | Drop_table { name; _ } ->
      pf f "@[<h> DROP TABLE %a@]" (pp_table_name dialect) name
end
