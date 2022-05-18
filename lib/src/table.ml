open Acid

module type META' = sig
  val schema : string option
  val name : string

  type columns
  type mapped

  val map : columns Hlist.t -> mapped
  val unmap : mapped -> columns Hlist.t
end

module type S' = sig
  include Selectable.S
  module Meta : META'

  module Column : sig
    type 't t = ('t, tag) Expression.Column.t

    val columns : (Meta.columns, tag) Expression.Column.List.t
  end

  val const : Meta.mapped -> (Meta.mapped, _) Expression.t
  val column_types : Meta.columns Expression.Type.List.t
  val type_ : Meta.mapped Expression.Type.t

  module Expression : sig
    include module type of Expression

    type 't t = ('t, tag) Expression.t
    type 't column = ('t, tag) Expression.Column.t

    module List : sig
      include module type of List

      type 't t = ('t, tag) List.t
      type 't value = ('t, tag) List.value
    end

    val columns : (Meta.columns, tag) Expression.List.t
  end

  val all : Meta.mapped Expression.t
  val make : Meta.columns Expression.List.t -> Meta.mapped Expression.t
end

type ('t, 'target) reference = {
  table : (module S' with type tag = 'target);
  column : ('t, 'target) Expression.Column.t;
}

type ('ref, 'target) reference_with_actions = {
  reference : 'ref;
  on_delete : referential_action; [@default No_action]
  on_update : referential_action; [@default No_action]
}

and referential_action =
  | No_action
  | Restrict
  | Cascade
  | Set_null
  | Set_default

type ('tag, 'target) foreign_reference =
  | Foreign_reference : {
      column : ('t, 'tag) Expression.Column.t;
      target : ('t, 'target) reference;
    }
      -> ('tag, 'target) foreign_reference
  | Foreign_reference_opt : {
      column : ('t option, 'tag) Expression.Column.t;
      target : ('t, 'target) reference;
    }
      -> ('tag, 'target) foreign_reference

module type COLLECTION = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

type 'tag constraint_ =
  | Foreign_key :
      ( ('tag, 'target) foreign_reference List_nonempty.t,
        'target )
      reference_with_actions
      -> 'tag constraint_

let pp_referential_action f =
  let open Fmt in
  function
  | No_action -> string f "NO ACTION"
  | Restrict -> string f "RESTRICT"
  | Cascade -> string f "CASCADE"
  | Set_null -> string f "SET NULL"
  | Set_default -> string f "SET DEFAULT"

type ordering =
  | Ascending
  | Descending

type 'tag primary_column =
  | Primary : { column : ('t, 'tag) Expression.Column.t } -> 'tag primary_column

type 'tag primary = 'tag primary_column list

type 'tag index_column =
  | Index : {
      column : ('t, 'tag) Expression.Column.t;
      order : ordering;
    }
      -> 'tag index_column

type 'tag index = {
  columns : 'tag index_column List_nonempty.t;
  name : string;
}

module Column = struct
  module Type = Expression.Column.Type

  module Foreign_key = struct
    type 't t =
      | Reference : (('t, 'tag) reference, 'tag) reference_with_actions -> 't t
      | Reference_opt :
          (('t, 'tag) reference, 'tag) reference_with_actions
          -> 't option t

    let make ?(on_delete = No_action) ?(on_update = No_action) reference =
      Reference { reference; on_delete; on_update }

    let make_nullable ?(on_delete = No_action) ?(on_update = No_action)
        reference =
      Reference_opt { reference; on_delete; on_update }
  end

  type 't t = {
    name : string;
    type_ : 't Expression.Column.Type.t;
    primary : bool;
    foreign_keys : 't Foreign_key.t list;
  }

  let make ?primary ?(foreign_keys = []) name type_ =
    { name; type_; primary = Option.value ~default:false primary; foreign_keys }

  let make_nullable ?primary ?(foreign_keys = []) name type_ =
    {
      name;
      type_ = Nullable type_;
      primary = Option.value ~default:false primary;
      foreign_keys;
    }

  module List = Hlist.Make (struct
    type nonrec 'a t = 'a t
  end)
end

let name (type tag) (module T : S' with type tag = tag) = T.Meta.name

module type META = sig
  include META'

  val columns : columns Column.List.t

  val constraints :
    (columns, 'tag) Expression.Column.List.t -> 'tag constraint_ list

  val primary : (columns, 'tag) Expression.Column.List.t -> 'tag primary
  val indexes : (columns, 'tag) Expression.Column.List.t -> 'tag index list
end

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

module type S = sig
  module Meta : META
  include S' with module Meta := Meta

  val constraints : tag constraint_ list
  val primary : tag primary
  val indexes : tag index list
end

type 'tag t = (module S with type tag = 'tag)

module Schema = struct
  type column = {
    name : string;
    type_ : Column.Type.any;
  }

  let equal_column (l : column) (r : column) = String.equal l.name r.name

  module Column_by_name = struct
    type t = column

    include Base.Comparable.Make (struct
      type t = column

      let compare ({ name = l; _ } : t) ({ name = r; _ } : t) =
        String.compare l r

      let sexp_of_t ({ name; _ } : t) = Sexp.Atom name
    end)
  end

  type t = {
    name : string;
    schema : string option;
    columns : (column, Column_by_name.comparator_witness) Set.t;
        [@equal Set.equal]
  }
  [@@deriving eq]

  let table_name = function
    | { schema = Some schema; name; _ } -> Fmt.str "%s.%s" schema name
    | { name; _ } -> name

  let of_table (module Table : S) =
    let columns =
      let f Column.{ name; type_; _ } = { name; type_ = Type type_ } in
      Column.List.map ~f:{ f } Table.Meta.columns
      |> Set.of_list (module Column_by_name)
    in
    { name = Table.Meta.name; schema = Table.Meta.schema; columns }
end
