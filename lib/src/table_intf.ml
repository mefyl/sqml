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

type referential_action =
  | No_action
  | Restrict
  | Cascade
  | Set_null
  | Set_default
[@@deriving eq]

type ('ref, 'target) reference_with_actions = {
  reference : 'ref;
  on_delete : referential_action; [@default No_action]
  on_update : referential_action; [@default No_action]
}

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

type 'tag constraint_ =
  | Foreign_key :
      ( ('tag, 'target) foreign_reference List_nonempty.t,
        'target )
      reference_with_actions
      -> 'tag constraint_

type ordering =
  | Ascending
  | Descending
[@@deriving eq]

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
  end

  type 't t = {
    name : string;
    type_ : 't Expression.Column.Type.t;
    primary : bool;
    foreign_keys : 't Foreign_key.t list;
  }

  module List = Hlist.Make (struct
    type nonrec 'a t = 'a t
  end)
end

module type META = sig
  include META'

  val columns : columns Column.List.t

  val constraints :
    (columns, 'tag) Expression.Column.List.t -> 'tag constraint_ list

  val primary : (columns, 'tag) Expression.Column.List.t -> 'tag primary
  val indexes : (columns, 'tag) Expression.Column.List.t -> 'tag index list
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
  type name = {
    name : string;
    schema : string option;
  }
  [@@deriving eq]

  type column = {
    name : string;
    type_ : Column.Type.any;
  }

  module Column_by_name = struct
    type t = column

    include Base.Comparable.Make (struct
      type t = column

      let compare ({ name = l; _ } : t) ({ name = r; _ } : t) =
        String.compare l r

      let sexp_of_t ({ name; _ } : t) = Sexp.Atom name
    end)
  end

  type constraint_ =
    | Foreign_key of {
        table : name;
        source : string list;
        target : string list;
        on_delete : referential_action;
        on_update : referential_action;
      }
    | Primary_key of string list
  [@@deriving eq]

  type index = {
    name : name;
    columns : (string * ordering) List_nonempty.t;
  }
  [@@deriving eq]

  type t = {
    columns : (column, Column_by_name.comparator_witness) Set.t;
        [@equal Set.equal]
    constraints : constraint_ list;
    indexes : index list;
    name : name;
  }
  [@@deriving eq]
end
