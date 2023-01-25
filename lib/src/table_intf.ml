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
[@@deriving eq, ord, sexp_of]

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

module Index = struct
  module Column = struct
    type ('t, 'tag) t = {
      column : ('t, 'tag) Expression.Column.t;
      order : ordering;
    }

    module List = Hlist.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t
    end)
  end

  type kind_non_unique = Non_unique
  type kind_unique = Unique

  type ('a, 'unique) kind =
    | Non_unique : ('a, kind_non_unique) kind
    | Unique : ('a, kind_unique) kind

  type ('columns, 'tag) t =
    | Non_unique : {
        columns : ('columns, 'tag) Column.List.t;
        name : string;
      }
        -> (('columns, kind_non_unique) kind, 'tag) t
    | Unique : {
        columns : ('columns, 'tag) Column.List.t;
        name : string;
      }
        -> (('columns, kind_unique) kind, 'tag) t

  type 'a unique = ('a, kind_unique) kind
  type 'a non_unique = ('a, kind_non_unique) kind

  module List = Hlist.Make2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t
  end)
end

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

  type primary

  val primary :
    (columns, 'tag) Expression.Column.List.t ->
    (primary, 'tag) Expression.Column.List.t

  type indexes

  val indexes :
    (columns, 'tag) Expression.Column.List.t -> (indexes, 'tag) Index.List.t
end

module type S = sig
  module Meta : META
  include S' with module Meta := Meta

  val constraints : tag constraint_ list
  val primary : (Meta.primary Index.unique, tag) Index.t
  val indexes : (Meta.indexes, tag) Index.List.t
end

type 'tag t = (module S with type tag = 'tag)

module Schema = struct
  type name = {
    schema : string option;
    name : string;
  }
  [@@deriving eq, ord, sexp_of]

  type column_atomic = {
    name : string;
    type_ : Column.Type.any_atomic;
  }
  [@@deriving eq]

  type column = {
    name : string;
    type_ : Column.Type.any;
  }
  [@@deriving eq]

  module Column_by_name = struct
    type t = column_atomic

    include Base.Comparable.Make (struct
      type t = column_atomic

      let compare ({ name = l; _ } : t) ({ name = r; _ } : t) =
        String.compare l r

      let sexp_of_t ({ name; _ } : t) = Sexp.Atom name
    end)
  end

  module Constraint = struct
    type t =
      | Foreign_key of {
          name : string;
          table : name;
          source : string list;
          target : string list;
          on_delete : referential_action;
          on_update : referential_action;
        }
      | Primary_key of {
          name : string;
          columns : string list;
        }
    [@@deriving eq, ord, sexp_of]
  end

  type index = {
    columns : (string * ordering) List_nonempty.t;
    name : name;
    unique : bool;
  }
  [@@deriving eq]

  type t = {
    columns : (column_atomic, Column_by_name.comparator_witness) Set.t;
        [@equal Set.equal]
    constraints : Constraint.t list;
    indexes : index list;
    name : name;
  }
  [@@deriving eq]
end
