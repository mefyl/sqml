open Acid

module type META' = sig
  val schema : string option

  val name : string

  type columns

  type mapped

  val map : columns -> mapped

  val unmap : mapped -> columns
end

module type S' = sig
  include Selectable.S

  module Meta : META'

  module Column : sig
    type 't t = ('t, tag) Expression.column

    val columns : (Meta.columns, tag) Expression.Column_list.t
  end

  module Expression : sig
    type 't t = ('t, tag) Expression.t

    val columns : (Meta.columns, tag) Expression.List.t
  end

  val all : Meta.mapped Expression.t
end

type ('t, 'target) reference = {
  table : (module S' with type tag = 'target);
  column : ('t, 'target) Expression.column;
}

type ('tag, 'target) foreign_reference =
  | Foreign_reference : {
      target : ('t, 'target) reference;
      column : ('t, 'tag) Expression.column;
    }
      -> ('tag, 'target) foreign_reference

module type COLLECTION = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module Nonempty_list = struct
  type 'a t = 'a * 'a list

  let map (hd, tl) ~f = (f hd, List.map ~f tl)

  let make ?(tail = []) hd = (hd, tail)

  let hd_nonempty (v, _) = v

  let to_list (hd, tl) = hd :: tl
end

type 'tag constraint_ =
  | Foreign_key :
      ('tag, 'target) foreign_reference Nonempty_list.t
      -> 'tag constraint_

module Column = struct
  type 't foreign_key = Reference : ('t, _) reference -> 't foreign_key

  type 't t = {
    name : string;
    type_ : 't Type.t;
    primary : bool;
    foreign_keys : 't foreign_key list;
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

  module List = S_list.Make (struct
    type nonrec 'a t = 'a t
  end)
end

type 'tag t = (module S' with type tag = 'tag)

let name (type tag) (module T : S' with type tag = tag) = T.Meta.name

module type META = sig
  include META'

  val columns : columns Column.List.t

  val constraints :
    (columns, 'tag) Expression.Column_list.t -> 'tag constraint_ list
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

  type mapped = columns

  let map v = v

  let unmap v = v

  let constraints _ = []
end

module type S = sig
  module Meta : META

  include S' with module Meta := Meta

  val constraints : tag constraint_ list
end
