module type META' = sig
  val schema : string option

  val name : string

  type columns

  val columns : columns Column.List.t

  type mapped

  val map : columns -> mapped

  val unmap : mapped -> columns
end

module type S = sig
  include Selectable.S

  module Meta : META'

  module Column : sig
    type 't t = ('t, tag) Expression.column

    module List : module type of S_list.Make (struct
      type 't t = ('t, tag) Expression.column
    end)

    val columns : Meta.columns List.t
  end

  module Expression : sig
    type 't t = ('t, tag) Expression.t

    module List : module type of S_list.Make (struct
      type 't t = ('t, tag) Expression.t
    end)
  end

  val all : Meta.mapped Expression.t

  val columns : Meta.columns Expression.List.t
end

type 'tag constraint_ =
  | Foreign_key : {
      table : (module S with type tag = 'target);
      target_column : ('t, 'target) Expression.column;
      column : ('t, 'tag) Expression.column;
    }
      -> 'tag constraint_

type 'tag t = (module S with type tag = 'tag)

module type META = sig
  include META'

  val constraints :
    (columns, 'tag) Bound_columns.List.t -> 'tag constraint_ list
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
