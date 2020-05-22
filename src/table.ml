module type Meta = sig
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

  module Meta : Meta

  val name : string

  module Expression : sig
    type 't t = ('t, tag) Expression.t

    module List : module type of S_list.Make (struct
      type 't t = ('t, tag) Expression.t
    end)
  end

  val columns : Meta.columns Expression.List.t
end

type 'tag t = (module S with type tag = 'tag)
